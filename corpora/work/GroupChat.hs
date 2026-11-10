{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module GroupChat where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.Aeson hiding (pairs)
import Data.Char
import Data.IORef
import Data.List
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory
import System.FilePath
import Types
import qualified Store as DB
import qualified Population as Population
import qualified Routines as Routine
import qualified Workspace as Workspace
import Memo
import Provider
import GroupModel
import GroupWork

mentions :: Service -> Value -> Text -> IO [Text]
mentions s group content = DB.agents s >>= validated . mentionedMembers group content

mentionedMembers :: Value -> Text -> [Value] -> Validation [Text]
mentionedMembers group content rows = do
 let members = filter ((`elem` items group "members") . (`get` "id")) rows
     word c = isAlphaNum c || c == '_'
     mentioned name = any (\(before,remaining) ->
       (T.null before || not (word (T.last before) || T.last before == '@')) &&
       let after = T.drop (T.length name + 1) remaining
       in T.null after || not (word (T.head after) || T.head after `elem` (":#+|()&$^-" :: String) || (T.head after == '.' && T.length after > 1 && word (T.index after 1)))) (T.breakOnAll ("@" <> name) content)
 found <- filterM (\row -> if mentioned (at row "name") then do
   check (length (filter ((== at row "name") . (`at` "name")) members) == 1) 400 "Ambiguous mention; address the member by target ID"
   pure True
  else pure False) members
 pure (map (`at` "id") found)
findRequest :: Service -> RunId -> IO (Maybe (Value,GroupRequest))
findRequest s rid = do
 groups <- DB.values s DB.Groups
 requests <- forM groups $ \g -> map (g,) <$> readRequests g
 pure (listToMaybe [(g,r) | (g,r) <- concat requests,requestId r == rid])
activeRoot :: Service -> Text -> Text -> IO Text
activeRoot s gid actor = if actor == "admin" then pure "" else do
 runs <- Map.elems <$> readMVar (serviceRuns s)
 requests <- mapM (findRequest s . runId) (filter ((== AgentId actor) . runOwner) runs)
 pure (fromMaybe "" (listToMaybe [at (toJSON r) "root" | Just (g,r) <- requests,at g "id" == gid]))
submitGroup :: Service -> Text -> Value -> Text -> IO Value
submitGroup s gid input actor = do
 group <- getGroup s gid True; authorizeGroup s group actor Member
 require (all (`elem` ["text","target","attachments"]) (keys input)) 400 "Group messages accept text and optional target"
 content <- either (failure 400) pure (messageText (get input "text"))
 attachments <- Workspace.resolveAttachments s gid (get input "attachments" `fallback` arr []) True
 require (not (T.null content) || not (null attachments)) 400 "Write a message or attach a file"
 routine <- if actor == "admin" then Routine.command s gid content (not (null attachments)) else pure Nothing
 case routine of
  Just r -> do
   entry <- message actor content; acknowledgment <- message (at group "lead") (Routine.receipt r content)
   void (saveGroup s (append "messages" acknowledgment (append "messages" entry group)) actor "routine saved" (get r "id"))
   pure entry
  Nothing -> do
   targets <- if has input "target" then pure [at input "target"] else mentions s group content
   require (all ((`elem` items group "members") . str) targets) 400 "Address only current Group members"
   initial <- message actor content
   let entry = merge initial (obj ([("attachments",arr attachments)] ++ [("memo_pending",Bool True) | actor == "admin"]))
       addressed = if actor == "admin" && null targets then [at group "lead"] else targets
   root <- activeRoot s gid actor
   calls <- mapM (\target -> newRequest entry target root) (filter (/= actor) (nub addressed))
   void (saveGroup s (put "requests" (arr (items group "requests" ++ calls)) (append "messages" entry group)) actor "message" (get entry "id"))
   when (actor == "admin") $ DB.forgetDrafts s gid attachments
   dispatchGroups s
   pure entry

dispatchGroups :: Service -> IO ()
dispatchGroups s = do
 stopping <- readIORef (serviceStopping s)
 unless stopping $ DB.values s DB.Groups >>= mapM_ dispatch
 where
 dispatch group = unless (boolean (get group "archived")) $ readRequests group >>= mapM_ (dispatchRequest group)
 dispatchRequest group typed = when (requestStatus typed == Queued) $ do
  let request=toJSON typed
  let target = if boolean (get request "memo") then at group "lead" else at request "target"
      actual = toJSON typed { requestTarget=AgentId target }
  when (target /= at request "target") $ do
   latest <- getGroup s (at group "id") False
   storeGroup s (replace "requests" actual latest)
  projected <- DB.turns s
  state <- agentState s target
  runs <- Map.elems <$> readMVar (serviceRuns s)
  workloads <- DB.values s DB.Workloads
  when (requestReady (AgentId target) (requestId typed) (map runOwner runs) state workloads projected) $ do
   entry <- maybe (failure 409 "Group request message is missing") pure (findId (at request "message") (items group "messages"))
   suffix <- Workspace.attachmentPrompt s (items entry "attachments")
   t <- now
   let origin = get request "origin" `fallback` obj [("group",get group "id"),("author",get entry "author")]
       content = at entry "text" <> suffix
       turn = toJSON (newTurn (requestId typed) Chat content t origin (has origin "routine"))
   modifyState s target $ \current -> append "turns" turn current
   DB.project s target

requestReady :: AgentId -> RunId -> [AgentId] -> AgentState -> [Value] -> [Value] -> Bool
requestReady target identity running state workloads projected =
 target `notElem` running &&
 not (any ((`elem` [Queued,Running]) . turnStatus) (agentTurns state)) &&
 not (any (\call -> at call "addressedTo" == agentIdText target && at call "state" == "Queued") (concatMap (`items` "calls") workloads)) &&
 isNothing (findId (runIdText identity) projected)

runGroup :: Service -> Run -> Turn -> IO (Text,[Value])
runGroup s run turn = do
 (context,isMemo) <- locked s $ do
  found <- findRequest s (turnId turn)
  (group,typed) <- maybe (failure 409 "Unknown Group call") pure found
  let request=toJSON typed
  require (not (boolean (get group "archived")) && str (agentIdText (runOwner run)) `elem` items group "members") 409 "Group is archived or membership changed before execution"
  let running = toJSON typed { requestStatus=Running }
  storeGroup s (replace "requests" running group)
  root <- folder s (at group "id")
  memo <- (get <$> readMemo root <*> pure "content") `catch` (\(e::SomeException) -> pure (obj [("path",str (T.pack (root </> "Notes.html"))),("error",str (T.pack (displayException e)))]))
  rows <- DB.agents s; chief <- DB.mainAgent s
  let isMemo = boolean (get request "memo")
  manifests <- forM ["identity","host-control","computer-use"] $ \name -> do
   let path = agentRoot s (agentIdText (runOwner run)) </> "manifests" </> T.unpack name <.> "md"
   exists <- doesFileExist path
   value <- if exists then str <$> readText path else pure Null
   pure (name,value)
  let manifest name = fromMaybe Null (lookup name manifests)
  pure (obj [("group",obj ([(k,get group k) | k <- ["id","name","description","lead","members"]] ++ [("workspace",str (T.pack root))])),("self_id",str (agentIdText (runOwner run))),("chief_id",str chief),("members",arr [r | r <- rows,get r "id" `elem` items group "members"]),("tasks",arr [select ["id","title","assignee","state","revision","deleted"] task | task <- takeLast 50 (items group "tasks")]),("messages",arr (if isMemo then items group "messages" else takeLast 30 (items group "messages"))),("memo",memo),("request",running),("scheduledExecutionPrompt",get (turnField turn "origin") "executionPrompt"),("identity",manifest "identity"),("host_control",manifest "host-control"),("computer",manifest "computer-use")],isMemo)
 template <- prompt s (if isMemo then "group-memo" else "group-conversation")
 extra <- if isMemo then ("\n\n" <>) . (`render` [("notes_example",T.pack (repositoryRoot s </> "corpora/prompts/examples/jarvis-notes.html"))]) <$> prompt s "notes-wiki" else pure ""
 prefix <- if has (turnField turn "origin") "routine" then (<> "\n\n") <$> prompt s "scheduled-execution" else pure ""
 let renderContext ctx = prefix <> render template [("context",jsonText ctx),("task",turnInput turn)] <> extra
     fit ctx = let rendered = renderContext ctx in if T.length rendered <= 60000 || null (items ctx "messages") then rendered else fit (put "history_truncated" (Bool True) (put "messages" (arr (drop 1 (items ctx "messages"))) ctx))
 let rendered = fit context
 outcome <- try (invoke s run rendered)
 activity <- readIORef (runActivity run)
 let record = obj ([("role",str (if isMemo then "group-memo" else "group")),("prompt",str rendered),("session",get activity "session")] ++ [("warning",get activity "warning") | has activity "warning"])
 case outcome of
  Right result -> pure (result,[put "answer" (str result) record])
  Left (error :: SomeException) -> throwIO (RunFailure (case fromException error of Just (APIError _ reason) -> reason; Nothing -> T.pack (displayException error)) [record])
 where takeLast count values = drop (length values-count) values

data GroupSettlement
 = KeepGroup Value
 | SettleArchived Value Value
 | SettleMemo Value Value Value
 | SettleMessage Value Value Value
settlementPlan :: [Value] -> Value -> GroupRequest -> Validation GroupSettlement
settlementPlan projected group initial = case findId (at request "id") projected of
 Nothing
  | requestStatus initial == Cancelled && not (requestSettled initial) -> Right (SettleArchived group request)
  | otherwise -> Right (KeepGroup group)
 Just turn -> do
  status <- validateValue 500 parseJSON (get turn "status")
  let current = toJSON initial { requestStatus=status }
      changed = replace "requests" current group
      settled = merge current (obj ([("settled",Bool True)] ++ [("error",get turn "error") | get turn "error" /= Null]))
      base = replace "requests" settled changed
  pure $ if activeStatus status || requestSettled initial then KeepGroup changed
   else if boolean (get request "memo") then SettleMemo base request turn
   else SettleMessage base request turn
 where request = fromMaybe (toJSON initial) (findId (runIdText (requestId initial)) (items group "requests"))

forwardingTargets :: Value -> Value -> [Text] -> Validation [Text]
forwardingTargets group request mentioned
 | routine && not (null targets) = do
    task <- maybe (Left (APIError 404 "Unknown Group task")) Right (findId (at request "task") (items group "tasks"))
    pure (if not (boolean (get task "deleted")) && get task "revision" == get request "task_revision" then take 1 targets else [])
 | otherwise = Right targets
 where
 routine = has (get request "origin") "routine"
 called = [at r "target" | r <- items group "requests",get r "root" == get request "root"]
 targets = filter (`notElem` called) mentioned

memoReady :: Value -> Value -> Bool
memoReady group entry = boolean (get entry "memo_pending") && not (null related) && all (boolean . (`get` "settled")) related
 where related = filter ((==get entry "id") . (`get` "root")) (items group "requests")

reconcileGroups :: Service -> IO ()
reconcileGroups s = DB.values s DB.Groups >>= mapM_ reconcile
 where
 reconcile original = do
  projected <- DB.turns s
  requests <- readRequests original
  updated <- foldM (settle projected) original requests
  withMemos <- if boolean (get updated "archived") then pure updated else foldM addMemo updated (items updated "messages")
  when (withMemos /= original) $ do
   t <- now
   storeGroup s (merge withMemos (obj [("revision",num (int (get withMemos "revision")+1)),("updated",str t)]))
 settle projected group initial = do
  plan <- validated (settlementPlan projected group initial)
  case plan of
   KeepGroup changed -> pure changed
   SettleArchived base request -> do
    (changed,_) <- settleTask base request (obj [("status",str "cancelled"),("error",str "Group archived")])
    pure (replace "requests" (put "settled" (Bool True) request) changed)
   SettleMemo base request turn -> do
    event <- eventRecord (at request "target") ("memo " <> at turn "status") (get request "id")
    pure (append "events" event base)
   SettleMessage base request turn -> do
    let routine = has (get request "origin") "routine"
    mentioned <- if get request "target" == get group "lead" && not (boolean (get group "archived")) && not (T.null (at turn "output")) then mentions s group (at turn "output") `catch` (\(_::APIError) -> pure []) else pure []
    recipients <- validated (forwardingTargets group request mentioned)
    (finished,stale) <- if routine && not (null recipients) then pure (base,TaskForwarded) else settleTask base request turn
    output <- if get turn "output" == Null then pure finished else do
     initialMessage <- message (at request "target") (at turn "output")
     let entry = merge initialMessage (obj [("turn",get request "id"),("stale",Bool (stale == StaleResultRecorded))])
     foldM (\g target -> do
       initialCall <- newRequest entry target (at request "root")
       if routine then do
        task <- findTask g (at request "task")
        let revised = merge task (obj [("assignee",str target),("revision",num (int (get task "revision")+1))])
            call = merge initialCall (obj [("origin",get request "origin"),("task",get task "id"),("task_revision",get revised "revision")])
        pure (append "requests" call (replace "tasks" revised g))
       else pure (append "requests" initialCall g)) (append "messages" entry finished) recipients
    event <- eventRecord (at request "target") (if stale == StaleResultRecorded then "result needs review" else "call " <> at turn "status") (get request "id")
    pure (append "events" event output)
 addMemo group entry
  | not (memoReady group entry) = pure group
  | otherwise = do
   initial <- newRequest entry (at group "lead") ""
   let request = merge initial (obj [("memo",Bool True),("origin",obj [("group",get group "id"),("author",str "system"),("memo",Bool True)])])
   pure (append "requests" request (replace "messages" (put "memo_pending" (Bool False) entry) group))

actionGroup :: Service -> Text -> Text -> Text -> IO Value
actionGroup s gid rid action = do
 group <- getGroup s gid (action == "retry")
 requests <- readRequests group
 typed <- maybe (failure 404 "Unknown Group call") pure (find ((==RunId rid) . requestId) requests)
 let request=toJSON typed
 turn <- findId rid <$> DB.turns s
 case turn of
  Nothing | action == "cancel" -> void (saveGroup s (replace "requests" (put "status" (str "cancelled") request) group) "admin" "call cancelled" (str rid))
  Nothing -> failure 400 "Only failed or interrupted calls can be retried"
  Just value -> do
   let owner = at request "target"
   Population.requireActive s owner
   status <- field 500 value "status"
   if action == "retry" then do
    require (get request "target" `elem` items group "members") 409 "The recipient is no longer a Group member"
    require (not (has request "task")) 409 "Start the current task revision from Work instead"
    turnResult (validateTurnAction Retry status)
    modifyTurns s owner (\t -> if turnId t == RunId rid then retryTurn t else t)
    let updated = replace "requests" (merge (remove ["error"] request) (obj [("settled",Bool False),("status",str "queued")])) group
        messages = [if get m "id" == get request "root" && at m "author" == "admin" && not (boolean (get request "memo")) then put "memo_pending" (Bool True) m else m | m <- items updated "messages"]
    void (saveGroup s (put "messages" (arr messages) updated) "admin" "call retried" (str rid))
   else if action == "cancel" then do
    turnResult (validateTurnAction Cancel status)
    runs <- readMVar (serviceRuns s)
    case Map.lookup (RunId rid) runs of
     Just run -> writeIORef (runCancel run) True
     Nothing -> modifyTurns s owner (\t -> if turnId t == RunId rid then t { turnStatus = Cancelled } else t)
    void (saveGroup s group "admin" "call cancelled" (str rid))
   else failure 404 "Unknown turn action"
   DB.project s owner
 reconcileGroups s; dispatchGroups s
 pure (obj [("saved",Bool True)])
