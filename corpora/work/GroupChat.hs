{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module GroupChat where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.Aeson hiding (pairs)
import Data.Aeson.Types (parseEither)
import qualified Data.Aeson.KeyMap as KM
import Data.Char
import Data.IORef
import Data.List (find,nub)
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
activeRoot :: Service -> Text -> Text -> IO (Maybe MessageId)
activeRoot s gid actor = if actor == "admin" then pure Nothing else do
 runs <- Map.elems <$> readMVar (serviceRuns s)
 requests <- mapM (findRequest s . runId) (filter ((== AgentId actor) . runOwner) runs)
 roots <- forM [(g,r) | Just (g,r) <- requests,at g "id" == gid] $ \(_,request) -> requestRoot <$> requireRequestLineage request
 pure (listToMaybe roots)

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
   latest <- getGroup s gid True
   entry <- message actor content; acknowledgment <- message (at latest "lead") (Routine.receipt r content)
   void (saveGroup s (append "messages" acknowledgment (append "messages" entry latest)) actor "routine saved" (get r "id"))
   pure entry
  Nothing -> do
   targets <- if has input "target" then pure [at input "target"] else mentions s group content
   require (all ((`elem` items group "members") . str) targets) 400 "Address only current Group members"
   initial <- message actor content
   let entry = merge initial (obj ([("attachments",arr attachments)] ++ [("memo_pending",Bool True) | actor == "admin"]))
       addressed = if actor == "admin" && null targets then [at group "lead"] else targets
   root <- activeRoot s gid actor
   calls <- mapM (\target -> newRequest entry (AgentId target) root) (filter (/= actor) (nub addressed))
   void (saveGroup s (put "requests" (arr (items group "requests" ++ map toJSON calls)) (append "messages" entry group)) actor "message" (get entry "id"))
   dispatchGroups s
   pure entry

checkedOperationalOrigin :: StoredOrigin -> Either Text ExecutionOrigin
checkedOperationalOrigin stored = do
 origin <- checkedOrigin stored
 case originPurpose origin of
  ScheduledExecution scheduled -> case scheduledOwner scheduled of
   GroupOwner group -> unless (originGroup origin == Just group) (Left "Scheduled execution Group context does not match its routine owner")
   PersonalOwner _ -> Right ()
  OrdinaryWork -> Right ()
  RoutinePolicySetup _ -> Right ()
 pure origin

checkedRequestOrigin :: Value -> GroupRequest -> Either Text ExecutionOrigin
checkedRequestOrigin group request = do
 lineage <- checkedRequestLineage request
 unless (isJust (findId (messageIdText (requestMessage lineage)) (items group "messages"))) (Left "Group request message is missing")
 unless (isJust (findId (messageIdText (requestRoot lineage)) (items group "messages"))) (Left "Group request root message is missing")
 forM_ (requestTask lineage) $ \link ->
  unless (isJust (findId (taskIdText (requestTaskId link)) (items group "tasks"))) (Left "Unknown Group task")
 expected <- either (Left . T.pack) Right (parseEither parseJSON (get group "id"))
 origin <- checkedOperationalOrigin (requestOrigin request)
 case originPurpose origin of
  ScheduledExecution _ -> do
   link <- maybe (Left "Scheduled Group request has no task revision") Right (requestTask lineage)
   raw <- maybe (Left "Unknown Group task") Right (findId (taskIdText (requestTaskId link)) (items group "tasks"))
   task <- either (Left . T.pack) Right (parseEither parseJSON raw)
   validateTaskOrigin task origin
  OrdinaryWork -> Right ()
  RoutinePolicySetup _ -> Right ()
 if toJSON (requestOrigin request) == Null then do
  let entry=findId (messageIdText (requestMessage lineage)) (items group "messages")
      author=maybe Null (`get` "author") entry
  pure (newOrigin OrdinaryWork (Just expected) (KM.singleton "author" author))
 else do
  case originGroup origin of
   Just actual | actual /= expected -> Left "Saved execution origin belongs to another Group"
   Nothing -> case originPurpose origin of
    ScheduledExecution _ -> Left "Scheduled Group request is missing its Group context"
    RoutinePolicySetup _ -> Left "Group policy setup is missing its Group context"
    OrdinaryWork -> Right ()
   Just _ -> Right ()
  pure origin

type GroupRequestIndex = Map.Map RunId (Value,GroupRequest)

loadGroupRequestIndex :: Service -> IO GroupRequestIndex
loadGroupRequestIndex s = do
 groups <- DB.values s DB.Groups
 requests <- forM groups $ \group -> do
  records <- readRequests group
  pure [(requestId request,(group,request)) | request <- records]
 pure (Map.fromListWith (\_ previous -> previous) (concat requests))

checkedTurnContext :: GroupRequestIndex -> Turn -> Either Text ExecutionOrigin
checkedTurnContext requests turn = checkedOriginContext requests (turnId turn) (turnOrigin turn)

checkedOriginContext :: GroupRequestIndex -> RunId -> StoredOrigin -> Either Text ExecutionOrigin
checkedOriginContext requests identity stored = do
 actual <- checkedOperationalOrigin stored
 case Map.lookup identity requests of
  Nothing -> Right actual
  Just (group,request) -> do
   expected <- checkedRequestOrigin group request
   unless (originPurpose actual == originPurpose expected) (Left "Saved execution purpose does not match its Group request")
   unless (originGroup actual == originGroup expected) (Left "Saved execution context does not match its Group request")
   Right actual

requestExecutionOrigin :: Value -> GroupRequest -> IO ExecutionOrigin
requestExecutionOrigin group = either (failure 500) pure . checkedRequestOrigin group

rejectGroupOrigin :: Value -> GroupRequest -> Text -> IO Value
rejectGroupOrigin group request reason = do
 let failed=request { requestStatus=Failed, requestSettled=True }
 result <- try (settleTask group failed (obj [("status",str "failed"),("error",str reason)]))
 let (changed,detail)=case result of
      Right (updated,_) -> (updated,reason)
      Left (APIError _ problem) -> (group,reason <> "; Task settlement is blocked: " <> problem)
     saved=failed { requestFields=KM.insert "error" (str detail) (requestFields failed) }
 pure (replace "requests" (toJSON saved) changed)

dispatchGroups :: Service -> IO ()
dispatchGroups s = do
 stopping <- readIORef (serviceStopping s)
 unless stopping $ DB.values s DB.Groups >>= mapM_ dispatch
 where
 dispatch group = unless (boolean (get group "archived")) $ readRequests group >>= mapM_ (dispatchRequest group)
 dispatchRequest group typed = when (requestStatus typed == Queued) $ case checkedRequestOrigin group typed of
  Left reason -> do
   latest <- getGroup s (at group "id") False
   changed <- rejectGroupOrigin latest typed reason
   void (saveGroup s changed "system" "call failed" (toJSON (requestId typed)))
  Right _ -> dispatchValid group typed
 dispatchValid group typed = do
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
   lineage <- requireRequestLineage typed
   entry <- maybe (failure 409 "Group request message is missing") pure (findId (messageIdText (requestMessage lineage)) (items group "messages"))
   suffix <- Workspace.attachmentPrompt s (items entry "attachments")
   t <- now
   origin <- requestExecutionOrigin group typed
   let content = at entry "text" <> suffix
       turn = toJSON (newTurn (requestId typed) Chat content t (toJSON origin) (originIsScheduled origin))
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
 parsed <- requireExecutionOrigin (turnOrigin turn)
 (context,isMemo) <- locked s $ do
  found <- findRequest s (turnId turn)
  (group,typed) <- maybe (failure 409 "Unknown Group call") pure found
  void (requestExecutionOrigin group typed)
  let request=toJSON typed
  require (requestTarget typed == runOwner run && not (boolean (get group "archived")) && str (agentIdText (runOwner run)) `elem` items group "members") 409 "Group is archived or membership changed before execution"
  when (isJust (originPolicyRoutine parsed)) $
   require (toJSON (runOwner run) == get group "lead") 409 "Routine policy setup belongs to the current Group Lead"
  specification <- if originIsScheduled parsed then do
    lineage <- requireRequestLineage typed
    link <- maybe (failure 500 "Scheduled Group request has no task revision") pure (requestTask lineage)
    GroupTask { specification=saved } <- readTask group (taskIdText (requestTaskId link))
    pure (maybe Null toJSON saved)
   else pure Null
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
  pure (obj [("group",obj ([(k,get group k) | k <- ["id","name","description","lead","members"]] ++ [("workspace",str (T.pack root))])),("self_id",str (agentIdText (runOwner run))),("chief_id",str chief),("members",arr [r | r <- rows,get r "id" `elem` items group "members"]),("tasks",arr [select ["id","title","assignee","state","revision","deleted"] task | task <- takeLast 50 (items group "tasks")]),("messages",arr (if isMemo then items group "messages" else takeLast 30 (items group "messages"))),("memo",memo),("request",running),("scheduledExecutionPrompt",get (turnField turn "origin") "executionPrompt"),("scheduledTaskSpecification",specification),("identity",manifest "identity"),("host_control",manifest "host-control"),("computer",manifest "computer-use")],isMemo)
 template <- prompt s (if isMemo then "group-memo" else "group-conversation")
 extra <- if isMemo then ("\n\n" <>) . (`render` [("notes_example",T.pack (repositoryRoot s </> "corpora/prompts/examples/jarvis-notes.html"))]) <$> prompt s "notes-wiki" else pure ""
 prefix <- if originIsScheduled parsed then (<> "\n\n") <$> prompt s "scheduled-execution" else pure ""
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
 | SettleArchived Value GroupRequest
 | SettleMemo Value GroupRequest Value
 | SettleMessage Value GroupRequest Value
settlementPlan :: [Value] -> Value -> GroupRequest -> Validation GroupSettlement
settlementPlan projected group initial = do
 request <- maybe (Right initial) (validateValue 500 parseJSON) (findId (runIdText (requestId initial)) (items group "requests"))
 case findId (runIdText (requestId request)) projected of
  Nothing
   | requestStatus initial == Cancelled && not (requestSettled initial) -> Right (SettleArchived group request)
   | otherwise -> Right (KeepGroup group)
  Just turn -> do
   status <- validateValue 500 parseJSON (get turn "status")
   let current=request { requestStatus=status }
       changed=replace "requests" (toJSON current) group
       settled=current { requestSettled=True, requestFields=KM.fromList [("error",get turn "error") | get turn "error" /= Null] <> requestFields current }
       base=replace "requests" (toJSON settled) changed
   pure $ if activeStatus status || requestSettled initial then KeepGroup changed
    else if boolean (get (Object (requestFields request)) "memo") then SettleMemo base request turn
    else SettleMessage base request turn

forwardingTargets :: ExecutionOrigin -> Value -> GroupRequest -> [Text] -> Validation [Text]
forwardingTargets origin group request mentioned = do
 lineage <- either (Left . APIError 500) Right (checkedRequestLineage request)
 requests <- validateValue 500 parseJSON (get group "requests")
 related <- requestsForRoot (requestRoot lineage) requests
 let called=map (agentIdText . requestTarget) related
     targets=filter (`notElem` called) mentioned
 if originIsScheduled origin && not (null targets) then do
  link <- maybe (Left (APIError 500 "Scheduled Group request has no task revision")) Right (requestTask lineage)
  value <- maybe (Left (APIError 404 "Unknown Group task")) Right (findId (taskIdText (requestTaskId link)) (items group "tasks"))
  task <- validateValue 500 parseJSON value
  pure (if currentTaskRevision link task then take 1 targets else [])
 else pure targets

memoReady :: [GroupRequest] -> MessageId -> Bool
memoReady requests identity = not (any (invalidLineageReferences identity) requests) && not (null related) && all requestSettled related
 where related=[request | request <- requests, ValidLineage lineage <- [requestLineage request], requestRoot lineage == identity]

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
 settle projected group initial = case checkedRequestOrigin group initial of
  Left reason | not (requestSettled initial) -> rejectGroupOrigin group initial reason
  Left _ -> pure group
  Right origin -> settleValid projected group initial origin
 settleValid projected group initial origin = do
  plan <- validated (settlementPlan projected group initial)
  case plan of
   KeepGroup changed -> pure changed
   SettleArchived base request -> do
    (changed,_) <- settleTask base request (obj [("status",str "cancelled"),("error",str "Group archived")])
    pure (replace "requests" (toJSON request { requestSettled=True }) changed)
   SettleMemo base request turn -> do
    event <- eventRecord (agentIdText (requestTarget request)) ("memo " <> at turn "status") (toJSON (requestId request))
    pure (append "events" event base)
   SettleMessage base request turn -> do
    mentioned <- if toJSON (requestTarget request) == get group "lead" && not (boolean (get group "archived")) && not (T.null (at turn "output")) then mentions s group (at turn "output") `catch` (\(_::APIError) -> pure []) else pure []
    case forwardingTargets origin group request mentioned of
     Left (APIError _ reason) -> rejectGroupOrigin base request reason
     Right recipients -> settleRecipients origin base request turn recipients
 settleRecipients origin base request turn recipients = do
    let routine=originIsScheduled origin
    (finished,stale) <- if routine && not (null recipients) then pure (base,TaskForwarded) else settleTask base request turn
    output <- if get turn "output" == Null then pure finished else do
     lineage <- requireRequestLineage request
     initialMessage <- message (agentIdText (requestTarget request)) (at turn "output")
     let entry = merge initialMessage (obj [("turn",toJSON (requestId request)),("stale",Bool (stale == StaleResultRecorded))])
     foldM (\g target -> do
       initialCall <- newRequest entry (AgentId target) (Just (requestRoot lineage))
       if routine then do
        link <- maybe (failure 500 "Scheduled Group request has no task revision") pure (requestTask lineage)
        task <- readTask g (taskIdText (requestTaskId link))
        childLineage <- requireRequestLineage initialCall
        let revised=forwardTask (AgentId target) task
            call=initialCall { requestOrigin=requestOrigin request, requestLineage=ValidLineage childLineage { requestTask=Just (taskLink revised) } }
        pure (append "requests" (toJSON call) (replace "tasks" (toJSON revised) g))
       else pure (append "requests" (toJSON initialCall) g)) (append "messages" entry finished) recipients
    event <- eventRecord (agentIdText (requestTarget request)) (if stale == StaleResultRecorded then "result needs review" else "call " <> at turn "status") (toJSON (requestId request))
    pure (append "events" event output)
 addMemo group entry
  | not (boolean (get entry "memo_pending")) = pure group
  | otherwise = case parseEither parseJSON (get entry "id") of
     Left _ -> pure group
     Right identity -> do
      requests <- readRequests group
      if not (memoReady requests identity) then pure group else do
       lead <- field 500 group "lead"
       initial <- newRequest entry lead Nothing
       groupId <- field 500 group "id"
       let origin=newOrigin OrdinaryWork (Just groupId) (KM.fromList [("author",str "system"),("memo",Bool True)])
           request=initial { requestOrigin=ValidOrigin origin, requestFields=KM.insert "memo" (Bool True) (requestFields initial) }
       pure (append "requests" (toJSON request) (replace "messages" (put "memo_pending" (Bool False) entry) group))

policySetupRetry :: AgentId -> [GroupRequest] -> GroupRequest -> Validation ()
policySetupRetry lead requests typed = do
 lineage <- either (Left . APIError 500) Right (checkedRequestLineage typed)
 origin <- either (Left . APIError 500) Right (checkedOrigin (requestOrigin typed))
 forM_ (originPolicyRoutine origin) $ \identity -> do
  check (requestTarget typed == lead) 409 "Routine policy setup belongs to the current Group Lead"
  related <- requestsForRoot (requestRoot lineage) requests
  origins <- traverse (\candidate -> (candidate,) <$> either (Left . APIError 500) Right (checkedOrigin (requestOrigin candidate))) related
  let latest=listToMaybe (reverse [candidate | (candidate,parsed) <- origins, originPolicyRoutine parsed == Just identity])
  check (fmap requestId latest == Just (requestId typed))
   409 "This policy setup was superseded; use the current setup request"

checkPolicyRetry :: Service -> Turn -> IO ()
checkPolicyRetry s turn = do
 origin <- requireExecutionOrigin (turnOrigin turn)
 forM_ ((,) <$> originGroup origin <*> originPolicyRoutine origin) $ \(groupId,_) -> do
  groupValue <- getGroup s (groupIdText groupId) True
  requests <- readRequests groupValue
  request <- maybe (failure 409 "Policy setup request is missing") pure
   (find ((==turnId turn) . requestId) requests)
  void (requestExecutionOrigin groupValue request)
  require (toJSON (requestOrigin request) == toJSON origin) 409 "Policy setup request origin changed"
  lead <- field 500 groupValue "lead"
  validated (policySetupRetry lead requests request)

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
    void (requestExecutionOrigin group typed)
    require (get request "target" `elem` items group "members") 409 "The recipient is no longer a Group member"
    lead <- field 500 group "lead"
    validated (policySetupRetry lead requests typed)
    lineage <- requireRequestLineage typed
    require (isNothing (requestTask lineage)) 409 "Start the current task revision from Work instead"
    turnResult (validateTurnAction Retry status)
    state <- agentState s owner
    turn <- case find ((==RunId rid) . turnId) (agentTurns state) of
     Just active -> pure active
     Nothing -> do
      origin <- decodeExecutionOrigin (get value "origin")
      require (at value "agent" == owner && isJust (originPolicyRoutine origin)) 409 "Group call is no longer in the active conversation"
      archived <- retainedTurn s owner (RunId rid) >>= maybe (failure 409 "Retained setup turn is unavailable; ask the Group Lead to finish routine setup in chat") pure
      require (turnStatus archived == status && isJust (turnAttempt archived) && turnField archived "origin" == get value "origin") 409 "Retained setup turn does not match its saved history; ask the Group Lead to finish routine setup in chat"
      pure archived
    void (either (failure 500) pure (checkedTurnContext (Map.singleton (requestId typed) (group,typed)) turn))
    let restored=if any ((==turnId turn) . turnId) (agentTurns state) then state else state { agentTurns=agentTurns state++[turn] }
    saveState s owner (mapTurns (\t -> if turnId t == RunId rid then retryTurn t else t) restored)
    let updated = replace "requests" (merge (remove ["error"] request) (obj [("settled",Bool False),("status",str "queued")])) group
        messages = [if get m "id" == toJSON (requestRoot lineage) && at m "author" == "admin" && not (boolean (get request "memo")) then put "memo_pending" (Bool True) m else m | m <- items updated "messages"]
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
