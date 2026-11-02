{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Management where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.Aeson hiding (pairs)
import Data.IORef
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import System.Environment
import System.FilePath
import Types
import qualified Store as DB
import Population
import Workspace
import Memo
import qualified Routines as R
import qualified Delegation as D
import qualified Groups as G

activeTurn :: Service -> Text -> Maybe Text -> IO (Maybe Text)
activeTurn s a specific = do
 state <- agentState s a
 candidates <- filterM (D.callAllowed s) [t | t<-agentTurns state,turnStatus t==Running,maybe True ((==turnId t) . RunId) specific]
 pure $ runIdText . turnId <$> listToMaybe candidates
status :: Service -> Text -> IO Value
status s a = do
 chief <- DB.mainAgent s
 members <- team s
 retiredMembers <- retiredTeam s
 groups <- G.groupFacts s a
 ws <- workspace s a
 notes <- memoMetadata (workspaceRoot s a)
 pure $ obj [("self_id",str a),("main_agent_id",str chief),("team",arr members),("retired_team",arr retiredMembers),("groups",arr groups),("workspace",ws),("notes",notes)]
prepare :: Service -> Text -> IO ()
prepare s a = do
 endpoint <- readIORef (serviceEndpoint s)
 unless (T.null endpoint) $ do
  let path=workspaceRoot s a </> "host-control.json"
  writeJSON path (obj [("url",str (endpoint<>"/api/agents/"<>a<>"/control"))])
  executable <- getExecutablePath
  template <- prompt s "host-control"
  writeText (agentRoot s a </> "manifests/host-control.md") (render template [("command",shellCommand executable <> " control " <> shellQuote path)])
 facts <- status s a
 notes <- memoContext (workspaceRoot s a)
 writeText (agentRoot s a </> "manifests/host-facts.md") (jsonText (put "notes" notes facts))
 let context= obj [("currentSapi",str a),("chief",get facts "main_agent_id"),("groups",get facts "groups"),("team",get facts "team"),("workspace",obj [("path",str (T.pack (workspaceRoot s a)))])]
 writeText (agentRoot s a </> "manifests/corpora-state.md") (jsonText context)
operationFields :: [(Text,[Text])]
operationFields = [("corpora_get",[]),("corpora_update",["name","device","scope","revision","archived"]),("batch",["operations"]),("status",["target"]),("delegate",["decision"]),("routines",[]),("routine_create",["minutes","prompt"]),("routine_update",["id","minutes","prompt"]),("routine_pause",["id"]),("routine_resume",["id"]),("group_create",["name","description","lead","members"]),("group_get",["group"]),("group_update",["group","revision","name","description","lead","members","archived"]),("group_message",["group","text","target"]),("group_task_create",["group","title","body","assignee"]),("group_task_update",["group","task","revision","title","body","assignee","state","deleted"]),("group_task_run",["group","task","revision"]),("create_agent",["name","role","manager"]),("retire_agent",["target","reason"]),("rehire_agent",["target"]),("computer_acquire",[]),("manager",["manager","target"]),("workspace",["group"]),("workspace_open",["url","path","id","group"]),("workspace_zoom",["id","factor","group"])] ++ [("workspace_"<>action,["id","group"]) | action<- ["close","focus","reload","back","forward","bookmark","unbookmark"]]
control :: Service -> Text -> Value -> IO Value
control s a payload = do
 void (DB.agent s a)
 let d=remove ["agencyRun"] payload; op=at d "op"; specific=if has payload "agencyRun" then Just (at payload "agencyRun") else Nothing
 current <- activeTurn s a specific
 when (isJust specific) $ require (isJust current) 409 "AgencyRun is no longer active"
 fields <- maybe (failure 400 "Unknown operation or field") pure (lookup op operationFields)
 require (all (`elem` ("op":fields)) (keys d)) 400 "Unknown operation or field"
 unless (op `elem` ["status","workspace"] || "workspace_" `T.isPrefixOf` op) $ requireActive s a
 chief <- DB.mainAgent s
 let receipt v=merge (obj [("self_id",str a),("saved",Bool True)]) v
 if "group_" `T.isPrefixOf` op then G.groupControl s a d else case op of
  "batch" -> do
   let operations=items d "operations"
   require (not (null operations) && length operations<=20) 400 "Provide 1–20 operations"
   forM_ operations $ \item -> require (at item "op" /= "batch" && maybe False (\fs -> all (`elem` ("op":fs)) (keys item)) (lookup (at item "op") operationFields)) 400 "Invalid batch operation or field; nested batches are not supported"
   let execute index results []=pure (receipt (obj [("results",arr results)]))
       execute index results (item:rest)=do
        result <- try (control s a item)
        case result of
         Right v -> execute (index+1) (results++[v]) rest
         Left (APIError code err) -> pure (obj [("self_id",str a),("saved",Bool False),("results",arr results),("failed_index",num index),("error",str err),("status",num code),("partial",Bool (not (null results)))])
   execute 0 [] operations
  "corpora_get" -> require (a==chief) 403 "Only ChiefSapi can change corpora settings" >> corporaIdentity s
  "corpora_update" -> require (a==chief) 403 "Only ChiefSapi can change corpora settings" >> updateCorpora s (remove ["op"] d)
  "status" -> (if has d "target" then resolve s (at d "target") True else pure a) >>= status s
  "create_agent" -> do
   require (a==chief) 403 "Only the main Sapiens can create agents through host-control"
   name <- validName d
   role <- textField d "role" 60
   parent <- if get d "manager"==Null then pure a else resolve s (at d "manager") False
   rows <- DB.agents s
   let matches=filter ((==T.toCaseFold name) . T.toCaseFold . (`at` "name")) rows
   (row,created) <- if null matches then (,True) <$> createAgent s (obj [("name",str name),("role",str role),("manager",str parent)]) else do
    row <- case matches of
     [candidate] | at candidate "role"==role && at candidate "id"/=a -> pure candidate
     _ -> failure 409 "Name already exists with a different role; use status to inspect it"
    let child=at row "id"
    requireActive s child
    dir <- directory s
    require (at (get dir child) "parent"==parent) 409 "Name already exists with a different manager"
    pure (row,False)
   prepare s (at row "id")
   pure (receipt (obj [("agent",row),("created",Bool created)]))
  "retire_agent" -> change chief receipt True
  "rehire_agent" -> change chief receipt False
  "manager" -> do
   require (has d "manager") 400 "manager is required; use null to clear it"
   target <- if has d "target" then resolve s (at d "target") False else pure a
   parent <- validateManager s target (get d "manager")
   assignManager s target parent
   pure (receipt (obj [("target",str target),("manager",parent)]))
  "computer_acquire" -> do
   identity <- maybe (failure 409 "Computer access requires a running Sapi turn") pure current
   lease <- readIORef (serviceLease s)
   require (maybe True ((==AgentId a) . fst) lease) 409 "Shared computer is busy with another Sapi. Continue non-UI work or report the blocker; do not retry in a loop."
   require (maybe True ((==RunId identity) . snd) lease) 409 "Shared computer is busy with another AgencyRun"
   writeIORef (serviceLease s) (Just (AgentId a,RunId identity))
   pure (obj [("owner",str a)])
  "routines" -> obj . (:[]) . ("routines",) . arr . filter ((==a) . (`at` "owner")) <$> DB.values s DB.ScheduledRoutines
  "routine_create" -> receipt . obj . (:[]) . ("routine",) <$> R.save s a (get d "minutes") (get d "prompt") Nothing Nothing current
  "routine_update" -> do i<-textField d "id" 80; receipt . obj . (:[]) . ("routine",) <$> R.save s a (get d "minutes") (get d "prompt") (Just i) Nothing current
  "routine_pause" -> pause receipt True
  "routine_resume" -> pause receipt False
  "delegate" -> D.applyDecision s a current (at d "decision")
  _ | op=="workspace" || "workspace_" `T.isPrefixOf` op -> do
   owner <- if has d "group" then do
    g <- G.getGroup s (at d "group") True
    G.authorizeGroup s g a Member
    pure (at d "group")
    else pure a
   if op=="workspace" then workspace s owner else receipt . obj . (:[]) . ("workspace",) <$> workspaceControl s owner (T.drop 10 op) d
  _ -> failure 400 "Unknown operation or field"
 where
 d=remove ["agencyRun"] payload
 change chief receipt retire=do
  require (a==chief) 403 "Only the main Sapi can retire or rehire Sapis through host-control"
  target <- resolve s (at d "target") True
  receipt <$> changeRetirement s target retire (at d "reason")
 pause receipt paused=do i<-textField d "id" 80; receipt . obj . (:[]) . ("routine",) <$> R.setPaused s a i paused
updateCorpora :: Service -> Value -> IO Value
updateCorpora s d = do
 require (not (null (keys d)) && all (`elem` ["name","device","scope","revision","archived"]) (keys d)) 400 "Unknown corpora setting"
 cfg <- corporaSettings s
 require (get d "revision"==num (corporaRevision cfg)) 409 "Corpora changed. Read its settings again."
 forM_ [("name",200),("device",200),("scope",16000)] $ \(k,limit) -> when (has d k) $ require (case get d k of String value -> T.length (T.strip value)<=limit && (k=="scope" || not (T.null (T.strip value))); _ -> False) 400 ("Invalid "<>k)
 when (has d "archived") $ do
  require (case get d "archived" of Bool _ -> True; _ -> False) 400 "archived must be a boolean"
  when (boolean (get d "archived")) $ readMVar (serviceRuns s) >>= mapM_ (\r->writeIORef (runCancel r) True) . M.elems
 let updated=cfg
      { corporaName=if has d "name" then Just (T.strip (at d "name")) else corporaName cfg
      , corporaDevice=if has d "device" then T.strip (at d "device") else corporaDevice cfg
      , corporaScope=if has d "scope" then T.strip (at d "scope") else corporaScope cfg
      , corporaRevision=corporaRevision cfg+1
      , corporaStatus=if has d "archived" then if boolean (get d "archived") then Archived else Paused else corporaStatus cfg }
 writeJSON (serviceRoot s </> "corpora-settings.json") updated
 modifyIORef' (servicePulse s) (put "running" (Bool (corporaStatus updated==Active)))
 DB.agents s >>= mapM_ (setManifests s)
 corporaIdentity s
lifecycle :: Service -> Value -> IO Value
lifecycle s d = do
 let operation=at d "operation"; action=at d "action"
 require (all (`elem` ["operation","action","revision"]) (keys d) && action `elem` ["pause","activate"] && T.length operation>=8 && T.length operation<=80) 400 "Provide an operation ID and pause or activate"
 cfg <- corporaSettings s
 let previous=fromMaybe Null (M.lookup operation (corporaOperations cfg)); intent=arr [str action,get d "revision"]
 if previous/=Null then require (previous==intent) 409 "Operation ID was reused" >> corporaIdentity s else do
  require (get d "revision"==num (corporaRevision cfg)) 409 "Corpora changed. Refresh before trying again."
  require (corporaStatus cfg/=Archived) 409 "Restore this archived corpora before activating it"
  runs <- readMVar (serviceRuns s)
  when (action=="activate" && corporaStatus cfg/=Active) $ require (M.null runs) 409 "Work is still stopping. Try again shortly."
  let updated=cfg { corporaStatus=if action=="pause" then Paused else Active, corporaRevision=corporaRevision cfg+1, corporaOperations=M.insert operation intent (corporaOperations cfg) }
  writeJSON (serviceRoot s </> "corpora-settings.json") updated
  modifyIORef' (servicePulse s) (put "running" (Bool (action=="activate")))
  when (action=="pause") $ forM_ (M.elems runs) $ \r -> writeIORef (runCancel r) True
  corporaIdentity s
