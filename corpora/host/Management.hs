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
 pure $ listToMaybe [runIdText (turnId t) | t<-agentTurns state,turnStatus t==Running,maybe True ((==turnId t) . RunId) specific]
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
data WorkspaceOperation = OpenWorkspace | ZoomWorkspace | CloseWorkspace | FocusWorkspace | ReloadWorkspace | BackWorkspace | ForwardWorkspace | BookmarkWorkspace | UnbookmarkWorkspace deriving (Eq, Show)
data Operation = ReadCorpora | UpdateCorpora | Batch | ReadStatus | Delegate | ReadRoutines | CreateRoutine | UpdateRoutine | SelectRoutinePolicy | PauseRoutine | ResumeRoutine | CreateAgent | RetireAgent | RehireAgent | AcquireComputer | ChangeManager | ReadWorkspace | GroupCommand G.GroupOperation | WorkspaceCommand WorkspaceOperation deriving (Eq, Show)
operationSchema :: [(Text,Operation,[Text])]
operationSchema =
 [ ("corpora_get", ReadCorpora, [])
 , ("corpora_update", UpdateCorpora, ["name","device","scope","revision","archived"])
 , ("batch", Batch, ["operations"])
 , ("status", ReadStatus, ["target"])
 , ("delegate", Delegate, ["decision"])
 , ("routines", ReadRoutines, ["group"])
 , ("routine_create", CreateRoutine, ["minutes","prompt","missedPolicy"])
 , ("routine_update", UpdateRoutine, ["id","minutes","prompt","missedPolicy"])
 , ("routine_policy", SelectRoutinePolicy, ["id","expectedPrompt","expectedSelection","missedPolicy"])
 , ("routine_pause", PauseRoutine, ["id"])
 , ("routine_resume", ResumeRoutine, ["id"])
 , ("group_create", GroupCommand G.GroupCreate, ["name","description","lead","members"])
 , ("group_get", GroupCommand G.GroupGet, ["group"])
 , ("group_update", GroupCommand G.GroupUpdate, ["group","revision","name","description","lead","members","archived"])
 , ("group_message", GroupCommand G.GroupMessage, ["group","text","target"])
 , ("group_task_create", GroupCommand G.GroupTaskCreate, ["group","title","body","assignee"])
 , ("group_task_update", GroupCommand G.GroupTaskUpdate, ["group","task","revision","title","body","assignee","state","deleted"])
 , ("group_task_run", GroupCommand G.GroupTaskRun, ["group","task","revision"])
 , ("create_agent", CreateAgent, ["name","role","manager"])
 , ("retire_agent", RetireAgent, ["target","reason"])
 , ("rehire_agent", RehireAgent, ["target"])
 , ("computer_acquire", AcquireComputer, [])
 , ("manager", ChangeManager, ["manager","target"])
 , ("workspace", ReadWorkspace, ["group"])
 , ("workspace_open", WorkspaceCommand OpenWorkspace, ["url","path","id","group"])
 , ("workspace_zoom", WorkspaceCommand ZoomWorkspace, ["id","factor","group"])
 , ("workspace_close", WorkspaceCommand CloseWorkspace, ["id","group"])
 , ("workspace_focus", WorkspaceCommand FocusWorkspace, ["id","group"])
 , ("workspace_reload", WorkspaceCommand ReloadWorkspace, ["id","group"])
 , ("workspace_back", WorkspaceCommand BackWorkspace, ["id","group"])
 , ("workspace_forward", WorkspaceCommand ForwardWorkspace, ["id","group"])
 , ("workspace_bookmark", WorkspaceCommand BookmarkWorkspace, ["id","group"])
 , ("workspace_unbookmark", WorkspaceCommand UnbookmarkWorkspace, ["id","group"])]
decodeOperation :: Value -> Validation Operation
decodeOperation input = case [(operation,fields) | (name,operation,fields) <- operationSchema, name==at input "op"] of
 [(operation,fields)] | all (`elem` ("op":fields)) (keys input) -> Right operation
 _ -> Left (APIError 400 "Unknown operation or field")
workspaceOperationName :: WorkspaceOperation -> Text
workspaceOperationName operation = case operation of
 OpenWorkspace -> "open"
 ZoomWorkspace -> "zoom"
 CloseWorkspace -> "close"
 FocusWorkspace -> "focus"
 ReloadWorkspace -> "reload"
 BackWorkspace -> "back"
 ForwardWorkspace -> "forward"
 BookmarkWorkspace -> "bookmark"
 UnbookmarkWorkspace -> "unbookmark"

requiresActiveAgent :: Operation -> Bool
requiresActiveAgent ReadStatus = False
requiresActiveAgent ReadWorkspace = False
requiresActiveAgent (WorkspaceCommand _) = False
requiresActiveAgent _ = True

control :: Service -> Text -> Value -> IO Value
control s a payload = do
 void (DB.agent s a)
 let d=remove ["agencyRun"] payload; specific=if has payload "agencyRun" then Just (at payload "agencyRun") else Nothing
 current <- activeTurn s a specific
 when (isJust specific) $ require (isJust current) 409 "AgencyRun is no longer active"
 operation <- validated (decodeOperation d)
 when (requiresActiveAgent operation) $ requireActive s a
 chief <- DB.mainAgent s
 let receipt v=merge (obj [("self_id",str a),("saved",Bool True)]) v
 case operation of
  GroupCommand group -> G.groupControl s a group d
  Batch -> do
   let operations=items d "operations"
   require (not (null operations) && length operations<=20) 400 "Provide 1–20 operations"
   forM_ operations $ \item -> require (case decodeOperation item of Right Batch -> False; Right _ -> True; Left _ -> False) 400 "Invalid batch operation or field; nested batches are not supported"
   let execute index results []=pure (receipt (obj [("results",arr results)]))
       execute index results (item:rest)=do
        result <- try (control s a item)
        case result of
         Right v -> execute (index+1) (results++[v]) rest
         Left (APIError code err) -> pure (obj [("self_id",str a),("saved",Bool False),("results",arr results),("failed_index",num index),("error",str err),("status",num code),("partial",Bool (not (null results)))])
   execute 0 [] operations
  ReadCorpora -> require (a==chief) 403 "Only ChiefSapi can change corpora settings" >> corporaIdentity s
  UpdateCorpora -> require (a==chief) 403 "Only ChiefSapi can change corpora settings" >> updateCorpora s (remove ["op"] d)
  ReadStatus -> (if has d "target" then resolve s (at d "target") True else pure a) >>= status s
  CreateAgent -> do
   require (a==chief) 403 "Only the main Sapiens can create agents through host-control"
   name <- validName d
   role <- textField d "role" 60
   parent <- if get d "manager"==Null then pure a else resolve s (at d "manager") False
   resolution <- ensureSpecialist s name role parent
   let (profile,created) = case resolution of
        ExistingSpecialist existing -> (existing,False)
        CreatedSpecialist new -> (new,True)
   prepare s (agentIdText (profileId profile))
   pure (receipt (obj [("agent",toJSON profile),("created",Bool created)]))
  RetireAgent -> change chief receipt True
  RehireAgent -> change chief receipt False
  ChangeManager -> do
   require (has d "manager") 400 "manager is required; use null to clear it"
   target <- if has d "target" then resolve s (at d "target") False else pure a
   parent <- validateManager s target (get d "manager")
   assignManager s target parent
   pure (receipt (obj [("target",str target),("manager",parent)]))
  AcquireComputer -> do
   identity <- maybe (failure 409 "Computer access requires a running Sapi turn") pure current
   lease <- readIORef (serviceLease s)
   require (maybe True ((==AgentId a) . fst) lease) 409 "Shared computer is busy with another Sapi. Continue non-UI work or report the blocker; do not retry in a loop."
   require (maybe True ((==RunId identity) . snd) lease) 409 "Shared computer is busy with another AgencyRun"
   writeIORef (serviceLease s) (Just (AgentId a,RunId identity))
   pure (obj [("owner",str a)])
  ReadRoutines -> do
   owner <- workspaceOwner
   obj . (:[]) . ("routines",) . arr . filter ((==owner) . (`at` "owner")) <$> R.routineSummaries s
  CreateRoutine -> do
   policy <- decodeValue 400 (get d "missedPolicy")
   receipt . obj . (:[]) . ("routine",) <$> R.save s a (get d "minutes") (get d "prompt") (Just policy) (R.AddRoutine current)
  UpdateRoutine -> do
   i <- routineIdentity
   policy <- if has d "missedPolicy" then Just <$> decodeValue 400 (get d "missedPolicy") else pure Nothing
   receipt . obj . (:[]) . ("routine",) <$> R.save s a (get d "minutes") (get d "prompt") policy (R.EditRoutine i Nothing)
  SelectRoutinePolicy -> do
   i <- routineIdentity
   expected <- textField d "expectedPrompt" 16000
   selection <- decodeValue 400 (get d "expectedSelection")
   policy <- decodeValue 400 (get d "missedPolicy")
   receipt . obj . (:[]) . ("routine",) <$> R.setPolicy s a i expected selection policy
  PauseRoutine -> pause receipt True
  ResumeRoutine -> pause receipt False
  Delegate -> D.applyDecision s a current (at d "decision")
  ReadWorkspace -> workspaceOwner >>= workspace s
  WorkspaceCommand action -> do
   owner <- workspaceOwner
   result <- workspaceControl s owner (workspaceOperationName action) d
   pure (receipt (obj [("workspace",result)]))
 where
 d=remove ["agencyRun"] payload
 workspaceOwner = if has d "group" then do
  group <- G.getGroup s (at d "group") True
  G.authorizeGroup s group a Member
  pure (at group "id")
  else pure a
 change chief receipt retire=do
  require (a==chief) 403 "Only the main Sapi can retire or rehire Sapis through host-control"
  target <- resolve s (at d "target") True
  receipt <$> changeRetirement s target retire (at d "reason")
 routineIdentity = do
  void (textField d "id" 80)
  decodeValue 400 (get d "id")
 pause receipt paused=do i<-routineIdentity; receipt . obj . (:[]) . ("routine",) <$> R.setPaused s a i paused
reviseCorpora :: Value -> CorporaConfig -> Validation CorporaConfig
reviseCorpora d cfg = do
 check (get d "revision"==num (corporaRevision cfg)) 409 "Corpora changed. Read its settings again."
 forM_ [("name",200),("device",200),("scope",16000)] $ \(k,limit) -> when (has d k) $ check (case get d k of String value -> T.length (T.strip value)<=limit && (k=="scope" || not (T.null (T.strip value))); _ -> False) 400 ("Invalid "<>k)
 when (has d "archived") $ check (case get d "archived" of Bool _ -> True; _ -> False) 400 "archived must be a boolean"
 let updated=cfg
      { corporaName=if has d "name" then Just (T.strip (at d "name")) else corporaName cfg
      , corporaDevice=if has d "device" then T.strip (at d "device") else corporaDevice cfg
      , corporaScope=if has d "scope" then T.strip (at d "scope") else corporaScope cfg
      , corporaRevision=corporaRevision cfg+1
      , corporaStatus=if has d "archived" then if boolean (get d "archived") then Archived else Paused else corporaStatus cfg }
 pure updated

updateCorpora :: Service -> Value -> IO Value
updateCorpora s d = do
 require (not (null (keys d)) && all (`elem` ["name","device","scope","revision","archived"]) (keys d)) 400 "Unknown corpora setting"
 cfg <- corporaSettings s
 updated <- validated (reviseCorpora d cfg)
 when (boolean (get d "archived")) $ readMVar (serviceRuns s) >>= mapM_ (\r->writeIORef (runCancel r) True) . M.elems
 writeJSON (serviceRoot s </> "corpora-settings.json") updated
 modifyIORef' (servicePulse s) (put "running" (Bool (corporaStatus updated==Active)))
 DB.agents s >>= mapM_ (setManifests s)
 corporaIdentity s
data LifecycleAction = PauseCorpora | ActivateCorpora deriving (Eq, Show)
data LifecycleRequest = LifecycleRequest Text LifecycleAction Value
data LifecycleChange = LifecycleReplay | LifecycleApplied CorporaConfig

lifecycleRequest :: Value -> Validation LifecycleRequest
lifecycleRequest d
 | all (`elem` ["operation","action","revision"]) (keys d)
 , T.length operation>=8, T.length operation<=80
 , Just action <- lookup (at d "action") [("pause",PauseCorpora),("activate",ActivateCorpora)] = Right (LifecycleRequest operation action (get d "revision"))
 | otherwise = Left (APIError 400 "Provide an operation ID and pause or activate")
 where operation=at d "operation"

changeLifecycle :: Bool -> LifecycleRequest -> CorporaConfig -> Validation LifecycleChange
changeLifecycle running (LifecycleRequest operation action revision) cfg
 | previous/=Null = check (previous==intent) 409 "Operation ID was reused" *> Right LifecycleReplay
 | revision/=num (corporaRevision cfg) = Left (APIError 409 "Corpora changed. Refresh before trying again.")
 | corporaStatus cfg==Archived = Left (APIError 409 "Restore this archived corpora before activating it")
 | action==ActivateCorpora && corporaStatus cfg/=Active && running = Left (APIError 409 "Work is still stopping. Try again shortly.")
 | otherwise = Right (LifecycleApplied cfg { corporaStatus=next, corporaRevision=corporaRevision cfg+1, corporaOperations=M.insert operation intent (corporaOperations cfg) })
 where
 previous=fromMaybe Null (M.lookup operation (corporaOperations cfg))
 intent=arr [str (case action of PauseCorpora->"pause"; ActivateCorpora->"activate"),revision]
 next=case action of PauseCorpora->Paused; ActivateCorpora->Active

lifecycle :: Service -> Value -> IO Value
lifecycle s d = do
 request <- validated (lifecycleRequest d)
 cfg <- corporaSettings s
 runs <- readMVar (serviceRuns s)
 change <- validated (changeLifecycle (not (M.null runs)) request cfg)
 case change of
  LifecycleReplay -> pure ()
  LifecycleApplied updated -> do
   writeJSON (serviceRoot s </> "corpora-settings.json") updated
   modifyIORef' (servicePulse s) (put "running" (Bool (corporaStatus updated==Active)))
   when (corporaStatus updated==Paused) $ forM_ (M.elems runs) $ \r -> writeIORef (runCancel r) True
 corporaIdentity s
