{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}
module Service (Service, APIError(..), openService, busyService, prepareService, prepareUpdatedService, startService, closeService, serviceRoot, attachEndpoint, dispatch, corporaIdentity) where

import Control.Concurrent
import Admission
import Control.Concurrent.STM (newTVarIO, readTVarIO)
import Control.Concurrent.Async
import Control.Exception
import Control.Monad
import Data.Aeson hiding (pairs)
import qualified Data.ByteString.Lazy as BL
import Data.IORef
import qualified Data.Map.Strict as M
import qualified Data.Set as Set
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import System.Directory
import System.Environment
import System.FileLock
import System.FilePath
import System.Info
import Types
import qualified Telemetry
import DataRoot
import qualified Computer
import qualified Store as DB
import qualified AppStorage as A
import qualified Migrations
import qualified Clock
import Population
import Workspace
import Voice (receivePiece, cancelRecording, retryTranscription, draftable, busyVoice, recoverVoice)
import Memo
import qualified Management as Mgt
import qualified Provider as Provider
import qualified Execution as E
import qualified Delegation as D
import qualified Routines as R
import qualified Groups as G
import qualified GroupChat as GC
import qualified GroupWork as GW

openService :: Clock.Clock -> Admission -> IORef (Maybe Telemetry.Telemetry) -> StorageChoice -> FilePath -> IO Service
openService clock gate telemetry choice path = do
 root <- selectedPath choice path
 repo <- lookupEnv "SAPIENS_REPOSITORY" >>= maybe getCurrentDirectory pure >>= canonicalizePath
 lock <- acquireStorage choice root
 let prepareIdentity = do
      let identityPath=root </> "corpora-identity.json"
      exists <- doesFileExist identityPath
      unless exists $ do i<-UUID.toText <$> UUID.nextRandom; writeJSON identityPath (obj [("version",num 1),("corpora_id",str i)])
      identity <- readJSON identityPath empty
      require (get identity "version"==num 1 && maybe False ((==at identity "corpora_id") . UUID.toText) (UUID.fromText (at identity "corpora_id"))) 409 "Invalid CORPORA identity. Restore corpora-identity.json from backup."
      pure (at identity "corpora_id")
 corporaId <- prepareIdentity `onException` unlockFile lock
 store <- Migrations.openCorpora root `onException` unlockFile lock
 let cleanup = A.closeStore store `finally` unlockFile lock
 let initialize = do
      guard <- newMVar ()
      stopping <- newIORef False
      worker <- newMVar Nothing
      runs <- newMVar M.empty
      lease <- newIORef Nothing
      endpoint <- newIORef ""
      pulse <- newIORef empty
      actors <- newMVar M.empty
      revision <- newTVarIO 0
      subscribers <- newTVarIO 0
      files <- newIORef Set.empty
      voicePieces <- newIORef M.empty
      let s=Service gate root repo guard lock stopping worker runs lease endpoint pulse actors store clock revision subscribers files voicePieces telemetry corporaId
      A.transaction store $ do
       rows <- DB.agents s
       when (null rows) $ void (createAgent s (obj [("name",str "ChiefSapi"),("role",str "Head of Corpora")]))
       let marker = initializingDirectory root
       pending <- doesDirectoryExist marker
       when (choice `elem` [StartNew,AttachExisting] && pending) $ removeDirectory marker
       agents <- DB.agents s
       mapM_ (ensureAgent s) agents
       chief <- DB.mainAgent s
       d <- directory s
       saveDirectory s $ foldl (\dir row -> let a=at row "id"; entry=get dir a in put a (put "parent" (if a==chief then Null else get entry "parent" `fallback` str chief) entry) dir) d agents
       mapM_ (E.recover s . (`at` "id")) agents
       savedPulse <- DB.singleton s DB.SystemPulse
       identityPulse <- if has savedPulse "pulse" then pure (at savedPulse "pulse") else ("pulse_" <>) <$> newId
       cfg <- corporaSettings s
       writeIORef pulse (obj [("id",str identityPulse),("running",Bool (corporaStatus cfg==Active)),("numbers",merge (obj [(k,num 0) | k<-["bpm120"]]) (get savedPulse "numbers"))])
       recoverVoice s
       D.reconcileWorks s
       GC.reconcileGroups s
       DB.record s [chief] "corpora" "system.started" empty "system" Nothing
       pure s
 initialize `onException` cleanup
attachEndpoint :: Service -> Text -> IO ()
attachEndpoint s url = locked s $ do
 writeIORef (serviceEndpoint s) url
 DB.agents s >>= mapM_ (Mgt.prepare s . (`at` "id"))
prepareService :: Service -> IO ()
prepareService s = locked s (prepareQueues s)

prepareUpdatedService :: Service -> IO ()
prepareUpdatedService s = locked s $ do
 DB.agents s >>= mapM_ (DB.project s . (`at` "id"))
 GC.reconcileGroups s
 DB.values s DB.Groups >>= mapM_ (G.folder s . (`at` "id"))
 prepareQueues s
 void (snapshotUnlocked s >>= evaluate . BL.length . encode)

prepareQueues :: Service -> IO ()
prepareQueues s = D.pendingWorks s >> GC.projectQueuedGroups s

startService :: Service -> IO ()
startService = E.startExecution

busyService :: Service -> IO Bool
busyService s = locked s $ do
 runs <- readMVar (serviceRuns s)
 turns <- DB.turns s
 works <- D.readWorks s
 voices <- busyVoice s
 groups <- GC.hasQueuedGroups s
 pure (voices || groups || not (M.null runs) || any (\t -> at t "status" `elem` ["queued","running","output_pending"]) turns
       || any (any ((`elem` [D.CallQueued,D.CallRunning]) . D.callState) . D.workCalls) works)
closeService :: Service -> IO ()
closeService s = do
 already <- atomicModifyIORef' (serviceStopping s) (\old -> (True,old))
 unless already $ do
  E.stopRuns s
  worker <- swapMVar (serviceWorker s) Nothing
  mapM_ cancel worker
  E.stopRuns s
  let awaitRuns = do runs<-readMVar (serviceRuns s); unless (M.null runs) (threadDelay 50000 >> awaitRuns)
  awaitRuns
  (do chief <- DB.mainAgent s
      DB.record s [chief] "corpora" "system.stopped" empty "system" Nothing)
   `finally` (A.closeStore (serviceStore s) `finally` unlockFile (serviceFileLock s))
snapshotUnlocked :: Service -> IO Value
snapshotUnlocked s = do
 revision <- readTVarIO (serviceRevision s)
 rows <- DB.agents s
 GC.reconcileGroups s
 turns <- DB.turns s
 prefs <- DB.singleton s DB.Preferences
 corpora <- corporaIdentity s
 groups <- G.snapshotGroups s
 routines <- DB.values s DB.ScheduledRoutines
 creation <- prompt s "creation-request"
 agentRows <- forM rows $ \row -> put "retired" . Bool <$> retired s (at row "id") <*> pure row
 owner <- maybe Null (toJSON . fst) <$> readIORef (serviceLease s)
 built <- blindly s >>= doesFileExist
 pulses <- DB.recentPulses s
 events <- DB.events s
 pulse <- readIORef (servicePulse s)
 chief <- DB.mainAgent s
 works <- D.readWorks s
 drafts <- forM (pairs (get prefs "attachment_drafts")) $ \(a,ids) -> do
  indexed <- attachmentIndex s a
  pure (a, arr [row | String i <- list ids, Just row <- [M.lookup i indexed], draftable row])
 dir <- directory s
 orchestration <- forM rows $ \row -> do
  let a=at row "id"
  notes <- memoMetadata (workspaceRoot s a)
  pure (a,obj [("manager",get (get dir a) "parent"),("notes",notes)])
 runs <- M.elems <$> readMVar (serviceRuns s)
 activity <- forM runs $ \run -> do
  value <- readIORef (runActivity run)
  stopping <- readIORef (runCancel run)
  pure (run,merge value (obj [("turn",str (runIdText (runId run))),("stopping",Bool stopping),("agencyKind",str "chatInput"),("slot",num (runSlot run))]))
 let groupTurns=M.fromList [(at request "id",at g "id") | g<-groups,request<-items g "requests"]
     projected=[maybe t (\gid->put "group" (str gid) t) (M.lookup (at t "id") groupTurns) | t<-turns]
 -- Watch metadata for inputs that may be edited without going through the API.
 binary <- blindly s
 let notes = [workspaceRoot s (at row "id") </> f | row <- rows ++ groups, f <- ["Notes.html","Notes.history.json"]]
     documents = [T.unpack (at t "path") | (_,w) <- pairs (get prefs "workspaces"), t <- items w "tabs", has t "path"]
 writeIORef (snapshotFiles s) $! Set.fromList $ [binary,repositoryRoot s </> "corpora/prompts/creation-request.md"] ++ notes ++ documents
 pure $ obj [("updates_revision",toJSON revision),("agents",arr agentRows),("turns",arr projected),("preferences",prefs),("corpora",corpora),("groups",arr groups),("routines",arr routines),("creation_template",str (T.strip creation)),("computer",obj [("owner",owner),("built",Bool built),("backend",str "native"),("platform",str (if os=="mingw32" then "win32" else T.pack os))]),("pulses",arr pulses),("events",events),("pulse",pulse),("provider",str "codex"),("activity",obj [(agentIdText (runOwner r),v) | (r,v)<-activity]),("run_activity",obj [(runIdText (runId r),v) | (r,v)<-activity]),("main_agent_id",str chief),("workloads",arr (D.workSummaries works)),("attachment_drafts",obj drafts),("orchestration",obj orchestration)]
validatePreferenceShape :: Value -> Validation ()
validatePreferenceShape d = do
 check (all (`elem` ["selected","panel","scope","groupView","panes","drafts","attachment_drafts"]) (keys d)) 400 "Unknown preference field"
 forM_ ["panes","drafts","attachment_drafts"] $ \k -> when (has d k) $ check (case get d k of Object _->True; _->False) 400 (k<>" must be an object")

validatePreferences :: [Value] -> [Value] -> Value -> Validation ()
validatePreferences rows groups d = do
 when (has d "selected") $ check (get d "selected" `elem` map (`get` "id") (rows++groups)) 400 "Unknown selected Sapi"
 when (get d "groupView"/=Null) $ check (get d "groupView" `elem` map (`get` "id") groups) 400 "Unknown Group view"
 check (get d "panel" `fallback` str "chat" `elem` map str ["chat","tasks","notes","work","updates"]) 400 "Unknown panel"
 check (get d "scope" `fallback` str "all" `elem` map str ["all","sapis","groups"]) 400 "Unknown view"
 forM_ (pairs (get d "panes")) $ \(k,v) -> check (k `elem` ["sidebar","chat","workspace"] && (case v of Bool _->True; _->False)) 400 "Invalid panel visibility"
 forM_ (pairs (get d "drafts")) $ \(_,v) -> check (case v of String t->T.length t<=16000; _->False) 400 "Invalid draft"

savePreferences :: Service -> Value -> IO Value
savePreferences s d = do
 validated (validatePreferenceShape d)
 rows <- DB.agents s
 groups <- DB.values s DB.Groups
 validated (validatePreferences rows groups d)
 drafts <- forM (pairs (get d "attachment_drafts")) $ \(a,ids) -> (\found -> (a, arr [get r "id" | r <- found, draftable r])) <$> resolveAttachments s a ids False
 prefs <- DB.singleton s DB.Preferences
 let current=merge prefs (if has d "attachment_drafts" then put "attachment_drafts" (obj drafts) d else d)
 DB.saveSingleton s DB.Preferences current
 pure (obj [("saved",Bool True),("preferences",current)])
dispatch :: Telemetry.RequestTransport -> Service -> Text -> Text -> Maybe Value -> IO (Int,BL.ByteString,Text)
dispatch _ s "PUT" "/api/model-settings" body = do
 requested <- decodeValue 400 (fromMaybe Null body)
 Provider.validateModelSettingsDraft s requested
 saved <- locked s (saveModelSettings s requested)
 pure (200,encode saved,"application/json; charset=utf-8")
dispatch transport s method target body = locked s $ do
 let (path,query)=T.breakOn "?" target; parts=filter (not . T.null) (T.splitOn "/" path); write=method `elem` ["POST","PUT"]; d=fromMaybe Null body
 when write $ require (case d of Object _->True; _->False) 400 "Expected a JSON object"
 cfg <- corporaSettings s
 when (write && corporaStatus cfg/=Active && ("/messages" `T.isSuffixOf` path || "/run" `T.isSuffixOf` path || "/turns/" `T.isInfixOf` path)) $ failure 409 "Activate this corpora before starting work"
 let json code action = do value<-action; when write (notifyChange s); pure (code,encode value,"application/json; charset=utf-8")
     owner kind a=if kind=="groups" then void (G.getGroup s a False) else void (DB.agent s a)
 case (method,parts) of
  ("GET",["api","telemetry"]) -> json 200 $ do
   telemetry <- readIORef (serviceTelemetry s)
   maybe (pure (obj [("enabled",Bool False),("available",Bool False)])) Telemetry.consentView telemetry
  ("POST",["api","telemetry","heartbeat"]) -> json 202 $ do
   telemetry <- readIORef (serviceTelemetry s)
   forM_ telemetry $ \reporter -> Telemetry.uiHeartbeat reporter (Just (serviceCorporaId s)) (Telemetry.parseClientContext d) d
   pure (obj [("accepted",Bool True)])
  ("GET",["api","model-settings"]) -> json 200 (toJSON <$> readModelSettings s)
  ("GET",["api","providers"]) -> json 200 (Provider.providersView (repositoryRoot s))
  ("GET",["api","health"]) -> json 200 (pure (obj [("status",str "ok"),("provider",str "codex")]))
  ("GET",["api","state"]) -> json 200 (snapshotUnlocked s)
  ("GET",["api","computer"]) -> json 200 (Computer.computerStatus (repositoryRoot s))
  ("GET",["api","corpora"]) -> json 200 (corporaIdentity s)
  ("GET",["api","corpora","operations",operation]) -> json 200 $ do identity<-corporaIdentity s; pure (obj [("applied",Bool (has (toJSON (corporaOperations cfg)) operation)),("corpora",identity)])
  ("PUT",["api","corpora","lifecycle"]) -> json 200 (Mgt.lifecycle s d)
  ("GET",["api","decision-prompts"]) -> json 200 (obj . (:[]) . ("prompts",) . arr <$> D.templates s)
  ("PUT",["api","decision-prompts",node]) -> json 200 (D.editTemplate s node d)
  ("GET",["api","agents",a,"tasks"]) -> do
   void (DB.agent s a)
   if "format=json" `T.isInfixOf` query then json 200 (D.taskList s a) else do value<-D.workTasks s a True; pure (200,encode value,"application/yaml; charset=utf-8")
  ("GET",["api",kind,a,"notes"]) | kind `elem` ["agents","groups"] -> do
   owner kind a
   require (T.null query) 404 "Unknown Notes resource"
   json 200 (readMemo (workspaceRoot s a))
  ("GET",["api",kind,a,"browser",tab,"content"]) | kind `elem` ["agents","groups"] -> owner kind a >> json 200 (workspaceText s a tab)
  ("GET",["api","groups"]) -> json 200 (obj . (:[]) . ("groups",) . arr <$> G.snapshotGroups s)
  ("GET",["api","groups",gid]) -> json 200 (G.getGroup s gid False)
  ("POST",["api","groups"]) -> json 201 (G.createGroup s d "admin")
  ("PUT",["api","groups",gid]) -> json 200 (G.updateGroup s gid d "admin")
  ("POST",["api","groups",gid,"messages"]) -> json 202 (GC.submitGroup s gid d "admin")
  ("POST",["api","groups",gid,"tasks"]) -> json 201 (GW.createTask s gid d "admin")
  ("PUT",["api","groups",gid,"tasks",tid]) -> json 200 (GW.updateTask s gid tid d "admin")
  ("POST",["api","groups",gid,"tasks",tid,"run"]) -> json 202 (GW.runTask s gid tid (get d "revision") "admin")
  ("POST",["api","groups",gid,"calls",rid,action]) -> json 200 (GC.actionGroup s gid rid action)
  ("POST",["api","agents"]) -> json 201 $ do row<-createAgent s d; Mgt.prepare s (at row "id"); pure row
  ("PUT",["api","agents",a]) -> json 200 (updateAgent s a d)
  ("POST",["api","agents",a,"messages"]) -> json 202 (E.submitTurn transport s a d)
  ("POST",["api","agents",a,"control"]) -> json 200 (withActor s a Nothing (Mgt.control s a d))
  ("GET",["api","agents",a,"turns",tid,"audit"]) -> json 200 $ do
   void (DB.agent s a)
   record <- readArchive s a ("runs/"<>tid)
   require (record/=empty) 404 "Unknown turn audit"
   pure record
  ("POST",["api","agents",a,"turns",tid,action]) -> json 200 (E.turnAction s a tid action)
  ("PUT",["api","preferences"]) -> json 200 (savePreferences s d)
  ("POST",["api",kind,a,"attachments"]) | kind `elem` ["agents","groups"] -> owner kind a >> json 201 (createAttachment s a d)
  ("POST",["api",kind,a,"voice"]) | kind `elem` ["agents","groups"] -> owner kind a >> json 200 (receivePiece s a d)
  ("POST",["api",kind,a,"voice",recording,"cancel"]) | kind `elem` ["agents","groups"] -> owner kind a >> json 200 (cancelRecording s a recording)
  ("POST",["api",kind,a,"attachments",i,"transcribe"]) | kind `elem` ["agents","groups"] -> owner kind a >> json 200 (retryTranscription s a i)
  ("POST",["api",kind,a,"browser"]) | kind `elem` ["agents","groups"] -> owner kind a >> json 200 (workspaceObserved s a d)
  ("POST",["api","groups",gid,"control"]) -> do
   void (G.getGroup s gid True)
   require ("workspace_" `T.isPrefixOf` at d "op") 400 "Only workspace operations are supported here"
   json 200 (workspaceControl s gid (T.drop 10 (at d "op")) d)
  _ -> failure 404 "Not found"
