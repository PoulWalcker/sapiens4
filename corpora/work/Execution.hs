{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Werror=incomplete-patterns #-}
module Execution where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception
import Control.Monad
import Data.Aeson hiding (pairs)
import Data.Aeson.Types (parseEither)
import qualified Data.Text.Encoding as TE
import qualified Database.SQLite.Simple as SQL
import qualified Data.ByteString.Lazy as BL
import Data.IORef
import Data.List (find,nub)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (getCurrentTime,addUTCTime,diffUTCTime)
import GHC.Clock
import System.Directory
import Types
import qualified Store as DB
import qualified ManagedState as Managed
import qualified Command as ScheduleCommand
import Population
import Workspace
import qualified Delegation as D
import qualified Routines as R
import qualified GeneratedCode as Code
import qualified Effects
import qualified Kit
import qualified Workflow
import qualified Composer
import qualified Automation
import Provider
import qualified GroupChat as G

trimConversation :: Service -> Text -> IO ()
trimConversation s a = do
 state <- stateJSON <$> agentState s a
 bounded <- foldM (\current field -> do
  let rows=items current field
      settled=filter (\row -> at row "status" `notElem` ["queued","running","output_pending"] && not (has row "managed")) rows
      removed=take (max 0 (length settled-100)) settled
  unless (null removed) $ newId >>= \i -> writeJSON (archivePath s a (field<>"/"<>i)) (arr removed)
  pure (put field (arr (filter (`notElem` removed) rows)) current)) state ["chat","events","turns"]
 final <- foldM (\current field -> do
  let prune value removed [] = pure (value,removed)
      prune value removed (row:rest)
       | BL.length (encode value)<=800000 = pure (value,removed)
       | has row "managed" || at row "status" `elem` ["queued","running","output_pending"] || (field=="turns" && Just row==listToMaybe (reverse (items value field))) = prune value removed rest
       | otherwise = prune (put field (arr (filter (/=row) (items value field))) value) (removed++[row]) rest
  (result,removed) <- prune current [] (items current field)
  unless (null removed) $ newId >>= \i -> writeJSON (archivePath s a (field<>"/"<>i)) (arr removed)
  pure result) bounded ["chat","events","turns"]
 when (final/=state) (decodeValue 500 final >>= saveState s a)
submitTurn :: Service -> Text -> Value -> IO Value
submitTurn s a d = do
 let raw=get d "text" `fallback` str ""; input=T.strip (text raw); flowValue=get d "flow" `fallback` str "chat"
 require (case raw of String t -> T.length t<=16000; _ -> False) 400 "Message must be text, at most 16000 characters"
 attachments <- resolveAttachments s a (get d "attachments" `fallback` arr []) True
 require (not (T.null input) || not (null attachments)) 400 "Write a message or attach a file"
 flow <- decodeValue 400 flowValue
 managed <- decodeValue 400 (get d "managed" `fallback` Bool False)
 when managed $ do
  require (flow==Chat && not (has d "workload")) 400 "Managed execution currently supports ordinary chat submissions only"
  require (ScheduleCommand.interpret (not (null attachments)) (T.unpack input)==Right Nothing) 400 "Managed execution does not accept schedule commands"
 binary <- codexBinary
 require (isJust binary) 503 "Codex CLI is missing. Install it and run codex login."
 when (flow==Computer) $ blindly s >>= doesFileExist >>= \built -> require built 503 "Blindly4 is not built. Run ./build/start.sh to build it."
 stopped <- readIORef (serviceStopping s)
 require (not stopped) 503 "Server is shutting down"
 requireActive s a
 routine <- R.command s a input (not (null attachments))
 case routine of
  Just r -> R.acknowledge s a input r
  Nothing -> do
   suffix <- attachmentPrompt s attachments
   let message=input<>suffix
   i <- if has d "workload" then D.clarify s a (at d "workload") message else do
    i <- newId
    time <- now
    let typed=newTurn (RunId i) flow message time Null (not managed)
        turn=toJSON typed
        msg=obj [("role",str "user"),("content",str message),("turn",str i),("time",str time),("origin",Null)]
    when managed (enrollManaged s a typed msg input attachments)
    modifyState s a $ \state -> merge state (obj ([("turns",arr (items state "turns"++[turn])) | not managed]++[("chat",arr (items state "chat"++[msg])) | not managed]))
    pure i
   DB.message s i input attachments
   DB.project s a
   trimConversation s a
   pure (obj [("id",str i),("agent",str a),("status",str "queued"),("flow",toJSON flow)])
turnAction :: Service -> Text -> Text -> Text -> IO Value
turnAction s a identity rawAction = turnCommand s a identity rawAction Nothing
turnCommand :: Service -> Text -> Text -> Text -> Maybe Value -> IO Value
turnCommand s a identity rawAction external = do
 requireActive s a
 action <- decodeValue 404 (str rawAction)
 state <- agentState s a
 turn <- maybe (failure 404 "Unknown active turn") pure (find ((==RunId identity) . turnId) (agentTurns state))
 when (rawAction=="retry") $ do
  Effects.requireRunClear s (RunId identity)
  require (not (has (turnField turn "origin") "analysis" || has (turnField turn "origin") "preparation")) 409 "Use explicit Routine reconsideration for a new analysis"
  require (not (boolean (turnField turn "externalBound"))) 409 "External occurrence evidence is retained; start a new authorized occurrence instead of Retry. Unknown deliveries require reconciliation first"
  require (not (boolean (turnField turn "codeBound"))) 409 "Code-run evidence is retained; start a new authorized occurrence instead of Retry"
  D.requireCallAllowed s turn
 work <- D.findWork s (RunId identity)
 require (not (maybe False (any ((==Just (RunId identity)) . D.callParent) . D.workCalls . fst) work)) 409 "This request was delegated; use the recipient call controls"
 command <- if has (toJSON turn) "managed" then do
  cmd <- case external of
   Just value -> pure value
   Nothing -> do i<-newId; pure (obj [("id",str i),("action",str rawAction),("revision",get (turnField turn "managed") "revision")])
  pure (Just (identity,cmd))
  else pure Nothing
 let duplicate=maybe False ((`elem` items (turnField turn "managed") "commands") . snd) command
     leader=batchLeader turn
     update f = mapTurns (\t -> if batchLeader t == leader then f t else t)
 if duplicate then saveStateCommand s a state command else case action of
  Retry -> do
   require (retryableStatus (turnStatus turn)) 409 "Only failed or interrupted batches can be retried"
   saveStateCommand s a (update (withTurnFields (remove ["cancel_requested"]) . retryTurn) state) command
  Cancel -> do
   require (turnStatus turn `notElem` [Done,OutputPending]) 409 "Completed input cannot be cancelled"
   runs <- readMVar (serviceRuns s)
   case M.lookup leader runs of
    Just run -> do
     if isJust command then saveStateCommitted s a (update (withTurnFields (put "cancel_requested" (Bool True))) state) command (writeIORef (runCancel run) True)
      else writeIORef (runCancel run) True
    Nothing -> saveStateCommand s a (update (\t -> t { turnStatus = Cancelled }) state) command
 when (action==Cancel) (Automation.cancelProviderWait s (RunId identity))
 DB.project s a
 trimConversation s a
 D.reconcileWorks s
 updated <- agentState s a
 pure (obj [("id",str identity),("status",maybe Null (toJSON . turnStatus) (find ((==RunId identity) . turnId) (agentTurns updated)))])
recover :: Service -> Text -> IO ()
recover s a = do
 restoreManaged s a
 modifyTurns s a $ \t -> if turnStatus t == Running then
  (withTurnFields (put "error" (str "Runner stopped before saving a reply; inspect effects before retrying")) t) { turnStatus = Interrupted } else t
 DB.project s a
finish :: Service -> Run -> Turn -> Either SomeException (Text,[Value]) -> IO ()
finish s run turn outcome | Left problem<-outcome, Just (RetryDeferred operation due)<-fromException problem = Automation.deferProvider s run operation due `catch` (\(problem::APIError)->finish s run turn (Left (toException problem)))
finish s run turn outcome = do
 fenced <- not <$> D.callAllowed s turn
 stop <- (||fenced) <$> readIORef (runCancel run)
 let a=agentIdText (runOwner run); i=runIdText (runId run)
     (rawOutput,logs,error)=case outcome of
      Left e -> case fromException e of
       Just (RunFailure message entries)->("",entries,Just message)
       Nothing->("",[],Just (D.exceptionText e))
      Right (value,entries)->(value,entries,Nothing)
     output=if fenced then "" else rawOutput
     failureText=if stop then Just "Stopped by Admin. Saved work is preserved; review external effects before retrying." else error
 writeJSON (archivePath s a ("runs/"<>i)) (obj [("output",str output),("logs",arr logs),("error",maybe Null str failureText)])
 state <- agentState s a
 time <- now
 ids <- decodeValue 500 (turnField turn "batch_members" `fallback` toJSON [turnId turn]) :: IO [RunId]
 let buffered=turnField turn "agency_kind"==str "chatInput"
     origin=turnField turn "origin"
     warnings=[at entry "warning" | entry<-logs,has entry "warning"]
     update t | turnId t `notElem` ids = t
              | otherwise = withTurnFields warningFields $ case failureText of
                 Just e -> (withTurnFields (put "error" (str e)) t) { turnStatus = if stop then Interrupted else Failed }
                 Nothing | buffered -> t { turnStatus = OutputPending }
                         | otherwise -> (withTurnFields (put "output_at" (str time)) t) { turnStatus = Done }
     warningFields = if null warnings then remove ["warning"] else put "warning" (str (T.intercalate " " warnings))
     updated=mapTurns update state
     result | isJust failureText = updated
            | buffered = mapStateFields (appendItems "output_buffer" [obj [("turn",str i),("members",toJSON ids),("content",str output),("origin",origin)]]) updated
            | has origin "group" || has origin "analysis" || has origin "preparation" = updated
            | otherwise = mapStateFields (put "last_output" (str output) . appendItems "chat" [obj [("role",str "agent"),("content",str output),("turn",str i),("time",str time),("origin",origin)]]) updated
 let receipt=obj [("status",str (if isJust failureText then if stop then "interrupted" else "failed" else "done")),("output",str output),("error",maybe Null str failureText),("recorded_at",str time)]
     recorded=mapTurns (\t -> if turnId t `elem` ids && has (toJSON t) "managed" then withTurnFields (put "managed_result" receipt) t else t) result
 saveState s a recorded
 DB.project s a
 trimConversation s a
 D.reconcileWorks s
 Effects.sealRoutineAttempt s (runId run)
launch :: Service -> Text -> NonEmpty Turn -> Pulse -> Int -> Bool -> IO ()
launch s a batch tick slot batching = do
 mapM_ (D.requireCallAllowed s) batch
 state <- agentState s a
 let first=NE.head batch; i=turnId first; ids=map turnId (NE.toList batch)
     reserve t
      | turnId t `notElem` ids = t
      | otherwise = (withTurnFields (mergeFields (batchFields ++ [("batch_members",toJSON ids) | batching && turnId t==i]) . remove ["error"]) t) { turnStatus = Running, turnBatchId = if batching then Just i else turnBatchId t }
     batchFields = if batching then [("agency_kind",str "chatInput"),("slot",num slot),("pulseId",str (pulseIdentity tick))] else []
     marked=mapTurns reserve state
     snapshot=mapStateFields (mergeFields ([("current_turn",toJSON i)] ++ [("agency_batch",arr [obj [("id",toJSON (turnId t)),("content",str (turnInput t))] | t <- NE.toList batch]) | batching])) marked
     execution=(reserve first) { turnInput = T.intercalate "\n\n" (map turnInput (NE.toList batch)) }
 saveState s a marked
 cancel <- newIORef False
 process <- newMVar Nothing
 activity <- newIORef empty
 deadline <- (+1800000000000) <$> getMonotonicTimeNSec
 let run=Run (AgentId a) i slot cancel process activity deadline
 modifyMVar_ (serviceRuns s) (pure . M.insert i run)
 DB.pulseCall s tick a "chatInput" slot (nub [at (turnField t "origin") "group" | t <- NE.toList batch,has (turnField t "origin") "group"])
 DB.project s a
 void $ forkIO $ do
  result <- try (Automation.withRepairBudget s run $ \repairRun -> withActor s a (Just "chatInput") (if has (turnField execution "origin") "group" then G.runGroup s repairRun execution else if has (turnField execution "origin") "kitInspection" then Automation.runKitInspection s repairRun execution else if has (turnField execution "origin") "repairAudit" then runSavedAudit s repairRun execution else if has (turnField execution "origin") "repair" then Automation.runDiagnosis s repairRun execution else if has (turnField execution "origin") "preparation" then Composer.run s repairRun execution else dispatchOrdinary s repairRun execution snapshot))
  locked s $ finish s run execution result `finally` do
   lease <- readIORef (serviceLease s)
   when (lease==Just (AgentId a,i)) $ writeIORef (serviceLease s) Nothing
   modifyMVar_ (serviceRuns s) (pure . M.delete i)
   D.pendingWorks s
   G.reconcileGroups s
   G.dispatchGroups s
-- Implementation is pinned at admission; the model does not dispatch code runs.
dispatchOrdinary :: Service -> Run -> Turn -> AgentState -> IO (Text,[Value])
dispatchOrdinary s running turn snapshot = do
 Effects.requireRunClear s (turnId turn)
 found<-D.findWork s (turnId turn)
 let profile=maybe False (Effects.profileBound . toJSON . fst) found
     execute=do
      when profile (Effects.beforeContent s running)
      result<-if maybe False (\(work,call)->has (toJSON work) "codeExecution" || at (D.callField call "executionBinding") "mode"=="script") found
       then runCode s running turn else D.runDecision s running turn snapshot
      when profile (Effects.deliver s running)
      pure result
 if not profile then execute else mask $ \restore -> do
  result<-try (restore execute)
  cleanup<-try (Effects.finishOccurrence s running)
  case result of
   Left (err::SomeException)->throwIO err
   Right value->case cleanup of Left (err::SomeException)->throwIO err; Right ()->pure value

data SemanticVerdict = SemanticallyAccepted | SemanticallyRejected | SemanticallyUnknown deriving (Eq,Show)
instance FromJSON SemanticVerdict where
 parseJSON value=case text value of "accepted"->pure SemanticallyAccepted; "rejected"->pure SemanticallyRejected; "unknown"->pure SemanticallyUnknown; _->fail "Expected accepted, rejected or unknown semantic verdict"
instance ToJSON SemanticVerdict where
 toJSON value=str (case value of SemanticallyAccepted->"accepted"; SemanticallyRejected->"rejected"; SemanticallyUnknown->"unknown")
data CodeRunError = ProvenCodeFault Text | VerificationUnknown Text deriving (Show)
instance Exception CodeRunError

codeUpdate :: Service -> Run -> (Value -> Value) -> IO ()
codeUpdate s running transform = locked s $ do
 (work,_)<-D.getWork s (runId running)
 D.saveWork s (D.setWorkField "codeExecution" (transform (D.workField work "codeExecution")) work)
codeGuard :: Service -> Run -> IO ()
codeGuard s running = do
 requireActive s (agentIdText (runOwner running))
 Effects.requireRunClear s (runId running)
 cancelled<-readIORef (runCancel running)
 require (not cancelled) 409 "Code occurrence stopped"
 (work,call)<-D.getWork s (runId running)
 forM_ (parseTime (at (D.workField work "codeExecution") "workflowDeadline")) $ \deadline->do
  wall<-getCurrentTime
  require (wall<deadline) 409 "Original workflow allowance expired"
 DB.requireOccurrence s (D.workField work "origin")
 when (has (toJSON work) "repairPin") $ DB.repairPinAllowed s (D.workField work "repairPin") >>= \clear->require clear 409 "Repair case is stopped or changed"
 Effects.requireCurrentInput s (toJSON work)
 if has (toJSON work) "savedAudit" then do
  (source,_,proof)<-DB.connection s (\db->Automation.savedAuditSourceOn db (D.workField work "repairPin") (at (D.workField work "savedAudit") "source"))
  require (proof==D.workField work "savedAudit") 409 "Saved audit source changed before model admission"
  Effects.requireRunClear s (RunId (D.workloadId source))
  Effects.requireCurrentInput s (toJSON source)
 else void (D.resolveCandidate s work call)
 works<-D.readWorks s
 require (not (any D.generatedCleanupPending works)) 503 "Owned code cleanup is unresolved; restore the runtime and restart the app"

runCode :: Service -> Run -> Turn -> IO (Text,[Value])
runCode s running turn = do
 locked s (D.begin s (runOwner running) turn)
 result<-try (executeCode s running) :: IO (Either SomeException Text)
 case result of
  Right reply->pure (reply,[])
  Left exception->do
   let asynchronous=isJust (fromException exception :: Maybe SomeAsyncException) || isJust (fromException exception :: Maybe AsyncCancelled)
   cancelled<-readIORef (runCancel running)
   permitted<-DB.occurrenceAllowed s (turnField turn "origin")
   let terminal=if asynchronous || cancelled || not permitted then "interrupted" else "failed"
   let interrupted code =
        let container=get code "container"
            pending=has container "name" && at container "status"/="removed"
            operations=[if at op "status" `elem` ["running","validating"] then mergeFields [("status",str terminal),("error",str (D.exceptionText exception))] op else op | op<-items code "operations"]
        in mergeFields [("status",str (if pending then "cleanup_pending" else terminal)),("operations",arr operations),("error",str (D.exceptionText exception))] code
   codeUpdate s running interrupted
   when (not (asynchronous || cancelled) && permitted) (Automation.recordAuditFailure s running (D.exceptionText exception))
   case fromException exception of
    Just (ProvenCodeFault reason)->when (not (asynchronous || cancelled) && permitted) (markCodeFault s running reason)
    Just (VerificationUnknown _)->pure ()
    Nothing->pure ()
   throwIO exception

executeCode :: Service -> Run -> IO Text
data KitPending = KitPending Text deriving (Show)
instance Exception KitPending

-- This check is used only before the calculation admission. It cannot replay a
-- calculation or an external action that already completed.
requireKitBody :: Service -> Run -> Value -> IO ()
requireKitBody s running version=do
 (work,_)<-D.getWork s (runId running)
 definition<-DB.connection s (\db->Effects.definitionOn db (at (D.workField work "routineSnapshot") "id"))
 works<-D.readWorks s
 unless (Kit.executionAllowed definition version (map toJSON works)) (throwIO (KitPending "Shared Kit bytes await this consumer's own applicability inspection"))

executeCode s originalRun = do
 codeGuard s originalRun
 (work,call)<-D.getWork s (runId originalRun)
 (version,_)<-D.resolveCandidate s work call
 running<-if has version "workflow" then workflowBudget s originalRun else pure originalRun
 executePreparedCode s running

executePreparedCode :: Service -> Run -> IO Text
executePreparedCode s running = executeCodeBody s running `catch` (\(KitPending reason)->do
 codeGuard s running
 (work,call)<-D.getWork s (runId running)
 (version,role)<-D.resolveCandidate s work call
 spec<-D.executionContract work call >>= maybe (failure 409 "Accepted task disappeared before safe fallback") pure
 explanation<-either (failure 409 . T.pack) pure (parseEither candidateExplanation version)
 let input=get (D.workField work "codeExecution") "input"
 (invocation,verdict,response,outcome,audit)<-wholeResult s running version role spec input (obj [("status",str "not_started"),("reason",str reason)]) explanation reason
 commitCodeOutcome s running version role invocation verdict response outcome audit
 case D.decisionBody response of D.ReportOutcome reply _->pure reply; _->failure 500 "Safe Kit fallback has no accepted outcome")

executeCodeBody :: Service -> Run -> IO Text
executeCodeBody s running = do
 codeGuard s running
 (work,call)<-D.getWork s (runId running)
 (version,role)<-D.resolveCandidate s work call
 spec<-D.executionContract work call >>= maybe (failure 409 "Code run has no accepted contract") pure
 let ordinaryInput=case D.specInputs spec of
      []->Null
      [raw]->either (const (str raw)) id (eitherDecodeStrict (TE.encodeUtf8 raw))
      values->toJSON values
 input<-if Effects.profileBound (toJSON work) then either (failure 409 . T.pack) (pure . (`get` "input")) (Effects.observedInput (toJSON work)) else pure ordinaryInput
 codeUpdate s running (put "input" input)
 requireKitBody s running version
 explanation<-either (failure 409 . T.pack) pure (parseEither candidateExplanation version)
 calculated<-try (executeCalculation s running version input) :: IO (Either CodeRunError (Value,Value))
 (actual,runtime)<-case calculated of
  Right (value,receipt)->pure (value,receipt)
  Left (VerificationUnknown reason)->throwIO (VerificationUnknown reason)
  Left (ProvenCodeFault reason)->do
   recordImplementationFault s running CalculationFault reason
   replacement<-runFallback s running CalculationFallback version spec input reason
   retained<-fst <$> D.getWork s (runId running)
   pure (get replacement "result",get (D.codeProjection (D.workField retained "codeExecution")) "runtimeResult")
 useful<-try (explainCalculation s running version spec input actual explanation) :: IO (Either SomeException (Maybe Text))
 initial<-case useful of
  Right narrative->do
   assessed<-assessCodeResult s running version role spec input actual runtime explanation narrative Nothing
   case assessed of
    (invocation,SemanticallyRejected,decision,outcome,audit)->do
     recordImplementationFault s running CompositionFault (D.decisionReason decision)
     wholeResult s running version role spec input runtime explanation (D.decisionReason decision)
      `onException` void (try (commitCodeOutcome s running version role invocation SemanticallyRejected decision outcome audit) :: IO (Either APIError ()))
    _->pure assessed
  Left exception->do
   -- Only an unsuccessful useful model step is replaceable here; cancellation,
   -- stale authority and unknown effects remain hard fences.
   codeGuard s running
   case fromException exception :: Maybe APIError of
    Just (APIError code reason) | code `elem` [422,500,503]->do
     when (code==422) (recordImplementationFault s running ExplanationFault reason)
     wholeResult s running version role spec input runtime explanation reason
    _->throwIO exception
 let (invocation,verdict,decision,outcome,audit)=initial
 commitCodeOutcome s running version role invocation verdict decision outcome audit
 case D.decisionBody decision of D.ReportOutcome reply _->pure reply; _->failure 500 "Invalid code assessment transition"

-- A maintenance call reassesses the exact retained calculation and explanation.
-- It does not execute code, regenerate explanation, create a business occurrence
-- or call delivery. The shared commit targets the original occurrence explicitly.
runSavedAudit :: Service -> Run -> Turn -> IO (Text,[Value])
runSavedAudit s running _ = do
 codeGuard s running
 (maintenance,_)<-D.getWork s (runId running)
 let pin=D.workField maintenance "repairPin"
     selected=D.workField maintenance "savedAudit"
 (source,version,proof)<-DB.connection s (\db->Automation.savedAuditSourceOn db pin (at selected "source"))
 require (proof==selected) 409 "Saved result changed before audit"
 let code=D.workField source "codeExecution"
     runtime=get (D.codeProjection code) "runtimeResult"
     actual=get code "evaluatedResult"
     narrative=if has version "explanation" then Just (at code "evaluatedExplanation") else Nothing
 callSpec<-D.executionContract source (NE.head (D.workCalls source)) >>= maybe (failure 409 "Original task criteria disappeared") pure
 role<-decodeValue 409 (get code "role")
 explanation<-either (failure 409 . T.pack) pure (parseEither candidateExplanation version)
 published<-either (failure 409 . T.pack) pure (Effects.auditPublication (toJSON source))
 (invocation,verdict,response,outcome,audit)<-assessCodeResult s running version role callSpec (get code "input") actual runtime explanation narrative published
 commitCodeOutcomeFor s running (Just selected) version role invocation verdict response outcome audit
 (original,_)<-D.getWork s (RunId (D.workloadId source))
 let activation=get (D.workField original "codeExecution") "activation"
     accepted=verdict==SemanticallyAccepted && all ((==D.CallCompleted) . D.callState) (D.workCalls original)
       && (role==ActiveCode || at activation "status"=="activated" && get audit "accepted"==Bool True)
     result=obj [("source",str (D.workloadId source)),("reference",get version "reference"),("verdict",toJSON verdict),("accepted",Bool accepted),("assessment",str invocation),("activation",activation)]
 locked s $ do
  (current,_)<-D.getWork s (runId running)
  D.saveWork s ((D.setWorkField "savedAuditResult" result current) {D.workCalls=fmap (\call->call {D.callState=D.CallCompleted,D.callOutput=Just "The original retained result was independently assessed without re-execution."}) (D.workCalls current)})
 pure ("The original retained result was independently assessed without re-execution.",[])

data FallbackScope = CalculationFallback | WholeFallback deriving (Eq,Show)
fallbackScope :: FallbackScope -> Text
fallbackScope CalculationFallback="calculate"
fallbackScope WholeFallback="whole"
data ImplementationFault = CalculationFault | ExplanationFault | CompositionFault deriving (Eq,Show)
faultScope :: ImplementationFault -> Text
faultScope CalculationFault="calculation"
faultScope ExplanationFault="explanation"
faultScope CompositionFault="composition"

recordImplementationFault :: Service -> Run -> ImplementationFault -> Text -> IO ()
recordImplementationFault s running scope reason = do
 codeGuard s running
 (source,call)<-D.getWork s (runId running)
 (version,_)<-D.resolveCandidate s source call
 codeUpdate s running (\code->appendItems "faults" [obj [("scope",str (faultScope scope)),("reference",get code "reference"),("reason",str reason),("sourceHash",get version "sourceHash"),("artifact",str (canonicalHash (Kit.descriptor version)))]] code)
 markCodeFault s running reason

executeCalculation :: Service -> Run -> Value -> Value -> IO (Value,Value)
executeCalculation s running version input | has version "workflow" = executeWorkflowCalculation s running version input
executeCalculation s running version input = do
 let reference=get version "reference"
 environmentId<-newId
 began<-getMonotonicTimeNSec
 codeUpdate s running (appendItems "operations" [obj [("id",str environmentId),("purpose",str "environment"),("status",str "running")]])
 environmentResult<-try (do
  profile<-Code.available
  require (Code.profileJSON profile==get version "runtime") 409 "Pinned code runtime identity is unavailable"
  pure profile) :: IO (Either APIError Code.Profile)
 environmentEnd<-getMonotonicTimeNSec
 let environmentFields=case environmentResult of Right profile->[("status",str "completed"),("runtime",Code.profileJSON profile)]; Left (APIError _ reason)->[("status",str "failed"),("error",str reason)]
 codeUpdate s running (\code->put "operations" (arr [if at op "id"==environmentId then mergeFields (("durationMs",toJSON ((environmentEnd-began) `div` 1000000)):environmentFields) op else op | op<-items code "operations"]) code)
 profile<-either throwIO pure environmentResult
 nonce<-newId
 started<-now
 let reserved=Code.businessReservation profile (agentIdText (runOwner running)) (runIdText (runId running)) nonce
     operation=obj [("id",str nonce),("purpose",str "code_execution"),("stage",str "calculate"),("status",str "running"),("started",str started),("durationMs",Null),("reference",reference),("inputHash",str (canonicalHash input))]
 locked s $ do
  requireKitBody s running version
  (current,_)<-D.getWork s (runId running)
  let code=mergeFields [("input",input),("status",str "running"),("container",reserved)] (appendItems "operations" [operation] (D.workField current "codeExecution"))
  D.saveWork s (D.setWorkField "codeExecution" code current)
 result<-Code.runFixture profile running reserved (at version "source") input (\receipt->do
  when (at receipt "status"/="removed") (codeGuard s running)
  codeUpdate s running (put "container" receipt))
 let actual=parseJSONText (at result "output")
     runtime=mergeFields [("actual",actual),("reference",reference),("sourceHash",get version "sourceHash"),("inputHash",str (canonicalHash input))] result
 let runtimeStatus=if at result "status"=="stopped" then "interrupted" else if at result "status"=="exited" && get result "exitCode"==num 0 && not (boolean (get result "wasRunning")) then "completed" else "failed"
 codeUpdate s running (\code->put "operations" (arr [if at op "id"==nonce then mergeFields [("status",str runtimeStatus),("result",runtime),("durationMs",get runtime "durationMs")] op else op | op<-items code "operations"]) code)
 codeGuard s running
 when (at result "status" `elem` ["output_limit","timeout"] || boolean (get result "oomKilled")) (throwIO (ProvenCodeFault "Candidate exceeded its verified runtime budget"))
 require (at result "status"=="exited" && not (boolean (get result "wasRunning"))) 503 "Runtime attachment ended without an established process result"
 when (get result "exitCode"/=num 0) (throwIO (ProvenCodeFault "Candidate body exited unsuccessfully"))
 validateCodeResult version actual
 pure (actual,runtime)

workflowBudget :: Service -> Run -> IO Run
workflowBudget s originalRun = do
 wall<-getCurrentTime
 clock<-getMonotonicTimeNSec
 require (clock<runDeadline originalRun) 409 "Workflow allowance is exhausted"
 (savedWork,_)<-D.getWork s (runId originalRun)
 let savedDeadline=get (D.workField savedWork "codeExecution") "workflowDeadline"
     initial=addUTCTime (fromIntegral (runDeadline originalRun-clock)/1000000000) wall
 deadline<-if savedDeadline==Null then pure initial else maybe (failure 409 "Invalid retained workflow deadline") pure (parseTime (text savedDeadline))
 require (deadline>wall) 409 "Original workflow allowance expired"
 let remaining=floor (diffUTCTime deadline wall*1000000000)
     running=originalRun {runDeadline=min (runDeadline originalRun) (clock+remaining)}
 codeUpdate s running (put "workflowDeadline" (str (timestamp deadline)))
 pure running

executeWorkflowCalculation :: Service -> Run -> Value -> Value -> IO (Value,Value)
executeWorkflowCalculation s running version input = do
 profile<-Code.available
 require (Code.profileJSON profile==get version "runtime") 409 "Pinned workflow runtime is unavailable"
 settings<-decodeValue 409 (get version "workflowModel")
 let loadTrace=do (work,_)<-D.getWork s (runId running); pure (get (D.workField work "codeExecution") "workflowRun" `fallback` obj [])
     publish trace=locked s $ do
      currentClock<-getMonotonicTimeNSec
      require (currentClock<runDeadline running) 409 "Original workflow allowance expired"
      codeGuard s running
      (work,call)<-D.getWork s (runId running)
      (current,_)<-D.resolveCandidate s work call
      require (current==version) 409 "Workflow candidate changed"
      D.saveWork s (D.setWorkField "codeExecution" (put "workflowRun" trace (D.workField work "codeExecution")) work)
     invoke identity node selected=case at node "kind" of
      "script"->do
       codeGuard s running
       nonce<-newId
       let reserved=Code.businessReservation profile (agentIdText (runOwner running)) (runIdText (runId running)) nonce
       codeUpdate s running (put "container" reserved)
       receipt<-Code.runFixture profile running reserved (at node "source") selected (\value->do
        when (at value "status"/="removed") (codeGuard s running)
        codeUpdate s running (put "container" value))
       require (at receipt "status"=="exited" && get receipt "exitCode"==num 0 && not (boolean (get receipt "wasRunning"))) 503 "Workflow script did not establish a successful result"
       either (failure 422 . T.pack) pure (eitherDecodeStrict (TE.encodeUtf8 (at receipt "output")))
      _->do
       codeGuard s running
       invocation<-newId
       let rendered=render (at version "workflowPrompt") [("instruction",at node "instruction"),("context",jsonText (obj [("selfId",toJSON (runOwner running)),("step",str identity),("input",selected)]))]
           operation=obj [("id",str invocation),("node",str "WorkflowStep"),("step",str identity),("purpose",str "workflow_response"),("policy",str "response_only"),("modelSettings",toJSON settings),("promptHash",str (sourceHash rendered)),("status",str "running")]
       codeUpdate s running (appendItems "operations" [operation])
       raw<-invokeResponseOnlyObservedWith s running settings rendered (\evidence->locked s $ do
        (work,_)<-D.getWork s (runId running)
        let code=D.workField work "codeExecution"
            updated=D.setWorkField "codeExecution" (put "operations" (arr [if at op "id"==invocation then put "evidence" (toJSON evidence) op else op | op<-items code "operations"]) code) work
        observed<-decodeValue 500 (Effects.observeEvidence invocation evidence (toJSON updated))
        D.saveWork s observed)
       codeGuard s running
       (observed,_)<-D.getWork s (runId running)
       let evidence=listToMaybe [get op "evidence" | op<-items (D.workField observed "codeExecution") "operations",at op "id"==invocation]
       require (maybe False Effects.completeActionFree evidence) 409 "Workflow model response has incomplete or action-bearing evidence; no replay"
       value<-either (failure 422 . T.pack) pure (eitherDecodeStrict (TE.encodeUtf8 raw))
       codeUpdate s running (\code->put "operations" (arr [if at op "id"==invocation then mergeFields [("status",str "completed"),("response",value)] op else op | op<-items code "operations"]) code)
       pure value
 actual<-Workflow.run (get version "workflow") input (select ["reference","workflowModel","workflowPrompt","runtime"] version) loadTrace publish invoke
 validateCodeResult version actual
 let runtime=obj [("status",str "exited"),("exitCode",num 0),("wasRunning",Bool False),("actual",actual),("reference",get version "reference"),("sourceHash",get version "sourceHash"),("inputHash",str (canonicalHash input)),("workflow",Bool True)]
 codeUpdate s running (appendItems "operations" [obj [("id",str (canonicalHash runtime)),("purpose",str "code_execution"),("stage",str "calculate"),("status",str "completed"),("result",runtime)]])
 pure (actual,runtime)

validateCodeResult :: Value -> Value -> IO ()
validateCodeResult version actual = do
 require (BL.length (encode actual)<=65536) 422 "Replacement result exceeds the existing output bound"
 unless ((at actual "status"=="ok" && has actual "value") || (at actual "status"=="rejected" && not (T.null (at actual "error")))) (throwIO (ProvenCodeFault "Candidate returned an invalid result envelope"))
 explanation<-either (failure 409 . T.pack) pure (parseEither candidateExplanation version)
 when ((isJust explanation || has version "inputBinding") && at actual "status"=="rejected") $ failure 422 ("Input rejected: "<>at actual "error")
 when (has version "inputBinding" && isNothing explanation) (void (either (throwIO . ProvenCodeFault . T.pack) pure (parseEither feedContentText (get actual "value"))))

explainCalculation :: Service -> Run -> Value -> D.TaskSpec -> Value -> Value -> Maybe Explanation -> IO (Maybe Text)
explainCalculation s running version spec input actual explanation = forM explanation $ \selected->do
 let context=mergeFields [("contract",toJSON spec) | has version "kitArtifact"] (obj [("instruction",str (explanationInstruction selected)),("acceptedInput",input),("facts",get actual "value"),("result",actual),("criteria",D.criterionContext spec),("reference",get version "reference"),("structureHash",get version "structureHash"),("implementationHash",get (get version "explanation") "implementationHash")])
 (identity,produced)<-invokeCodeModel s running "HybridExplanation" "hybrid_explanation" "hybrid-explanation" context (Just selected)
 let parsed=parseEither narrativeText produced
 codeUpdate s running (\code->put "operations" (arr [if at op "id"==identity then put "status" (str (either (const "failed") (const "completed") parsed)) op else op | op<-items code "operations"]) code)
 text_<-either (failure 422 . T.pack) pure parsed
 codeGuard s running
 pure text_

type CodeAssessment = (Text,SemanticVerdict,D.DecisionResponse,D.Outcome,Value)
assessCodeResult :: Service -> Run -> Value -> CodeRole -> D.TaskSpec -> Value -> Value -> Value -> Maybe Explanation -> Maybe Text -> Maybe Text -> IO CodeAssessment
assessCodeResult s running version role spec input actual runtime explanation narrative fallbackText = do
 codeUpdate s running (mergeFields [("evaluatedResult",actual),("evaluatedExplanation",maybe Null str narrative)])
 (current,_)<-D.getWork s (runId running)
 let replacement=not (null (pairs (get (D.workField current "codeExecution") "fallbacks")))
 let scriptText=fallbackText `mplus` (if has version "inputBinding" && isNothing explanation then Just (at (get actual "value") "text") else Nothing)
 let purpose=if has (toJSON current) "savedAudit" then "repair_saved_result_audit" else if role==CandidateTrial then "code_acceptance_and_activation_audit" else "code_acceptance"
     context=obj ([("contract",toJSON spec),("input",input),("result",actual),("runtime",runtime),("version",version),("initialTrial",Bool (role==CandidateTrial && not replacement)),("resultText",maybe Null str (fallbackText `mplus` narrative `mplus` scriptText)),("resultProvenance",str (if replacement then "fallback" else "implementation")),("savedEvidenceOnly",Bool (has (toJSON current) "savedAudit")),("savedSource",D.workField current "savedAudit")]++concat [[("criteria",D.criterionContext spec),("rubric",get version "rubric"),("hybridResult",obj [("facts",get actual "value"),("explanation",str text_)])] | text_<-maybeToList narrative])
 (invocation,response)<-invokeCodeModel s running (if isJust explanation then "HybridAssessment" else "CodeAssessment") purpose (if isJust explanation then "hybrid-assessment" else "code-assessment") context Nothing
 parsed<-try $ do
  verdict<-field 422 response "verdict"
  body<-case narrative of
   Nothing->pure (mergeFields ([("event",str "Outcome"),("outcome",get response "acceptance")]++[("reply",str produced) | produced<-maybeToList scriptText]) (select ["reply","reason","evidence"] response))
   Just text_->do
    judgments<-D.criterionAssessments spec (get response "criteria")
    require (verdict/=SemanticallyRejected || any (\(CriterionAssessment _ result_ _)->result_==CriterionFail) judgments) 422 "Rejected hybrid assessment has no failed original criterion"
    pure (mergeFields [("event",str "Outcome"),("reply",str text_),("outcome",D.criteriaOutcome spec judgments)] (select ["reason","evidence"] response))
  decision<-parseValue 422 (D.parseDecision D.Execution) body
  case D.decisionBody decision of
   D.ReportOutcome _ outcome->do
    D.validateOutcome decision outcome (NE.toList (D.specCriteria spec))
    require (case verdict of SemanticallyAccepted->D.outcomeStatus outcome==D.Completed; SemanticallyRejected->D.outcomeStatus outcome==D.Unresolved && not (null (D.decisionEvidence decision)); SemanticallyUnknown->D.outcomeStatus outcome==D.Unresolved) 422 "Semantic verdict contradicts the task outcome"
    pure (verdict,decision,outcome)
   _->failure 422 "Code assessment has no outcome"
 case parsed of
  Left (APIError _ reason)->throwIO (VerificationUnknown reason)
  Right (verdict,decision,outcome)->do
   let audit=get response "activationAudit"
       validAudit=case get audit "accepted" of Bool _->not (T.null (T.strip (at audit "reason"))); _->False
       auditFact=if role==CandidateTrial then if validAudit then put "invocation" (str invocation) audit else obj [("status",str "unknown"),("invocation",str invocation),("reason",str "Activation audit is missing or malformed")] else Null
       acceptance=obj [("verdict",toJSON verdict),("outcome",get (D.decisionRaw decision) "outcome"),("invocation",str invocation),("reason",str (D.decisionReason decision))]
   codeUpdate s running (mergeFields [("acceptance",acceptance),("activationAudit",auditFact)])
   when (role==CandidateTrial && not validAudit) (throwIO (VerificationUnknown "Activation audit is missing or malformed"))
   pure (invocation,verdict,decision,outcome,auditFact)


wholeResult :: Service -> Run -> Value -> CodeRole -> D.TaskSpec -> Value -> Value -> Maybe Explanation -> Text -> IO CodeAssessment
wholeResult s running version role spec input runtime explanation reason = do
 replacement<-runFallback s running WholeFallback version spec input reason
 let actual=get replacement "result"; text_=at replacement "text"
 assessCodeResult s running version role spec input actual runtime explanation (if isJust explanation then Just text_ else Nothing) (Just text_)

runFallback :: Service -> Run -> FallbackScope -> Value -> D.TaskSpec -> Value -> Text -> IO Value
runFallback s running scope version spec input reason = do
 operation<-newId
 clock<-getMonotonicTimeNSec
 let key=fallbackScope scope; reference=get version "reference"
 retained<-locked s $ do
  codeGuard s running
  require (clock<runDeadline running) 504 "Occurrence budget is exhausted; fallback cannot start"
  (work,_)<-D.getWork s (runId running)
  let code=D.workField work "codeExecution"; attempts=get code "fallbacks"
  case get attempts key of
   saved | saved/=Null->do
    require (at saved "status"=="completed" && get saved "reference"==reference && at saved "inputHash"==canonicalHash input) 409 "Fallback scope has a failed, unknown or mismatched retained attempt; it cannot be replayed"
    pure (Left (get saved "response"))
   _->do
    require (all (\attempt->at attempt "status"=="completed") (map snd (pairs attempts))) 409 "Failed or unknown fallback cannot escalate to another scope"
    let context=obj [("scope",str key),("fallbackOperation",str operation),("reason",str reason),("contract",toJSON spec),("criteria",D.criterionContext spec),("originalIntent",D.workField work "originalIntent"),("input",input),("version",version),("progress",select ["operations","acceptance","faults","evaluatedResult","evaluatedExplanation"] code),("externalExecution",D.workField work "externalExecution"),("remainingMilliseconds",toJSON ((runDeadline running-clock) `div` 1000000))]
        attempt=obj [("status",str "reserved"),("reason",str reason),("operation",str operation),("reference",reference),("inputHash",str (canonicalHash input)),("context",context)]
    D.saveWork s (D.setWorkField "codeExecution" (mergeFields [("status",str "fallback"),("fallbacks",put key attempt attempts)] code) work)
    pure (Right context)
 case retained of
  Left response->pure response
  Right context->do
   let save fields=codeUpdate s running (\code->put "operations" (arr [if at op "id"==operation then merge op fields else op | op<-items code "operations"]) (put "fallbacks" (put key (merge (get (get code "fallbacks") key) fields) (get code "fallbacks")) code))
   result<-try $ do
    (_,response)<-invokeCodeModel s running (if scope==CalculationFallback then "LocalFallback" else "WholeFallback") (if scope==CalculationFallback then "code_local_fallback" else "code_whole_fallback") "code-fallback" context Nothing
    (work,_)<-D.getWork s (runId running)
    let evidence=maybe Null (`get` "evidence") (find ((==operation) . (`at` "id")) (items (D.workField work "codeExecution") "operations"))
        completeness=get evidence "completeness"
    require (boolean (get evidence "finished") && get evidence "processExit"==num 0 && all (\key_->boolean (get completeness key_)) ["finalSeen","turnEndSeen","streamEndSeen"]) 422 "Fallback response lacks complete terminal evidence"
    require (get response "resolved"==Bool True && not (T.null (T.strip (at response "reason")))) 422 "Fallback did not establish a resolved result"
    require (BL.length (encode response)<=65536) 422 "Fallback output exceeds the existing output bound"
    validateCodeResult version (get response "result") `catch` (\(fault::CodeRunError)->case fault of ProvenCodeFault message->failure 422 message; VerificationUnknown message->failure 422 message)
    require (at (get response "result") "status"=="ok") 422 "Fallback did not produce a successful replacement result"
    when (scope==WholeFallback) $ do
     text_<-either (failure 422 . T.pack) pure (parseEither narrativeText (select ["text"] response))
     when (has version "inputBinding" && not (has version "explanation")) (require (get (get (get response "result") "value") "text"==str text_) 422 "Whole fallback text differs from its feed result")
    codeGuard s running
    save (obj [("status",str "completed"),("response",response)])
    pure response
   case result of
    Right response->pure response
    Left (exception::SomeException)->do
     cancelled<-readIORef (runCancel running)
     save (obj [("status",str (if cancelled then "interrupted" else "failed")),("error",str (D.exceptionText exception))])
     throwIO exception

-- Both useful explanation and final assessment retain one canonical invocation.
invokeCodeModel :: Service -> Run -> Text -> Text -> Text -> Value -> Maybe Explanation -> IO (Text,Value)
invokeCodeModel s running node purpose templateName context explanation = do
 codeGuard s running
 (instruction,envelope)<-case explanation of
  Just selected->explanationTemplates s selected
  Nothing->(,) <$> prompt s templateName <*> prompt s "preparation-envelope"
 let rendered=render envelope [("node",node),("instruction",instruction),("context",jsonText (put "selfId" (toJSON (runOwner running)) context))]
 (ownerWork,_)<-D.getWork s (runId running)
 if has (toJSON ownerWork) "savedAudit" then do
  let stage=node<>":"<>canonicalHash context
  operation<-Provider.operationFor s running node purpose stage rendered Nothing (items (D.workField ownerWork "codeExecution") "operations")
  let identity=at operation "id"
      save value=locked s $ do
       (current,_)<-D.getWork s (runId running)
       let code=D.workField current "codeExecution"
           updated=D.setWorkField "codeExecution" (put "operations" (arr (filter ((/=identity) . (`at` "id")) (items code "operations")++[value])) code) current
           observed=if any Effects.positiveAnomaly (Provider.operationEvidence value) && D.workField updated "routineEffectUnknown"==Null then D.setWorkField "routineEffectUnknown" (obj [("decisionId",str identity),("reason",str "Saved audit provider trace contains unexpected activity")]) updated else updated
       D.saveWork s observed
  codeUpdate s running (put "verification" (obj [("id",str identity)]) . put "status" (str "assessment"))
  (_,response,())<-Provider.validatedResponse s running operation save $ \response->do
   void (field 422 response "verdict" :: IO SemanticVerdict)
   void (parseValue 422 (textFieldP "reason" 8000) response)
   void (field 422 response "evidence" :: IO [Text])
   if node=="HybridAssessment" then do
    spec<-field 422 context "contract"
    void (D.criterionAssessments spec (get response "criteria"))
    else require (case get response "acceptance" of Object _->True; _->False) 422 "Assessment acceptance must be an outcome object"
   when (get context "initialTrial"==Bool True) $ do
    void (field 422 (get response "activationAudit") "accepted" :: IO Bool)
    void (parseValue 422 (textFieldP "reason" 8000) (get response "activationAudit"))
  codeGuard s running
  codeUpdate s running (\code->put "operations" (arr [if at op "id"==identity then put "response" response op else op | op<-items code "operations"]) code)
  pure (identity,response)
 else do
  invocation<-if has context "fallbackOperation" then pure (at context "fallbackOperation") else newId
  started<-now
  let operation=obj ([("id",str invocation),("node",str node),("purpose",str purpose),("status",str "running"),("started",str started),("promptHash",str (sourceHash rendered))]++concat [[("stage",str "explain"),("modelSettings",toJSON (explanationModel selected)),("input",context),("inputHash",str (canonicalHash context)),("reference",get context "reference"),("structureHash",get context "structureHash"),("implementationHash",get context "implementationHash")] | selected<-maybeToList explanation])
      save fields=codeUpdate s running (\code->put "operations" (arr [if at op "id"==invocation then merge op fields else op | op<-items code "operations"]) code)
      phaseFields=if has context "fallbackOperation" then [("status",str "fallback")] else if isJust explanation then [("status",str "explanation")] else [("status",str "assessment"),("verification",obj [("id",str invocation)])]
  codeUpdate s running (mergeFields phaseFields . appendItems "operations" [operation])
  raw<-(maybe (invokeAnalysisObserved s running) (invokeResponseOnlyObservedWith s running . explanationModel) explanation) rendered (\evidence->locked s $ do
   (work,_)<-D.getWork s (runId running)
   let code=D.workField work "codeExecution"
       updated=D.setWorkField "codeExecution" (put "operations" (arr [if at op "id"==invocation then put "evidence" (toJSON evidence) op else op | op<-items code "operations"]) code) work
   observed<-decodeValue 500 (Effects.observeEvidence invocation evidence (toJSON updated))
   D.saveWork s observed)
   `catch` (\(exception::SomeException)->save (obj [("status",str "failed"),("error",str (D.exceptionText exception))]) >> throwIO exception)
  codeGuard s running
  let response=parseJSONText raw
  save (obj [("status",str (if isJust explanation then "validating" else "completed")),("response",response)])
  pure (invocation,response)

commitCodeOutcome :: Service -> Run -> Value -> CodeRole -> Text -> SemanticVerdict -> D.DecisionResponse -> D.Outcome -> Value -> IO ()
commitCodeOutcome s running = commitCodeOutcomeFor s running Nothing

commitCodeOutcomeFor :: Service -> Run -> Maybe Value -> Value -> CodeRole -> Text -> SemanticVerdict -> D.DecisionResponse -> D.Outcome -> Value -> IO ()
commitCodeOutcomeFor s running selected version role invocation verdict response outcome audit = locked s $ do
 codeGuard s running
 decidedAt<-now
 DB.connection s $ \db->SQL.withTransaction db $ do
  rows<-DB.queryRowsOn db "SELECT value FROM workloads ORDER BY rowid" ["value"] []
  works<-traverse (DB.decodeStored . (`at` "value") >=> decodeValue 500) rows
  maintenance<-maybe (failure 409 "Assessment authority disappeared") pure (find ((==runIdText (runId running)) . D.workloadId) works)
  let sourceIdentity=maybe (runId running) (RunId . (`at` "source")) selected
  original<-maybe (failure 409 "Code occurrence disappeared") pure (find ((==runIdText sourceIdentity) . D.workloadId) works)
  forM_ selected $ \proof->do
   (_,_,currentProof)<-Automation.savedAuditSourceOn db (D.workField maintenance "repairPin") (D.workloadId original)
   require (currentProof==proof && D.workField maintenance "savedAudit"==proof && D.workField maintenance "executionGate"==toJSON ExecutionOpen) 409 "Saved audit or its maintenance authority changed before commit"
  let receipt=D.workField maintenance "codeExecution"
      current=case selected of
       Nothing->original
       Just _->D.setWorkField "codeExecution" (mergeFields [("operations",arr (items (D.workField original "codeExecution") "operations"++items receipt "operations")),("acceptance",get receipt "acceptance"),("activationAudit",get receipt "activationAudit"),("verification",get receipt "verification"),("savedAudit",obj [("maintenance",toJSON (runId running)),("source",toJSON sourceIdentity),("proof",fromMaybe Null selected)])] (D.workField original "codeExecution")) original
  let call=NE.head (D.workCalls current)
      reference=get version "reference"
  require (D.workField current "executionGate"==toJSON ExecutionOpen && get (D.workField current "codeExecution") "reference"==reference) 409 "Code occurrence was stopped or rebound"
  spec<-D.executionContract current call >>= maybe (failure 409 "Accepted task is missing") pure
  typedReference<-decodeValue 409 reference
  void (D.resolveCandidateFrom works (runOwner running) (D.workField current "routineSnapshot") spec typedReference)
  definitions<-DB.queryRowsOn db "SELECT value FROM scheduled_routines ORDER BY sequence" ["value"] [] >>= traverse (DB.decodeStored . (`at` "value"))
  let CandidateReference _ preparationId _=typedReference
  sourcePreparation<-maybe (failure 409 "Candidate preparation disappeared") pure (find ((==runIdText preparationId) . D.workloadId) works)
  pin<-decodeValue 409 (D.workField sourcePreparation "preparationPin")
  feedback<-DB.routineOperationsOn db
  let modelAccepted=verdict==SemanticallyAccepted && D.outcomeStatus outcome==D.Completed
      proposedOutcome=mergeFields [("reply",get (D.decisionRaw response) "reply"),("evidence",toJSON (D.decisionEvidence response)),("author",toJSON (runOwner running))] (get (D.decisionRaw response) "outcome")
  contentCommit<-if modelAccepted && not (isJust selected && Effects.profileBound (toJSON current)) then Effects.acceptContentOn db (toJSON (D.setWorkField "outcome" proposedOutcome current)) invocation (at proposedOutcome "reply") else pure (Effects.ContentCommitted (toJSON current))
  (contentWork,contentAccepted)<-case contentCommit of
   Effects.ContentCommitted value->(,) <$> decodeValue 500 value <*> pure modelAccepted
   Effects.ContentConflict value->(,) <$> decodeValue 500 value <*> pure False
  let snapshot=D.workField current "routineSnapshot"
      definition=find (\d->get d "id"==get snapshot "id" && get d "owner"==toJSON (runOwner running)) definitions
      applicable=maybe False (\d->not (boolean (get d "deleted")) && get d "contractGeneration"==get snapshot "contractGeneration" && DB.analysisFeedbackIds feedback d (map toJSON works) (analysisSources pin)==analysisFeedback pin && Kit.executionAllowed d version (map toJSON works) && (not (has version "expectedBinding") || Kit.bindingCurrent d (get version "expectedBinding"))) definition
      accepted=contentAccepted
      fallbackUsed=not (null (pairs (get (D.workField current "codeExecution") "fallbacks")))
      activating=not fallbackUsed && role==CandidateTrial && accepted && get audit "accepted"==Bool True && applicable
      activationReason=if fallbackUsed then "Fallback completion does not establish implementation health or authorize activation" else if role==ActiveCode then "Previously activated version; no new activation required" else if not applicable then "Current routine contract or source feedback changed; candidate was not activated" else if modelAccepted && not contentAccepted then "Source cursor changed; content and candidate activation were not accepted" else if not accepted then "Task result was not accepted; candidate was not activated" else if get audit "accepted"/=Bool True then "Activation audit did not approve this candidate" else at audit "reason"
      activation=obj [("status",str (if role==ActiveCode then "not_required" else if activating then "activated" else "not_activated")),("reference",reference),("run",toJSON (sourceIdentity)),("invocation",str invocation),("decidedAt",str decidedAt),("reason",str activationReason)]
      saved=if modelAccepted && not contentAccepted then mergeFields [("status",toJSON D.Unresolved),("hostReason",str "Source cursor changed concurrently; model proposal retained without content acceptance")] proposedOutcome else proposedOutcome
      assessment=put "decisionId" (str invocation) (assessmentValue (if accepted then AssessmentAccepted else if verdict==SemanticallyRejected then AssessmentRejected else AssessmentUnresolved) "model_contract_assessment" (D.decisionReason response))
      record=obj [("decisionId",str invocation),("callId",toJSON (sourceIdentity)),("node",str (if has version "explanation" then "HybridAssessment" else "CodeAssessment")),("accepted",Bool True),("response",D.decisionRaw response),("assessment",assessment),("evidenceOperation",str invocation)]
      finished=D.setWorkField "decisions" (arr (if any (\entry->get entry "decisionId"==str invocation) (D.workItems contentWork "decisions") then [if get entry "decisionId"==str invocation then merge entry record else entry | entry<-D.workItems contentWork "decisions"] else D.workItems contentWork "decisions"++[record])) (D.setWorkField "outcome" saved (D.setWorkField "codeExecution" (mergeFields [("status",str "completed"),("completedViaFallback",Bool (fallbackUsed && accepted)),("activation",activation)] (D.workField contentWork "codeExecution")) contentWork))
      complete=finished {D.workCalls=fmap (\c->c {D.callState=if accepted then D.CallCompleted else D.CallUnresolved,D.callOutput=Just (at saved "reply")}) (D.workCalls finished)}
  when activating $ forM_ definition $ \d->do
   -- Content acceptance just advanced sourceState in this same transaction.
   latest<-Effects.definitionOn db (at d "id")
   void (DB.saveRoutineOn db (put "activeCode" (mergeFields [("status",str "active"),("contractGeneration",get snapshot "contractGeneration")] activation) latest))
  when (role==CandidateTrial) $ do
   let CandidateReference _ preparation _=typedReference
   source<-maybe (failure 409 "Trial preparation disappeared") pure (find ((==runIdText preparation) . D.workloadId) works)
   let prepared=D.workField source "preparation"; claim=get prepared "trial"
   require (get claim "run"==toJSON (sourceIdentity) && get claim "reference"==reference) 409 "Candidate trial claim changed"
   DB.putValueOn db DB.Workloads (D.workloadId source) (toJSON (D.setWorkField "preparation" (put "trial" (merge claim activation) prepared) source))
  let auditRejected=isJust selected && verdict==SemanticallyRejected
      fault=obj [("scope",str "composition"),("reference",reference),("reason",str (D.decisionReason response)),("assessment",str invocation)]
      final=if auditRejected then D.setWorkField "codeExecution" (appendItems "faults" [fault] (D.workField complete "codeExecution")) complete else complete
  D.saveWorkOn s db final
  when auditRejected $ do
   Automation.recordRepairFaultOn db final reference (D.decisionReason response)
   forM_ definition $ \d->do
    latest<-Effects.definitionOn db (at d "id")
    when (get (get latest "activeCode") "reference"==reference) $
     void (DB.saveRoutineOn db (put "activeCode" (mergeFields [("status",str "suspect"),("reason",str (D.decisionReason response)),("failedRun",toJSON sourceIdentity)] (get latest "activeCode")) latest))

markCodeFault :: Service -> Run -> Text -> IO ()
markCodeFault s running reason = locked s $ do
 (work,_)<-D.getWork s (runId running)
 let reference=get (D.workField work "codeExecution") "reference"
 cancelled<-readIORef (runCancel running)
 allowed<-DB.occurrenceAllowed s (D.workField work "origin")
 unless (cancelled || not allowed) $ DB.connection s $ \db->SQL.withTransaction db $ do
  Automation.recordRepairFaultOn db work reference reason
  definitions<-DB.queryRowsOn db "SELECT value FROM scheduled_routines ORDER BY sequence" ["value"] [] >>= traverse (DB.decodeStored . (`at` "value"))
  forM_ [d | d<-definitions,get d "id"==get (D.workField work "routineSnapshot") "id",get d "owner"==toJSON (runOwner running),get (get d "activeCode") "reference"==reference] $ \definition->
   void (DB.saveRoutineOn db (put "activeCode" (mergeFields [("status",str "suspect"),("reason",str reason),("failedRun",toJSON (runId running))] (get definition "activeCode")) definition))

recoverCode :: Service -> IO ()
recoverCode s = do
 works<-D.readWorks s
 forM_ works $ \work->when (has (toJSON work) "codeExecution") $ do
  let code=D.workField work "codeExecution"; container=get code "container"
  recovered<-if has container "name" && at container "status"/="removed" then do
   result<-try (Code.reconcile container) :: IO (Either APIError ())
   pure (case result of Right ()->mergeFields [("status",str "interrupted"),("container",put "status" (str "removed") container)] code; Left (APIError _ reason)->mergeFields [("status",str "cleanup_pending"),("error",str reason)] code)
   else pure (if at code "status" `elem` ["running","explanation","assessment","fallback"] then put "status" (str "interrupted") code else code)
  let terminal=if at recovered "status"=="cleanup_pending" then "uncertain" else "interrupted"
      fallbackState=if not (has recovered "fallbacks") then recovered else put "fallbacks" (obj [(key,if at attempt "status"=="reserved" then put "status" (str "interrupted") attempt else attempt) | (key,attempt)<-pairs (get recovered "fallbacks")]) recovered
      finalized=put "operations" (arr [if at op "status" `elem` ["running","validating"] then put "status" (str terminal) op else op | op<-items recovered "operations"]) fallbackState
  when (finalized/=code) (D.saveWork s (D.setWorkField "codeExecution" finalized work))
  -- Resume only the gap between confirmed nodes. An admitted node without a
  -- complete receipt, a started assessment or any Stop remains terminal.
  let receipts=pairs (get (get finalized "workflowRun") "nodes")
      safe=not (null receipts) && all ((=="completed") . (`at` "status") . snd) receipts
        && all (\op->at op "purpose" `elem` ["workflow_response","code_execution"] && at op "status"=="completed") (items finalized "operations")
        && not (has finalized "evaluatedResult") && at recovered "status"/="cleanup_pending"
        && D.workField work "executionGate"==toJSON ExecutionOpen && not (has (toJSON work) "repairPin")
  when safe $ do
   allowed<-DB.occurrenceAllowed s (D.workField work "origin")
   let owner=agentIdText (D.callOwner (NE.head (D.workCalls work)))
       identity=RunId (D.workloadId work)
   state<-agentState s owner
   case find ((==identity) . turnId) (agentTurns state) of
    Just turn | allowed && turnStatus turn==Interrupted && not (any ((=="cancel") . (`at` "action")) (items (turnField turn "managed") "commands"))->do
     time<-getCurrentTime
     when (maybe False (>time) (parseTime (at finalized "workflowDeadline"))) $ do
      (current,_)<-D.getWork s identity
      D.saveWork s ((D.setWorkField "codeExecution" (put "status" (str "queued") finalized) current) {D.workCalls=fmap (\call->call {D.callState=D.CallQueued}) (D.workCalls current)})
      saveState s owner (mapTurns (\item->if turnId item==identity then item {turnStatus=Queued} else item) state)
    _->pure ()

publish :: Service -> Text -> Pulse -> IO ()
publish s a tick = forM_ [1,2] $ \slot -> do
 state <- agentState s a
 let document=stateJSON state
     scheduled=pulseFrequency tick==Bph60
     eligible item=has (get item "origin") "routine" == scheduled
 case find eligible (items document "output_buffer") of
  Nothing -> pure ()
  Just item -> do
   time <- now
   ids <- field 500 item "members" :: IO [RunId]
   let sequence=int (get document "output_sequence")+1
       origin=get item "origin"
       updated=mapTurns (\t -> if turnId t `elem` ids then (withTurnFields (mergeFields [("output_at",str time),("output_order",num sequence)]) t) { turnStatus = Done } else t) state
       messages=if has origin "group" || any (\m -> get m "turn"==get item "turn" && at m "role"=="agent") (items document "chat") then items document "chat" else items document "chat"++[obj [("role",str "agent"),("content",get item "content"),("turn",get item "turn"),("time",str time),("origin",origin)]]
   saveState s a $ mapStateFields (mergeFields ([("output_buffer",arr (filter (/=item) (items document "output_buffer"))),("output_sequence",num sequence),("chat",arr messages)] ++ [("last_output",get item "content") | not (has origin "group")])) updated
   DB.pulseCall s tick a "chatOutput" slot [at origin "group" | has origin "group"]
   DB.project s a
   trimConversation s a
admit :: Service -> Text -> Pulse -> IO ()
admit s a tick = do
 runs <- readMVar (serviceRuns s)
 state <- agentState s a
 let owners=nub (map runOwner (M.elems runs))
     mine=filter ((==AgentId a) . runOwner) (M.elems runs)
     available=[n | n<-[1,2],n `notElem` map runSlot mine]
     scheduled=pulseFrequency tick==Bph60
     queued=[t | t<-agentTurns state,turnStatus t==Queued,boolean (turnField t "batchable"),has (turnField t "origin") "routine"==scheduled]
     legacyCandidates=[t | t<-agentTurns state,turnStatus t==Queued,not (boolean (turnField t "batchable"))]
 legacy<-filterM (Automation.providerDue s) legacyCandidates
 when (AgentId a `elem` owners || length owners<4) $ case (legacy,available,NE.nonEmpty queued) of
  (first:_,_,_) | null mine && not scheduled -> launch s a (first :| []) tick 0 False
  (_,slot:_,Just batch) | all ((/=0) . runSlot) mine -> launch s a (if scheduled then NE.head batch :| [] else batch) tick slot True
  _ -> pure ()
onPulse :: Service -> Pulse -> IO ()
onPulse s tick = do
 stopRoutineRuns s
 R.dispatch s tick
 when (pulseFrequency tick==Bph60 || (pulseFrequency tick==Bpm60 && even (pulseNumber tick))) $ do
  D.pendingWorks s
  Automation.dispatch s
  Automation.dispatchRepairs s
  G.reconcileGroups s
  G.dispatchGroups s
  rows <- DB.agents s
  forM_ rows $ \row -> do
   let a=at row "id"
   inactive <- retired s a
   unless inactive $ (do
    restoreManaged s a
    current <- agentState s a
    activeRuns <- readMVar (serviceRuns s)
    forM_ (agentTurns current) $ \t -> when (has (toJSON t) "managed" && boolean (turnField t "cancel_requested")) $
     forM_ (M.lookup (turnId t) activeRuns) $ \run -> writeIORef (runCancel run) True
    let pending=Managed.commands current
    forM_ pending $ \(identity,command) -> void (turnCommand s a identity (at command "action") (Just command))
     `catch` (\e@(APIError code _) -> if code==409 then pure () else throwIO e)
    publish s a tick
    admit s a tick
    when (pulseFrequency tick==Bph60) $ admit s a tick)
    `catches` [Handler (\e@(APIError code _) -> if code==409 then pure () else throwIO e),Handler (\(_::IOException) -> pure ())]
-- Replay durable occurrence fences through the existing call/process runtime.
stopRoutineRuns :: Service -> IO ()
stopRoutineRuns s = do
 works <- D.readWorks s
 blockedWorks <- filterM (fmap not . DB.occurrenceAllowed s . (`D.workField` "origin")) works
 let owners=nub [agentIdText (D.callOwner call) | work<-blockedWorks,call<-NE.toList (D.workCalls work)]
 forM_ owners $ \owner -> do
  state <- agentState s owner
  blocked <- filterM (fmap not . D.callAllowed s) [t | t<-agentTurns state,turnStatus t `elem` [Queued,Running,OutputPending]]
  unless (null blocked) $ do
   live <- readMVar (serviceRuns s)
   forM_ blocked $ \t -> forM_ (M.lookup (turnId t) live) $ \r -> writeIORef (runCancel r) True
   let ids=map turnId blocked
       update t | turnId t `elem` ids && turnStatus t/=Running = t {turnStatus=Cancelled}
                | otherwise = t
   let updated=mapStateFields (put "output_buffer" (arr [b | b<-items (stateJSON state) "output_buffer",get b "turn" `notElem` map toJSON ids])) (mapTurns update state)
   when (stateJSON updated/=stateJSON state) $ do
    saveState s owner updated
    DB.project s owner
 D.reconcileWorks s

milliseconds :: IO Int
milliseconds = fromIntegral . (`div` 1000000) <$> getMonotonicTimeNSec
startExecution :: Service -> IO ()
startExecution s = modifyMVar_ (serviceWorker s) $ \existing -> case existing of
 Just _ -> pure existing
 Nothing -> Just <$> async loop
 where
 loop = do
  t <- milliseconds
  deadlines <- newIORef (M.fromList [(f,t+periodMillis f) | f <- [minBound..maxBound]])
  let step = do
       stopped <- readIORef (serviceStopping s)
       unless stopped $ do
        locked s $ do
         config <- corporaSettings s
         pulse <- readIORef (servicePulse s)
         let active=corporaStatus config==Active
         writeIORef (servicePulse s) (put "running" (Bool active) pulse)
         time <- milliseconds
         if not active then writeIORef deadlines (M.fromList [(f,time+periodMillis f) | f <- [minBound..maxBound]]) else forM_ [minBound..maxBound] $ \frequency -> do
          let interval=periodMillis frequency; key=wireName frequency
          due <- fromMaybe time . M.lookup frequency <$> readIORef deadlines
          when (time>=due) $ do
           let elapsed=(time-due) `div` interval+1
           modifyIORef' deadlines (M.insert frequency (due+elapsed*interval))
           current <- readIORef (servicePulse s)
           let number=int (get (get current "numbers") key)+elapsed; numbers=put key (num number) (get current "numbers"); identity=at current "id"
           writeIORef (servicePulse s) (put "numbers" numbers current)
           DB.saveSingleton s DB.SystemPulse (obj [("pulse",str identity),("numbers",numbers)])
           onPulse s (Pulse identity frequency number)
        threadDelay 50000
        step
  step
stopRuns :: Service -> IO ()
stopRuns s = readMVar (serviceRuns s) >>= mapM_ (\run -> writeIORef (runCancel run) True) . M.elems
