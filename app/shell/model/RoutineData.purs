module Sapiens.RoutineData
  ( RoutineDefinition, RoutineHistoryPage, RoutineRunDetail, RunSummary
  , TaskSpec, CompletionCriterion(..), Outcome, OutcomeStatus(..), CriterionResult(..)
  , ExactResultRef(..), SavedRunResult, ResultNotification(..), ResultRejection, ResultReviewer(..)
  , PolicySelection(..), MissedDuePolicy(..), RunTrigger(..), CallState(..), ExecutionStatus(..)
  , RoutineControl(..), ControlReceipt(..), StopReceipt(..), controlRoutine, controlAction, controlLabel, decodeControlReceipt
  , RoutineCondition(..), ProblemRef, ProblemReason, ProblemKind(..), AppliedRepair, RepairEntry, RepairVerification(..), decodeCondition
  , decodeHistory, decodeRun, triggerLabel, policyLabel, callLabel, executionLabel, outcomeLabel
  ) where

import Prelude
import Data.Argonaut.Core (Json, jsonNull, toObject, toArray, toString, toBoolean, toNumber)
import Data.Array as A
import Data.Either (Either(..))
import Data.Int as I
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String as S
import Data.Traversable (traverse)
import Foreign.Object as O
import Sapiens.Data as D
import Sapiens.Navigation as N

data MissedDuePolicy = RecoverEach | RunLatest | SkipMissed
derive instance eqMissedDuePolicy :: Eq MissedDuePolicy

data PolicySelection = AwaitingSelection | PolicySelected MissedDuePolicy
derive instance eqPolicySelection :: Eq PolicySelection

data RunTrigger = ScheduledTrigger | ManualTrigger String
derive instance eqRunTrigger :: Eq RunTrigger

-- A Work call and its physical execution have different wire states.
data CallState = CallQueued | CallRunning | CallCompleted | CallUnresolved | CallDelegated | CallWaiting | CallFailed | CallInterrupted | CallCancelled
derive instance eqCallState :: Eq CallState

data ExecutionStatus = ExecutionQueued | ExecutionRunning | ExecutionOutputPending | ExecutionDone | ExecutionFailed | ExecutionInterrupted | ExecutionCancelled | ExecutionWarning
derive instance eqExecutionStatus :: Eq ExecutionStatus

data CompletionCriterion = Required String String | Conditional String String String
derive instance eqCompletionCriterion :: Eq CompletionCriterion

type TaskSpec =
  { objective :: String, inputs :: Array String, expectedOutputs :: Array String
  , completionCriteria :: Array CompletionCriterion }

data OutcomeStatus = Completed | Unresolved
derive instance eqOutcomeStatus :: Eq OutcomeStatus

data CriterionResult = Satisfied String String | NotApplicable String String
derive instance eqCriterionResult :: Eq CriterionResult

type Outcome =
  { status :: OutcomeStatus, criterionResults :: Array CriterionResult, artifacts :: Array String }

data ExactResultRef = PersonalDecision String | GroupResult N.RunId Int
derive instance eqExactResultRef :: Eq ExactResultRef

data ResultNotification = Notify | Quiet
derive instance eqResultNotification :: Eq ResultNotification

data ResultReviewer = AdminReviewer | SapiReviewer String
derive instance eqResultReviewer :: Eq ResultReviewer

type ResultRejection = { rejectedAt :: String, rejectedBy :: ResultReviewer, reason :: String }

type RoutineDefinition =
  { ref :: N.RoutineRef, prompt :: String, minutes :: Int, nextDue :: String
  , paused :: Boolean, policy :: PolicySelection, runCount :: Int
  , deletedAt :: Maybe String, specification :: Maybe TaskSpec }

type RunSummary =
  { id :: N.RunId, routine :: N.RoutineId, revision :: String, created :: String, trigger :: RunTrigger
  , due :: Maybe String, pending :: Boolean, stoppedAt :: Maybe String
  , callState :: Maybe CallState, executionStatus :: Maybe ExecutionStatus, executor :: Maybe String
  , live :: Boolean, finished :: Boolean, result :: Maybe String, error :: Maybe String
  , stale :: Boolean, attempt :: Maybe Int, taskRevision :: Maybe Int, groupSettled :: Maybe Boolean
  , outcome :: Maybe Outcome, rejection :: Maybe ResultRejection, resultTarget :: Maybe ExactResultRef
  , problem :: Maybe ProblemRef, reason :: Maybe ProblemReason, notification :: ResultNotification }

type SavedRunResult =
  { reference :: ExactResultRef, reply :: Maybe String, error :: Maybe String, stale :: Boolean
  , notification :: ResultNotification, outcome :: Maybe Outcome, rejection :: Maybe ResultRejection }

type RoutineHistoryPage =
  { routine :: RoutineDefinition, runs :: Array RunSummary, current :: Maybe RunSummary
  , latest :: Maybe RunSummary, next :: Maybe N.RunId }

type RoutineRunDetail =
  { summary :: RunSummary, snapshot :: RoutineDefinition, results :: Array SavedRunResult, repairs :: Array RepairEntry }

decodeHistory :: N.RoutineRef -> Json -> Either String RoutineHistoryPage
decodeHistory ref value = do
  fields <- objectValue value
  routine <- field "routine" (decodeRoutine ref) fields
  runs <- field "runs" (arrayOf $ decodeSummary ref) fields
  current <- nullable "current" (decodeSummary ref) fields
  latest <- nullable "latest" (decodeSummary ref) fields
  next <- nullable "next" runIdentity fields
  check (A.length runs <= 20 && A.length (A.nub $ map (N.runId <<< _.id) runs) == A.length runs) "Invalid Routine history page"
  check (fromMaybe true (not <<< _.finished <$> current)) "Current Run is already finished"
  check (fromMaybe true (_.finished <$> latest)) "Latest Run is not finished"
  case next of
    Just cursor -> check ((_.id <$> A.last runs) == Just cursor) "History cursor does not match its page"
    Nothing -> pure unit
  pure { routine, runs, current, latest, next }

decodeRun :: N.RunRef -> Json -> Either String RoutineRunDetail
decodeRun ref value = do
  fields <- objectValue value
  summary <- decodeSummary ref.routine value
  check (summary.id == ref.id) "Run response does not match the requested Run"
  snapshot <- field "snapshot" (decodeRoutine ref.routine) fields
  results <- field "results" (arrayOf $ decodeSavedResult ref.routine.owner) fields
  repairs <- fromMaybe [] <$> optional "repairs" (arrayOf decodeRepairEntry) fields
  check (A.all (\entry -> entry.repair.problem.run == ref.id) repairs) "Repair history belongs to another Run"
  pure { summary, snapshot, results, repairs }

decodeRoutine :: N.RoutineRef -> Json -> Either String RoutineDefinition
decodeRoutine ref value = do
  fields <- objectValue value
  identity <- field "id" nonempty fields
  owner <- field "owner" nonempty fields
  check (identity == N.routineId ref.id && owner == N.ownerId ref.owner) "Routine response does not match its owner and ID"
  prompt <- field "prompt" stringValue fields
  minutes <- field "minutes" integerValue fields
  check (minutes >= 1 && minutes <= 525600) "Invalid Routine interval"
  nextDue <- field "nextDue" nonempty fields
  paused <- field "paused" booleanValue fields
  policy <- field "policySelection" decodePolicy fields
  runCount <- field "runCount" nonnegative fields
  deletedAt <- optional "deletedAt" nonempty fields
  specification <- optional "specification" decodeSpecification fields
  pure { ref, prompt, minutes, nextDue, paused, policy, runCount, deletedAt, specification }

decodeSummary :: N.RoutineRef -> Json -> Either String RunSummary
decodeSummary ref value = do
  fields <- objectValue value
  id <- field "id" runIdentity fields
  routine <- N.RoutineId <$> field "routine" nonempty fields
  check (routine == ref.id) "Run response belongs to another Routine"
  created <- field "created" nonempty fields
  trigger <- field "trigger" decodeTrigger fields
  due <- nullable "due" nonempty fields
  pending <- field "pending" booleanValue fields
  stoppedAt <- nullable "stoppedAt" nonempty fields
  callState <- nullable "callState" decodeCallState fields
  executionStatus <- nullable "executionStatus" decodeExecutionStatus fields
  executor <- nullable "executor" nonempty fields
  live <- field "live" booleanValue fields
  finished <- field "finished" booleanValue fields
  result <- nullable "result" stringValue fields
  error <- nullable "error" stringValue fields
  stale <- field "stale" booleanValue fields
  attempt <- nullable "attempt" nonnegative fields
  taskRevision <- nullable "taskRevision" nonnegative fields
  groupSettled <- nullable "groupSettled" booleanValue fields
  outcome <- nullable "outcome" decodeOutcome fields
  rejection <- nullable "rejection" decodeRejection fields
  resultTarget <- nullable "resultTarget" (decodeResultRef ref.owner) fields
  problem <- nullable "problem" decodeProblem fields
  reason <- nullable "reason" decodeReason fields
  notification <- fromMaybe Notify <$> optional "notification" decodeNotification fields
  check (fromMaybe true ((id == _) <<< _.run <$> problem)) "Problem belongs to another Run"
  pure { id, routine, revision:D.encode value, created, trigger, due, pending, stoppedAt
       , callState, executionStatus, executor, live, finished, result, error, stale, attempt
       , taskRevision, groupSettled, outcome, rejection, resultTarget, problem, reason, notification }

decodeSavedResult :: N.OwnerRef -> Json -> Either String SavedRunResult
decodeSavedResult owner value = do
  fields <- objectValue value
  reference <- field "result" (decodeResultRef owner) fields
  reply <- nullable "reply" stringValue fields
  error <- nullable "error" stringValue fields
  stale <- fromMaybe false <$> nullable "stale" booleanValue fields
  notification <- fromMaybe Notify <$> optional "notification" decodeNotification fields
  outcome <- nullable "outcome" decodeOutcome fields
  rejection <- nullable "rejection" decodeRejection fields
  pure { reference, reply, error, stale, notification, outcome, rejection }

decodeSpecification :: Json -> Either String TaskSpec
decodeSpecification value = do
  fields <- objectValue value
  objective <- field "objective" nonempty fields
  inputs <- field "inputs" (arrayOf stringValue) fields
  expectedOutputs <- field "expectedOutputs" (arrayOf nonempty) fields
  completionCriteria <- field "completionCriteria" (arrayOf decodeCriterion) fields
  check (not (A.null expectedOutputs) && not (A.null completionCriteria)) "Task specification has no outputs or completion criteria"
  pure { objective, inputs, expectedOutputs, completionCriteria }

decodeCriterion :: Json -> Either String CompletionCriterion
decodeCriterion value = do
  fields <- objectValue value
  id <- field "id" nonempty fields
  requirement <- field "requirement" nonempty fields
  applicability <- field "applicability" objectValue fields
  kind <- field "kind" stringValue applicability
  case kind of
    "Required" -> pure $ Required id requirement
    "Conditional" -> do
      condition <- field "condition" nonempty applicability
      pure $ Conditional id condition requirement
    _ -> Left "Unknown completion criterion applicability"

decodeOutcome :: Json -> Either String Outcome
decodeOutcome value = do
  fields <- objectValue value
  status <- field "status" decodeOutcomeStatus fields
  criterionResults <- field "criterionResults" (arrayOf decodeCriterionResult) fields
  artifacts <- field "artifacts" (arrayOf stringValue) fields
  pure { status, criterionResults, artifacts }

decodeOutcomeStatus :: Json -> Either String OutcomeStatus
decodeOutcomeStatus value = stringValue value >>= case _ of
  "Completed" -> Right Completed
  "Unresolved" -> Right Unresolved
  _ -> Left "Unknown Run outcome"

decodeCriterionResult :: Json -> Either String CriterionResult
decodeCriterionResult value = do
  fields <- objectValue value
  id <- field "id" nonempty fields
  evidence <- field "evidence" nonempty fields
  kind <- field "result" stringValue fields
  case kind of
    "Satisfied" -> pure $ Satisfied id evidence
    "NotApplicable" -> pure $ NotApplicable id evidence
    _ -> Left "Unknown criterion result"

decodeResultRef :: N.OwnerRef -> Json -> Either String ExactResultRef
decodeResultRef owner value = do
  fields <- objectValue value
  case owner of
    N.PersonalOwner _ -> do
      check (not $ O.member "request" fields || O.member "revision" fields) "Unexpected Group result reference"
      PersonalDecision <$> field "decision" nonempty fields
    N.GroupOwner _ -> do
      check (not $ O.member "decision" fields) "Unexpected Personal result reference"
      request <- field "request" runIdentity fields
      revision <- field "revision" nonnegative fields
      pure $ GroupResult request revision

decodeRejection :: Json -> Either String ResultRejection
decodeRejection value = do
  fields <- objectValue value
  rejectedAt <- field "rejectedAt" nonempty fields
  rejectedBy <- field "rejectedBy" decodeReviewer fields
  reason <- field "reason" nonempty fields
  pure { rejectedAt, rejectedBy, reason }

decodeReviewer :: Json -> Either String ResultReviewer
decodeReviewer value = do
  fields <- objectValue value
  kind <- field "kind" stringValue fields
  case kind of
    "admin" -> pure AdminReviewer
    "sapi" -> SapiReviewer <$> field "id" nonempty fields
    _ -> Left "Unknown result reviewer"

decodeNotification :: Json -> Either String ResultNotification
decodeNotification value = stringValue value >>= case _ of
  "notify" -> Right Notify
  "quiet" -> Right Quiet
  _ -> Left "Unknown result notification policy"

decodePolicy :: Json -> Either String PolicySelection
decodePolicy value = do
  fields <- objectValue value
  kind <- field "kind" stringValue fields
  case kind of
    "awaiting_selection" -> pure AwaitingSelection
    "selected" -> PolicySelected <$> field "policy" decodeMissedPolicy fields
    _ -> Left "Unknown Routine policy selection"

decodeMissedPolicy :: Json -> Either String MissedDuePolicy
decodeMissedPolicy value = stringValue value >>= case _ of
  "recover_each" -> Right RecoverEach
  "run_latest" -> Right RunLatest
  "skip_missed" -> Right SkipMissed
  _ -> Left "Unknown missed-run policy"

decodeTrigger :: Json -> Either String RunTrigger
decodeTrigger value = do
  fields <- objectValue value
  kind <- field "kind" stringValue fields
  case kind of
    "scheduled" -> pure ScheduledTrigger
    "manual" -> ManualTrigger <$> field "request" nonempty fields
    _ -> Left "Unknown Run trigger"

decodeCallState :: Json -> Either String CallState
decodeCallState value = stringValue value >>= case _ of
  "Queued" -> Right CallQueued
  "Running" -> Right CallRunning
  "Completed" -> Right CallCompleted
  "Unresolved" -> Right CallUnresolved
  "Delegated" -> Right CallDelegated
  "WaitingForAdmin" -> Right CallWaiting
  "Failed" -> Right CallFailed
  "Interrupted" -> Right CallInterrupted
  "Cancelled" -> Right CallCancelled
  _ -> Left "Unknown Run call state"

decodeExecutionStatus :: Json -> Either String ExecutionStatus
decodeExecutionStatus value = stringValue value >>= case _ of
  "queued" -> Right ExecutionQueued
  "running" -> Right ExecutionRunning
  "output_pending" -> Right ExecutionOutputPending
  "done" -> Right ExecutionDone
  "failed" -> Right ExecutionFailed
  "interrupted" -> Right ExecutionInterrupted
  "cancelled" -> Right ExecutionCancelled
  "warning" -> Right ExecutionWarning
  _ -> Left "Unknown Run execution status"

triggerLabel :: RunTrigger -> String
triggerLabel = case _ of
  ScheduledTrigger -> "Scheduled"
  ManualTrigger _ -> "Manual"

policyLabel :: PolicySelection -> String
policyLabel = case _ of
  AwaitingSelection -> "Waiting for missed-run instructions"
  PolicySelected RecoverEach -> "Recover each missed run"
  PolicySelected RunLatest -> "Run once for the latest missed time"
  PolicySelected SkipMissed -> "Skip missed runs"

callLabel :: CallState -> String
callLabel = case _ of
  CallQueued -> "Queued"
  CallRunning -> "Running"
  CallCompleted -> "Completed"
  CallUnresolved -> "Needs attention"
  CallDelegated -> "Delegated"
  CallWaiting -> "Needs input"
  CallFailed -> "Failed"
  CallInterrupted -> "Interrupted"
  CallCancelled -> "Cancelled"

executionLabel :: ExecutionStatus -> String
executionLabel = case _ of
  ExecutionQueued -> "Queued"
  ExecutionRunning -> "Running"
  ExecutionOutputPending -> "Awaiting output"
  ExecutionDone -> "Finished"
  ExecutionFailed -> "Failed"
  ExecutionInterrupted -> "Interrupted"
  ExecutionCancelled -> "Cancelled"
  ExecutionWarning -> "Finished with warning"

outcomeLabel :: OutcomeStatus -> String
outcomeLabel = case _ of
  Completed -> "Completed"
  Unresolved -> "Needs attention"

type Decoder a = Json -> Either String a

objectValue :: Decoder (O.Object Json)
objectValue value = case toObject value of
  Just fields -> Right fields
  Nothing -> Left "Expected an object"

field :: forall a. String -> Decoder a -> O.Object Json -> Either String a
field key decoder fields = case O.lookup key fields of
  Nothing -> Left $ "Missing " <> key
  Just value -> case decoder value of
    Left error -> Left $ key <> ": " <> error
    Right decoded -> Right decoded

-- Absent optional fields are legacy data; a present invalid value is an error.
optional :: forall a. String -> Decoder a -> O.Object Json -> Either String (Maybe a)
optional key decoder fields = case O.lookup key fields of
  Nothing -> Right Nothing
  Just _ -> Just <$> field key decoder fields

nullable :: forall a. String -> Decoder a -> O.Object Json -> Either String (Maybe a)
nullable key decoder fields = case O.lookup key fields of
  Just value | value /= jsonNull -> Just <$> field key decoder fields
  _ -> Right Nothing

arrayOf :: forall a. Decoder a -> Decoder (Array a)
arrayOf decoder value = case toArray value of
  Just values -> traverse decoder values
  Nothing -> Left "Expected an array"

stringValue :: Decoder String
stringValue value = case toString value of
  Just text -> Right text
  Nothing -> Left "Expected text"

nonempty :: Decoder String
nonempty value = do
  text <- stringValue value
  check (S.trim text /= "") "Expected nonempty text"
  pure text

booleanValue :: Decoder Boolean
booleanValue value = case toBoolean value of
  Just flag -> Right flag
  Nothing -> Left "Expected a Boolean"

integerValue :: Decoder Int
integerValue value = case toNumber value >>= I.fromNumber of
  Just number -> Right number
  Nothing -> Left "Expected an integer"

nonnegative :: Decoder Int
nonnegative value = do
  number <- integerValue value
  check (number >= 0) "Expected a nonnegative integer"
  pure number

runIdentity :: Decoder N.RunId
runIdentity value = N.RunId <$> nonempty value

check :: Boolean -> String -> Either String Unit
check valid message = if valid then Right unit else Left message

-- Every mutation captures its target before the request leaves the UI.
data RoutineControl = PauseSchedule N.RoutineRef | ResumeSchedule N.RoutineRef | StopRun N.RunRef | DeleteRoutine N.RoutineRef
derive instance eqRoutineControl :: Eq RoutineControl

data StopReceipt = Stopping | Stopped | AlreadyFinished

data ControlReceipt = ScheduleChanged RoutineDefinition | RunStopReceived StopReceipt | RoutineDeleted

controlRoutine :: RoutineControl -> N.RoutineRef
controlRoutine = case _ of
  PauseSchedule ref -> ref
  ResumeSchedule ref -> ref
  StopRun ref -> ref.routine
  DeleteRoutine ref -> ref

controlAction :: RoutineControl -> String
controlAction = case _ of
  PauseSchedule _ -> "pause"
  ResumeSchedule _ -> "resume"
  StopRun _ -> "stop"
  DeleteRoutine _ -> "delete"

controlLabel :: RoutineControl -> String
controlLabel = case _ of
  PauseSchedule _ -> "Pause schedule"
  ResumeSchedule _ -> "Resume schedule"
  StopRun _ -> "Stop run"
  DeleteRoutine _ -> "Delete routine"

decodeControlReceipt :: RoutineControl -> Json -> Either String ControlReceipt
decodeControlReceipt control value = case control of
  PauseSchedule ref -> schedule ref true
  ResumeSchedule ref -> schedule ref false
  DeleteRoutine ref -> do
    fields <- objectValue value
    id <- field "id" nonempty fields
    check (id == N.routineId ref.id) "Delete response belongs to another routine"
    _ <- field "deletedAt" nonempty fields
    pure RoutineDeleted
  StopRun ref -> do
    fields <- objectValue value
    id <- field "id" nonempty fields
    routine <- field "routine" nonempty fields
    check (id == N.runId ref.id && routine == N.routineId ref.routine.id) "Stop response belongs to another run"
    status <- field "status" stringValue fields
    RunStopReceived <$> case status of
      "stopping" -> Right Stopping
      "stopped" -> Right Stopped
      "finished" -> Right AlreadyFinished
      _ -> Left "Unknown Stop response"
  where
  schedule ref paused = do
    routine <- decodeRoutine ref value
    check (routine.paused == paused) "Schedule change was not confirmed"
    pure $ ScheduleChanged routine

-- The host owns condition and repair verification; the UI only decodes them.
data RoutineCondition = NoIssue | AttentionRequired ProblemRef ProblemReason | RepairPending AppliedRepair

type ProblemRef = {run :: N.RunId, version :: String}
data ProblemKind = PhysicalProblem | UnresolvedProblem | RejectedProblem | StaleProblem | MissingOutcomeProblem
derive instance eqProblemKind :: Eq ProblemKind

type ProblemReason = {kind :: ProblemKind, message :: String}
type AppliedRepair = {key :: String, problem :: ProblemRef, description :: String, actor :: String, confirmedAt :: String, admissionFence :: N.RunId}
data RepairVerification = AwaitingVerification | VerificationRunning N.RunId | VerificationPassed N.RunId | VerificationFailed N.RunId
derive instance eqRepairVerification :: Eq RepairVerification

type RepairEntry = {repair :: AppliedRepair, verification :: RepairVerification}

decodeProblem :: Json -> Either String ProblemRef
decodeProblem value = do
  fields <- objectValue value
  run <- field "run" runIdentity fields
  version <- field "version" nonempty fields
  pure {run,version}

decodeReason :: Json -> Either String ProblemReason
decodeReason value = do
  fields <- objectValue value
  raw <- field "kind" stringValue fields
  kind <- case raw of
    "physical_failure" -> Right PhysicalProblem
    "unresolved" -> Right UnresolvedProblem
    "rejected" -> Right RejectedProblem
    "stale" -> Right StaleProblem
    "missing_outcome" -> Right MissingOutcomeProblem
    _ -> Left "Unknown routine problem"
  message <- field "message" stringValue fields
  pure {kind,message}

decodeRepair :: Json -> Either String AppliedRepair
decodeRepair value = do
  fields <- objectValue value
  key <- field "key" nonempty fields
  problem <- field "problem" decodeProblem fields
  description <- field "description" nonempty fields
  actor <- field "actor" nonempty fields
  confirmedAt <- field "confirmedAt" nonempty fields
  admissionFence <- field "admissionFence" runIdentity fields
  pure {key,problem,description,actor,confirmedAt,admissionFence}

decodeCondition :: Json -> Either String RoutineCondition
decodeCondition value = do
  fields <- objectValue value
  kind <- field "kind" stringValue fields
  case kind of
    "clear" -> Right NoIssue
    "attention" -> AttentionRequired <$> field "problem" decodeProblem fields <*> field "reason" decodeReason fields
    "repairing" -> RepairPending <$> field "repair" decodeRepair fields
    _ -> Left "Unknown routine condition"

decodeRepairEntry :: Json -> Either String RepairEntry
decodeRepairEntry value = do
  repair <- decodeRepair value
  fields <- objectValue =<< field "verification" Right =<< objectValue value
  kind <- field "kind" stringValue fields
  verification <- case kind of
    "pending" -> Right AwaitingVerification
    "running" -> VerificationRunning <$> field "run" runIdentity fields
    "passed" -> VerificationPassed <$> field "run" runIdentity fields
    "failed" -> VerificationFailed <$> field "run" runIdentity fields
    _ -> Left "Unknown repair verification"
  pure {repair,verification}
