{-# LANGUAGE OverloadedStrings #-}
module RoutineCommands (command, snapshot, detail) where

import Control.Exception
import Control.Monad
import Data.Aeson
import Data.Aeson.Types (Parser)
import qualified Data.Aeson.KeyMap as KM
import Data.List (find)
import qualified Data.List.NonEmpty as NE
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Database.SQLite.Simple as SQL
import Types
import Population
import qualified Store as DB
import Data.Time (UTCTime, getCurrentTime)
import Routines (Revision(..), Minutes, RoutineFields(..), RoutineContract(..), Definition(..))
import qualified Routines as R
import qualified Delegation as D
import qualified Automation

newtype OperationId = OperationId Text deriving (Eq, Show)

data RoutineCommand = Create RoutineFields | Edit Revision RoutineFields | RunNow Revision Text | StopRun Revision RunId | Delete Revision | Schedule Revision Minutes | Pause Revision Bool | RejectRun Revision RunId Text | Analyze Revision | StopAnalysis Revision RunId deriving (Eq, Show)
data CommandError = CommandError Int Text deriving (Eq, Show)
data CommandPlan = CreateDefinition RoutineFields | UpdateDefinition Definition | Admit Definition Text | StopOccurrences [D.Workload] | DeleteDefinition Definition [D.Workload] | RecordFeedback Text RunId | AnalyzeDefinition Definition | StopAnalysisCall Definition RunId

parseCommand :: Text -> Value -> Parser RoutineCommand
parseCommand action value = withObject "Routine command" (\o -> do
 unless (all (`elem` ["operation","revision","title","goal","criteria","inputs","run","minutes","reason","analysis"]) (keys value)) (fail "Unknown Routine command field")
 case action of
  "create" -> Create <$> parseJSON value
  "edit" -> Edit <$> o .: "revision" <*> parseJSON value
  "run" -> do
   inputs <- o .:? "inputs" .!= ""
   unless (T.length inputs<=16000) (fail "Run inputs are too long")
   RunNow <$> o .: "revision" <*> pure inputs
  "stop" -> do
   identity <- o .: "run"
   unless (not (T.null identity) && T.length identity<=128) (fail "Run identity is empty or too long")
   StopRun <$> o .: "revision" <*> pure (RunId identity)
  "reject" -> do
   reason <- o .: "reason"
   unless (not (T.null (T.strip reason)) && T.length reason<=4000) (fail "Feedback reason must contain 1–4000 characters")
   RejectRun <$> o .: "revision" <*> (RunId <$> o .: "run") <*> pure reason
  "delete" -> Delete <$> o .: "revision"
  "schedule" -> Schedule <$> o .: "revision" <*> o .: "minutes"
  "pause" -> Pause <$> o .: "revision" <*> pure True
  "resume" -> Pause <$> o .: "revision" <*> pure False
  "analyze" -> Analyze <$> o .: "revision"
  "stop-analysis" -> StopAnalysis <$> o .: "revision" <*> o .: "analysis"
  _ -> fail "Unknown Routine command") value

-- Pure policy; all persistence and clocks stay in the IO interpreter below.
decide :: UTCTime -> Maybe Definition -> [D.Workload] -> RoutineCommand -> Either CommandError CommandPlan
decide time current occurrences request = case request of
 Create fields -> Right (CreateDefinition fields)
 Edit revision fields -> do
  d <- existing revision
  let changed=R.changeContract (AcceptedContract fields) d
  pure (UpdateDefinition (R.bumpRevision changed {definitionFields=KM.delete "prompt" (definitionFields changed)}))
 Analyze revision -> AnalyzeDefinition <$> existing revision
 StopAnalysis revision identity -> StopAnalysisCall <$> existing revision <*> pure identity
 RunNow revision inputs -> Admit <$> existing revision <*> pure inputs
 StopRun revision identity -> do
  _ <- existing revision
  w <- maybe (Left (CommandError 404 "Unknown Routine run")) Right (find ((==runIdText identity) . D.workloadId) occurrences)
  if finished w then Left (CommandError 409 "Routine run is already finished") else Right (StopOccurrences [w])
 RejectRun revision identity _ -> do
  definition <- maybe (Left (CommandError 404 "Unknown personal Routine")) Right current
  work <- maybe (Left (CommandError 404 "Unknown Routine run")) Right (find ((==runIdText identity) . D.workloadId) occurrences)
  if get (D.workField work "routineSnapshot") "revision"/=toJSON revision then Left (CommandError 409 "Feedback must reference the saved run revision")
   else if not (finished work) then Left (CommandError 409 "Wait until the run is finished before rejecting its result")
   else Right (RecordFeedback (definitionId definition) identity)
 Delete revision -> do
  d <- existing revision
  pure (DeleteDefinition (R.bumpRevision d {definitionDeleted=True}) (filter (not . finished) occurrences))
 Schedule revision minutes -> do
  d <- existing revision
  pure (UpdateDefinition (R.bumpRevision (R.setInterval time minutes d)))
 Pause revision paused -> do
  d <- existing revision
  changed <- either (Left . CommandError 409) Right (R.setSchedulePaused time paused d)
  pure (UpdateDefinition (if changed==d then d else R.bumpRevision changed))
 where
 existing revision = do
  d <- maybe (Left (CommandError 404 "Unknown personal Routine")) Right current
  if definitionRevision d/=revision then Left (CommandError 409 "Routine changed; reload before trying again")
   else if definitionDeleted d then Left (CommandError 409 "Routine is deleted") else Right d
 finished w = let frontier=D.frontierCalls w
  in not (null frontier) && all ((`elem` [D.CallCompleted,D.CallUnresolved,D.CallFailed,D.CallInterrupted,D.CallCancelled]) . D.callState) frontier

definitions :: Service -> IO [Definition]
definitions s = filter (not . T.isPrefixOf "group_" . definitionOwner) <$> R.readDefinitions s

command :: Service -> Text -> Text -> Text -> Value -> IO (Int,Value)
command s owner action identity payload = do
 void (DB.agent s owner)
 operation <- OperationId <$> textField payload "operation" 128
 let request=obj [("action",str action),("target",str identity),("payload",payload)]
 previous <- receipt s owner operation
 case previous of
  Just old | get old "request"==request -> pure (int (get old "code"),get old "response")
  Just _ -> do
   let message="Operation id was already used with different contents"
   DB.record s [owner] identity "routine.command_refused" (obj [("error",str message),("operation",get payload "operation")]) "admin" Nothing
   pure (409,obj [("error",str message)])
  Nothing -> do
   clock <- getCurrentTime
   let time=timestamp clock
   runIdentity <- RunId <$> newId
   result <- try $ do
    requireActive s owner
    request_ <- parseValue 400 (parseCommand action) payload
    case request_ of
     RunNow _ _ -> do
      cfg <- corporaSettings s
      require (corporaStatus cfg==Active) 409 "Activate this corpora before starting work"
     _ -> pure ()
    case request_ of
     Analyze _ -> do
      cfg <- corporaSettings s
      require (corporaStatus cfg==Active) 409 "Activate this corpora before starting analysis"
      Automation.preflight
     _ -> pure ()
    DB.connection s $ \db -> SQL.withTransaction db $ do
     -- Revision, stop targets and admission are decided from the same transaction.
     stored <- DB.queryRowsOn db "SELECT value FROM scheduled_routines ORDER BY sequence" ["value"] [] >>= traverse (DB.decodeStored . (`at` "value"))
     allDefinitions <- traverse (decodeValue 500) stored
     workValues <- DB.queryRowsOn db "SELECT value FROM workloads ORDER BY rowid" ["value"] [] >>= traverse (DB.decodeStored . (`at` "value"))
     works <- traverse (decodeValue 500) workValues
     let current=find (\d->definitionId d==identity && definitionOwner d==owner) allDefinitions
         occurrences=filter (\w->at (D.workField w "origin") "routine"==identity) works
     plan <- either (\(CommandError code message)->failure code message) pure (decide clock current occurrences request_)
     (target,response) <- applyPlan db (AgentId owner) runIdentity time plan
     case plan of
      RecordFeedback _ run -> do
       rows <- DB.queryRowsOn db "SELECT request FROM command_receipts WHERE scope='routine' AND owner=? AND target=? AND code<300 ORDER BY rowid" ["request"] [SQL.SQLText owner,SQL.SQLText identity]
       requests <- traverse (DB.decodeStored . (`at` "request")) rows
       let rejected=any (\old -> at old "action"=="reject" && get (get old "payload") "run"==toJSON run) requests
           entity=maybe (runIdText run) (\work -> at (D.workField work "task") "taskId") (find ((==runIdText run) . D.workloadId) occurrences)
       unless rejected $ DB.recordOn s db [owner] entity "task.result.rejected" (mergeFields [("run",toJSON run)] (select ["reason","revision"] payload)) "admin" Nothing
      _ -> pure ()
     saveReceipt db owner operation target request 200 response time
     pure (200,response)
   case result of
    Right value -> pure value
    Left (APIError code message) -> do
     let response=obj [("error",str message)]
     DB.connection s $ \db -> saveReceipt db owner operation identity request code response time
     pure (code,response)

applyPlan :: SQL.Connection -> AgentId -> RunId -> Text -> CommandPlan -> IO (Text,Value)
applyPlan db owner runIdentity time plan = case plan of
 AnalyzeDefinition definition -> do
  response <- Automation.requestOn db definition runIdentity time True
  pure (definitionId definition,response)
 StopAnalysisCall definition identity -> do
  response <- Automation.stopOn db definition identity
  pure (definitionId definition,response)
 RecordFeedback identity run -> pure (identity,obj [("routine",str identity),("run",toJSON run)])
 CreateDefinition fields -> do
  let initial=Definition "" (agentIdText owner) (Revision 1) (AcceptedContract fields) Nothing False (KM.fromList [("created",str time),("contractGeneration",num 1)])
  saved <- DB.saveRoutineOn db (remove ["id"] (toJSON initial))
  pure (at saved "id",obj [("routine",get saved "id")])
 UpdateDefinition definition -> saveDefinition definition
 DeleteDefinition definition works -> mapM_ stop works >> saveDefinition definition
 StopOccurrences works -> do
  mapM_ stop works
  case works of
   [w] -> pure (at (D.workField w "origin") "routine",obj [("run",str (D.workloadId w))])
   _ -> failure 500 "Invalid stop plan"
 Admit definition inputs -> do
  let identity=runIdText runIdentity
      snapshot_=merge (toJSON definition) (obj [("inputs",str inputs)])
      origin=obj [("routine",str (definitionId definition)),("occurrence",str identity),("trigger",toJSON ManualTrigger)]
      spec=case definitionContract definition of
       LegacyPrompt _ -> Nothing
       AcceptedContract fields -> Just (D.TaskSpec (fieldGoal fields) [inputs | not (T.null inputs)] (fieldCriteria fields) (fieldCriteria fields))
      turn=newTurn runIdentity Chat (R.definitionGoal definition) time origin False
  D.admitRoutineOn db owner turn snapshot_ spec
  pure (definitionId definition,obj [("routine",str (definitionId definition)),("run",str identity)])
 where
 saveDefinition definition = do
  void (DB.saveRoutineOn db (toJSON definition))
  pure (definitionId definition,obj [("routine",str (definitionId definition))])
 stop work = do
  let cancel call = if D.callState call `elem` [D.CallQueued,D.CallRunning,D.CallWaiting] then call {D.callState=D.CallCancelled,D.callOutput=Nothing} else call
      fenced=D.setWorkField "origin" (put "occurrence" (str (D.workloadId work)) (D.workField work "origin")) work
      stopped=(D.setWorkField "executionGate" (toJSON ExecutionStopped) fenced) {D.workCalls=fmap cancel (D.workCalls work)}
  DB.putValueOn db DB.Workloads (D.workloadId work) (toJSON stopped)

receipt :: Service -> Text -> OperationId -> IO (Maybe Value)
receipt s owner (OperationId identity) = do
 rows <- DB.queryRows s "SELECT request,code,response FROM command_receipts WHERE scope='routine' AND owner=? AND id=?" ["request","code","response"] [SQL.SQLText owner,SQL.SQLText identity]
 forM (listToMaybe rows) $ \row -> do
  request <- DB.decodeStored (at row "request")
  response <- DB.decodeStored (at row "response")
  pure (merge row (obj [("request",request),("response",response)]))
saveReceipt :: SQL.Connection -> Text -> OperationId -> Text -> Value -> Int -> Value -> Text -> IO ()
saveReceipt db owner (OperationId identity) target request code response time =
 SQL.execute db "INSERT INTO command_receipts(scope,owner,id,target,request,code,response,created) VALUES ('routine',?,?,?,?,?,?,?)" (owner,identity,target,jsonText request,code,jsonText response,time)

data RunProjection = RunSummary | RunDetail

readOperations :: Service -> IO [Value]
readOperations = DB.routineOperations

snapshot :: Service -> IO Value
snapshot s = do
 ds <- definitions s
 works <- D.readWorks s
 operations <- readOperations s
 rendered <- forM ds $ \d -> do
  history <- traverse (renderRun RunSummary operations (definitionRevision d)) [w|w<-works,at (D.workField w "origin") "routine"==definitionId d]
  analysis <- Automation.projection s d works
  pure (merge (toJSON d) (obj [("analysis",analysis),("trigger",str (maybe "manual" (const "interval") (definitionSchedule d))),("title",str (R.definitionTitle d)),("goal",str (R.definitionGoal d)),("criteria",maybe Null toJSON (R.definitionCriteria d)),("revision",toJSON (definitionRevision d)),("deleted",Bool (definitionDeleted d)),("runs",arr history)]))
 pure (obj [("definitions",arr rendered),("errors",arr [remove ["request"] op|op<-reverse operations,int (get op "code")>=400])])

detail :: Service -> Text -> Text -> Text -> IO Value
detail s owner identity run = do
 ds <- definitions s
 definition <- maybe (failure 404 "Unknown personal Routine") pure (find (\d -> definitionId d==identity && definitionOwner d==owner) ds)
 works <- D.readWorks s
 work <- maybe (failure 404 "Unknown Routine run") pure (find (\w -> D.workloadId w==run && at (D.workField w "origin") "routine"==identity) works)
 operations <- readOperations s
 renderRun RunDetail operations (definitionRevision definition) work

renderRun :: RunProjection -> [Value] -> Revision -> D.Workload -> IO Value
renderRun projection operations revision work = do
 gate <- if has (D.workField work "origin") "occurrence" then field 500 (toJSON work) "executionGate" else pure ExecutionOpen
 let call=NE.last (D.workCalls work)
     status=if gate==ExecutionStopped then "stopped" else case D.callState call of
      D.CallQueued -> "queued"
      D.CallRunning -> "running"
      D.CallCompleted -> "done"
      D.CallUnresolved -> "unresolved"
      D.CallWaiting -> "waiting"
      D.CallDelegated -> "queued"
      D.CallFailed -> "failed"
      D.CallInterrupted -> "interrupted"
      D.CallCancelled -> "cancelled"
 let original=maybe Null (`get` "assessment") (listToMaybe (reverse (D.workItems work "decisions"))) `fallback` assessmentValue AssessmentUnresolved "application_observation" "No recorded contract assessment"
     feedback=DB.workFeedback operations (toJSON work)
     assessment=DB.workAssessment operations (toJSON work)
     invocations=[select ["decisionId","callId","node","evidence","assessment"] entry | entry<-D.workItems work "decisions",has entry "evidence"]
     reported=get (maybe Null (`get` "response") (listToMaybe (reverse (D.workItems work "decisions")))) "outcome"
     criteriaAssessment=[obj [("criterion",criterion),("status",str (if criterion `elem` items reported "satisfiedCriteria" then "Model reports satisfied" else if has (get reported "notApplicableCriteria") (text criterion) then "Model reports not applicable" else "Unresolved")),("reason",get (get reported "notApplicableCriteria") (text criterion)),("source",str "model_statement")] | criterion<-items (D.workField work "routineSnapshot") "criteria"]
     observations=obj [("source",str "model_statement"),("statements",get (D.workField work "outcome") "evidence" `fallback` arr []),("independent",Bool False)]
 binding <- D.bindingDetail work call
 let lastDecision=maybe Null (`get` "decisionId") (listToMaybe (reverse (D.workItems work "decisions")))
     lastFeedback=maybe Null (`get` "operation") (listToMaybe (reverse feedback))
     version=jsonText (arr [str status,lastDecision,lastFeedback,toJSON revision])
     details=case projection of
      RunSummary -> []
      RunDetail -> [("executionBinding",binding),("criteriaAssessment",arr criteriaAssessment),("invocations",arr invocations),("observations",observations),("feedback",arr feedback),("rejection",fromMaybe Null (listToMaybe feedback)),("outcome",D.workField work "outcome")]
 pure (obj (details++[("assessment",assessment),("originalAssessment",original),("detailVersion",str version),("invocationCount",num (length invocations)),("currentRevision",toJSON revision),("historical",Bool (get (D.workField work "routineSnapshot") "revision"/=toJSON revision)),("id",str (D.workloadId work)),("created",D.workField work "created"),("trigger",get (D.workField work "origin") "trigger"),("snapshot",D.workField work "routineSnapshot"),("status",str status),("result",if gate==ExecutionStopped then Null else get (D.workField work "outcome") "reply"),("message",if gate==ExecutionOpen && D.callState call==D.CallWaiting then toJSON (D.callOutput call) else Null),("error",D.callField call "error"),("currentOwner",toJSON (D.callOwner call))]))
