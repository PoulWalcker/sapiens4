{-# LANGUAGE OverloadedStrings #-}
module RoutineCommands (command, snapshot) where

import Control.Exception
import Control.Monad
import Data.Aeson
import Data.Aeson.Types (Parser)
import qualified Data.Aeson.KeyMap as KM
import Data.List (find)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Database.SQLite.Simple as SQL
import Types
import Population
import qualified Store as DB
import qualified ManagedState as Managed
import qualified Delegation as D

newtype OperationId = OperationId Text deriving (Eq, Show)
newtype Revision = Revision Int deriving (Eq, Show)
instance FromJSON Revision where
 parseJSON value = do
  n <- parseJSON value
  if n>0 then pure (Revision n) else fail "Revision must be a positive integer"
instance ToJSON Revision where toJSON (Revision n)=toJSON n

data RoutineFields = RoutineFields
 { fieldTitle :: Text, fieldGoal :: Text, fieldCriteria :: NonEmpty Text
 } deriving (Eq, Show)
instance FromJSON RoutineFields where
 parseJSON = withObject "Routine fields" $ \o -> do
  title <- o .: "title" >>= boundedText "Routine title" 60
  goal <- o .: "goal" >>= boundedText "Goal" 16000
  criteria <- o .: "criteria" >>= traverse (boundedText "Criterion" 2000)
  unless (length criteria<=30) (fail "Provide at most 30 completion criteria")
  nonempty <- maybe (fail "Provide at least one completion criterion") pure (NE.nonEmpty criteria)
  pure (RoutineFields title goal nonempty)
boundedText :: String -> Int -> Text -> Parser Text
boundedText label limit raw = do
 let value=T.strip raw
 unless (not (T.null value) && T.length value<=limit) (fail (label<>" is empty or too long"))
 pure value

data Definition = Definition
 { definitionId :: Text, definitionOwner :: AgentId, definitionRevision :: Revision
 , definitionFields :: RoutineFields, definitionDeleted :: Bool, definitionCreated :: Text
 } deriving (Eq, Show)
instance FromJSON Definition where
 parseJSON value = withObject "Routine definition" (\o -> do
  trigger <- routineTrigger value
  unless (trigger==ManualTrigger) (fail "This command requires a manual trigger")
  Definition <$> o .: "id" <*> o .: "owner" <*> o .: "revision" <*> parseJSON value <*> o .:? "deleted" .!= False <*> o .: "created") value
instance ToJSON Definition where
 toJSON d = obj [("id",str (definitionId d)),("owner",toJSON (definitionOwner d)),("revision",toJSON (definitionRevision d)),("title",str (fieldTitle f)),("goal",str (fieldGoal f)),("criteria",toJSON (NE.toList (fieldCriteria f))),("trigger",toJSON ManualTrigger),("deleted",Bool (definitionDeleted d)),("created",str (definitionCreated d))]
  where f=definitionFields d

data RoutineCommand = Create RoutineFields | Edit Revision RoutineFields | RunNow Revision Text | StopRun Revision RunId | Delete Revision deriving (Eq, Show)
data CommandError = CommandError Int Text deriving (Eq, Show)
data CommandPlan = CreateDefinition RoutineFields | UpdateDefinition Definition | Admit Definition Text | StopOccurrences [D.Workload] | DeleteDefinition Definition [D.Workload]

parseCommand :: Text -> Value -> Parser RoutineCommand
parseCommand action value = withObject "Routine command" (\o -> do
 unless (all (`elem` ["operation","revision","title","goal","criteria","inputs","run"]) (keys value)) (fail "Unknown Routine command field")
 case action of
  "create" -> Create <$> parseJSON value
  "edit" -> Edit <$> o .: "revision" <*> parseJSON value
  "run" -> do
   inputs <- o .:? "inputs" .!= ""
   unless (T.length inputs<=16000) (fail "Run inputs are too long")
   RunNow <$> o .: "revision" <*> pure inputs
  "stop" -> do
   identity <- o .: "run" >>= boundedText "Run identity" 128
   StopRun <$> o .: "revision" <*> pure (RunId identity)
  "delete" -> Delete <$> o .: "revision"
  _ -> fail "Unknown Routine command") value

-- Pure policy; all persistence and clocks stay in the IO interpreter below.
decide :: Maybe Definition -> [D.Workload] -> RoutineCommand -> Either CommandError CommandPlan
decide current occurrences request = case request of
 Create fields -> Right (CreateDefinition fields)
 Edit revision fields -> do
  d <- existing revision
  pure (UpdateDefinition d {definitionFields=fields,definitionRevision=next revision})
 RunNow revision inputs -> Admit <$> existing revision <*> pure inputs
 StopRun revision identity -> do
  _ <- existing revision
  w <- maybe (Left (CommandError 404 "Unknown Routine run")) Right (find ((==runIdText identity) . D.workloadId) occurrences)
  if finished w then Left (CommandError 409 "Routine run is already finished") else Right (StopOccurrences [w])
 Delete revision -> do
  d <- existing revision
  pure (DeleteDefinition d {definitionDeleted=True,definitionRevision=next revision} (filter (not . finished) occurrences))
 where
 existing revision = do
  d <- maybe (Left (CommandError 404 "Unknown personal Routine")) Right current
  if definitionRevision d/=revision then Left (CommandError 409 "Routine changed; reload before trying again")
   else if definitionDeleted d then Left (CommandError 409 "Routine is deleted") else Right d
 next (Revision n)=Revision (n+1)
 finished w = not (any ((`elem` [D.CallQueued,D.CallRunning,D.CallWaiting]) . D.callState) (D.workCalls w)) && D.callState (NE.last (D.workCalls w)) `elem` [D.CallCompleted,D.CallUnresolved,D.CallFailed,D.CallInterrupted,D.CallCancelled]

definitions :: Service -> IO [Definition]
definitions s = do
 rows <- DB.values s DB.ScheduledRoutines
 manual <- filterM (fmap (==ManualTrigger) . parseValue 500 routineTrigger) rows
 traverse (decodeValue 500) manual

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
   time <- now
   runIdentity <- RunId <$> newId
   result <- try $ do
    requireActive s owner
    request_ <- parseValue 400 (parseCommand action) payload
    case request_ of
     RunNow _ _ -> do
      cfg <- corporaSettings s
      require (corporaStatus cfg==Active) 409 "Activate this corpora before starting work"
     _ -> pure ()
    DB.connection s $ \db -> SQL.withTransaction db $ do
     -- Revision, stop targets and admission are decided from the same transaction.
     stored <- DB.queryRowsOn db "SELECT value FROM scheduled_routines ORDER BY sequence" ["value"] [] >>= traverse (DB.decodeStored . (`at` "value"))
     manual <- filterM (fmap (==ManualTrigger) . parseValue 500 routineTrigger) stored >>= traverse (decodeValue 500)
     workValues <- DB.queryRowsOn db "SELECT value FROM workloads ORDER BY rowid" ["value"] [] >>= traverse (DB.decodeStored . (`at` "value"))
     works <- traverse (decodeValue 500) workValues
     let current=find (\d->definitionId d==identity && definitionOwner d==AgentId owner) manual
         occurrences=filter (\w->at (D.workField w "origin") "routine"==identity && at (D.workField w "routineSnapshot") "owner"==owner) works
     plan <- either (\(CommandError code message)->failure code message) pure (decide current occurrences request_)
     (target,response) <- applyPlan db (AgentId owner) runIdentity time plan
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
 CreateDefinition fields -> do
  let initial=Definition "" owner (Revision 1) fields False time
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
      fields=definitionFields definition
      snapshot_=merge (toJSON definition) (obj [("inputs",str inputs)])
      origin=obj [("routine",str (definitionId definition)),("occurrence",str identity),("trigger",toJSON ManualTrigger)]
      spec=D.TaskSpec (fieldGoal fields) [inputs | not (T.null inputs)] (fieldCriteria fields) (fieldCriteria fields)
      first=D.newCall runIdentity owner Nothing D.Execution D.CallQueued (fieldGoal fields)
      work=D.Workload identity True (first :| []) (KM.fromList [("originalIntent",str (fieldGoal fields)),("task",obj [("taskId",str ("task_"<>identity)),("taskType",str "automated"),("specification",toJSON spec)]),("origin",origin),("routineSnapshot",snapshot_),("executionGate",toJSON ExecutionOpen),("decisions",arr []),("outcome",Null)])
      turn=newTurn runIdentity Chat (fieldGoal fields) time origin False
      message=obj [("role",str "user"),("content",str (fieldGoal fields)),("turn",str identity),("time",str time),("origin",origin)]
  DB.putValueOn db DB.Workloads identity (toJSON work)
  Managed.enrollOn db (agentIdText owner) turn message (fieldGoal fields) []
  pure (definitionId definition,obj [("routine",str (definitionId definition)),("run",str identity)])
 where
 saveDefinition definition = do
  void (DB.saveRoutineOn db (toJSON definition))
  pure (definitionId definition,obj [("routine",str (definitionId definition))])
 stop work = do
  let cancel call = if D.callState call `elem` [D.CallQueued,D.CallRunning,D.CallWaiting] then call {D.callState=D.CallCancelled,D.callOutput=Nothing} else call
      stopped=(D.setWorkField "executionGate" (toJSON ExecutionStopped) work) {D.workCalls=fmap cancel (D.workCalls work)}
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

snapshot :: Service -> IO Value
snapshot s = do
 ds <- definitions s
 works <- D.readWorks s
 operations <- DB.queryRows s "SELECT id,owner,target,code,response,created FROM command_receipts WHERE scope='routine' ORDER BY rowid DESC" ["id","owner","target","code","response","created"] []
 decoded <- forM operations $ \op -> do response<-DB.decodeStored (at op "response"); pure (put "response" response op)
 rendered <- forM ds $ \d -> do
  history <- traverse renderRun [w|w<-works,at (D.workField w "origin") "routine"==definitionId d,has (D.workField w "origin") "occurrence"]
  pure (put "runs" (arr history) (toJSON d))
 pure (obj [("definitions",arr rendered),("errors",arr [op|op<-decoded,int (get op "code")>=400])])
 where
 renderRun work = do
  gate <- field 500 (toJSON work) "executionGate"
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
  pure (obj [("id",str (D.workloadId work)),("snapshot",D.workField work "routineSnapshot"),("status",str status),("result",if gate==ExecutionStopped then Null else get (D.workField work "outcome") "reply"),("message",if gate==ExecutionOpen && D.callState call==D.CallWaiting then toJSON (D.callOutput call) else Null),("error",D.callField call "error"),("currentOwner",toJSON (D.callOwner call))])
