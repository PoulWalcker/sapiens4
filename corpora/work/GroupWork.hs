{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields, NoFieldSelectors, OverloadedRecordDot #-}
{-# OPTIONS_GHC -Werror=incomplete-patterns #-}
module GroupWork where

import Control.Monad
import Data.Aeson hiding (pairs)
import qualified Data.Aeson.KeyMap as KM
import Data.Maybe (fromMaybe, isNothing, isJust, maybeToList)
import Data.List (find)
import Data.Aeson.Types (parseEither, parseMaybe)
import Data.Text (Text)
import Data.Time (UTCTime, getCurrentTime)
import qualified Data.Text as T
import Types
import qualified Store as DB
import GroupModel
import qualified Delegation as D

-- Parsing preserves the established wire format; decisions below use typed values.
data GroupTask = GroupTask
 { identity :: TaskId, title :: Text, body :: Text, assignee :: AgentId
 , state :: TaskState, deleted :: Bool, revision :: Int, specification :: Maybe D.TaskSpec, metadata :: Object
 } deriving (Eq, Show)
instance FromJSON GroupTask where
 parseJSON = withObject "Group task" $ \o -> do
  specification <- if KM.member "specification" o then Just <$> o .: "specification" else pure Nothing
  GroupTask <$> o .: "id" <*> o .: "title" <*> o .: "body" <*> o .: "assignee" <*> o .: "state" <*> o .: "deleted" <*> o .: "revision" <*> pure specification <*> pure (foldr KM.delete o ["id","title","body","assignee","state","deleted","revision","specification"])
instance ToJSON GroupTask where
 toJSON task = mergeFields ([("id",toJSON task.identity),("title",str task.title),("body",str task.body),("assignee",toJSON task.assignee),("state",toJSON task.state),("deleted",Bool task.deleted),("revision",num task.revision)] ++ [("specification",toJSON spec) | spec <- maybeToList task.specification]) (Object task.metadata)

data TaskPatch = TaskPatch
 { title :: Maybe Text, body :: Maybe Text, assignee :: Maybe AgentId
 , state :: Maybe TaskState, deleted :: Maybe Bool, revision :: Maybe Int
 } deriving (Eq, Show)
instance FromJSON TaskPatch where
 parseJSON value = onlyFields ["title","body","assignee","state","deleted","revision"] value *>
  (TaskPatch <$> optional "title" (\raw -> textFieldP "title" 500 value *> parseJSON raw)
             <*> optional "body" body <*> optional "assignee" parseJSON
             <*> optional "state" parseJSON <*> optional "deleted" parseJSON <*> optional "revision" parseJSON)
  where
  optional key parser = if has value key then Just <$> parser (get value key) else pure Nothing
  body raw = do
   content <- parseJSON raw
   unless (T.length content <= 16000) (fail "Task body must be text, at most 16000 characters")
   pure content

data CreateTask = CreateTask { title :: Text, body :: Text, assignee :: AgentId } deriving (Eq, Show)
data TaskError = InvalidTask Text | MissingTask | TaskChanged | RestoreRequired | RestoreAssigneeRequired | StartConflict | StartAssigneeRequired | AlreadyPending deriving (Eq, Show)
data TaskCompletion = TaskCompletion
 { run :: RunId, owner :: AgentId, revision :: Int, status :: TurnStatus
 , output :: Maybe Text, error :: Maybe Text, outcome :: Maybe D.StructurallyValidOutcome, publication :: Maybe ResultPublication
 } deriving (Eq, Show)
data Settlement = NoTask | TaskForwarded | StaleResultRecorded | TaskUpdated deriving (Eq, Show)

validateTask :: [AgentId] -> Value -> Either TaskError TaskPatch
validateTask members input = case parseEither parseJSON input of
 Left reason -> Left (InvalidTask (T.pack reason))
 Right patch
  | Just owner <- patch.assignee, owner `notElem` members -> Left (InvalidTask "Assign tasks only to current Group members")
  | otherwise -> Right patch
prepareTask :: AgentId -> TaskPatch -> Either TaskError CreateTask
prepareTask lead patch = case patch.title of
 Nothing -> Left (InvalidTask "title must be nonempty text, at most 500 characters")
 Just title -> Right (CreateTask (T.strip title) (fromMaybe "" patch.body) (fromMaybe lead patch.assignee))
newTask :: TaskId -> UTCTime -> Text -> CreateTask -> GroupTask
newTask identity created actor draft = GroupTask identity draft.title draft.body draft.assignee Backlog False 1 Nothing
 (KM.fromList [("created",str (timestamp created)),("author",str actor),("updated_by",str actor),("history",arr []),("results",arr [])])
newScheduledTask :: TaskId -> UTCTime -> Text -> Text -> AgentId -> RoutineId -> Maybe D.TaskSpec -> GroupTask
newScheduledTask identity created title body assignee routine specification = base
 { state=InProgress, specification=specification, metadata=KM.fromList [("taskType",str "automated"),("routine",toJSON routine)] <> base.metadata }
 where base=newTask identity created "admin" (CreateTask title body assignee)

taskLink :: GroupTask -> TaskRevision
taskLink task = TaskRevision task.identity task.revision

taskRoutine :: GroupTask -> Either Text (Maybe RoutineId)
taskRoutine task = case KM.lookup "routine" task.metadata of
 Nothing -> Right Nothing
 Just raw -> Just <$> either (Left . T.pack) Right (parseEither parseJSON raw)

validateTaskOrigin :: GroupTask -> ExecutionOrigin -> Either Text ()
validateTaskOrigin task origin = case originPurpose origin of
 ScheduledExecution scheduled -> do
  routine <- taskRoutine task
  unless (routine == Just (scheduledRoutine scheduled)) (Left "Scheduled Group request belongs to another Routine task")
 OrdinaryWork -> Right ()
 RoutinePolicySetup _ -> Right ()

restartTaskOrigin :: GroupTask -> [GroupRequest] -> Either Text StoredOrigin
restartTaskOrigin task requests = do
 routine <- taskRoutine task
 case routine of
  Nothing -> Right MissingOrigin
  Just identity -> do
   let related = [request | request <- requests, ValidLineage lineage <- [requestLineage request]
                 , Just link <- [requestTask lineage], requestTaskId link == task.identity]
   origins <- traverse (checkedOrigin . requestOrigin) related
   forM_ origins $ \origin -> case originPurpose origin of
    ScheduledExecution _ -> validateTaskOrigin task origin
    OrdinaryWork -> Right ()
    RoutinePolicySetup _ -> Left "Routine task has policy preparation provenance"
   original <- case reverse [origin | origin <- origins, originRoutine origin == Just identity] of
    origin:_ -> Right origin
    [] -> Left "Routine task has no saved scheduled execution origin"
   -- A Work revision uses its own instructions, while retaining the admission's identity and extensions.
   pure (ValidOrigin (newOrigin (originPurpose original) (originGroup original)
    (KM.insert "executionPrompt" (str task.body) (originMetadata original))))

activeRoutineRequest :: Maybe TaskId -> Value -> RoutineId -> Either Text (Maybe RunId)
activeRoutineRequest restartingTask group identity = do
 requests <- either (Left . T.pack) Right (parseEither parseJSON (get group "requests"))
 origins <- forM [request | request <- requests, not (requestSettled request)] $ \request -> do
  origin <- checkedOrigin (requestOrigin request)
  pure (request,origin)
 let historical=[(request,origin) | request <- requests, requestSettled request, ValidOrigin origin <- [requestOrigin request]]
 related <- filterM active [(request,origin) | (request,origin) <- origins ++ historical, originRoutine origin == Just identity]
 pure (case related of (request,_):_ -> Just (requestId request); [] -> Nothing)
 where
 active (request,_)
  | not (requestSettled request) = Right True
  | otherwise = do
     lineage <- checkedRequestLineage request
     case requestTask lineage of
      Nothing -> Right False
      Just link
       -- Editing a Task to InProgress does not start it; only an unsettled Request holds its own restart.
       | Just (requestTaskId link) == restartingTask -> Right False
       | otherwise -> case findId (taskIdText (requestTaskId link)) (items group "tasks") of
          Nothing -> Right False
          Just raw -> taskInProgress <$> either (Left . T.pack) Right (parseEither parseJSON raw)

taskInProgress :: GroupTask -> Bool
taskInProgress task = task.state == InProgress

currentTaskRevision :: TaskRevision -> GroupTask -> Bool
currentTaskRevision expected task = not task.deleted && taskLink task == expected

forwardTask :: AgentId -> GroupTask -> GroupTask
forwardTask assignee task = task { assignee=assignee, revision=task.revision+1, specification=task.specification }

stopTask :: UTCTime -> Text -> GroupTask -> GroupTask
stopTask time actor task
 | task.state /= InProgress = task
 | otherwise = task { state=Backlog, revision=task.revision+1
   , metadata=KM.fromList [("updated_by",str actor),("updated",str (timestamp time))
      ,("history",arr (items (Object task.metadata) "history" ++ [remove ["history","results"] (toJSON task)]))] <> task.metadata }

reviseTask :: [AgentId] -> UTCTime -> Text -> TaskPatch -> GroupTask -> Either TaskError GroupTask
reviseTask members updated actor patch task
 | patch.revision /= Just task.revision = Left TaskChanged
 | task.deleted && patch.deleted /= Just False = Left RestoreRequired
 | patch.deleted == Just False && task.assignee `notElem` members && isNothing patch.assignee = Left RestoreAssigneeRequired
 | otherwise = Right task
    { title=fromMaybe task.title patch.title, body=fromMaybe task.body patch.body
    , assignee=fromMaybe task.assignee patch.assignee, deleted=fromMaybe task.deleted patch.deleted
    , state=if reset then Backlog else fromMaybe task.state patch.state
    , revision=task.revision+1, metadata=metadata }
 where
 reset = (isJust patch.title || isJust patch.body || isJust patch.assignee || isJust patch.deleted) && isNothing patch.state
 history = items (Object task.metadata) "history" ++ [remove ["history","results"] (toJSON task)]
 metadata = KM.fromList [("updated_by",str actor),("updated",str (timestamp updated)),("history",arr history)] <> task.metadata
beginTask :: [AgentId] -> [TaskId] -> Maybe Int -> Text -> GroupTask -> Either TaskError GroupTask
beginTask members pending revision actor task
 | revision /= Just task.revision || task.deleted = Left StartConflict
 | task.assignee `notElem` members = Left StartAssigneeRequired
 | task.identity `elem` pending = Left AlreadyPending
 | otherwise = Right task { state=InProgress, revision=task.revision+1, metadata=KM.insert "updated_by" (str actor) task.metadata }
settleTaskResult :: UTCTime -> TaskCompletion -> GroupTask -> (GroupTask, Settlement)
settleTaskResult time completion task =
 let stale = task.deleted || task.revision /= completion.revision
     result = obj ([("turn",toJSON completion.run),("author",toJSON completion.owner),("text",toJSON completion.output),("error",toJSON completion.error),("revision",num completion.revision),("stale",Bool stale),("created",str (timestamp time))]
      ++ [("outcome",toJSON (D.validatedOutcome outcome)) | outcome <- maybeToList completion.outcome]
      ++ maybe [] publicationFields completion.publication)
     metadata = KM.insert "results" (arr (items (Object task.metadata) "results" ++ [result])) task.metadata
     changed = task { metadata=metadata }
 in if stale then (changed,StaleResultRecorded)
    else (changed { state=if completedStatus completion.status && maybe True ((==D.Completed) . D.outcomeStatus . D.validatedOutcome) completion.outcome then TaskDone else Backlog, revision=task.revision+1, metadata=KM.insert "updated_by" (toJSON completion.owner) metadata },TaskUpdated)

withResultProtocol :: Maybe D.TaskSpec -> GroupRequest -> GroupRequest
withResultProtocol specification request = case specification of
 Nothing -> request
 Just _ -> request { requestFields=KM.insert "resultProtocol" (str "outcome") (requestFields request) }

usesResultProtocol :: GroupRequest -> Validation Bool
usesResultProtocol request = case KM.lookup "resultProtocol" (requestFields request) of
 Nothing -> Right False
 Just (String "outcome") -> Right True
 _ -> Left (APIError 500 "Unsupported Group result protocol")

requestDecision :: GroupRequest -> Validation (Maybe D.DecisionResponse)
requestDecision request = traverse (validateValue 500 D.parseGroupDecision) (KM.lookup "resultDecision" (requestFields request))

completionOutcome :: GroupTask -> GroupRequest -> TurnStatus -> Validation (Maybe D.StructurallyValidOutcome)
completionOutcome task request status = do
 structured <- usesResultProtocol request
 if not structured || not (completedStatus status) then Right Nothing else do
  specification <- maybe (Left (APIError 500 "Structured Group execution has no saved specification")) Right task.specification
  response <- requestDecision request >>= maybe (Left (APIError 500 "Completed Group execution has no saved decision")) Right
  case D.decisionBody response of
   D.ReportOutcome publication outcome -> do
    origin <- either (Left . APIError 500) Right (checkedOrigin (requestOrigin request))
    either (Left . APIError 500) Right (validatePublication origin (D.outcomeStatus outcome == D.Completed) publication)
    case D.validateOutcome response outcome specification of
     Left problem -> Left (APIError 500 (D.outcomeError problem))
     Right checked -> Right (Just checked)
   _ -> Left (APIError 500 "A Group handoff cannot complete its Task")

resultHistory :: GroupTask -> Validation [Value]
resultHistory task = forM (items (Object task.metadata) "results") $ \result -> do
 request <- validateValue 500 parseJSON (get result "turn") :: Validation RunId
 revision <- validateValue 500 parseJSON (get result "revision") :: Validation Int
 outcome <- traverse (validateValue 500 parseJSON) (if has result "outcome" then Just (get result "outcome") else Nothing) :: Validation (Maybe D.Outcome)
 quiet <- validateValue 500 historyOnlyP result
 when quiet $ do
  check (maybe False ((==D.Completed) . D.outcomeStatus) outcome) 500 "History-only Group result requires a Completed outcome"
  void (validateValue 500 (textFieldP "text" 64000) result)
 rejection <- traverse (validateValue 500 parseJSON) (if has result "rejection" then Just (get result "rejection") else Nothing) :: Validation (Maybe ResultRejection)
 pure (obj [("result",obj [("request",toJSON request),("revision",num revision)]),("reply",get result "text"),("notification",str (if quiet then "quiet" else "notify")),("error",get result "error"),("stale",get result "stale"),("outcome",toJSON outcome),("rejection",toJSON rejection)])

rejectResult :: ResultRejection -> RunId -> Int -> GroupTask -> Validation (GroupTask,ResultRejection)
rejectResult proposed request revision task = do
 let matches result=get result "turn" == toJSON request && get result "revision" == num revision && get result "text" /= Null
     results=items (Object task.metadata) "results"
 result <- maybe (Left (APIError 404 "Unknown saved Group result")) Right (find matches results)
 check (length (filter matches results) == 1) 409 "Saved Group result reference is ambiguous"
 previous <- traverse (validateValue 500 parseJSON) (if has result "rejection" then Just (get result "rejection") else Nothing)
 case previous of
  Just saved -> do
   check (rejectedBy saved == rejectedBy proposed && rejectionReason saved == rejectionReason proposed) 409 "This result already has a different rejection"
   Right (task,saved)
  Nothing -> do
   let updated=map (\item -> if matches item then put "rejection" (toJSON proposed) item else item) results
   Right (task { metadata=KM.insert "results" (arr updated) task.metadata },proposed)

-- Only these adapters acquire time/identity, authorize actors and persist changes.
taskResult :: Either TaskError a -> IO a
taskResult = either reject pure
 where
 reject problem = case problem of
  InvalidTask reason -> failure 400 reason
  MissingTask -> failure 404 "Unknown Group task"
  TaskChanged -> failure 409 "Task changed; read its latest revision before editing"
  RestoreRequired -> failure 409 "Restore this task before editing"
  RestoreAssigneeRequired -> failure 409 "Choose a current member before restoring this task"
  StartConflict -> failure 409 "Task changed or was deleted; refresh before starting"
  StartAssigneeRequired -> failure 409 "Assign this task to a current member first"
  AlreadyPending -> failure 409 "This task already has a pending execution"
findTask :: Value -> Text -> IO Value
findTask group tid = taskResult (maybe (Left MissingTask) Right (findId tid (items group "tasks")))
readTask :: Value -> Text -> IO GroupTask
readTask group tid = findTask group tid >>= decodeValue 500
createTask :: Service -> Text -> Value -> Text -> IO Value
createTask s gid input actor = do
 group <- getGroup s gid True
 authorizeGroup s group actor Member
 members <- field 500 group "members"
 patch <- taskResult (validateTask members input)
 lead <- field 500 group "lead"
 draft <- taskResult (prepareTask lead patch)
 tid <- TaskId <$> fresh "gtask_"; created <- getCurrentTime
 let task = newTask tid created actor draft
 void (saveGroup s (append "tasks" (toJSON task) group) actor "task created" (toJSON tid))
 pure (toJSON task)
updateTask :: Service -> Text -> Text -> Value -> Text -> IO Value
updateTask s gid tid input actor = do
 group <- getGroup s gid True; authorizeGroup s group actor Member
 task <- readTask group tid
 members <- field 500 group "members"
 patch <- taskResult (validateTask members input)
 updated <- getCurrentTime
 changed <- taskResult (reviseTask members updated actor patch task)
 void (saveGroup s (replace "tasks" (toJSON changed) group) actor (if changed.deleted then "task deleted" else "task updated") (str tid))
 pure (toJSON changed)
startTask :: Service -> Text -> Text -> Value -> Text -> Maybe MessageId -> IO Value
startTask s gid tid revision actor root = do
 group <- getGroup s gid True; authorizeGroup s group actor Member
 task <- readTask group tid
 members <- field 500 group "members"
 requests <- readRequests group
 lineages <- traverse requireRequestLineage [request | request <- requests, requestStatus request `elem` [Queued,Running]]
 let pending=[requestTaskId link | lineage <- lineages, link <- maybeToList (requestTask lineage)]
 changed <- taskResult (beginTask members pending (parseMaybe parseJSON revision) actor task)
 origin <- either (failure 500) pure (restartTaskOrigin task requests)
 parsed <- requireExecutionOrigin origin
 case originPurpose parsed of
  ScheduledExecution scheduled -> do
   groupId <- field 500 group "id"
   require (scheduledOwner scheduled == GroupOwner groupId && originGroup parsed == Just groupId)
    500 "Saved task execution origin belongs to another Group"
  OrdinaryWork -> pure ()
  RoutinePolicySetup _ -> pure ()
 forM_ (originRoutine parsed) $ \identity -> do
  active <- either (failure 500) pure (activeRoutineRequest (Just task.identity) group identity)
  taskResult (if isNothing active then Right () else Left AlreadyPending)
 initial <- message actor (task.title <> "\n\n" <> task.body)
 let entry=mergeFields ([("task",str tid)] ++ [("memo_pending",Bool True) | actor == "admin" && not (originIsScheduled parsed)]) initial
 request <- newRequest entry task.assignee root
 lineage <- requireRequestLineage request
 let call=withResultProtocol changed.specification request { requestOrigin=origin, requestLineage=ValidLineage lineage { requestTask=Just (taskLink changed) } }
 void (saveGroup s (append "requests" (toJSON call) (append "messages" entry (replace "tasks" (toJSON changed) group))) actor "task started" (str tid))
 pure (toJSON changed)

newRequest :: Value -> AgentId -> Maybe MessageId -> IO GroupRequest
newRequest entry target root = do
 identity <- RunId <$> newId
 time <- now
 messageId <- field 500 entry "id"
 pure (newGroupRequest identity target MissingOrigin (RequestLineage messageId (fromMaybe messageId root) Nothing) time)

settleTask :: Value -> GroupRequest -> Value -> IO (Value, Settlement)
settleTask group request turn = do
 lineage <- requireRequestLineage request
 case requestTask lineage of
  Nothing -> pure (group,NoTask)
  Just link -> do
   task <- readTask group (taskIdText (requestTaskId link))
   origin <- requireExecutionOrigin (requestOrigin request)
   either (failure 500) pure (validateTaskOrigin task origin)
   time <- getCurrentTime
   status <- field 500 turn "status"
   outcome <- validated (completionOutcome task request status)
   response <- validated (requestDecision request)
   let publication=if completedStatus status then case D.decisionBody <$> response of
        Just (D.ReportOutcome saved _) -> Just saved
        _ -> Nothing
       else Nothing
   completion <- TaskCompletion (requestId request) (requestTarget request) (requestTaskRevision link) status
    <$> field 500 turn "output" <*> field 500 turn "error" <*> pure outcome <*> pure publication
   let (changed,settlement) = settleTaskResult time completion task
   pure (replace "tasks" (toJSON changed) group,settlement)

assignedTasks :: Service -> Text -> IO [Value]
assignedTasks s actor = do
 groups <- DB.values s DB.Groups
 pure [merge task (obj [("group",get g "id"),("group_name",get g "name"),("group_archived",get g "archived")]) | g <- groups,task <- items g "tasks",at task "assignee" == actor]
runTask :: Service -> Text -> Text -> Value -> Text -> IO Value
runTask s gid tid revision actor = startTask s gid tid revision actor Nothing
