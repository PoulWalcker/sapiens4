{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields, NoFieldSelectors, OverloadedRecordDot #-}
{-# OPTIONS_GHC -Werror=incomplete-patterns #-}
module GroupWork where

import Control.Monad
import Data.Aeson hiding (pairs)
import qualified Data.Aeson.KeyMap as KM
import Data.Maybe (fromMaybe, isNothing, isJust)
import Data.Aeson.Types (parseEither, parseMaybe)
import Data.Text (Text)
import Data.Time (UTCTime, getCurrentTime)
import qualified Data.Text as T
import Types
import qualified Store as DB
import GroupModel

-- Parsing preserves the established wire format; decisions below use typed values.
data GroupTask = GroupTask
 { identity :: TaskId, title :: Text, body :: Text, assignee :: AgentId
 , state :: TaskState, deleted :: Bool, revision :: Int, metadata :: Object
 } deriving (Eq, Show)
instance FromJSON GroupTask where
 parseJSON = withObject "Group task" $ \o -> GroupTask <$> o .: "id" <*> o .: "title" <*> o .: "body" <*> o .: "assignee" <*> o .: "state" <*> o .: "deleted" <*> o .: "revision" <*> pure (foldr KM.delete o ["id","title","body","assignee","state","deleted","revision"])
instance ToJSON GroupTask where
 toJSON task = mergeFields [("id",toJSON task.identity),("title",str task.title),("body",str task.body),("assignee",toJSON task.assignee),("state",toJSON task.state),("deleted",Bool task.deleted),("revision",num task.revision)] (Object task.metadata)

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
 , output :: Maybe Text, error :: Maybe Text
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
newTask identity created actor draft = GroupTask identity draft.title draft.body draft.assignee Backlog False 1
 (KM.fromList [("created",str (timestamp created)),("author",str actor),("updated_by",str actor),("history",arr []),("results",arr [])])
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
     result = obj [("turn",toJSON completion.run),("author",toJSON completion.owner),("text",toJSON completion.output),("error",toJSON completion.error),("revision",num completion.revision),("stale",Bool stale),("created",str (timestamp time))]
     metadata = KM.insert "results" (arr (items (Object task.metadata) "results" ++ [result])) task.metadata
     changed = task { metadata=metadata }
 in if stale then (changed,StaleResultRecorded)
    else (changed { state=if completedStatus completion.status then TaskDone else Backlog, revision=task.revision+1, metadata=KM.insert "updated_by" (toJSON completion.owner) metadata },TaskUpdated)

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
startTask :: Service -> Text -> Text -> Value -> Text -> Text -> IO Value
startTask s gid tid revision actor root = do
 group <- getGroup s gid True; authorizeGroup s group actor Member
 task <- readTask group tid
 members <- field 500 group "members"
 requests <- readRequests group
 pending <- traverse (\request -> field 500 (Object (requestFields request)) "task") [request | request <- requests, requestStatus request `elem` [Queued,Running], KM.member "task" (requestFields request)]
 changed <- taskResult (beginTask members pending (parseMaybe parseJSON revision) actor task)
 initial <- message actor (task.title <> "\n\n" <> task.body)
 let entry=mergeFields ([("task",str tid)] ++ [("memo_pending",Bool True) | actor == "admin"]) initial
 request <- newRequest entry (agentIdText task.assignee) root
 let call=mergeFields [("task",str tid),("task_revision",num changed.revision)] request
 void (saveGroup s (append "requests" call (append "messages" entry (replace "tasks" (toJSON changed) group))) actor "task started" (str tid))
 pure (toJSON changed)

newRequest :: Value -> Text -> Text -> IO Value
newRequest entry target root = do
 i <- newId; t <- now
 pure (toJSON (GroupRequest (RunId i) (AgentId target) Queued False (KM.fromList [("message",get entry "id"),("root",if T.null root then get entry "id" else str root),("created",str t)])))
settleTask :: Value -> Value -> Value -> IO (Value, Settlement)
settleTask group request turn = if not (has request "task") then pure (group,NoTask) else do
 task <- readTask group (at request "task"); time <- getCurrentTime
 completion <- TaskCompletion <$> field 500 request "id" <*> field 500 request "target" <*> field 500 request "task_revision"
                              <*> field 500 turn "status" <*> field 500 turn "output" <*> field 500 turn "error"
 let (changed,settlement) = settleTaskResult time completion task
 pure (replace "tasks" (toJSON changed) group,settlement)
assignedTasks :: Service -> Text -> IO [Value]
assignedTasks s actor = do
 groups <- DB.values s DB.Groups
 pure [merge task (obj [("group",get g "id"),("group_name",get g "name"),("group_archived",get g "archived")]) | g <- groups,task <- items g "tasks",at task "assignee" == actor]
runTask :: Service -> Text -> Text -> Value -> Text -> IO Value
runTask s gid tid revision actor = startTask s gid tid revision actor ""
