{-# LANGUAGE OverloadedStrings #-}
module Routines
 ( Minutes, Routine(..), readRoutines, storeRoutine, initializeRoutines
 , RoutineEdit(..), save, setPaused, command, receipt, acknowledge, dispatch, restoreOccurrences
 ) where

import Control.Monad
import Control.Exception (SomeException, SomeAsyncException, fromException, throwIO, try)
import Control.Concurrent.Async (AsyncCancelled)
import qualified Command as C
import Data.Aeson hiding (pairs)
import Data.Aeson.Types (Parser)
import qualified Data.Aeson.KeyMap as KM
import Data.List (find)
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time hiding (Minutes)
import qualified Data.UUID as UUID
import qualified Data.UUID.V5 as UUID5
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString as BS
import Data.Int (Int64)
import qualified Database.SQLite.Simple as SQL
import System.IO (hPutStrLn, stderr)
import Text.Read (readMaybe)
import Types
import qualified Store as DB
import Population
import qualified Delegation as D

newtype Minutes = Minutes { minuteCount :: Int } deriving (Eq, Show)
instance FromJSON Minutes where
 parseJSON value = do
  n <- parseJSON value
  unless (n >= 1 && n <= 525600) (fail "Choose 1–525600 minutes")
  pure (Minutes n)
instance ToJSON Minutes where toJSON = toJSON . minuteCount
intervalSeconds :: Minutes -> NominalDiffTime
intervalSeconds = fromIntegral . (*60) . minuteCount

data Routine = Routine
 { routineId :: Text, routineOwner :: Text, routinePrompt :: Text, routineMinutes :: Minutes
 , routineDue :: UTCTime, routineCount :: Int, routinePaused :: Bool, routineFields :: Object
 } deriving (Eq, Show)
instance FromJSON Routine where
 parseJSON = withObject "Scheduled Routine" $ \o -> do
  due <- o .: "nextDue" >>= maybe (fail "Invalid routine due time") pure . parseTime
  count <- o .:? "runCount" .!= 0
  unless (count >= 0) (fail "Invalid routine run count")
  Routine <$> o .: "id" <*> o .: "owner" <*> o .: "prompt" <*> o .: "minutes" <*> pure due <*> pure count <*> o .:? "paused" .!= False <*> pure (foldr KM.delete o ["id","owner","prompt","minutes","runCount","paused"])
-- The persisted due-time spelling participates in deterministic occurrence IDs.
-- Retain it until the deadline changes, including legacy fractional precision.
instance ToJSON Routine where
 toJSON r = mergeFields [("id",str (routineId r)),("owner",str (routineOwner r)),("prompt",str (routinePrompt r)),("minutes",toJSON (routineMinutes r)),("nextDue",if parseTime (at (Object (routineFields r)) "nextDue") == Just (routineDue r) then get (Object (routineFields r)) "nextDue" else str (timestamp (routineDue r))),("runCount",num (routineCount r)),("paused",Bool (routinePaused r))] (Object (routineFields r))

newtype GroupId = GroupId { groupIdText :: Text } deriving (Eq, Show)
instance FromJSON GroupId where parseJSON value = GroupId <$> parseJSON value
instance ToJSON GroupId where toJSON = str . groupIdText

data ScheduledTarget = PersonalTarget AgentId | GroupTarget GroupId AgentId deriving (Eq, Show)
instance FromJSON ScheduledTarget where
 parseJSON = withObject "Scheduled target" $ \o -> do
  kind <- o .: "kind" :: Parser Text
  case kind of
   "personal" -> PersonalTarget <$> o .: "agent"
   "group" -> GroupTarget <$> o .: "group" <*> o .: "agent"
   _ -> fail "Unknown scheduled target"
instance ToJSON ScheduledTarget where
 toJSON target = case target of
  PersonalTarget agent -> obj [("kind",str "personal"),("agent",toJSON agent)]
  GroupTarget group agent -> obj [("kind",str "group"),("group",toJSON group),("agent",toJSON agent)]

data ScheduledOccurrence = ScheduledOccurrence
 { occurrenceId :: RunId, occurrenceRoutine :: Routine
 , occurrenceTarget :: ScheduledTarget, occurrenceCreated :: UTCTime
 } deriving (Eq, Show)
instance FromJSON ScheduledOccurrence where
 parseJSON = withObject "Scheduled occurrence" $ \o -> do
  created <- o .: "created" >>= maybe (fail "Invalid occurrence creation time") pure . parseTime
  occurrence <- ScheduledOccurrence <$> o .: "id" <*> o .: "routine" <*> o .: "target" <*> pure created
  unless (occurrenceId occurrence == occurrenceIdFor (occurrenceRoutine occurrence) && validTarget occurrence)
   (fail "Invalid scheduled occurrence identity or target")
  pure occurrence
instance ToJSON ScheduledOccurrence where
 toJSON occurrence = obj
  [("id",toJSON (occurrenceId occurrence)),("routine",toJSON (occurrenceRoutine occurrence))
  ,("target",toJSON (occurrenceTarget occurrence)),("created",str (timestamp (occurrenceCreated occurrence)))]

validTarget :: ScheduledOccurrence -> Bool
validTarget occurrence = case occurrenceTarget occurrence of
 PersonalTarget agent -> agentIdText agent == routineOwner (occurrenceRoutine occurrence)
 GroupTarget group _ -> groupIdText group == routineOwner (occurrenceRoutine occurrence)
readRoutines :: Service -> IO [Routine]
readRoutines s = DB.values s DB.ScheduledRoutines >>= traverse (decodeValue 500)
storeRoutine :: Service -> Routine -> IO Value
storeRoutine s = DB.saveRoutine s . toJSON

initializeRoutines :: Service -> IO ()
initializeRoutines s = do
 rows <- DB.queryRows s "SELECT sequence,value FROM scheduled_routines ORDER BY sequence" ["sequence","value"] []
 works <- DB.values s DB.Workloads
 groups <- DB.values s DB.Groups
 forM_ rows $ \row -> tryRoutine ("scheduled routine " <> jsonText (get row "sequence")) $ do
  r <- DB.decodeStored (at row "value")
  void (decodeValue 500 r :: IO Routine)
  unless (has r "runCount") $ do
   let group=findId (at r "owner") groups
       count=case group of Just g->length [t | t<-items g "tasks",get t "routine"==get r "id"]; Nothing->length [w | w<-works,get (get w "origin") "routine"==get r "id"]
   void (DB.saveRoutine s (merge r (obj [("runCount",num count),("paused",Bool (count>=1000))])))
data RoutineEdit = AddRoutine (Maybe Text) | EditRoutine Text (Maybe Int)
data RoutineWrite = NewRoutine (Maybe Text) | ReviseRoutine Value
data RoutineSelection = RoutineReplay Value | RoutineChange RoutineWrite

routineInput :: Value -> Value -> Validation (Text,Minutes)
routineInput minutes execution = do
 body <- validateValue 400 (textFieldP "prompt" 16000) (obj [("prompt",execution)])
 interval <- validateValue 400 parseJSON minutes
 check (body `notElem` ["PROMPT","TODO"]) 400 "Replace PROMPT with the work to execute"
 pure (body,interval)
selectRoutine :: RoutineEdit -> Value -> Text -> [Value] -> Validation RoutineSelection
selectRoutine edit minutes body routines = case edit of
 EditRoutine identity previous -> do
  existing <- maybe (Left (APIError 404 "Unknown Scheduled Routine in this conversation")) Right (findId identity routines)
  forM_ previous $ \old -> check (int (get existing "minutes")==old) 409 "Frequency changed; open Edit again for the current routine"
  pure (RoutineChange (ReviseRoutine existing))
 AddRoutine source -> case source >>= \call -> find (\r -> get r "sourceCall" == str call && get r "minutes" == minutes && at r "prompt" == body) routines of
  Just existing -> Right (RoutineReplay existing)
  Nothing -> Right (RoutineChange (NewRoutine source))
routineValue :: UTCTime -> Text -> Minutes -> Text -> RoutineWrite -> Value
routineValue time owner interval body change = merge initial (obj
 [("owner",str owner),("title",str (T.unwords (take 7 (T.words body)))),("minutes",toJSON interval)
 ,("prompt",str body),("updated",str (timestamp time)),("nextDue",str (timestamp (addUTCTime (intervalSeconds interval) time)))])
 where
 initial = case change of
  ReviseRoutine existing -> existing
  NewRoutine source -> obj ([("created",str (timestamp time)),("runCount",num 0),("paused",Bool False)] ++ [("sourceCall",str call) | call <- maybeToList source])

save :: Service -> Text -> Value -> Value -> RoutineEdit -> IO Value
save s owner minutes execution edit = do
 (body,interval) <- validated (routineInput minutes execution)
 if "group_" `T.isPrefixOf` owner then do
  groups <- DB.values s DB.Groups
  g <- maybe (failure 404 "Unknown Group") pure (findId owner groups)
  require (not (boolean (get g "archived"))) 409 "Restore this Group before changing its work"
 else requireActive s owner
 routines <- filter ((==owner) . (`at` "owner")) <$> DB.values s DB.ScheduledRoutines
 selected <- validated (selectRoutine edit minutes body routines)
 case selected of
  RoutineReplay existing -> pure existing
  RoutineChange change -> do
   time <- getCurrentTime
   result <- DB.saveRoutine s (routineValue time owner interval body change)
   let action = case change of NewRoutine _ -> "routine.created"; ReviseRoutine _ -> "routine.updated"
   DB.record s [owner] (at result "id") action result "admin" Nothing
   pure result
setPaused :: Service -> Text -> Text -> Bool -> IO Value
setPaused s owner identity paused = do
 routines <- readRoutines s
 r <- maybe (failure 404 "Unknown Scheduled Routine in this conversation") pure (find (\r -> routineOwner r==owner && routineId r==identity) routines)
 if routinePaused r==paused then pure (toJSON r) else do
  t <- getCurrentTime
  let updated=pauseRoutine t paused r
  result <- storeRoutine s updated
  DB.record s [owner] identity (if paused then "routine.paused" else "routine.resumed") result "admin" Nothing
  pure result
command :: Service -> Text -> Text -> Bool -> IO (Maybe Value)
command s owner input attached = case C.interpret attached (T.unpack input) of
 Left error -> failure 400 (T.pack error)
 Right Nothing -> pure Nothing
 Right (Just c) -> Just <$> case c of
  C.Create interval body -> save s owner (toJSON interval) (str (T.pack body)) (AddRoutine Nothing)
  C.Edit i old interval body -> save s owner (toJSON interval) (str (T.pack body)) (EditRoutine (T.pack i) (Just (fromIntegral old)))
  C.SetPaused i paused -> setPaused s owner (T.pack i) paused
receipt :: Value -> Text -> Text
receipt r input = action <> " @"<>at r "id"<>" ( "<>at r "title"<>" )\nStatus: "<>status<>" · "<>jsonText (get r "runCount")<>"/1000 runs\nFrequency: every "<>jsonText (get r "minutes")<>" minutes\nExecute: "<>at r "prompt"
 where action=fromMaybe "Updated" (listToMaybe (T.words input) >>= (`lookup` [("Pause","Paused"),("Resume","Resumed"),("Add","Created")]))
       status=if boolean (get r "paused") then "Paused" else "Planned"
acknowledge :: Service -> Text -> Text -> Value -> IO Value
acknowledge s owner input r = do
 i <- newId
 t <- now
 let output=receipt r input
     turn=obj [("id",str i),("flow",str "chat"),("input",str input),("status",str "done"),("created",str t),("output_at",str t)]
     messages=[obj [("role",str role),("content",str content),("turn",str i),("time",str t)] | (role,content)<-[("user",input),("agent",output)]]
 modifyState s owner $ \state -> merge state (obj [("turns",arr (items state "turns"++[toJSON turn])),("chat",arr (items state "chat"++messages))])
 DB.project s owner
 pure (obj [("id",str i),("agent",str owner),("status",str "done"),("flow",str "chat")])
data RoutineDecision = WaitForRoutine | PauseAtLimit | RunOccurrence deriving (Eq, Show)
routineDecision :: UTCTime -> Routine -> RoutineDecision
routineDecision time routine
 | routinePaused routine = WaitForRoutine
 | routineCount routine>=1000 = PauseAtLimit
 | routineDue routine<=time = RunOccurrence
 | otherwise = WaitForRoutine

advanceRoutine :: UTCTime -> Routine -> Routine
advanceRoutine time routine = routine
 { routineDue=addUTCTime (interval*fromIntegral jumps) (routineDue routine)
 , routineCount=count, routinePaused=count>=1000 }
 where
 interval=intervalSeconds (routineMinutes routine)
 jumps=floor (diffUTCTime time (routineDue routine) / interval)+1 :: Integer
 count=routineCount routine+1

pauseRoutine :: UTCTime -> Bool -> Routine -> Routine
pauseRoutine time paused routine = routine
 { routinePaused=paused
 , routineDue=if paused then routineDue routine else addUTCTime (intervalSeconds (routineMinutes routine)) time
 , routineCount=if not paused && routineCount routine>=1000 then 0 else routineCount routine
 , routineFields=KM.insert "updated" (str (timestamp time)) (routineFields routine) }

dispatch :: Service -> Pulse -> IO ()
dispatch s tick = when (pulseFrequency tick == Bph60) $ do
 restoreOccurrences s
 time <- getCurrentTime
 rows <- DB.queryRows s "SELECT sequence,value FROM scheduled_routines ORDER BY sequence" ["sequence","value"] []
 forM_ rows $ \row -> tryRoutine ("scheduled routine " <> jsonText (get row "sequence")) $ do
  value <- DB.decodeStored (at row "value")
  routine <- decodeValue 500 value
  dispatchRoutine s time routine

tryRoutine :: Text -> IO () -> IO ()
tryRoutine label action = try action >>= either (reportRoutineError label) pure

reportRoutineError :: Text -> SomeException -> IO ()
reportRoutineError label exception
 | isJust (fromException exception :: Maybe SomeAsyncException) = throwIO exception
 | isJust (fromException exception :: Maybe AsyncCancelled) = throwIO exception
 | otherwise = hPutStrLn stderr (T.unpack label <> ": " <> show exception)

dispatchRoutine :: Service -> UTCTime -> Routine -> IO ()
dispatchRoutine s time routine = case routineDecision time routine of
 WaitForRoutine -> pure ()
 PauseAtLimit -> do
  void (storeRoutine s routine { routinePaused=True })
  DB.record s [routineOwner routine] (routineId routine) "routine.paused"
   (obj [("reason",str "run limit"),("runCount",num (routineCount routine))]) "system" Nothing
 RunOccurrence -> do
  let owner=routineOwner routine
  rows <- DB.queryRows s "SELECT value FROM groups WHERE id=?" ["value"] [SQL.SQLText owner]
  group <- traverse (DB.decodeStored . (`at` "value")) (listToMaybe rows)
  when ("group_" `T.isPrefixOf` owner && isNothing group) $ failure 409 "Routine Group no longer exists"
  let agent=AgentId (maybe owner (`at` "lead") group)
      target=maybe (PersonalTarget agent) (\_ -> GroupTarget (GroupId owner) agent) group
  inactive <- retired s (agentIdText agent)
  unless (inactive || maybe False (boolean . (`get` "archived")) group) $ do
   admitted <- admitDue s time routine target
   forM_ admitted (materializeOccurrence s)

routineSequence :: Routine -> IO Int64
routineSequence routine = maybe (failure 500 "Invalid scheduled routine ID") pure
 (T.stripPrefix "routine_sch_" (routineId routine) >>= readMaybe . T.unpack)

occurrenceIdFor :: Routine -> RunId
occurrenceIdFor routine = RunId (T.filter (/='-') (UUID.toText
 (UUID5.generateNamed UUID5.namespaceURL (BS.unpack (TE.encodeUtf8
 (routineId routine <> ":" <> at (toJSON routine) "nextDue"))))))

occurrenceTurn :: ScheduledOccurrence -> Turn
occurrenceTurn occurrence = newTurn (occurrenceId occurrence) Chat
 (routinePrompt routine) (timestamp (occurrenceCreated occurrence)) origin True
 where
 routine=occurrenceRoutine occurrence
 value=toJSON routine
 origin=obj [("routine",str (routineId routine)),("routineOwner",str (routineOwner routine))
  ,("due",get value "nextDue"),("title",get value "title"),("minutes",toJSON (routineMinutes routine))
  ,("taskType",str "automated"),("executionPrompt",str (routinePrompt routine))]

admitDue :: Service -> UTCTime -> Routine -> ScheduledTarget -> IO (Maybe ScheduledOccurrence)
admitDue s time candidate target = do
 sequenceId <- routineSequence candidate
 DB.connection s $ \db -> SQL.withTransaction db $ do
  rows <- DB.queryRowsOn db "SELECT value FROM scheduled_routines WHERE sequence=?" ["value"] [SQL.SQLInteger sequenceId]
  case rows of
   [] -> pure Nothing
   row:_ -> do
    value <- DB.decodeStored (at row "value")
    routine <- decodeValue 500 value
    case routineDecision time routine of
     RunOccurrence -> do
      let occurrence=ScheduledOccurrence (occurrenceIdFor routine) routine target time
          identity=runIdText (occurrenceId occurrence)
          advanced=advanceRoutine time routine
          owner=routineOwner routine
      require (validTarget occurrence) 409 "Routine owner changed before admission"
      validateTargetOn db target
      DB.executeOn db "INSERT INTO routine_occurrences(id,value,materialized) VALUES (?,?,0)"
       [SQL.SQLText identity,SQL.SQLText (jsonText (toJSON occurrence))]
      void (DB.saveRoutineOn db (toJSON advanced))
      DB.recordOn s db [owner] (routineId routine) "routine.triggered"
       (obj [("turn",str identity),("runCount",num (routineCount advanced))
       ,("nextDue",str (timestamp (routineDue advanced)))]) "system" Nothing
      when (routinePaused advanced) $ DB.recordOn s db [owner] (routineId routine) "routine.paused"
       (obj [("reason",str "run limit"),("runCount",num (routineCount advanced))]) "system" Nothing
      pure (Just occurrence)
     _ -> pure Nothing

validateTargetOn :: SQL.Connection -> ScheduledTarget -> IO ()
validateTargetOn db target = case target of
 PersonalTarget _ -> pure ()
 GroupTarget group agent -> do
  rows <- DB.queryRowsOn db "SELECT value FROM groups WHERE id=?" ["value"] [SQL.SQLText (groupIdText group)]
  value <- case rows of
   row:_ -> DB.decodeStored (at row "value")
   [] -> failure 409 "Group disappeared before routine admission"
  require (not (boolean (get value "archived")) && get value "lead" == toJSON agent)
   409 "Group changed before routine admission"

materializeOccurrence :: Service -> ScheduledOccurrence -> IO ()
materializeOccurrence s occurrence = do
 case occurrenceTarget occurrence of
  PersonalTarget agent -> materializePersonal s agent (occurrenceTurn occurrence)
  GroupTarget group agent -> materializeGroup s group agent (occurrenceTurn occurrence)
 DB.executeSQL s "UPDATE routine_occurrences SET materialized=1 WHERE id=?"
  [SQL.SQLText (runIdText (occurrenceId occurrence))]

materializePersonal :: Service -> AgentId -> Turn -> IO ()
materializePersonal s agent turn = do
 let owner=agentIdText agent
     identity=runIdText (turnId turn)
     created=get (toJSON turn) "created"
     origin=turnField turn "origin"
 existingState <- agentState s owner
 forM_ (find ((==turnId turn) . turnId) (agentTurns existingState)) $ \existing ->
  require (turnField existing "origin" == origin) 409 "Occurrence ID belongs to another Turn"
 modifyState s owner $ \currentState ->
  if isJust (findId identity (items currentState "turns")) then currentState
  else merge currentState (obj
   [("turns",arr (items currentState "turns"++[toJSON turn]))
   ,("chat",arr (items currentState "chat"++[obj
     [("role",str "user"),("content",str (turnInput turn)),("turn",str identity)
     ,("time",created),("origin",origin)]]))])
 DB.project s owner
 found <- D.findWork s (turnId turn)
 when (isNothing found) (D.begin s agent turn)
 (work,call) <- D.getWork s (turnId turn)
 when (D.callState call == D.CallRunning && null (D.workItems work "decisions")) $
  D.saveWork s (D.withCall work call { D.callProgress=D.QueuedAt (D.callNode call) })

materializeGroup :: Service -> GroupId -> AgentId -> Turn -> IO ()
materializeGroup s groupId agent turn = DB.connection s $ \db -> SQL.withTransaction db $ do
 let owner=groupIdText groupId
     identity=runIdText (turnId turn)
 rows <- DB.queryRowsOn db "SELECT value FROM groups WHERE id=?" ["value"] [SQL.SQLText owner]
 group <- case rows of
  row:_ -> DB.decodeStored (at row "value")
  [] -> failure 409 "Group disappeared before routine materialization"
 forM_ (findId identity (items group "requests")) $ \existing ->
  require (get (get existing "origin") "routine" == get (turnField turn "origin") "routine")
   409 "Occurrence ID belongs to another Group request"
 unless (isJust (findId identity (items group "requests"))) $ do
  let origin=turnField turn "origin"
      created=get (toJSON turn) "created"
      body=turnInput turn
      taskId="gtask_"<>identity
      groupOrigin=merge origin (obj [("group",str owner),("author",str "admin")])
      message=obj [("id",str identity),("author",str "admin"),("text",str body),("created",created),("routine",get origin "routine")]
      task=obj [("id",str taskId),("title",get origin "title"),("body",str body),("assignee",toJSON agent),("state",str "in_progress"),("deleted",Bool False),("revision",num 1),("created",created),("author",str "admin"),("updated_by",str "admin"),("history",arr []),("results",arr []),("taskType",str "automated"),("routine",get origin "routine")]
      request=obj [("id",str identity),("message",str identity),("target",toJSON agent),("root",str identity),("created",created),("status",str "queued"),("settled",Bool False),("task",str taskId),("task_revision",num 1),("origin",groupOrigin)]
  DB.putValueOn db DB.Groups owner (merge group (obj
   [("messages",arr (items group "messages"++[message])),("tasks",arr (items group "tasks"++[task]))
   ,("requests",arr (items group "requests"++[request]))
   ,("revision",num (int (get group "revision")+1)),("updated",created)]))

restoreOccurrences :: Service -> IO ()
restoreOccurrences s = do
 rows <- DB.queryRows s "SELECT id,value FROM routine_occurrences WHERE materialized=0 ORDER BY rowid" ["id","value"] []
 forM_ rows $ \row -> tryRoutine ("scheduled occurrence " <> at row "id") $ do
  value <- DB.decodeStored (at row "value")
  occurrence <- decodeValue 500 value
  require (toJSON (occurrenceId occurrence) == get row "id") 500 "Stored occurrence ID mismatch"
  materializeOccurrence s occurrence
