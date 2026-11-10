{-# LANGUAGE OverloadedStrings #-}
module Routines
 ( Minutes, Routine(..), readRoutines, storeRoutine, initializeRoutines
 , RoutineEdit(..), save, setPaused, command, receipt, acknowledge, dispatch
 ) where

import Control.Monad
import qualified Command as C
import Data.Aeson hiding (pairs)
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
readRoutines :: Service -> IO [Routine]
readRoutines s = DB.values s DB.ScheduledRoutines >>= traverse (decodeValue 500)
storeRoutine :: Service -> Routine -> IO Value
storeRoutine s = DB.saveRoutine s . toJSON

initializeRoutines :: Service -> IO ()
initializeRoutines s = do
 routines <- DB.values s DB.ScheduledRoutines
 works <- DB.values s DB.Workloads
 groups <- DB.values s DB.Groups
 forM_ routines $ \r -> unless (has r "runCount") $ do
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
 t <- getCurrentTime
 routines <- readRoutines s
 forM_ routines $ \routine -> do
  let owner=routineOwner routine; count=routineCount routine; r=toJSON routine
  case routineDecision t routine of
   WaitForRoutine -> pure ()
   PauseAtLimit -> do
    void (storeRoutine s routine { routinePaused=True })
    DB.record s [owner] (at r "id") "routine.paused" (obj [("reason",str "run limit"),("runCount",num count)]) "system" Nothing
   RunOccurrence -> do
    groups <- DB.values s DB.Groups
    let group=findId owner groups; a=maybe owner (`at` "lead") group
    inactive <- retired s a
    unless (inactive || maybe False (boolean . (`get` "archived")) group) $ do
     let identity=T.filter (/='-') (UUID.toText (UUID5.generateNamed UUID5.namespaceURL (BS.unpack (TE.encodeUtf8 (at r "id"<>":"<>at r "nextDue")))))
         origin=obj [("routine",get r "id"),("routineOwner",str owner),("title",get r "title"),("minutes",get r "minutes"),("taskType",str "automated"),("executionPrompt",get r "prompt")]
         turn=newTurn (RunId identity) Chat (routinePrompt routine) (timestamp t) origin True
     case group of
      Nothing -> do
       projected <- DB.turns s
       when (isNothing (findId identity projected)) $ do
        modifyState s a $ \state -> merge state (obj [("turns",arr (items state "turns"++[toJSON turn])),("chat",arr (items state "chat"++[obj [("role",str "user"),("content",get r "prompt"),("turn",str identity),("time",str (timestamp t)),("origin",origin)]]))])
        DB.project s a
       D.begin s (AgentId a) turn
       (w,c) <- D.getWork s (RunId identity)
       when (D.callState c==D.CallRunning && null (D.workItems w "decisions")) $ D.saveWork s (D.withCall w c { D.callProgress=D.QueuedAt (D.callNode c) })
      Just g -> when (isNothing (findId identity (items g "requests"))) $ do
       let taskId="gtask_"<>identity; groupOrigin=merge origin (obj [("group",str owner),("author",str "admin")])
           message=obj [("id",str identity),("author",str "admin"),("text",get r "prompt"),("created",str (timestamp t)),("routine",get r "id")]
           task=obj [("id",str taskId),("title",get r "title"),("body",get r "prompt"),("assignee",str a),("state",str "in_progress"),("deleted",Bool False),("revision",num 1),("created",str (timestamp t)),("author",str "admin"),("updated_by",str "admin"),("history",arr []),("results",arr []),("taskType",str "automated"),("routine",get r "id")]
           request=obj [("id",str identity),("message",str identity),("target",str a),("root",str identity),("created",str (timestamp t)),("status",str "queued"),("settled",Bool False),("task",str taskId),("task_revision",num 1),("origin",groupOrigin)]
       DB.putValue s DB.Groups owner (merge g (obj [("messages",arr (items g "messages"++[message])),("tasks",arr (items g "tasks"++[task])),("requests",arr (items g "requests"++[request])),("revision",num (int (get g "revision")+1)),("updated",str (timestamp t))]))
     let advanced=advanceRoutine t routine
         next=routineDue advanced
     void (storeRoutine s advanced)
     DB.record s [owner] (at r "id") "routine.triggered" (obj [("turn",str identity),("runCount",num (count+1)),("nextDue",str (timestamp next))]) "system" Nothing
     when (count+1>=1000) $ DB.record s [owner] (at r "id") "routine.paused" (obj [("reason",str "run limit"),("runCount",num (count+1))]) "system" Nothing
