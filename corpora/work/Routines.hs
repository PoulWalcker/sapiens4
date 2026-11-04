{-# LANGUAGE OverloadedStrings #-}
module Routines
 ( Minutes(..), Revision(..), RoutineFields(..), RoutineContract(..)
 , RoutineSchedule(..), Definition(..), readDefinitions, readRoutines
 , definitionTitle, definitionGoal, definitionCriteria, intervalView, bumpRevision
 , setSchedule, setSchedulePaused, storeRoutine, initializeRoutines
 , contractGeneration, changeContract
 , RoutineEdit(..), save, setPaused, command, receipt, acknowledge, dispatch
 ) where

import Control.Monad
import Control.Exception (catch)
import qualified Command as C
import Data.Aeson hiding (pairs)
import Data.Aeson.Types (Parser)
import qualified Data.Aeson.KeyMap as KM
import Data.List (find)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import qualified Data.UUID as UUID
import qualified Data.UUID.V5 as UUID5
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString as BS
import qualified Database.SQLite.Simple as SQL
import Types
import qualified Store as DB
import Population
import qualified Delegation as D
import Schedule (Minutes(..), ScheduleRule)
import qualified Schedule

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

data RoutineContract = LegacyPrompt Text | AcceptedContract RoutineFields deriving (Eq, Show)
data RoutineSchedule = RoutineSchedule
 { scheduleRule :: ScheduleRule, scheduleDue :: UTCTime, scheduleCount :: Int, schedulePaused :: Bool
 } deriving (Eq, Show)
data Definition = Definition
 { definitionId :: Text, definitionOwner :: Text, definitionRevision :: Revision
 , definitionContract :: RoutineContract, definitionSchedule :: Maybe RoutineSchedule
 , definitionDeleted :: Bool, definitionFields :: Object
 } deriving (Eq, Show)
instance FromJSON Definition where
 parseJSON value = withObject "Routine definition" (\o -> do
  contract <- case (KM.member "goal" o,KM.member "criteria" o) of
   (True,True) -> AcceptedContract <$> parseJSON value
   (False,False) -> LegacyPrompt <$> o .: "prompt"
   _ -> fail "Routine goal and criteria must be supplied together"
  revision <- case KM.lookup "revision" o of
   Just stored -> parseJSON stored
   Nothing -> case contract of
    LegacyPrompt _ -> pure (Revision 1)
    AcceptedContract _ -> fail "Routine revision is required"
  forM_ (KM.lookup "contractGeneration" o) (\generation -> void (parseJSON generation :: Parser ContractGeneration))
  trigger <- routineTrigger value
  schedule <- case trigger of
   ManualTrigger -> pure Nothing
   IntervalTrigger -> do
    due <- o .: "nextDue" >>= maybe (fail "Invalid routine due time") pure . parseTime
    count <- o .:? "runCount" .!= 0
    unless (count>=0) (fail "Invalid routine run count")
    Just <$> (RoutineSchedule <$> Schedule.storedRule value <*> pure due <*> pure count <*> o .:? "paused" .!= False)
  Definition <$> o .: "id" <*> o .: "owner" <*> pure revision <*> pure contract <*> pure schedule <*> o .:? "deleted" .!= False <*> pure o) value
-- The persisted due-time spelling participates in deterministic occurrence IDs.
-- Retain it until the deadline changes, including legacy fractional precision.
instance ToJSON Definition where
 toJSON d = mergeFields (identity++contract++schedule) (remove ["minutes","schedule","nextDue","paused"] original)
  where
   original=Object (definitionFields d)
   explicit=case definitionContract d of AcceptedContract _ -> True; LegacyPrompt _ -> False
   identity=[("id",str (definitionId d)),("owner",str (definitionOwner d))]
     ++[("revision",toJSON (definitionRevision d)) | explicit || has original "revision"]
     ++[("deleted",Bool (definitionDeleted d)) | definitionDeleted d || explicit || has original "deleted"]
   contract=case definitionContract d of
    LegacyPrompt body -> [("prompt",str body)]
    AcceptedContract fields -> [("title",str (fieldTitle fields)),("goal",str (fieldGoal fields)),("criteria",toJSON (NE.toList (fieldCriteria fields)))]
   schedule=case definitionSchedule d of
    Nothing -> [("trigger",toJSON ManualTrigger)]
    Just interval -> Schedule.ruleFields (scheduleRule interval) ++ [("runCount",num (scheduleCount interval)),("paused",Bool (schedulePaused interval)),("nextDue",if parseTime (at original "nextDue")==Just (scheduleDue interval) then get original "nextDue" else str (timestamp (scheduleDue interval)))]
      ++[("trigger",toJSON IntervalTrigger) | explicit || has original "trigger"]
definitionTitle :: Definition -> Text
definitionTitle d = case definitionContract d of
 AcceptedContract fields -> fieldTitle fields
 LegacyPrompt body -> at (Object (definitionFields d)) "title" `orText` T.unwords (take 7 (T.words body))
 where orText a b=if T.null a then b else a
definitionGoal :: Definition -> Text
definitionGoal d = case definitionContract d of AcceptedContract fields -> fieldGoal fields; LegacyPrompt body -> body
definitionCriteria :: Definition -> Maybe (NonEmpty Text)
definitionCriteria d = case definitionContract d of AcceptedContract fields -> Just (fieldCriteria fields); LegacyPrompt _ -> Nothing
-- Compatibility output only: the explicit definition still stores one goal.
intervalView :: Definition -> Value
intervalView definition = put "prompt" (str (definitionGoal definition)) (toJSON definition)
bumpRevision :: Definition -> Definition
bumpRevision d = let Revision previous=definitionRevision d; next=Revision (previous+1)
 in d {definitionRevision=next,definitionFields=KM.insert "revision" (toJSON next) (definitionFields d)}
contractGeneration :: Definition -> ContractGeneration
contractGeneration d = ContractGeneration (max 1 (int (get (Object (definitionFields d)) "contractGeneration")))
changeContract :: RoutineContract -> Definition -> Definition
changeContract contract d = d {definitionContract=contract,definitionFields=KM.insert "contractGeneration" (toJSON next) fields}
 where
 current=contractGeneration d
 fields=if next==current then definitionFields d else KM.delete "acceptedTaskSource" (KM.delete "acceptedTask" (definitionFields d))
 semantic value = case value of
  LegacyPrompt goal -> (goal,Nothing)
  AcceptedContract fields -> (fieldGoal fields,Just (fieldCriteria fields))
 next=if semantic contract==semantic (definitionContract d) then current else let ContractGeneration n=current in ContractGeneration (n+1)
setSchedule :: UTCTime -> Maybe ScheduleRule -> Definition -> Definition
setSchedule time rule d = d {definitionSchedule=make <$> rule}
 where
 make selected = case (selected,definitionSchedule d) of
  (Schedule.Daily _ _ _,Just previous) | scheduleRule previous==selected -> previous
  _ -> RoutineSchedule selected (Schedule.nextDue selected time Nothing) count paused
 count=maybe (max 0 (int (get (Object (definitionFields d)) "runCount"))) scheduleCount (definitionSchedule d)
 paused=maybe False schedulePaused (definitionSchedule d)
setSchedulePaused :: UTCTime -> Bool -> Definition -> Either Text Definition
setSchedulePaused time paused d = case definitionSchedule d of
 Nothing -> Left "This Routine has no schedule"
 Just schedule | schedulePaused schedule==paused -> Right d
 Just schedule -> Right d {definitionSchedule=Just schedule
  {schedulePaused=paused,scheduleDue=if paused then scheduleDue schedule else Schedule.nextDue (scheduleRule schedule) time Nothing
  ,scheduleCount=if not paused && scheduleCount schedule>=1000 then 0 else scheduleCount schedule}}
readDefinitions :: Service -> IO [Definition]
readDefinitions s = DB.values s DB.ScheduledRoutines >>= traverse (decodeValue 500)
readRoutines :: Service -> IO [Definition]
readRoutines s = filter (isJust . definitionSchedule) <$> readDefinitions s
storeRoutine :: Service -> Definition -> IO Value
storeRoutine s = DB.saveRoutine s . toJSON

initializeRoutines :: Service -> IO ()
initializeRoutines s = do
 routines <- DB.values s DB.ScheduledRoutines
 works <- DB.values s DB.Workloads
 groups <- DB.values s DB.Groups
 forM_ routines $ \r -> do
  trigger <- parseValue 500 routineTrigger r
  when (trigger==IntervalTrigger && not (has r "runCount")) $ do
   let group=findId (at r "owner") groups
       count=case group of Just g->length [t | t<-items g "tasks",get t "routine"==get r "id"]; Nothing->length [w | w<-works,get (get w "origin") "routine"==get r "id"]
   void (DB.saveRoutine s (merge r (obj [("runCount",num count),("paused",Bool (count>=1000))])))
 enrolled <- DB.values s DB.ScheduledRoutines
 forM_ enrolled $ \r -> unless (has r "contractGeneration") $
  void (DB.saveRoutine s (put "contractGeneration" (num 1) r))
data RoutineEdit = AddRoutine (Maybe Text) | EditRoutine Text (Maybe Int)
data RoutineWrite = NewRoutine (Maybe Text) | ReviseRoutine Value
data RoutineSelection = RoutineReplay Value | RoutineChange RoutineWrite

routineInput :: Value -> Value -> Validation (Text,ScheduleRule)
routineInput minutes execution = do
 body <- validateValue 400 (textFieldP "prompt" 16000) (obj [("prompt",execution)])
 interval <- validateValue 400 Schedule.storedRule (case minutes of Object _ -> minutes; _ -> obj [("minutes",minutes)])
 check (body `notElem` ["PROMPT","TODO"]) 400 "Replace PROMPT with the work to execute"
 pure (body,interval)
selectRoutine :: RoutineEdit -> Value -> Text -> [Value] -> Validation RoutineSelection
selectRoutine edit minutes body routines = case edit of
 EditRoutine identity previous -> do
  existing <- maybe (Left (APIError 404 "Unknown Scheduled Routine in this conversation")) Right (findId identity routines)
  check (not (boolean (get existing "deleted"))) 409 "Routine is deleted"
  forM_ previous $ \old -> check (int (get existing "minutes")==old) 409 "Frequency changed; open Edit again for the current routine"
  pure (RoutineChange (ReviseRoutine existing))
 AddRoutine source -> case source >>= \call -> find (\r -> get r "sourceCall" == str call && (if get minutes "schedule"/=Null then get r "schedule"==get minutes "schedule" else get r "minutes"==minutes) && at r "prompt" == body) routines of
  Just existing -> Right (RoutineReplay existing)
  Nothing -> Right (RoutineChange (NewRoutine source))
save :: Service -> Text -> Value -> Value -> RoutineEdit -> IO Value
save s owner minutes execution edit = do
 (body,interval) <- validated (routineInput minutes execution)
 if "group_" `T.isPrefixOf` owner then do
  groups <- DB.values s DB.Groups
  g <- maybe (failure 404 "Unknown Group") pure (findId owner groups)
  require (not (boolean (get g "archived"))) 409 "Restore this Group before changing its work"
 else requireActive s owner
 definitions <- filter ((==owner) . definitionOwner) <$> readDefinitions s
 selected <- validated (selectRoutine edit minutes body (map toJSON definitions))
 case selected of
  RoutineReplay prior -> pure prior
  RoutineChange change -> do
   t <- getCurrentTime
   let (existing,source)=case change of
        NewRoutine selectedSource -> (Nothing,selectedSource)
        ReviseRoutine prior -> (find ((==at prior "id") . definitionId) definitions,Nothing)
   let initial=fromMaybe (Definition "" owner (Revision 1) (LegacyPrompt body) Nothing False
         (KM.fromList ([("created",str (timestamp t)),("revision",num 1)]++[("sourceCall",str c) | c<-maybeToList source]))) existing
       contract=case definitionContract initial of
        LegacyPrompt _ -> LegacyPrompt body
        AcceptedContract fields -> AcceptedContract fields {fieldGoal=body}
       changed=changeContract contract (setSchedule t (Just interval) initial)
       updated=changed {definitionFields=KM.insert "updated" (str (timestamp t)) (case contract of
          LegacyPrompt _ -> KM.insert "title" (str (T.unwords (take 7 (T.words body)))) (definitionFields changed)
          AcceptedContract _ -> definitionFields changed)}
       value=toJSON (if isJust existing then bumpRevision updated else updated)
   r <- DB.saveRoutine s (if isJust existing then value else remove ["id"] value)
   DB.record s [owner] (at r "id") (if isJust existing then "routine.updated" else "routine.created") r "admin" Nothing
   pure (put "prompt" (str body) r)
setPaused :: Service -> Text -> Text -> Bool -> IO Value
setPaused s owner identity paused = do
 routines <- readDefinitions s
 r <- maybe (failure 404 "Unknown Scheduled Routine in this conversation") pure (find (\r -> definitionOwner r==owner && definitionId r==identity) routines)
 require (not (definitionDeleted r)) 409 "Routine is deleted"
 schedule <- maybe (failure 409 "This Routine has no schedule") pure (definitionSchedule r)
 if schedulePaused schedule==paused then pure (intervalView r) else do
  t <- getCurrentTime
  changed <- either (failure 409) pure (setSchedulePaused t paused r)
  let updated=bumpRevision changed {definitionFields=KM.insert "updated" (str (timestamp t)) (definitionFields changed)}
  result <- storeRoutine s updated
  DB.record s [owner] identity (if paused then "routine.paused" else "routine.resumed") result "admin" Nothing
  pure (put "prompt" (str (definitionGoal updated)) result)
command :: Service -> Text -> Text -> Bool -> IO (Maybe Value)
command s owner input attached = case C.interpret attached (T.unpack input) of
 Left error -> failure 400 (T.pack error)
 Right Nothing -> pure Nothing
 Right (Just c) -> Just <$> case c of
  C.Create interval body -> save s owner (toJSON interval) (str (T.pack body)) (AddRoutine Nothing)
  C.Edit i old interval body -> save s owner (toJSON interval) (str (T.pack body)) (EditRoutine (T.pack i) (Just (fromIntegral old)))
  C.SetPaused i paused -> setPaused s owner (T.pack i) paused
receipt :: Value -> Text -> Text
receipt r input = action <> " @"<>at r "id"<>" ( "<>at r "title"<>" )\nStatus: "<>status<>" · "<>jsonText (get r "runCount")<>"/1000 runs\nFrequency: "<>frequency<>"\nExecute: "<>text (get r "prompt" `fallback` get r "goal")
 where frequency=case validateValue 500 Schedule.storedRule r of Right rule -> Schedule.frequency rule; Left _ -> "on request"
       action=fromMaybe "Updated" (listToMaybe (T.words input) >>= (`lookup` [("Pause","Paused"),("Resume","Resumed"),("Add","Created")]))
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
-- Admission identity uses the original persisted deadline, not a reformatted time.
occurrenceIdentity :: Definition -> Text
occurrenceIdentity definition = T.filter (/='-') (UUID.toText (UUID5.generateNamed UUID5.namespaceURL
 (BS.unpack (TE.encodeUtf8 (definitionId definition<>":"<>at (toJSON definition) "nextDue")))))
advanceSchedule :: UTCTime -> RoutineSchedule -> RoutineSchedule
advanceSchedule time schedule =
 let count=scheduleCount schedule+1
 in schedule {scheduleDue=Schedule.nextDue (scheduleRule schedule) time (Just (scheduleDue schedule)),scheduleCount=count,schedulePaused=count>=1000}

dispatch :: Service -> Pulse -> IO ()
dispatch s tick = when (pulseFrequency tick==Bph60) $ do
 time <- getCurrentTime
 routines <- readRoutines s
 forM_ routines $ \candidate -> (forM_ (definitionSchedule candidate) $ \schedule ->
  when (not (definitionDeleted candidate) && not (schedulePaused schedule) && (scheduleCount schedule>=1000 || scheduleDue schedule<=time)) $ do
   let owner=definitionOwner candidate
   groups <- DB.values s DB.Groups
   let group=findId owner groups
       agent=maybe owner (`at` "lead") group
   inactive <- retired s agent
   unless (inactive || maybe False (boolean . (`get` "archived")) group) $ do
    -- File-backed legacy state is observed before entering SQLite's transaction.
    -- New managed calls restore from the same database through agentState.
    works <- D.readWorks s
    let previous=find ((==occurrenceIdentity candidate) . D.workloadId) works
        owners=maybe [agent] (\w -> map (agentIdText . D.callOwner) (NE.toList (D.workCalls w))) previous
    runtime <- concatMap agentTurns <$> traverse (agentState s) (unique owners)
    DB.connection s $ \db -> SQL.withTransaction db $ do
     rows <- DB.queryRowsOn db "SELECT value FROM scheduled_routines ORDER BY sequence" ["value"] [] >>= traverse (DB.decodeStored . (`at` "value"))
     definitions <- traverse (decodeValue 500) rows
     current <- maybe (failure 409 "Routine disappeared before admission") pure (find ((==definitionId candidate) . definitionId) definitions)
     forM_ (definitionSchedule current) $ \interval -> when (not (definitionDeleted current) && not (schedulePaused interval)) $
      if scheduleCount interval>=1000 then do
       void (DB.saveRoutineOn db (toJSON current {definitionSchedule=Just interval {schedulePaused=True}}))
       DB.recordOn s db [owner] (definitionId current) "routine.paused" (obj [("reason",str "run limit"),("runCount",num (scheduleCount interval))]) "system" Nothing
      else when (scheduleDue interval<=time) $ do
       let identity=occurrenceIdentity current
           origin=obj (Schedule.ruleFields (scheduleRule interval) ++ [("routine",str (definitionId current)),("routineOwner",str owner),("occurrence",str identity),("trigger",toJSON IntervalTrigger),("due",get (toJSON current) "nextDue"),("title",str (definitionTitle current)),("taskType",str "automated"),("executionPrompt",str (definitionGoal current))])
           turn=newTurn (RunId identity) Chat (definitionGoal current) (timestamp time) origin True
           snapshot_=toJSON current
           accepted=case definitionContract current of
            LegacyPrompt _ -> Nothing
            AcceptedContract fields -> Just (D.TaskSpec (fieldGoal fields) [] (fieldCriteria fields) (fieldCriteria fields) mempty)
       case group of
        Nothing -> admitPersonalOn db (AgentId agent) turn snapshot_ accepted runtime
        Just _ -> do
         stored <- DB.queryRowsOn db "SELECT value FROM groups WHERE id=?" ["value"] [SQL.SQLText owner] >>= traverse (DB.decodeStored . (`at` "value"))
         g <- maybe (failure 409 "Group disappeared before admission") pure (listToMaybe stored)
         require (not (boolean (get g "archived"))) 409 "Group is archived"
         admitGroupOn db current g turn
       let next=advanceSchedule time interval
       void (DB.saveRoutineOn db (toJSON current {definitionSchedule=Just next}))
       DB.recordOn s db [owner] (definitionId current) "routine.triggered" (obj [("turn",str identity),("runCount",num (scheduleCount next)),("nextDue",str (timestamp (scheduleDue next)))]) "system" Nothing
       when (schedulePaused next) $ DB.recordOn s db [owner] (definitionId current) "routine.paused" (obj [("reason",str "run limit"),("runCount",num (scheduleCount next))]) "system" Nothing)
  `catch` reportDispatchFailure ("Scheduled Routine enqueue failed: " <> definitionId candidate)
 where unique=foldr (\x xs -> if x `elem` xs then xs else x:xs) []

admitPersonalOn :: SQL.Connection -> AgentId -> Turn -> Value -> Maybe D.TaskSpec -> [Turn] -> IO ()
admitPersonalOn db owner turn snapshot_ accepted runtime = do
 let identity=runIdText (turnId turn)
 rows <- DB.queryRowsOn db "SELECT value FROM workloads WHERE id=?" ["value"] [SQL.SQLText identity] >>= traverse (DB.decodeStored . (`at` "value"))
 existing <- traverse (decodeValue 500) (listToMaybe rows)
 canonical <- DB.queryRowsOn db "SELECT id FROM managed_turns" ["id"] []
 case existing of
  Just work -> do
   require (get (D.workField work "origin") "routine"==get (turnField turn "origin") "routine") 409 "Occurrence identity belongs to another Routine"
   let known call=any ((==D.callIdentity call) . turnId) runtime || any ((==runIdText (D.callIdentity call)) . (`at` "id")) canonical
       pendingChild call=isJust (D.callParent call) && D.callState call==D.CallQueued
       missingIds=[D.callIdentity call | call<-D.frontierCalls work,D.callState call `elem` [D.CallQueued,D.CallRunning],not (known call),not (pendingChild call)]
   unless (null missingIds) $ do
    let missing call=if D.callIdentity call `elem` missingIds then (D.setCallFields [("error",str "Existing admission has no runtime evidence; restore its saved execution before retrying")] call) {D.callProgress=D.UnresolvedAt (D.callNode call) Nothing} else call
    DB.putValueOn db DB.Workloads identity (toJSON work {D.workCalls=fmap missing (D.workCalls work)})
  Nothing -> case find ((==turnId turn) . turnId) runtime of
   Nothing -> do
    require (not (any ((==identity) . (`at` "id")) canonical)) 500 "Managed occurrence is missing its Workload"
    D.admitRoutineOn db owner turn snapshot_ accepted
   Just old -> do
    require (not (has (toJSON old) "managed")) 500 "Managed occurrence is missing its Workload"
    let restored=D.setWorkField "bindingSchema" Null (D.routineWorkload owner old snapshot_ Nothing)
        previous=NE.head (D.workCalls restored)
        node=D.callNode previous
        state=case turnStatus old of
         Queued -> D.QueuedAt node
         Running -> D.InterruptedAt node
         Interrupted -> D.InterruptedAt node
         Failed -> D.FailedAt node
         Cancelled -> D.CancelledAt node
         _ -> D.UnresolvedAt node Nothing
        diagnostic=if turnStatus old `notElem` [Queued,Running,Interrupted,Failed,Cancelled] then str "Legacy turn has no accepted task outcome; saved turn evidence is retained" else turnField old "error"
        call=(D.setCallFields [("error",diagnostic)] (NE.head (D.workCalls restored))) {D.callProgress=state}
        recovered=D.setWorkField "legacyTurnEvidence" (toJSON old) restored {D.workCalls=call :| []}
    DB.putValueOn db DB.Workloads identity (toJSON recovered)

admitGroupOn :: SQL.Connection -> Definition -> Value -> Turn -> IO ()
admitGroupOn db definition group turn = do
 let identity=runIdText (turnId turn)
     owner=definitionOwner definition
     time=turnField turn "created"
     body=definitionGoal definition
     origin=remove ["occurrence"] (turnField turn "origin")
 when (isNothing (findId identity (items group "requests"))) $ do
  let taskId="gtask_"<>identity
      groupOrigin=merge origin (obj [("group",str owner),("author",str "admin")])
      message=obj [("id",str identity),("author",str "admin"),("text",str body),("created",time),("routine",str (definitionId definition))]
      task=obj [("id",str taskId),("title",str (definitionTitle definition)),("body",str body),("assignee",get group "lead"),("state",str "in_progress"),("deleted",Bool False),("revision",num 1),("created",time),("author",str "admin"),("updated_by",str "admin"),("history",arr []),("results",arr []),("taskType",str "automated"),("routine",str (definitionId definition))]
      request=obj [("id",str identity),("message",str identity),("target",get group "lead"),("root",str identity),("created",time),("status",str "queued"),("settled",Bool False),("task",str taskId),("task_revision",num 1),("origin",groupOrigin)]
  DB.putValueOn db DB.Groups owner (merge group (obj [("messages",arr (items group "messages"++[message])),("tasks",arr (items group "tasks"++[task])),("requests",arr (items group "requests"++[request])),("revision",num (int (get group "revision")+1)),("updated",time)]))
