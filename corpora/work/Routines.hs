{-# LANGUAGE OverloadedStrings #-}
module Routines
 ( Minutes, Routine(..), routineSummaries, storeRoutine, initializeRoutines
 , MissedDuePolicy(..), RoutineEdit(..), save, setPolicy, setPaused, command, receipt, acknowledge, dispatch, restoreOccurrences
 , verifyPolicyPreparation
 ) where

import Control.Monad
import Control.Exception (SomeException, SomeAsyncException, fromException, throwIO, try, catch)
import Control.Concurrent.Async (AsyncCancelled)
import qualified Command as C
import Data.Aeson hiding (pairs)
import Data.Aeson.Types (Parser)
import qualified Data.Aeson.KeyMap as KM
import Data.List (find, nub, sortOn)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
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
import qualified GroupModel as G
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

data MissedDuePolicy = RecoverEach | RunLatest | SkipMissed deriving (Eq, Show)
instance FromJSON MissedDuePolicy where
 parseJSON = withText "Missed due policy" $ \value -> case value of
  "recover_each" -> pure RecoverEach
  "run_latest" -> pure RunLatest
  "skip_missed" -> pure SkipMissed
  _ -> fail "Choose recover_each, run_latest or skip_missed"
instance ToJSON MissedDuePolicy where
 toJSON policy = str $ case policy of
  RecoverEach -> "recover_each"
  RunLatest -> "run_latest"
  SkipMissed -> "skip_missed"

data PolicySelection = AwaitingSelection | PolicySelected MissedDuePolicy deriving (Eq, Show)
instance FromJSON PolicySelection where
 parseJSON = withObject "Routine policy selection" $ \o -> do
  kind <- o .: "kind" :: Parser Text
  case kind of
   "awaiting_selection" | KM.size o == 1 -> pure AwaitingSelection
   "selected" | KM.size o == 2 -> PolicySelected <$> o .: "policy"
   _ -> fail "Invalid routine policy selection"
instance ToJSON PolicySelection where
 toJSON selection = case selection of
  AwaitingSelection -> obj [("kind",str "awaiting_selection")]
  PolicySelected policy -> obj [("kind",str "selected"),("policy",toJSON policy)]

data Routine = Routine
 { routineId :: Text, routineOwner :: Text, routinePrompt :: Text, routineMinutes :: Minutes
 , routineDue :: UTCTime, routineCount :: Int, routinePaused :: Bool
 , routinePolicy :: PolicySelection, routineFields :: Object
 } deriving (Eq, Show)
instance FromJSON Routine where
 parseJSON = withObject "Scheduled Routine" $ \o -> do
  due <- o .: "nextDue" >>= maybe (fail "Invalid routine due time") pure . parseTime
  count <- o .:? "runCount" .!= 0
  unless (count >= 0) (fail "Invalid routine run count")
  Routine <$> o .: "id" <*> o .: "owner" <*> o .: "prompt" <*> o .: "minutes" <*> pure due <*> pure count <*> o .:? "paused" .!= False <*> o .: "policySelection" <*> pure (foldr KM.delete o ["id","owner","prompt","minutes","runCount","paused","policySelection"])
-- The persisted due-time spelling participates in deterministic occurrence IDs.
-- Retain it until the deadline changes, including legacy fractional precision.
instance ToJSON Routine where
 toJSON r = mergeFields [("id",str (routineId r)),("owner",str (routineOwner r)),("prompt",str (routinePrompt r)),("minutes",toJSON (routineMinutes r)),("nextDue",if parseTime (at (Object (routineFields r)) "nextDue") == Just (routineDue r) then get (Object (routineFields r)) "nextDue" else str (timestamp (routineDue r))),("runCount",num (routineCount r)),("paused",Bool (routinePaused r)),("policySelection",toJSON (routinePolicy r))] (Object (routineFields r))

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

newtype DueId = DueId { dueIdText :: Text } deriving (Eq, Ord, Show)
instance FromJSON DueId where parseJSON value = DueId <$> parseJSON value
instance ToJSON DueId where toJSON = str . dueIdText

data DeferredReason = BlockedBy RunId | AwaitingPolicy | AwaitingRecovery deriving (Eq, Show)
instance FromJSON DeferredReason where
 parseJSON = withObject "Deferred reason" $ \o -> do
  kind <- o .: "kind" :: Parser Text
  case kind of
   "active_run" -> BlockedBy <$> o .: "run"
   "awaiting_policy" -> pure AwaitingPolicy
   "awaiting_recovery" -> pure AwaitingRecovery
   _ -> fail "Unknown deferred reason"
instance ToJSON DeferredReason where
 toJSON reason = case reason of
  BlockedBy run -> obj [("kind",str "active_run"),("run",toJSON run)]
  AwaitingPolicy -> obj [("kind",str "awaiting_policy")]
  AwaitingRecovery -> obj [("kind",str "awaiting_recovery")]

-- A deadline exists independently of whether it waited, ran or was skipped.
data Due = Due
 { dueId :: DueId, dueRoutine :: Routine, dueTarget :: ScheduledTarget
 , dueObserved :: UTCTime
 } deriving (Eq, Show)
instance FromJSON Due where
 parseJSON = withObject "Routine due" $ \o -> do
  observed <- o .: "created" >>= maybe (fail "Invalid due observation time") pure . parseTime
  due <- Due <$> o .: "id" <*> o .: "routine" <*> o .: "target" <*> pure observed
  unless (dueId due == dueIdFor (dueRoutine due) && validDueTarget due)
   (fail "Invalid due identity or target")
  pure due
instance ToJSON Due where
 toJSON due = obj
  [("id",toJSON (dueId due)),("routine",toJSON (dueRoutine due)),("target",toJSON (dueTarget due))
  ,("created",str (timestamp (dueObserved due)))]

-- A contiguous grid of deadlines with one immutable instruction/interval snapshot.
-- The constructor stays private; persisted ranges must be ordered and aligned.
data DueSpan = DueSpan { spanFirst :: Due, spanThrough :: UTCTime } deriving (Eq, Show)
instance FromJSON DueSpan where
 parseJSON value = withObject "Routine due range" (\o -> do
  first <- parseJSON value
  through <- o .: "through" >>= maybe (fail "Invalid due range end") pure . parseTime
  let distance=diffUTCTime through (routineDue (dueRoutine first))
      interval=intervalSeconds (routineMinutes (dueRoutine first))
      steps=floor (distance / interval) :: Integer
  unless (distance >= 0 && through <= dueObserved first && fromInteger steps * interval == distance)
   (fail "Due range must end on its observed deadline grid")
  pure (DueSpan first through)) value
instance ToJSON DueSpan where
 toJSON range = merge (toJSON (spanFirst range)) (obj [("through",str (timestamp (spanThrough range)))])

spanCount :: DueSpan -> Integer
spanCount range = 1 + floor (diffUTCTime (spanThrough range) (routineDue routine) / intervalSeconds (routineMinutes routine))
 where routine=dueRoutine (spanFirst range)

spanDueAt :: DueSpan -> UTCTime -> Due
spanDueAt range time
 | time == routineDue original = first
 | otherwise = first { dueId=dueIdFor updated, dueRoutine=updated }
 where
 first=spanFirst range
 original=dueRoutine first
 updated=original { routineDue=time }

spanLast :: DueSpan -> Due
spanLast range = spanDueAt range (spanThrough range)

-- Splitting retains the captured observation, target and instruction snapshot.
spanTail :: DueSpan -> Maybe DueSpan
spanTail range
 | spanCount range == 1 = Nothing
 | otherwise = Just (range { spanFirst=spanDueAt range (routineDue (advanceDeadline (dueRoutine (spanFirst range)))) })

spanPrefix :: DueSpan -> Maybe DueSpan
spanPrefix range
 | spanCount range == 1 = Nothing
 | otherwise = Just (range { spanThrough=addUTCTime (negate (intervalSeconds (routineMinutes (dueRoutine (spanFirst range))))) (spanThrough range) })

data ClosureReason = SkippedByPolicy | SupersededBy RunId deriving (Eq, Show)
instance FromJSON ClosureReason where
 parseJSON = withObject "Due closure" $ \o -> do
  kind <- o .: "kind" :: Parser Text
  case kind of
   "skipped" | KM.size o == 1 -> pure SkippedByPolicy
   "superseded" | KM.size o == 2 -> SupersededBy <$> o .: "run"
   _ -> fail "Invalid due closure"
instance ToJSON ClosureReason where
 toJSON reason = case reason of
  SkippedByPolicy -> obj [("kind",str "skipped")]
  SupersededBy run -> obj [("kind",str "superseded"),("run",toJSON run)]

data Materialization = Pending | Ready deriving (Eq, Show)
data StoredDue
 = Deferred DueSpan DeferredReason
 | Admitted ScheduledOccurrence Materialization
 | Closed DueSpan ClosureReason
 deriving (Eq, Show)

-- At most one admission, compact closures, and unconsumed ranges.
-- Unmentioned spans remain queued. All replacements commit together.
data DuePlan = KeepDues | ResolveDues (Maybe Due) [(DueSpan,ClosureReason)] [(DueSpan,DeferredReason)] deriving (Eq, Show)
resolveDues :: UTCTime -> PolicySelection -> Maybe RunId -> [(DueSpan,DeferredReason)] -> DuePlan
resolveDues time selection active pending
 | isJust active = KeepDues
 | otherwise = case selection of
    AwaitingSelection -> KeepDues
    PolicySelected policy -> case policy of
     RecoverEach -> case ordered of
      [] -> KeepDues
      (range,reason):_ -> ResolveDues (Just (spanFirst range)) [] [(remainder,reason) | remainder <- maybeToList (spanTail range)]
     RunLatest -> case latest of
      [] -> KeepDues
      (range,_):rest -> selectLast range rest (SupersededBy (occurrenceIdFor (dueRoutine (spanLast range))))
     SkipMissed ->
      let eligible=sortOn (spanThrough . fst) (filter current pending)
          admitted=spanLast . fst <$> listToMaybe eligible
          closed=[(range,SkippedByPolicy) | entry@(range,_) <- ordered, not (current entry)]
           ++ [(prefix,SkippedByPolicy) | (range,_) <- eligible, prefix <- maybeToList (spanPrefix range)]
          -- Other current slots remain queued. If a range's stale prefix was
          -- closed above, retain its current singleton under that deadline's ID.
          remaining=[(DueSpan (spanLast range) (spanThrough range),reason)
           | (range,reason) <- drop 1 eligible, spanCount range > 1]
      in ResolveDues admitted closed remaining
 where
 ordered = sortOn (routineDue . dueRoutine . spanFirst . fst) pending
 latest = reverse (sortOn (spanThrough . fst) pending)
 selectLast range rest reason = ResolveDues (Just (spanLast range))
  ([(other,reason) | (other,_) <- rest] ++ [(prefix,reason) | prefix <- maybeToList (spanPrefix range)]) []
 current (range,reason) = case reason of
  BlockedBy _ -> False
  _ -> addUTCTime (intervalSeconds (routineMinutes (dueRoutine (spanFirst range)))) (spanThrough range) > time

validDueTarget :: Due -> Bool
validDueTarget due = case dueTarget due of
 PersonalTarget agent -> agentIdText agent == routineOwner (dueRoutine due)
 GroupTarget group _ -> groupIdText group == routineOwner (dueRoutine due)

validTarget :: ScheduledOccurrence -> Bool
validTarget occurrence = case occurrenceTarget occurrence of
 PersonalTarget agent -> agentIdText agent == routineOwner (occurrenceRoutine occurrence)
 GroupTarget group _ -> groupIdText group == routineOwner (occurrenceRoutine occurrence)
routineSummaries :: Service -> IO [Value]
routineSummaries s = do
 routines <- DB.values s DB.ScheduledRoutines
 rows <- DB.queryRows s "SELECT id,value,status FROM routine_occurrences WHERE status='deferred' ORDER BY rowid" ["id","value","status"] []
 dues <- catMaybes <$> traverse readSummaryDue rows
 let deferred=sortOn (routineDue . dueRoutine . spanFirst) [range | Deferred range _ <- dues]
     pending routine = filter ((==at routine "id") . routineId . dueRoutine . spanFirst) deferred
     summarize routine = case pending routine of
      [] -> routine
      due:rest -> merge routine (obj
       [("deferredCount",toJSON (sum (map spanCount (due:rest))))
       ,("oldestDeferredDue",get (toJSON (dueRoutine (spanFirst due))) "nextDue")])
 pure (map summarize routines)
 where
 readSummaryDue row = (Just <$> readStoredDue row) `catch` invalidSummary row
 invalidSummary :: Value -> APIError -> IO (Maybe StoredDue)
 invalidSummary row problem = do
  hPutStrLn stderr ("Deferred due " <> T.unpack (at row "id")
   <> " omitted from routine summaries: " <> show problem)
  pure Nothing
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
  routine <- decodeValue 500 r
  unless (routinePaused routine) (ensurePolicyPreparation s routine)
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
routineValue :: UTCTime -> Text -> Minutes -> Text -> Maybe MissedDuePolicy -> RoutineWrite -> Value
routineValue time owner interval body policy change = merge initial (obj
 [("owner",str owner),("title",str (T.unwords (take 7 (T.words body)))),("minutes",toJSON interval)
 ,("prompt",str body),("updated",str (timestamp time)),("nextDue",str (timestamp (addUTCTime (intervalSeconds interval) time)))
 ,("policySelection",maybe (get initial "policySelection") (toJSON . PolicySelected) policy)])
 where
 initial = case change of
  ReviseRoutine existing -> existing
  NewRoutine source -> obj ([("created",str (timestamp time)),("runCount",num 0),("paused",Bool False),("policySelection",toJSON AwaitingSelection)] ++ [("sourceCall",str call) | call <- maybeToList source])

save :: Service -> Text -> Value -> Value -> Maybe MissedDuePolicy -> RoutineEdit -> IO Value
save s owner minutes execution policy edit = do
 (body,interval) <- validated (routineInput minutes execution)
 if "group_" `T.isPrefixOf` owner then do
  groups <- DB.values s DB.Groups
  g <- maybe (failure 404 "Unknown Group") pure (findId owner groups)
  require (not (boolean (get g "archived"))) 409 "Restore this Group before changing its work"
 else requireActive s owner
 routines <- filter ((==owner) . (`at` "owner")) <$> DB.values s DB.ScheduledRoutines
 selected <- validated (selectRoutine edit minutes body routines)
 case selected of
  RoutineReplay existing -> do
   forM_ policy $ \chosen -> require (get existing "policySelection" == toJSON (PolicySelected chosen))
    409 "This creation already has a different policy; update the existing routine"
   pure existing
  RoutineChange change -> do
   destination <- case change of
    NewRoutine _ -> pure Nothing
    ReviseRoutine existing -> decodeValue 500 existing >>= scheduledTarget s
   time <- getCurrentTime
   result <- DB.connection s $ \db -> SQL.withTransaction db $ do
    captured <- case change of
     NewRoutine _ -> pure change
     ReviseRoutine existing -> do
      previous <- decodeValue 500 existing
      sequenceId <- routineSequence previous
      latest <- readRoutineOn db sequenceId
      -- Preserve the elapsed old grid before Edit replaces nextDue. Deliberate
      -- paused time is excluded, just as it is when the schedule resumes.
      unless (routinePaused latest || routineDue latest > time) $ do
       target <- maybe (failure 409 "Routine target is unavailable before editing elapsed deadlines") pure destination
       validateTargetOn db target
       let (elapsed,_)=captureDues time latest target
       stored <- readDuesOn db latest
       active <- activeRun s db latest target stored
       let reason=deferralReason (routinePolicy latest) active
       mapM_ (insertDeferredOn db reason) elapsed
       recordDeferralOn s db latest reason elapsed
      pure (ReviseRoutine (toJSON latest))
    saved <- DB.saveRoutineOn db (routineValue time owner interval body policy captured)
    let action = case change of NewRoutine _ -> "routine.created"; ReviseRoutine _ -> "routine.updated"
    DB.recordOn s db [owner] (at saved "id") action saved "admin" Nothing
    pure saved
   prepared <- decodeValue 500 result
   tryRoutine ("routine policy " <> routineId prepared) (ensurePolicyPreparation s prepared)
   pure result

-- Policy-only edits must not reset nextDue or overwrite a newer execution prompt.
setPolicy :: Service -> Text -> Text -> Text -> PolicySelection -> MissedDuePolicy -> IO Value
setPolicy s actor identity expectedPrompt expectedSelection policy = do
 sequenceId <- parseRoutineSequence identity
 routine <- DB.connection s $ \db -> readRoutineOn db sequenceId
 if "group_" `T.isPrefixOf` routineOwner routine then do
  group <- G.getGroup s (routineOwner routine) True
  G.authorizeGroup s group actor Management
 else require (actor == routineOwner routine) 403 "Only the owner can select this routine's policy"
 DB.connection s $ \db -> SQL.withTransaction db $ do
  latest <- readRoutineOn db sequenceId
  require (routinePrompt latest == expectedPrompt) 409 "Routine instructions changed; read them before selecting a policy"
  let updated=latest { routinePolicy=PolicySelected policy }
  require (routinePolicy latest == expectedSelection || routinePolicy latest == routinePolicy updated)
   409 "Routine policy changed; read it before selecting another policy"
  when (routinePolicy latest /= routinePolicy updated) $ do
   void (DB.saveRoutineOn db (toJSON updated))
   DB.recordOn s db [routineOwner updated] identity "routine.policy_selected"
    (obj [("policy",toJSON policy)]) actor Nothing
  pure (toJSON updated)
setPaused :: Service -> Text -> Text -> Bool -> IO Value
setPaused s owner identity paused = do
 sequenceId <- parseRoutineSequence identity
 routine <- DB.connection s $ \db -> readRoutineOn db sequenceId
 require (routineOwner routine == owner) 404 "Unknown Scheduled Routine in this conversation"
 target <- if paused then scheduledTarget s routine else pure Nothing
 time <- getCurrentTime
 DB.connection s $ \db -> SQL.withTransaction db $ do
  latest <- readRoutineOn db sequenceId
  if routinePaused latest == paused then pure (toJSON latest) else do
   -- Capture deadlines already elapsed before Pause. Resume excludes only the
   -- intentionally paused period; it must not erase an unobserved earlier due.
   captured <- case target of
    Nothing -> pure latest
    Just destination -> do
     let (elapsed,advanced)=captureDues time latest destination
     unless (null elapsed) $ do
      stored <- readDuesOn db latest
      active <- activeRun s db latest destination stored
      let reason=maybe AwaitingRecovery BlockedBy active
      mapM_ (insertDeferredOn db reason) elapsed
      recordDeferralOn s db latest reason elapsed
     pure advanced
   let updated=pauseRoutine time paused captured
   result <- DB.saveRoutineOn db (toJSON updated)
   DB.recordOn s db [owner] identity (if paused then "routine.paused" else "routine.resumed") result "admin" Nothing
   pure result
command :: Service -> Text -> Text -> Bool -> IO (Maybe Value)
command s owner input attached = case C.interpret attached (T.unpack input) of
 Left error -> failure 400 (T.pack error)
 Right Nothing -> pure Nothing
 Right (Just c) -> Just <$> case c of
  C.Create interval body -> save s owner (toJSON interval) (str (T.pack body)) Nothing (AddRoutine Nothing)
  C.Edit i old interval body -> save s owner (toJSON interval) (str (T.pack body)) Nothing (EditRoutine (T.pack i) (Just (fromIntegral old)))
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
advanceDeadline :: Routine -> Routine
advanceDeadline routine = routine
 { routineDue=addUTCTime (intervalSeconds (routineMinutes routine)) (routineDue routine) }

-- Capture an arbitrarily long downtime without enumerating its deadlines.
captureDues :: UTCTime -> Routine -> ScheduledTarget -> ([DueSpan],Routine)
captureDues time routine target
 | routineDue routine > time = ([],routine)
 | otherwise = ([DueSpan first through],routine { routineDue=addUTCTime interval through })
 where
 interval=intervalSeconds (routineMinutes routine)
 steps=floor (diffUTCTime time (routineDue routine) / interval) :: Integer
 through=addUTCTime (fromInteger steps * interval) (routineDue routine)
 first=Due (dueIdFor routine) routine target time

deferralReason :: PolicySelection -> Maybe RunId -> DeferredReason
deferralReason selection active = case active of
 Just run -> BlockedBy run
 Nothing -> case selection of
  AwaitingSelection -> AwaitingPolicy
  PolicySelected _ -> AwaitingRecovery

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
dispatchRoutine s time routine
 | routinePaused routine = pure ()
 | routineCount routine >= 1000 = do
    void (storeRoutine s routine { routinePaused=True })
    DB.record s [routineOwner routine] (routineId routine) "routine.paused"
     (obj [("reason",str "run limit"),("runCount",num (routineCount routine))]) "system" Nothing
 | otherwise = do
    ensurePolicyPreparation s routine
    target <- scheduledTarget s routine
    forM_ target $ \destination -> admitDue s time routine destination >>= mapM_ (materializeOccurrence s)

scheduledTarget :: Service -> Routine -> IO (Maybe ScheduledTarget)
scheduledTarget s routine = do
 let owner=routineOwner routine
 rows <- DB.queryRows s "SELECT value FROM groups WHERE id=?" ["value"] [SQL.SQLText owner]
 group <- traverse (DB.decodeStored . (`at` "value")) (listToMaybe rows)
 when ("group_" `T.isPrefixOf` owner && isNothing group) $ failure 409 "Routine Group no longer exists"
 let agent=AgentId (maybe owner (`at` "lead") group)
 inactive <- retired s (agentIdText agent)
 pure $ if inactive || maybe False (boolean . (`get` "archived")) group then Nothing
  else Just (maybe (PersonalTarget agent) (\_ -> GroupTarget (GroupId owner) agent) group)

routineSequence :: Routine -> IO Int64
routineSequence = parseRoutineSequence . routineId

parseRoutineSequence :: Text -> IO Int64
parseRoutineSequence identity = maybe (failure 400 "Invalid scheduled routine ID") pure
 (T.stripPrefix "routine_sch_" identity >>= readMaybe . T.unpack)

stableRunId :: Text -> RunId
stableRunId key = RunId (T.filter (/='-') (UUID.toText
 (UUID5.generateNamed UUID5.namespaceURL (BS.unpack (TE.encodeUtf8 key)))))

occurrenceIdFor :: Routine -> RunId
occurrenceIdFor routine = stableRunId (routineId routine <> ":" <> at (toJSON routine) "nextDue")

dueIdFor :: Routine -> DueId
dueIdFor = DueId . runIdText . occurrenceIdFor

readStoredDue :: Value -> IO StoredDue
readStoredDue row = do
 value <- DB.decodeStored (at row "value")
 stored <- case at row "status" of
  "deferred" -> do
   require (not (has value "closure")) 500 "Deferred due already contains a closure"
   Deferred <$> decodeValue 500 value <*> decodeValue 500 (get value "reason")
  "admitted_pending" -> admitted Pending value
  "admitted_ready" -> admitted Ready value
  "closed" -> do
   require (not (has value "reason")) 500 "Closed due still contains a deferral reason"
   Closed <$> decodeValue 500 value <*> decodeValue 500 (get value "closure")
  _ -> failure 500 "Invalid stored due status"
 let storedId = case stored of
      Deferred range _ -> toJSON (dueId (spanFirst range))
      Admitted occurrence _ -> toJSON (occurrenceId occurrence)
      Closed range _ -> toJSON (dueId (spanFirst range))
 require (storedId == get row "id") 500 "Stored due ID mismatch"
 pure stored
 where
 admitted materialization value = do
  require (not (has value "reason") && not (has value "closure") && not (has value "through")) 500 "Due cannot be admitted without resolution"
  Admitted <$> decodeValue 500 value <*> pure materialization

readDuesOn :: SQL.Connection -> Routine -> IO [StoredDue]
readDuesOn db routine = do
 rows <- DB.queryRowsOn db
  "SELECT id,value,status FROM routine_occurrences WHERE status<>'closed' AND CASE WHEN json_valid(value) THEN json_extract(value,'$.routine.id') ELSE NULL END =?"
  ["id","value","status"] [SQL.SQLText (routineId routine)]
 traverse readStoredDue rows

activePersonal :: Service -> SQL.Connection -> Routine -> [StoredDue] -> IO (Maybe RunId)
activePersonal s db routine dues = do
 state <- agentState s (routineOwner routine)
 workRows <- DB.queryRowsOn db "SELECT id,value FROM workloads" ["id","value"] []
 turnRows <- DB.queryRowsOn db "SELECT id,status FROM turns" ["id","status"] []
 let matching turn = get (turnField turn "origin") "routine" == str (routineId routine)
     turns = filter matching (agentTurns state)
     admitted = [occurrenceId occurrence | Admitted occurrence _ <- dues]
     identities = nub (admitted ++ map turnId turns)
     works = M.fromList [(at row "id",row) | row<-workRows]
     projected = M.fromList [(at row "id",row) | row<-turnRows]
 active <- filterM (activeIdentity turns works projected) identities
 pure (case active of i:_ -> Just i; [] -> Nothing)
 where
 activeIdentity turns works projected identity = do
  let inState = find ((==identity) . turnId) turns
  work <- traverse (DB.decodeStored . (`at` "value")) (M.lookup (runIdText identity) works) :: IO (Maybe D.Workload)
  status <- traverse (\row -> decodeValue 500 (get row "status")) (M.lookup (runIdText identity) projected) :: IO (Maybe TurnStatus)
  let liveCall = case work of
       Just w -> D.callState (NE.last (D.workCalls w)) `elem`
        [D.CallQueued,D.CallRunning,D.CallDelegated,D.CallWaiting]
       Nothing -> False
      liveTurn = maybe False (activeStatus . turnStatus) inState || maybe False activeStatus status
  when (isNothing inState && isNothing status && isNothing work) $
   failure 500 "Admitted routine Run has no persisted state"
  pure (liveCall || liveTurn)

activeGroup :: SQL.Connection -> Routine -> IO (Maybe RunId)
activeGroup db routine = do
 rows <- DB.queryRowsOn db "SELECT value FROM groups WHERE id=?" ["value"] [SQL.SQLText (routineOwner routine)]
 group <- case rows of
  row:_ -> DB.decodeStored (at row "value")
  [] -> failure 409 "Routine Group disappeared before admission"
 requests <- G.readRequests group
 let belongs request = get (get (Object (G.requestFields request)) "origin") "routine" == str (routineId routine)
     pending request = not (G.requestSettled request)
     taskPending request = case findId (at (toJSON request) "task") (items group "tasks") of
      Just task -> at task "state" == "in_progress"
      Nothing -> False
 pure (G.requestId <$> find (\request -> belongs request && (pending request || taskPending request)) requests)

activeRun :: Service -> SQL.Connection -> Routine -> ScheduledTarget -> [StoredDue] -> IO (Maybe RunId)
activeRun s db routine target dues = case [occurrenceId occurrence | Admitted occurrence Pending <- dues] of
 i:_ -> pure (Just i)
 [] -> case target of
  PersonalTarget _ -> activePersonal s db routine dues
  GroupTarget _ _ -> activeGroup db routine

occurrenceTurn :: ScheduledOccurrence -> Turn
occurrenceTurn occurrence = newTurn (occurrenceId occurrence) Chat
 (routinePrompt routine) (timestamp (occurrenceCreated occurrence)) origin True
 where
 routine=occurrenceRoutine occurrence
 value=toJSON routine
 origin=obj [("routine",str (routineId routine)),("routineOwner",str (routineOwner routine))
  ,("due",get value "nextDue"),("title",get value "title"),("minutes",toJSON (routineMinutes routine))
  ,("taskType",str "automated"),("executionPrompt",str (routinePrompt routine))]

-- Admission fixes the current instructions; the original deadline owns identity
-- and interval. Never replace its nextDue with the edited schedule's nextDue.
prepareOccurrence :: UTCTime -> Routine -> ScheduledTarget -> Due -> Validation ScheduledOccurrence
prepareOccurrence time current target due = do
 let original=dueRoutine due
 check (routineId current == routineId original && routineOwner current == routineOwner original)
  409 "Routine identity or owner changed before admission"
 let fields=case KM.lookup "title" (routineFields current) of
      Nothing -> KM.delete "title" (routineFields original)
      Just title -> KM.insert "title" title (routineFields original)
     snapshot=original { routinePrompt=routinePrompt current, routinePolicy=routinePolicy current, routineFields=fields }
     occurrence=ScheduledOccurrence (occurrenceIdFor snapshot) snapshot target time
 check (dueId due == dueIdFor snapshot && validTarget occurrence) 409 "Due identity or target changed before admission"
 pure occurrence

readRoutineOn :: SQL.Connection -> Int64 -> IO Routine
readRoutineOn db sequenceId = do
 rows <- DB.queryRowsOn db "SELECT value FROM scheduled_routines WHERE sequence=?" ["value"] [SQL.SQLInteger sequenceId]
 case rows of
  row:_ -> DB.decodeStored (at row "value")
  [] -> failure 409 "Routine disappeared before admission"

admitDue :: Service -> UTCTime -> Routine -> ScheduledTarget -> IO (Maybe ScheduledOccurrence)
admitDue s time candidate target = do
 sequenceId <- routineSequence candidate
 DB.connection s $ \db -> SQL.withTransaction db $ do
  routine <- readRoutineOn db sequenceId
  if routinePaused routine || routineCount routine >= 1000 then pure Nothing else do
   validateTargetOn db target
   stored <- readDuesOn db routine
   let (newDues,advanced)=captureDues time routine target
       waiting=[() | Deferred _ _ <- stored]
   if null newDues && null waiting then pure Nothing else do
    active <- activeRun s db routine target stored
    let waitingReason=deferralReason (routinePolicy routine) active
        pending=[(due,reason) | Deferred due reason <- stored] ++ [(due,waitingReason) | due <- newDues]
        plan=resolveDues time (routinePolicy routine) active pending
    mapM_ (insertDeferredOn db waitingReason) newDues
    result <- case plan of
     KeepDues -> pure Nothing
     ResolveDues admitted closed remaining -> do
      forM_ closed $ \(due,reason) -> do
       DB.executeOn db "UPDATE routine_occurrences SET value=?,status='closed' WHERE id=? AND status='deferred'"
        [SQL.SQLText (jsonText (merge (toJSON due) (obj [("closure",toJSON reason)]))),SQL.SQLText (dueIdText (dueId (spanFirst due)))]
       changed <- SQL.changes db
       require (changed == 1) 409 "Deferred range changed before closure"
      -- Each durable row retains its own closure. Summarize the batch in the
      -- bounded activity feed instead of replacing that feed thousands of times.
      case closed of
       [] -> pure ()
       (first,reason):rest -> do
        let spans=first:map fst rest
            firstTime=foldr (min . routineDue . dueRoutine . spanFirst . fst) (routineDue (dueRoutine (spanFirst first))) rest
            lastTime=foldr (max . spanThrough . fst) (spanThrough first) rest
        DB.recordOn s db [routineOwner routine] (routineId routine) "routine.dues_closed"
         (obj [("count",toJSON (sum (map spanCount spans))),("reason",toJSON reason)
         ,("earliest",str (timestamp firstTime))
         ,("latest",str (timestamp lastTime))]) "system" Nothing
      forM_ remaining $ \(range,reason) -> insertDeferredOn db reason range
      forM admitted $ \due -> do
       occurrence <- validated (prepareOccurrence time routine target due)
       let snapshot=occurrenceRoutine occurrence
       DB.executeOn db "INSERT INTO routine_occurrences(id,value,status) VALUES (?,?,'admitted_pending') ON CONFLICT(id) DO UPDATE SET value=excluded.value,status=excluded.status WHERE routine_occurrences.status='deferred'"
        [SQL.SQLText (runIdText (occurrenceId occurrence)),SQL.SQLText (jsonText (toJSON occurrence))]
       changed <- SQL.changes db
       require (changed == 1) 409 "Due already resolved before admission"
       DB.recordOn s db [routineOwner routine] (routineId routine) "routine.triggered"
        (obj [("turn",toJSON (occurrenceId occurrence)),("runCount",num (routineCount routine+1))
        ,("due",get (toJSON snapshot) "nextDue"),("nextDue",str (timestamp (routineDue advanced)))]) "system" Nothing
       pure occurrence
    let count=routineCount routine + if isJust result then 1 else 0
        updated=advanced { routineCount=count, routinePaused=count>=1000 }
        stillDeferred=case plan of
         KeepDues -> newDues
         ResolveDues admitted closed remaining ->
          let resolved=map (dueId . spanFirst . fst) closed ++ maybeToList (dueId <$> admitted)
              untouched=[range | range <- newDues, dueId (spanFirst range) `notElem` resolved]
              tails=[remainder | (remainder,_) <- remaining, captured <- newDues, spanThrough captured == spanThrough remainder, dueObserved (spanFirst captured) == dueObserved (spanFirst remainder)]
          in untouched++tails
    recordDeferralOn s db routine waitingReason stillDeferred
    when (updated /= routine) (void (DB.saveRoutineOn db (toJSON updated)))
    when (routinePaused updated) $ DB.recordOn s db [routineOwner routine] (routineId routine) "routine.paused"
     (obj [("reason",str "run limit"),("runCount",num count)]) "system" Nothing
    pure result

insertDeferredOn :: SQL.Connection -> DeferredReason -> DueSpan -> IO ()
insertDeferredOn db reason due =
 DB.executeOn db "INSERT INTO routine_occurrences(id,value,status) VALUES (?,?,'deferred')"
  [SQL.SQLText (dueIdText (dueId (spanFirst due))),SQL.SQLText (jsonText (merge (toJSON due) (obj [("reason",toJSON reason)])))]

recordDeferralOn :: Service -> SQL.Connection -> Routine -> DeferredReason -> [DueSpan] -> IO ()
recordDeferralOn s db routine reason dues = case dues of
 [] -> pure ()
 first:rest -> DB.recordOn s db [routineOwner routine] (routineId routine) "routine.deferred"
  (obj [("due",get (toJSON (dueRoutine (spanFirst first))) "nextDue"),("through",str (timestamp (foldr (max . spanThrough) (spanThrough first) rest))),("count",toJSON (sum (map spanCount (first:rest)))),("reason",toJSON reason)]) "system" Nothing

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

-- The pending Routine is the durable intent; the existing Task owns execution,
-- failure and retry. Stable identity repairs partial creation without new calls.
policyPreparationId :: Routine -> RunId
policyPreparationId routine = stableRunId ("routine-policy:" <> routineId routine <> ":" <> routinePrompt routine)

data PolicyPreparationError = PolicyNotSaved | PolicyInstructionsChanged | PolicyOwnerChanged

policyPreparationCompletion :: Text -> RunId -> Routine -> Either PolicyPreparationError ()
policyPreparationCompletion owner identity routine
 | routineOwner routine /= owner = Left PolicyOwnerChanged
 | PolicySelected _ <- routinePolicy routine = Right ()
 | identity /= policyPreparationId routine = Left PolicyInstructionsChanged
 | otherwise = Left PolicyNotSaved

-- Callers hold the host lock through completion. Read the authoritative Routine
-- after execution; a newer valid selection satisfies setup without another write.
verifyPolicyPreparation :: Service -> Text -> RunId -> Value -> IO ()
verifyPolicyPreparation s owner identity origin = when (has origin "policyRoutine") $ do
 sequenceId <- parseRoutineSequence (at origin "policyRoutine")
 routine <- DB.connection s $ \db -> readRoutineOn db sequenceId
 require (routineId routine == at origin "policyRoutine") 409 "Stored routine identity does not match policy setup"
 either reject pure (policyPreparationCompletion owner identity routine)
 where
 reject problem = failure 409 $ case problem of
  PolicyNotSaved -> "Routine policy was not saved. Retry this setup or ask the owner to finish routine setup in chat."
  PolicyInstructionsChanged -> "Routine instructions changed before policy setup completed. Ask the owner to finish the current routine setup in chat."
  PolicyOwnerChanged -> "Routine owner changed before policy setup completed. Ask the current owner to finish routine setup in chat."

ensurePolicyPreparation :: Service -> Routine -> IO ()
ensurePolicyPreparation s routine = case routinePolicy routine of
 PolicySelected _ -> pure ()
 AwaitingSelection -> do
  destination <- scheduledTarget s routine
  forM_ destination $ \target -> do
   template <- prompt s "routine-policy"
   let identity=policyPreparationId routine
       created=at (toJSON routine) "created"
       input=render template [("routine",jsonText (toJSON routine))]
       origin=obj [("policyRoutine",str (routineId routine)),("author",str "system")]
       turn=newTurn identity Chat input created origin False
   case target of
    PersonalTarget agent -> do
     state <- agentState s (agentIdText agent)
     archived <- DB.queryRows s "SELECT id FROM turns WHERE id=? UNION SELECT id FROM workloads WHERE id=?"
      ["id"] [SQL.SQLText (runIdText identity),SQL.SQLText (runIdText identity)]
     -- Conversation retention removes old Turns, not their durable history.
     when (any ((==identity) . turnId) (agentTurns state) || null archived) $
      materializePersonal s agent turn
    GroupTarget group agent -> DB.connection s $ \db -> SQL.withTransaction db $ do
     validateTargetOn db target
     rows <- DB.queryRowsOn db "SELECT value FROM groups WHERE id=?" ["value"] [SQL.SQLText (groupIdText group)]
     value <- case rows of
      row:_ -> DB.decodeStored (at row "value")
      [] -> failure 409 "Group disappeared during routine setup"
     requests <- G.readRequests value
     unless (any ((==identity) . G.requestId) requests) $ do
      let requestOrigin=merge origin (obj [("group",toJSON group)])
          entry=obj [("id",toJSON identity),("author",str "system"),("text",str input),("created",str created)]
          request=G.GroupRequest identity agent Queued False (KM.fromList
           [("message",toJSON identity),("root",toJSON identity),("created",str created),("origin",requestOrigin)])
      DB.putValueOn db DB.Groups (groupIdText group) (merge value (obj
       [("messages",arr (items value "messages"++[entry])),("requests",arr (items value "requests"++[toJSON request]))
       ,("revision",num (int (get value "revision")+1))]))

materializeOccurrence :: Service -> ScheduledOccurrence -> IO ()
materializeOccurrence s occurrence = do
 case occurrenceTarget occurrence of
  PersonalTarget agent -> materializePersonal s agent (occurrenceTurn occurrence)
  GroupTarget group agent -> materializeGroup s group agent (occurrenceTurn occurrence)
 DB.executeSQL s "UPDATE routine_occurrences SET status='admitted_ready' WHERE id=? AND status='admitted_pending'"
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
 rows <- DB.queryRows s "SELECT id,value FROM routine_occurrences WHERE status='admitted_pending' ORDER BY rowid" ["id","value"] []
 forM_ rows $ \row -> tryRoutine ("scheduled occurrence " <> at row "id") $ do
  value <- DB.decodeStored (at row "value")
  occurrence <- decodeValue 500 value
  require (toJSON (occurrenceId occurrence) == get row "id") 500 "Stored occurrence ID mismatch"
  materializeOccurrence s occurrence
