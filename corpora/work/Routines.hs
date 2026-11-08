{-# LANGUAGE OverloadedStrings #-}
module Routines
 ( Minutes, Routine, routineSummaries, storeRoutine, initializeRoutines
 , MissedDuePolicy(..), RoutineEdit(..), save, setPolicy, setPaused, setRoutinePaused, command, receipt, acknowledge, dispatch, restoreOccurrences
 , verifyPolicyPreparationCompletion, ensureRetryAvailable, runNow, stopRun, ensureExecutionAllowed, ensureTaskAvailable
 , runHistory, runDetail, deleteRoutine, ensureOriginAvailable, stopDeletedForwarding, rejectRunResult, readRunResult, confirmRepair
 ) where

import Control.Monad
import Control.Applicative ((<|>))
import Control.Exception (catch, SomeException, SomeAsyncException, fromException, throwIO, try, mask_)
import Control.Concurrent (readMVar)
import Data.IORef (writeIORef)
import Control.Concurrent.Async (AsyncCancelled)
import qualified Command as C
import Data.Aeson hiding (pairs)
import Data.Aeson.Types (Parser)
import qualified Data.Aeson.KeyMap as KM
import Data.List (find, nub, sortOn)
import Data.Containers.ListUtils (nubOrd)
import Data.Ord (Down(..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time hiding (Minutes)
import qualified Data.UUID as UUID
import qualified Data.UUID.V5 as UUID5
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString as BS
import qualified Database.SQLite.Simple as SQL
import qualified GroupModel as G
import qualified GroupWork as GW
import System.IO (hPutStrLn, stderr)
import Types
import qualified Store as DB
import Population
import qualified Delegation as D
import qualified Provider as P

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
 { routineId :: RoutineId, routineOwner :: RoutineOwner, routinePrompt :: Text, routineMinutes :: Minutes
 , routineDeadline :: Deadline, routineAllowanceUsed :: Int, routinePaused :: Bool
 , routinePolicy :: PolicySelection, routineSpecification :: Maybe D.TaskSpec, routineDeletedAt :: Maybe UTCTime, routineFields :: Object
 } deriving (Eq, Show)
instance FromJSON Routine where
 parseJSON = withObject "Scheduled Routine" $ \o -> do
  due <- o .: "nextDue"
  count <- o .:? "runCount" .!= 0
  unless (count >= 0) (fail "Invalid routine run count")
  specification <- if KM.member "specification" o then Just <$> o .: "specification" else pure Nothing
  deleted <- if KM.member "deletedAt" o then Just <$> (o .: "deletedAt" >>= maybe (fail "Invalid Routine deletion time") pure . parseTime) else pure Nothing
  Routine <$> o .: "id" <*> o .: "owner" <*> o .: "prompt" <*> o .: "minutes" <*> pure due <*> pure count <*> o .: "paused" <*> o .: "policySelection" <*> pure specification <*> pure deleted <*> pure (foldr KM.delete o ["id","owner","prompt","minutes","nextDue","runCount","paused","policySelection","specification","deletedAt"])
instance ToJSON Routine where
 toJSON r = mergeFields
  ([("id",toJSON (routineId r)),("owner",toJSON (routineOwner r)),("prompt",str (routinePrompt r)),("minutes",toJSON (routineMinutes r)),("nextDue",toJSON (routineDeadline r)),("runCount",num (routineAllowanceUsed r)),("paused",Bool (routinePaused r)),("policySelection",toJSON (routinePolicy r))]
   ++ [("specification",toJSON spec) | spec <- maybeToList (routineSpecification r)]
   ++ [("deletedAt",str (timestamp time)) | time <- maybeToList (routineDeletedAt r)]) (Object (routineFields r))

availableRoutine :: Routine -> Validation ()
availableRoutine routine = check (isNothing (routineDeletedAt routine)) 409 "This Routine was deleted; its history is preserved, but it cannot be changed or started again"

ensureOriginAvailable :: Service -> ExecutionOrigin -> IO ()
ensureOriginAvailable s origin = forM_ (originRoutine origin <|> originPolicyRoutine origin) $ \identity ->
 DB.connection s (\db -> readRoutineOn db identity) >>= validated . availableRoutine

routineDue :: Routine -> UTCTime
routineDue = deadlineTime . routineDeadline

data ScheduledTarget = PersonalTarget AgentId | GroupTarget GroupId AgentId deriving (Eq, Show)
instance FromJSON ScheduledTarget where
 parseJSON = withObject "Scheduled target" $ \o -> do
  kind <- o .: "kind" :: Parser Text
  case kind of
   "personal" -> PersonalTarget <$> (o .: "agent" >>= personalAgentP)
   "group" -> GroupTarget <$> o .: "group" <*> (o .: "agent" >>= personalAgentP)
   _ -> fail "Unknown scheduled target"
instance ToJSON ScheduledTarget where
 toJSON target = case target of
  PersonalTarget agent -> obj [("kind",str "personal"),("agent",toJSON agent)]
  GroupTarget group agent -> obj [("kind",str "group"),("group",toJSON group),("agent",toJSON agent)]

data RunTrigger = ScheduledTrigger | ManualTrigger Text deriving (Eq, Show)
instance FromJSON RunTrigger where
 parseJSON = withObject "Routine Run trigger" $ \o -> do
  kind <- o .: "kind" :: Parser Text
  case kind of
   "scheduled" | KM.size o == 1 -> pure ScheduledTrigger
   "manual" | KM.size o == 2 -> ManualTrigger <$> textFieldP "request" 128 (Object o)
   _ -> fail "Invalid routine Run trigger"
instance ToJSON RunTrigger where
 toJSON trigger = case trigger of
  ScheduledTrigger -> obj [("kind",str "scheduled")]
  ManualTrigger request -> obj [("kind",str "manual"),("request",str request)]

data ScheduledOccurrence = ScheduledOccurrence
 { occurrenceId :: RunId, occurrenceRoutineSnapshot :: Routine
 , occurrenceTarget :: ScheduledTarget, occurrenceAdmittedAt :: UTCTime
 , occurrenceTrigger :: RunTrigger
 , occurrenceStoppedAt :: Maybe UTCTime
 , occurrenceRepairs :: [AppliedRepair]
 } deriving (Eq, Show)
instance FromJSON ScheduledOccurrence where
 parseJSON = withObject "Scheduled occurrence" $ \o -> do
  created <- o .: "created" >>= maybe (fail "Invalid occurrence creation time") pure . parseTime
  trigger <- if KM.member "trigger" o then o .: "trigger" else pure ScheduledTrigger
  stopped <- if KM.member "stoppedAt" o then Just <$> (o .: "stoppedAt" >>= maybe (fail "Invalid Run stop time") pure . parseTime) else pure Nothing
  occurrence <- ScheduledOccurrence <$> o .: "id" <*> o .: "routine" <*> o .: "target" <*> pure created <*> pure trigger <*> pure stopped <*> o .:? "repairs" .!= []
  unless (occurrenceId occurrence == triggeredId (occurrenceRoutineSnapshot occurrence) trigger && validTarget occurrence)
   (fail "Invalid scheduled occurrence identity or target")
  unless (all ((==occurrenceId occurrence) . problemRun . repairProblem) (occurrenceRepairs occurrence)
    && length (nub (map repairKey (occurrenceRepairs occurrence))) == length (occurrenceRepairs occurrence))
   (fail "Invalid Routine repair history")
  pure occurrence
instance ToJSON ScheduledOccurrence where
 toJSON occurrence = obj $
  [("id",toJSON (occurrenceId occurrence)),("routine",toJSON (occurrenceRoutineSnapshot occurrence))
  ,("target",toJSON (occurrenceTarget occurrence)),("created",str (timestamp (occurrenceAdmittedAt occurrence)))]
  ++ [("repairs",toJSON (occurrenceRepairs occurrence)) | not (null (occurrenceRepairs occurrence))]
  ++ [("stoppedAt",str (timestamp stopped)) | stopped <- maybeToList (occurrenceStoppedAt occurrence)]
  ++ case occurrenceTrigger occurrence of
      ScheduledTrigger -> []
      trigger@(ManualTrigger _) -> [("trigger",toJSON trigger)]

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

data Due = Due
 { dueId :: DueId, dueRoutineSnapshot :: Routine, dueTarget :: ScheduledTarget
 , dueObservedAt :: UTCTime
 } deriving (Eq, Show)
instance FromJSON Due where
 parseJSON = withObject "Routine due" $ \o -> do
  observed <- o .: "created" >>= maybe (fail "Invalid due observation time") pure . parseTime
  due <- Due <$> o .: "id" <*> o .: "routine" <*> o .: "target" <*> pure observed
  unless (dueId due == dueIdFor (dueRoutineSnapshot due) && validDueTarget due)
   (fail "Invalid due identity or target")
  pure due
instance ToJSON Due where
 toJSON due = obj
  [("id",toJSON (dueId due)),("routine",toJSON (dueRoutineSnapshot due)),("target",toJSON (dueTarget due))
  ,("created",str (timestamp (dueObservedAt due)))]

data DueSpan = DueSpan { spanFirst :: Due, spanThrough :: UTCTime } deriving (Eq, Show)
instance FromJSON DueSpan where
 parseJSON value = withObject "Routine due range" (\o -> do
  first <- parseJSON value
  through <- o .: "through" >>= maybe (fail "Invalid due range end") pure . parseTime
  let distance=diffUTCTime through (routineDue (dueRoutineSnapshot first))
      interval=intervalSeconds (routineMinutes (dueRoutineSnapshot first))
      steps=floor (distance / interval) :: Integer
  unless (distance >= 0 && through <= dueObservedAt first && fromInteger steps * interval == distance)
   (fail "Due range must end on its observed deadline grid")
  pure (DueSpan first through)) value
instance ToJSON DueSpan where
 toJSON range = merge (toJSON (spanFirst range)) (obj [("through",str (timestamp (spanThrough range)))])

spanCount :: DueSpan -> Integer
spanCount range = 1 + floor (diffUTCTime (spanThrough range) (routineDue routine) / intervalSeconds (routineMinutes routine))
 where routine=dueRoutineSnapshot (spanFirst range)

spanDueAt :: DueSpan -> UTCTime -> Due
spanDueAt range time
 | time == routineDue original = first
 | otherwise = first { dueId=dueIdFor updated, dueRoutineSnapshot=updated }
 where
 first=spanFirst range
 original=dueRoutineSnapshot first
 updated=original { routineDeadline=newDeadline time }

spanLast :: DueSpan -> Due
spanLast range = spanDueAt range (spanThrough range)

spanTail :: DueSpan -> Maybe DueSpan
spanTail range
 | spanCount range == 1 = Nothing
 | otherwise = Just (range { spanFirst=spanDueAt range (routineDue (advanceDeadline (dueRoutineSnapshot (spanFirst range)))) })

spanPrefix :: DueSpan -> Maybe DueSpan
spanPrefix range
 | spanCount range == 1 = Nothing
 | otherwise = Just (range { spanThrough=addUTCTime (negate (intervalSeconds (routineMinutes (dueRoutineSnapshot (spanFirst range))))) (spanThrough range) })

data ClosureReason = SkippedByPolicy | SupersededBy RunId | RoutineDeleted deriving (Eq, Show)
instance FromJSON ClosureReason where
 parseJSON = withObject "Due closure" $ \o -> do
  kind <- o .: "kind" :: Parser Text
  case kind of
   "skipped" | KM.size o == 1 -> pure SkippedByPolicy
   "superseded" | KM.size o == 2 -> SupersededBy <$> o .: "run"
   "routine_deleted" | KM.size o == 1 -> pure RoutineDeleted
   _ -> fail "Invalid due closure"
instance ToJSON ClosureReason where
 toJSON reason = case reason of
  SkippedByPolicy -> obj [("kind",str "skipped")]
  SupersededBy run -> obj [("kind",str "superseded"),("run",toJSON run)]
  RoutineDeleted -> obj [("kind",str "routine_deleted")]

data MaterializationStatus = MaterializationPending | Materialized deriving (Eq, Show)
data StoredDue
 = Deferred DueSpan DeferredReason
 | Admitted ScheduledOccurrence MaterializationStatus
 | Closed DueSpan ClosureReason
 deriving (Eq, Show)

data DueResolution = DueResolution
 { dueToAdmit :: Maybe Due
 , rangesToClose :: [(DueSpan,ClosureReason)]
 , deferredRemainders :: [(DueSpan,DeferredReason)]
 } deriving (Eq, Show)
data DuePlan = KeepDues | ResolveDues DueResolution deriving (Eq, Show)
resolveDues :: UTCTime -> PolicySelection -> Maybe RunId -> [(DueSpan,DeferredReason)] -> DuePlan
resolveDues time selection active pending
 | isJust active = KeepDues
 | otherwise = case selection of
    AwaitingSelection -> KeepDues
    PolicySelected policy -> case policy of
     RecoverEach -> case ordered of
      [] -> KeepDues
      (range,reason):_ -> ResolveDues (DueResolution
       { dueToAdmit=Just (spanFirst range), rangesToClose=[]
       , deferredRemainders=[(remainder,reason) | remainder <- maybeToList (spanTail range)] })
     RunLatest -> case latest of
      [] -> KeepDues
      (range,_):rest -> selectLast range rest (SupersededBy (occurrenceIdFor (dueRoutineSnapshot (spanLast range))))
     SkipMissed ->
      let eligible=sortOn (spanThrough . fst) (filter current pending)
          admitted=spanLast . fst <$> listToMaybe eligible
          closed=[(range,SkippedByPolicy) | entry@(range,_) <- ordered, not (current entry)]
           ++ [(prefix,SkippedByPolicy) | (range,_) <- eligible, prefix <- maybeToList (spanPrefix range)]
          remaining=[(DueSpan (spanLast range) (spanThrough range),reason)
           | (range,reason) <- drop 1 eligible, spanCount range > 1]
      in ResolveDues (DueResolution { dueToAdmit=admitted, rangesToClose=closed, deferredRemainders=remaining })
 where
 ordered = sortOn (routineDue . dueRoutineSnapshot . spanFirst . fst) pending
 latest = reverse (sortOn (spanThrough . fst) pending)
 selectLast range rest reason = ResolveDues (DueResolution
  { dueToAdmit=Just (spanLast range)
  , rangesToClose=[(other,reason) | (other,_) <- rest] ++ [(prefix,reason) | prefix <- maybeToList (spanPrefix range)]
  , deferredRemainders=[] })
 current (range,reason) = case reason of
  BlockedBy _ -> False
  _ -> addUTCTime (intervalSeconds (routineMinutes (dueRoutineSnapshot (spanFirst range)))) (spanThrough range) > time

validDueTarget :: Due -> Bool
validDueTarget due = targetOwner (dueTarget due) == routineOwner (dueRoutineSnapshot due)

validTarget :: ScheduledOccurrence -> Bool
validTarget occurrence = targetOwner (occurrenceTarget occurrence) == routineOwner (occurrenceRoutineSnapshot occurrence)

targetOwner :: ScheduledTarget -> RoutineOwner
targetOwner target = case target of
 PersonalTarget agent -> PersonalOwner agent
 GroupTarget group _ -> GroupOwner group
routineSummaries :: Service -> IO [Value]
routineSummaries s = DB.connection s $ \db -> do
 routineRows <- DB.queryRowsOn db "SELECT value FROM scheduled_routines ORDER BY sequence" ["value"] []
 routines <- forM routineRows $ \row -> do
  value <- DB.decodeStored (at row "value") :: IO Value
  decoded <- try (decodeValue 500 value) :: IO (Either APIError Routine)
  pure (remove ["activeRun","activeRunError"] value,decoded)
 rows <- DB.queryRowsOn db "SELECT id,value,status FROM routine_occurrences WHERE status<>'closed' ORDER BY rowid" ["id","value","status"] []
 dues <- traverse readStoredDue rows
 let visible=filter (isNothing . routineDeletedAt) [routine | (_,Right routine) <- routines]
     visibleIds=S.fromList (map routineId visible)
     admitted=[(occurrence,status) | Admitted occurrence status <- dues, routineId (occurrenceRoutineSnapshot occurrence) `S.member` visibleIds]
     byRoutine=M.fromListWith (++) [(routineId (occurrenceRoutineSnapshot occurrence),[(occurrence,status)]) | (occurrence,status) <- admitted]
     deferred=M.fromListWith combine
      [(routineId (dueRoutineSnapshot (spanFirst range)),(spanCount range,spanFirst range)) | Deferred range _ <- dues]
     combine (count,oldest) (otherCount,otherOldest) =
      (count+otherCount,if routineDue (dueRoutineSnapshot oldest) < routineDue (dueRoutineSnapshot otherOldest) then oldest else otherOldest)
     dueFields identity=case M.lookup identity deferred of
      Nothing -> []
      Just (count,oldest) -> [("deferredCount",toJSON count),("oldestDeferredDue",get (toJSON (dueRoutineSnapshot oldest)) "nextDue")]
 context <- try (readSummaryContextOn s db admitted) :: IO (Either APIError SummaryContext)
 fmap catMaybes $ forM routines $ \(value,decoded) -> case decoded of
  Left problem -> Just <$> invalidActive value problem
  Right routine | isJust (routineDeletedAt routine) -> pure Nothing
  Right routine -> do
   let stored=M.findWithDefault [] (routineId routine) byRoutine
       base=merge (toJSON routine) (obj (dueFields (routineId routine)))
   Just <$> (do
    -- Keep the batched fast path; isolate a malformed neighbour on failure.
    local <- either (const (readSummaryContextOn s db stored)) pure context
    summaries <- readRoutineProjection s local stored
    condition <- validated (routineCondition summaries)
    let current=find (not . runFinished) summaries
        latest=find runFinished summaries
        reference=toJSON . occurrenceId . summaryOccurrence
    pure (merge base (obj ([("condition",toJSON condition),("currentRun",maybe Null reference current),("latestRun",maybe Null reference latest)]
     ++ [("activeRun",reference active) | active <- maybeToList current])))) `catch` invalidActive base
 where
 invalidActive value problem@(APIError _ reason) = do
  hPutStrLn stderr ("Routine " <> T.unpack (at value "id") <> " active Run unavailable: " <> show problem)
  pure (put "activeRunError" (str reason) value)

storeRoutine :: Service -> Routine -> IO Value
storeRoutine s = DB.saveRoutine s . toJSON

initializeRoutines :: Service -> IO ()
initializeRoutines s = do
 rows <- DB.queryRows s "SELECT sequence,value FROM scheduled_routines ORDER BY sequence" ["sequence","value"] []
 works <- DB.values s DB.Workloads
 groups <- DB.values s DB.Groups
 forM_ rows $ \row -> tryRoutine ("scheduled routine " <> jsonText (get row "sequence")) $ do
  r <- DB.decodeStored (at row "value")
  initial <- decodeValue 500 r :: IO Routine
  unless (has r "runCount") $ do
   let group=findId (at r "owner") groups
       count=case group of Just g->length [t | t<-items g "tasks",get t "routine"==get r "id"]; Nothing->length [w | w<-works, ValidOrigin origin<-[decodeStoredOrigin (Just (get w "origin"))], originRoutine origin==Just (routineId initial)]
   void (DB.saveRoutine s (merge r (obj [("runCount",num count),("paused",Bool (count>=1000))])))
  routine <- decodeValue 500 r
  if isJust (routineDeletedAt routine) then completeDeletion s routine
   else unless (routinePaused routine) (ensurePolicyPreparation s routine)
data RoutineEdit = AddRoutine (Maybe Text) | EditRoutine RoutineId (Maybe Int)
data RoutineWrite = NewRoutine (Maybe Text) | ReviseRoutine Routine
data RoutineSelection = RoutineReplay Routine | RoutineChange RoutineWrite

routineInput :: Value -> Value -> Validation (Text,Minutes)
routineInput minutes execution = do
 body <- validateValue 400 (textFieldP "prompt" 16000) (obj [("prompt",execution)])
 interval <- validateValue 400 parseJSON minutes
 check (body `notElem` ["PROMPT","TODO"]) 400 "Replace PROMPT with the work to execute"
 pure (body,interval)
-- Decode only the candidate so unrelated corrupt rows cannot block this write.
decodeRoutineCandidate :: RoutineEdit -> Minutes -> Text -> [Value] -> Validation (Maybe Routine)
decodeRoutineCandidate edit interval body routines = traverse (validateValue 500 parseJSON) candidate
 where
 candidate = case edit of
  EditRoutine identity _ -> findId (routineIdText identity) routines
  AddRoutine source -> source >>= \call -> find (\r -> get r "sourceCall" == str call && get r "minutes" == toJSON interval && at r "prompt" == body) routines

selectRoutine :: RoutineEdit -> Maybe Routine -> Validation RoutineSelection
selectRoutine edit candidate = case edit of
 EditRoutine _ previous -> do
  existing <- maybe (Left (APIError 404 "Unknown Scheduled Routine in this conversation")) Right candidate
  availableRoutine existing
  forM_ previous $ \old -> check (minuteCount (routineMinutes existing)==old) 409 "Frequency changed; open Edit again for the current routine"
  pure (RoutineChange (ReviseRoutine existing))
 AddRoutine source -> case candidate of
  Just existing -> availableRoutine existing >> Right (RoutineReplay existing)
  Nothing -> Right (RoutineChange (NewRoutine source))
routineValue :: UTCTime -> RoutineOwner -> Minutes -> Text -> Maybe D.TaskSpec -> Maybe MissedDuePolicy -> RoutineWrite -> Validation Value
routineValue time owner interval body specification policy change = do
 case change of ReviseRoutine existing -> availableRoutine existing; NewRoutine _ -> pure ()
 case change of
  ReviseRoutine existing -> check
   (body == routinePrompt existing || isNothing (routineSpecification existing) || isJust specification)
   400 (case owner of
    PersonalOwner _ -> "This Routine has a structured specification. The text edit template can change its frequency with the prompt unchanged. To change the work, ask this Sapi in an ordinary message to update the full objective, inputs, expected outputs and criteria together. No changes were saved."
    GroupOwner _ -> "This Group Routine has a structured specification. The text edit template can change its frequency with the prompt unchanged. To change its work, ask the Group Lead to update the full objective, inputs, expected outputs and criteria together. No changes were saved.")
  NewRoutine _ -> pure ()
 preparedSpecification <- traverse (either (Left . APIError 400) Right . D.preparePublicationSpec previousSpecification) specification
 pure $ merge initial (obj (
  [("owner",toJSON owner),("title",str (T.unwords (take 7 (T.words body)))),("minutes",toJSON interval)
  ,("prompt",str body),("updated",str (timestamp time)),("nextDue",toJSON deadline)
  ,("policySelection",toJSON (maybe initialPolicy PolicySelected policy))]
  ++ [("specification",toJSON spec) | spec <- maybeToList preparedSpecification]))
 where
 previousSpecification = case change of ReviseRoutine existing -> routineSpecification existing; NewRoutine _ -> Nothing
 deadline = case change of
  ReviseRoutine existing | routineMinutes existing == interval -> routineDeadline existing
  _ -> newDeadline (addUTCTime (intervalSeconds interval) time)
 initialPolicy = case change of
  ReviseRoutine existing -> routinePolicy existing
  NewRoutine _ -> AwaitingSelection
 initial = case change of
  ReviseRoutine existing -> toJSON existing
  NewRoutine source -> obj ([("created",str (timestamp time)),("runCount",num 0),("paused",Bool False),("policySelection",toJSON AwaitingSelection)] ++ [("sourceCall",str call) | call <- maybeToList source])

save :: Service -> Text -> Value -> Value -> Maybe D.TaskSpec -> Maybe MissedDuePolicy -> RoutineEdit -> IO Value
save s rawOwner minutes execution specification policy edit = do
 owner <- decodeValue 400 (str rawOwner)
 (body,interval) <- validated (routineInput minutes execution)
 case owner of
  GroupOwner group -> do
   groups <- DB.values s DB.Groups
   g <- maybe (failure 404 "Unknown Group") pure (findId (groupIdText group) groups)
   require (not (boolean (get g "archived"))) 409 "Restore this Group before changing its work"
  PersonalOwner agent -> requireActive s (agentIdText agent)
 routines <- filter ((==toJSON owner) . (`get` "owner")) <$> DB.values s DB.ScheduledRoutines
 selected <- validated (decodeRoutineCandidate edit interval body routines >>= selectRoutine edit)
 case selected of
  RoutineReplay existing -> do
   forM_ specification $ \spec -> do
    prepared <- either (failure 400) pure (D.preparePublicationSpec (routineSpecification existing) spec)
    require (routineSpecification existing == Just prepared)
     409 "This creation already has a different specification; update the existing routine"
   forM_ policy $ \chosen -> require (routinePolicy existing == PolicySelected chosen)
    409 "This creation already has a different policy; update the existing routine"
   pure (toJSON existing)
  RoutineChange change -> do
   destination <- case change of
    NewRoutine _ -> pure Nothing
    ReviseRoutine existing -> scheduledTarget s existing
   time <- getCurrentTime
   result <- DB.connection s $ \db -> SQL.withTransaction db $ do
    captured <- case change of
     NewRoutine _ -> pure change
     ReviseRoutine existing -> do
      latest <- readRoutineOn db (routineId existing)
      validated (availableRoutine latest)
      advanced <- if routinePaused latest || routineDue latest > time then pure latest else do
       target <- maybe (failure 409 "Routine target is unavailable before editing elapsed deadlines") pure destination
       validateTargetOn db target
       let (elapsed,next)=captureDues time latest target
       stored <- readDuesOn db latest
       active <- activeRun s db latest target stored
       let reason=deferralReason (routinePolicy latest) active
       mapM_ (insertDeferredOn db reason) elapsed
       recordDeferralOn s db latest reason elapsed
       pure next
      pure (ReviseRoutine advanced)
    value <- validated (routineValue time owner interval body specification policy captured)
    saved <- DB.saveRoutineOn db value
    let action = case change of NewRoutine _ -> "routine.created"; ReviseRoutine _ -> "routine.updated"
    DB.recordOn s db [routineOwnerText owner] (at saved "id") action saved "admin" Nothing
    pure saved
   prepared <- decodeValue 500 result
   tryRoutine ("routine policy " <> routineIdText (routineId prepared)) (ensurePolicyPreparation s prepared)
   pure result

setPolicy :: Service -> Text -> RoutineId -> Text -> PolicySelection -> MissedDuePolicy -> IO Value
setPolicy s actor identity expectedPrompt expectedSelection policy = do
 routine <- DB.connection s $ \db -> readRoutineOn db identity
 case routineOwner routine of
  GroupOwner groupId -> do
   group <- G.getGroup s (groupIdText groupId) True
   G.authorizeGroup s group actor Management
  PersonalOwner agent -> require (actor == agentIdText agent) 403 "Only the owner can select this routine's policy"
 DB.connection s $ \db -> SQL.withTransaction db $ do
  latest <- readRoutineOn db identity
  validated (availableRoutine latest)
  require (routinePrompt latest == expectedPrompt) 409 "Routine instructions changed; read them before selecting a policy"
  let updated=latest { routinePolicy=PolicySelected policy }
  require (routinePolicy latest == expectedSelection || routinePolicy latest == routinePolicy updated)
   409 "Routine policy changed; read it before selecting another policy"
  when (routinePolicy latest /= routinePolicy updated) $ do
   void (DB.saveRoutineOn db (toJSON updated))
   DB.recordOn s db [routineOwnerText (routineOwner updated)] (routineIdText identity) "routine.policy_selected"
    (obj [("policy",toJSON policy)]) actor Nothing
  pure (toJSON updated)
setRoutinePaused :: Service -> Text -> RoutineId -> Bool -> IO Value
setRoutinePaused s actor identity paused = do
 routine <- resultRoutine s actor identity
 setPaused s (routineOwnerText (routineOwner routine)) identity paused

setPaused :: Service -> Text -> RoutineId -> Bool -> IO Value
setPaused s rawOwner identity paused = do
 owner <- decodeValue 400 (str rawOwner)
 routine <- DB.connection s $ \db -> readRoutineOn db identity
 require (routineOwner routine == owner) 404 "Unknown Scheduled Routine in this conversation"
 target <- if paused then scheduledTarget s routine else pure Nothing
 time <- getCurrentTime
 DB.connection s $ \db -> SQL.withTransaction db $ do
  latest <- readRoutineOn db identity
  validated (availableRoutine latest)
  if routinePaused latest == paused then pure (toJSON latest) else do
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
   DB.recordOn s db [routineOwnerText owner] (routineIdText identity) (if paused then "routine.paused" else "routine.resumed") result "admin" Nothing
   pure result
command :: Service -> Text -> Text -> Bool -> IO (Maybe (C.Command,Routine))
command s owner input attached = case C.interpret attached (T.unpack input) of
 Left error -> failure 400 (T.pack error)
 Right Nothing -> pure Nothing
 Right (Just change) -> do
  value <- case change of
   C.Create interval body -> save s owner (toJSON interval) (str (T.pack body)) Nothing Nothing (AddRoutine Nothing)
   C.Edit i old interval body -> do
    identity <- decodeValue 400 (str (T.pack i))
    save s owner (toJSON interval) (str (T.pack body)) Nothing Nothing (EditRoutine identity (Just (fromIntegral old)))
   C.SetPaused i paused -> do
    identity <- decodeValue 400 (str (T.pack i))
    setPaused s owner identity paused
  routine <- decodeValue 500 value
  pure (Just (change,routine))

receipt :: C.Command -> Routine -> Text
receipt change routine = action <> interval <> availability
 where
 action=case change of
  C.Create _ _ -> "Schedule created."
  C.Edit _ _ _ _ -> "Schedule updated."
  C.SetPaused _ _ -> if routinePaused routine then "Schedule paused." else "Schedule resumed."
 minutes=minuteCount (routineMinutes routine)
 interval=" It is set to run every " <> T.pack (show minutes) <> (if minutes == 1 then " minute." else " minutes.")
 availability
  | routinePaused routine = case change of
     C.SetPaused _ _ -> ""
     C.Create _ _ -> " It is paused."
     C.Edit _ _ _ _ -> " It is paused."
  | routinePolicy routine == AwaitingSelection = " Automatic runs are waiting for setup to finish."
  | otherwise = case change of
     C.SetPaused _ _ -> ""
     C.Create _ _ -> " It is active."
     C.Edit _ _ _ _ -> " It is active."

acknowledge :: Service -> Text -> Text -> C.Command -> Routine -> IO Value
acknowledge s owner input change routine = do
 i <- newId
 t <- now
 let output=receipt change routine
     turn=obj [("id",str i),("flow",str "chat"),("input",str input),("status",str "done"),("created",str t),("output_at",str t)]
     messages=[obj [("role",str role),("content",str content),("turn",str i),("time",str t)] | (role,content)<-[("user",input),("agent",output)]]
 modifyState s owner $ \state -> merge state (obj [("turns",arr (items state "turns"++[toJSON turn])),("chat",arr (items state "chat"++messages))])
 DB.project s owner
 pure (obj [("id",str i),("agent",str owner),("status",str "done"),("flow",str "chat")])

advanceDeadline :: Routine -> Routine
advanceDeadline routine = routine
 { routineDeadline=newDeadline (addUTCTime (intervalSeconds (routineMinutes routine)) (routineDue routine)) }

captureDues :: UTCTime -> Routine -> ScheduledTarget -> ([DueSpan],Routine)
captureDues time routine target
 | routineDue routine > time = ([],routine)
 | otherwise = ([DueSpan first through],routine { routineDeadline=newDeadline (addUTCTime interval through) })
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
 , routineDeadline=if paused then routineDeadline routine else rescheduleDeadline (addUTCTime (intervalSeconds (routineMinutes routine)) time) (routineDeadline routine)
 , routineAllowanceUsed=if not paused && routineAllowanceUsed routine>=1000 then 0 else routineAllowanceUsed routine
 , routineFields=KM.insert "updated" (str (timestamp time)) (routineFields routine) }

dispatch :: Service -> Pulse -> IO ()
dispatch s tick = when (pulseFrequency tick == Bph60) $ do
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
 | isJust (routineDeletedAt routine) = completeDeletion s routine
 | routinePaused routine = pure ()
 | routineAllowanceUsed routine >= 1000 = do
    void (storeRoutine s routine { routinePaused=True })
    DB.record s [routineOwnerText (routineOwner routine)] (routineIdText (routineId routine)) "routine.paused"
     (obj [("reason",str "run limit"),("runCount",num (routineAllowanceUsed routine))]) "system" Nothing
 | otherwise = do
    ensurePolicyPreparation s routine
    target <- scheduledTarget s routine
    forM_ target $ \destination -> admitDue s time routine destination >>= mapM_ (materializeOccurrence s)

scheduledTarget :: Service -> Routine -> IO (Maybe ScheduledTarget)
scheduledTarget s routine = case routineOwner routine of
 PersonalOwner agent -> do
  inactive <- retired s (agentIdText agent)
  pure (if inactive then Nothing else Just (PersonalTarget agent))
 GroupOwner groupId -> do
  rows <- DB.queryRows s "SELECT value FROM groups WHERE id=?" ["value"] [SQL.SQLText (groupIdText groupId)]
  group <- case rows of
   row:_ -> DB.decodeStored (at row "value")
   [] -> failure 409 "Routine Group no longer exists"
  agent <- parseValue 500 personalAgentP (get group "lead")
  inactive <- retired s (agentIdText agent)
  pure (if inactive || boolean (get group "archived") then Nothing else Just (GroupTarget groupId agent))

stableRunId :: Text -> RunId
stableRunId key = RunId (T.filter (/='-') (UUID.toText
 (UUID5.generateNamed UUID5.namespaceURL (BS.unpack (TE.encodeUtf8 key)))))

occurrenceIdFor :: Routine -> RunId
occurrenceIdFor routine = stableRunId (routineIdText (routineId routine) <> ":" <> deadlineText (routineDeadline routine))

triggeredId :: Routine -> RunTrigger -> RunId
triggeredId routine trigger = case trigger of
 ScheduledTrigger -> occurrenceIdFor routine
 ManualTrigger request -> stableRunId ("routine-manual:" <> routineIdText (routineId routine) <> ":" <> request)

dueIdFor :: Routine -> DueId
dueIdFor = DueId . runIdText . occurrenceIdFor

readStoredDue :: Value -> IO StoredDue
readStoredDue row = do
 value <- DB.decodeStored (at row "value")
 stored <- case at row "status" of
  "deferred" -> do
   require (not (has value "closure")) 500 "Deferred due already contains a closure"
   Deferred <$> decodeValue 500 value <*> decodeValue 500 (get value "reason")
  "admitted_pending" -> admitted MaterializationPending value
  "admitted_ready" -> admitted Materialized value
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
  ["id","value","status"] [SQL.SQLText (routineIdText (routineId routine))]
 traverse readStoredDue rows

activePersonal :: Service -> SQL.Connection -> Routine -> [StoredDue] -> IO (Maybe RunId)
activePersonal s db routine dues = do
 state <- agentState s (routineOwnerText (routineOwner routine))
 forM_ [turn | turn <- agentTurns state, activeStatus (turnStatus turn)] $ \turn -> void (requireExecutionOrigin (turnOrigin turn))
 let turns = [turn | turn <- agentTurns state, ValidOrigin origin <- [turnOrigin turn], originRoutine origin == Just (routineId routine)]
     admitted = [occurrenceId occurrence | Admitted occurrence _ <- dues]
     identities = nubOrd (admitted ++ map turnId turns)
     stopped = S.fromList [occurrenceId occurrence | Admitted occurrence _ <- dues, isJust (occurrenceStoppedAt occurrence)]
 workRows <- readRuns "SELECT id,value FROM workloads" ["id","value"] identities
 savedWorks <- traverse (DB.decodeStored . (`at` "value")) workRows :: IO [D.Workload]
 let calls=concatMap (map D.callIdentity . NE.toList . D.workCalls) savedWorks
     works=M.fromList [(D.workloadId work,work) | work<-savedWorks]
 turnRows <- readRuns "SELECT id,status FROM turns" ["id","status"] (nubOrd (identities++calls))
 projected <- fmap M.fromList $ forM turnRows $ \row -> do
  status <- field 500 row "status"
  pure (at row "id",status)
 active <- filterM (activeIdentity turns works projected stopped) identities
 pure (case active of i:_ -> Just i; [] -> Nothing)
 where
 -- Bound placeholders for SQLite builds with lower variable limits.
 readRuns _ _ [] = pure []
 readRuns query columns identities = do
  let (batch,remaining)=splitAt 128 identities
      placeholders=T.intercalate "," (replicate (length batch) "?")
  rows <- DB.queryRowsOn db (query<>" WHERE id IN ("<>placeholders<>")") columns
   [SQL.SQLText (runIdText identity) | identity<-batch]
  (rows++) <$> readRuns query columns remaining
 activeIdentity turns works projected stopped identity = do
  let inState = find ((==identity) . turnId) turns
      work=M.lookup (runIdText identity) works
      status=M.lookup (runIdText identity) projected
      liveCall = case work of
       Just w -> D.callState (NE.last (D.workCalls w)) `elem`
        [D.CallQueued,D.CallRunning,D.CallDelegated,D.CallWaiting]
       Nothing -> False
      liveTurn = maybe False (activeStatus . turnStatus) inState || maybe False activeStatus status
       || maybe False (any (maybe False activeStatus . (\call -> M.lookup (runIdText (D.callIdentity call)) projected)) . NE.toList . D.workCalls) work
  runs <- readMVar (serviceRuns s)
  let liveProcess=maybe False (any ((`M.member` runs) . D.callIdentity) . NE.toList . D.workCalls) work
  when (isNothing inState && isNothing status && isNothing work && identity `S.notMember` stopped) $
   failure 500 "Admitted routine Run has no persisted state"
  pure (liveCall || liveTurn || liveProcess)

activeGroup :: SQL.Connection -> Routine -> IO (Maybe RunId)
activeGroup db routine = do
 rows <- DB.queryRowsOn db "SELECT value FROM groups WHERE id=?" ["value"] [SQL.SQLText (routineOwnerText (routineOwner routine))]
 group <- case rows of
  row:_ -> DB.decodeStored (at row "value")
  [] -> failure 409 "Routine Group disappeared before admission"
 active <- either (failure 500) pure (GW.activeRoutineRequest Nothing group (routineId routine))
 requests <- G.readRequests group
 case active >>= \identity -> find ((==identity) . G.requestId) requests of
  Nothing -> pure active
  Just request -> do
   lineage <- G.requireRequestLineage request
   pure (groupAdmission lineage <|> active)

groupAdmission :: G.RequestLineage -> Maybe RunId
groupAdmission lineage = do
 link <- G.requestTask lineage
 identity <- T.stripPrefix "gtask_" (taskIdText (G.requestTaskId link))
 pure (RunId identity)

activeRun :: Service -> SQL.Connection -> Routine -> ScheduledTarget -> [StoredDue] -> IO (Maybe RunId)
activeRun s db routine target dues = case [occurrenceId occurrence | Admitted occurrence MaterializationPending <- dues, isNothing (occurrenceStoppedAt occurrence)] of
 i:_ -> pure (Just i)
 [] -> case target of
  PersonalTarget _ -> activePersonal s db routine dues
  GroupTarget _ _ -> activeGroup db routine

-- Caller must hold the host lock through requeueing.
ensureRetryAvailable :: Service -> Turn -> IO ()
ensureRetryAvailable s turn = do
 ensureExecutionAllowed s (turnId turn)
 origin <- requireExecutionOrigin (turnOrigin turn)
 forM_ (originRoutine origin) $ \identity -> do
  DB.connection s $ \db -> do
   routine <- readRoutineOn db identity
   stored <- readDuesOn db routine
   target <- maybe (failure 409 "Routine Run has no recorded admission") pure
    (listToMaybe [occurrenceTarget occurrence | Admitted occurrence _ <- stored])
   case target of
    GroupTarget _ _ -> failure 409 "Start the current task revision from Work instead"
    PersonalTarget _ -> do
     active <- activeRun s db routine target stored
     require (isNothing active) 409 "Another Run of this Routine is active or awaiting publication. Wait for its result to be published, or stop it if it is still running, before retrying"

occurrenceTurn :: ScheduledOccurrence -> Turn
occurrenceTurn occurrence = newTurn (occurrenceId occurrence) Chat
 (routinePrompt routine) (timestamp (occurrenceAdmittedAt occurrence)) origin True
 where
 routine=occurrenceRoutineSnapshot occurrence
 value=toJSON routine
 due=case occurrenceTrigger occurrence of ScheduledTrigger -> Just (routineDeadline routine); ManualTrigger _ -> Nothing
 origin=toJSON (newOrigin (ScheduledExecution (ScheduledProvenance (routineId routine) (routineOwner routine) due)) Nothing
  (KM.fromList [("title",get value "title"),("minutes",toJSON (routineMinutes routine))
   ,("taskType",str "automated"),("executionPrompt",str (routinePrompt routine))]))

-- Run identity uses the original deadline, even after instruction edits.
prepareOccurrence :: UTCTime -> Routine -> ScheduledTarget -> Due -> Validation ScheduledOccurrence
prepareOccurrence time current target due = do
 let original=dueRoutineSnapshot due
 check (routineId current == routineId original && routineOwner current == routineOwner original)
  409 "Routine identity or owner changed before admission"
 let fields=case KM.lookup "title" (routineFields current) of
      Nothing -> KM.delete "title" (routineFields original)
      Just title -> KM.insert "title" title (routineFields original)
     snapshot=original { routinePrompt=routinePrompt current, routinePolicy=routinePolicy current
                       , routineSpecification=routineSpecification current, routineFields=fields }
     occurrence=ScheduledOccurrence (occurrenceIdFor snapshot) snapshot target time ScheduledTrigger Nothing []
 check (dueId due == dueIdFor snapshot && validTarget occurrence) 409 "Due identity or target changed before admission"
 pure occurrence

prepareManualOccurrence :: UTCTime -> Routine -> ScheduledTarget -> Text -> Maybe RunId -> Validation ScheduledOccurrence
prepareManualOccurrence time routine target request active = do
 forM_ active $ \run -> check False 409
  ("Another Run of this Routine is active or awaiting publication: " <> runIdText run <> ". Wait for its result to be published, or stop it if it is still running, before running it again.")
 let trigger=ManualTrigger request
     occurrence=ScheduledOccurrence (triggeredId routine trigger) routine target time trigger Nothing []
 check (validTarget occurrence) 409 "Routine target changed before admission"
 pure occurrence

runNow :: Service -> Text -> RoutineId -> Text -> RunId -> IO Value
runNow s actor identity request caller = do
 routine <- DB.connection s $ \db -> readRoutineOn db identity
 case routineOwner routine of
  PersonalOwner agent -> require (actor == agentIdText agent) 404 "Unknown Scheduled Routine in this conversation"
  GroupOwner groupId -> G.getGroup s (groupIdText groupId) True >>= \group -> G.authorizeGroup s group actor Management
 let trigger=ManualTrigger request
 require (not (T.null (T.strip request)) && T.length request<=128) 500 "Invalid saved task identity for Run now"
 destination <- scheduledTarget s routine
 time <- getCurrentTime
 (occurrence,materialization) <- DB.connection s $ \db -> SQL.withTransaction db $ do
  latest <- readRoutineOn db identity
  validated (availableRoutine latest)
  let run=triggeredId latest trigger
  previous <- DB.queryRowsOn db "SELECT id,value,status FROM routine_occurrences WHERE id=?"
   ["id","value","status"] [SQL.SQLText (runIdText run)]
  case previous of
   row:_ -> do
    saved <- readStoredDue row
    case saved of
     Admitted existing status -> do
      require (routineId (occurrenceRoutineSnapshot existing) == identity && occurrenceTrigger existing == trigger)
       409 "Manual request identity belongs to another Run"
      pure (existing,status)
     _ -> failure 409 "Manual request identity belongs to a scheduled deadline"
   [] -> do
    target <- maybe (failure 409 "Routine owner must be active before running it") pure destination
    validateTargetOn db target
    stored <- readDuesOn db latest
    active <- activeRun s db latest target stored
    admitted <- validated (prepareManualOccurrence time latest target request active)
    DB.executeOn db "INSERT INTO routine_occurrences(id,value,status) VALUES (?,?,'admitted_pending')"
     [SQL.SQLText (runIdText run),SQL.SQLText (jsonText (toJSON admitted))]
    DB.recordOn s db [routineOwnerText (routineOwner latest)] (routineIdText identity) "routine.triggered"
     (obj [("turn",toJSON run),("trigger",toJSON trigger)]) actor (Just (runIdText caller))
    pure (admitted,MaterializationPending)
 when (materialization == MaterializationPending) (materializeOccurrence s occurrence)
 pure (obj [("id",toJSON (occurrenceId occurrence)),("routine",toJSON (routineId (occurrenceRoutineSnapshot occurrence)))])

ensureOccurrenceAvailable :: Service -> RunId -> IO ()
ensureOccurrenceAvailable s identity = do
 rows <- DB.queryRows s "SELECT id,value,status FROM routine_occurrences WHERE id=?"
  ["id","value","status"] [SQL.SQLText (runIdText identity)]
 forM_ rows $ \row -> do
  stored <- readStoredDue row
  case stored of
   Admitted occurrence _ -> do
    DB.connection s (\db -> readRoutineOn db (routineId (occurrenceRoutineSnapshot occurrence))) >>= validated . availableRoutine
    require (isNothing (occurrenceStoppedAt occurrence)) 409 "This Routine Run was stopped; request a new Run now instead"
   _ -> failure 409 "This identity is not an admitted Routine Run"

ensureTaskAvailable :: Service -> TaskId -> IO ()
ensureTaskAvailable s task = forM_ (T.stripPrefix "gtask_" (taskIdText task)) (ensureOccurrenceAvailable s . RunId)

ensureExecutionAllowed :: Service -> RunId -> IO ()
ensureExecutionAllowed s identity = do
 work <- D.findWork s identity
 case work of
  Just (saved,_) -> do
   requireExecutionOrigin (D.workOrigin saved) >>= ensureOriginAvailable s
   ensureOccurrenceAvailable s (RunId (D.workloadId saved))
  Nothing -> do
   groups <- DB.values s DB.Groups
   forM_ groups $ \group -> do
    requests <- G.readRequests group
    forM_ (find ((==identity) . G.requestId) requests) $ \request -> do
     requireExecutionOrigin (G.requestOrigin request) >>= ensureOriginAvailable s
     lineage <- G.requireRequestLineage request
     forM_ (groupAdmission lineage) (ensureOccurrenceAvailable s)

occurrenceRequests :: ScheduledOccurrence -> Value -> IO [G.GroupRequest]
occurrenceRequests occurrence group = do
 requests <- G.readRequests group
 filterM belongs requests
 where
 belongs request = do
  lineage <- G.requireRequestLineage request
  let related=groupAdmission lineage == Just (occurrenceId occurrence)
  when related $ do
   origin <- requireExecutionOrigin (G.requestOrigin request)
   require (originRoutine origin == Just (routineId (occurrenceRoutineSnapshot occurrence)))
    500 "Routine Run request has inconsistent provenance"
  pure related

occurrenceCalls :: Service -> ScheduledOccurrence -> IO [(AgentId,RunId)]
occurrenceCalls s occurrence = case occurrenceTarget occurrence of
 PersonalTarget agent -> do
  saved <- D.findWork s (occurrenceId occurrence)
  pure $ case saved of
   Nothing -> [(agent,occurrenceId occurrence)]
   Just (work,_) -> [(D.callRecipient call,D.callIdentity call) | call <- NE.toList (D.workCalls work)]
 GroupTarget group _ -> do
  value <- G.getGroup s (groupIdText group) False
  requests <- occurrenceRequests occurrence value
  pure [(G.requestTarget request,G.requestId request) | request <- requests]

occurrenceActive :: Service -> ScheduledOccurrence -> MaterializationStatus -> IO Bool
occurrenceActive s occurrence materialization = do
 calls <- occurrenceCalls s occurrence
 runs <- readMVar (serviceRuns s)
 live <- forM calls $ \(owner,identity) -> do
  state <- agentState s (agentIdText owner)
  pure (M.member identity runs || any (\turn -> turnId turn == identity && turnStatus turn `elem` [Queued,Running]) (agentTurns state))
 logical <- case occurrenceTarget occurrence of
  PersonalTarget _ -> do
   saved <- D.findWork s (occurrenceId occurrence)
   pure (maybe False ((`elem` [D.CallQueued,D.CallRunning,D.CallWaiting]) . D.callState . NE.last . D.workCalls . fst) saved)
  GroupTarget group _ -> do
   value <- G.getGroup s (groupIdText group) False
   requests <- occurrenceRequests occurrence value
   task <- traverse (decodeValue 500) (findId ("gtask_" <> runIdText (occurrenceId occurrence)) (items value "tasks"))
   pure (any (\request -> not (G.requestSettled request) && G.requestStatus request `elem` [Queued,Running]) requests
    || (maybe False GW.taskInProgress task && not (any ((==OutputPending) . G.requestStatus) requests)))
 pure (or live || logical || materialization == MaterializationPending)

cancelOccurrenceTurn :: Service -> AgentId -> RunId -> IO ()
cancelOccurrenceTurn s owner identity = do
 runs <- readMVar (serviceRuns s)
 state <- agentState s (agentIdText owner)
 case find ((==identity) . turnId) (agentTurns state) of
  Just turn | Just running <- M.lookup (batchLeader turn) runs -> writeIORef (runCancel running) True
  Just turn | turnStatus turn `elem` [Queued,Running] -> do
   let cancel saved = if turnId saved == identity then
        (withTurnFields (put "error" (str "Stopped by Admin")) saved) { turnStatus=Cancelled } else saved
   saveState s (agentIdText owner) (mapTurns cancel state)
   DB.project s (agentIdText owner)
  _ -> pure ()

cancelOccurrence :: Service -> ScheduledOccurrence -> IO ()
cancelOccurrence s occurrence = do
 calls <- occurrenceCalls s occurrence
 mapM_ (uncurry (cancelOccurrenceTurn s)) calls
 case occurrenceTarget occurrence of
  PersonalTarget _ -> do
   saved <- D.findWork s (occurrenceId occurrence)
   forM_ saved (cancelWorkload s . fst)
  GroupTarget group _ -> do
   original <- G.getGroup s (groupIdText group) False
   requests <- occurrenceRequests occurrence original
   runs <- readMVar (serviceRuns s)
   changed <- foldM (cancelRequest runs) original requests
   time <- getCurrentTime
   let taskId=TaskId ("gtask_" <> runIdText (occurrenceId occurrence))
   updated <- case findId (taskIdText taskId) (items changed "tasks") of
    Nothing -> pure changed
    Just raw -> do
     task <- decodeValue 500 raw
     pure (G.replace "tasks" (toJSON (GW.stopTask time "system" task)) changed)
   when (updated /= original) $ void (G.saveGroup s updated "system" "routine stopped" (toJSON (occurrenceId occurrence)))
 where
 cancelRequest runs group request
  | G.requestSettled request || G.requestStatus request `notElem` [Queued,Running] || M.member (G.requestId request) runs = pure group
  | otherwise = do
     let stopped=request { G.requestStatus=Cancelled, G.requestSettled=True
                         , G.requestFields=KM.insert "error" (str "Stopped by Admin") (G.requestFields request) }
         turn=obj [("status",toJSON Cancelled),("output",Null),("error",str "Stopped by Admin")]
     (updated,_) <- GW.settleTask group request turn
     pure (G.replace "requests" (toJSON stopped) updated)

cancelWorkload :: Service -> D.Workload -> IO ()
cancelWorkload s work = do
 let frontier=D.callIdentity (NE.last (D.workCalls work))
     cancel call = if D.callState call `elem` [D.CallQueued,D.CallRunning]
       || (D.callIdentity call == frontier && D.callState call == D.CallWaiting)
      then call { D.callProgress=D.CancelledAt (D.callNode call) } else call
     changed=fmap cancel (D.workCalls work)
 when (changed /= D.workCalls work) (D.saveWork s work { D.workCalls=changed })

stopOccurrenceOn :: SQL.Connection -> UTCTime -> ScheduledOccurrence -> IO ScheduledOccurrence
stopOccurrenceOn db time occurrence = do
 (latest,_) <- readOccurrenceOn db (occurrenceId occurrence)
 let stopped=latest { occurrenceStoppedAt=occurrenceStoppedAt latest <|> Just time }
 DB.executeOn db "UPDATE routine_occurrences SET value=?,status='admitted_pending' WHERE id=?"
  [SQL.SQLText (jsonText (toJSON stopped)),SQL.SQLText (runIdText (occurrenceId occurrence))]
 pure stopped

-- The tombstone and Stop intents commit before cancellation can fail or be interrupted.
deleteRoutine :: Service -> Text -> RoutineId -> IO Value
deleteRoutine s actor identity = mask_ $ do
 routine <- DB.connection s (\db -> readRoutineOn db identity)
 target <- case routineOwner routine of
  PersonalOwner owner -> do
   require (actor == "admin" || actor == agentIdText owner) 404 "Unknown Scheduled Routine in this conversation"
   pure (PersonalTarget owner)
  GroupOwner group -> do
   value <- G.getGroup s (groupIdText group) False
   G.authorizeGroup s value actor Management
   GroupTarget group <$> field 500 value "lead"
 stored <- DB.connection s (\db -> readDuesOn db routine)
 active <- filterM (uncurry (occurrenceActive s)) [(occurrence,status) | Admitted occurrence status <- stored]
 time <- getCurrentTime
 deleted <- DB.connection s $ \db -> SQL.withTransaction db $ do
  latest <- readRoutineOn db identity
  case routineDeletedAt latest of
   Just _ -> pure latest
   Nothing -> do
    let (elapsed,advanced)=if routinePaused latest then ([],latest) else captureDues time latest target
        closed range=merge (toJSON range) (obj [("closure",toJSON RoutineDeleted)])
    forM_ elapsed $ \range -> DB.executeOn db "INSERT INTO routine_occurrences(id,value,status) VALUES (?,?,'closed')"
     [SQL.SQLText (dueIdText (dueId (spanFirst range))),SQL.SQLText (jsonText (closed range))]
    forM_ [range | Deferred range _ <- stored] $ \range ->
     DB.executeOn db "UPDATE routine_occurrences SET value=?,status='closed' WHERE id=? AND status='deferred'"
      [SQL.SQLText (jsonText (closed range)),SQL.SQLText (dueIdText (dueId (spanFirst range)))]
    forM_ active $ \(occurrence,_) -> void (stopOccurrenceOn db time occurrence)
    let updated=advanced { routineDeletedAt=Just time, routinePaused=True }
    void (DB.saveRoutineOn db (toJSON updated))
    DB.recordOn s db [routineOwnerText (routineOwner updated)] (routineIdText identity) "routine.deleted" (toJSON updated) actor Nothing
    pure updated
 completeDeletion s deleted
 pure (obj [("id",toJSON identity),("deletedAt",maybe Null (str . timestamp) (routineDeletedAt deleted))])

completeDeletion :: Service -> Routine -> IO ()
completeDeletion s routine = forM_ (routineDeletedAt routine) $ \time -> do
 stored <- DB.connection s (\db -> readDuesOn db routine)
 forM_ [(occurrence,status) | Admitted occurrence status <- stored] $ \(occurrence,status) -> do
  active <- occurrenceActive s occurrence status
  when (active || status == MaterializationPending) $ do
   stopped <- DB.connection s $ \db -> SQL.withTransaction db (stopOccurrenceOn db time occurrence)
   materializeOccurrence s stopped
 cancelPolicyPreparation s routine

cancelPolicyPreparation :: Service -> Routine -> IO ()
cancelPolicyPreparation s routine = do
 rows <- DB.queryRows s "SELECT value FROM workloads WHERE CASE WHEN json_valid(value) THEN json_extract(value,'$.origin.policyRoutine') ELSE NULL END=?"
  ["value"] [SQL.SQLText (routineIdText (routineId routine))]
 works <- traverse (DB.decodeStored . (`at` "value")) rows :: IO [D.Workload]
 forM_ works $ \work -> do
  forM_ (D.workCalls work) $ \call -> cancelOccurrenceTurn s (D.callRecipient call) (D.callIdentity call)
  cancelWorkload s work
 case routineOwner routine of
  PersonalOwner owner -> do
   state <- agentState s (agentIdText owner)
   forM_ [turn | turn <- agentTurns state, ValidOrigin origin <- [turnOrigin turn]
               , originPolicyRoutine origin == Just (routineId routine)] $ \turn -> cancelOccurrenceTurn s owner (turnId turn)
  GroupOwner group -> do
   original <- G.getGroup s (groupIdText group) False
   requests <- G.readRequests original
   let related=[request | request <- requests, ValidOrigin origin <- [G.requestOrigin request]
                        , originPolicyRoutine origin == Just (routineId routine)]
   forM_ related $ \request -> cancelOccurrenceTurn s (G.requestTarget request) (G.requestId request)
   runs <- readMVar (serviceRuns s)
   changed <- foldM (cancelRequest runs) original related
   when (changed /= original) $ void (G.saveGroup s changed "system" "routine setup stopped" (toJSON (routineId routine)))
 where
 cancelRequest runs group request = do
  (_,status,_,_) <- historyTurn s (G.requestId request)
  if G.requestSettled request || G.requestStatus request `notElem` [Queued,Running]
    || status `elem` map Just [OutputPending,Done,Warning] || M.member (G.requestId request) runs
   then pure group
   else pure (G.replace "requests" (toJSON request { G.requestStatus=Cancelled, G.requestSettled=True
     , G.requestFields=KM.insert "error" (str "Routine deleted") (G.requestFields request) }) group)

stopDeletedForwarding :: Service -> ExecutionOrigin -> Maybe TaskId -> IO Bool
stopDeletedForwarding s origin task = case originRoutine origin <|> originPolicyRoutine origin of
 Nothing -> pure False
 Just identity -> DB.connection s $ \db -> SQL.withTransaction db $ do
  routine <- readRoutineOn db identity
  case routineDeletedAt routine of
   Nothing -> pure False
   Just time -> do
    forM_ (task >>= T.stripPrefix "gtask_" . taskIdText) $ \run -> do
     rows <- DB.queryRowsOn db "SELECT id,value,status FROM routine_occurrences WHERE id=?" ["id","value","status"] [SQL.SQLText run]
     forM_ rows $ \row -> do
      stored <- readStoredDue row
      case stored of
       Admitted occurrence _ -> do
        require (routineId (occurrenceRoutineSnapshot occurrence) == identity) 500 "Forwarded task belongs to another Routine"
        void (stopOccurrenceOn db time occurrence)
       _ -> failure 500 "Forwarded task has no admitted Run"
    pure True

stopRun :: Service -> Text -> RoutineId -> RunId -> IO Value
stopRun s actor identity run = mask_ $ do
 routine <- DB.connection s $ \db -> readRoutineOn db identity
 case routineOwner routine of
  PersonalOwner owner -> require (actor == "admin" || actor == agentIdText owner) 404 "Unknown Scheduled Routine in this conversation"
  GroupOwner group -> G.getGroup s (groupIdText group) False >>= \value -> G.authorizeGroup s value actor Management
 (occurrence,status) <- DB.connection s $ \db -> do
  rows <- DB.queryRowsOn db "SELECT id,value,status FROM routine_occurrences WHERE id=?"
   ["id","value","status"] [SQL.SQLText (runIdText run)]
  row <- maybe (failure 404 "Unknown Routine Run") pure (listToMaybe rows)
  stored <- readStoredDue row
  case stored of
   Admitted saved materialization -> do
    require (routineId (occurrenceRoutineSnapshot saved) == identity) 404 "Run belongs to another Routine"
    pure (saved,materialization)
   _ -> failure 404 "This deadline has no admitted Run"
 active <- occurrenceActive s occurrence status
 case (occurrenceStoppedAt occurrence,active) of
  (Nothing,False) -> pure (stopReceipt "finished")
  _ -> do
   time <- getCurrentTime
   stopped <- DB.connection s $ \db -> SQL.withTransaction db $ do
    saved <- stopOccurrenceOn db time occurrence
    when (isNothing (occurrenceStoppedAt occurrence)) $
     DB.recordOn s db [routineOwnerText (routineOwner routine)] (routineIdText identity) "routine.stopped"
      (obj [("run",toJSON run)]) actor Nothing
    pure saved
   materializeOccurrence s stopped
   calls <- occurrenceCalls s stopped
   runs <- readMVar (serviceRuns s)
   pure (stopReceipt (if any ((`M.member` runs) . snd) calls then "stopping" else "stopped"))
 where
 stopReceipt status = obj [("id",toJSON run),("routine",toJSON identity),("status",str status)]

resultRoutine :: Service -> Text -> RoutineId -> IO Routine
resultRoutine s actor identity = do
 routine <- DB.connection s $ \db -> readRoutineOn db identity
 case routineOwner routine of
  PersonalOwner owner -> require (actor == "admin" || actor == agentIdText owner) 404 "Unknown Scheduled Routine in this conversation"
  GroupOwner group -> G.getGroup s (groupIdText group) False >>= \value -> G.authorizeGroup s value actor Management
 pure routine

readRunResult :: Service -> Text -> RoutineId -> RunId -> IO Value
readRunResult s actor identity run = do
 routine <- resultRoutine s actor identity
 runDetail s (routineOwnerText (routineOwner routine)) identity run

rejectRunResult :: Service -> Text -> RoutineId -> RunId -> Value -> RejectionReason -> IO Value
rejectRunResult s actor identity run target reason = do
 routine <- resultRoutine s actor identity
 (occurrence,_) <- historyAdmission s routine run
 reviewer <- if actor == "admin" then pure AdminReviewer else SapiReviewer <$> parseValue 400 personalAgentP (str actor)
 time <- getCurrentTime
 let proposed=ResultRejection time reviewer reason
 rejection <- case occurrenceTarget occurrence of
  PersonalTarget _ -> do
   decision <- parseValue 400 (\value -> onlyFields ["decision"] value >> textFieldP "decision" 200 value) target
   D.rejectResult s run decision proposed
  GroupTarget groupId _ -> do
   (requestId,revision) <- parseValue 400 (\value -> onlyFields ["request","revision"] value >> (,) <$> parseJSON (get value "request") <*> parseJSON (get value "revision")) target
   group <- G.getGroup s (groupIdText groupId) False
   requests <- occurrenceRequests occurrence group
   request <- maybe (failure 404 "Result request belongs to another Run") pure (find ((==requestId) . G.requestId) requests)
   lineage <- G.requireRequestLineage request
   link <- maybe (failure 409 "Result request has no Work revision") pure (G.requestTask lineage)
   require (G.requestTaskRevision link == revision && G.requestSettled request) 409 "Result revision does not match its settled request"
   task <- GW.readTask group (taskIdText (G.requestTaskId link))
   (changed,saved) <- validated (GW.rejectResult proposed requestId revision task)
   when (changed /= task) $ void (G.saveGroup s (G.replace "tasks" (toJSON changed) group) actor "result rejected" target)
   pure (toJSON saved)
 pure (obj [("routine",toJSON identity),("run",toJSON run),("result",target),("rejection",rejection)])

data ProblemRef = ProblemRef { problemRun :: RunId, problemVersion :: Text } deriving (Eq, Show)
instance FromJSON ProblemRef where
 parseJSON = withObject "Routine problem" $ \o -> do
  onlyFields ["run","version"] (Object o)
  ProblemRef <$> o .: "run" <*> textFieldP "version" 128 (Object o)
instance ToJSON ProblemRef where
 toJSON problem = obj [("run",toJSON (problemRun problem)),("version",str (problemVersion problem))]

data ProblemReason = PhysicalFailure Text | UnresolvedOutcome Text | RejectedResult Text | StaleResult | MissingOutcome deriving (Eq, Show)
instance ToJSON ProblemReason where
 toJSON reason = obj [("kind",str kind),("message",str (T.take 240 message))]
  where
  (kind,message)=case reason of
   PhysicalFailure detail -> ("physical_failure",detail)
   UnresolvedOutcome detail -> ("unresolved",detail)
   RejectedResult detail -> ("rejected",detail)
   StaleResult -> ("stale","This result belongs to an earlier revision. Review the saved run.")
   MissingOutcome -> ("missing_outcome","No verifiable outcome was recorded. Review the saved run.")

data RoutineCondition = NoIssue | AttentionRequired ProblemRef ProblemReason | RepairPending AppliedRepair deriving (Eq, Show)
instance ToJSON RoutineCondition where
 toJSON NoIssue = obj [("kind",str "clear")]
 toJSON (AttentionRequired problem reason) = obj [("kind",str "attention"),("problem",toJSON problem),("reason",toJSON reason)]
 toJSON (RepairPending repair) = obj [("kind",str "repairing"),("repair",toJSON repair)]

runProblem :: RunSummary -> Maybe (ProblemRef,ProblemReason)
runProblem summary
 | not (runFinished summary) || isJust (occurrenceStoppedAt occurrence) = Nothing
 | otherwise = fmap (\reason -> (ProblemRef (occurrenceId occurrence) version,reason)) issue
 where
 occurrence=summaryOccurrence summary
 issue
  | summaryExecutionStatus summary `elem` map Just [Failed,Interrupted,Cancelled]
      || summaryCallState summary `elem` map Just [D.CallFailed,D.CallInterrupted,D.CallCancelled] || isJust (summaryError summary) =
     Just (PhysicalFailure (fromMaybe "Execution did not finish successfully. Review the saved run." (summaryError summary)))
  | Just rejection <- summaryRejection summary = Just (RejectedResult (at (toJSON rejection) "reason"))
  | summaryStale summary = Just StaleResult
  | Just outcome <- summaryOutcome summary = case D.outcomeStatus outcome of
     D.Completed -> Nothing
     D.Unresolved -> Just (UnresolvedOutcome (fromMaybe "The assignment could not be completed. Review the saved run." (summaryResult summary)))
  | otherwise = Just MissingOutcome
 version=runIdText (stableRunId ("routine-problem:" <> jsonText (obj
  [("run",toJSON (occurrenceId occurrence)),("result",toJSON (summaryResultTarget summary)),("outcome",toJSON (summaryOutcome summary))
  ,("rejection",toJSON (summaryRejection summary)),("stale",Bool (summaryStale summary)),("error",toJSON (summaryError summary))
  ,("execution",toJSON (summaryExecutionStatus summary)),("call",toJSON (summaryCallState summary)),("attempt",toJSON (summaryAttempt summary))])))

newtype RepairKey = RepairKey Text deriving (Eq, Show)
instance FromJSON RepairKey where parseJSON value = RepairKey <$> textFieldP "key" 128 (obj [("key",value)])
instance ToJSON RepairKey where toJSON (RepairKey value) = str value
newtype RepairDescription = RepairDescription Text deriving (Eq, Show)
instance FromJSON RepairDescription where parseJSON value = RepairDescription <$> textFieldP "description" 1000 (obj [("description",value)])
instance ToJSON RepairDescription where toJSON (RepairDescription value) = str value

data AppliedRepair = AppliedRepair
 { repairKey :: RepairKey, repairProblem :: ProblemRef, repairDescription :: RepairDescription
 , repairActor :: AgentId, repairConfirmedAt :: UTCTime, repairAdmissionFence :: RunId
 } deriving (Eq, Show)
instance FromJSON AppliedRepair where
 parseJSON = withObject "Applied Routine repair" $ \o -> do
  onlyFields ["key","problem","description","actor","confirmedAt","admissionFence"] (Object o)
  time <- o .: "confirmedAt" >>= maybe (fail "Invalid repair confirmation time") pure . parseTime
  AppliedRepair <$> o .: "key" <*> o .: "problem" <*> o .: "description" <*> (o .: "actor" >>= personalAgentP . str) <*> pure time <*> o .: "admissionFence"
instance ToJSON AppliedRepair where
 toJSON repair = obj [("key",toJSON (repairKey repair)),("problem",toJSON (repairProblem repair)),("description",toJSON (repairDescription repair))
  ,("actor",toJSON (repairActor repair)),("confirmedAt",str (timestamp (repairConfirmedAt repair))),("admissionFence",toJSON (repairAdmissionFence repair))]

data RepairVerification = AwaitingVerification | VerificationRunning RunId | VerificationPassed RunId | VerificationFailed RunId deriving (Eq, Show)
instance ToJSON RepairVerification where
 toJSON verification = case verification of
  AwaitingVerification -> obj [("kind",str "pending")]
  VerificationRunning run -> entry "running" run
  VerificationPassed run -> entry "passed" run
  VerificationFailed run -> entry "failed" run
  where entry kind run = obj [("kind",str kind),("run",toJSON run)]

repairVerification :: [RunSummary] -> AppliedRepair -> Validation RepairVerification
repairVerification summaries repair = do
 fence <- maybe (Left (APIError 500 "Repair admission fence is missing")) Right
  (find ((==repairAdmissionFence repair) . occurrenceId . summaryOccurrence) summaries)
 let following=sortOn (admissionOrder . summaryOccurrence)
      [summary | summary <- summaries, admissionOrder (summaryOccurrence summary) > admissionOrder (summaryOccurrence fence)
       , isNothing (occurrenceStoppedAt (summaryOccurrence summary))]
 pure $ case following of
  [] -> AwaitingVerification
  summary:_ -> let run=occurrenceId (summaryOccurrence summary) in
   if not (runFinished summary) then VerificationRunning run else
    if isJust (runProblem summary) then VerificationFailed run else VerificationPassed run

latestProblem :: [RunSummary] -> Maybe (ProblemRef,ProblemReason)
latestProblem summaries = find (\summary -> runFinished summary && isNothing (occurrenceStoppedAt (summaryOccurrence summary)))
 (sortOn (Down . admissionOrder . summaryOccurrence) summaries) >>= runProblem

routineCondition :: [RunSummary] -> Validation RoutineCondition
routineCondition summaries = do
 let repairs=concatMap (occurrenceRepairs . summaryOccurrence) summaries
     latest=listToMaybe (sortOn (Down . (\repair -> (repairConfirmedAt repair,jsonText (toJSON (repairKey repair))))) repairs)
     base=maybe NoIssue (uncurry AttentionRequired) (latestProblem summaries)
 case latest of
  Nothing -> pure base
  Just repair -> do
   verification <- repairVerification summaries repair
   pure $ case verification of
    AwaitingVerification -> pendingCondition repair base
    VerificationRunning _ -> pendingCondition repair base
    VerificationPassed _ -> base
    VerificationFailed _ -> base
 where
 pendingCondition repair condition = case condition of
  AttentionRequired problem _ | problem /= repairProblem repair -> condition
  _ -> RepairPending repair

repairHistory :: [RunSummary] -> ScheduledOccurrence -> Validation [Value]
repairHistory summaries occurrence = forM (occurrenceRepairs occurrence) $ \repair -> do
 verification <- repairVerification summaries repair
 pure (merge (toJSON repair) (obj [("verification",toJSON verification)]))

-- A single grouped read serves list/history projection and the transactional repair guard.
-- No provider traces or per-run HTTP reads are involved.
data SummaryContext = SummaryContext
 { contextWorks :: M.Map RunId D.Workload, contextGroups :: M.Map GroupId Value
 , contextTurns :: M.Map RunId Value, contextLive :: S.Set RunId }

summaryRowsOn :: SQL.Connection -> Text -> [Text] -> [Text] -> IO [Value]
summaryRowsOn _ _ _ [] = pure []
summaryRowsOn db query columns identities = do
 let (batch,remaining)=splitAt 128 identities
     placeholders=T.intercalate "," (replicate (length batch) "?")
 rows <- DB.queryRowsOn db (query <> " WHERE id IN (" <> placeholders <> ")") columns (map SQL.SQLText batch)
 (rows++) <$> summaryRowsOn db query columns remaining

readSummaryContextOn :: Service -> SQL.Connection -> [(ScheduledOccurrence,MaterializationStatus)] -> IO SummaryContext
readSummaryContextOn s db admitted = do
 let personal=nub [occurrenceId occurrence | (occurrence,_) <- admitted, PersonalTarget _ <- [occurrenceTarget occurrence]]
     groups=nub [group | (occurrence,_) <- admitted, GroupTarget group _ <- [occurrenceTarget occurrence]]
 workRows <- summaryRowsOn db "SELECT id,value FROM workloads" ["id","value"] (map runIdText personal)
 works <- forM workRows $ \row -> do
  work <- DB.decodeStored (at row "value")
  pure (RunId (at row "id"),work)
 groupRows <- summaryRowsOn db "SELECT id,value FROM groups" ["id","value"] (map groupIdText groups)
 savedGroups <- forM groupRows $ \row -> do
  group <- DB.decodeStored (at row "value") :: IO G.GroupRecord
  pure (G.groupIdentity group,toJSON group)
 requests <- concat <$> traverse (G.readRequests . snd) savedGroups
 let admissions=S.fromList (map (occurrenceId . fst) admitted)
 related <- filterM (\request -> do
  lineage <- G.requireRequestLineage request
  pure (maybe False (`S.member` admissions) (groupAdmission lineage))) requests
 let calls=nub (personal ++ concatMap (map D.callIdentity . NE.toList . D.workCalls . snd) works ++ map G.requestId related)
 turns <- summaryRowsOn db "SELECT id,agent,status,output,error FROM turns" ["id","agent","status","output","error"] (map runIdText calls)
 live <- M.keysSet <$> readMVar (serviceRuns s)
 pure (SummaryContext (M.fromList works) (M.fromList savedGroups) (M.fromList [(RunId (at row "id"),row) | row <- turns]) live)

summaryTurn :: SummaryContext -> RunId -> IO (Maybe AgentId,Maybe TurnStatus,Maybe Text,Maybe Text)
summaryTurn context identity = case M.lookup identity (contextTurns context) of
 Nothing -> pure (Nothing,Nothing,Nothing,Nothing)
 Just row -> (,,,) <$> (Just <$> parseValue 500 personalAgentP (get row "agent"))
   <*> (Just <$> field 500 row "status") <*> field 500 row "output" <*> field 500 row "error"

data RunSummary = RunSummary
 { summaryOccurrence :: ScheduledOccurrence, summaryMaterialization :: MaterializationStatus
 , summaryCallState :: Maybe D.CallState, summaryExecutionStatus :: Maybe TurnStatus
 , summaryExecutor :: Maybe AgentId, summaryLive :: Bool, summaryResult :: Maybe Text
 , summaryError :: Maybe Text, summaryStale :: Bool, summaryAttempt :: Maybe Int
 , summaryTaskRevision :: Maybe Int, summaryGroupSettled :: Maybe Bool
 , summaryOutcome :: Maybe D.Outcome, summaryRejection :: Maybe ResultRejection, summaryResultTarget :: Maybe Value
 , summaryQuiet :: Bool
 }

runFinished :: RunSummary -> Bool
runFinished summary
 | summaryLive summary = False
 | isJust (occurrenceStoppedAt (summaryOccurrence summary)) = True
 | summaryExecutionStatus summary `elem` [Just Queued,Just Running] = False
 | summaryCallState summary `elem` map Just [D.CallQueued,D.CallRunning,D.CallDelegated,D.CallWaiting] = False
 | summaryGroupSettled summary == Just False && summaryExecutionStatus summary `elem` map Just [OutputPending,Done,Warning] = False
 | isJust (summaryExecutionStatus summary) = True
 | otherwise = summaryCallState summary `elem` map Just [D.CallCompleted,D.CallUnresolved,D.CallFailed,D.CallInterrupted,D.CallCancelled]

instance ToJSON RunSummary where
 toJSON summary = obj
  [("id",toJSON (occurrenceId occurrence)),("routine",toJSON (routineId routine))
  ,("created",str (timestamp (occurrenceAdmittedAt occurrence))),("trigger",toJSON (occurrenceTrigger occurrence))
  ,("due",case occurrenceTrigger occurrence of ScheduledTrigger -> toJSON (routineDeadline routine); ManualTrigger _ -> Null)
  ,("pending",Bool (summaryMaterialization summary == MaterializationPending))
  ,("stoppedAt",maybe Null (str . timestamp) (occurrenceStoppedAt occurrence))
  ,("callState",toJSON (summaryCallState summary)),("executionStatus",toJSON (summaryExecutionStatus summary))
  ,("executor",toJSON (summaryExecutor summary)),("live",Bool (summaryLive summary)),("finished",Bool (runFinished summary))
  ,("result",toJSON (T.take 240 <$> summaryResult summary)),("error",toJSON (T.take 240 <$> summaryError summary))
  ,("stale",Bool (summaryStale summary)),("attempt",toJSON (summaryAttempt summary)),("taskRevision",toJSON (summaryTaskRevision summary)),("groupSettled",toJSON (summaryGroupSettled summary))]
  `merge` obj [("outcome",toJSON (summaryOutcome summary)),("rejection",toJSON (summaryRejection summary)),("resultTarget",toJSON (summaryResultTarget summary)),("notification",str (if summaryQuiet summary then "quiet" else "notify"))
  ,("problem",toJSON (fst <$> runProblem summary)),("reason",maybe Null (toJSON . snd) (runProblem summary))]
  where occurrence=summaryOccurrence summary; routine=occurrenceRoutineSnapshot occurrence

historyTurn :: Service -> RunId -> IO (Maybe AgentId,Maybe TurnStatus,Maybe Text,Maybe Text)
historyTurn s identity = do
 rows <- DB.queryRows s "SELECT agent,status,output,error FROM turns WHERE id=?"
  ["agent","status","output","error"] [SQL.SQLText (runIdText identity)]
 case rows of
  [] -> pure (Nothing,Nothing,Nothing,Nothing)
  row:_ -> (,,,) <$> (Just <$> parseValue 500 personalAgentP (get row "agent"))
   <*> (Just <$> field 500 row "status") <*> field 500 row "output" <*> field 500 row "error"

-- Only decode business results needed for the visible condition/current/latest.
-- Older admissions remain cheap metadata for the stable repair verification fence.
readRoutineProjection :: Service -> SummaryContext -> [(ScheduledOccurrence,MaterializationStatus)] -> IO [RunSummary]
readRoutineProjection s context admitted = do
 let ordered=sortOn (Down . admissionOrder . fst) admitted
 recent <- recentFinished False ordered
 active <- filterM possiblyActive admitted
 let repairs=concatMap (occurrenceRepairs . fst) admitted
     latestRepair=listToMaybe (sortOn (Down . (\repair -> (repairConfirmedAt repair,jsonText (toJSON (repairKey repair))))) repairs)
 links <- repairEntries admitted (maybeToList latestRepair)
 let known=S.fromList (map (occurrenceId . summaryOccurrence) recent)
     remaining=nub (active ++ links)
 extra <- traverse (readRunSummaryWith s context) (filter ((`S.notMember` known) . occurrenceId . fst) remaining)
 pure (sortOn (Down . admissionOrder . summaryOccurrence) (recent ++ extra))
 where
 recentFinished _ [] = pure []
 recentFinished foundTerminal (entry:rest)
  | foundTerminal && isJust (occurrenceStoppedAt (fst entry)) = recentFinished True rest
  | otherwise = do
     summary <- readRunSummaryWith s context entry
     if runFinished summary && isNothing (occurrenceStoppedAt (fst entry)) then pure [summary]
      else (summary:) <$> recentFinished (foundTerminal || runFinished summary) rest
 possiblyActive (occurrence,status) = do
  let stopped=isJust (occurrenceStoppedAt occurrence)
  case occurrenceTarget occurrence of
   PersonalTarget _ -> do
    let work=M.lookup (occurrenceId occurrence) (contextWorks context)
        calls=maybe [occurrenceId occurrence] (map D.callIdentity . NE.toList . D.workCalls) work
        live=any (`S.member` contextLive context) calls
        frontier=NE.last . D.workCalls <$> work
        callActive=maybe False ((`elem` [D.CallQueued,D.CallRunning,D.CallDelegated,D.CallWaiting]) . D.callState) frontier
    statuses <- forM calls $ \call -> case M.lookup call (contextTurns context) of
     Nothing -> pure Nothing
     Just turn -> Just <$> field 500 turn "status"
    pure (live || not stopped && (status == MaterializationPending || callActive || any (maybe False activeStatus) statuses))
   GroupTarget groupId _ -> do
    group <- maybe (failure 500 "Routine Group is unavailable") pure (M.lookup groupId (contextGroups context))
    requests <- occurrenceRequests occurrence group
    let live=any ((`S.member` contextLive context) . G.requestId) requests
        active request=not (G.requestSettled request) && G.requestStatus request `elem` [Queued,Running,OutputPending,Done,Warning]
    pure (live || not stopped && (status == MaterializationPending || any active requests))

repairEntries :: [(ScheduledOccurrence,MaterializationStatus)] -> [AppliedRepair] -> IO [(ScheduledOccurrence,MaterializationStatus)]
repairEntries admitted repairs = fmap (nub . concat) $ forM repairs $ \repair -> do
 fence <- maybe (failure 500 "Repair admission fence is missing") pure (find ((==repairAdmissionFence repair) . occurrenceId . fst) admitted)
 problem <- maybe (failure 500 "Repair problem Run is missing") pure (find ((==problemRun (repairProblem repair)) . occurrenceId . fst) admitted)
 let following=listToMaybe (sortOn (admissionOrder . fst)
      [entry | entry <- admitted, admissionOrder (fst entry) > admissionOrder (fst fence), isNothing (occurrenceStoppedAt (fst entry))])
 pure ([problem,fence] ++ maybeToList following)

readRunSummaryWith :: Service -> SummaryContext -> (ScheduledOccurrence,MaterializationStatus) -> IO RunSummary
readRunSummaryWith s context (occurrence,materialization) = do
 (call,execution,executor,result,problem,stale,attempt,revision,settled,identities,businessResult) <- case occurrenceTarget occurrence of
  PersonalTarget owner -> do
   let work=M.lookup (occurrenceId occurrence) (contextWorks context)
   forM_ work $ \saved -> do
    origin <- requireExecutionOrigin (D.workOrigin saved)
    require (originRoutine origin == Just (routineId (occurrenceRoutineSnapshot occurrence))) 500 "Routine Run workload has inconsistent provenance"
   let frontier=NE.last . D.workCalls <$> work
       current=maybe (occurrenceId occurrence) D.callIdentity frontier
       recipient=maybe owner D.callRecipient frontier
       state=D.callState <$> frontier
   (actual,status,output,err) <- summaryTurn context current
   require (materialization == MaterializationPending || isJust status || state == Just D.CallQueued || isJust (occurrenceStoppedAt occurrence))
    500 "Materialized Routine Run has no persisted execution"
   callError <- maybe (pure Nothing) (\saved -> field 500 (toJSON saved) "error") frontier
   number <- maybe (pure Nothing) (\saved -> field 500 (toJSON saved) "attempt") frontier
   results <- maybe (pure []) (validated . D.resultHistory) work
   let business=listToMaybe (reverse (filter ((==toJSON current) . (`get` "call")) results))
   pure (state,status,actual <|> (recipient <$ frontier),(frontier >>= D.callOutput) <|> output,err <|> callError,False,number,Nothing,Nothing
    ,maybe [current] (map D.callIdentity . NE.toList . D.workCalls) work,business)
  GroupTarget groupId _ -> do
   group <- maybe (failure 500 "Routine Group is unavailable") pure (M.lookup groupId (contextGroups context))
   requests <- occurrenceRequests occurrence group
   ranked <- forM requests $ \request -> do
    lineage <- G.requireRequestLineage request
    link <- maybe (failure 500 "Routine Group request has no Task revision") pure (G.requestTask lineage)
    created <- field 500 (toJSON request) "created" >>= maybe (failure 500 "Invalid Group request time") pure . parseTime
    pure ((G.requestTaskRevision link,created,G.requestId request),(request,link))
   let current=snd <$> listToMaybe (sortOn (Down . fst) ranked)
   require (isJust current || materialization == MaterializationPending || isJust (occurrenceStoppedAt occurrence))
    500 "Materialized Routine Run has no Group request"
   case current of
    Nothing -> pure (Nothing,Nothing,Nothing,Nothing,Nothing,False,Nothing,Nothing,Nothing,[],Nothing)
    Just (request,link) -> do
     (actual,status,output,err) <- summaryTurn context (G.requestId request)
     task <- GW.readTask group (taskIdText (G.requestTaskId link))
     let results=items (toJSON task) "results"
         matches value=get value "turn" == toJSON (G.requestId request) && get value "revision" == toJSON (G.requestTaskRevision link)
         recorded=listToMaybe (reverse (filter matches results))
     savedText <- maybe (pure Nothing) (\value -> field 500 value "text") recorded
     savedError <- maybe (pure Nothing) (\value -> field 500 value "error") recorded
     earlier <- maybe (pure False) (\value -> fromMaybe False <$> (field 500 value "stale" :: IO (Maybe Bool))) recorded
     resultsView <- validated (GW.resultHistory task)
     let business=listToMaybe (reverse (filter (\value -> get (get value "result") "request" == toJSON (G.requestId request)
           && get (get value "result") "revision" == toJSON (G.requestTaskRevision link)) resultsView))
     let execution=fromMaybe (G.requestStatus request) status
     archive <- if execution == OutputPending && isNothing output && isNothing savedText then
      readJSON (archivePath s (agentIdText (G.requestTarget request)) ("runs/" <> runIdText (G.requestId request))) empty else pure empty
     archivedOutput <- field 500 archive "output"
     pure (Nothing,Just execution,actual <|> Just (G.requestTarget request),savedText <|> output <|> archivedOutput,err <|> savedError,earlier,Nothing
      ,Just (G.requestTaskRevision link),Just (G.requestSettled request),map G.requestId requests,business)

 outcome <- maybe (pure Nothing) (\value -> field 500 value "outcome") businessResult
 rejection <- maybe (pure Nothing) (\value -> field 500 value "rejection") businessResult
 quiet <- maybe (pure False) (parseValue 500 historyOnlyP) businessResult
 let live=any (`S.member` contextLive context) identities
     initial=RunSummary occurrence materialization call execution executor live Nothing problem stale attempt revision settled outcome rejection (get <$> businessResult <*> pure "result") quiet
     successful=execution `elem` map Just [OutputPending,Done,Warning]
      && call `notElem` map Just [D.CallQueued,D.CallRunning,D.CallDelegated,D.CallWaiting,D.CallFailed,D.CallInterrupted,D.CallCancelled]
      && isNothing (occurrenceStoppedAt occurrence) && runFinished initial
     stoppedError=Just "Stopped by request. Saved work and completed external actions are preserved."
 pure initial { summaryResult=if successful then result else Nothing
              , summaryError=if isJust (occurrenceStoppedAt occurrence) then stoppedError else problem }

historyRoutine :: Service -> Text -> RoutineId -> IO Routine
historyRoutine s rawOwner identity = do
 owner <- decodeValue 400 (str rawOwner)
 rows <- DB.queryRows s "SELECT value FROM scheduled_routines WHERE sequence=?" ["value"] [SQL.SQLInteger (routineIdSequence identity)]
 row <- maybe (failure 404 "Unknown Routine") pure (listToMaybe rows)
 routine <- DB.decodeStored (at row "value")
 require (routineId routine == identity && routineOwner routine == owner) 404 "Unknown Routine in this conversation"
 pure routine

readOccurrenceOn :: SQL.Connection -> RunId -> IO (ScheduledOccurrence,MaterializationStatus)
readOccurrenceOn db run = do
 rows <- DB.queryRowsOn db "SELECT id,value,status FROM routine_occurrences WHERE id=?" ["id","value","status"] [SQL.SQLText (runIdText run)]
 row <- maybe (failure 404 "Unknown Run of this Routine") pure (listToMaybe rows)
 stored <- readStoredDue row
 case stored of
  Admitted occurrence status -> pure (occurrence,status)
  _ -> failure 404 "This deadline has no admitted Run"

historyAdmission :: Service -> Routine -> RunId -> IO (ScheduledOccurrence,MaterializationStatus)
historyAdmission s routine run = DB.connection s $ \db -> do
 entry@(occurrence,_) <- readOccurrenceOn db run
 let snapshot=occurrenceRoutineSnapshot occurrence
 require (routineId snapshot == routineId routine) 404 "Unknown Run of this Routine"
 require (routineOwner snapshot == routineOwner routine) 500 "Routine Run owner does not match its Routine"
 pure entry

confirmRepair :: Service -> Text -> RoutineId -> Value -> Value -> Value -> IO Value
confirmRepair s actor identity rawProblem rawKey rawDescription = do
 actorId <- parseValue 400 personalAgentP (str actor)
 requireActive s actor
 chief <- DB.mainAgent s
 problem <- decodeValue 400 rawProblem
 key <- decodeValue 400 rawKey
 description <- decodeValue 400 rawDescription
 DB.connection s $ \db -> SQL.withTransaction db $ do
  routine <- readRoutineOn db identity
  validated (availableRoutine routine)
  stored <- readDuesOn db routine
  let admitted=[(occurrence,status) | Admitted occurrence status <- stored]
  context <- readSummaryContextOn s db admitted
  case routineOwner routine of
   PersonalOwner owner -> require (owner == actorId) 404 "Unknown Scheduled Routine in this conversation"
   GroupOwner groupId -> do
    group <- case M.lookup groupId (contextGroups context) of
     Just value -> pure value
     Nothing -> do
      rows <- DB.queryRowsOn db "SELECT value FROM groups WHERE id=?" ["value"] [SQL.SQLText (groupIdText groupId)]
      maybe (failure 404 "Unknown Group") (DB.decodeStored . (`at` "value")) (listToMaybe rows)
    validated (G.groupPermission chief group actor Management)
  let repairs=concatMap (occurrenceRepairs . fst) admitted
  case find ((==key) . repairKey) repairs of
   Just repair -> do
    require (repairProblem repair == problem && repairDescription repair == description) 409 "Repair key already identifies a different confirmation"
    pure (obj [("routine",toJSON identity),("repair",toJSON repair)])
   Nothing -> do
    summaries <- readRoutineProjection s context admitted
    require ((fst <$> latestProblem summaries) == Just problem) 409 "Routine problem changed; read its current problem before confirming a repair"
    entry <- maybe (failure 404 "Problem Run does not belong to this Routine") pure
     (find ((==problemRun problem) . occurrenceId . fst) admitted)
    fence <- maybe (failure 409 "Routine has no admitted Run") (pure . occurrenceId . fst)
     (listToMaybe (sortOn (Down . admissionOrder . fst) admitted))
    time <- getCurrentTime
    let repair=AppliedRepair key problem description actorId time fence
        occurrence=(fst entry) { occurrenceRepairs=occurrenceRepairs (fst entry) ++ [repair] }
    DB.executeOn db "UPDATE routine_occurrences SET value=? WHERE id=?"
     [SQL.SQLText (jsonText (toJSON occurrence)),SQL.SQLText (runIdText (occurrenceId occurrence))]
    DB.recordOn s db [routineOwnerText (routineOwner routine)] (routineIdText identity) "routine.repair_confirmed" (toJSON repair) actor Nothing
    pure (obj [("routine",toJSON identity),("repair",toJSON repair)])

historyAdmissions :: Service -> Text -> RoutineId -> IO (Routine,[(ScheduledOccurrence,MaterializationStatus)])
historyAdmissions s rawOwner identity = do
 routine <- historyRoutine s rawOwner identity
 stored <- DB.connection s (\db -> readDuesOn db routine)
 let admitted=[(occurrence,status) | Admitted occurrence status <- stored]
 forM_ admitted $ \(occurrence,_) -> require (routineOwner (occurrenceRoutineSnapshot occurrence) == routineOwner routine)
  500 "Routine Run owner does not match its Routine"
 pure (routine,sortOn (Down . admissionOrder . fst) admitted)

admissionOrder :: ScheduledOccurrence -> (UTCTime,RunId)
admissionOrder occurrence = (occurrenceAdmittedAt occurrence,occurrenceId occurrence)

runHistory :: Service -> Text -> RoutineId -> Maybe RunId -> IO Value
runHistory s owner identity before = do
 (routine,admitted) <- historyAdmissions s owner identity
 cutoff <- forM before $ \cursor -> maybe (failure 404 "Unknown history cursor") (pure . admissionOrder . fst)
  (find ((==cursor) . occurrenceId . fst) admitted)
 context <- DB.connection s (\db -> readSummaryContextOn s db admitted)
 projection <- readRoutineProjection s context admitted
 condition <- validated (routineCondition projection)
 let eligible=filter (\entry -> maybe True (admissionOrder (fst entry) <) cutoff) admitted
     current=find (not . runFinished) projection
     latest=find runFinished projection
 page <- forM (take 20 eligible) $ \entry -> case find ((==occurrenceId (fst entry)) . occurrenceId . summaryOccurrence) projection of
  Just summary -> pure summary
  Nothing -> readRunSummaryWith s context entry
 pure (obj [("routine",toJSON routine),("runs",toJSON page),("current",toJSON current),("latest",toJSON latest),("condition",toJSON condition)
  ,("next",if length eligible>20 then maybe Null (toJSON . occurrenceId . summaryOccurrence) (listToMaybe (reverse page)) else Null)])

runDetail :: Service -> Text -> RoutineId -> RunId -> IO Value
runDetail s owner identity run = do
 routine <- historyRoutine s owner identity
 entry <- historyAdmission s routine run
 admitted <- if null (occurrenceRepairs (fst entry)) then pure [entry] else snd <$> historyAdmissions s owner identity
 links <- repairEntries admitted (occurrenceRepairs (fst entry))
 let selected=nub (entry:links)
 context <- DB.connection s (\db -> readSummaryContextOn s db selected)
 summaries <- traverse (readRunSummaryWith s context) selected
 summary <- maybe (failure 404 "Unknown Run of this Routine") pure (find ((==run) . occurrenceId . summaryOccurrence) summaries)
 repairs <- validated (repairHistory summaries (summaryOccurrence summary))
 references <- case occurrenceTarget (fst entry) of
  PersonalTarget _ -> do
   work <- D.findWork s run
   forM [decision | (saved,_) <- maybeToList work,decision <- D.workItems saved "decisions",has decision "providerTrace"] $ \decision -> do
    reference <- decodeValue 500 (get decision "providerTrace")
    owner <- parseValue 500 personalAgentP (get decision "agent")
    call <- field 500 decision "callId"
    require (P.traceBelongsTo owner call reference) 500 "Provider trace does not belong to this decision"
    pure reference
  GroupTarget groupId _ -> do
   group <- G.getGroup s (groupIdText groupId) False
   requests <- occurrenceRequests (fst entry) group
   fmap concat $ forM requests $ \request -> do
    references <- decodeValue 500 (get (Object (G.requestFields request)) "providerTraces" `fallback` arr [])
    require (all (P.traceBelongsTo (G.requestTarget request) (G.requestId request)) references) 500 "Provider trace does not belong to this Group request"
    pure references
 traces <- mapM (P.readProviderTrace s) references
 results <- case occurrenceTarget (fst entry) of
  PersonalTarget _ -> D.findWork s run >>= maybe (pure []) (validated . D.resultHistory . fst)
  GroupTarget groupId _ -> do
   group <- G.getGroup s (groupIdText groupId) False
   requests <- occurrenceRequests (fst entry) group
   task <- traverse (decodeValue 500) (findId ("gtask_" <> runIdText run) (items group "tasks"))
   rows <- maybe (pure []) (validated . GW.resultHistory) task
   pure (filter (\value -> get (get value "result") "request" `elem` map (toJSON . G.requestId) requests) rows)
 pure (merge (toJSON summary) (obj [("snapshot",toJSON (occurrenceRoutineSnapshot (fst entry)))
  ,("result",toJSON (summaryResult summary)),("error",toJSON (summaryError summary)),("results",arr results),("providerTraces",arr traces),("repairs",arr repairs)]))

readRoutineOn :: SQL.Connection -> RoutineId -> IO Routine
readRoutineOn db identity = do
 rows <- DB.queryRowsOn db "SELECT value FROM scheduled_routines WHERE sequence=?" ["value"] [SQL.SQLInteger (routineIdSequence identity)]
 case rows of
  row:_ -> do
   routine <- DB.decodeStored (at row "value")
   require (routineId routine == identity) 409 "Stored routine identity does not match the requested routine"
   pure routine
  [] -> failure 409 "Routine disappeared before admission"

admitDue :: Service -> UTCTime -> Routine -> ScheduledTarget -> IO (Maybe ScheduledOccurrence)
admitDue s time candidate target = do
 DB.connection s $ \db -> SQL.withTransaction db $ do
  routine <- readRoutineOn db (routineId candidate)
  if isJust (routineDeletedAt routine) || routinePaused routine || routineAllowanceUsed routine >= 1000 then pure Nothing else do
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
     ResolveDues resolution -> do
      let admitted=dueToAdmit resolution; closed=rangesToClose resolution; remaining=deferredRemainders resolution
      forM_ closed $ \(due,reason) -> do
       DB.executeOn db "UPDATE routine_occurrences SET value=?,status='closed' WHERE id=? AND status='deferred'"
        [SQL.SQLText (jsonText (merge (toJSON due) (obj [("closure",toJSON reason)]))),SQL.SQLText (dueIdText (dueId (spanFirst due)))]
       changed <- SQL.changes db
       require (changed == 1) 409 "Deferred range changed before closure"
      -- Keep per-deadline closures in storage and emit one batch event.
      case closed of
       [] -> pure ()
       (first,reason):rest -> do
        let spans=first:map fst rest
            firstTime=foldr (min . routineDue . dueRoutineSnapshot . spanFirst . fst) (routineDue (dueRoutineSnapshot (spanFirst first))) rest
            lastTime=foldr (max . spanThrough . fst) (spanThrough first) rest
        DB.recordOn s db [routineOwnerText (routineOwner routine)] (routineIdText (routineId routine)) "routine.dues_closed"
         (obj [("count",toJSON (sum (map spanCount spans))),("reason",toJSON reason)
         ,("earliest",str (timestamp firstTime))
         ,("latest",str (timestamp lastTime))]) "system" Nothing
      forM_ remaining $ \(range,reason) -> insertDeferredOn db reason range
      forM admitted $ \due -> do
       occurrence <- validated (prepareOccurrence time routine target due)
       let snapshot=occurrenceRoutineSnapshot occurrence
       DB.executeOn db "INSERT INTO routine_occurrences(id,value,status) VALUES (?,?,'admitted_pending') ON CONFLICT(id) DO UPDATE SET value=excluded.value,status=excluded.status WHERE routine_occurrences.status='deferred'"
        [SQL.SQLText (runIdText (occurrenceId occurrence)),SQL.SQLText (jsonText (toJSON occurrence))]
       changed <- SQL.changes db
       require (changed == 1) 409 "Due already resolved before admission"
       DB.recordOn s db [routineOwnerText (routineOwner routine)] (routineIdText (routineId routine)) "routine.triggered"
        (obj [("turn",toJSON (occurrenceId occurrence)),("runCount",num (routineAllowanceUsed routine+1))
        ,("due",get (toJSON snapshot) "nextDue"),("nextDue",str (timestamp (routineDue advanced)))]) "system" Nothing
       pure occurrence
    let count=routineAllowanceUsed routine + if isJust result then 1 else 0
        updated=advanced { routineAllowanceUsed=count, routinePaused=count>=1000 }
        stillDeferred=case plan of
         KeepDues -> newDues
         ResolveDues resolution ->
          let admitted=dueToAdmit resolution; closed=rangesToClose resolution; remaining=deferredRemainders resolution
              resolved=map (dueId . spanFirst . fst) closed ++ maybeToList (dueId <$> admitted)
              untouched=[range | range <- newDues, dueId (spanFirst range) `notElem` resolved]
              tails=[remainder | (remainder,_) <- remaining, captured <- newDues, spanThrough captured == spanThrough remainder, dueObservedAt (spanFirst captured) == dueObservedAt (spanFirst remainder)]
          in untouched++tails
    recordDeferralOn s db routine waitingReason stillDeferred
    when (updated /= routine) (void (DB.saveRoutineOn db (toJSON updated)))
    when (routinePaused updated) $ DB.recordOn s db [routineOwnerText (routineOwner routine)] (routineIdText (routineId routine)) "routine.paused"
     (obj [("reason",str "run limit"),("runCount",num count)]) "system" Nothing
    pure result

insertDeferredOn :: SQL.Connection -> DeferredReason -> DueSpan -> IO ()
insertDeferredOn db reason due =
 DB.executeOn db "INSERT INTO routine_occurrences(id,value,status) VALUES (?,?,'deferred')"
  [SQL.SQLText (dueIdText (dueId (spanFirst due))),SQL.SQLText (jsonText (merge (toJSON due) (obj [("reason",toJSON reason)])))]

recordDeferralOn :: Service -> SQL.Connection -> Routine -> DeferredReason -> [DueSpan] -> IO ()
recordDeferralOn s db routine reason dues = case dues of
 [] -> pure ()
 first:rest -> DB.recordOn s db [routineOwnerText (routineOwner routine)] (routineIdText (routineId routine)) "routine.deferred"
  (obj [("due",get (toJSON (dueRoutineSnapshot (spanFirst first))) "nextDue"),("through",str (timestamp (foldr (max . spanThrough) (spanThrough first) rest))),("count",toJSON (sum (map spanCount (first:rest)))),("reason",toJSON reason)]) "system" Nothing

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

policyPreparationId :: Routine -> RunId
policyPreparationId routine = stableRunId ("routine-policy:" <> routineIdText (routineId routine) <> ":" <> routinePrompt routine)

data PolicyPreparationError = PolicyNotSaved | PolicyInstructionsChanged | PolicyOwnerChanged

policyPreparationCompletion :: RoutineOwner -> RunId -> Routine -> Either PolicyPreparationError ()
policyPreparationCompletion owner identity routine
 | routineOwner routine /= owner = Left PolicyOwnerChanged
 | PolicySelected _ <- routinePolicy routine = Right ()
 | identity /= policyPreparationId routine = Left PolicyInstructionsChanged
 | otherwise = Left PolicyNotSaved

verifyPolicyPreparationCompletion :: Service -> RoutineOwner -> RunId -> ExecutionOrigin -> IO ()
verifyPolicyPreparationCompletion s owner identity parsed =
 forM_ (originPolicyRoutine parsed) $ \routineIdentity -> do
  routine <- DB.connection s $ \db -> readRoutineOn db routineIdentity
  validated (availableRoutine routine)
  require (routineId routine == routineIdentity) 409 "Stored routine identity does not match policy setup"
  generation <- case originGroup parsed of
   Just _ -> do
    group <- G.getGroup s (routineOwnerText owner) False
    requests <- G.readRequests group
    request <- maybe (failure 409 "Policy setup request is missing") pure
     (find ((==identity) . G.requestId) requests)
    require (toJSON (G.requestOrigin request) == toJSON parsed)
     409 "Policy setup request origin changed"
    RunId . messageIdText . G.requestRoot <$> G.requireRequestLineage request
   Nothing -> pure identity
  either reject pure (policyPreparationCompletion owner generation routine)
 where
 reject problem = failure 409 $ case problem of
  PolicyNotSaved -> "Routine policy was not saved. Retry this setup or ask the owner to finish routine setup in chat."
  PolicyInstructionsChanged -> "Routine instructions changed before policy setup completed. Ask the owner to finish the current routine setup in chat."
  PolicyOwnerChanged -> "Routine owner changed before policy setup completed. Ask the current owner to finish routine setup in chat."

data GroupPolicyPreparationPlan = KeepGroupPolicyPreparation | StartGroupPolicyPreparation (Maybe G.GroupRequest)

-- Use the projected Turn when Request status lags behind execution.
planGroupPolicyPreparation :: AgentId -> Maybe (G.GroupRequest,Maybe TurnStatus) -> GroupPolicyPreparationPlan
planGroupPolicyPreparation _ Nothing = StartGroupPolicyPreparation Nothing
planGroupPolicyPreparation lead (Just (request,projected))
 | G.requestTarget request == lead || status == Running = KeepGroupPolicyPreparation
 | otherwise = StartGroupPolicyPreparation (Just request { G.requestStatus=status })
 where status=fromMaybe (G.requestStatus request) projected

ensurePolicyPreparation :: Service -> Routine -> IO ()
ensurePolicyPreparation s routine = unless (isJust (routineDeletedAt routine)) $ case routinePolicy routine of
 PolicySelected _ -> pure ()
 AwaitingSelection -> do
  destination <- scheduledTarget s routine
  forM_ destination $ \target -> do
   template <- prompt s "routine-policy"
   let identity=policyPreparationId routine
       created=at (toJSON routine) "created"
       input=render template [("routine",jsonText (toJSON routine))]
       origin=newOrigin (RoutinePolicySetup (routineId routine)) Nothing (KM.singleton "author" (str "system"))
       turn=newTurn identity Chat input created (toJSON origin) False
   case target of
    PersonalTarget agent -> do
     state <- agentState s (agentIdText agent)
     archived <- DB.queryRows s "SELECT id FROM turns WHERE id=? UNION SELECT id FROM workloads WHERE id=?"
      ["id"] [SQL.SQLText (runIdText identity),SQL.SQLText (runIdText identity)]
     when (any ((==identity) . turnId) (agentTurns state) || null archived) $
      materializePersonal s agent turn Nothing
    GroupTarget group agent -> do
     value <- G.getGroup s (groupIdText group) True
     requests <- G.readRequests value
     sameRoot <- validated (G.requestsForRoot (MessageId (runIdText identity)) requests)
     related <- forM sameRoot $ \request -> do
      parsed <- requireExecutionOrigin (G.requestOrigin request)
      pure (request,parsed)
     let latestSetup=listToMaybe (reverse [request | (request,parsed) <- related, originPolicyRoutine parsed == Just (routineId routine)])
     projected <- forM latestSetup $ \request -> do
      rows <- DB.queryRows s "SELECT status FROM turns WHERE id=?" ["status"]
       [SQL.SQLText (runIdText (G.requestId request))]
      status <- traverse (\row -> field 500 row "status") (listToMaybe rows)
      pure (request,status)
     case planGroupPolicyPreparation agent projected of
      KeepGroupPolicyPreparation -> pure ()
      StartGroupPolicyPreparation predecessor -> do
       let next=maybe identity (\previous -> stableRunId
            (runIdText (G.requestId previous) <> ":lead:" <> agentIdText agent)) predecessor
       require (all ((/=next) . G.requestId) requests) 409 "Policy setup request identity is already occupied; repair its saved lineage before retrying"
       -- Persist cancellation before the successor so recovery cannot run both.
       forM_ predecessor $ \previous -> when (G.requestStatus previous == Queued) $ do
        let oldOwner=agentIdText (G.requestTarget previous)
        modifyTurns s oldOwner $ \saved -> if turnId saved == G.requestId previous
         then saved { turnStatus=Cancelled } else saved
        DB.project s oldOwner
       time <- now
       let requestOrigin=newOrigin (originPurpose origin) (Just group) (originMetadata origin)
           entry=obj [("id",toJSON identity),("author",str "system"),("text",str input),("created",str created)]
           request=G.newGroupRequest next agent (ValidOrigin requestOrigin)
            (G.RequestLineage (MessageId (runIdText identity)) (MessageId (runIdText identity)) Nothing) time
           superseded previous = toJSON $ if G.requestStatus previous == Queued
            then previous { G.requestStatus=Cancelled } else previous
       event <- forM predecessor $ \previous -> G.eventRecord "system" "policy setup reassigned"
        (obj [("previous",toJSON (G.requestId previous)),("request",toJSON next),("lead",toJSON agent)])
       DB.connection s $ \db -> SQL.withTransaction db $ do
        validateTargetOn db target
        let retained=maybe value (\previous -> G.replace "requests" (superseded previous) value) predecessor
            messages=items value "messages"++[entry | isNothing predecessor]
        DB.putValueOn db DB.Groups (groupIdText group) (merge retained (obj
         [("messages",arr messages),("requests",arr (items retained "requests"++[toJSON request]))
         ,("events",arr (items value "events"++maybeToList event))
         ,("revision",num (int (get value "revision")+1)),("updated",str time)]))

materializeOccurrence :: Service -> ScheduledOccurrence -> IO ()
materializeOccurrence s saved = do
 (original,_) <- DB.connection s (\db -> readOccurrenceOn db (occurrenceId saved))
 routine <- DB.connection s (\db -> readRoutineOn db (routineId (occurrenceRoutineSnapshot original)))
 occurrence <- case (routineDeletedAt routine,occurrenceStoppedAt original) of
  (Just time,Nothing) -> DB.connection s $ \db -> SQL.withTransaction db (stopOccurrenceOn db time original)
  _ -> pure original
 materialization <- case occurrenceStoppedAt occurrence of
  Just _ -> cancelOccurrence s occurrence >> pure Materialized
  Nothing -> case occurrenceTarget occurrence of
   PersonalTarget agent -> materializePersonal s agent (occurrenceTurn occurrence) specification >> pure Materialized
   GroupTarget group _ -> materializeGroup s group (occurrenceTurn occurrence) specification
 when (materialization == Materialized) $
  DB.executeSQL s "UPDATE routine_occurrences SET status='admitted_ready' WHERE id=? AND status='admitted_pending'"
   [SQL.SQLText (runIdText (occurrenceId occurrence))]
 where specification=routineSpecification (occurrenceRoutineSnapshot saved)

data PersonalMaterializationPlan = CreateTurn | KeepActiveTurn | KeepTerminalTurn | KeepRetainedHistory deriving (Eq)
data PersonalRecoveryError = MissingLiveActiveTurn

planPersonalMaterialization :: Maybe Turn -> Maybe TurnStatus -> Either PersonalRecoveryError PersonalMaterializationPlan
planPersonalMaterialization (Just turn) _ = Right $
 if activeStatus (turnStatus turn) then KeepActiveTurn else KeepTerminalTurn
planPersonalMaterialization Nothing Nothing = Right CreateTurn
planPersonalMaterialization Nothing (Just status) = case status of
 Queued -> Left MissingLiveActiveTurn
 Running -> Left MissingLiveActiveTurn
 OutputPending -> Left MissingLiveActiveTurn
 Done -> Right KeepRetainedHistory
 Warning -> Right KeepRetainedHistory
 Failed -> Right KeepRetainedHistory
 Interrupted -> Right KeepRetainedHistory
 Cancelled -> Right KeepRetainedHistory

materializePersonal :: Service -> AgentId -> Turn -> Maybe D.TaskSpec -> IO ()
materializePersonal s agent turn specification = do
 let owner=agentIdText agent
     identity=runIdText (turnId turn)
     created=get (toJSON turn) "created"
     origin=turnField turn "origin"
 existingState <- agentState s owner
 let existing=find ((==turnId turn) . turnId) (agentTurns existingState)
 forM_ existing $ \saved ->
  require (turnField saved "origin" == origin) 409 "Occurrence ID belongs to another Turn"
 history <- listToMaybe <$> DB.queryRows s
  "SELECT turns.agent,turns.status,turn_details.value FROM turns LEFT JOIN turn_details ON turn_details.id=turns.id WHERE turns.id=?"
  ["agent","status","details"] [SQL.SQLText identity]
 status <- traverse (\stored -> field 500 stored "status") history
 materialization <- either reject pure (planPersonalMaterialization existing status)
 case materialization of
  KeepActiveTurn -> pure ()
  KeepTerminalTurn -> pure ()
  KeepRetainedHistory -> forM_ history $ \stored -> do
   details <- DB.decodeStored (at stored "details")
   require (at stored "agent" == owner && get details "origin" == origin)
    409 "Occurrence ID belongs to another retained Turn"
  CreateTurn -> modifyState s owner $ \currentState -> merge currentState (obj
   [("turns",arr (items currentState "turns"++[toJSON turn]))
   ,("chat",arr (items currentState "chat"++[obj
     [("role",str "user"),("content",str (turnInput turn)),("turn",str identity)
     ,("time",created),("origin",origin)]]))])
 -- Retained terminal Runs must not be recreated or restarted.
 unless (materialization == KeepRetainedHistory) (DB.project s owner)
 when (materialization `elem` [CreateTurn,KeepActiveTurn]) $ do
  found <- D.findWork s (turnId turn)
  when (isNothing found) (D.begin s agent turn specification)
  (work,call) <- D.getWork s (turnId turn)
  when (D.callState call == D.CallRunning && null (D.workItems work "decisions")) $
   D.saveWork s (D.withCall work call { D.callProgress=D.QueuedAt (D.callNode call) })
 where
 reject MissingLiveActiveTurn = failure 409 "Routine recovery is blocked: active Turn history has no live state. Restore the Sapi state before continuing this Run."

data GroupMaterializationPlan = KeepExistingRequest | CreateRequestFor AgentId
data GroupRecoveryError = GroupArchived | LeadUnavailable

planGroupMaterialization :: G.GroupRecord -> Maybe G.GroupRequest -> Bool -> Either GroupRecoveryError GroupMaterializationPlan
planGroupMaterialization group existing leadActive
 | isJust existing = Right KeepExistingRequest
 | G.groupArchived group = Left GroupArchived
 | not leadActive = Left LeadUnavailable
 | otherwise = Right (CreateRequestFor (rosterLead (G.groupRoster group)))

groupRecoveryReason :: GroupRecoveryError -> Text
groupRecoveryReason problem = case problem of
 GroupArchived -> "Routine recovery is waiting: restore this archived Group to continue the same Run."
 LeadUnavailable -> "Routine recovery is waiting: choose an active Group Lead to continue the same Run."

-- Existing requests keep their assignee; new requests use the current Lead.
materializeGroup :: Service -> GroupId -> Turn -> Maybe D.TaskSpec -> IO MaterializationStatus
materializeGroup s groupId turn specification = DB.connection s $ \db -> SQL.withTransaction db $ do
 parsed <- requireExecutionOrigin (turnOrigin turn)
 routineIdentity <- maybe (failure 500 "Routine materialization requires scheduled provenance") pure (originRoutine parsed)
 let owner=groupIdText groupId
     identity=runIdText (turnId turn)
 rows <- DB.queryRowsOn db "SELECT value FROM groups WHERE id=?" ["value"] [SQL.SQLText owner]
 group <- case rows of
  row:_ -> DB.decodeStored (at row "value")
  [] -> failure 409 "Group disappeared before routine materialization"
 typed <- decodeValue 500 group
 requests <- G.readRequests group
 let existing=find ((==turnId turn) . G.requestId) requests
     lead=rosterLead (G.groupRoster typed)
 forM_ existing $ \request -> do
  lineage <- G.requireRequestLineage request
  previous <- requireExecutionOrigin (G.requestOrigin request)
  let initialMessage=MessageId identity
      initialTask=TaskId ("gtask_"<>identity)
  require (originRoutine previous == Just routineIdentity && G.requestMessage lineage == initialMessage
   && G.requestRoot lineage == initialMessage && fmap G.requestTaskId (G.requestTask lineage) == Just initialTask)
   409 "Occurrence ID belongs to another Group request"
 leadActive <- if isJust existing || G.groupArchived typed then pure False else do
  profiles <- DB.queryRowsOn db "SELECT id FROM agents WHERE id=?" ["id"] [SQL.SQLText (agentIdText lead)]
  inactive <- retired s (agentIdText lead)
  pure (not (null profiles) && not inactive)
 case planGroupMaterialization typed existing leadActive of
  Right KeepExistingRequest -> pure Materialized
  Left problem -> do
   let reason=groupRecoveryReason problem
       detail=obj [("run",toJSON (turnId turn)),("routine",toJSON routineIdentity),("reason",str reason)]
       action="routine recovery blocked"
       alreadyRecorded event = at event "action" == action && get event "detail" == detail
   unless (any alreadyRecorded (items group "events")) $ do
    event <- G.eventRecord "system" action detail
    DB.putValueOn db DB.Groups owner (merge (G.append "events" event group) (obj
     [("revision",num (G.groupRevision typed+1)),("updated",get event "created")]))
    DB.recordOn s db [owner] identity "routine.recovery_blocked" detail "system" Nothing
   pure MaterializationPending
  Right (CreateRequestFor agent) -> do
   created <- field 500 (toJSON turn) "created"
   title <- field 500 (Object (originMetadata parsed)) "title"
   createdTime <- maybe (failure 500 "Invalid scheduled occurrence creation time") pure (parseTime created)
   let body=turnInput turn
       taskId=TaskId ("gtask_"<>identity)
       messageId=MessageId identity
       groupOrigin=newOrigin (originPurpose parsed) (Just groupId) (KM.insert "author" (str "admin") (originMetadata parsed))
       message=obj [("id",toJSON messageId),("author",str "admin"),("text",str body),("created",str created),("routine",toJSON routineIdentity)]
       task=GW.newScheduledTask taskId createdTime title body agent routineIdentity specification
       request=GW.withResultProtocol specification (G.newGroupRequest (turnId turn) agent (ValidOrigin groupOrigin)
        (G.RequestLineage messageId messageId (Just (GW.taskLink task))) created)
   DB.putValueOn db DB.Groups owner (merge group (obj
    [("messages",arr (items group "messages"++[message])),("tasks",arr (items group "tasks"++[toJSON task]))
    ,("requests",arr (items group "requests"++[toJSON request]))
    ,("revision",num (G.groupRevision typed+1)),("updated",str created)]))
   pure Materialized

restoreOccurrences :: Service -> IO ()
restoreOccurrences s = do
 rows <- DB.queryRows s "SELECT id,value,status FROM routine_occurrences WHERE status='admitted_pending' ORDER BY rowid" ["id","value","status"] []
 forM_ rows $ \row -> tryRoutine ("scheduled occurrence " <> at row "id") $ do
  stored <- readStoredDue row
  case stored of
   Admitted occurrence MaterializationPending -> materializeOccurrence s occurrence
   _ -> failure 500 "Expected a pending scheduled occurrence"
