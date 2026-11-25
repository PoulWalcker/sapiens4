module Store where
import Control.Monad
import Data.Aeson hiding (pairs)
import Data.List (nub,sortOn)
import Data.Maybe
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import qualified Data.Text as T
import qualified AppStorage as A
import Types

data Collection = Workloads | Groups | ScheduledRoutines | PulseCalls | Preferences | SystemPulse deriving (Eq, Show)
prefix :: Collection -> Text
prefix = (<>"/") . T.pack . show
values :: Service -> Collection -> IO [Value]
values s c = map snd <$> A.values (serviceStore s) (prefix c)
putValue :: Service -> Collection -> Text -> Value -> IO ()
putValue s c i v = do
 old <- A.getValue (serviceStore s) (prefix c<>i)
 when (old/=Just v) (saveValue s (prefix c<>i) v >> notifyChange s)
singleton :: Service -> Collection -> IO Value
singleton s c = loadValue s (prefix c<>"singleton") empty
saveSingleton :: Service -> Collection -> Value -> IO ()
saveSingleton s c = putValue s c "singleton"
agents :: Service -> IO [Value]
agents s = sortOn (\v -> (at v "created",at v "id")) . map snd <$> A.values (serviceStore s) "agents/"
agentProfiles :: Service -> IO [AgentProfile]
agentProfiles s = agents s >>= traverse (decodeValue 500)
agent :: Service -> Text -> IO Value
agent s i = loadValue s ("agents/"<>i) Null >>= \v -> if v==Null then failure 404 "Unknown Sapi" else pure v
mainAgent :: Service -> IO Text
mainAgent s = population s >>= maybe (failure 500 "CORPORA has no Chief Sapi") (pure . agentIdText . populationChief)
population :: Service -> IO (Maybe Population)
population s = fmap populationRoster . NE.nonEmpty . map profileId <$> agentProfiles s
addAgent :: Service -> Value -> IO ()
addAgent s v = saveValue s ("agents/"<>at v "id") v
attachments :: Service -> Text -> IO [Value]
attachments s a = map snd <$> A.values (serviceStore s) ("attachments/"<>a<>"/")
saveAttachment, updateAttachment :: Service -> Value -> IO ()
saveAttachment s v = saveValue s ("attachments/"<>at v "agent"<>"/"<>at v "id") v
updateAttachment s v = saveAttachment s v >> notifyChange s
pendingVoices :: Service -> IO [Value]
pendingVoices s = take 1 . filter (\v -> at v "kind"=="voice" && at v "transcript_status"=="pending" && not (boolean (get v "cancelled"))) . map snd <$> A.recordsAt 2 (serviceStore s) "attachments/"
message :: Service -> Text -> Text -> [Value] -> IO ()
message s i t as = saveValue s ("messages/"<>i) (obj [("input",str t),("attachments",arr as)])
record :: Service -> [Text] -> Text -> Text -> Value -> Text -> Maybe Text -> IO ()
record s owners entity event details suppliedActor suppliedAgency = do
 (source,kind) <- currentActor s
 let actor=if suppliedActor=="admin" then source else suppliedActor
     agency=suppliedAgency `mplus` kind
 time <- now
 forM_ (nub (owners ++ [actor | "sapi_" `T.isPrefixOf` actor])) $ \owner -> do
  rows <- loadValue s ("events/"<>owner) []
  let next=1+maximum (0:map (int . (`get` "id")) rows)
      value=obj [("id",num next),("timestamp",str time),("actor",str actor),("entity",str entity),("event",str event),("agency",toJSON agency),("details",details)]
  saveValue s ("events/"<>owner) (take 100 (value:rows))
 notifyChange s
events :: Service -> IO Value
events s = obj . map (\(k,v) -> (T.drop 7 k,v)) <$> A.values (serviceStore s) "events/"
data TurnProjection = TurnProjection
 { projectionStatus :: TurnStatus, projectionOrigin :: Value, projectionOwners :: [Text]
 , projectionActor :: Text, projectionOutput :: Maybe Value }

projectTurn :: Text -> AgentState -> Turn -> TurnProjection
projectTurn owner state turn = TurnProjection status origin owners actor output
 where
 status=if turnStatus turn==Done && turnField turn "warning"/=Null then Warning else turnStatus turn
 origin=turnField turn "origin" `fallback` empty
 owners=owner:[at origin "group" | has origin "group"]
 actor=if status==Queued then if has origin "routine" then "system" else text (get origin "author" `fallback` str "admin") else owner
 output=listToMaybe [get m "content" | m<-items (stateJSON state) "chat",get m "turn"==toJSON (turnId turn),at m "role"=="agent"]

-- Persist completed turn summaries once; the active state is the working set.
project :: Service -> Text -> IO ()
project s a = do
 state <- agentState s a
 forM_ (agentTurns state) $ \typed -> do
  let turn=toJSON typed; i=at turn "id"; p=projectTurn a state typed
      status=wireName (projectionStatus p)
  old <- loadValue s ("turns/"<>i) empty
  when (get old "status" /= str status) $
   record s (projectionOwners p) i ("call."<>status) (obj [("previous",get old "status"),("origin",projectionOrigin p),("error",get turn "error"),("warning",get turn "warning")]) (projectionActor p) (Just (at turn "agency_kind"))
  audit <- readArchive s a ("runs/"<>i)
  let output=fromMaybe (get audit "output" `fallback` get old "output") (projectionOutput p)
  saveValue s ("turns/"<>i) (merge turn (obj [("agent",str a),("status",str status),("output",output),("error",get turn "error" `fallback` get turn "warning")]))
turns :: Service -> IO [Value]
turns s = do
 rows <- sortOn (\v -> (at v "created",at v "id")) . map snd <$> A.values (serviceStore s) "turns/"
 forM rows $ \row -> do
  msg <- loadValue s ("messages/"<>at row "id") empty
  pure (merge row msg)
saveRoutine :: Service -> Value -> IO Value
saveRoutine s v = do
 i <- if has v "id" then pure (at v "id") else fresh "routine_sch_"
 let value=put "id" (str i) v
 putValue s ScheduledRoutines i value
 pure value
pulseCall :: Service -> Pulse -> Text -> Text -> Int -> [Text] -> IO ()
pulseCall s tick a kind slot groups = do
 t <- now
 i <- newId
 let v=merge (toJSON tick) (obj [("agent",str a),("agencyKind",str kind),("slot",num slot),("timestamp",str t)])
 putValue s PulseCalls i v
 record s (a:groups) a "pulse.dispatched" v "system" (Just kind)
recentPulses :: Service -> IO [Value]
recentPulses s = take 500 . reverse . sortOn (`at` "timestamp") <$> values s PulseCalls
-- A sent attachment is marked "sent" and leaves the composer drafts on the host itself,
-- so a device that is a few seconds behind can never put it back.
forgetDrafts :: Service -> Text -> [Value] -> IO ()
forgetDrafts _ _ [] = pure ()
forgetDrafts s owner rows = do
 forM_ rows $ \row -> updateAttachment s (put "sent" (Bool True) row)
 dropDrafts s owner rows

-- | Take these attachments out of the owner's composer drafts (sent or cancelled ones).
dropDrafts :: Service -> Text -> [Value] -> IO ()
dropDrafts _ _ [] = pure ()
dropDrafts s owner rows = do
 prefs <- singleton s Preferences
 let drafts = get prefs "attachment_drafts"
     ids = map (`get` "id") rows
 when (has drafts owner) $ saveSingleton s Preferences (put "attachment_drafts" (put owner (arr (filter (`notElem` ids) (list (get drafts owner)))) drafts) prefs)
