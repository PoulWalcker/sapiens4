{-# LANGUAGE OverloadedStrings #-}
module Store where

import Control.Exception
import Control.Concurrent.MVar
import Control.Monad
import Data.Aeson hiding (pairs)
import Data.Int
import Data.List (nub,find)
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding
import Database.SQLite.Simple hiding (withTransaction)
import qualified Database.SQLite.Simple as SQLite
import System.FilePath
import Text.Read (readMaybe)
import qualified Data.List.NonEmpty as NE
import Types

-- Only these tables contain JSON documents; callers cannot interpolate SQL names.
data Collection = Workloads | Groups | ScheduledRoutines | PulseCalls | TurnDetails | Preferences | SystemPulse deriving (Eq, Show)
tableName :: Collection -> Text
tableName table = case table of
 Workloads -> "workloads"
 Groups -> "groups"
 ScheduledRoutines -> "scheduled_routines"
 PulseCalls -> "pulse_calls"
 TurnDetails -> "turn_details"
 Preferences -> "preferences"
 SystemPulse -> "system_pulse"

openDatabase :: FilePath -> IO Connection
openDatabase root = bracketOnError (open (root </> "corpora.sqlite3")) close $ \db -> do
 execute_ db "PRAGMA busy_timeout=10000"
 execute_ db "PRAGMA foreign_keys=ON"
 pure db
closeDatabase :: Service -> IO ()
closeDatabase s = modifyMVar_ (serviceDatabase s) $ \database -> mapM_ close database >> pure Nothing
closeConnection :: Connection -> IO ()
closeConnection = close
connection :: Service -> (Connection -> IO a) -> IO a
connection s action = withMVar (serviceDatabase s) $ maybe (failure 503 "Database is closed") action
queryRows :: Service -> Text -> [Text] -> [SQLData] -> IO [Value]
queryRows s sql columns args = connection s $ \db -> queryRowsOn db sql columns args
queryRowsOn :: Connection -> Text -> [Text] -> [SQLData] -> IO [Value]
queryRowsOn db sql columns args = map (obj . zip columns . map val) <$> (query db (fromString (T.unpack sql)) args :: IO [[SQLData]])
 where
 val SQLNull = Null
 val (SQLText t) = str t
 val (SQLInteger n) = toJSON n
 val (SQLFloat n) = toJSON n
 val (SQLBlob _) = Null
executeSQL :: Service -> Text -> [SQLData] -> IO ()
executeSQL s sql args = connection s $ \db -> executeOn db sql args
executeOn :: Connection -> Text -> [SQLData] -> IO ()
executeOn db sql args = execute db (fromString (T.unpack sql)) args
sql :: Value -> SQLData
sql Null = SQLNull
sql (String t) = SQLText t
sql (Number n) = SQLInteger (fromIntegral (int (Number n)))
sql (Bool b) = SQLInteger (if b then 1 else 0)
sql v = SQLText (jsonText v)
values :: Service -> Collection -> IO [Value]
values s table = queryRows s ("SELECT value FROM " <> tableName table <> " ORDER BY rowid") ["value"] [] >>= traverse (decodeStored . (`at` "value"))
recentPulses :: Service -> IO [Value]
recentPulses s = queryRows s "SELECT value FROM pulse_calls ORDER BY sequence DESC LIMIT 500" ["value"] [] >>= traverse (decodeStored . (`at` "value"))
decodeStored :: FromJSON a => Text -> IO a
decodeStored raw = either (failure 500 . T.pack) pure (eitherDecodeStrict (Data.Text.Encoding.encodeUtf8 raw))
putValue :: Service -> Collection -> Text -> Value -> IO ()
putValue s table i v = connection s $ \db -> putValueOn db table i v
putValueOn :: Connection -> Collection -> Text -> Value -> IO ()
putValueOn db table i v = executeOn db ("INSERT INTO " <> tableName table <> " VALUES (?,?) ON CONFLICT(id) DO UPDATE SET value=excluded.value WHERE value IS NOT excluded.value") [SQLText i,SQLText (jsonText v)]
singleton :: Service -> Collection -> IO Value
singleton s table = fromMaybe empty . listToMaybe <$> values s table
saveSingleton :: Service -> Collection -> Value -> IO ()
saveSingleton s table v = executeSQL s ("INSERT INTO " <> tableName table <> " VALUES (1,?) ON CONFLICT(id) DO UPDATE SET value=excluded.value") [SQLText (jsonText v)]
initialize :: Service -> IO ()
initialize s = connection s $ \db -> do
 execute_ db "PRAGMA journal_mode=WAL"
 [Only version] <- query_ db "PRAGMA user_version" :: IO [Only Int]
 require (version `elem` [0..12]) 500 ("Unsupported CORPORA schema: " <> T.pack (show version))
 mapM_ (execute_ db) [
  "CREATE TABLE IF NOT EXISTS scheduled_routines (sequence INTEGER PRIMARY KEY AUTOINCREMENT, value TEXT NOT NULL)",
  "CREATE TABLE IF NOT EXISTS routine_occurrences (id TEXT PRIMARY KEY, value TEXT NOT NULL, status TEXT NOT NULL CHECK(status IN ('deferred','admitted_pending','admitted_ready','closed')))",
  "CREATE TABLE IF NOT EXISTS system_pulse (id INTEGER PRIMARY KEY CHECK(id=1), value TEXT NOT NULL)",
  "CREATE TABLE IF NOT EXISTS pulse_calls (sequence INTEGER PRIMARY KEY AUTOINCREMENT, value TEXT NOT NULL)",
  "CREATE TABLE IF NOT EXISTS turn_details (id TEXT PRIMARY KEY, value TEXT NOT NULL)",
  "CREATE TABLE IF NOT EXISTS agents (id TEXT PRIMARY KEY, name TEXT NOT NULL, role TEXT NOT NULL, color TEXT NOT NULL, face TEXT NOT NULL, created TEXT NOT NULL)",
  "CREATE TABLE IF NOT EXISTS turns (id TEXT PRIMARY KEY, agent TEXT NOT NULL REFERENCES agents(id), flow TEXT NOT NULL, input TEXT NOT NULL, status TEXT NOT NULL, output TEXT, error TEXT, tokens INTEGER NOT NULL, created TEXT NOT NULL)",
  "CREATE INDEX IF NOT EXISTS turns_agent ON turns(agent,created)",
  "CREATE TABLE IF NOT EXISTS preferences (id INTEGER PRIMARY KEY CHECK(id=1), value TEXT NOT NULL)",
  "CREATE TABLE IF NOT EXISTS attachments (id TEXT PRIMARY KEY, agent TEXT NOT NULL REFERENCES agents(id), value TEXT NOT NULL)",
  "CREATE TABLE IF NOT EXISTS message_inputs (job TEXT PRIMARY KEY, text TEXT NOT NULL, attachments TEXT NOT NULL)",
  "CREATE TABLE IF NOT EXISTS workloads (id TEXT PRIMARY KEY, value TEXT NOT NULL)",
  "CREATE TABLE IF NOT EXISTS groups (id TEXT PRIMARY KEY, value TEXT NOT NULL)",
  "CREATE TABLE IF NOT EXISTS group_attachments (id TEXT PRIMARY KEY, agent TEXT NOT NULL REFERENCES groups(id), value TEXT NOT NULL)",
  "CREATE TABLE IF NOT EXISTS activity_events (id INTEGER PRIMARY KEY, owner TEXT NOT NULL, timestamp TEXT NOT NULL, actor TEXT NOT NULL, entity TEXT NOT NULL, event TEXT NOT NULL, agency TEXT, details TEXT NOT NULL)",
  "CREATE INDEX IF NOT EXISTS activity_events_owner ON activity_events(owner,id)"]
 when (version `elem` [1,2]) $ execute_ db "INSERT OR IGNORE INTO turns SELECT * FROM jobs WHERE flow IN ('chat','computer')"
 when (version < 11) $ SQLite.withTransaction db $ do
  when (version `elem` [9,10]) $ do
   execute_ db "CREATE TABLE routine_occurrences_next (id TEXT PRIMARY KEY, value TEXT NOT NULL, status TEXT NOT NULL CHECK(status IN ('deferred','admitted_pending','admitted_ready','closed')))"
   if version == 9 then
    execute_ db "INSERT INTO routine_occurrences_next SELECT id,value,CASE materialized WHEN 0 THEN 'admitted_pending' ELSE 'admitted_ready' END FROM routine_occurrences"
   else execute_ db "INSERT INTO routine_occurrences_next SELECT id,value,status FROM routine_occurrences"
   execute_ db "DROP TABLE routine_occurrences"
   execute_ db "ALTER TABLE routine_occurrences_next RENAME TO routine_occurrences"
  -- Absence of a policy is explicit; migration never guesses the user's intent.
  -- Leave corrupt JSON untouched for the existing per-record error reporting.
  execute_ db "UPDATE scheduled_routines SET value=json_set(value,'$.policySelection',json('{\"kind\":\"awaiting_selection\"}')) WHERE CASE WHEN json_valid(value) THEN json_type(value)='object' AND json_type(value,'$.policySelection') IS NULL ELSE 0 END"
  execute_ db "UPDATE routine_occurrences SET value=json_set(value,'$.routine.policySelection',json('{\"kind\":\"awaiting_selection\"}')) WHERE CASE WHEN json_valid(value) THEN json_type(value,'$.routine')='object' AND json_type(value,'$.routine.policySelection') IS NULL ELSE 0 END"
  execute_ db "PRAGMA user_version=11"
 when (version < 12) $ SQLite.withTransaction db $ do
  -- Existing individual deferred/closed deadlines are singleton spans. IDs,
  -- snapshots, closure reasons and admitted Runs remain unchanged.
  execute_ db "UPDATE routine_occurrences SET value=json_set(value,'$.through',json_extract(value,'$.routine.nextDue')) WHERE status IN ('deferred','closed') AND CASE WHEN json_valid(value) THEN json_type(value,'$.through') IS NULL AND json_type(value,'$.routine.nextDue')='text' ELSE 0 END"
  execute_ db "PRAGMA user_version=12"

agents :: Service -> IO [Value]
agents s = map toJSON <$> agentProfiles s
agentProfiles :: Service -> IO [AgentProfile]
agentProfiles s = queryRows s "SELECT * FROM agents ORDER BY created,id" ["id","name","role","color","face","created"] [] >>= traverse (decodeValue 500)
agent :: Service -> Text -> IO Value
agent s i = agents s >>= maybe (failure 404 "Unknown Sapi") pure . findId i
mainAgent :: Service -> IO Text
mainAgent s = do
 roster <- population s >>= maybe (failure 500 "CORPORA has no Chief Sapi") pure
 pure (agentIdText (populationChief roster))
population :: Service -> IO (Maybe Population)
population s = fmap populationRoster . NE.nonEmpty . map profileId <$> agentProfiles s
addAgent :: Service -> Value -> IO ()
addAgent s v = executeSQL s "INSERT INTO agents VALUES (?,?,?,?,?,?)" [sql (get v k) | k <- ["id","name","role","color","face","created"]]
attachments :: Service -> Text -> IO [Value]
attachments s a = map (parseJSONText . (`at` "value")) <$> queryRows s ("SELECT value FROM " <> attachmentTable a <> " WHERE agent=?") ["value"] [SQLText a]
attachmentTable :: Text -> Text
attachmentTable a = if "group_" `T.isPrefixOf` a then "group_attachments" else "attachments"
saveAttachment :: Service -> Value -> IO ()
saveAttachment s v = executeSQL s ("INSERT INTO " <> attachmentTable (at v "agent") <> " VALUES (?,?,?)") [sql (get v "id"),sql (get v "agent"),SQLText (jsonText v)]
message :: Service -> Text -> Text -> [Value] -> IO ()
message s i t as = executeSQL s "INSERT OR IGNORE INTO message_inputs VALUES (?,?,?)" [SQLText i,SQLText t,SQLText (jsonText (arr as))]
record :: Service -> [Text] -> Text -> Text -> Value -> Text -> Maybe Text -> IO ()
record s owners entity event details suppliedActor suppliedAgency = connection s $ \db -> SQLite.withTransaction db $ recordOn s db owners entity event details suppliedActor suppliedAgency
recordOn :: Service -> Connection -> [Text] -> Text -> Text -> Value -> Text -> Maybe Text -> IO ()
recordOn s db owners entity event details suppliedActor suppliedAgency = do
 (source,kind) <- currentActor s
 let actor=if suppliedActor=="admin" then source else suppliedActor
     agency=if isJust suppliedAgency then suppliedAgency else kind
 time <- now
 forM_ (nub (owners ++ [actor | "sapi_" `T.isPrefixOf` actor])) $ \owner -> do
  executeOn db "INSERT INTO activity_events(owner,timestamp,actor,entity,event,agency,details) VALUES (?,?,?,?,?,?,?)" [SQLText owner,SQLText time,SQLText actor,SQLText entity,SQLText event,maybe SQLNull SQLText agency,SQLText (jsonText details)]
  executeOn db "DELETE FROM activity_events WHERE owner=? AND id NOT IN (SELECT id FROM activity_events WHERE owner=? ORDER BY id DESC LIMIT 100)" [SQLText owner,SQLText owner]
events :: Service -> IO Value
events s = do
 rows <- queryRows s "SELECT * FROM activity_events ORDER BY id DESC" ["id","owner","timestamp","actor","entity","event","agency","details"] []
 pure $ obj [(k,arr vs) | (k,vs) <- M.toList (M.fromListWith (flip (++)) [(at v "owner",[remove ["owner"] (put "details" (parseJSONText (at v "details")) v)]) | v <- rows])]
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

project :: Service -> Text -> IO ()
project s a = do
 state <- agentState s a
 connection s $ \db -> SQLite.withTransaction db $ forM_ (agentTurns state) $ \typed -> do
  let turn=toJSON typed
  previous <- queryRowsOn db "SELECT status FROM turns WHERE id=?" ["status"] [sql (get turn "id")]
  let projection=projectTurn a state typed
      status=wireName (projectionStatus projection)
      origin=projectionOrigin projection
      owners=projectionOwners projection
      actor=projectionActor projection
      old=maybe Null (get `flip` "status") (listToMaybe previous)
  when (old /= str status) $ recordOn s db owners (at turn "id") ("call." <> status) (obj [("previous",old),("origin",origin),("error",get turn "error"),("warning",get turn "warning")]) actor (Just (if has turn "output_order" then "chatOutput" else at turn "agency_kind"))
  let details = selectPresent ["origin","batch_id","batch_members","agency_kind","slot","pulseId","output_at","output_order"] turn
  putValueOn db TurnDetails (at turn "id") details
  let output=projectionOutput projection
  archived <- if isNothing output && status `elem` ["done","warning"] && batchLeader typed == turnId typed then readJSON (archivePath s a ("runs/" <> at turn "id")) empty else pure empty
  executeOn db "INSERT INTO turns VALUES (?,?,?,?,?,?,?,?,?) ON CONFLICT(id) DO UPDATE SET status=excluded.status,output=COALESCE(excluded.output,turns.output),error=excluded.error WHERE turns.status IS NOT excluded.status OR turns.output IS NOT COALESCE(excluded.output,turns.output) OR turns.error IS NOT excluded.error" [sql (get turn "id"),SQLText a,sql (get turn "flow"),sql (get turn "input"),SQLText status,sql (fromMaybe (get archived "output") output),sql (get turn "error" `fallback` get turn "warning"),SQLInteger 0,sql (get turn "created")]
turns :: Service -> IO [Value]
turns s = do
 rows <- queryRows s "SELECT id,agent,flow,input,status,output,error,created FROM turns ORDER BY created,id" ["id","agent","flow","input","status","output","error","created"] []
 messages <- queryRows s "SELECT * FROM message_inputs" ["job","text","attachments"] []
 details <- queryRows s "SELECT * FROM turn_details" ["id","value"] []
 let messagesById=M.fromList [(at m "job",m) | m<-messages]
     detailsById=M.fromList [(at d "id",parseJSONText (at d "value")) | d<-details]
 pure [let msg=M.findWithDefault empty (at v "id") messagesById; d=M.findWithDefault empty (at v "id") detailsById in merge (merge v d) (obj [("attachments",get msg "attachments" `asJSON` arr []),("input",get msg "text" `fallback` get v "input")]) | v <- rows]
 where asJSON (String t) _ = parseJSONText t; asJSON _ def = def
saveRoutine :: Service -> Value -> IO Value
saveRoutine s v = connection s $ \db -> saveRoutineOn db v
saveRoutineOn :: Connection -> Value -> IO Value
saveRoutineOn db v = do
 sequenceId <- if has v "id" then maybe (failure 500 "Invalid scheduled routine ID") pure (T.stripPrefix "routine_sch_" (at v "id") >>= readMaybe . T.unpack :: Maybe Int64) else execute_ db "INSERT INTO scheduled_routines(value) VALUES ('{}')" >> lastInsertRowId db
 let i = "routine_sch_" <> T.justifyRight 5 '0' (T.pack (show sequenceId)); value = put "id" (str i) v
 execute db "UPDATE scheduled_routines SET value=? WHERE sequence=?" (jsonText value,sequenceId)
 pure value
pulseCall :: Service -> Pulse -> Text -> Text -> Int -> [Text] -> IO ()
pulseCall s tick a kind slot groups = do
 t <- now
 let v = merge (toJSON tick) (obj [("agent",str a),("agencyKind",str kind),("slot",num slot),("timestamp",str t)])
 executeSQL s "INSERT INTO pulse_calls(value) VALUES (?)" [SQLText (jsonText v)]
 record s (a:groups) a "pulse.dispatched" v "system" (Just kind)
