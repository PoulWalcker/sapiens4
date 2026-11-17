{-# LANGUAGE OverloadedStrings #-}
module SQLiteSchema (currentVersion, readVersion, checkSchema, migrate) where

import Control.Monad
import Data.String (fromString)
import qualified Data.Text as T
import Database.SQLite.Simple

currentVersion :: Int
currentVersion = 8

readVersion :: Connection -> IO Int
readVersion db = do
 rows <- query_ db "PRAGMA user_version"
 case rows of
  [Only version] -> pure version
  _ -> fail "SQLite returned no unique schema version."

checkSchema :: Connection -> Int -> IO ()
checkSchema db version
 | version >= 1 && version <= currentVersion = checkThrough8 db version
 | otherwise = fail ("Unsupported Corpora schema: " ++ show version)

migrate :: Connection -> IO ()
migrate db = do
 version <- readVersion db
 -- Zero is also used when initializing a new database.
 unless (version >= 0 && version <= currentVersion) $
  fail ("Unsupported Corpora schema: " ++ show version)
 when (version < currentVersion) $ do
  execute_ db "PRAGMA journal_mode=WAL"
  withTransaction db $ do
   migrateTo8 db version
   checkSchema db currentVersion
   execute_ db (fromString ("PRAGMA user_version=" ++ show currentVersion))

migrateTo8 :: Connection -> Int -> IO ()
migrateTo8 db version = do
 mapM_ (execute_ db) [
  "CREATE TABLE IF NOT EXISTS scheduled_routines (sequence INTEGER PRIMARY KEY AUTOINCREMENT, value TEXT NOT NULL)",
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

checkThrough8 :: Connection -> Int -> IO ()
checkThrough8 db version = do
 let turns = "id agent flow input status output error tokens created"
     layout = [("agents","id name role color face created",True)
              ,("jobs",turns,version `elem` [1,2])
              ,("events","id agent job source_key kind detail time",version `elem` [1,2,3])
              ,("preferences","id value",version>=1)
              ,("turns",turns,version>=3)
              ,("attachments","id agent value",version>=2)
              ,("message_inputs","job text attachments",version>=2)
              ,("workloads","id value",version>=4)
              ,("groups","id value",version>=5)
              ,("group_attachments","id agent value",version>=5)
              ,("system_pulse","id value",version>=6)
              ,("pulse_calls","sequence value",version>=6)
              ,("turn_details","id value",version>=6)
              ,("scheduled_routines","sequence value",version>=7)
              ,("activity_events","id owner timestamp actor entity event agency details",version>=8)]
 forM_ layout $ \(name,fields,required) -> do
  columns <- map fromOnly <$> (query db "SELECT name FROM pragma_table_info(?) ORDER BY cid" (Only (name :: T.Text)) :: IO [Only T.Text])
  when ((required || not (null columns)) && columns /= T.words fields) $
   fail ("The Corpora database has an incompatible or missing " ++ T.unpack name ++ " table. Restore a complete Data Root from backup.")
