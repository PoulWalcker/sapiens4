{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module DataRoot (module DataRoot, module RuntimePaths) where

import RuntimePaths (configDirectory, defaultDataDirectory)
import Control.Exception
import Control.Monad
import Data.Aeson
import qualified Data.Aeson.KeyMap as K
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Data.Char (isAscii, isAlphaNum)
import Data.List (isInfixOf)
import qualified Data.Text as T
import qualified Data.UUID as UUID
import Database.SQLite.Simple
import Domain (AgentState, CorporaConfig)
import Network.URI (escapeURIString, isUnreserved)
import System.Directory
import System.FilePath
import System.FileLock
import System.IO
import System.IO.Error (ioeGetErrorString)

-- A new location may be empty; attaching must always find a complete Data Root.
data StorageChoice = StartNew | AttachExisting | CreateCorpora deriving (Eq, Show)

selectionPath :: IO FilePath
selectionPath = (</> "storage.json") <$> configDirectory

savedDataDirectory :: IO (Maybe FilePath)
savedDataDirectory = do
 path <- selectionPath
 exists <- doesFileExist path
 if not exists then pure Nothing else do
  value <- either (ioError . userError . ("Could not read saved storage: " ++)) pure . eitherDecode =<< L.readFile path
  case value of
   Object fields | Just (Number 1) <- K.lookup "version" fields, Just (String folder) <- K.lookup "directory" fields, isAbsolute (T.unpack folder) -> pure (Just (T.unpack folder))
   _ -> ioError (userError "Saved storage settings are incompatible. Choose your data directory again.")

saveDataDirectory :: FilePath -> IO ()
saveDataDirectory directory = do
 path <- selectionPath
 createDirectoryIfMissing True (takeDirectory path)
 bracketOnError (openBinaryTempFile (takeDirectory path) "storage.json.tmp")
  (\(temporary,h) -> do
   hClose h `catch` (\(_::IOException) -> pure ())
   removeFile temporary `catch` (\(_::IOException) -> pure ())) $ \(temporary,h) -> do
   L.hPut h (encode (object ["version" .= (1::Int), "directory" .= directory]))
   hFlush h
   hClose h
   renameFile temporary path

-- The pinned SQLite enables URI filenames. Read-only mode prevents journal
-- recovery or accidental creation while checking an attached installation.
withReadOnlyDatabase :: FilePath -> (Connection -> IO a) -> IO a
withReadOnlyDatabase root = bracket (open uri) close
 where
 path=map (\c -> if c=='\\' then '/' else c) (root </> "corpora.sqlite3")
 uri="file:" ++ escapeURIString (\c -> isUnreserved c || c=='/' || c==':') path ++ "?mode=ro"

checkDataRoot :: StorageChoice -> FilePath -> IO ()
checkDataRoot choice directory = do
 when (choice==StartNew) (createDirectoryIfMissing True directory)
 exists <- doesDirectoryExist directory
 unless exists $ storageError "This data directory is unavailable. Reconnect the drive or choose another directory."
 root <- canonicalizePath directory
 bracket (storageLock root) unlockFile $ \_ -> validateDataRoot choice root

storageLock :: FilePath -> IO FileLock
storageLock root = tryLockFile (root </> "host.lock") Exclusive >>= maybe
 (storageError "Another Sapiens4 server is using this data directory. Quit it before opening these data.") pure

storageMessage :: Exception e => e -> String
storageMessage errorValue = maybe (displayException errorValue) ioeGetErrorString (fromException (toException errorValue))

-- Startup diagnostics belong in the log, rather than the storage picker.
startupMessage :: String -> String
startupMessage diagnostic
 | "Could not read onboarding.json." `isInfixOf` diagnostic = "We couldn't load your saved setup settings. Your agents and conversations are unchanged. Restore your setup settings from a backup, then reopen Sapiens4."
 | otherwise = "Sapiens4 couldn't start. Please reopen the app and try again."

storageError :: String -> IO a
storageError = ioError . userError

readDataJSON :: FromJSON a => FilePath -> IO a
readDataJSON path = either (storageError . ("Could not read data: " ++)) pure . eitherDecode =<< L.readFile path

-- The caller owns host.lock. Check the whole registered root before any
-- initialization/recovery writes; child services later acquire their own locks.
validateDataRoot :: StorageChoice -> FilePath -> IO ()
validateDataRoot choice = validate []
 where
 validate ancestors directory = do
  exists <- doesDirectoryExist directory
  unless exists $ storageError "This data directory is unavailable. Reconnect the drive or choose another directory."
  root <- canonicalizePath directory
  when (root `elem` ancestors) $ storageError "The Corpora registry contains a directory cycle. Restore it from backup."
  names <- listDirectory root
  let meaningful=filter (`notElem` ["host.lock","desktop-server.log"]) names
  when (null ancestors && choice==StartNew && not (null meaningful)) $
   storageError "Start fresh requires an empty data directory. Choose Use another folder to open your existing data."
  let initializing = null ancestors && (null meaningful && choice==StartNew || choice==CreateCorpora && all (`elem` ["corpora-identity.json","corpora-settings.json"]) meaningful)
  if initializing && choice==CreateCorpora then do
   identity <- readDataJSON (root </> "corpora-identity.json")
   case identity of
    Object fields | Just (Number 1) <- K.lookup "version" fields, Just (String ident) <- K.lookup "corpora_id" fields, Just _ <- UUID.fromText ident -> pure ()
    _ -> storageError "Invalid new Corpora identity."
   void (readDataJSON (root </> "corpora-settings.json") :: IO CorporaConfig)
  else unless initializing $ do
   complete <- and <$> mapM (doesFileExist . (root </>)) ["corpora-identity.json","corpora.sqlite3"]
   unless complete $ storageError "This is not a complete Sapiens Data Root. Select the folder containing corpora-identity.json and corpora.sqlite3."
   identity <- readDataJSON (root </> "corpora-identity.json")
   case identity of
    Object fields | Just (Number 1) <- K.lookup "version" fields,
                    Just (String ident) <- K.lookup "corpora_id" fields,
                    Just _ <- UUID.fromText ident -> pure ()
    _ -> storageError "Invalid CORPORA identity. Restore corpora-identity.json from backup."
   let database=root </> "corpora.sqlite3"
   header <- withBinaryFile database ReadMode (\h -> B.hGet h 100)
   unless (B.take 16 header=="SQLite format 3\NUL" && B.length header==100) $
    storageError "The Corpora database is invalid. Restore it from a backup."
   agents <- withReadOnlyDatabase root $ \db -> do
    versions <- query_ db "PRAGMA user_version" :: IO [Only Int]
    unless (versions==[Only 8]) $ storageError "Unsupported Corpora schema. Open these data with a compatible Sapiens version; no migration was performed."
    tables <- map fromOnly <$> (query_ db "SELECT name FROM sqlite_master WHERE type='table'" :: IO [Only T.Text])
    unless (all (`elem` tables) ["agents","turns","scheduled_routines","preferences","workloads","groups","system_pulse","pulse_calls","turn_details","attachments","message_inputs","group_attachments","activity_events"]) $
     storageError "The Corpora database is incomplete. Restore a complete Data Root from backup."
    rows <- query_ db "SELECT id FROM agents ORDER BY created,id" :: IO [Only T.Text]
    when (null rows) $ storageError "The Corpora database has no agents. Restore a complete Data Root from backup."
    pure (map fromOnly rows)
   forM_ agents $ \agent -> do
    unless (not (T.null agent) && T.all (\c -> isAscii c && (isAlphaNum c || c `elem` ("_-"::String))) agent) $
     storageError "The Corpora database contains an invalid agent identity."
    let path=root </> "agentpy/agents" </> T.unpack agent </> "state.json"
    state <- readDataJSON path
    case state of
     Object fields | Just (Number version) <- K.lookup "schema_version" fields, version `elem` [1,2,3],
                     Just (String savedAgent) <- K.lookup "agid" fields, savedAgent==agent -> do
      -- Versions 1/2 have an existing backup-backed migration. Future versions
      -- must fail before ensureAgent can write manifests or create directories.
      when (version==3) $ case fromJSON state :: Result AgentState of
       Success _ -> pure ()
       Error _ -> storageError "Saved agent state is invalid. Restore it from backup."
     _ -> storageError "Unsupported or mismatched agent state. Restore compatible data from backup."
   settingsExist <- doesFileExist (root </> "corpora-settings.json")
   when settingsExist $ void (readDataJSON (root </> "corpora-settings.json") :: IO CorporaConfig)
   registryExists <- doesFileExist (root </> "host-corpora.json")
   when registryExists $ do
    registry <- readDataJSON (root </> "host-corpora.json")
    case registry of
     Object fields -> case K.lookup "locals" fields of
      Nothing -> pure ()
      Just (Array locals) -> forM_ locals $ \value -> case value of
       String ident | Just _ <- UUID.fromText ident -> do
        let child=root </> "corporas" </> T.unpack ident
        available <- doesDirectoryExist child
        unless available $ storageError "A registered Corpora is missing. Restore the complete Data Root before opening it."
        bracket (storageLock child) unlockFile $ \_ -> do
         validate (root:ancestors) child
         childIdentity <- readDataJSON (child </> "corpora-identity.json")
         unless (case childIdentity of Object row -> K.lookup "corpora_id" row==Just (String ident); _ -> False) $
          storageError "A registered Corpora identity does not match its directory. Restore it from backup."
       _ -> storageError "The Corpora registry contains an invalid identity. Restore it from backup."
      _ -> storageError "The Corpora registry is invalid. Restore it from backup."
     _ -> storageError "The Corpora registry is invalid. Restore it from backup."
