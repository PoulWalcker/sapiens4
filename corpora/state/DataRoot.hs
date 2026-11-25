{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}
module DataRoot (module DataRoot, module RuntimePaths) where

import Types (writeJSON)
import qualified V00_02_03Files as TypedFiles
import RuntimePaths
import V00_02_00Launcher (previousLauncherDirectory)
import UpdateState (assertDataAvailable)
import Control.Exception
import Control.Monad
import Data.Aeson hiding (pairs)
import qualified Data.Aeson.KeyMap as K
import qualified Data.ByteString.Lazy as L
import Data.List (isPrefixOf)
import qualified Data.Text as T
import qualified Data.UUID as UUID
import System.Directory
import System.FilePath
import System.FileLock
import System.IO.Error (ioeGetErrorString, isAlreadyExistsError, isUserError)
import System.Info (os)
#ifndef mingw32_HOST_OS
import qualified System.Posix.Directory as Posix
#endif

data StorageChoice = StartNew | AttachExisting | AttachChild FilePath T.Text | CreateCorpora deriving (Eq, Show)

selectionPath :: IO FilePath
selectionPath = configDirectory >>= \root -> containedPath root (root </> "storage.json")

savedDataDirectory :: IO (Maybe FilePath)
savedDataDirectory = do
 path <- selectionPath
 exists <- doesFileExist path
 if not exists then pure Nothing else do
  value <- TypedFiles.readValue path Null
  case value of
   Object fields | Just (Number 1) <- K.lookup "version" fields, Just (String folder) <- K.lookup "directory" fields, isAbsolute (T.unpack folder) -> Just <$> externalStoragePath (T.unpack folder)
   _ -> ioError (userError "Saved storage settings are incompatible. Choose your data directory again.")

startupDataDirectory :: Maybe FilePath -> IO (Maybe FilePath)
startupDataDirectory (Just explicit) = do
 when (null explicit) $ storageError "Data directory must not be empty"
 externalStoragePath explicit >>= populatedDataDirectory
startupDataDirectory Nothing = do
 saved <- savedDataDirectory
 case saved of
  Just directory -> pure (Just directory)
  Nothing -> do
   previous <- configDirectory >>= previousLauncherDirectory
   case previous of
    Nothing -> defaultDataDirectory >>= populatedDataDirectory
    Just path -> do
     directory <- externalStoragePath path
     validateDataRoot AttachExisting directory
     pure (Just directory)

populatedDataDirectory :: FilePath -> IO (Maybe FilePath)
populatedDataDirectory directory = do
 exists <- doesPathExist directory
 names <- if exists then listDirectory directory else pure []
 pure (if null (filter (/="host.lock") names) then Nothing else Just directory)

storageChoiceForDirectory :: FilePath -> IO StorageChoice
storageChoiceForDirectory directory = do
 names <- listDirectory directory
 pure (if null names then StartNew else AttachExisting)

saveDataDirectory :: FilePath -> IO ()
saveDataDirectory directory = do
 root <- externalStoragePath directory
 path <- selectionPath
 writeJSON path (object ["version" .= (1::Int), "directory" .= root])

makePrivateDirectory :: FilePath -> IO ()
#ifdef mingw32_HOST_OS
makePrivateDirectory = createDirectory
#else
makePrivateDirectory path = Posix.createDirectory path 0o700
#endif

createPrivateDirectory :: FilePath -> IO ()
createPrivateDirectory path = do
 exists <- doesDirectoryExist path
 unless exists $ do
  createDirectoryIfMissing True (takeDirectory path)
  makePrivateDirectory path `catch` \e -> unless (isAlreadyExistsError e) (throwIO e)

selectedPath :: StorageChoice -> FilePath -> IO FilePath
selectedPath choice directory = do
 root <- externalStoragePath directory
 case choice of
  AttachChild parent ident -> do
   expected <- childPath parent ident
   unless (equalFilePath expected root) $ storageError "The registered Corpora directory changed."
  _ -> pure ()
 pure root

childPath :: FilePath -> T.Text -> IO FilePath
childPath root ident = do
 unless ((UUID.toText <$> UUID.fromText ident) == Just ident) $ storageError "Invalid registered Corpora identity."
 folder <- containedPath root (root </> "corporas")
 containedPath folder (folder </> T.unpack ident)

checkWritableParent :: FilePath -> IO ()
checkWritableParent path = do
 dir <- doesDirectoryExist path
 if dir then do
  perms <- getPermissions path
  unless (writable perms && (os=="mingw32" || searchable perms)) $ storageError "The selected data directory is not writable."
 else do
  let parent = takeDirectory path
  when (path==parent) $ storageError "This data directory is unavailable."
  checkWritableParent parent

initializingDirectory :: FilePath -> FilePath
initializingDirectory root = root </> ".sapiens-initializing"

acquireStorage :: StorageChoice -> FilePath -> IO FileLock
acquireStorage choice root = do
 when (choice==StartNew) $ checkWritableParent root >> createPrivateDirectory root
 path <- containedPath root (root </> "host.lock")
 lockedBefore <- doesFileExist path
 unless lockedBefore $ validateDataRoot choice root
 bracketOnError (storageLock root) unlockFile $ \lock -> do
  location <- selectedPath choice root
  unless (equalFilePath location root) $ storageError "The selected data directory changed during startup."
  assertDataAvailable root
  validateDataRoot choice root
  names <- listDirectory root
  when (names==["host.lock"] && choice `elem` [StartNew,AttachExisting]) $
   makePrivateDirectory (initializingDirectory root)
  pure lock

storageLock :: FilePath -> IO FileLock
storageLock root = do
 path <- containedPath root (root </> "host.lock")
 tryLockFile path Exclusive >>= maybe
  (storageError "Another Sapiens4 server is using this data directory. Quit it before opening these data.") pure

withExistingLock :: FilePath -> IO a -> IO a
withExistingLock root action = do
 path <- containedPath root (root </> "host.lock")
 exists <- doesFileExist path
 if exists then bracket (storageLock root) unlockFile (const action) else action

storageMessage :: Exception e => e -> String
storageMessage errorValue = case fromException (toException errorValue) of
 Just err | isUserError err -> ioeGetErrorString err
 _ -> displayException errorValue

openingStorage, openedStorage, startupErrorPrefix :: String
openingStorage = "Sapiens4 storage: opening"
openedStorage = "Sapiens4 storage: opened"
startupErrorPrefix = "Sapiens4 startup error: "

startupMessage :: String
startupMessage = "Sapiens4 couldn't start. See desktop-server.log in the application configuration folder for details."

storageError :: String -> IO a
storageError = ioError . userError

readDataJSON :: FromJSON a => FilePath -> IO a
readDataJSON path = TypedFiles.readValue path Null >>= \v -> case fromJSON v of Error e -> storageError e; Success a -> pure a

-- Validation is read-only. Versioned conversion runs under host.lock in Migrations.
validateDataRoot :: StorageChoice -> FilePath -> IO ()
validateDataRoot choice root = do
 exists <- doesDirectoryExist root
 unless exists $ storageError "This data directory is unavailable."
 names <- filter (/="host.lock") <$> listDirectory root
 forM_ ["corpora.sqlite3","corpora.sqlite3-wal","corpora.sqlite3-journal","corpora.lmdb","corpora.lmdb.migration.lock","corpora.lmdb.migrating","corpora.lmdb.migrating.lock","corpora.lmdb.migrating-lock","corpora.lmdb.lock","corpora.lmdb-lock","corpora.lmdb.single-migration.lock","corpora.lmdb.v00.02.01","corpora.lmdb.v00.02.02","corpora.lmdb.v00.02.03.staging","corpora.lmdb.v00.02.03.staging.lock","corpora.lmdb.v00.02.03.staging-lock","corpora-identity.json","corpora-identity.typed","workspaces","uploads","agentpy"] $ \name -> void (containedPath root (root </> name))
 when ("corpora.append4" `elem` names && not (any (`elem` names) ["corpora.lmdb","corpora.sqlite3"])) $ storageError "Unreleased Append4 journal format has no registered migration; originals retained."
 pending <- doesDirectoryExist (initializingDirectory root)
 when pending $ do
  void (containedPath root (initializingDirectory root))
  contents <- listDirectory (initializingDirectory root)
  let allowed name = name `elem` [".sapiens-initializing","corpora-identity.json","corpora-identity.typed","corpora-identity.typed.v00.02.02","corpora.sqlite3","corpora.sqlite3-wal","corpora.sqlite3-journal","corpora.lmdb","corpora.lmdb.migration.lock","corpora.lmdb.migrating","corpora.lmdb.migrating.lock","corpora.lmdb.migrating-lock","corpora.lmdb.lock","corpora.lmdb-lock","corpora.lmdb.single-migration.lock","corpora.lmdb.v00.02.01","corpora.lmdb.v00.02.02","corpora.lmdb.v00.02.03.staging","corpora.lmdb.v00.02.03.staging.lock","corpora.lmdb.v00.02.03.staging-lock","agentpy","workspaces"] || any (`isPrefixOf` name) [".writing-",".typed-writing-"]
  unless (null contents && all allowed names) $ storageError "Unexpected files in an unfinished data directory."
 let creating = null names || pending || choice==CreateCorpora && names==["corpora-identity.json"]
 when (choice==StartNew && not creating) $ storageError "Start fresh requires an empty data directory."
 unless creating $ do
  unless ("corpora-identity.json" `elem` names && any (`elem` names) ["corpora.lmdb","corpora.sqlite3"]) $ storageError "Incomplete Append4 data directory."
  identity <- readDataJSON (root </> "corpora-identity.json")
  case identity of
   Object fields | Just (String ident) <- K.lookup "corpora_id" fields, (UUID.toText <$> UUID.fromText ident)==Just ident ->
    case choice of
     AttachChild _ expected | ident/=expected -> storageError "Registered Corpora identity changed."
     _ -> pure ()
   _ -> storageError "Invalid CORPORA identity."

checkRegisteredIdentity :: FilePath -> T.Text -> IO ()
checkRegisteredIdentity child ident = do
 path <- containedPath child (child </> "corpora-identity.json")
 identity <- readDataJSON path
 unless (case identity of Object fields -> K.lookup "corpora_id" fields==Just (String ident); _ -> False) $
  storageError "A registered Corpora identity does not match its directory. Restore it from backup."
