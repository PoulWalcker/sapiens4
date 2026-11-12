{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}
module DataRoot (module DataRoot, module RuntimePaths) where

import Types (writeJSON, migrateAgentState, validateValue, pairs, get)
import RuntimePaths
import Control.Exception
import Control.Monad
import Data.Aeson hiding (pairs)
import qualified Data.Aeson.KeyMap as K
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Data.List (isPrefixOf)
import Data.Char (isAscii, isAlphaNum)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUIDv4
import Database.SQLite.Simple
import Domain (CorporaConfig)
import Network.URI (escapeURIString, isUnreserved)
import System.Directory
import System.FilePath
import System.FileLock
import System.IO
import System.IO.Error (ioeGetErrorString, isAlreadyExistsError, isUserError)
import System.Info (os)
#ifndef mingw32_HOST_OS
import qualified System.Posix.Directory as Posix
#endif

supportedDatabaseVersions :: [Int]
supportedDatabaseVersions = [0..8]

data StorageChoice = StartNew | AttachExisting | AttachChild FilePath T.Text | CreateCorpora deriving (Eq, Show)

selectionPath :: IO FilePath
selectionPath = configDirectory >>= \root -> containedPath root (root </> "storage.json")

savedDataDirectory :: IO (Maybe FilePath)
savedDataDirectory = do
 path <- selectionPath
 exists <- doesFileExist path
 if not exists then pure Nothing else do
  value <- either (ioError . userError . ("Could not read saved storage: " ++)) pure . eitherDecode =<< L.readFile path
  case value of
   Object fields | Just (Number 1) <- K.lookup "version" fields, Just (String folder) <- K.lookup "directory" fields, isAbsolute (T.unpack folder) -> pure (Just (T.unpack folder))
   _ -> ioError (userError "Saved storage settings are incompatible. Choose your data directory again.")

startupDataDirectory :: Maybe FilePath -> IO (Maybe FilePath)
startupDataDirectory (Just explicit) = do
 when (null explicit) $ storageError "Data directory must not be empty"
 Just <$> makeAbsolute explicit
startupDataDirectory Nothing = savedDataDirectory

storageChoiceForDirectory :: FilePath -> IO StorageChoice
storageChoiceForDirectory directory = do
 names <- listDirectory directory
 pure (if null names then StartNew else AttachExisting)

saveDataDirectory :: FilePath -> IO ()
saveDataDirectory directory = do
 path <- selectionPath
 writeJSON path (object ["version" .= (1::Int), "directory" .= directory])

withReadOnlyDatabase :: FilePath -> (Connection -> IO a) -> IO a
withReadOnlyDatabase root action = do
 let database = root </> "corpora.sqlite3"
     nonempty path = do
      exists <- doesFileExist path
      if exists then (>0) <$> getFileSize path else pure False
 journals <- filterM (nonempty . (database ++)) ["-wal","-journal"]
 if null journals then bracket (open (sqliteURI database ++ "?mode=ro&immutable=1")) close action
 else do
  temporary <- getTemporaryDirectory
  nonce <- UUID.toString <$> UUIDv4.nextRandom
  let folder = temporary </> ("sapiens-storage-" ++ nonce)
      copy = folder </> "corpora.sqlite3"
  bracket_ (makePrivateDirectory folder) (removePathForcibly folder) $ do
   forM_ ("":journals) $ \suffix -> copyFile (database ++ suffix) (copy ++ suffix)
   bracket (open copy) close $ \db -> execute_ db "PRAGMA query_only=ON" >> action db

sqliteURI :: FilePath -> String
sqliteURI native = "file:" ++ escapeURIString (\c -> isUnreserved c || c=='/' || c==':') path
 where
 path | os/="mingw32" = native
      | otherwise = let urlPath = map (\c -> if isPathSeparator c then '/' else c) native
                    in if take 2 urlPath=="//" then "//" ++ urlPath else '/' : urlPath

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

checkSchema :: Connection -> Int -> IO ()
checkSchema db version = do
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
   storageError ("The Corpora database has an incompatible or missing " ++ T.unpack name ++ " table. Restore a complete Data Root from backup.")

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
readDataJSON path = either (storageError . ("Could not read data: " ++)) pure . eitherDecode =<< L.readFile path

validateDataRoot :: StorageChoice -> FilePath -> IO ()
validateDataRoot choice = validate []
 where
 validate ancestors directory = do
  exists <- doesDirectoryExist directory
  unless exists $ storageError "This data directory is unavailable. Reconnect the drive or choose another directory."
  root <- canonicalizePath directory
  when (root `elem` ancestors) $ storageError "The Corpora registry contains a directory cycle. Restore it from backup."
  names <- listDirectory root
  forM_ ["agentpy/corpora/archive","uploads","decision-prompts","corpora.sqlite3","corpora.sqlite3-wal","corpora.sqlite3-shm","corpora.sqlite3-journal","corpora-identity.json","corpora-settings.json","host-corpora.json","agentpy","agentpy/corpora","agentpy/agents","workspaces","remote-relay.json","remote-device.json"] $ \name ->
   void (containedPath root (root </> name))
  let meaningful=filter (/="host.lock") names
      topLevel = null ancestors && choice `elem` [StartNew,AttachExisting]
  lockOnly <- if topLevel && names==["host.lock"] then do
   let lock = root </> "host.lock"
   regular <- doesFileExist lock
   link <- pathIsSymbolicLink lock
   if regular && not link then (==0) <$> getFileSize lock else pure False
   else pure False
  pending <- if topLevel then doesDirectoryExist (initializingDirectory root) else pure False
  when pending $ do
   linked <- pathIsSymbolicLink (initializingDirectory root)
   contents <- listDirectory (initializingDirectory root)
   unless ("host.lock" `elem` names && not linked && null contents && all bootstrapFile meaningful) $
    storageError "Unexpected files in an unfinished Sapiens Data Root."
  when (null ancestors && choice==StartNew && not (null meaningful) && not pending) $
   storageError "Start fresh requires an empty data directory. Choose Use another folder to open your existing data."
  let initializing = topLevel && (null names && choice==StartNew || lockOnly)
                  || null ancestors && choice==CreateCorpora && all (`elem` ["corpora-identity.json","corpora-settings.json"]) meaningful
  when (choice==CreateCorpora && not initializing) $ storageError "New Corpora directory contains unexpected data."
  if initializing && choice==CreateCorpora then do
   identity <- readDataJSON (root </> "corpora-identity.json")
   case identity of
    Object fields | Just (Number 1) <- K.lookup "version" fields, Just (String ident) <- K.lookup "corpora_id" fields, Just _ <- UUID.fromText ident -> pure ()
    _ -> storageError "Invalid new Corpora identity."
   void (readDataJSON (root </> "corpora-settings.json") :: IO CorporaConfig)
  else unless initializing $ do
   let database=root </> "corpora.sqlite3"
   hasIdentity <- doesFileExist (root </> "corpora-identity.json")
   hasDatabase <- doesFileExist database
   unless ((hasIdentity || pending && not hasDatabase) && (hasDatabase || pending)) $ storageError "This is not a complete Sapiens Data Root. Select the folder containing corpora-identity.json and corpora.sqlite3."
   when hasIdentity $ do
    identity <- readDataJSON (root </> "corpora-identity.json")
    case identity of
     Object fields | Just (Number 1) <- K.lookup "version" fields,
                     Just (String ident) <- K.lookup "corpora_id" fields,
                     Just ident == (UUID.toText <$> UUID.fromText ident) -> case choice of
                      AttachChild _ expected | null ancestors && ident/=expected -> storageError "Registered Corpora identity changed."
                      _ -> pure ()
     _ -> storageError "Invalid CORPORA identity. Restore corpora-identity.json from backup."
   -- A new database can be empty or have only SQLite's WAL-mode header
   -- until the schema transaction commits. Published databases must pass
   -- the normal schema and history checks, including during bootstrap.
   blank <- if pending && hasDatabase then withReadOnlyDatabase root $ \db -> do
    versions <- query_ db "PRAGMA user_version" :: IO [Only Int]
    tables <- query_ db "SELECT count(*) FROM sqlite_master" :: IO [Only Int]
    pure (versions==[Only 0] && tables==[Only 0])
    else pure False
   let beforeSchema = pending && (not hasDatabase || blank)
   when (beforeSchema && any (`elem` names) ["agentpy","workspaces"]) $
    storageError "The unfinished Data Root has agent files without a database."
   unless beforeSchema $ do
    header <- withBinaryFile database ReadMode (\h -> B.hGet h 100)
    unless (B.take 16 header=="SQLite format 3\NUL" && B.length header==100) $
     storageError "The Corpora database is invalid. Restore it from a backup."
   agents <- if beforeSchema then pure [] else withReadOnlyDatabase root $ \db -> do
    [Only version] <- query_ db "PRAGMA user_version" :: IO [Only Int]
    unless (version > 0 && version `elem` supportedDatabaseVersions && (not pending || version==8)) $ storageError "Unsupported Corpora schema. Open these data with a compatible Sapiens version; no migration was performed."
    integrity <- query_ db "PRAGMA quick_check" :: IO [Only T.Text]
    unless (integrity==[Only "ok"]) $ storageError "The Corpora database is corrupted. Restore it from a backup."
    checkSchema db version
    rows <- query_ db "SELECT id FROM agents ORDER BY created,id" :: IO [Only T.Text]
    when (null rows && not pending) $ storageError "The Corpora database has no agents. Restore a complete Data Root from backup."
    preferences <- query_ db "SELECT count(*) FROM sqlite_master WHERE type='table' AND name='preferences'" :: IO [Only Int]
    when (preferences==[Only 1]) $ do
     prefs <- query_ db "SELECT value FROM preferences WHERE id=1" :: IO [Only T.Text]
     groupsPresent <- query_ db "SELECT count(*) FROM sqlite_master WHERE type='table' AND name='groups'" :: IO [Only Int]
     groups <- if groupsPresent==[Only 1] then query_ db "SELECT id FROM groups" else pure []
     forM_ prefs $ \(Only encoded) -> do
      doc <- either storageError pure (eitherDecodeStrict (TE.encodeUtf8 encoded))
      when (get doc "browser_version" /= Number 1) $ forM_ (pairs (get doc "workspaces")) $ \(owner,_) -> do
       unless (Only owner `elem` rows ++ groups && safeId owner) $ storageError "Workspace preferences contain an unknown owner."
       void (containedPath root (root </> "workspaces" </> T.unpack owner))
    pure (map fromOnly rows)
   forM_ agents $ \agent -> do
    unless (safeId agent) $
     storageError "The Corpora database contains an invalid agent identity."
    let path=root </> "agentpy/agents" </> T.unpack agent </> "state.json"
    let archive=root </> "agentpy/corpora/archive" </> T.unpack agent
        work=root </> "workspaces" </> T.unpack agent
    forM_ ([takeDirectory path,path,archive,archive </> "turns",work,work </> "Notes.html",work </> "Notes.history.json"] ++
           [takeDirectory path </> "manifests" </> name | name <- ["identity.md","computer-use.md","corpora-scope.md"]]) $ void . containedPath root
    state <- readDataJSON path
    case state of
     Object fields | Just (Number version) <- K.lookup "schema_version" fields, version `elem` [1,2,3],
                     Just (String savedAgent) <- K.lookup "agid" fields, savedAgent==agent -> do
      let decoded = if version==3 then validateValue 500 parseJSON state else migrateAgentState (round version) state
      case decoded of
       Right _ -> pure ()
       Left _ -> storageError "Saved agent state is invalid. Restore it from backup."
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
        child <- childPath root ident
        available <- doesDirectoryExist child
        unless available $ storageError "A registered Corpora is missing. Restore the complete Data Root before opening it."
        withExistingLock child $ do
         checkRegisteredIdentity child ident
         validate (root:ancestors) child
       _ -> storageError "The Corpora registry contains an invalid identity. Restore it from backup."
      _ -> storageError "The Corpora registry is invalid. Restore it from backup."
     _ -> storageError "The Corpora registry is invalid. Restore it from backup."

-- Only files produced before first-agent publication can accompany the marker.
bootstrapFile :: FilePath -> Bool
bootstrapFile name = name `elem` [".sapiens-initializing","corpora-identity.json","agentpy","workspaces"]
 || name `elem` map ("corpora.sqlite3" ++) ["","-journal","-wal","-shm"]
 || ".writing-" `isPrefixOf` name

safeId :: T.Text -> Bool
safeId ident = not (T.null ident) && T.all (\c -> isAscii c && (isAlphaNum c || c `elem` ("_-"::String))) ident

checkRegisteredIdentity :: FilePath -> T.Text -> IO ()
checkRegisteredIdentity child ident = do
 path <- containedPath child (child </> "corpora-identity.json")
 identity <- readDataJSON path
 unless (case identity of Object fields -> K.lookup "corpora_id" fields==Just (String ident); _ -> False) $
  storageError "A registered Corpora identity does not match its directory. Restore it from backup."
