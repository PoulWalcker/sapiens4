{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module DataRoot (module DataRoot, module RuntimePaths) where

import Types (writeJSON, migrateAgentState, validateValue, pairs, get)
import RuntimePaths
import Control.Exception
import Control.Monad
import Data.Aeson hiding (pairs)
import Data.Aeson.Key (fromText)
import qualified Data.Aeson.KeyMap as K
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Data.List (isPrefixOf, nub, sort)
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
import qualified SQLiteSchema
import qualified MigrationFiles

data StartupFormats = AllowExistingConversions | CurrentFormatsOnly deriving (Eq)

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
 makeAbsolute explicit >>= populatedDataDirectory
startupDataDirectory Nothing = do
 saved <- savedDataDirectory
 case saved of
  Just directory -> pure (Just directory)
  Nothing -> defaultDataDirectory >>= populatedDataDirectory

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
makePrivateDirectory = MigrationFiles.privateDirectory

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

-- Once an agent was published, a surviving bootstrap marker no longer
-- authorizes writing that profile without a verified original.
publishedBootstrap :: FilePath -> IO Bool
publishedBootstrap root = do
 exists <- doesDirectoryExist (initializingDirectory root)
 if not exists then pure False else do
  database <- managedDataPath root (root </> "corpora.sqlite3")
  present <- doesFileExist database
  if not present then pure False else do
   forM_ ["-wal","-shm","-journal"] $ \suffix -> void (managedDataPath root (database ++ suffix))
   withReadOnlyDatabase root $ \db -> do
    table <- query_ db "SELECT 1 FROM sqlite_master WHERE type='table' AND name='agents'" :: IO [Only Int]
    if null table then pure False else not . null <$> (query_ db "SELECT 1 FROM agents LIMIT 1" :: IO [Only Int])

acquireStorage :: StorageChoice -> FilePath -> IO FileLock
acquireStorage = acquireStorageWith AllowExistingConversions

acquireStorageWith :: StartupFormats -> StorageChoice -> FilePath -> IO FileLock
acquireStorageWith formats choice root = do
 when (choice==StartNew) $ checkWritableParent root >> createPrivateDirectory root
 path <- managedDataPath root (root </> "host.lock")
 lockedBefore <- doesFileExist path
 unless lockedBefore $ validateRootFormats formats choice root
 bracketOnError (storageLock root) unlockFile $ \lock -> do
  location <- selectedPath choice root
  unless (equalFilePath location root) $ storageError "The selected data directory changed during startup."
  validateRootFormats formats choice root
  names <- listDirectory root
  when (names==["host.lock"] && choice `elem` [StartNew,AttachExisting]) $
   makePrivateDirectory (initializingDirectory root)
  pure lock

storageLock :: FilePath -> IO FileLock
storageLock root = do
 path <- managedDataPath root (root </> "host.lock")
 tryLockFile path Exclusive >>= maybe
  (storageError ("Another Sapiens4 server is using " ++ root ++ ". Quit it before opening these data.")) pure

withExistingLock :: FilePath -> IO a -> IO a
withExistingLock root action = do
 path <- managedDataPath root (root </> "host.lock")
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
readDataJSON path = either (storageError . ("Could not read data: " ++)) pure . eitherDecode =<< L.readFile path

validateDataRoot :: StorageChoice -> FilePath -> IO ()
validateDataRoot = validateRootFormats AllowExistingConversions

-- The preflight never creates a missing lock or database in the selected tree.
checkUpdateRoot :: FilePath -> IO ()
checkUpdateRoot path = do
 root <- selectedPath AttachExisting path
 withUpdateLock root (validateRootFormats CurrentFormatsOnly AttachExisting root)

withUpdateLock :: FilePath -> IO a -> IO a
withUpdateLock root action = do
 lock <- managedDataPath root (root </> "host.lock")
 exists <- doesFileExist lock
 unless exists $ storageError ("Missing existing Corpora lock: " ++ root)
 bracket (storageLock root) unlockFile (const action)

validateRootFormats :: StartupFormats -> StorageChoice -> FilePath -> IO ()
validateRootFormats formats choice root = do
 tree <- storageTree choice root
 let lockChild (_,path) = if formats==CurrentFormatsOnly then withUpdateLock path else withExistingLock path
 foldr lockChild (mapM_ (\(mode,path) -> inspectRoot (formatRequirement formats path) mode path) tree) (drop 1 tree)

-- Recovery identifies converter writes by their logical paths. An alias into
-- another part of the same root would otherwise write a different inventory entry.
managedDataPath :: FilePath -> FilePath -> IO FilePath
managedDataPath root path = do
 resolved <- containedPath root path
 unless (equalFilePath resolved path) $ storageError ("A managed data path was redirected: " ++ path)
 pure resolved

regularFileExists :: FilePath -> IO Bool
regularFileExists path = do
 exists <- doesPathExist path
 when exists $ do
  regular <- doesFileExist path
  unless regular $ storageError ("Expected a regular file: " ++ path)
 pure exists

formatRequirement :: StartupFormats -> FilePath -> String -> IO ()
formatRequirement AllowExistingConversions _ _ = pure ()
formatRequirement CurrentFormatsOnly root what = storageError ("Update requires current " ++ what ++ " in " ++ root)

registeredChildren :: FilePath -> IO [(StorageChoice,FilePath)]
registeredChildren root = do
 path <- managedDataPath root (root </> "host-corpora.json")
 exists <- regularFileExists path
 if not exists then pure [] else do
  doc <- readDataJSON path
  ids <- case doc of
   Object fields -> case K.lookup "locals" fields of
    Nothing -> pure []
    Just (Array entries) -> forM (foldr (:) [] entries) $ \v -> case v of
     String ident | Just ident == (UUID.toText <$> UUID.fromText ident) -> pure ident
     _ -> storageError "Invalid registered Corpora identity."
    _ -> storageError "Invalid local Corpora list."
   _ -> storageError "Invalid Corpora registry."
  unless (length (nub ids)==length ids) $ storageError "Duplicate registered Corpora identity."
  forM (sort ids) $ \ident -> do
   child <- childPath root ident
   unless (equalFilePath child (root </> "corporas" </> T.unpack ident)) $
    storageError "A registered Corpora directory has been redirected."
   available <- doesDirectoryExist child
   unless available $ storageError "A registered Corpora is missing. Restore the complete Data Root before opening it."
   pure (AttachChild root ident,child)

storageTree :: StorageChoice -> FilePath -> IO [(StorageChoice,FilePath)]
storageTree = walk []
 where
 walk ancestors choice root = do
  identity <- MigrationFiles.fileIdentity root
  when (identity `elem` ancestors) $ storageError "The local Corpora registry contains a directory cycle."
  children <- registeredChildren root
  descendants <- concat <$> mapM (uncurry (walk (identity:ancestors))) children
  pure ((choice,root):descendants)

-- Keep every participant locked through conversion and final inspection. The caller
-- takes ownership of the root lock only when its action succeeds.
openStorageTree :: StartupFormats -> StorageChoice -> FilePath
                -> (FileLock -> [(StorageChoice,FilePath,FileLock)] -> IO a) -> IO a
openStorageTree formats choice root action = do
 tree <- storageTree choice root
 let ids = [ident | (AttachChild _ ident,_) <- tree]
 unless (length (nub ids)==length ids) $ storageError "A Corpora identity occurs twice in the registry tree."
 let paths = map snd tree
 locations <- mapM MigrationFiles.fileIdentity paths
 present <- mapM (regularFileExists . (</> "host.lock")) paths
 unless (and present) $ forM_ tree $ \(mode,path) -> do
  inspectRoot (formatRequirement formats path) mode path
  void (managedDataPath path (path </> "host.lock"))
 bracketOnError (storageLock root) unlockFile $ \owner ->
  let lockChildren [] acquired = do
       location <- selectedPath choice root
       unless (equalFilePath location root) $ storageError "The selected Data Root changed while opening."
       actual <- storageTree choice root
       let descendants = [ident | (AttachChild _ ident,_) <- drop 1 actual]
       unless (null descendants) $ do
        identity <- managedDataPath root (root </> "corpora-identity.json") >>= readDataJSON
        when (get identity "corpora_id" `elem` map String descendants) $ storageError "The parent Corpora identity also occurs in its registry."
       lockedLocations <- mapM MigrationFiles.fileIdentity paths
       unless (actual==tree && lockedLocations==locations) $ storageError "The registered Corpora changed while opening. Try again."
       action owner ((choice,root,owner):reverse acquired)
      lockChildren ((mode,path):rest) acquired = bracket (storageLock path) unlockFile $ \lock ->
       lockChildren rest ((mode,path,lock):acquired)
  in lockChildren (drop 1 tree) []

inspectRoot :: (String -> IO ()) -> StorageChoice -> FilePath -> IO ()
inspectRoot legacy choice root = do
  exists <- doesDirectoryExist root
  unless exists $ storageError "This data directory is unavailable. Reconnect the drive or choose another directory."
  names <- listDirectory root
  forM_ ["agentpy/corpora/archive","uploads","decision-prompts","corpora.sqlite3","corpora.sqlite3-wal","corpora.sqlite3-shm","corpora.sqlite3-journal","corpora-identity.json","corpora-settings.json","host-corpora.json","agentpy","agentpy/corpora","agentpy/agents","workspaces","remote-relay.json","remote-device.json"] $ \name ->
   void (containedPath root (root </> name))
  forM_ ["corpora.sqlite3","corpora.sqlite3-wal","corpora.sqlite3-shm","corpora.sqlite3-journal","corpora-identity.json"] $
   void . managedDataPath root . (root </>)
  let meaningful=filter (/="host.lock") names
      topLevel = choice `elem` [StartNew,AttachExisting]
  lockOnly <- if topLevel && names==["host.lock"] then do
   let lock = root </> "host.lock"
   regular <- doesFileExist lock
   link <- pathIsSymbolicLink lock
   if regular && not link then (==0) <$> getFileSize lock else pure False
   else pure False
  pending <- doesDirectoryExist (initializingDirectory root)
  when (pending && not topLevel) $ storageError "A registered Corpora has unfinished storage initialization."
  let current condition what = unless condition (legacy what)
  current (not pending) "completed storage initialization"
  when pending $ do
   void (managedDataPath root (initializingDirectory root))
   linked <- pathIsSymbolicLink (initializingDirectory root)
   contents <- listDirectory (initializingDirectory root)
   unless ("host.lock" `elem` names && not linked && null contents && all bootstrapFile meaningful) $
    storageError "Unexpected files in an unfinished Sapiens Data Root."
  when (choice==StartNew && not (null meaningful) && not pending) $
   storageError "Start fresh requires an empty data directory. Choose Use another folder to open your existing data."
  let initializing = topLevel && (null names && choice==StartNew || lockOnly)
                  || choice==CreateCorpora && all (`elem` ["corpora-identity.json","corpora-settings.json"]) meaningful
  current (not initializing || choice `elem` [StartNew,CreateCorpora]) "existing Corpora"
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
                      AttachChild _ expected | ident/=expected -> storageError "Registered Corpora identity changed."
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
    version <- SQLiteSchema.readVersion db
    unless (version > 0 && version <= SQLiteSchema.currentVersion && (not pending || version==SQLiteSchema.currentVersion)) $ storageError "Unsupported Corpora schema. Open these data with a compatible Sapiens version; no migration was performed."
    current (version==SQLiteSchema.currentVersion) ("SQLite schema " ++ show SQLiteSchema.currentVersion)
    integrity <- query_ db "PRAGMA quick_check" :: IO [Only T.Text]
    unless (integrity==[Only "ok"]) $ storageError "The Corpora database is corrupted. Restore it from a backup."
    SQLiteSchema.checkSchema db version
    rows <- query_ db "SELECT id FROM agents ORDER BY created,id" :: IO [Only T.Text]
    faces <- query_ db "SELECT face FROM agents" :: IO [Only T.Text]
    current (length (nub faces)==length faces) "agent avatars"
    when (version >= 7) $ do
     routines <- query_ db "SELECT value FROM scheduled_routines" :: IO [Only T.Text]
     forM_ routines $ \(Only encoded) -> do
      routine <- either storageError pure (eitherDecodeStrict (TE.encodeUtf8 encoded))
      case routine of
       Object f -> case K.lookup "runCount" f of
        Nothing -> legacy "routine run counts"
        Just n | Success count <- fromJSON n, (count :: Int)>=0 -> pure ()
        _ -> storageError "Invalid routine run count."
       _ -> storageError "Invalid scheduled routine."
    when (null rows && not pending) $ storageError "The Corpora database has no agents. Restore a complete Data Root from backup."
    preferences <- query_ db "SELECT count(*) FROM sqlite_master WHERE type='table' AND name='preferences'" :: IO [Only Int]
    when (preferences==[Only 1]) $ do
     prefs <- query_ db "SELECT value FROM preferences WHERE id=1" :: IO [Only T.Text]
     current (length prefs==1) "workspace preferences"
     groupsPresent <- query_ db "SELECT count(*) FROM sqlite_master WHERE type='table' AND name='groups'" :: IO [Only Int]
     groups <- if groupsPresent==[Only 1] then query_ db "SELECT id FROM groups" else pure []
     forM_ prefs $ \(Only encoded) -> do
      doc <- either storageError pure (eitherDecodeStrict (TE.encodeUtf8 encoded))
      fields <- case doc of Object f -> pure f; _ -> storageError "Invalid workspace preferences."
      case K.lookup "workspace_revision" fields of
       Nothing -> pure ()
       Just n | Success rev <- fromJSON n, (rev :: Int)>=0 -> pure ()
       _ -> storageError "Invalid workspace revision."
      let browser = K.lookup "browser_version" fields
      unless (browser `elem` [Nothing, Just (Number 0), Just (Number 1)]) $ storageError "Unsupported browser preferences version."
      current (browser==Just (Number 1)) "browser preferences v1"
      ws <- case K.lookup "workspaces" fields of
       Nothing -> legacy "workspaces" >> pure (Object K.empty)
       Just v@(Object _) -> pure v
       _ -> storageError "Invalid workspaces."
      forM_ (pairs ws) $ \(owner,w) -> do
       unless (safeId owner && (browser==Just (Number 1) || Only owner `elem` rows ++ groups)) $ storageError "Workspace preferences contain an unknown owner."
       folder <- managedDataPath root (root </> "workspaces" </> T.unpack owner)
       entry <- case w of Object f -> pure f; _ -> storageError "Invalid workspace."
       forM_ ["tabs","bookmarks"] $ \key -> case K.lookup key entry of
        Nothing -> legacy "workspace tabs and bookmarks"
        Just (Array elements) -> forM_ elements $ \element -> case element of
         Object tab | key=="tabs" && browser==Just (Number 1) && any (`K.member` tab) ["type","html","artifact"] ->
          storageError "Legacy tab payload inside current browser preferences. Restore a compatible profile."
         Object tab -> when (key=="tabs" && browser/=Just (Number 1)) $ do
          forM_ ["artifact","type","html","url","path"] $ \field -> case K.lookup field tab of
           Nothing -> pure ()
           Just (String _) -> pure ()
           _ -> storageError "Invalid legacy workspace tab."
          case K.lookup "artifact" tab of
           Just (String name) | not (T.null name) -> do
            artifacts <- managedDataPath root (folder </> "artifacts")
            void (containedPath artifacts (artifacts </> T.unpack name))
           _ -> pure ()
         _ -> storageError "Invalid workspace entry."
        _ -> storageError "Invalid workspace tabs or bookmarks."
       case K.lookup "memo_bookmark_seeded" entry of
        Just (Bool _) -> pure ()
        Nothing -> pure ()
        _ -> storageError "Invalid Notes bookmark flag."
      current (all (\(Only agent) -> get (get ws agent) "memo_bookmark_seeded"==Bool True) rows) "Notes bookmarks"
      current (not (K.member "work_views" fields)) "preferences"
      current (get doc "panel" `notElem` map String ["mindmap","cron","log"]) "panel preferences"
    pure (map fromOnly rows)
   unless (null agents) $ do
    dirPath <- managedDataPath root (root </> "agentpy/corpora/directory.json")
    saved <- regularFileExists dirPath
    directory <- if saved then readDataJSON dirPath else pure (Object K.empty)
    entries <- case directory of Object f -> pure f; _ -> storageError "Invalid agent directory."
    forM_ (zip [0::Int ..] agents) $ \(index,agent) -> case K.lookup (fromText agent) entries of
     Nothing -> legacy "agent directory"
     Just (Object entry) -> case K.lookup "parent" entry of
      Nothing -> legacy "agent parent"
      Just Null -> current (index==0) "agent parent"
      Just (String _) -> current (index/=0) "agent parent"
      _ -> storageError "Invalid agent parent."
     _ -> storageError "Invalid agent directory entry."
   forM_ agents $ \agent -> do
    unless (safeId agent) $
     storageError "The Corpora database contains an invalid agent identity."
    let path=root </> "agentpy/agents" </> T.unpack agent </> "state.json"
    let archive=root </> "agentpy/corpora/archive" </> T.unpack agent
        work=root </> "workspaces" </> T.unpack agent
    forM_ ([takeDirectory path,path,archive,archive </> "turns",work,work </> "Notes.html",work </> "Notes.history.json"] ++
           [takeDirectory path </> "manifests" </> name | name <- ["identity.md","computer-use.md","corpora-scope.md"]]) $ void . containedPath root
    mapM_ (managedDataPath root) [path, work]
    state <- readDataJSON path
    case state of
     Object fields | Just (Number version) <- K.lookup "schema_version" fields, version `elem` [1,2,3],
                     Just (String savedAgent) <- K.lookup "agid" fields, savedAgent==agent -> do
      current (version==3 && not (K.member "initial_manager" fields)) "agent state v3"
      forM_ [1::Int,2] $ \v -> do
       backup <- managedDataPath (takeDirectory path) (takeDirectory path </> ("legacy-state-v" ++ show v ++ ".json"))
       void (regularFileExists backup)
      forM_ ["Notes.html","Notes.yaml","Notes.md"] $ \name -> do
       note <- managedDataPath work (work </> name)
       exists <- doesPathExist note
       when exists $ do
        file <- doesFileExist note
        linked <- pathIsSymbolicLink (work </> name)
        unless (file && not linked) $ storageError ("Invalid Notes file: " ++ note)
      notes <- doesFileExist (work </> "Notes.html")
      unless notes $ do
       legacy "Notes.html"
       sources <- filterM doesFileExist [work </> "Notes.yaml",work </> "Notes.md"]
       case sources of
        source:_ -> do
         bytes <- B.readFile source
         case TE.decodeUtf8' bytes of
          Left e -> storageError ("Invalid Notes text in " ++ source ++ ": " ++ show e)
          Right _ -> pure ()
        [] -> pure ()
      case K.lookup "initial_manager" fields of
       Nothing -> pure ()
       Just Null -> pure ()
       Just (String _) -> pure ()
       _ -> storageError "Invalid legacy agent manager."
      let decoded = if version==3 then validateValue 500 parseJSON state else migrateAgentState (round version) state
      case decoded of
       Right _ -> pure ()
       Left _ -> storageError "Saved agent state is invalid. Restore it from backup."
     _ -> storageError "Unsupported or mismatched agent state. Restore compatible data from backup."
   settingsExist <- regularFileExists (root </> "corpora-settings.json")
   when settingsExist $ void (readDataJSON (root </> "corpora-settings.json") :: IO CorporaConfig)

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
