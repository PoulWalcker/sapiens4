{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module DataRoot (module DataRoot, module RuntimePaths) where

import Types (writeJSON)
import RuntimePaths
import Control.Exception
import Control.Monad
import Data.Aeson
import qualified Data.Aeson.KeyMap as K
import qualified Data.ByteString.Lazy as L
import Data.List (nub, sort)
import Data.Char (isAscii, isAlphaNum)
import qualified Data.Text as T
import qualified Data.UUID as UUID
import Domain (CorporaConfig)
import System.Directory
import System.FilePath
import System.FileLock
import System.IO.Error (ioeGetErrorString, isAlreadyExistsError, isUserError)
import System.Info (os)
import qualified MigrationFiles

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

acquireStorage :: StorageChoice -> FilePath -> IO FileLock
acquireStorage choice root = do
 when (choice==StartNew) $ checkWritableParent root >> createPrivateDirectory root
 path <- managedDataPath root (root </> "host.lock")
 lockedBefore <- doesFileExist path
 unless lockedBefore $ validateRoot choice root
 bracketOnError (storageLock root) unlockFile $ \lock -> do
  location <- selectedPath choice root
  unless (equalFilePath location root) $ storageError "The selected data directory changed during startup."
  validateRoot choice root
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

validateRoot :: StorageChoice -> FilePath -> IO ()
validateRoot choice root = do
 tree <- storageTree choice root
 let lockChild (_,path) = withExistingLock path
 foldr lockChild (mapM_ (\(mode,path) -> void (inspectRoot mode path)) tree) (drop 1 tree)

-- Managed writers must not follow aliases outside their owned storage paths.
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

-- Keep every participant locked through the complete preparation. The caller
-- takes ownership of the root lock only when its action succeeds.
openStorageTree :: StorageChoice -> FilePath
                -> (FileLock -> [(StorageChoice,FilePath,FileLock)] -> IO a) -> IO a
openStorageTree choice root action = do
 when (choice==StartNew) $ checkWritableParent root >> createPrivateDirectory root
 tree <- storageTree choice root
 let ids = [ident | (AttachChild _ ident,_) <- tree]
 unless (length (nub ids)==length ids) $ storageError "A Corpora identity occurs twice in the registry tree."
 let paths = map snd tree
 locations <- mapM MigrationFiles.fileIdentity paths
 present <- mapM (regularFileExists . (</> "host.lock")) paths
 unless (and present) $ forM_ tree $ \(mode,path) -> do
  void (inspectRoot mode path)
  void (managedDataPath path (path </> "host.lock"))
 bracketOnError (storageLock root) unlockFile $ \owner ->
  let lockChildren [] acquired = do
       location <- selectedPath choice root
       unless (equalFilePath location root) $ storageError "The selected Data Root changed while opening."
       actual <- storageTree choice root
       let descendants = [ident | (AttachChild _ ident,_) <- drop 1 actual]
       unless (null descendants) $ do
        identity <- readCorporaIdentity root
        when (identity `elem` descendants) $ storageError "The parent Corpora identity also occurs in its registry."
       lockedLocations <- mapM MigrationFiles.fileIdentity paths
       unless (actual==tree && lockedLocations==locations) $ storageError "The registered Corpora changed while opening. Try again."
       action owner ((choice,root,owner):reverse acquired)
      lockChildren ((mode,path):rest) acquired = bracket (storageLock path) unlockFile $ \lock ->
       lockChildren rest ((mode,path,lock):acquired)
  in lockChildren (drop 1 tree) []

-- The identity carries the whole-storage revision; backups are recovery data,
-- never evidence of a data format. Missing storageVersion is the supported legacy.
-- Every required storage conversion must advance this revision. Supported rollback
-- starts with the first D2 release enforcing this gate; absent/0 is not a wildcard.
data StorageState = Initializing | LegacyStorage | CurrentStorage deriving (Eq, Show)

inspectRoot :: StorageChoice -> FilePath -> IO StorageState
inspectRoot choice root = do
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
 when (choice==StartNew && not (null meaningful)) $
  storageError "Start fresh requires an empty data directory. Choose Use another folder to open your existing data."
 let initializing = topLevel && (null names && choice==StartNew || lockOnly)
                 || choice==CreateCorpora && all (`elem` ["corpora-identity.json","corpora-settings.json"]) meaningful
 when (choice==CreateCorpora && not initializing) $ storageError "New Corpora directory contains unexpected data."
 hasIdentity <- regularFileExists (root </> "corpora-identity.json")
 hasDatabase <- regularFileExists (root </> "corpora.sqlite3")
 unless (initializing || hasIdentity && hasDatabase) $
  storageError "This is not a complete Sapiens Data Root. Select the folder containing corpora-identity.json and corpora.sqlite3."
 revision <- if hasIdentity then do
  (ident,state) <- readStorageIdentity root
  case choice of
   AttachChild _ expected | ident/=expected -> storageError "Registered Corpora identity changed."
   _ -> pure ()
  pure state
  else pure LegacyStorage
 settings <- regularFileExists (root </> "corpora-settings.json")
 when (settings || choice==CreateCorpora) $ void (readDataJSON (root </> "corpora-settings.json") :: IO CorporaConfig)
 when (choice==CreateCorpora && not hasIdentity) $ storageError "Missing new Corpora identity."
 pure (if initializing then Initializing else revision)

safeId :: T.Text -> Bool
safeId ident = not (T.null ident) && T.all (\c -> isAscii c && (isAlphaNum c || c `elem` ("_-"::String))) ident

readStorageIdentity :: FilePath -> IO (T.Text, StorageState)
readStorageIdentity root = do
 identity <- managedDataPath root (root </> "corpora-identity.json") >>= readDataJSON
 case identity of
  Object fields | Just (Number 1) <- K.lookup "version" fields,
                  Just (String ident) <- K.lookup "corpora_id" fields,
                  Just ident == (UUID.toText <$> UUID.fromText ident) -> do
   revision <- case K.lookup "storageVersion" fields of
    Nothing -> pure LegacyStorage
    Just (Number 0) -> pure LegacyStorage
    Just (Number 1) -> pure CurrentStorage
    _ -> storageError "Unsupported storage version. Open this Data Root with a compatible Sapiens version."
   pure (ident,revision)
  _ -> storageError "Invalid CORPORA identity. Restore corpora-identity.json from backup."

readCorporaIdentity :: FilePath -> IO T.Text
readCorporaIdentity root = fst <$> readStorageIdentity root

-- Called only inside the protected conversion, after every domain converter succeeds.
markStorageCurrent :: FilePath -> IO ()
markStorageCurrent root = do
 path <- managedDataPath root (root </> "corpora-identity.json")
 fields <- readDataJSON path
 writeJSON path (Object (K.insert "storageVersion" (Number 1) fields))

checkRegisteredIdentity :: FilePath -> T.Text -> IO ()
checkRegisteredIdentity child ident = do
 saved <- readCorporaIdentity child
 unless (saved==ident) $ storageError "A registered Corpora identity does not match its directory. Restore it from backup."
