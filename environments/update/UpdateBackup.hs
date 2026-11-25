module UpdateBackup (snapshot, restoreSnapshot, withRootLocks, validateRoots) where

import Control.Exception
import Control.Monad
import Data.Aeson
import Data.Aeson.Types (parseEither)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Data.List (isPrefixOf, nub, sort)
import qualified Data.Set as Set
import qualified Data.Text as T
import System.Directory
import System.FilePath
import System.FileLock
import UpdatePlatform
import UpdateState
import qualified Migrations

data EntryKind = FileEntry | DirectoryEntry deriving Eq
data TreeEntry = TreeEntry { relativePath :: FilePath, entryKind :: EntryKind } deriving Eq
data Entry = Entry { location :: TreeEntry, fingerprint :: Sha256 } deriving Eq

data Inventory = Inventory
 { inventoryRoot :: FilePath, writerLocks :: [FilePath], exclusions :: [FilePath], entries :: [Entry] }
 deriving Eq

data BackupInventory = BackupInventory { dataInventory :: Inventory, configInventory :: Inventory }
 deriving Eq

instance ToJSON Entry where
 toJSON entry = toJSON (relativePath (location entry),entryKind (location entry) == DirectoryEntry,fingerprint entry)

instance ToJSON Inventory where
 toJSON (Inventory root locks excluded saved) = toJSON (root,locks,excluded,saved)

instance ToJSON BackupInventory where
 toJSON (BackupInventory savedData savedConfig) = toJSON [savedData,savedConfig]

backupRoots :: FilePath -> UpdateRoots
backupRoots destination = UpdateRoots {dataRoot=destination </> "0", configRoot=destination </> "1"}

managedChildren :: FilePath -> IO [FilePath]
managedChildren root = do
 validatePath root
 -- The registry can be SQLite-era JSON or current LMDB during an upgrade.
 saved <- Migrations.readRegistry root
 names <- maybe (pure []) (either fail pure . parseEither registry) saved
 forM names $ \name -> case canonicalUUID name of
  Just _ -> pure (root </> "corporas" </> T.unpack name)
  _ -> fail "Invalid registered child Corpora"
 where registry = withObject "Corpora registry" (\o -> o .:? "locals" .!= [])

managedRoots :: FilePath -> IO [FilePath]
managedRoots root = managedChildren root >>= fmap ((root:) . concat) . mapM managedRoots

withRootLocks :: FilePath -> IO a -> IO a
withRootLocks root action = withLock (root </> "host.lock") $ do
 assertDataAvailable root
 children <- managedChildren root
 foldr withRootLocks action (sort (nub children))

withLock :: FilePath -> IO a -> IO a
withLock path action = do
 validatePath (takeDirectory path)
 void (pathPresent path)
 bracket (tryLockFile path Exclusive >>= maybe (fail "A Corpora writer is still running; update refused") pure)
  unlockFile (const action)

withSurvivingLock :: FilePath -> IO a -> IO a
withSurvivingLock path action = do
 let parent = takeDirectory path
 exists <- pathPresent parent
 directory <- if exists then doesDirectoryExist parent else pure False
 if directory then withLock path action else action

resolveExclusions :: FilePath -> [FilePath] -> IO (Set.Set FilePath)
resolveExclusions root paths = Set.fromList . concat <$> mapM (resolve "" . splitDirectories) paths
 where
 resolve relative [] = pure [relative]
 resolve relative (name:names) = do
  let parent = root </> relative
  validatePath parent
  directory <- doesDirectoryExist parent
  found <- if directory then actualName (parent </> name) else pure Nothing
  maybe (pure []) (\actual -> resolve (relative </> actual) names) found

tree :: FilePath -> Set.Set FilePath -> IO [TreeEntry]
tree root excluded = walk ""
 where
 walk relative | Set.member relative excluded = pure []
 walk relative = do
  let path = root </> relative
  validatePath path
  directory <- doesDirectoryExist path
  children <- if not directory then pure [] else sort <$> listDirectory path >>= fmap concat . mapM (walk . (relative </>))
  pure (TreeEntry relative (if directory then DirectoryEntry else FileEntry):children)

inventory :: FilePath -> Set.Set FilePath -> IO [Entry]
inventory root excluded = tree root excluded >>= mapM (\entry ->
 Entry entry . sha256 . L.fromStrict <$> metadata (root </> relativePath entry))

snapshot :: FilePath -> UpdateRoots -> IO Sha256
snapshot destination live = do
 unless (null profileExclusions) $ do
  corpora <- managedRoots (dataRoot live)
  forM_ (configRoot live:corpora) $ \root ->
   forM_ (nub (map takeDirectory profileExclusions)) $ \parent -> do
    let path = root </> parent
    exists <- pathPresent path
    unless exists (createDirectory path)
    validatePath path
    directory <- doesDirectoryExist path
    unless directory (fail ("Expected a browser profile directory: " ++ path))
 original <- inventories resolveExclusions live live
 let backup = backupRoots destination
 createDirectory destination
 copyTree (dataRoot live) (dataRoot backup) (entries (dataInventory original))
 copyTree (configRoot live) (configRoot backup) (entries (configInventory original))
 copied <- backupInventories live backup
 unless (copied == original) (fail "Backup verification failed; original data are unchanged")
 pure (sha256 (encode original))

validateRoots :: FilePath -> UpdateRoots -> IO ()
validateRoots installation (UpdateRoots dataPath configPath) = do
 mapM_ validatePath [installation,dataPath,configPath]
 let overlaps a b = containsPath a b || containsPath b a
 when (overlaps dataPath configPath || any (overlaps installation) [dataPath,configPath]) $
  fail "Installation, Data and Config must be separate roots for updates"

backupInventories :: UpdateRoots -> UpdateRoots -> IO BackupInventory
backupInventories = inventories (\_ _ -> pure Set.empty)

inventories :: (FilePath -> [FilePath] -> IO (Set.Set FilePath)) -> UpdateRoots -> UpdateRoots -> IO BackupInventory
inventories resolve live source = do
 corpora <- managedRoots (dataRoot source)
 let dataPath = dataRoot source
     locks = map (makeRelative dataPath . (</> "host.lock")) corpora
     relative child = if child == dataPath then "" else makeRelative dataPath child
     profiles = [relative child </> profile | child <- corpora,
       profile <- reservationName:"corpora.lmdb.lock":"corpora.lmdb-lock":"corpora.lmdb.migration.lock":"corpora.lmdb.migrating.lock":"corpora.lmdb.migrating-lock":profileExclusions]
     configExcluded = configOwnerName:configWriterLockName:profileExclusions
 skippedData <- resolve dataPath (locks ++ profiles)
 savedData <- Inventory (dataRoot live) locks profiles <$> inventory dataPath skippedData
 skippedConfig <- resolve (configRoot source) configExcluded
 savedConfig <- Inventory (configRoot live) [] configExcluded <$> inventory (configRoot source) skippedConfig
 pure (BackupInventory savedData savedConfig)

copyTree :: FilePath -> FilePath -> [Entry] -> IO ()
copyTree source destination saved = do
 forM_ (map location saved) $ \(TreeEntry relative kind) -> case kind of
  DirectoryEntry -> createDirectoryIfMissing True (destination </> relative)
  FileEntry -> copyOwned (source </> relative) (destination </> relative)
 forM_ (map location saved) $ \(TreeEntry relative kind) ->
  copyMetadata (kind == DirectoryEntry) (source </> relative) (destination </> relative)

restoreSnapshot :: FilePath -> PendingUpdate -> IO () -> IO ()
restoreSnapshot installation pending settle = do
 let backup = backupRoots (backupPath installation (attempt pending))
     live = roots pending
     originalData = dataRoot live
     originalConfig = configRoot live
 validateRoots installation live
 withRootIdentity originalData $ \dataId -> withRootIdentity originalConfig $ \configId -> do
  unless (dataId == dataIdentity pending && configId == configIdentity pending) $
   fail "The original Data or Config directory was replaced. Recovery refused; backup retained."
  configOwner <- readBinding (originalConfig </> configOwnerName)
  unless (configOwner == Just installation) $
   fail "Config ownership changed. Recovery refused; backup retained."
  original <- backupInventories live backup
  unless (sha256 (encode original) == backupDigest pending) (fail "Backup changed or is incomplete; recovery refused")
  let childLocks = map (originalData </>) (nub (drop 1 (writerLocks (dataInventory original))))
  withLock (originalData </> "host.lock") $ do
   reservation <- readBinding (originalData </> reservationName)
   unless (reservation == Just (DataReservation installation (attempt pending))) $
    fail "Data Root update ownership changed. Recovery refused; backup retained."
   foldr withSurvivingLock (restore backup original >> settle) childLocks
 where
 restore backup original = do
  let savedData = dataInventory original
      savedConfig = configInventory original
      observe saved = do
       let root = inventoryRoot saved
       skipped <- resolveExclusions root (writerLocks saved ++ exclusions saved)
       existing <- tree root skipped
       pure (skipped,existing)
  (dataSkipped,liveData) <- observe savedData
  (configSkipped,liveConfig) <- observe savedConfig
  restoreTree (dataRoot backup) savedData dataSkipped liveData
  restoreTree (configRoot backup) savedConfig configSkipped liveConfig

restoreTree :: FilePath -> Inventory -> Set.Set FilePath -> [TreeEntry] -> IO ()
restoreTree backup saved skipped existing = do
 let root = inventoryRoot saved
 forM_ (removals saved skipped existing) $ \(TreeEntry relative kind) -> do
  let path = root </> relative
  case kind of
   FileEntry -> do
    permissions <- getPermissions path
    setPermissions path permissions {writable=True}
    removeFile path
   DirectoryEntry -> removeDirectory path
 copyTree backup root (entries saved)
 actual <- inventory root skipped
 unless (actual == entries saved) (fail "Recovery verification failed; original backup retained for retry")

removals :: Inventory -> Set.Set FilePath -> [TreeEntry] -> [TreeEntry]
removals saved skipped = filter shouldRemove . reverse
 where
 savedDirectories = Set.fromList [path | Entry (TreeEntry path DirectoryEntry) _ <- entries saved]
 shouldRemove (TreeEntry path kind) = not (null path) && case kind of
  FileEntry -> True
  DirectoryEntry -> not (Set.member path savedDirectories || any ((splitDirectories path `isPrefixOf`) . splitDirectories) skipped)
