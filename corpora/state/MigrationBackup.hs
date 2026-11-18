{-# LANGUAGE DeriveGeneric #-}
module MigrationBackup (withMigrationBackup, checkNoPending, restoreData, hasOriginal) where

import Control.Exception
import Control.Monad
import Data.Aeson
import qualified Data.ByteString as B
import Data.List (nub, sort)
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import GHC.Generics (Generic)
import DataRoot (storageLock)
import System.FileLock (unlockFile)
import MigrationFiles
import System.Directory
import System.FilePath
import System.IO.Error
import qualified Types

-- Runtime is admitted after Migrated, independently of the later UI Ready signal.
-- Restoring owns a complete newer copy and the exact union of participant locks.
data Phase = Migrating | Migrated | Restoring FilePath [Member] | Restored FilePath
  deriving (Eq, Show, Generic)
instance FromJSON Phase
instance ToJSON Phase
data Member = Member { folder :: FilePath, directoryId :: String, lockId :: String }
  deriving (Eq, Show, Generic)
instance FromJSON Member
instance ToJSON Member
data Receipt = Receipt { source :: FilePath, participants :: [Member], updateAttempt :: Maybe String, phase :: Phase }
  deriving (Generic)
instance FromJSON Receipt
instance ToJSON Receipt

-- A directly opened child finds its ancestor's incomplete operation, even when
-- a partially restored registry is unreadable. Roots and host.lock are never replaced.
treeTop path
  | takeFileName parent == "corporas", validId (takeFileName path) = treeTop (takeDirectory parent)
  | otherwise = path
  where parent = takeDirectory path
validId s = Just s == (UUID.toString <$> UUID.fromString s)
area root = let top = treeTop root in takeDirectory top </> ("." ++ takeFileName top ++ ".sapiens-migrations")
childOf parent path = case splitDirectories (makeRelative parent path) of
  "corporas":i:rest -> validId i && (null rest || childOf (parent </> "corporas" </> i) path)
  _ -> False
related a b = equalFilePath a b || childOf a b || childOf b a
ensure b reason = unless b $ fail reason
missing e = if isDoesNotExistError e then Just () else Nothing
noLink path = do
  resolved <- canonicalizePath path
  link <- pathIsSymbolicLink path
  ensure (equalFilePath resolved path && not link) ("Migration path was redirected: " ++ path)
member path = do
  let lock = path </> "host.lock"
  mapM_ noLink [path,lock]
  Member path <$> fileIdentity path <*> fileIdentity lock
lockPaths root members = [makeRelative root (folder m </> "host.lock") | m <- members]
snapshot dir = dir </> "original"
record dir receipt = Types.writeJSON (dir </> "record.json") receipt

receipts root = do
  let base = area root
  names <- tryJust missing (noLink base >> listDirectory base)
  case names of
    Left () -> pure []
    Right ids -> fmap concat $ forM (sort ids) $ \ident -> do
      ensure (validId ident) ("Unrecognized migration directory: " ++ base </> ident)
      let dir = base </> ident
          path = dir </> "record.json"
      noLink dir
      found <- tryJust missing $ do
        noLink path
        bytes <- B.readFile path
        either (fail . (("Invalid migration record " ++ path ++ ": ") ++)) pure (eitherDecodeStrict bytes)
      case found of
        Left () -> pure [] -- Unpublished preparations never authorize live writes or restore.
        Right r -> do
          let paths = map folder (participants r)
              inside p = p == source r || childOf (source r) p
          ensure (equalFilePath (area (source r)) base && source r `elem` paths &&
                  length paths == length (nub paths) && all inside paths)
            ("Invalid migration participants: " ++ path)
          case phase r of
            Restoring newer members -> ensure
              (validId (takeFileName newer) && takeDirectory newer == dir &&
               all (inside . folder) members && all (`elem` members) (participants r))
              ("Invalid restoration record: " ++ path)
            _ -> pure ()
          pure [(dir,r) | related root (source r)]

incomplete Migrating = True
incomplete (Restoring _ _) = True
incomplete _ = False

checkNoPending :: FilePath -> IO ()
checkNoPending root = do
  active <- filter (incomplete . phase . snd) <$> receipts root
  forM_ active $ \(dir,r) -> fail ("Data recovery required at " ++ source r ++
    ". Original: " ++ snapshot dir ++ ". Explicit recovery: sapiens4 restore-data " ++
    show (source r) ++ " " ++ takeFileName dir ++ ". No data were restored automatically.")

hasOriginal :: String -> FilePath -> IO Bool
hasOriginal attempt root = any (\(_,r) -> root `elem` map folder (participants r) && updateAttempt r == Just attempt && phase r == Migrated) <$> receipts root

withMigrationBackup :: Maybe String -> FilePath -> [FilePath] -> IO () -> IO ()
-- Caller holds every participant lock, with all SQLite connections closed.
withMigrationBackup attempt root paths convert = do
  checkNoPending root
  let base = area root
  exists <- doesDirectoryExist base
  unless exists $ privateDirectory base `catchIOError` \e -> do
    raced <- doesDirectoryExist base
    unless raced (ioError e)
  noLink base
  syncEntry True (takeDirectory base)
  ident <- UUID.toString <$> UUID.nextRandom
  let dir = base </> ident
  privateDirectory dir
  syncEntry True base
  members <- mapM member paths
  privateDirectory (snapshot dir)
  snapshotTree (lockPaths root members) root (snapshot dir)
  let receipt = Receipt root members attempt Migrating
  record dir receipt
  -- Failure leaves the immutable original and a closed runtime. No automatic rollback.
  convert
  syncTree (lockPaths root members) root
  record dir receipt {phase = Migrated}

-- The live registry may be precisely what an interrupted migration damaged.
-- Lock every physical child, including newer/unregistered Corpora, before saving
-- and restoring the tree. Existing receipts still require the original identities.
recoveryTree :: FilePath -> IO [FilePath]
recoveryTree root = do
  noLink root
  let children = root </> "corporas"
  exists <- doesPathExist children
  descendants <- if not exists then pure [] else do
    noLink children
    names <- sort <$> listDirectory children
    fmap concat $ forM (filter validId names) $ \name -> do
      let child = children </> name
      directory <- doesDirectoryExist child
      if directory then recoveryTree child else pure []
  pure (root:descendants)

restoreData :: FilePath -> String -> IO ()
restoreData root ident = do
  ensure (validId ident) "Invalid migration recovery identifier."
  records <- receipts root
  (dir,seen) <- case [(d,r) | (d,r) <- records, takeFileName d == ident, source r == root] of
    [entry] -> pure entry
    _ -> fail "No recovery original for this Data Root and operation."
  case phase seen of
    Restored _ -> fail "This original was already restored; it cannot overwrite newer work again."
    _ -> pure ()
  ensure (all (\(d,r) -> d == dir || not (incomplete (phase r))) records) "Another migration or restoration needs recovery first."
  paths <- case phase seen of
    Restoring _ members -> pure (map folder members)
    _ -> nub . (map folder (participants seen) ++) <$> recoveryTree root
  let required = case phase seen of
        Restoring _ locked -> locked
        _ -> participants seen
  actual <- mapM (member . folder) required
  ensure (actual == required) "Original Data Root or lock identity changed."
  locations <- mapM fileIdentity paths
  -- Only unpublished/newer children may need a lock created. Original and
  -- interrupted-restoration lock identities were checked above and again below.
  let lock path action = bracket (storageLock path) unlockFile (const action)
  foldr lock (restoreLocked dir seen required paths locations) paths
  where
  restoreLocked dir seen required paths locations = do
    members <- mapM member paths
    ensure (map directoryId members == locations && all (`elem` members) required)
      "Recovery participant changed while acquiring locks."
    current <- receipts root
    ensure (all (\(d,r) -> d == dir || not (incomplete (phase r))) current) "Another migration or restoration needs recovery first."
    receipt <- maybe (fail "The recovery record disappeared.") pure (lookup dir current)
    ensure (phase receipt == phase seen) "Recovery state changed; retry explicitly."
    newer <- case phase receipt of
      Restoring saved locked -> do
        ensure (locked == members) "Recovery lock set changed."
        noLink saved
        pure saved
      _ -> do
        now <- recoveryTree root
        ensure (all (`elem` map folder members) now) "The Corpora directories changed; retry recovery."
        nonce <- UUID.toString <$> UUID.nextRandom
        let saved = dir </> nonce
        privateDirectory saved
        snapshotTree (lockPaths root members) root saved
        -- A prepared copy is reusable ONLY after this durable gate. A crash before
        -- it leaves an orphan; a later attempt captures all newer user work afresh.
        record dir receipt {phase = Restoring saved members}
        pure saved
    noLink (snapshot dir)
    restoreTree (lockPaths root members) (snapshot dir) root
    record dir receipt {phase = Restored newer}
    putStrLn ("Original data restored. Newer data preserved at " ++ newer ++ ". Reinstall the previous application before opening these data.")
