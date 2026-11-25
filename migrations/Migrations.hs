{-# LANGUAGE CPP #-}
module Migrations (openCorpora, openRelay, readRegistry, openPreviousVersioned, module MigrationVersion) where
import qualified Append4 as A
import qualified Append4.KV as K
import qualified Append4.Flat as Atom
import qualified AppStorage as App
import qualified V00_02_03 as Single
import Control.Exception
import Control.Monad
import Data.Aeson
import qualified Data.ByteString as B
import System.IO (withBinaryFile, IOMode(ReadMode))
import qualified Data.Map.Strict as M
import System.Directory
import System.FilePath
import System.FileLock
import MigrationVersion
import MigrationPlan
import qualified V00_02_02 as Flat
import qualified V00_02_03Files as TypedFiles
import qualified V00_01_99 as Old
import qualified V00_02_00 as Upgrade
import qualified V00_02_01 as Lifecycle
#ifndef mingw32_HOST_OS
import qualified System.Posix.IO as Posix
import qualified System.Posix.Unistd as Posix
#endif

-- The caller holds host.lock, including for managed children. No workers have
-- started yet. Registered migrations are compiled into every shipped binary.
openCorpora :: FilePath -> IO App.Store
openCorpora root = do
 store <- openVersioned A.defaultOptions (root </> "corpora.lmdb") (root </> "corpora.sqlite3") (Upgrade.corpora root) (Upgrade.corporaFingerprint root) Lifecycle.corpora
 TypedFiles.migrateIdentity (root </> "corpora-identity.json") `onException` App.closeStore store
 pure store

-- Keep accepting explicit old --db paths. The source always remains untouched.
openRelay :: FilePath -> IO App.Store
openRelay requested = do
 exists <- doesFileExist requested
 sqlite <- if exists then withBinaryFile requested ReadMode (\h -> (=="SQLite format 3\NUL") <$> B.hGet h 16) else pure False
 let legacyName = takeExtension requested `elem` [".sqlite3",".sqlite",".db"]
     legacyPath = sqlite || not exists && legacyName
     target = if legacyPath then (if legacyName then replaceExtension requested "lmdb" else requested<.>"lmdb") else requested
     source = if legacyPath then requested else replaceExtension requested "sqlite3"
 openVersioned A.defaultOptions {A.historyPolicy=A.CurrentOnly} target source (Upgrade.relay source) (Upgrade.relayFingerprint source) Lifecycle.relay

openVersioned :: A.Options -> FilePath -> FilePath -> (A.Store -> IO ()) -> IO Value -> (A.Store -> IO ()) -> IO App.Store
openVersioned options target source convert fingerprint cleanup = do
 createDirectoryIfMissing True (takeDirectory target)
 void(Old.safePath(takeDirectory target)(takeFileName(target<>".single-migration.lock")))
 -- The outer app lock excludes active writers. This lock spans both versioned
 -- format publication steps, including recovery after a rename.
 bracket (tryLockFile (target<>".single-migration.lock") Exclusive >>= maybe (fail "Another single-key migration is running") pure) unlockFile $ \_ -> do
  forM_ [target,target<>".v00.02.02",target<>".v00.02.03.staging"] $ \p -> void(Old.safePath(takeDirectory target)(takeFileName p))
  Single.recover target
  exists<-doesFileExist target
  single<-if exists then K.isSingleStorage target else pure False
  unless single $ do
   bracket (openPreviousVersioned options target source convert fingerprint cleanup) A.closeStore (const(pure()))
   Single.prepare options target
  marker<-K.getOfflineValue target versionKey
  unless(marker==Just(Atom.TextAtom(versionText currentVersion))) (fail "Incompatible single-key app version")
  App.openStoreWith (Single.newOptions options) target

openPreviousVersioned :: A.Options -> FilePath -> FilePath -> (A.Store -> IO ()) -> IO Value -> (A.Store -> IO ()) -> IO A.Store
openPreviousVersioned options target source convert sourceFingerprint cleanup = do
 createDirectoryIfMissing True (takeDirectory target)
 forM_ [target,source,target++".migration.lock",target++".migrating",target++".v00.02.01",target++".v00.02.02.staging"] $ \path ->
  void (Old.safePath (takeDirectory target) (takeFileName path))
 bracket (tryLockFile (target++".migration.lock") Exclusive >>= maybe (fail "Another app migration is running") pure) unlockFile $ \_ -> do
  let steps = [Migration Upgrade.sourceVersion Upgrade.targetVersion convert
              ,Migration Lifecycle.sourceVersion Lifecycle.targetVersion cleanup
              ,Migration Flat.sourceVersion Flat.targetVersion Flat.finish]
  Flat.recover target
  published <- doesFileExist target
  legacy <- doesFileExist source
  unless published $ when legacy $ do
   let staging=target++".migrating"
   -- The reserved staging file is resumable. Its version is committed together
   -- with all converted keys. No partially converted file is ever published.
   fingerprint <- sourceFingerprint
   Flat.recover staging
   oldStage <- doesFileExist staging >>= \exists -> if exists then A.isLegacyStorage staging else pure False
   expected <- bracket (A.openStoreWith options staging) A.closeStore $ \store -> do
    saved <- A.getValue store versionKey
    case saved of
     Nothing -> do
      entries <- A.entries <$> A.readSnapshot store
      unless (M.null entries) $ fail "Unrecognized migration staging file"
      A.transaction store $ do
       A.putValue store versionKey (String (versionText baselineVersion))
       A.putValue store "local/migration-source" (toJSON source)
       A.putValue store "local/migration-fingerprint" fingerprint
     Just (String value) -> void (either fail pure (plan steps (AppVersion value) Flat.targetVersion))
     _ -> fail "Unexpected staging version"
    owner <- A.getValue store "local/migration-source"
    unless (owner==Just (toJSON source)) $ fail "Staging file belongs to another source"
    previous <- A.getValue store "local/migration-fingerprint"
    when (previous/=Just fingerprint) $ A.transaction store $ do
     A.values store "" >>= mapM_ (A.deleteValue store . fst)
     A.putValue store versionKey (String (versionText baselineVersion))
     A.putValue store "local/migration-source" (toJSON source)
     A.putValue store "local/migration-fingerprint" fingerprint
    current <- storedVersion store
    runMigrations steps current (if oldStage then Flat.sourceVersion else Flat.targetVersion) store
    A.readSnapshot store
   unchanged <- sourceFingerprint
   unless (unchanged==fingerprint) $ fail "Legacy files changed during conversion; retry after stopping the old app"
   when oldStage (Flat.prepare options staging)
   let verified = if oldStage then expected { A.version = A.version expected+1
         , A.entries = M.insert versionKey (String (versionText Flat.targetVersion)) (A.entries expected) } else expected
   actual <- bracket (A.openStoreWith options staging) A.closeStore A.readSnapshot
   unless (actual==verified) $ fail "Migration read-back verification failed; originals retained"
   -- Both files are on the same volume. Sync the directory after publication.
   renameFile staging target
   syncDirectory (takeDirectory target)
  -- Never let an old app silently open a newer layout.
  when published $ do
   marker <- bracket (tryLockFile (target<.>"lock") Exclusive >>= maybe (fail "Store is already open") pure) unlockFile $ \_ ->
    A.getOfflineValue target versionKey
   case marker of
    Just (String value) -> void (either fail pure (plan steps (AppVersion value) Flat.targetVersion))
    Nothing | legacy -> fail "Both unversioned LMDB and legacy storage exist; refusing to guess which is authoritative"
    Nothing -> pure () -- pre-versioning PR91 LMDB: already the target layout
    _ -> fail "Invalid persisted app version"
  when published $ do
   oldFormat <- A.isLegacyStorage target
   when oldFormat $ do
    bracket (A.openStoreWith options target) A.closeStore $ \store -> do
     marker <- A.getValue store versionKey
     when (marker==Nothing) $ A.putValue store versionKey (String (versionText Upgrade.targetVersion))
     current <- storedVersion store
     runMigrations steps current Flat.sourceVersion store
    Flat.prepare options target
  bracketOnError (A.openStoreWith options target) A.closeStore $ \store -> do
   marker <- A.getValue store versionKey
   when (marker==Nothing) $ A.putValue store versionKey (String (versionText (if published then Upgrade.targetVersion else Flat.targetVersion)))
   current <- storedVersion store
   runMigrations steps current Flat.targetVersion store
   pure store

storedVersion :: A.Store -> IO AppVersion
storedVersion store = A.getValue store versionKey >>= \saved -> case saved of
 Just (String value) -> pure (AppVersion value)
 _ -> fail "Missing persisted app version"

syncDirectory :: FilePath -> IO ()
#ifdef mingw32_HOST_OS
syncDirectory _ = pure () -- LMDB synchronously flushes data; rename is same-volume.
#else
syncDirectory path = bracket (Posix.openFd path Posix.ReadOnly Posix.defaultFileFlags) Posix.closeFd Posix.fileSynchronise
#endif

-- Backup inspection must work before conversion too, without modifying sources.
readRegistry :: FilePath -> IO (Maybe Value)
readRegistry root = do
 database <- Old.safePath root "corpora.lmdb"
 current <- doesFileExist database
 if current then do
  single<-K.isSingleStorage database
  if single then App.getOfflineValue database "local/registry" else A.getOfflineValue database "local/registry"
 else do
  path <- Old.safePath root "host-corpora.json"
  exists <- doesFileExist path
  if exists then Just <$> Old.readJSON path else pure Nothing
