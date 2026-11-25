{-# LANGUAGE CPP #-}
-- v00.02.01 JSON LMDB -> v00.02.02 flat typed LMDB. The previous store remains
-- beside the new one. A crash between the two renames resumes from that source.
module V00_02_02 (sourceVersion,targetVersion,prepare,recover,finish) where
import qualified Append4 as A
import Control.Exception
import Control.Monad
import Data.Aeson
import System.Directory
import System.FilePath
import System.FileLock
import MigrationVersion
#ifndef mingw32_HOST_OS
import qualified System.Posix.IO as P
import qualified System.Posix.Unistd as P
#endif

sourceVersion,targetVersion :: AppVersion
sourceVersion = AppVersion "v00.02.01"
targetVersion = AppVersion "v00.02.02"

syncParent :: FilePath -> IO ()
#ifdef mingw32_HOST_OS
syncParent _ = pure ()
#else
syncParent p = bracket (P.openFd (takeDirectory p) P.ReadOnly P.defaultFileFlags) P.closeFd P.fileSynchronise
#endif

recover :: FilePath -> IO ()
recover target = do
 exists <- doesFileExist target
 previous <- doesFileExist (target<>".v00.02.01")
 when (not exists && previous) $ do
  -- The source was already verified before its rename; restoring its name is
  -- safe and lets prepare rebuild an interrupted stage rather than trust it.
  renameFile (target<>".v00.02.01") target
  syncParent target

prepare :: A.Options -> FilePath -> IO ()
prepare options target = do
 exists <- doesFileExist target
 when exists $ do
  old <- A.isLegacyStorage target
  when old $ bracket (tryLockFile (target<.>"lock") Exclusive >>= maybe (fail "Store is already open") pure) unlockFile $ \_ -> do
   version <- A.getOfflineValue target versionKey
   unless (version==Just (String (versionText sourceVersion))) (fail "Flat migration requires v00.02.01")
   let stage=target<>".v00.02.02.staging"; backup=target<>".v00.02.01"
   -- Only this version owns this reserved stage. Never reuse an unverified or
   -- stale stage after a crash. All user input remains in target/backup.
   forM_ [stage,stage<>"-lock",stage<>".lock"] $ \p -> do
    present <- doesFileExist p
    when present (removeFile p)
   A.convertLegacyStorage options target stage
   bracket (A.openStoreWith options stage) A.closeStore $ \store ->
    A.transaction store $ A.putValue store versionKey (String (versionText targetVersion))
   marker <- A.getOfflineValue stage versionKey
   unless (marker==Just (String (versionText targetVersion))) (fail "Flat migration marker verification failed")
   backed <- doesFileExist backup
   when backed (fail "A previous v00.02.01 backup already exists; originals retained")
   renameFile target backup
   syncParent target
   renameFile stage target
   syncParent target

-- New/SQLite-imported stores already use typed cells; still register an explicit
-- sequential marker step. Legacy file conversion is completed by prepare first.
finish :: A.Store -> IO ()
finish _ = pure ()
