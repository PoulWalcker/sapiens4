{-# LANGUAGE CPP #-}
-- v00.02.02 field-path LMDB -> v00.02.03 single-key LMDB. The previous store remains
-- beside the new one. A crash between the two renames resumes from that source.
module V00_02_03 (sourceVersion,targetVersion,prepare,recover,newOptions) where
import qualified Append4 as A
import qualified Append4.KV as K
import qualified Append4.Flat as F
import qualified Append4.Convert as Convert
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
sourceVersion = AppVersion "v00.02.02"
targetVersion = AppVersion "v00.02.03"

syncParent :: FilePath -> IO ()
#ifdef mingw32_HOST_OS
syncParent _ = pure ()
#else
syncParent p = bracket (P.openFd (takeDirectory p) P.ReadOnly P.defaultFileFlags) P.closeFd P.fileSynchronise
#endif

recover :: FilePath -> IO ()
recover target = do
 exists <- doesFileExist target
 previous <- doesFileExist (target<>".v00.02.02")
 when (not exists && previous) $ do
  -- The source was already verified before its rename; restoring its name is
  -- safe and lets prepare rebuild an interrupted stage rather than trust it.
  renameFile (target<>".v00.02.02") target
  syncParent target

prepare :: A.Options -> FilePath -> IO ()
prepare options target = do
 exists <- doesFileExist target
 when exists $ do
  old <- not <$> K.isSingleStorage target
  when old $ bracket (tryLockFile (target<.>"lock") Exclusive >>= maybe (fail "Store is already open") pure) unlockFile $ \_ -> do
   version <- A.getOfflineValue target versionKey
   unless (version==Just (String (versionText sourceVersion))) (fail "Flat migration requires v00.02.02")
   let stage=target<>".v00.02.03.staging"; backup=target<>".v00.02.02"
   -- Only this version owns this reserved stage. Never reuse an unverified or
   -- stale stage after a crash. All user input remains in target/backup.
   forM_ [stage,stage<>"-lock",stage<>".lock"] $ \p -> do
    present <- doesFileExist p
    when present (removeFile p)
   Convert.convert (newOptions options) target stage
   bracket (K.openStoreWith (newOptions options) stage) K.closeStore $ \store ->
    K.transaction store $ K.putValue store versionKey (F.TextAtom (versionText targetVersion))
   marker <- K.getOfflineValue stage versionKey
   unless (marker==Just (F.TextAtom (versionText targetVersion))) (fail "Flat migration marker verification failed")
   backed <- doesFileExist backup
   when backed (fail "A previous v00.02.02 backup already exists; originals retained")
   renameFile target backup
   syncParent target
   renameFile stage target
   syncParent target

newOptions :: A.Options -> K.Options
newOptions opts = K.Options (case A.historyPolicy opts of A.KeepHistory->K.KeepHistory;A.CurrentOnly->K.CurrentOnly) (A.initialMapBytes opts) (A.maximumMapBytes opts)
