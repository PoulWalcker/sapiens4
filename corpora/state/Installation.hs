-- Admission is keyed to the physical installed application, before resolving user storage.
module Installation
  ( Installation, appPath, statePath, installed, withInstallation, reserveUpdate, cancelUpdate, resumeUse, beginMaintenance
  , pendingPath, runtimePath, shellPath
  ) where

import Control.Concurrent.MVar
import Control.Exception
import qualified Data.ByteString as B
import Control.Monad
import System.Directory
import System.Environment (getExecutablePath)
import System.FileLock
import System.FilePath
import System.Info (os)
import Data.List (find)

data Installation = Installation
  { appPath :: FilePath
  , statePath :: FilePath
  , useLock :: MVar (Maybe FileLock)
  }

runtimePath :: FilePath -> FilePath
runtimePath app = app </> if os == "darwin" then "Contents/Resources/runtime" else "runtime"

shellPath :: FilePath -> FilePath
shellPath app = app </> if os == "darwin" then "Contents/MacOS/Sapiens4" else "Sapiens4.exe"

-- A path rule shared by *every* user/process. Shared Mac installations remain manual.
-- Determine the paths from the running image even while resources are being replaced.
installed :: FilePath -> Maybe FilePath
installed exe
  | os == "darwin" = do
      app <- find ((== ".app") . takeExtension) (parents (takeDirectory exe))
      case splitDirectories app of
        "/":"Users":_:"Applications":_ -> Just app
        _ -> Nothing
  | os == "mingw32" =
      let dir = takeDirectory exe
          runtime = takeDirectory (takeDirectory dir)
      in if equalFilePath (takeFileName runtime) "runtime" && equalFilePath (takeFileName dir) "bin"
           && equalFilePath (takeFileName (takeDirectory dir)) ".build"
         then Just (takeDirectory runtime)
         else if equalFilePath (takeFileName exe) "Sapiens4.exe" && not (equalFilePath (takeFileName dir) "bin")
              then Just dir else Nothing
  | otherwise = Nothing
  where parents path = path : [p | path /= takeDirectory path, p <- parents (takeDirectory path)]

pendingPath :: Installation -> FilePath
pendingPath owner = statePath owner </> "pending"

withInstallation :: (Maybe Installation -> IO a) -> IO a
withInstallation action = do
  path <- canonicalizePath =<< getExecutablePath
  case installed path of
    Nothing -> action Nothing
    Just app -> do
      let directory = takeDirectory app </> ("." ++ takeFileName app ++ ".update")
      createDirectoryIfMissing False directory
      bracket (open app directory) (\owner -> modifyMVar_ (useLock owner) (\lease -> mapM_ unlockFile lease >> pure Nothing)) (action . Just)
  where
    open app directory = withFileLock (directory </> "admission.lock") Exclusive $ \_ -> do
      blocked <- doesDirectoryExist (directory </> "pending")
      when blocked $ do
        let outcome = directory </> "outcome.txt"
        recorded <- doesFileExist outcome
        detail <- if recorded then readFile outcome else pure "An update is in progress."
        fail (detail ++ " If installation was interrupted, repair this installed copy before reopening.")
      lease <- lockFile (directory </> "use.lock") Shared
      Installation app directory <$> newMVar (Just lease)

-- Reserve admission while the existing application continues using its data.
reserveUpdate :: Installation -> String -> IO ()
reserveUpdate owner attempt = withFileLock (statePath owner </> "admission.lock") Exclusive $ \_ -> do
  createDirectory (pendingPath owner)
  B.writeFile (pendingPath owner </> attempt) B.empty `onException` removeDirectory (pendingPath owner)

cancelUpdate :: Installation -> String -> IO ()
cancelUpdate owner attempt = withFileLock (statePath owner </> "admission.lock") Exclusive $ \_ -> do
  let token = pendingPath owner </> attempt
  reserved <- doesFileExist token
  when reserved (removeFile token)
  pending <- doesDirectoryExist (pendingPath owner)
  when pending $ do
    contents <- listDirectory (pendingPath owner)
    when (null contents) (removeDirectory (pendingPath owner))

resumeUse :: Installation -> IO Bool
resumeUse owner = withFileLock (statePath owner </> "admission.lock") Exclusive $ \_ -> do
  pending <- doesDirectoryExist (pendingPath owner)
  if pending then pure False else do
    modifyMVar_ (useLock owner) $ \current -> case current of
      Just _ -> pure current
      Nothing -> Just <$> lockFile (statePath owner </> "use.lock") Shared
    pure True

-- New readers and promotions serialize on admission.lock. The original reader is
-- reacquired before opening admission on failure; pending survives loss of the shell.
beginMaintenance :: forall a. Installation -> IO a -> IO a
beginMaintenance owner launch = mask_ $ do
  outcome <-
   withFileLock (statePath owner </> "admission.lock") Exclusive $ \_ ->
    modifyMVar (useLock owner) $ \current -> case current of
      Nothing -> fail "Update is already in progress"
      Just reader -> do
        let path = statePath owner </> "use.lock"
        unlockFile reader
        outcome <- try $ bracket (tryLockFile path Exclusive) (mapM_ unlockFile) $ \writer ->
          case writer of
            Nothing -> fail "Another instance is using this installation. Close it and try again."
            Just _ -> do
              launch
        case (outcome :: Either IOException a) of
          Right result -> pure (Nothing, Right result)
          Left err -> do
            replacement <- lockFile path Shared
            -- Keep the restored lease alive until the shell exits.
            pure (Just replacement, Left err)
  either ioError pure outcome
