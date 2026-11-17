-- Admission is keyed to the physical installed application, before resolving user storage.
module Installation
  ( Installation, appPath, statePath, installed, withInstallation, beginMaintenance
  , sameFormatContract, sameFormatFamily, strictStartup, pendingPath, runtimePath, shellPath
  ) where

import Control.Concurrent.MVar
import Control.Exception
import Data.Aeson
import Data.Aeson.Types (parseEither, Parser)
import qualified Data.ByteString as B
import System.IO.Error (isDoesNotExistError)
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

sameFormatContract, sameFormatFamily :: String
sameFormatContract = "same-format-v1"
sameFormatFamily = "sapiens4-2026-11"

strictStartup :: Installation -> IO Bool
strictStartup owner = do
  bytes <- tryJust (\e -> if isDoesNotExistError e then Just () else Nothing) $
    B.readFile (statePath owner </> "same-format")
  case bytes of
    Left () -> pure False
    Right marker -> do
      (target, contract) <- parse marker
      (running, declared) <- parse =<< B.readFile (runtimePath (appPath owner) </> "desktop-update.json")
      unless (contract == sameFormatContract && declared == sameFormatContract) $
        fail "Unknown installed update contract"
      pure (target == running)
  where
    parse input = either fail pure $ eitherDecodeStrict input >>= parseEither
      (withObject "desktop update contract" $ \o -> (,) <$> (o .: "version" :: Parser String) <*> o .: "contract")

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
              createDirectory (pendingPath owner)
              launch `onException` removeDirectory (pendingPath owner)
        case (outcome :: Either IOException a) of
          Right result -> pure (Nothing, Right result)
          Left err -> do
            replacement <- lockFile path Shared
            -- Keep the restored lease alive until the shell exits.
            pure (Just replacement, Left err)
  either ioError pure outcome
