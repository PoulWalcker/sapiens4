{-# LANGUAGE OverloadedStrings #-}
module RuntimePaths (configDirectory, defaultDataDirectory, legacyDataDirectory) where

import Data.Aeson
import Control.Monad
import System.Directory
import System.Environment
import System.FilePath
import System.Info

newtype Profile = Profile String
instance FromJSON Profile where
 parseJSON = withObject "package profile" $ \o -> do
  channel <- o .: "channel"
  identity <- o .: "appId"
  case (channel :: String, identity :: String) of
   ("development","com.poulwalcker.sapiens4.development") -> pure (Profile "Sapiens4 Development")
   ("stable","com.poulwalcker.sapiens4") -> pure (Profile "Sapiens4")
   ("stable","com.sapiens4.desktop") -> pure (Profile "Sapiens4")
   _ -> fail "Invalid package channel/identity"

profileRoot :: IO FilePath
profileRoot = do
 executable <- getExecutablePath
 let folder = takeDirectory executable
     manifests = [folder </> "../Resources/runtime/package-manifest.json",folder </> "runtime/package-manifest.json",folder </> "../../package-manifest.json"]
 present <- filterM doesFileExist manifests
 Profile name <- case present of
  [] -> do
   portable <- or <$> mapM doesDirectoryExist [folder </> "runtime/.build/bin",folder </> "../Resources/runtime/.build/bin"]
   embedded <- doesFileExist (folder </> "../../app/shell/bootstrap.html")
   source <- doesFileExist (folder </> "../../build/cabal.project")
   when (portable || embedded && not source) (fail "The portable package is missing package-manifest.json. Extract a complete package.")
   pure (Profile "Sapiens4 Development")
  [path] -> either fail pure =<< eitherDecodeFileStrict path
  _ -> fail "Ambiguous package profile"
 home <- getHomeDirectory
 base <- case os of
  "mingw32" -> lookupEnv "LOCALAPPDATA" >>= maybe (pure home) pure
  "darwin" -> pure (home </> "Library/Application Support")
  _ -> getXdgDirectory XdgData ""
 pure (base </> name)

configDirectory :: IO FilePath
configDirectory = do
 supplied <- lookupEnv "SAPIENS_CONFIG_DIR"
 when (supplied==Just "") $ fail "SAPIENS_CONFIG_DIR must not be empty"
 maybe ((</> "config") <$> profileRoot) makeAbsolute supplied

defaultDataDirectory :: IO FilePath
defaultDataDirectory = do
 supplied <- lookupEnv "SAPIENS_DATA_DIR"
 when (supplied==Just "") $ fail "SAPIENS_DATA_DIR must not be empty"
 maybe ((</> "data") <$> profileRoot) makeAbsolute supplied

-- The specified Windows baseline used the stable profile root itself. It had
-- no development default; never discover stable data from a development build.
legacyDataDirectory :: IO (Maybe FilePath)
legacyDataDirectory = do
 root <- profileRoot
 pure (if os=="mingw32" && takeFileName root=="Sapiens4" then Just root else Nothing)
