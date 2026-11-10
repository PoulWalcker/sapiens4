{-# LANGUAGE OverloadedStrings #-}
module RuntimePaths (configDirectory, defaultDataDirectory) where

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
   _ -> fail "Invalid package channel/identity"

profileRoot :: IO FilePath
profileRoot = do
 executable <- getExecutablePath
 let folder = takeDirectory executable
     manifests = [folder </> "../Resources/runtime/package-manifest.json",folder </> "runtime/package-manifest.json",folder </> "../../package-manifest.json"]
 present <- filterM doesFileExist manifests
 Profile name <- case present of
  [] -> pure (Profile "Sapiens4 Development")
  [path] -> either fail pure =<< eitherDecodeFileStrict path
  _ -> fail "Ambiguous package profile"
 home <- getHomeDirectory
 base <- if os=="mingw32" then lookupEnv "LOCALAPPDATA" >>= maybe (pure home) pure else pure (home </> "Library/Application Support")
 pure (base </> name)

configDirectory :: IO FilePath
configDirectory = lookupEnv "SAPIENS_CONFIG_DIR" >>= maybe ((</> "config") <$> profileRoot) makeAbsolute

defaultDataDirectory :: IO FilePath
defaultDataDirectory = lookupEnv "SAPIENS_DATA_DIR" >>= maybe ((</> "data") . takeDirectory <$> configDirectory) makeAbsolute
