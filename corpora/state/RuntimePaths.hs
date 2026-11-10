{-# LANGUAGE OverloadedStrings #-}
module RuntimePaths (configDirectory, defaultDataDirectory) where

import Data.Aeson
import System.Directory
import System.Environment
import System.FilePath
import System.Info

newtype Profile = Profile FilePath
instance FromJSON Profile where
 parseJSON = withObject "package profile" $ \o -> do
  channel <- o .: "channel"
  identity <- o .: "appId"
  case (channel :: String, identity :: String) of
   ("development","com.poulwalcker.sapiens4.development") -> pure (Profile "Sapiens4 Development")
   ("stable","com.poulwalcker.sapiens4") -> pure (Profile "Sapiens4")
   _ -> fail "Invalid package channel/identity"

profileRoot :: FilePath -> IO FilePath
profileRoot runtime = do
 Profile name <- either fail pure =<< eitherDecodeFileStrict (runtime </> "package-manifest.json")
 home <- getHomeDirectory
 base <- case os of
  "mingw32" -> maybe home id <$> lookupEnv "LOCALAPPDATA"
  "darwin" -> pure (home </> "Library/Application Support")
  _ -> getXdgDirectory XdgData ""
 pure (base </> name)

configDirectory :: FilePath -> IO FilePath
configDirectory runtime = lookupEnv "SAPIENS_CONFIG_DIR" >>= maybe ((</> "config") <$> profileRoot runtime) makeAbsolute

defaultDataDirectory :: FilePath -> IO FilePath
defaultDataDirectory runtime = lookupEnv "SAPIENS_DATA_DIR" >>= maybe ((</> "data") <$> profileRoot runtime) makeAbsolute
