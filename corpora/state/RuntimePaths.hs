{-# LANGUAGE OverloadedStrings #-}
module RuntimePaths (configDirectory, defaultDataDirectory) where

import System.Directory
import Control.Monad (when)
import System.Environment
import System.FilePath
import System.Info

profileRoot :: FilePath -> IO FilePath
profileRoot _ = do
 home <- getHomeDirectory
 base <- case os of
  "mingw32" -> maybe (home </> "AppData/Local") id <$> lookupEnv "LOCALAPPDATA"
  "darwin" -> pure (home </> "Library/Application Support")
  _ -> getXdgDirectory XdgData ""
 pure (base </> "Sapiens4")

configDirectory :: FilePath -> IO FilePath
configDirectory runtime = do
 supplied <- lookupEnv "SAPIENS_CONFIG_DIR"
 when (supplied==Just "") $ fail "SAPIENS_CONFIG_DIR must not be empty"
 maybe ((</> "config") <$> profileRoot runtime) makeAbsolute supplied

defaultDataDirectory :: FilePath -> IO FilePath
defaultDataDirectory runtime = do
 supplied <- lookupEnv "SAPIENS_DATA_DIR"
 when (supplied==Just "") $ fail "SAPIENS_DATA_DIR must not be empty"
 maybe ((</> "data") <$> profileRoot runtime) makeAbsolute supplied
