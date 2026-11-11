{-# LANGUAGE OverloadedStrings #-}
module RuntimePaths (configDirectory, defaultDataDirectory) where

import System.Directory
import System.Environment
import System.FilePath
import System.Info

-- The runtime argument is retained for native host callers; profile identity
-- is fixed and never depends on mutable package metadata.
profileRoot :: FilePath -> IO FilePath
profileRoot _ = do
 home <- getHomeDirectory
 base <- case os of
  "mingw32" -> maybe (home </> "AppData/Local") id <$> lookupEnv "LOCALAPPDATA"
  "darwin" -> pure (home </> "Library/Application Support")
  _ -> getXdgDirectory XdgData ""
 pure (base </> "Sapiens4")

configDirectory :: FilePath -> IO FilePath
configDirectory runtime = lookupEnv "SAPIENS_CONFIG_DIR" >>= maybe ((</> "config") <$> profileRoot runtime) makeAbsolute

defaultDataDirectory :: FilePath -> IO FilePath
defaultDataDirectory runtime = lookupEnv "SAPIENS_DATA_DIR" >>= maybe ((</> "data") <$> profileRoot runtime) makeAbsolute
