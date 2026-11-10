module RuntimePaths (configDirectory, defaultDataDirectory) where

import System.Directory
import System.Environment
import System.FilePath
import System.Info

applicationDirectory :: IO FilePath
applicationDirectory = do
 home <- getHomeDirectory
 base <- case os of
  "mingw32" -> maybe home id <$> lookupEnv "LOCALAPPDATA"
  "darwin" -> pure (home </> "Library/Application Support")
  _ -> getXdgDirectory XdgData ""
 pure (base </> "Sapiens4")

configDirectory :: IO FilePath
configDirectory = lookupEnv "SAPIENS_CONFIG_DIR" >>= maybe ((</> "config") <$> applicationDirectory) makeAbsolute

defaultDataDirectory :: IO FilePath
defaultDataDirectory = lookupEnv "SAPIENS_DATA_DIR" >>= maybe ((</> "data") <$> applicationDirectory) makeAbsolute
