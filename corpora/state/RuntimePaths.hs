{-# LANGUAGE OverloadedStrings #-}
module RuntimePaths (configDirectory, defaultDataDirectory, externalStoragePath, containedPath) where

import qualified V00_02_03Files as TypedFiles
import Data.Aeson
import Control.Monad
import Data.Char (toLower)
import Data.List (find)
import Data.Maybe (catMaybes)
import System.Directory
import System.Environment
import System.FilePath
import System.IO.Error (catchIOError, isDoesNotExistError)
import System.Info
import UpdateState (configOwnerName, containsPath, installationOf, readBinding, statePath)

newtype Profile = Profile String
instance FromJSON Profile where
 parseJSON = withObject "package profile" $ \o -> do
  channel <- o .: "channel"
  identity <- o .: "appId"
  case (channel :: String, identity :: String) of
   ("stable","com.sapiens4.desktop") -> pure (Profile "Sapiens4")
   _ -> fail "Invalid package channel/identity"

ancestors :: FilePath -> [FilePath]
ancestors path = path : if parent == path then [] else ancestors parent
 where parent = takeDirectory path

storagePath :: FilePath -> IO FilePath
storagePath path = do
 absolute <- makeAbsolute path
 let check p = do
      exists <- doesPathExist p
      unless exists $ do
       link <- pathIsSymbolicLink p `catchIOError` \e ->
        if isDoesNotExistError e then pure False else ioError e
       when link $ fail "A storage path contains a broken link. Reconnect it before starting."
       let parent = takeDirectory p
       unless (parent==p) (check parent)
 check absolute
 canonicalizePath absolute

containedPath :: FilePath -> FilePath -> IO FilePath
containedPath root path = do
 parent <- storagePath root
 full <- storagePath path
 unless (any (equalFilePath parent) (drop 1 (ancestors full))) $
  fail "A managed data path points outside its directory. Restore the complete Data Root."
 pure full

externalStoragePath :: FilePath -> IO FilePath
externalStoragePath path = do
 full <- storagePath path
 executable <- getExecutablePath >>= canonicalizePath
 installed <- installationOf executable
 let bin = takeDirectory executable
     bundle = find ((==".app") . map toLower . takeExtension) (ancestors bin)
     runtime = takeDirectory (takeDirectory bin)
 launcher <- doesDirectoryExist (bin </> "runtime")
 let backend = takeFileName runtime == "runtime" && takeFileName (takeDirectory bin) == ".build"
     packages = catMaybes [bundle] ++ [bin | os=="mingw32" && launcher] ++ [takeDirectory runtime | os=="mingw32" && backend]
 let foldCase = if os `elem` ["mingw32","darwin"] then map toLower else id
     inside package = any (equalFilePath (foldCase package) . foldCase) (ancestors full)
     overlaps (root,_) = containsPath root full || containsPath full root
 insideInstallation <- or <$> forM (ancestors full) (\parent -> do
  marked <- doesFileExist (statePath parent)
  if marked then doesDirectoryExist (parent </> "versions") else pure False)
 when (any inside packages || any overlaps installed || insideInstallation) $
  fail "Choose a data or configuration folder outside the Sapiens installation."
 pure full

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
 directory <- maybe ((</> "config") <$> profileRoot) makeAbsolute supplied >>= externalStoragePath
 owner <- readBinding (directory </> configOwnerName) :: IO (Maybe FilePath)
 forM_ owner $ \_ -> do
  installed <- getExecutablePath >>= installationOf
  when (null installed) (fail "This Config folder belongs to an installed Sapiens. Choose a separate Config folder.")
 TypedFiles.migrateConfig directory
 pure directory

defaultDataDirectory :: IO FilePath
defaultDataDirectory = do
 supplied <- lookupEnv "SAPIENS_DATA_DIR"
 when (supplied==Just "") $ fail "SAPIENS_DATA_DIR must not be empty"
 maybe ((</> "data") <$> profileRoot) makeAbsolute supplied >>= externalStoragePath
