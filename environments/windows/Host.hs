{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Host (Host(..), prepareHost, startHost, checkStorage, closeHost) where

import Control.Concurrent.Async
import Control.Exception
import Control.Monad
import Data.Aeson
import Data.Char (toUpper)
import Data.IORef
import Data.List (stripPrefix)
import Data.Maybe
import DataRoot (configDirectory, defaultDataDirectory, StorageChoice(..), startupMessage)
import Network.HTTP.Client
import Network.URI
import Native (processId)
import System.Directory
import System.Environment
import System.Exit (ExitCode(..))
import System.FilePath
import System.IO
import System.Process
import System.Timeout
import qualified Data.Aeson.KeyMap as K

data Host = Host
  { dataDirectory :: FilePath
  , runtimeDirectory :: FilePath
  , serverProcess :: IORef (Maybe ServerProcess)
  , origin :: IORef (Maybe URI)
  }

data ServerProcess = ServerProcess
  { processHandle :: ProcessHandle
  , processInput :: Handle
  , processOutput :: Handle
  , processErrors :: Handle
  , errorReader :: Async ()
  }

data HostPaths = HostPaths { dataPath :: FilePath, runtimePath :: FilePath }

hostPaths :: [String] -> FilePath -> FilePath -> Maybe FilePath -> Maybe FilePath -> HostPaths
hostPaths args executable fallback dataEnv runtimeEnv = HostPaths
  (fromMaybe fallback (option "--data-dir" `mplus` dataEnv))
  (fromMaybe (takeDirectory executable </> "runtime") (option "--runtime-dir" `mplus` runtimeEnv))
  where option name = listToMaybe [value | (key,value) <- zip args (drop 1 args), key == name]

hostEnvironment :: [(String,String)] -> [(String,String)] -> [(String,String)]
-- Windows names are case-insensitive; leave explicit path values unchanged.
hostEnvironment overrides inherited = overrides ++ filter keep inherited
  where
    owned = map (map toUpper . fst) overrides
    keep (key,_) = map toUpper key `notElem` owned

prepareHost :: [String] -> IO Host
prepareHost args = do
  executable <- getExecutablePath
  fallback <- defaultDataDirectory
  dataEnv <- lookupEnv "SAPIENS_DATA_DIR"
  runtimeEnv <- lookupEnv "SAPIENS_RUNTIME_DIR"
  let paths = hostPaths args executable fallback dataEnv runtimeEnv
  directory <- makeAbsolute (dataPath paths)
  runtime <- makeAbsolute (runtimePath paths)
  Host directory runtime <$> newIORef Nothing <*> newIORef Nothing

startHost :: StorageChoice -> Host -> IO String
startHost choice host = do
  let binary = runtimeDirectory host </> ".build/bin/sapiens4.exe"
  available <- doesFileExist binary
  unless available $ ioError $ userError $ "Haskell runtime is missing. Extract the complete Sapiens4 package: " ++ binary
  environment <- getEnvironment
  configRoot <- configDirectory
  createDirectoryIfMissing True configRoot
  pid <- processId
  let overrides = [("SAPIENS_STORAGE_MODE",if choice==StartNew then "new" else "existing"),("SAPIENS_DESKTOP_PID", show pid),("SAPIENS_REPOSITORY",runtimeDirectory host),("SAPIENS_CONFIG_DIR",configRoot),("SAPIENS_DATA_DIR",dataDirectory host)]
  let command = (proc binary ["--port", "0", "--data-dir", dataDirectory host, "--desktop"])
        { cwd = Just (runtimeDirectory host), std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe
        , env = Just (hostEnvironment overrides environment)
        , create_group = True, use_process_jobs = True, detach_console = True }
  launchHost host command

launchHost :: Host -> CreateProcess -> IO String
launchHost host command = mask $ \restore -> do
  closeHost host
  (Just input, Just output, Just errors, child) <- createProcess command
  collect <- async $ (do
    hSetEncoding errors utf8
    hSetNewlineMode errors universalNewlineMode
    forever (void $ hGetLine errors)
    `catch` \(_ :: IOException) -> pure ())
  writeIORef (serverProcess host) (Just (ServerProcess child input output errors collect))
  (do
    mapM_ (\stream -> hSetEncoding stream utf8 >> hSetNewlineMode stream universalNewlineMode) [input, output]
    restore (awaitHost host output)) `onException` closeHost host

checkStorage :: Host -> StorageChoice -> IO ()
checkStorage host choice = do
  let binary = runtimeDirectory host </> ".build/bin/sapiens4.exe"
      mode = if choice == StartNew then "new" else "existing"
  result <- timeout 35000000 $ readCreateProcessWithExitCode
    (proc binary ["check-data",dataDirectory host,mode])
      { cwd=Just (runtimeDirectory host), detach_console=True, use_process_jobs=True } ""
  (code,_,errors) <- maybe (ioError $ userError "Storage validation did not finish within 35 seconds.") pure result
  unless (code==ExitSuccess) $ ioError $ userError $ if null errors then "This data directory could not be opened." else errors

readyAddress :: String -> Maybe URI
readyAddress line = do
  uri <- stripPrefix "Sapiens4: " line >>= parseURI
  auth <- uriAuthority uri
  guard (uriScheme uri == "http:" && uriRegName auth == "127.0.0.1" && null (uriUserInfo auth))
  pure uri

awaitAddress :: Handle -> IO URI
awaitAddress output = do
  ended <- hIsEOF output
  when ended $ ioError $ userError startupMessage
  line <- hGetLine output
  maybe (awaitAddress output) pure (readyAddress line)

awaitHost :: Host -> Handle -> IO String
awaitHost host output = do
  result <- timeout 35000000 (awaitAddress output)
  uri <- maybe (ioError $ userError "Sapiens4 did not become ready within 35 seconds") pure result
  let base = uri { uriPath = "", uriQuery = "", uriFragment = "" }
  manager <- newManager defaultManagerSettings
  request <- parseRequest (show base ++ "/api/health")
  response <- httpLbs request { responseTimeout = responseTimeoutMicro 10000000 } manager
  case decode (responseBody response) of
    Just (Object fields) | K.lookup "status" fields == Just (String "ok") -> writeIORef (origin host) (Just base) >> pure (show uri)
    _ -> ioError $ userError "Server readiness check failed"

serverRunning :: Maybe ServerProcess -> IO Bool
serverRunning = maybe (pure False) (fmap isNothing . getProcessExitCode . processHandle)

closeHost :: Host -> IO ()
closeHost host = mask_ $ do
  current <- readIORef (serverProcess host)
  let clear = writeIORef (serverProcess host) Nothing >> writeIORef (origin host) Nothing
  case current of
    Nothing -> clear
    Just server -> stopServer server `finally` (do
      alive <- serverRunning current
      -- Retain ownership if termination failed and the process is still alive.
      -- A stopped process must never remain the active server after an error.
      unless alive clear)

stopServer :: ServerProcess -> IO ()
stopServer server = stop `finally` (cancel (errorReader server) `finally` mapM_ closeStream streams)
  where
    streams = [processInput server, processOutput server, processErrors server]
    stop = do
      let input = processInput server
      (hPutStrLn input "stop" >> hFlush input >> hClose input) `catch` \(_ :: IOException) -> pure ()
      result <- timeout 10000000 (waitForProcess (processHandle server))
      when (isNothing result) $ do
        terminateProcess (processHandle server)
        confirmed <- timeout 5000000 (waitForProcess (processHandle server))
        when (isNothing confirmed) $ ioError $ userError "The application server could not be stopped"

closeStream :: Handle -> IO ()
closeStream stream = hClose stream `catch` \(_ :: IOException) -> pure ()
