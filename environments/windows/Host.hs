{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Host (Host(..), prepareHost, startHost, closeHost, watchHost, desktopArgs) where

import DesktopStorage (storageMode)
import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception
import Control.Monad
import Data.Aeson
import Data.Char (toUpper)
import Data.IORef
import Data.List (stripPrefix)
import Data.Maybe
import DataRoot (configDirectory, defaultDataDirectory, StorageChoice(..), startupMessage, containedPath)
import Network.HTTP.Client
import Network.URI
import Native (processId, openProcessForWait, waitProcessHandle, closeWaitHandle)
import Foreign.Ptr (nullPtr)
import qualified Data.ByteString as BS
import System.Directory
import System.Environment
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
  , outputReader :: Maybe (Async ())
  , exitWatcher :: Maybe (Async ())
  , errorReader :: Maybe (Async ())
  }

data HostPaths = HostPaths { dataPath :: FilePath, runtimePath :: FilePath }

desktopArgs :: [String] -> [String]
desktopArgs = concatMap split
 where split arg = case break (=='=') arg of
        (key,'=':value) | key `elem` ["--data-dir","--runtime-dir"] -> [key,value]
        _ -> [arg]

hostPaths :: [String] -> FilePath -> FilePath -> Maybe FilePath -> Maybe FilePath -> HostPaths
hostPaths args executable fallback dataEnv runtimeEnv = HostPaths
  (fromMaybe fallback (option "--data-dir" `mplus` dataEnv))
  (fromMaybe (takeDirectory executable </> "runtime") (option "--runtime-dir" `mplus` runtimeEnv))
  where option name = listToMaybe [value | (key,value) <- zip args (drop 1 args), key == name]

hostEnvironment :: [(String,String)] -> [(String,String)] -> [(String,String)]
hostEnvironment overrides inherited = overrides ++ filter keep inherited
  where
    owned = map (map toUpper . fst) overrides
    keep (key,_) = map toUpper key `notElem` owned

prepareHost :: [String] -> IO Host
prepareHost args = do
  executable <- getExecutablePath
  dataEnv <- lookupEnv "SAPIENS_DATA_DIR"
  runtimeEnv <- lookupEnv "SAPIENS_RUNTIME_DIR"
  let explicit = lookup "--data-dir" (zip args (drop 1 args)) `mplus` dataEnv
  fallback <- maybe defaultDataDirectory pure explicit
  let paths = hostPaths args executable fallback dataEnv runtimeEnv
  when (null (dataPath paths)) $ fail "Data directory must not be empty"
  when (null (runtimePath paths)) $ fail "Runtime directory must not be empty"
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
  let overrides = [("SAPIENS_STORAGE_MODE",storageMode choice),("SAPIENS_DESKTOP_PID", show pid),("SAPIENS_REPOSITORY",runtimeDirectory host),("SAPIENS_CONFIG_DIR",configRoot),("SAPIENS_DATA_DIR",dataDirectory host)]
  let command = (proc binary ["--port", "0", "--data-dir", dataDirectory host, "--desktop"])
        { cwd = Just (runtimeDirectory host), std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe
        , env = Just (hostEnvironment overrides environment)
        , create_group = True, use_process_jobs = True, detach_console = True }
  launchHost host command

launchHost :: Host -> CreateProcess -> IO String
launchHost host command = mask $ \restore -> do
  closeHost host
  (Just input, Just output, Just errors, child) <- createProcess command
  let owned = ServerProcess child input output errors Nothing Nothing Nothing
  writeIORef (serverProcess host) (Just owned)
  (do
    mapM_ (\stream -> hSetEncoding stream utf8 >> hSetNewlineMode stream universalNewlineMode) [input, output, errors]
    recent <- newIORef BS.empty
    let drainErrors = do
          bytes <- BS.hGetSome errors 4096
          unless (BS.null bytes) $ do
            modifyIORef' recent $ \old -> let combined=old<>bytes in BS.drop (BS.length combined-8192) combined
            drainErrors
    collect <- async (drainErrors `catch` \(_ :: IOException) -> pure ())
    writeIORef (serverProcess host) (Just owned {errorReader=Just collect})
    readiness <- newEmptyMVar
    drain <- async $ do
      result <- try (awaitHost host output) :: IO (Either SomeException String)
      putMVar readiness result
      case result of
        Left _ -> pure ()
        Right _ -> (forever (void $ hGetLine output)) `catch` \(_ :: IOException) -> pure ()
    writeIORef (serverProcess host) (Just owned {outputReader=Just drain,errorReader=Just collect})
    restore (takeMVar readiness >>= either (\e -> do
      void (timeout 250000 (waitCatch collect))
      bytes <- readIORef recent
      unless (BS.null bytes) $ do
        folder <- configDirectory
        logPath <- containedPath folder (folder </> "desktop-server.log")
        BS.writeFile logPath bytes
      throwIO e) pure)) `onException` closeHost host

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

watchHost :: Host -> IO () -> IO ()
watchHost host notify = do
 current <- readIORef (serverProcess host)
 forM_ current $ \server -> do
  pid <- getPid (processHandle server)
  handle <- maybe (pure nullPtr) (openProcessForWait 0x100000 0 . fromIntegral) pid
  let revoked = writeIORef (origin host) Nothing >> notify
  if handle==nullPtr then revoked else do
   observer <- async $ flip finally (void $ closeWaitHandle handle) $ do
    let wait = waitProcessHandle handle 250 >>= \result -> if result==258 then wait else revoked
    wait
   writeIORef (serverProcess host) (Just server {exitWatcher=Just observer})

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
      unless alive clear)

stopServer :: ServerProcess -> IO ()
stopServer server = mask_ $ do
  mapM_ cancel (exitWatcher server)
  stop
  mapM_ cancel (catMaybes [outputReader server,errorReader server]) `finally` mapM_ closeStream streams
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
