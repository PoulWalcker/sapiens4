{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Host (Host(..), prepareHost, startHost, closeHost) where

import Control.Concurrent.Async
import Control.Exception
import Control.Monad
import Data.Aeson
import Data.IORef
import Data.List (stripPrefix)
import Data.Maybe
import Network.HTTP.Client
import Network.URI
import Native (processId)
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
  , errorReader :: Async ()
  }

data HostPaths = HostPaths { dataPath :: FilePath, runtimePath :: FilePath }

hostPaths :: [String] -> FilePath -> FilePath -> Maybe FilePath -> Maybe FilePath -> HostPaths
hostPaths args executable local dataEnv runtimeEnv = HostPaths
  (fromMaybe (local </> "Sapiens4") (option "--data-dir" `mplus` dataEnv))
  (fromMaybe (takeDirectory executable </> "runtime") (option "--runtime-dir" `mplus` runtimeEnv))
  where option name = listToMaybe [value | (key,value) <- zip args (drop 1 args), key == name]

prepareHost :: [String] -> IO Host
prepareHost args = do
  executable <- getExecutablePath
  local <- lookupEnv "LOCALAPPDATA" >>= maybe getHomeDirectory pure
  dataEnv <- lookupEnv "SAPIENS_DATA_DIR"
  runtimeEnv <- lookupEnv "SAPIENS_RUNTIME_DIR"
  let paths = hostPaths args executable local dataEnv runtimeEnv
  directory <- makeAbsolute (dataPath paths)
  runtime <- makeAbsolute (runtimePath paths)
  Host directory runtime <$> newIORef Nothing <*> newIORef Nothing

startHost :: Host -> IO String
startHost host = mask $ \restore -> do
  createDirectoryIfMissing True (dataDirectory host)
  let binary = runtimeDirectory host </> ".build/bin/sapiens4.exe"
  available <- doesFileExist binary
  unless available $ ioError $ userError $ "Haskell runtime is missing. Extract the complete Sapiens4 package: " ++ binary
  environment <- getEnvironment
  pid <- processId
  (Just input, Just output, Just errors, child) <- createProcess (proc binary ["--port", "0", "--data-dir", dataDirectory host, "--desktop"])
    { cwd = Just (runtimeDirectory host), std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe
    , env = Just (("SAPIENS_DESKTOP_PID", show pid) : filter ((/= "SAPIENS_DESKTOP_PID") . fst) environment)
    , create_group = True, use_process_jobs = True, detach_console = True }
  let cleanup = terminateProcess child `finally` mapM_ closeStream [input, output, errors]
  mapM_ (\stream -> hSetEncoding stream utf8 >> hSetNewlineMode stream universalNewlineMode) [input, output, errors] `onException` cleanup
  errorText <- newIORef ""
  collect <- async $ (forever $ hGetLine errors >>= \line -> modifyIORef' errorText (\old -> let next = old ++ line ++ "\n" in drop (max 0 $ length next - 12000) next))
    `catch` \(_ :: IOException) -> pure ()
  writeIORef (serverProcess host) (Just (ServerProcess child input output errors collect))
  restore (awaitHost host output errorText) `onException` closeHost host

readyAddress :: String -> Maybe URI
readyAddress line = do
  uri <- stripPrefix "Sapiens4: " line >>= parseURI
  auth <- uriAuthority uri
  guard (uriScheme uri == "http:" && uriRegName auth == "127.0.0.1" && null (uriUserInfo auth))
  pure uri

awaitAddress :: Handle -> IORef String -> IO URI
awaitAddress output errors = do
  ended <- hIsEOF output
  when ended $ readIORef errors >>= ioError . userError . ("Sapiens4 server stopped. " ++)
  line <- hGetLine output
  maybe (awaitAddress output errors) pure (readyAddress line)

awaitHost :: Host -> Handle -> IORef String -> IO String
awaitHost host output errors = do
  result <- timeout 35000000 (awaitAddress output errors)
  uri <- maybe (ioError $ userError "Sapiens4 did not become ready within 35 seconds") pure result
  let base = uri { uriPath = "", uriQuery = "", uriFragment = "" }
  manager <- newManager defaultManagerSettings
  request <- parseRequest (show base ++ "/api/health")
  response <- httpLbs request { responseTimeout = responseTimeoutMicro 10000000 } manager
  case decode (responseBody response) of
    Just (Object fields) | K.lookup "status" fields == Just (String "ok") -> writeIORef (origin host) (Just base) >> pure (show uri)
    _ -> ioError $ userError "Server readiness check failed"

closeHost :: Host -> IO ()
closeHost host = do
  writeIORef (origin host) Nothing
  atomicModifyIORef' (serverProcess host) (\value -> (Nothing, value)) >>= mapM_ stopServer

stopServer :: ServerProcess -> IO ()
stopServer server = stop `finally` (cancel (errorReader server) `finally` mapM_ closeStream streams)
  where
    streams = [processInput server, processOutput server, processErrors server]
    stop = do
      let input = processInput server
      (hPutStrLn input "stop" >> hFlush input >> hClose input) `catch` \(_ :: IOException) -> pure ()
      result <- timeout 10000000 (waitForProcess (processHandle server))
      when (isNothing result) $ terminateProcess (processHandle server) `catch` \(_ :: IOException) -> pure ()

closeStream :: Handle -> IO ()
closeStream stream = hClose stream `catch` \(_ :: IOException) -> pure ()
