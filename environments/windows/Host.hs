{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Host (Host(..), prepareHost, startHost, closeHost) where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception
import Control.Monad
import Data.Char (toUpper)
import Data.IORef
import Data.List (stripPrefix)
import Data.Maybe
import Network.URI
import Native (processId)
import qualified RuntimePaths as Paths
import System.Directory
import System.Environment
import System.FilePath
import System.IO
import System.Process
import System.Timeout

data Host = Host
  { dataDirectory :: FilePath
  , configDirectory :: FilePath
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
  , errorReader :: Maybe (Async ())
  }

hostEnvironment :: [(String,String)] -> [(String,String)] -> [(String,String)]
hostEnvironment overrides inherited = overrides ++ filter keep inherited
  where
    owned = map (map toUpper . fst) overrides
    keep (key,_) = map toUpper key `notElem` owned

prepareHost :: [String] -> IO Host
prepareHost args = do
  executable <- getExecutablePath
  runtimeEnv <- lookupEnv "SAPIENS_RUNTIME_DIR"
  let option name = listToMaybe [value | (key,value) <- zip args (drop 1 args), key == name]
  when ((option "--runtime-dir" `mplus` runtimeEnv)==Just "") $ fail "Runtime directory must not be empty"
  when (option "--data-dir"==Just "") $ fail "Data directory must not be empty"
  runtime <- makeAbsolute $ fromMaybe (takeDirectory executable </> "runtime") (option "--runtime-dir" `mplus` runtimeEnv)
  source <- doesFileExist (runtime </> "corpora/Main.hs")
  let sourcePath key fallback = lookupEnv key >>= maybe (pure fallback) (\path -> if null path then fail (key++" must not be empty") else makeAbsolute path)
      defaultData = if source then sourcePath "SAPIENS_DATA_DIR" (runtime </> ".sapiens4")
        else Paths.defaultDataDirectory runtime
  directory <- maybe defaultData makeAbsolute (option "--data-dir")
  config <- if source then sourcePath "SAPIENS_CONFIG_DIR" (runtime </> ".sapiens4/config")
    else Paths.configDirectory runtime
  Host directory config runtime <$> newIORef Nothing <*> newIORef Nothing

startHost :: Host -> IO String
startHost host = mask $ \restore -> do
  closeHost host
  createDirectoryIfMissing True (dataDirectory host)
  let binary = runtimeDirectory host </> ".build/bin/sapiens4.exe"
  available <- doesFileExist binary
  unless available $ ioError $ userError $ "Haskell runtime is missing. Extract the complete Sapiens4 package: " ++ binary
  environment <- getEnvironment
  let configRoot = configDirectory host
  createDirectoryIfMissing True configRoot
  pid <- processId
  let overrides = [("SAPIENS_DESKTOP_PID", show pid),("SAPIENS_REPOSITORY",runtimeDirectory host),("SAPIENS_CONFIG_DIR",configRoot),("SAPIENS_DATA_DIR",dataDirectory host)]
  (Just input, Just output, Just errors, child) <- createProcess (proc binary ["--port", "0", "--data-dir", dataDirectory host, "--desktop"])
    { cwd = Just (runtimeDirectory host), std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe
    , env = Just (hostEnvironment overrides environment)
    , create_group = True, use_process_jobs = True, detach_console = True }
  let owned = ServerProcess child input output errors Nothing Nothing
      cleanup = closeHost host
  writeIORef (serverProcess host) (Just owned)
  mapM_ (\stream -> hSetEncoding stream utf8 >> hSetNewlineMode stream universalNewlineMode) [input, output, errors] `onException` cleanup
  errorText <- newIORef ""
  collect <- async $ (forever $ hGetLine errors >>= \line -> modifyIORef' errorText (\old -> let next = old ++ line ++ "\n" in drop (max 0 $ length next - 12000) next))
    `catch` \(_ :: IOException) -> pure ()
  writeIORef (serverProcess host) (Just owned { errorReader=Just collect })
  readiness <- newEmptyMVar
  drain <- async $ do
    result <- try (awaitHost host output errorText) :: IO (Either IOException String)
    putMVar readiness result
    case result of
      Left _ -> pure ()
      Right _ -> (forever (void (hGetLine output))) `catch` (\(_ :: IOException) -> pure ())
  writeIORef (serverProcess host) (Just (ServerProcess child input output errors (Just drain) (Just collect)))
  restore (takeMVar readiness >>= either throwIO pure) `onException` closeHost host

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
  writeIORef (origin host) (Just base)
  pure (show uri)

closeHost :: Host -> IO ()
closeHost host = mask_ $ do
  current <- readIORef (serverProcess host)
  forM_ current $ \server -> do
    stopServer server
    writeIORef (serverProcess host) Nothing
  writeIORef (origin host) Nothing

stopServer :: ServerProcess -> IO ()
stopServer server = mask_ $ do
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
