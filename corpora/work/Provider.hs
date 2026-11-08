{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}
module Provider where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception
import Control.Monad
import Data.Aeson hiding (pairs)
import Data.Aeson.Types (Parser)
import Text.Read (readMaybe)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Data.IORef
import Data.Char (toUpper)
import Data.List (nub)
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time.Clock.POSIX
import GHC.Clock (getMonotonicTimeNSec)
import System.Directory
import System.Environment
import System.Exit
import System.FileLock
import System.FilePath
import System.Info
import System.IO
import System.Process hiding (runProcess)
import System.Timeout
#ifndef mingw32_HOST_OS
import System.Posix.Signals
import System.Posix.Files (setFileMode)
#endif
import Types
import Population (executionSettings)
import qualified Computer as Computer

data ProviderEvent
 = SessionStarted Text | ToolStarted Text Text | ToolCompleted Text Text
 | AgentMessage Text | ProviderFailed Value | OtherEvent
 deriving (Eq, Show)
instance FromJSON ProviderEvent where
 parseJSON = withObject "Provider event" $ \o -> do
  kind <- o .: "type" :: Parser Text
  case kind of
   "thread.started" -> SessionStarted <$> o .: "thread_id"
   "turn.failed" -> pure (ProviderFailed (Object o))
   "item.started" -> itemEvent True =<< o .: "item"
   "item.completed" -> itemEvent False =<< o .: "item"
   _ -> pure OtherEvent -- Provider protocols may add events we do not consume.
  where
  itemEvent started = withObject "Provider item" $ \item -> do
   kind <- item .: "type" :: Parser Text
   let value=Object item
       label=T.take 200 (text (get value "command" `fallback` get value "tool" `fallback` str kind))
       identity=text (get value "id" `fallback` str label)
   if kind `elem` ["command_execution","mcp_tool_call","web_search","file_change"] then
    pure ((if started then ToolStarted else ToolCompleted) identity label)
   else if not started && kind=="agent_message" then AgentMessage <$> item .: "text" else pure OtherEvent

data ProviderState = ProviderState
 { providerTools :: M.Map Text Text, providerMessage :: Maybe Text }

providerStep :: Double -> ProviderEvent -> ProviderState -> Value -> Validation (ProviderState,Value)
providerStep time event state activity = case event of
 ProviderFailed details -> Left (APIError 500 ("Codex turn failed: " <> jsonText details))
 _ -> Right (updated,mergeFields [("updated",toJSON time),("phase",str (if M.null tools then "Waiting for model" else "Running tool")),("tool",str (fromMaybe "" (listToMaybe (M.elems tools))))] reported)
 where
 updated=case event of
  ToolStarted key label -> state { providerTools=M.insert key label (providerTools state) }
  ToolCompleted key _ -> state { providerTools=M.delete key (providerTools state) }
  AgentMessage message -> state { providerMessage=Just message }
  _ -> state
 tools=providerTools updated
 reported=case event of
  SessionStarted session -> put "session" (str session) activity
  ToolCompleted _ label -> put "last_action" (str label) activity
  _ -> activity

modelDefaults :: IO (Text,Text)
modelDefaults = (,) <$> (maybe "gpt-6-sol" T.pack <$> lookupEnv "SAPIENS_CODEX_MODEL") <*> (maybe "high" T.pack <$> lookupEnv "SAPIENS_CODEX_REASONING_EFFORT")
-- Login and execution deliberately share an app-owned credential/config root.
-- Never inherit a shell's Codex home, API key or workload identity credentials.
codexEnvironment :: IO [(String,String)]
codexEnvironment = do
 home <- codexHome
 environment <- getEnvironment
 binary <- codexBinary
 let name=if os=="mingw32" then map toUpper else id
     tools=maybe "" (\p -> takeDirectory (takeDirectory p)</>"codex-path") binary
     path=fromMaybe "" (listToMaybe [value | (key,value)<-environment,name key=="PATH"])
 pure (("CODEX_HOME",home):("PATH",tools<>[searchPathSeparator]<>path):filter (\(key,_) -> name key/="PATH" && not ("CODEX_" `T.isPrefixOf` T.pack (name key) || "OPENAI_" `T.isPrefixOf` T.pack (name key))) environment)
codexHome :: IO FilePath
codexHome = do
 config <- lookupEnv "SAPIENS_CONFIG_DIR" >>= maybe (do
  home <- getHomeDirectory
  base <- if os=="mingw32" then fromMaybe (home</>"AppData/Local") <$> lookupEnv "LOCALAPPDATA" else pure (home</>"Library/Application Support")
  pure (base</>"Sapiens4 Development/config")) pure
 home <- makeAbsolute (config</>"codex")
 createDirectoryIfMissing True home
#ifndef mingw32_HOST_OS
 setFileMode home 0o700
#endif
 pure home
codexConfig :: [String]
codexConfig = ["-c","cli_auth_credentials_store=\"file\"","-c","forced_login_method=\"chatgpt\""]

-- A profile-wide OS lock covers whole runs, including gaps between model calls.
-- Credential changes take the exclusive side, across corpora and host processes.
profileFile :: FilePath -> IO FilePath
profileFile name = (</>name) . takeDirectory <$> codexHome

profileHold :: IO Text
profileHold = do
 path <- profileFile "account-state.json"
 (do
  value <- readJSON path empty
  pure (at value "hold")) `catch` (\(_::IOException) -> pure "uncertain")

holdProfile :: Text -> IO ()
holdProfile hold = profileFile "account-state.json" >>= \path -> writeJSON path (obj [("hold",str hold)])

profileLock :: SharedExclusive -> IO (Maybe FileLock)
profileLock mode = profileFile "account.lock" >>= flip tryLockFile mode

runLease :: IO (Maybe FileLock)
runLease = mask_ $ do
 lease <- profileLock Shared
 case lease of
  Nothing -> pure Nothing
  Just lock -> do
   hold <- profileHold `onException` unlockFile lock
   if T.null hold then pure lease else unlockFile lock >> pure Nothing

withReadyProfile :: IO a -> IO a
withReadyProfile action = bracket
 (runLease >>= maybe (failure 409 "Connect ChatGPT in Settings and resume AI work before starting a task.") pure)
 unlockFile (const action)

codexBinary :: IO (Maybe FilePath)
codexBinary = do
 override <- lookupEnv "SAPIENS_CODEX_BINARY"
 root <- fromMaybe "" <$> lookupEnv "SAPIENS_REPOSITORY"
 packaged <- doesFileExist (root</>"package-manifest.json")
 let payload=root</>if packaged then "codex" else ".build/codex"
     candidate=payload</>"bin"</>if os=="mingw32" then "codex.exe" else "codex"
 case override of
  Just path | not (null path) -> findExecutable path
  _ -> do
   exists <- doesFileExist candidate
   pure (if exists then Just candidate else Nothing)
killTree :: ProcessHandle -> IO ()
killTree ph = do
#ifdef mingw32_HOST_OS
 -- Retain the job handle until termination, even if its root already exited.
 -- getProcessExitCode can otherwise close the root and lose the job handle.
 terminateProcess ph
#else
 result <- getProcessExitCode ph
 when (isNothing result) $ do
  pid <- getPid ph
  case pid of
   Nothing -> terminateProcess ph
   Just root -> do
    let send sig p = signalProcess sig p `catch` (\(_::IOException) -> pure ())
        collect stopped pending = do
         mapM_ (send sigSTOP) pending
         (_,out,_) <- readProcessWithExitCode "ps" ["-axo","pid=,ppid="] ""
         let known = nub (stopped++pending)
             children = nub [child | line <- lines out,[p,pp] <- [words line],Just child <- [readMaybe p],Just parent <- [readMaybe pp],parent `elem` known,child `notElem` known]
         if null children then pure known else collect known children
    stopped <- collect [] [root] `catch` (\(_::SomeException) -> pure [root])
    mapM_ (send sigKILL) (reverse stopped)
  pure ()
#endif
invoke :: Service -> Run -> Text -> IO Text
invoke s run input = do
 foreground <- Computer.captureForeground (repositoryRoot s) (workspaceRoot s (agentIdText (runOwner run)))
 invokeModel s run input `finally` do
  lease <- locked s (readIORef (serviceLease s))
  when (lease==Just (runOwner run,runId run)) $ do
   warning <- Computer.restoreForeground (repositoryRoot s) foreground
   forM_ warning $ \message -> modifyIORef' (runActivity run) (put "warning" (str message))
   locked s $ do
    current <- readIORef (serviceLease s)
    when (current==lease) $ writeIORef (serviceLease s) Nothing
invokeModel :: Service -> Run -> Text -> IO Text
invokeModel s run input = do
 cancelled <- readIORef (runCancel run)
 require (not cancelled) 409 "Stopped by Admin"
 before <- getMonotonicTimeNSec
 require (before < runDeadline run) 504 "Call time limit exhausted between decision nodes"
 binary <- codexBinary >>= maybe (failure 503 "Codex is missing. Open Sapiens setup or reinstall the application.") pure
 (model,defaultEffort) <- modelDefaults
 settings <- executionSettings s (agentIdText (runOwner run))
 let effort = if executionMode settings == Deep then "xhigh" else defaultEffort
     workdir = workspaceRoot s (agentIdText (runOwner run))
     args = codexConfig ++ ["exec","-c","model_reasoning_effort=" ++ T.unpack (jsonText (str effort)),"-c","model=" ++ T.unpack (jsonText (str model)),"--skip-git-repo-check","--json","--dangerously-bypass-approvals-and-sandbox","--cd",workdir,"--color","never","-"]
 environment <- codexEnvironment
 started <- realToFrac . (*1000) <$> getPOSIXTime :: IO Double
 writeIORef (runActivity run) (obj [("started",toJSON started),("updated",toJSON started),("phase",str "Waiting for model"),("last_action",str "")])
 bracket (createProcess (proc binary args) { cwd=Just workdir,env=Just (("SAPIENS_AGENCY_RUN",T.unpack (runIdText (runId run))):filter ((/="SAPIENS_AGENCY_RUN") . fst) environment),std_in=CreatePipe,std_out=CreatePipe,std_err=CreatePipe,create_group=True,use_process_jobs=True,detach_console=os=="mingw32" }) cleanup $ \handles -> do
  (hin,hout,herr,ph) <- case handles of
   (Just inputHandle,Just outputHandle,Just errorHandle,process) -> pure (inputHandle,outputHandle,errorHandle,process)
   _ -> failure 500 "Provider process did not expose its configured pipes"
  modifyMVar_ (runProcess run) (const (pure (Just ph)))
  mapM_ (`hSetBinaryMode` True) [hin,hout,herr]
  stream <- newIORef (ProviderState M.empty Nothing)
  recent <- newIORef []
  errors <- newIORef []
  timedOut <- newIORef False
  let append ref t = modifyIORef' ref (take 20 . (t:))
      consume line = do
       append recent line
       case eitherDecodeStrict' (TE.encodeUtf8 line) of
        Left _ -> pure ()
        Right rawEvent -> do
         event <- decodeValue 500 rawEvent
         state <- readIORef stream
         activity <- readIORef (runActivity run)
         time <- realToFrac . (*1000) <$> getPOSIXTime :: IO Double
         (updated,reported) <- validated (providerStep time event state activity)
         writeIORef stream updated
         writeIORef (runActivity run) reported
      drain h f = let loop = do eof <- hIsEOF h; unless eof (BC.hGetLine h >>= f . TE.decodeUtf8With (\_ _ -> Just '\xfffd') >> loop) in loop
  withAsync ((BS.hPut hin (TE.encodeUtf8 input) `catch` (\(_::IOException) -> pure ())) `finally` hClose hin) $ \writer ->
   withAsync (drain herr (append errors)) $ \stderrReader ->
    withAsync (do
      let deadline=runDeadline run
      let watch = do
           threadDelay 50000
           stop <- readIORef (runCancel run)
           time <- getMonotonicTimeNSec
           if stop || time >= deadline then writeIORef timedOut (not stop) >> killTree ph else watch
      watch) $ \_ -> do
     drain hout consume
     exit <- waitForProcess ph
     void (waitCatch writer)
     void (waitCatch stderrReader)
     stop <- readIORef (runCancel run)
     expired <- readIORef timedOut
     require (not stop) 409 "Stopped by Admin; review external effects before retrying"
     require (not expired) 504 "Codex exceeded 1800s; its process tree was stopped."
     err <- T.unlines . reverse <$> readIORef errors
     out <- T.unlines . reverse <$> readIORef recent
     require (exit == ExitSuccess) 500 ("codex exec failed: " <> err <> out)
     (providerMessage <$> readIORef stream) >>= maybe (failure 500 "Codex completed without an agent message.") pure
 where cleanup (hin,hout,herr,ph) = do
        killTree ph
        void (waitForProcess ph)
        forM_ (catMaybes [hin,hout,herr]) $ \h -> hClose h `catch` (\(_::IOException) -> pure ())
        modifyMVar_ (runProcess run) (const (pure Nothing))
