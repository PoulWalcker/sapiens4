{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}
module Provider where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception
import Control.Monad
import Data.Aeson hiding (pairs)
import Data.Aeson.Types (Parser, parseEither)
import Text.Read (readMaybe)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Data.IORef
import Data.Char (toUpper, toLower)
import Data.List (nub,sortOn)
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.UUID as UUID
import Data.Time.Clock.POSIX
import GHC.Clock (getMonotonicTimeNSec)
import System.Directory
import System.Environment
import System.Exit
import System.FileLock
import System.FilePath
import System.Info
import System.IO
import System.IO.Error (isDoesNotExistError)
import System.Process hiding (runProcess)
import System.Timeout
#ifndef mingw32_HOST_OS
import System.Posix.Signals
import System.Posix.Files (setFileMode)
#endif
import qualified RuntimePaths as Paths
import Text.Regex.TDFA ((=~))
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
 config <- Paths.configDirectory
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

-- Setup and account safety are independent. Login can clear an account hold,
-- but only the durable setup decision authorizes the first execution.
data SetupStep = AIProvider | ComputerAccess | CorporaChoice | SetupReady | SetupComplete deriving (Eq)

stepName :: SetupStep -> Text
stepName step = case step of
 AIProvider -> "provider"
 ComputerAccess -> "permissions"
 CorporaChoice -> "corpora"
 SetupReady -> "ready"
 SetupComplete -> "complete"

readSetup :: IO (SetupStep,Maybe UUID.UUID)
readSetup = do
 path <- profileFile "onboarding.json"
 saved <- (Just <$> BS.readFile path) `catch` (\(e::IOException) -> if isDoesNotExistError e then pure Nothing else invalid)
 case saved of
  Nothing -> pure (AIProvider,Nothing)
  Just bytes -> either (const invalid) pure (eitherDecodeStrict' bytes >>= parseEither parseSetup)
 where
 invalid :: IO a
 invalid = failure 503 "Could not read onboarding.json. Restore valid setup settings from backup and restart Sapiens; saved decisions have been preserved."
 parseSetup = withObject "setup" $ \o -> do
  schema <- o .: "schema" :: Parser Int
  unless (schema==2) (fail "Unsupported setup schema")
  name <- o .: "step"
  step <- maybe (fail "Invalid setup step") pure (lookup name [(stepName step,step) | step <- [AIProvider,ComputerAccess,CorporaChoice,SetupReady,SetupComplete]])
  selected <- o .:? "selected_corpora" >>= traverse (maybe (fail "Invalid Corpora identity") pure . UUID.fromText)
  when (step `elem` [SetupReady,SetupComplete] && isNothing selected) (fail "Missing starting Corpora")
  pure (step,selected)

runLease :: IO (Maybe FileLock)
runLease = mask_ $ do
 lease <- profileLock Shared
 case lease of
  Nothing -> pure Nothing
  Just lock -> do
   ready <- (do
    (step,_) <- readSetup
    hold <- profileHold
    pure (step==SetupComplete && T.null hold)) `catch` (\(_::APIError) -> pure False)
    `onException` unlockFile lock
   if ready then pure lease else unlockFile lock >> pure Nothing

withReadyProfile :: IO a -> IO a
withReadyProfile action = bracket
 (runLease >>= maybe (failure 409 "Complete setup, connect ChatGPT in Settings and resume AI work before starting a task.") pure)
 unlockFile (const action)
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

data CodexConfig = CodexConfig
 { codexModel :: Text, codexEffort :: Text }
 deriving (Eq, Show)

modelDefaults :: IO CodexConfig
modelDefaults = CodexConfig <$> (maybe "gpt-6-sol" T.pack <$> lookupEnv "SAPIENS_CODEX_MODEL") <*> (maybe "high" T.pack <$> lookupEnv "SAPIENS_CODEX_REASONING_EFFORT")

resolveCodexConfig :: ExecutionSettings -> CodexConfig -> CodexConfig
resolveCodexConfig settings defaults = case executionMode settings of
 Normal -> defaults
 Deep -> defaults { codexEffort = "xhigh" }

codexArguments :: FilePath -> CodexConfig -> [String]
codexArguments workdir config = codexConfig ++
 ["exec","-c","model_reasoning_effort=" ++ T.unpack (jsonText (str (codexEffort config))),"-c","model=" ++ T.unpack (jsonText (str (codexModel config))),"--skip-git-repo-check","--json","--dangerously-bypass-approvals-and-sandbox","--cd",workdir,"--color","never","-"]

codexBinary :: IO (Maybe FilePath)
codexBinary = do
 override <- lookupEnv "SAPIENS_CODEX_BINARY"
 case override of
  Just p | not (null p) -> findExecutable p
  _ -> do
   root <- lookupEnv "SAPIENS_REPOSITORY" >>= maybe getCurrentDirectory pure
   packaged <- doesFileExist (root </> "package-manifest.json")
   let candidate = root </> (if packaged then "codex" else ".build/codex") </> "bin" </> (if os=="mingw32" then "codex.exe" else "codex")
   bundled <- doesFileExist candidate
   if packaged || bundled then pure (if bundled then Just candidate else Nothing) else do
    path <- findExecutable "codex"
    home <- getHomeDirectory
    local <- fromMaybe (home </> "AppData/Local") <$> lookupEnv "LOCALAPPDATA"
    roaming <- fromMaybe (home </> "AppData/Roaming") <$> lookupEnv "APPDATA"
    let children folder = do
         present <- doesDirectoryExist folder
         if present then map (folder </>) <$> listDirectory folder else pure []
        vendors scope = do
         packages <- filter (T.isPrefixOf "codex" . T.pack . takeFileName) <$> children scope
         concat <$> forM packages (\package -> map (</> "codex/codex.exe") <$> children (package </> "vendor"))
    installed <- if os /= "mingw32" then pure [] else do
     app <- map (</> "codex.exe") <$> children (local </> "OpenAI/Codex/bin")
     npm <- vendors (roaming </> "npm/node_modules/@openai")
     bundled <- vendors (roaming </> "npm/node_modules/@openai/codex/node_modules/@openai")
     pure (app ++ npm ++ bundled)
    let candidates = maybeToList path ++ installed ++ ["/Applications/" ++ app ++ ".app/Contents/Resources/" ++ binary | os == "darwin", app <- ["Codex","ChatGPT"],binary <- ["codex","codex-cli/bin/codex"]]
        native = filter (\p -> os /= "mingw32" || map toLower (takeExtension p) == ".exe") candidates
    existing <- filterM doesFileExist (nub native)
    runnable <- filterM (fmap executable . getPermissions) existing
    versions <- forM runnable $ \p -> do
     result <- try (timeout 10000000 (readProcessWithExitCode p ["--version"] "")) :: IO (Either SomeException (Maybe (ExitCode,String,String)))
     case result of
      Left errorValue | Just (_ :: SomeAsyncException) <- fromException errorValue -> throwIO errorValue
      Left errorValue | Just (_ :: AsyncCancelled) <- fromException errorValue -> throwIO errorValue
      _ -> pure ()
     let version = case result of
          Right (Just (ExitSuccess,out,_)) -> let (_,_,_,numbers) = out =~ ("codex-cli[[:space:]]+([0-9]+)\\.([0-9]+)\\.([0-9]+)" :: String) :: (String,String,String,[String]) in mapMaybe readMaybe numbers
          _ -> [] :: [Int]
     pure (version,p)
    pure (snd <$> listToMaybe (reverse (sortOn id (filter (not . null . fst) versions))))
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
 defaults <- modelDefaults
 settings <- executionSettings s (agentIdText (runOwner run))
 let config = resolveCodexConfig settings defaults
     workdir = workspaceRoot s (agentIdText (runOwner run))
     args = codexArguments workdir config
 environment <- codexEnvironment
 started <- realToFrac . (*1000) <$> getPOSIXTime :: IO Double
 writeIORef (runActivity run) (obj [("started",toJSON started),("updated",toJSON started),("phase",str "Waiting for model"),("last_action",str "")])
 notifyChange s
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
         when (reported /= activity) (notifyChange s)
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
