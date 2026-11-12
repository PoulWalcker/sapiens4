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
import Data.List (find,isPrefixOf,isSuffixOf,nub,sortOn)
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
import System.FilePath
import System.Info
import System.IO
import System.Process hiding (runProcess)
import System.Timeout
import Text.Regex.TDFA ((=~))
#ifndef mingw32_HOST_OS
import System.Posix.Signals
#endif
import Types
import Population (readModelSettings, readProviders, ModelSelection(..), ModelSettingsDraft(..), Provider(..), ProviderModel(..), Providers(..))
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
 ProviderFailed details -> Left (APIError 500 ("Provider turn failed: " <> jsonText details))
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

runModelSettings :: Service -> Text -> IO ModelConfig
runModelSettings s owner = do
 configured <- readModelSettings s
 let choice = M.findWithDefault (modelSettingsDefault configured) owner (modelSettingsOverrides configured)
 pure (ModelConfig (selectionProvider choice) (selectionModel choice) (selectionEffort choice))

findProvider :: FilePath -> ProviderId -> IO Provider
findProvider root key = readProviders root >>= maybe (failure 400 "Unsupported model provider") pure . find ((==key) . providerId) . providersList

providerFor :: Service -> ModelConfig -> IO (Provider,FilePath)
providerFor s config = do
 provider <- findProvider (repositoryRoot s) (configuredProvider config)
 binary <- providerBinary provider >>= maybe (failure 503 (providerLabel provider <> " CLI was not found on this host.")) pure
 pure (provider,binary)

providersView :: FilePath -> IO Value
providersView root = do
 configured <- providersList <$> readProviders root
 pure (obj [("providers",arr [obj [("id",str (providerName (providerId provider))),("name",str (providerLabel provider)),("recommended",toJSON (providerRecommended provider)),("models",toJSON (providerModels provider))] | provider <- configured])])

providerArguments :: Provider -> FilePath -> ModelConfig -> [String]
providerArguments provider workdir config = concatMap expand (providerRun provider)
 where
 effort = configuredEffort config
 expand "{effort-args}" = if T.null effort then [] else map fill (providerEffort provider)
 expand arg = [fill arg]
 fill = T.unpack . T.replace "{workdir}" (T.pack workdir) . T.replace "{model}" (configuredModel config) . T.replace "{effort}" effort

validateModelSettingsDraft :: Service -> ModelSettingsDraft -> IO ()
validateModelSettingsDraft s draft = forM_ (modelSettingsDefault draft : M.elems (modelSettingsOverrides draft)) $ \choice -> do
 provider <- findProvider (repositoryRoot s) (selectionProvider choice)
 model <- maybe (failure 400 "The selected model is not in the current provider catalog.") pure (find ((== selectionModel choice) . modelName) (providerModels provider))
 require (T.null (selectionEffort choice) || selectionEffort choice `elem` modelEfforts model) 400 "The selected effort is not supported by this model."

providerBinary :: Provider -> IO (Maybe FilePath)
providerBinary provider = do
 override <- lookupEnv ("SAPIENS_" ++ map toUpper (T.unpack (providerName (providerId provider))) ++ "_BINARY")
 case override of
  Just p | not (null p) -> findExecutable p
  _ -> do
   path <- findExecutable (providerCommand provider)
   home <- getHomeDirectory
   listed <- concat <$> mapM (installed home) (providerPaths provider)
   existing <- filterM doesFileExist (nub (maybeToList path ++ listed))
   runnable <- filterM (fmap executable . getPermissions) existing
   versions <- forM runnable $ \p -> do
    result <- try (timeout 10000000 (readProcessWithExitCode p ["--version"] "")) :: IO (Either SomeException (Maybe (ExitCode,String,String)))
    case result of
     Left errorValue | Just (_ :: SomeAsyncException) <- fromException errorValue -> throwIO errorValue
     Left errorValue | Just (_ :: AsyncCancelled) <- fromException errorValue -> throwIO errorValue
     _ -> pure ()
    let version = case result of
         Right (Just (ExitSuccess,out,_)) -> let (_,_,_,numbers) = out =~ ("([0-9]+)\\.([0-9]+)\\.([0-9]+)" :: String) :: (String,String,String,[String]) in mapMaybe readMaybe numbers
         _ -> [] :: [Int]
    pure (version,p)
   pure (snd <$> listToMaybe (reverse (sortOn id (filter (not . null . fst) versions))))
 where
 installed home entry = foldM expand [""] (splitDirectories (if "~/" `isPrefixOf` entry then home </> drop 2 entry else entry))
 expand roots part
  | '*' `elem` part = concat <$> forM roots (\root -> do
     present <- doesDirectoryExist root
     names <- if present then listDirectory root else pure []
     pure [root </> name | name <- names, matches part name])
  | otherwise = pure [root </> part | root <- roots]
 matches part name = let (before,after) = break (=='*') part in length name >= length part - 1 && before `isPrefixOf` name && drop 1 after `isSuffixOf` name

class EventFormat f where
 events :: f -> Value -> IO [ProviderEvent]

data CodexJson = CodexJson
data ClaudeStream = ClaudeStream

instance EventFormat CodexJson where
 events _ raw = (:[]) <$> decodeValue 500 raw

instance EventFormat ClaudeStream where
 events _ = claudeEvents

formats :: [(Text, Value -> IO [ProviderEvent])]
formats = [("codex-json",events CodexJson),("claude-stream",events ClaudeStream)]

claudeEvents :: Value -> IO [ProviderEvent]
claudeEvents value = case at value "type" of
 "system" | at value "subtype" == "init" -> (\session -> [SessionStarted session]) <$> field 502 value "session_id"
 "result" -> do
  isError <- field 502 value "is_error" :: IO Bool
  if isError || at value "subtype" /= "success" then pure [ProviderFailed value]
   else (\result -> [AgentMessage result]) <$> field 502 value "result"
 "assistant" | has value "error" && get value "error" /= Null -> pure [ProviderFailed value]
 "assistant" -> pure [ToolStarted (at tool "id") (at tool "name") | tool <- items (get value "message") "content",at tool "type" == "tool_use"]
 "user" -> pure [ToolCompleted (at tool "tool_use_id") "Tool completed" | tool <- items (get value "message") "content",at tool "type" == "tool_result"]
 _ -> pure []

killTree :: ProcessHandle -> IO ()
killTree ph = do
 result <- getProcessExitCode ph
 when (isNothing result) $ do
  pid <- getPid ph
  case pid of
   Nothing -> terminateProcess ph
   Just root -> do
#ifdef mingw32_HOST_OS
    terminateProcess ph
#else
    let send sig p = signalProcess sig p `catch` (\(_::IOException) -> pure ())
        collect stopped pending = do
         mapM_ (send sigSTOP) pending
         (_,out,_) <- readProcessWithExitCode "ps" ["-axo","pid=,ppid="] ""
         let known = nub (stopped++pending)
             children = nub [child | line <- lines out,[p,pp] <- [words line],Just child <- [readMaybe p],Just parent <- [readMaybe pp],parent `elem` known,child `notElem` known]
         if null children then pure known else collect known children
    stopped <- collect [] [root] `catch` (\(_::SomeException) -> pure [root])
    mapM_ (send sigKILL) (reverse stopped)
#endif
  pure ()
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
 let config = runModelConfig run
     workdir = workspaceRoot s (agentIdText (runOwner run))
 (provider,binary) <- providerFor s config
 let args = providerArguments provider workdir config
     name = providerLabel provider
 readEvents <- maybe (failure 500 ("Unknown output format: " <> providerEvents provider)) pure (lookup (providerEvents provider) formats)
 environment <- getEnvironment
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
         parsed <- readEvents rawEvent
         forM_ parsed $ \event -> do
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
     require (not expired) 504 (name <> " exceeded 1800s; its process tree was stopped.")
     err <- T.unlines . reverse <$> readIORef errors
     out <- T.unlines . reverse <$> readIORef recent
     message <- providerMessage <$> readIORef stream
     require (exit == ExitSuccess) 500 (name <> " execution failed: " <> err <> out)
     maybe (failure 500 (name <> " completed without a successful result.")) pure message
 where cleanup (hin,hout,herr,ph) = do
        killTree ph
        void (waitForProcess ph)
        forM_ (catMaybes [hin,hout,herr]) $ \h -> hClose h `catch` (\(_::IOException) -> pure ())
        modifyMVar_ (runProcess run) (const (pure Nothing))
