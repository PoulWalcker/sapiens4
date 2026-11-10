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
import Data.Char (toLower)
import Data.List (nub,sortOn)
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

modelDefaults :: IO (Text,Text)
modelDefaults = (,) <$> (maybe "gpt-6-sol" T.pack <$> lookupEnv "SAPIENS_CODEX_MODEL") <*> (maybe "high" T.pack <$> lookupEnv "SAPIENS_CODEX_REASONING_EFFORT")
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
 binary <- codexBinary >>= maybe (failure 503 "Codex CLI was not found. Install Codex and run codex login.") pure
 (model,defaultEffort) <- modelDefaults
 settings <- executionSettings s (agentIdText (runOwner run))
 let effort = if executionMode settings == Deep then "xhigh" else defaultEffort
     workdir = workspaceRoot s (agentIdText (runOwner run))
     args = ["exec","-c","model_reasoning_effort=" ++ T.unpack (jsonText (str effort)),"-c","model=" ++ T.unpack (jsonText (str model)),"--skip-git-repo-check","--json","--dangerously-bypass-approvals-and-sandbox","--cd",workdir,"--color","never","-"]
 environment <- getEnvironment
 started <- realToFrac . (*1000) <$> getPOSIXTime :: IO Double
 writeIORef (runActivity run) (obj [("started",toJSON started),("updated",toJSON started),("phase",str "Waiting for model"),("last_action",str "")])
 bracket (createProcess (proc binary args) { cwd=Just workdir,env=Just (("SAPIENS_AGENCY_RUN",T.unpack (runIdText (runId run))):filter ((/="SAPIENS_AGENCY_RUN") . fst) environment),std_in=CreatePipe,std_out=CreatePipe,std_err=CreatePipe,create_group=True,use_process_jobs=True,detach_console=os=="mingw32" }) cleanup $ \handles -> do
  (hin,hout,herr,ph) <- case handles of
   (Just inputHandle,Just outputHandle,Just errorHandle,process) -> pure (inputHandle,outputHandle,errorHandle,process)
   _ -> failure 500 "Provider process did not expose its configured pipes"
  modifyMVar_ (runProcess run) (const (pure (Just ph)))
  mapM_ (`hSetBinaryMode` True) [hin,hout,herr]
  tools <- newIORef M.empty
  final <- newIORef Nothing
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
         case event of
          SessionStarted session -> modifyIORef' (runActivity run) (put "session" (str session))
          ToolStarted key label -> modifyIORef' tools (M.insert key label)
          ToolCompleted key label -> modifyIORef' tools (M.delete key) >> modifyIORef' (runActivity run) (put "last_action" (str label))
          AgentMessage message -> writeIORef final (Just message)
          ProviderFailed details -> failure 500 ("Codex turn failed: " <> jsonText details)
          OtherEvent -> pure ()
         current <- readIORef tools
         updated <- realToFrac . (*1000) <$> getPOSIXTime :: IO Double
         modifyIORef' (runActivity run) (mergeFields [("updated",toJSON updated),("phase",str (if M.null current then "Waiting for model" else "Running tool")),("tool",str (fromMaybe "" (listToMaybe (M.elems current))))])
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
     readIORef final >>= maybe (failure 500 "Codex completed without an agent message.") pure
 where cleanup (hin,hout,herr,ph) = do
        killTree ph
        void (waitForProcess ph)
        forM_ (catMaybes [hin,hout,herr]) $ \h -> hClose h `catch` (\(_::IOException) -> pure ())
        modifyMVar_ (runProcess run) (const (pure Nothing))
