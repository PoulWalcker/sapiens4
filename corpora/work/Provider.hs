{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}
module Provider
 ( codexBinary, modelDefaults, killTree, invoke, invokeModel, invokeObserved
 , TraceReference, traceBelongsTo, readProviderTrace
 ) where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception
import Control.Monad
import Data.Aeson hiding (pairs)
import Data.Aeson.Types (Parser)
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Vector as V
import Data.Scientific (base10Exponent,coefficient,toBoundedInteger)
import Data.Word (Word64)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Numeric.Natural (Natural)
import Text.Read (readMaybe)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.IORef
import Data.Char (toLower,isAscii,isAlphaNum,isHexDigit)
import Data.List (nub,sortOn,find)
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
 = SessionStarted CodexThreadId | ToolStarted Text Text | ToolCompleted Text Text
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
 ProviderFailed _ -> Left (APIError 500 "Codex turn failed; provider details omitted")
 _ -> Right (updated,mergeFields [("updated",toJSON time),("phase",str (if M.null tools then "Waiting for model" else "Running tool")),("tool",str (fromMaybe "" (listToMaybe (M.elems tools))))] reported)
 where
 updated=case event of
  ToolStarted key label | T.length key<=256 && M.size (providerTools state)<256 -> state { providerTools=M.insert key label (providerTools state) }
  ToolCompleted key _ -> state { providerTools=M.delete key (providerTools state) }
  AgentMessage message -> state { providerMessage=Just message }
  _ -> state
 tools=providerTools updated
 reported=case event of
  SessionStarted session -> put "session" (toJSON session) activity
  ToolCompleted _ label -> put "last_action" (str label) activity
  _ -> activity

newtype CodexThreadId = CodexThreadId Text deriving (Eq,Show)
instance FromJSON CodexThreadId where
 parseJSON = withText "Codex thread ID" $ \identity -> do
  unless (not (T.null (T.strip identity)) && T.length identity<=256) (fail "Invalid Codex thread ID")
  pure (CodexThreadId identity)
instance ToJSON CodexThreadId where toJSON (CodexThreadId identity) = str identity

data TraceReference = TraceReference
 { traceIdentity :: Text, traceOwner :: AgentId, traceCall :: RunId } deriving (Eq,Show)
instance ToJSON TraceReference where
 toJSON reference = obj [("id",str (traceIdentity reference)),("owner",toJSON (traceOwner reference)),("call",toJSON (traceCall reference))]
instance FromJSON TraceReference where
 parseJSON value = do
  identity <- textFieldP "id" 32 value
  unless (T.length identity==32 && T.all isHexDigit identity) (fail "Invalid provider invocation ID")
  TraceReference identity <$> personalAgentP (get value "owner") <*> parseJSON (get value "call")

traceBelongsTo :: AgentId -> RunId -> TraceReference -> Bool
traceBelongsTo owner call reference = traceOwner reference==owner && traceCall reference==call

data TraceGap = ProcessUnfinished | MissingTerminal | MissingFinal | MissingStreamEnd
 | OpenTools | MalformedEvents | UnknownEvents | DroppedEvents | OutOfOrderEvents
 | OmittedContent | ConflictingUsage deriving (Eq,Show)
data TraceStatus = Collecting | Complete | Incomplete (NonEmpty TraceGap) deriving (Eq,Show)
instance ToJSON TraceGap where
 toJSON gap = str $ case gap of
  ProcessUnfinished -> "ProcessUnfinished"
  MissingTerminal -> "MissingTerminal"
  MissingFinal -> "MissingFinal"
  MissingStreamEnd -> "MissingStreamEnd"
  OpenTools -> "OpenTools"
  MalformedEvents -> "MalformedEvents"
  UnknownEvents -> "UnknownEvents"
  DroppedEvents -> "DroppedEvents"
  OutOfOrderEvents -> "OutOfOrderEvents"
  OmittedContent -> "OmittedContent"
  ConflictingUsage -> "ConflictingUsage"
instance ToJSON TraceStatus where
 toJSON status = case status of
  Collecting -> obj [("kind",str "Collecting")]
  Complete -> obj [("kind",str "Complete")]
  Incomplete gaps -> obj [("kind",str "Incomplete"),("gaps",toJSON (NE.toList gaps))]

data TokenUsage = TokenUsage
 { usageInput :: Maybe Natural, usageCached :: Maybe Natural, usageOutput :: Maybe Natural
 , usageDuplicates :: Natural, usageConflict :: Bool, usageInvalid :: Natural, usageFields :: Object
 } deriving (Eq,Show)
instance ToJSON TokenUsage where
 toJSON usage = mergeFields [("inputTokens",toJSON (usageInput usage)),("cachedInputTokens",toJSON (usageCached usage)),("outputTokens",toJSON (usageOutput usage))
  ,("duplicates",toJSON (usageDuplicates usage)),("conflict",Bool (usageConflict usage)),("invalidReports",toJSON (usageInvalid usage)),("price",Null)] (Object (usageFields usage))
instance FromJSON TokenUsage where
 parseJSON = withObject "Provider usage" $ \o -> TokenUsage <$> o .: "inputTokens" <*> o .: "cachedInputTokens" <*> o .: "outputTokens"
  <*> o .: "duplicates" <*> o .: "conflict" <*> o .: "invalidReports"
  <*> pure (foldr KM.delete o ["inputTokens","cachedInputTokens","outputTokens","duplicates","conflict","invalidReports","price"])

data TraceReceipt = TraceReceipt
 { receiptSequence :: Natural, receiptTime :: Text, receiptKind :: Text
 , receiptItem :: Maybe Int, receiptStatus :: Maybe Text, receiptExit :: Maybe Int, receiptSummary :: Maybe ToolSummary
 } deriving (Eq,Show)
instance ToJSON TraceReceipt where
 toJSON receipt = obj [("sequence",toJSON (receiptSequence receipt)),("receivedAt",str (receiptTime receipt)),("kind",str (receiptKind receipt))
  ,("item",toJSON (receiptItem receipt)),("status",toJSON (receiptStatus receipt)),("exitCode",toJSON (receiptExit receipt)),("summary",toJSON (receiptSummary receipt))]
instance FromJSON TraceReceipt where
 parseJSON = withObject "Provider receipt" $ \o -> TraceReceipt <$> o .: "sequence" <*> o .: "receivedAt" <*> o .: "kind"
  <*> o .: "item" <*> o .: "status" <*> o .: "exitCode" <*> o .: "summary"

data ProviderTrace = ProviderTrace
 { evidenceReference :: TraceReference, evidenceModel :: Text, evidenceStarted :: Text
 , evidenceDuration :: Maybe Double, evidenceExit :: Maybe Int, evidenceFinished :: Bool
 , evidenceSession :: Maybe CodexThreadId, evidenceReceipts :: [TraceReceipt], evidenceUsage :: TokenUsage
 , evidenceFinal :: Bool, evidenceTerminal :: Maybe Bool, evidenceStreamEnd :: Bool, evidencePending :: Maybe Int
 , evidenceMalformed :: Natural, evidenceUnknown :: Natural, evidenceDropped :: Natural, evidenceDroppedBytes :: Natural
 , evidenceOutOfOrder :: Natural, evidenceOmitted :: Natural, evidenceFields :: Object
 } deriving (Eq,Show)
instance ToJSON ProviderTrace where
 toJSON trace = mergeFields [("reference",toJSON (evidenceReference trace)),("provider",str "codex"),("model",str (evidenceModel trace)),("startedAt",str (evidenceStarted trace))
  ,("durationMs",toJSON (evidenceDuration trace)),("processExit",toJSON (evidenceExit trace)),("finished",Bool (evidenceFinished trace))
  ,("session",toJSON (evidenceSession trace)),("events",toJSON (evidenceReceipts trace)),("usage",toJSON (evidenceUsage trace))
  ,("finalSeen",Bool (evidenceFinal trace)),("terminalSuccess",toJSON (evidenceTerminal trace)),("streamEndSeen",Bool (evidenceStreamEnd trace)),("pendingTools",toJSON (evidencePending trace))
  ,("malformedEvents",toJSON (evidenceMalformed trace)),("unknownEvents",toJSON (evidenceUnknown trace)),("droppedEvents",toJSON (evidenceDropped trace)),("droppedBytes",toJSON (evidenceDroppedBytes trace))
  ,("outOfOrderEvents",toJSON (evidenceOutOfOrder trace)),("omittedContent",toJSON (evidenceOmitted trace)),("status",toJSON (traceStatus True trace))] (Object (evidenceFields trace))
instance FromJSON ProviderTrace where
 parseJSON = withObject "Provider trace" $ \o -> do
  unless (KM.lookup "provider" o == Just (str "codex")) (fail "Unsupported provider trace")
  ProviderTrace <$> o .: "reference" <*> o .: "model" <*> o .: "startedAt"
  <*> o .: "durationMs" <*> o .: "processExit" <*> o .: "finished" <*> o .: "session" <*> o .: "events" <*> o .: "usage"
  <*> o .: "finalSeen" <*> o .: "terminalSuccess" <*> o .: "streamEndSeen" <*> o .: "pendingTools"
  <*> o .: "malformedEvents" <*> o .: "unknownEvents" <*> o .: "droppedEvents" <*> o .: "droppedBytes" <*> o .: "outOfOrderEvents" <*> o .: "omittedContent"
  <*> pure (foldr KM.delete o ["reference","provider","model","startedAt","durationMs","processExit","finished","session","events","usage","finalSeen","terminalSuccess","streamEndSeen","pendingTools","malformedEvents","unknownEvents","droppedEvents","droppedBytes","outOfOrderEvents","omittedContent","status"])

traceStatus :: Bool -> ProviderTrace -> TraceStatus
traceStatus live trace
 | not (evidenceFinished trace) && live = Collecting
 | Just gaps <- NE.nonEmpty problems = Incomplete gaps
 | otherwise = Complete
 where
 problems=[ProcessUnfinished | not (evidenceFinished trace) || isNothing (evidenceExit trace)]
  ++ [MissingTerminal | isNothing (evidenceTerminal trace)]
  ++ [MissingFinal | evidenceTerminal trace == Just True && not (evidenceFinal trace)]
  ++ [MissingStreamEnd | not (evidenceStreamEnd trace)] ++ [OpenTools | evidencePending trace /= Just 0]
  ++ [MalformedEvents | evidenceMalformed trace>0] ++ [UnknownEvents | evidenceUnknown trace>0]
  ++ [DroppedEvents | evidenceDropped trace>0] ++ [OutOfOrderEvents | evidenceOutOfOrder trace>0]
  ++ [OmittedContent | evidenceOmitted trace>0]
  ++ [ConflictingUsage | usageConflict (evidenceUsage trace) || usageInvalid (evidenceUsage trace)>0]

readProviderTrace :: Service -> TraceReference -> IO Value
readProviderTrace s reference = do
 let path=archivePath s (agentIdText (traceOwner reference)) ("provider/"<>traceIdentity reference)
 exists <- doesFileExist path
 if not exists then pure (obj [("reference",toJSON reference),("status",obj [("kind",str "Unavailable")])]) else do
  trace <- readJSON path Nothing >>= maybe (failure 500 "Empty provider trace") pure
  require (evidenceReference trace == reference) 500 "Provider trace belongs to another invocation"
  runs <- readMVar (serviceRuns s)
  active <- traverse (readIORef . runActivity) (M.lookup (traceCall reference) runs)
  let live=maybe False ((==toJSON reference) . (`get` "providerTrace")) active
  pure (put "status" (toJSON (traceStatus live trace)) (toJSON trace))

modelDefaults :: IO (Text,Text)
modelDefaults = (,) <$> (maybe "gpt-6-sol" T.pack <$> lookupEnv "SAPIENS_CODEX_MODEL") <*> (maybe "high" T.pack <$> lookupEnv "SAPIENS_CODEX_REASONING_EFFORT")
codexBinary :: IO (Maybe FilePath)
codexBinary = do
 override <- lookupEnv "SAPIENS_CODEX_BINARY"
 case override of
  Just p | not (null p) -> findExecutable p
  _ -> do
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
invoke s run input = invokeObserved s run input (const (pure ()))

invokeObserved :: Service -> Run -> Text -> (TraceReference -> IO ()) -> IO Text
invokeObserved s run input retain = do
 foreground <- Computer.captureForeground (repositoryRoot s) (workspaceRoot s (agentIdText (runOwner run)))
 invokeModelObserved s run input retain `finally` do
  lease <- locked s (readIORef (serviceLease s))
  when (lease==Just (runOwner run,runId run)) $ do
   warning <- Computer.restoreForeground (repositoryRoot s) foreground
   forM_ warning $ \message -> modifyIORef' (runActivity run) (put "warning" (str message))
   locked s $ do
    current <- readIORef (serviceLease s)
    when (current==lease) $ writeIORef (serviceLease s) Nothing
invokeModel :: Service -> Run -> Text -> IO Text
invokeModel s run input = invokeModelObserved s run input (const (pure ()))

invokeModelObserved :: Service -> Run -> Text -> (TraceReference -> IO ()) -> IO Text
invokeModelObserved s run input retain = do
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
 identity <- newId
 savedState <- agentState s (agentIdText (runOwner run))
 let reference=TraceReference identity (runOwner run) (runId run)
     attempt=turnAttempt =<< find ((==runId run) . turnId) (agentTurns savedState)
     path=archivePath s (agentIdText (runOwner run)) ("provider/"<>identity)
 begun <- initialEvidence reference model <$> now <*> pure attempt
 let initial=begun { evidenceFields=KM.insert "reasoningEffort" (str effort) (evidenceFields begun) }
 capture <- newIORef (Capture initial M.empty M.empty 0 2 False)
 checkpoint <- newIORef before
 let persist=readIORef capture >>= writeJSON path . captureSnapshot
     update f = do
      modifyIORef' capture f
      clock <- getMonotonicTimeNSec
      previous <- readIORef checkpoint
      when (clock-previous>=250000000) (persist >> writeIORef checkpoint clock)
 persist
 retain reference
 modifyIORef' (runActivity run) (put "providerTrace" (toJSON reference))
 result <- try $ bracket (createProcess (proc binary args) { cwd=Just workdir,env=Just (("SAPIENS_AGENCY_RUN",T.unpack (runIdText (runId run))):filter ((/="SAPIENS_AGENCY_RUN") . fst) environment),std_in=CreatePipe,std_out=CreatePipe,std_err=CreatePipe,create_group=True,use_process_jobs=True,detach_console=os=="mingw32" }) (cleanup capture) $ \handles -> do
  (hin,hout,herr,ph) <- case handles of
   (Just inputHandle,Just outputHandle,Just errorHandle,process) -> pure (inputHandle,outputHandle,errorHandle,process)
   _ -> failure 500 "Provider process did not expose its configured pipes"
  modifyMVar_ (runProcess run) (const (pure (Just ph)))
  mapM_ (`hSetBinaryMode` True) [hin,hout,herr]
  stream <- newIORef (ProviderState M.empty Nothing)
  errors <- newIORef []
  timedOut <- newIORef False
  let secrets=[T.pack value | (key,value)<-environment,sensitiveKey (T.pack key),length value>=4]
      appendError _ payload = forM_ payload $ \line -> do
       let content=TE.decodeUtf8With (\_ _ -> Just '\xfffd') line
           safe=if sensitiveText secrets content then "[provider diagnostic omitted]" else T.take 1024 content
       modifyIORef' errors (take 20 . (safe:))
      consume bytes payload = case payload of
       Nothing -> update (dropCapture bytes)
       Just line -> case eitherDecodeStrict' line of
        Left _ -> update malformedCapture
        Right rawEvent -> do
         received <- now
         update (receiveCapture secrets received bytes rawEvent)
         event <- case fromJSON rawEvent of
          Error _ -> update malformedCapture >> failure 500 "Provider event schema was invalid; payload omitted"
          Success value -> pure value
         when (at rawEvent "type"=="error") (failure 500 "Codex reported an error; provider details omitted")
         state <- readIORef stream
         activity <- readIORef (runActivity run)
         time <- realToFrac . (*1000) <$> getPOSIXTime :: IO Double
         (updated,reported) <- validated (providerStep time event state activity)
         writeIORef stream updated
         writeIORef (runActivity run) reported
  withAsync ((BS.hPut hin (TE.encodeUtf8 input) `catch` (\(_::IOException) -> pure ())) `finally` hClose hin) $ \writer ->
   withAsync (drainBounded herr appendError) $ \stderrReader ->
    withAsync (do
      let deadline=runDeadline run
      let watch = do
           threadDelay 50000
           stop <- readIORef (runCancel run)
           time <- getMonotonicTimeNSec
           if stop || time >= deadline then writeIORef timedOut (not stop) >> killTree ph else watch
      watch) $ \_ -> do
     drainBounded hout consume
     update (mapSnapshot (\trace -> trace { evidenceStreamEnd=True }))
     exit <- waitForProcess ph
     update (mapSnapshot (\trace -> trace { evidenceExit=Just (case exit of ExitSuccess -> 0; ExitFailure code -> code) }))
     void (waitCatch writer)
     void (waitCatch stderrReader)
     stop <- readIORef (runCancel run)
     expired <- readIORef timedOut
     require (not stop) 409 "Stopped by Admin; review external effects before retrying"
     require (not expired) 504 "Codex exceeded 1800s; its process tree was stopped."
     err <- T.unlines . reverse <$> readIORef errors
     require (exit == ExitSuccess) 500 ("codex exec failed: " <> err)
     (providerMessage <$> readIORef stream) >>= maybe (failure 500 "Codex completed without an agent message.") pure
 elapsed <- getMonotonicTimeNSec
 modifyIORef' capture (mapSnapshot (\trace -> trace { evidenceFinished=True,evidenceDuration=Just (fromIntegral (elapsed-before)/1000000) }))
 saved <- try persist :: IO (Either SomeException ())
 case result of
  Left (problem :: SomeException) -> throwIO problem
  Right answer -> either throwIO (const (pure answer)) saved
 where cleanup observations (hin,hout,herr,ph) = do
        killTree ph
        exit <- waitForProcess ph
        modifyIORef' observations (mapSnapshot (\trace -> trace { evidenceExit=Just (case exit of ExitSuccess -> 0; ExitFailure code -> code) }))
        forM_ (catMaybes [hin,hout,herr]) $ \h -> hClose h `catch` (\(_::IOException) -> pure ())
        modifyMVar_ (runProcess run) (const (pure Nothing))

drainBounded :: Handle -> (Integer -> Maybe BS.ByteString -> IO ()) -> IO ()
drainBounded stream consume = loop BS.empty 0
 where
 limit=2097152
 loop pending count = do
  chunk <- BS.hGetSome stream 8192
  if BS.null chunk then when (count>0) (consume count (if count>limit then Nothing else Just pending))
   else split pending count chunk
 split pending count chunk = do
  let (part,rest)=BS.break (==10) chunk
      total=count+fromIntegral (BS.length part)
      kept=if total>limit then BS.empty else pending<>part
  if BS.null rest then loop kept total else do
   consume total (if total>limit then Nothing else Just kept)
   split BS.empty 0 (BS.drop 1 rest)

data SummaryField = SummaryField
 { summaryName :: Text, summaryValue :: Value, summaryRedacted :: Bool, summaryTruncated :: Bool, summaryOmitted :: Bool } deriving (Eq,Show)
instance ToJSON SummaryField where
 toJSON value = obj [("name",str (summaryName value)),("value",summaryValue value),("redacted",Bool (summaryRedacted value)),("truncated",Bool (summaryTruncated value)),("omitted",Bool (summaryOmitted value))]
instance FromJSON SummaryField where
 parseJSON = withObject "Provider summary field" $ \o -> SummaryField <$> o .: "name" <*> o .: "value" <*> o .: "redacted" <*> o .: "truncated" <*> o .: "omitted"
data ToolSummary = ToolSummary
 { summaryFields :: [SummaryField], summaryOmittedFields :: Int, summaryRedactedFields :: Int, summaryTruncatedFields :: Int } deriving (Eq,Show)
instance ToJSON ToolSummary where
 toJSON value = obj [("fields",toJSON (summaryFields value)),("omittedFields",num (summaryOmittedFields value)),("redactedFields",num (summaryRedactedFields value)),("truncatedFields",num (summaryTruncatedFields value))]
instance FromJSON ToolSummary where
 parseJSON = withObject "Provider tool summary" $ \o -> ToolSummary <$> o .: "fields" <*> o .: "omittedFields" <*> o .: "redactedFields" <*> o .: "truncatedFields"

data Capture = Capture
 { captureSnapshot :: ProviderTrace, captureIdentities :: M.Map Text Int, capturePending :: M.Map Int Bool
 , captureSequence :: Natural, captureBytes :: Int, captureTurnStarted :: Bool }
initialEvidence :: TraceReference -> Text -> Text -> Maybe Int -> ProviderTrace
initialEvidence reference model started attempt = ProviderTrace
 { evidenceReference=reference,evidenceModel=model,evidenceStarted=started,evidenceDuration=Nothing,evidenceExit=Nothing,evidenceFinished=False
 , evidenceSession=Nothing,evidenceReceipts=[],evidenceUsage=TokenUsage Nothing Nothing Nothing 0 False 0 KM.empty
 , evidenceFinal=False,evidenceTerminal=Nothing,evidenceStreamEnd=False,evidencePending=Just 0
 , evidenceMalformed=0,evidenceUnknown=0,evidenceDropped=0,evidenceDroppedBytes=0,evidenceOutOfOrder=0,evidenceOmitted=0
 , evidenceFields=KM.fromList [("attempt",toJSON attempt),("scope",str "provider_events"),("externalEffectsVerified",Bool False)] }
mapSnapshot :: (ProviderTrace -> ProviderTrace) -> Capture -> Capture
mapSnapshot f capture = capture { captureSnapshot=f (captureSnapshot capture) }
dropCapture :: Integer -> Capture -> Capture
dropCapture bytes = mapSnapshot (\trace -> trace { evidenceDropped=evidenceDropped trace+1,evidenceDroppedBytes=evidenceDroppedBytes trace+fromInteger bytes,evidencePending=Nothing })
malformedCapture :: Capture -> Capture
malformedCapture = mapSnapshot (\trace -> trace { evidenceMalformed=evidenceMalformed trace+1 })

receiveCapture :: [Text] -> Text -> Integer -> Value -> Capture -> Capture
receiveCapture secrets received bytes value capture = appendReceipt identified
 where
 kind=at value "type"
 item=get value "item"
 itemKind=at item "type"
 tool=itemKind `elem` ["command_execution","mcp_tool_call","web_search","file_change"]
 lifecycle=kind `elem` ["item.started","item.updated","item.completed"]
 known=kind `elem` ["thread.started","turn.started","turn.completed","turn.failed","error"] || lifecycle && (tool || itemKind=="agent_message" || itemKind=="reasoning")
 identity=at item "id"
 identities=captureIdentities capture
 existing=M.lookup identity identities
 admissible=not (T.null identity) && T.length identity<=256 && M.size identities<256
 local=if tool && lifecycle then existing `fallbackMaybe` (if admissible then Just (M.size identities+1) else Nothing) else Nothing
 fallbackMaybe (Just a) _=Just a
 fallbackMaybe Nothing b=b
 nextIds=case local of Just n -> M.insert identity n identities; Nothing -> identities
 pending=capturePending capture
 terminal=kind=="item.completed" && tool
 nextPending=case local of
  Just n | kind=="item.started" -> M.insert n True pending
         | terminal -> M.delete n pending
  _ -> pending
 prior=captureSnapshot capture
 incomingSession=case fromJSON (get value "thread_id") of Success thread | kind=="thread.started" -> Just thread; _ -> Nothing
 changedSession=isJust incomingSession && isJust (evidenceSession prior) && incomingSession /= evidenceSession prior
 snapshot=prior
  {evidenceSession=case evidenceSession prior of Just thread -> Just thread; Nothing -> incomingSession
  ,evidenceFinal=evidenceFinal prior || kind=="item.completed" && itemKind=="agent_message"
  ,evidenceTerminal=case kind of "turn.completed" -> Just True; "turn.failed" -> Just False; "error" -> Just False; _ -> evidenceTerminal prior
  ,evidenceUnknown=evidenceUnknown prior + if known then 0 else 1
  ,evidencePending=if evidencePending prior==Nothing || tool && lifecycle && local==Nothing then Nothing else Just (M.size nextPending)
  ,evidenceOutOfOrder=evidenceOutOfOrder prior + if (terminal && maybe True (`M.notMember` pending) local)
    || (kind=="item.started" && maybe False (`M.member` pending) local)
    || (lifecycle && isJust (evidenceTerminal prior)) || changedSession
    || (kind=="turn.started" && (captureTurnStarted capture || isJust (evidenceTerminal prior))) then 1 else 0
  ,evidenceOmitted=evidenceOmitted prior+usageOmissions+metadataOmissions+fromIntegral (maybe 0 (\detail -> summaryOmittedFields detail+summaryRedactedFields detail+summaryTruncatedFields detail) summary)
  ,evidenceUsage=usage}
 (usage,usageOmissions)=if kind=="turn.completed" then receiveUsage (get value "usage") (evidenceUsage prior) else (evidenceUsage prior,0)
 identified=capture {captureSnapshot=snapshot,captureIdentities=nextIds,capturePending=nextPending,captureSequence=captureSequence capture+1
  ,captureTurnStarted=captureTurnStarted capture || kind=="turn.started"}
 summary=if tool && lifecycle then Just (toolSummary secrets itemKind item) else Nothing
 status=at item "status"
 metadataOmissions=if tool && lifecycle && ((has item "status" && status `notElem` ["in_progress","completed","failed"])
  || (has item "exit_code" && case fromJSON (get item "exit_code") :: Result Int of Error _ -> True; Success _ -> False)) then 1 else 0
 receipt=TraceReceipt (captureSequence capture+1) received (if known then kind<>if lifecycle then ":"<>itemKind else "" else "unknown") local
  (if status `elem` ["in_progress","completed","failed"] then Just status else Nothing) (case fromJSON (get item "exit_code") of Success n -> Just n; Error _ -> Nothing) summary
 size=fromIntegral (BL.length (encode receipt)) + if null (evidenceReceipts snapshot) then 0 else 1
 appendReceipt current
  | length (evidenceReceipts snapshot)>=256 || captureBytes capture+size>131072 = mapSnapshot (\e -> e {evidenceDropped=evidenceDropped e+1,evidenceDroppedBytes=evidenceDroppedBytes e+fromInteger bytes}) current
  | otherwise = current {captureSnapshot=snapshot {evidenceReceipts=evidenceReceipts snapshot++[receipt]},captureBytes=captureBytes capture+size}

receiveUsage :: Value -> TokenUsage -> (TokenUsage,Natural)
receiveUsage value original = (updated,fromIntegral omitted)
 where
 updated
  | incoming==(Nothing,Nothing,Nothing) = previous { usageConflict=usageConflict previous || old/=(Nothing,Nothing,Nothing) }
  | old==(Nothing,Nothing,Nothing) = previous { usageInput=input,usageCached=cached,usageOutput=output }
  | incoming==old = previous { usageDuplicates=usageDuplicates previous+1 }
  | otherwise = previous { usageConflict=True }
 primary=["input_tokens","cached_input_tokens","output_tokens"]
 additional=["cache_write_input_tokens","reasoning_output_tokens"]
 invalid=(value /= Null && case value of Object _ -> False; _ -> True)
  || any (\key -> has value key && number key==Nothing) (primary++additional)
 previous=original { usageInvalid=usageInvalid original+if invalid then 1 else 0
  ,usageConflict=usageConflict original || any (\(key,child) -> maybe False (/=child) (KM.lookup key (usageFields original))) (KM.toList extras)
  ,usageFields=KM.union (usageFields original) extras }
 extras=KM.fromList [(K.fromText key,toJSON amount) | key<-additional,Just amount<-[number key]]
 omitted=length [key | key<-keys value,key `notElem` (primary++additional)]
 number :: Text -> Maybe Natural
 number key = case get value key of
  Number n -> fromIntegral <$> (toBoundedInteger n :: Maybe Word64)
  _ -> Nothing
 input=number "input_tokens";cached=number "cached_input_tokens";output=number "output_tokens"
 incoming=(input,cached,output)
 old=(usageInput previous,usageCached previous,usageOutput previous)

-- Recognized credentials cause whole-field omission. This is not an arbitrary
-- secret detector; unrecognized prose remains an explicitly limited summary.
sensitiveKey :: Text -> Bool
sensitiveKey key = any (`T.isInfixOf` normalized) ["password","passwd","secret","token","apikey","authorization","cookie","credential","privatekey","accesskey"]
 where normalized=T.filter isAlphaNum (T.toCaseFold key)
sensitiveText :: [Text] -> Text -> Bool
sensitiveText secrets value = any (`T.isInfixOf` value) secrets || (T.unpack (T.toCaseFold value) =~ pattern_ :: Bool)
 where pattern_ = "(password|passwd|secret|token|api[_-]?key|authorization|cookie|credential|private[_-]?key|access[_-]?key|client[_-]?secret)[\"']?[[:space:]]*[:=]|(bearer|basic)[[:space:]]+[a-z0-9+/=_-]+|sk-[a-z0-9_-]{8,}|ghp_[a-z0-9]+|github_pat_[a-z0-9_]+|akia[a-z0-9]{16}|eyj[a-z0-9_-]+\\.[a-z0-9_-]+\\.[a-z0-9_-]+|-----begin .*private key|--(password|passwd|token|secret|api[_-]?key|access[_-]?key|client[_-]?secret)[[:space:]]+|[a-z][a-z0-9+.-]*://[^[:space:]/:@]*:[^[:space:]/]*@|https?://[^[:space:]]*[?&][^=[:space:]]*%[a-f0-9]{2}[^=[:space:]]*=" :: String

data Sanitized = Sanitized { cleanValue :: Value, cleanOmitted :: Int, cleanRedacted :: Int, cleanTruncated :: Int }
mergeClean :: Value -> [Sanitized] -> Sanitized
mergeClean value children = Sanitized value (sum (map cleanOmitted children)) (sum (map cleanRedacted children)) (sum (map cleanTruncated children))
cleanText :: [Text] -> Int -> Text -> Sanitized
cleanText secrets limit value
 | sensitiveText secrets value = Sanitized Null 1 1 0
 | otherwise = Sanitized (str bounded) 0 0 (if bounded==value then 0 else 1)
 where bounded=T.take limit value
cleanJSON :: [Text] -> Int -> Value -> Sanitized
cleanJSON secrets depth value = case value of
 String message -> cleanText secrets 512 message
 Object fields
  | depth<=0 -> Sanitized Null 1 0 1
  | otherwise ->
   let entries=KM.toList fields
       safeKey key=T.length key<=64 && T.all (\c -> isAscii c && (isAlphaNum c || c `elem` ['_','-','.'])) key && not (sensitiveText secrets key)
       clean (key,child)
        | not (safeKey (K.toText key)) = (Nothing,Sanitized Null 1 0 0)
        | sensitiveKey (K.toText key) = (Just (key,str "[redacted]"),Sanitized Null 1 1 0)
        | otherwise = let cleaned=cleanJSON secrets (depth-1) child in (Just (key,cleanValue cleaned),cleaned)
       children=map clean (take 16 entries)
       result=mergeClean (Object (KM.fromList (mapMaybe fst children))) (map snd children)
       extra=max 0 (length entries-16)
   in result {cleanOmitted=cleanOmitted result+extra,cleanTruncated=cleanTruncated result+if extra>0 then 1 else 0}
 Array values
  | depth<=0 -> Sanitized Null 1 0 1
  | otherwise -> let children=map (cleanJSON secrets (depth-1)) (take 8 (V.toList values)); extra=max 0 (V.length values-8)
                     result=mergeClean (arr (map cleanValue children)) children
                 in result {cleanOmitted=cleanOmitted result+extra,cleanTruncated=cleanTruncated result+if extra>0 then 1 else 0}
 Number number | abs (base10Exponent number)>308 || length (show (coefficient number))>64 -> Sanitized Null 1 0 1
 _ -> Sanitized value 0 0 0

-- Byte bounds apply to actual encoded JSON, including escaping overhead.
boundClean :: Int -> Sanitized -> Sanitized
boundClean budget value
 | BL.length (encode (cleanValue value))<=fromIntegral budget = value
 | String content <- cleanValue value, not (T.null content) = boundClean budget value {cleanValue=str (T.take (T.length content `div` 2) content),cleanTruncated=max 1 (cleanTruncated value)}
 | otherwise = value {cleanValue=Null,cleanOmitted=cleanOmitted value+1,cleanTruncated=max 1 (cleanTruncated value)}

toolSummary :: [Text] -> Text -> Value -> ToolSummary
toolSummary secrets kind item = if BL.length (encode summary)<=8192 then summary else
 summary {summaryFields=[entry {summaryValue=Null,summaryOmitted=True,summaryTruncated=True} | entry<-summaryFields summary],summaryOmittedFields=summaryOmittedFields summary+length fields,summaryTruncatedFields=summaryTruncatedFields summary+length fields}
 where
 summaryTextField name key limit = (name,key,\value -> case value of String content -> cleanText secrets limit content; Null -> Sanitized Null 0 0 0; _ -> Sanitized Null 1 0 0)
 jsonField name key = (name,key,cleanJSON secrets 3)
 fields=case kind of
  "command_execution" -> [summaryTextField "command" "command" 1024,summaryTextField "output" "aggregated_output" 1024]
  "mcp_tool_call" -> [summaryTextField "server" "server" 128,summaryTextField "tool" "tool" 128,jsonField "arguments" "arguments",jsonField "result" "result",jsonField "error" "error"]
  "web_search" -> [summaryTextField "query" "query" 512]
  "file_change" -> [("changes","changes",cleanChanges)]
  _ -> []
 cleanChanges value = case value of
  Array changes ->
   let clean change =
        let path=case get change "path" of String value_ -> cleanText secrets 512 value_; _ -> Sanitized Null 1 0 0
            action=at change "kind"
            known=action `elem` ["add","delete","update","create","modify"]
            omitted=length (filter (`notElem` ["path","kind"]) (keys change))
        in mergeClean (obj [("path",cleanValue path),("kind",if known then str action else Null)]) [path,Sanitized Null (omitted+if known then 0 else 1) 0 0]
       chosen=map clean (take 8 (V.toList changes)); extra=max 0 (V.length changes-8)
       result=mergeClean (arr (map cleanValue chosen)) chosen
   in result {cleanOmitted=cleanOmitted result+extra,cleanTruncated=cleanTruncated result+if extra>0 then 1 else 0}
  _ -> Sanitized Null 1 0 0
 selected=[(name,boundClean 4096 (clean value)) | (name,key,clean)<-fields,let value=get item key,has item key]
 allowed=["id","type","status","exit_code"]++[key|(_,key,_)<-fields]
 unknown=length (filter (`notElem` allowed) (keys item))
 summary=ToolSummary [SummaryField name (cleanValue value) (cleanRedacted value>0) (cleanTruncated value>0) (cleanOmitted value>0) | (name,value)<-selected]
  (unknown+sum [cleanOmitted value|(_,value)<-selected]) (sum [cleanRedacted value|(_,value)<-selected]) (sum [cleanTruncated value|(_,value)<-selected])
