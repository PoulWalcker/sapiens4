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
import qualified Data.ByteString.Lazy as BL
import Data.IORef
import Data.Int (Int64)
import Data.Char (toLower, isAscii, isAlphaNum)
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.Scientific (base10Exponent, coefficient)
import qualified Data.Vector as V
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
       label=kind
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

invokeObserved :: Service -> Run -> Text -> (EvidenceSnapshot -> IO ()) -> IO Text
invokeObserved s run input observe = do
 foreground <- Computer.captureForeground (repositoryRoot s) (workspaceRoot s (agentIdText (runOwner run)))
 invokeModelObserved s run input observe `finally` do
  lease <- locked s (readIORef (serviceLease s))
  when (lease==Just (runOwner run,runId run)) $ do
   warning <- Computer.restoreForeground (repositoryRoot s) foreground
   forM_ warning $ \message -> modifyIORef' (runActivity run) (put "warning" (str message))
   locked s $ do
    current <- readIORef (serviceLease s)
    when (current==lease) $ writeIORef (serviceLease s) Nothing

data InvocationPolicy = OrdinaryInvocation | EvidenceAnalysisResponseOnly deriving (Eq, Show)

-- This narrow policy is verified against the pinned CLI's public process and
-- transport interfaces. Unsupported versions/models never fall back to tools.
analysisCatalog :: IO Value
analysisCatalog = analysisCatalogFor Nothing
analysisCatalogFor :: Maybe Run -> IO Value
analysisCatalogFor activeRun = do
 binary <- codexBinary >>= maybe (failure 503 "Codex CLI is unavailable for analysis") pure
 started <- getMonotonicTimeNSec
 let configurationDeadline=started+10000000000
     guardRun = do
      clock <- getMonotonicTimeNSec
      require (clock < configurationDeadline) 503 "Response-only provider configuration exceeded its deadline"
      forM_ activeRun $ \running -> do
       cancelled <- readIORef (runCancel running)
       require (not cancelled) 409 "Analysis stopped before provider admission"
       require (clock < runDeadline running) 504 "Call time limit exhausted during provider configuration"
     monitor = guardRun >> threadDelay 50000 >> monitor
     probe arguments = do
      guardRun
      clock <- getMonotonicTimeNSec
      result <- timeout (fromIntegral ((configurationDeadline-clock) `div` 1000)) (race (readProcessWithExitCode binary arguments "") monitor)
      case result of
       Just (Left (ExitSuccess,output,_)) -> pure (T.pack output)
       _ -> failure 503 "Response-only provider configuration could not be verified"
 version <- T.strip <$> probe ["--version"]
 require (version=="codex-cli 0.162.1") 503 "Automation analysis requires verified Codex CLI 0.162.1"
 catalog <- probe ["debug","models","--bundled"]
 (model,_) <- modelDefaults
 selected <- maybe (failure 503 "Configured model has no verified response-only catalog entry") pure
  (listToMaybe [entry | entry<-items (parseJSONText catalog) "models",at entry "slug"==model])
 pure (mergeFields [("shell_type",str "disabled"),("apply_patch_tool_type",Null),("experimental_supported_tools",arr []),("tool_mode",str "direct"),("supports_search_tool",Bool False),("include_skills_usage_instructions",Bool False),("include_plugin_usage_instructions",Bool False),("include_apps_usage_instructions",Bool False)] selected)

withInvocationPolicy :: Service -> Run -> InvocationPolicy -> (FilePath -> [String] -> IO Text) -> IO Text
withInvocationPolicy s run policy action = do
 (model,defaultEffort) <- modelDefaults
 settings <- executionSettings s (agentIdText (runOwner run))
 let effort=if executionMode settings==Deep then "xhigh" else defaultEffort
     settingsArgs=["-c","model_reasoning_effort="++T.unpack (jsonText (str effort)),"-c","model="++T.unpack (jsonText (str model))]
 case policy of
  OrdinaryInvocation -> do
   let directory=workspaceRoot s (agentIdText (runOwner run))
   action directory (["exec"]++settingsArgs++["--skip-git-repo-check","--json","--dangerously-bypass-approvals-and-sandbox","--cd",directory,"--color","never","-"])
  EvidenceAnalysisResponseOnly -> do
   catalog <- analysisCatalogFor (Just run)
   temporary <- getTemporaryDirectory
   identity <- newId
   let directory=temporary </> ("sapiens-analysis-"++T.unpack identity)
   bracket (createDirectory directory >> pure directory) removePathForcibly $ \folder -> do
    let catalogPath=folder </> "models.json"
    writeJSON catalogPath (obj [("models",arr [catalog])])
    let disabled=["shell_tool","shell_snapshot","apps","plugins","multi_agent","computer_use","browser_use","browser_use_external","view_image","image_generation","code_mode_host","hooks","memories","goals","sleep_tool","skill_mcp_dependency_install","tool_suggest","enable_request_compression"]
        config=[("model_catalog_json",str (T.pack catalogPath)),("approval_policy",str "never"),("web_search",str "disabled"),("project_doc_max_bytes",num 0),("mcp_servers",empty)]++[("features."<>feature,Bool False) | feature<-disabled]
        options=concat [["-c",T.unpack key++"="++T.unpack (jsonText value)] | (key,value)<-config]
    action folder (["exec","--ignore-user-config","--ignore-rules","--strict-config","--ephemeral","--skip-git-repo-check","--json","--sandbox","read-only","--cd",folder,"--color","never"]++settingsArgs++options++["-"])

invokeAnalysisObserved :: Service -> Run -> Text -> (EvidenceSnapshot -> IO ()) -> IO Text
invokeAnalysisObserved s run input observe = invokeModelWithPolicy s run EvidenceAnalysisResponseOnly input observe
invokeModel :: Service -> Run -> Text -> IO Text
invokeModel s run input = invokeModelObserved s run input (const (pure ()))

invokeModelObserved :: Service -> Run -> Text -> (EvidenceSnapshot -> IO ()) -> IO Text
invokeModelObserved s run input observe = invokeModelWithPolicy s run OrdinaryInvocation input observe

invokeModelWithPolicy :: Service -> Run -> InvocationPolicy -> Text -> (EvidenceSnapshot -> IO ()) -> IO Text
invokeModelWithPolicy s run policy input observe = withInvocationPolicy s run policy $ \workdir args -> do
 cancelled <- readIORef (runCancel run)
 require (not cancelled) 409 "Stopped by Admin"
 before <- getMonotonicTimeNSec
 require (before < runDeadline run) 504 "Call time limit exhausted between decision nodes"
 binary <- codexBinary >>= maybe (failure 503 "Codex CLI was not found. Install Codex and run codex login.") pure
 (model,_) <- modelDefaults
 environment <- getEnvironment
 started <- realToFrac . (*1000) <$> getPOSIXTime :: IO Double
 writeIORef (runActivity run) (obj [("started",toJSON started),("updated",toJSON started),("phase",str "Waiting for model"),("last_action",str "")])
 startedAt <- now
 let initial=(initialEvidence model startedAt) {evidencePolicy=case policy of OrdinaryInvocation -> OrdinaryEvidence; EvidenceAnalysisResponseOnly -> ResponseOnlyEvidence "codex-cli 0.162.1"}
 capture <- newIORef (Capture initial M.empty M.empty 0 2 False)
 checkpoint <- newIORef before
 let persist = readIORef capture >>= observe . captureSnapshot
     update f = do
      modifyIORef' capture f
      clock <- getMonotonicTimeNSec
      previous <- readIORef checkpoint
      when (clock-previous>=250000000) (persist >> writeIORef checkpoint clock)
 persist
 let invocationEnvironment=case policy of
      OrdinaryInvocation -> ("SAPIENS_AGENCY_RUN",T.unpack (runIdText (runId run))):filter ((/="SAPIENS_AGENCY_RUN") . fst) environment
      EvidenceAnalysisResponseOnly -> filter (\(key,_) -> key `notElem` ["SAPIENS_AGENCY_RUN","SAPIENS_CALL_TOKEN"]) environment
 result <- try $ bracket (createProcess (proc binary args) { cwd=Just workdir,env=Just invocationEnvironment,std_in=CreatePipe,std_out=CreatePipe,std_err=CreatePipe,create_group=True,use_process_jobs=True,detach_console=os=="mingw32" }) cleanup $ \handles -> do
  (hin,hout,herr,ph) <- case handles of
   (Just inputHandle,Just outputHandle,Just errorHandle,process) -> pure (inputHandle,outputHandle,errorHandle,process)
   _ -> failure 500 "Provider process did not expose its configured pipes"
  modifyMVar_ (runProcess run) (const (pure (Just ph)))
  mapM_ (`hSetBinaryMode` True) [hin,hout,herr]
  tools <- newIORef M.empty
  final <- newIORef Nothing
  timedOut <- newIORef False
  let secrets=[T.pack value | (key,value)<-environment,sensitiveKey (T.pack key),length value>=4]
      consume bytes payload = do
       received <- now
       case payload of
        Nothing -> update (dropCapture bytes)
        Just line -> case eitherDecodeStrict' line of
         Left _ -> update (malformedCapture bytes)
         Right rawEvent -> do
          update (receiveCapture secrets received bytes rawEvent)
          event <- case fromJSON rawEvent of
           Error _ -> failure 500 "Provider event schema was invalid; payload omitted"
           Success value -> pure value
          case event of
           SessionStarted session -> modifyIORef' (runActivity run) (put "session" (str session))
           ToolStarted key label -> modifyIORef' tools (\current -> if T.length key<=256 && M.size current<256 then M.insert key label current else current)
           ToolCompleted key label -> modifyIORef' tools (M.delete key) >> modifyIORef' (runActivity run) (put "last_action" (str label))
           AgentMessage message -> writeIORef final (Just message)
           ProviderFailed _ -> failure 500 "Codex turn failed; provider details omitted"
           OtherEvent -> when (at rawEvent "type"=="error") (failure 500 "Codex reported an error; provider details omitted")
          current <- readIORef tools
          updated <- realToFrac . (*1000) <$> getPOSIXTime :: IO Double
          modifyIORef' (runActivity run) (mergeFields [("updated",toJSON updated),("phase",str (if M.null current then "Waiting for model" else "Running tool")),("tool",str (fromMaybe "" (listToMaybe (M.elems current))))])
  withAsync ((BS.hPut hin (TE.encodeUtf8 input) `catch` (\(_::IOException) -> pure ())) `finally` hClose hin) $ \writer ->
   withAsync (drainBounded herr (\_ _ -> pure ())) $ \stderrReader ->
    withAsync (do
      let deadline=runDeadline run
      let watch = do
           threadDelay 50000
           stop <- readIORef (runCancel run)
           time <- getMonotonicTimeNSec
           if stop || time >= deadline then writeIORef timedOut (not stop) >> killTree ph else watch
      watch) $ \_ -> do
     drainBounded hout consume
     update (mapSnapshot (\e -> e {evidenceStreamEnd=True}))
     exit <- waitForProcess ph
     update (mapSnapshot (\e -> e {evidenceExit=Just (case exit of ExitSuccess -> 0; ExitFailure n -> n)}))
     void (waitCatch writer)
     void (waitCatch stderrReader)
     stop <- readIORef (runCancel run)
     expired <- readIORef timedOut
     require (not stop) 409 "Stopped by Admin; review external effects before retrying"
     require (not expired) 504 "Codex exceeded 1800s; its process tree was stopped."
     require (exit == ExitSuccess) 500 "codex exec failed; provider stdout and stderr omitted"
     oversized <- captureOversized <$> readIORef capture
     require (not oversized) 500 "Provider event exceeded the 2 MiB transport limit; result is unresolved"
     readIORef final >>= maybe (failure 500 "Codex completed without an agent message.") pure
 elapsed <- getMonotonicTimeNSec
 modifyIORef' capture (mapSnapshot (\e -> e {evidenceFinished=True,evidenceDuration=Just (fromIntegral (elapsed-before)/1000000)}))
 saved <- try persist :: IO (Either SomeException ())
 case result of
  Left (e::SomeException) -> throwIO e
  Right answer -> either throwIO (const (pure answer)) saved
 where cleanup (hin,hout,herr,ph) = do
        killTree ph
        void (waitForProcess ph)
        forM_ (catMaybes [hin,hout,herr]) $ \h -> hClose h `catch` (\(_::IOException) -> pure ())
        modifyMVar_ (runProcess run) (const (pure Nothing))

-- Payload limits are independent of the supported 100000-character decision reply.
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

data Capture = Capture
 { captureSnapshot :: EvidenceSnapshot, captureIdentities :: M.Map Text Int
 , capturePending :: M.Map Int Bool, captureSequence :: Int, captureBytes :: Int, captureOversized :: Bool
 }
initialEvidence :: Text -> Text -> EvidenceSnapshot
initialEvidence model started = EvidenceSnapshot OrdinaryEvidence model started Nothing Nothing False []
 (EvidenceUsage Nothing Nothing Nothing 0 False 0) False False False (Just 0) 0 0 0 0 0 0 0 0
mapSnapshot :: (EvidenceSnapshot -> EvidenceSnapshot) -> Capture -> Capture
mapSnapshot f capture = capture {captureSnapshot=f (captureSnapshot capture)}
dropCapture :: Integer -> Capture -> Capture
dropCapture bytes capture = (mapSnapshot (\e -> e {evidenceDropped=evidenceDropped e+1,evidenceDroppedBytes=evidenceDroppedBytes e+bytes,evidencePending=Nothing}) capture) {captureOversized=True}
malformedCapture :: Integer -> Capture -> Capture
malformedCapture _ = mapSnapshot (\e -> e {evidenceMalformed=evidenceMalformed e+1})

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
 snapshot=prior
  {evidenceFinal=evidenceFinal prior || kind=="item.completed" && itemKind=="agent_message"
  ,evidenceTurnEnd=evidenceTurnEnd prior || kind=="turn.completed"
  ,evidenceUnknown=evidenceUnknown prior + if known then 0 else 1
  ,evidencePending=if evidencePending prior==Nothing || tool && lifecycle && local==Nothing then Nothing else Just (M.size nextPending)
  ,evidenceOutOfOrder=evidenceOutOfOrder prior + if terminal && maybe True (`M.notMember` pending) local then 1 else 0
  ,evidenceOmittedFields=evidenceOmittedFields prior+maybe 0 summaryOmittedFields summary
  ,evidenceRedactedFields=evidenceRedactedFields prior+maybe 0 summaryRedactedFields summary
  ,evidenceTruncatedFields=evidenceTruncatedFields prior+maybe 0 summaryTruncatedFields summary
  ,evidenceUsage=if kind=="turn.completed" then receiveUsage (get value "usage") (evidenceUsage prior) else evidenceUsage prior}
 identified=capture {captureSnapshot=snapshot,captureIdentities=nextIds,capturePending=nextPending,captureSequence=captureSequence capture+1}
 summary=if tool && lifecycle then Just (toolSummary secrets itemKind item) else Nothing
 status=at item "status"
 receipt=EvidenceReceipt (captureSequence capture+1) received (if known then kind<>if lifecycle then ":"<>itemKind else "" else "unknown") local
  (if status `elem` ["in_progress","completed","failed"] then Just status else Nothing) (case fromJSON (get item "exit_code") of Success n -> Just n; Error _ -> Nothing) summary
 size=fromIntegral (BL.length (encode receipt)) + if null (evidenceReceipts snapshot) then 0 else 1
 appendReceipt current
  | length (evidenceReceipts snapshot)>=256 || captureBytes capture+size>131072 = mapSnapshot (\e -> e {evidenceDropped=evidenceDropped e+1,evidenceDroppedBytes=evidenceDroppedBytes e+bytes}) current
  | otherwise = current {captureSnapshot=snapshot {evidenceReceipts=evidenceReceipts snapshot++[receipt]},captureBytes=captureBytes capture+size}

receiveUsage :: Value -> EvidenceUsage -> EvidenceUsage
receiveUsage value original
 | incoming==(Nothing,Nothing,Nothing) = previous {usageConflict=usageConflict previous || old/=(Nothing,Nothing,Nothing)}
 | old==(Nothing,Nothing,Nothing) = previous {usageInput=input,usageCached=cached,usageOutput=output}
 | incoming==old = previous {usageDuplicates=usageDuplicates previous+1}
 | otherwise = previous {usageConflict=True}
 where
 invalid=any (\key -> has value key && number key==Nothing) ["input_tokens","cached_input_tokens","output_tokens"]
 previous=original {usageInvalid=usageInvalid original+if invalid then 1 else 0}
 number :: Text -> Maybe Integer
 number key = case (fromJSON (get value key) :: Result Int64) of Success n | n>=0 -> Just (fromIntegral n); _ -> Nothing
 input=number "input_tokens"; cached=number "cached_input_tokens"; output=number "output_tokens"
 incoming=(input,cached,output)
 old=(usageInput previous,usageCached previous,usageOutput previous)

-- Recognized credentials cause whole-field omission. This is not an arbitrary
-- secret detector; unrecognized prose remains an explicitly limited summary.
sensitiveKey :: Text -> Bool
sensitiveKey key = any (`T.isInfixOf` normalized) ["password","passwd","secret","token","apikey","authorization","cookie","credential","privatekey","accesskey"]
 where normalized=T.filter isAlphaNum (T.toCaseFold key)
sensitiveText :: [Text] -> Text -> Bool
sensitiveText secrets value = any (`T.isInfixOf` value) secrets || (T.unpack (T.toCaseFold value) =~ pattern_ :: Bool)
 where pattern_ = "(password|passwd|secret|token|api[_-]?key|authorization|cookie|credential|private[_-]?key|access[_-]?key|client[_-]?secret)[\"']?[[:space:]]*[:=]|(bearer|basic)[[:space:]]+[a-z0-9+/=_-]+|sk-[a-z0-9_-]{8,}|ghp_[a-z0-9]+|github_pat_[a-z0-9_]+|akia[a-z0-9]{16}|eyj[a-z0-9_-]+\\.[a-z0-9_-]+\\.[a-z0-9_-]+|-----begin .*private key|--(password|passwd|token|secret|api[_-]?key|access[_-]?key|client[_-]?secret)[[:space:]]+|https?://[^[:space:]/]+:[^[:space:]/]*@|https?://[^[:space:]]*[?&][^=[:space:]]*%[a-f0-9]{2}[^=[:space:]]*=" :: String

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
        in mergeClean (obj [("path",cleanValue path),("kind",if known then str action else Null)]) [path,Sanitized Null (if known then 0 else 1) 0 0]
       chosen=map clean (take 8 (V.toList changes)); extra=max 0 (V.length changes-8)
       result=mergeClean (arr (map cleanValue chosen)) chosen
   in result {cleanOmitted=cleanOmitted result+extra,cleanTruncated=cleanTruncated result+if extra>0 then 1 else 0}
  _ -> Sanitized Null 1 0 0
 selected=[(name,boundClean 4096 (clean value)) | (name,key,clean)<-fields,let value=get item key,has item key]
 allowed=["id","type","status","exit_code"]++[key|(_,key,_)<-fields]
 unknown=length (filter (`notElem` allowed) (keys item))
 summary=ToolSummary [SummaryField name (cleanValue value) (cleanRedacted value>0) (cleanTruncated value>0) (cleanOmitted value>0) | (name,value)<-selected]
  (unknown+sum [cleanOmitted value|(_,value)<-selected]) (sum [cleanRedacted value|(_,value)<-selected]) (sum [cleanTruncated value|(_,value)<-selected])
