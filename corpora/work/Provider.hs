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
import Data.Time.Clock (getCurrentTime,addUTCTime,diffUTCTime)
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
import qualified Effects

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

-- Adapter-owned outcome facts; host owners decide retry and resource policy.
data FailureClass = Temporary503 | PermanentProviderFailure | UnknownProviderOutcome deriving (Eq,Show)
data ProviderFailure = ProviderFailure FailureClass EvidenceSnapshot
instance Show ProviderFailure where
 show (ProviderFailure Temporary503 _)="Provider temporarily unavailable (503); usage may be unknown"
 show (ProviderFailure PermanentProviderFailure _)="Provider authentication or configuration refused"
 show (ProviderFailure UnknownProviderOutcome _)="Provider outcome unknown; no automatic retry"
instance Exception ProviderFailure
data RetryDeferred = RetryDeferred Text Text deriving (Show)
instance Exception RetryDeferred
data ProviderConfigurationFailure = ProviderConfigurationFailure Text deriving (Show)
instance Exception ProviderConfigurationFailure

failureClass :: Text -> FailureClass
failureClass message
 | "unexpected status 503 Service Unavailable:" `T.isPrefixOf` message = Temporary503
 | any (`T.isPrefixOf` message) ["unexpected status 401 Unauthorized:","unexpected status 403 Forbidden:","unexpected status 400 Bad Request:"] = PermanentProviderFailure
 | otherwise = UnknownProviderOutcome

operationFor :: Service -> Run -> Text -> Text -> Text -> Text -> Maybe ModelSettings -> [Value] -> IO Value
operationFor s running node purpose stage request settings operations = case findOperation of
 Just saved->do
  require (at saved "policy"=="response_only" && at saved "node"==node && at saved "promptHash"==sourceHash request && maybe True ((==get saved "modelSettings") . toJSON) settings) 409 "Saved provider operation policy changed"
  pure saved
 Nothing->do
  identity<-newId
  time<-getCurrentTime
  clock<-getMonotonicTimeNSec
  selected<-maybe (resolveModelSettings s running) pure settings
  let remaining=if runDeadline running>clock then runDeadline running-clock else 0
  pure (obj [("id",str identity),("node",str node),("purpose",str purpose),("stageKey",str stage),("status",str "ready"),("request",str request),("promptHash",str (sourceHash request)),("modelSettings",toJSON selected),("pinnedSettings",Bool (isJust settings)),("policy",str "response_only"),("deadline",str (timestamp (addUTCTime (fromIntegral remaining/1000000000) time))),("attempts",arr [])])
 where findOperation=listToMaybe (reverse [op | op<-operations,at op "stageKey"==stage])

-- Host-side maintenance coordination uses the existing owner receipts. The
-- concrete adapter below reports failure facts; this helper proposes bounded
-- waits but owns neither the queue nor the case/preparation allowance.
invokeRetained :: Service -> Run -> Value -> (Value -> IO ()) -> IO Text
invokeRetained s running saved save = do
 let status=at saved "status"
 require (at saved "promptHash"==sourceHash (at saved "request")) 409 "Saved provider request changed"
 if status=="completed" then pure (at saved "rawResponse") else do
  require (status `elem` ["ready","retry_wait"]) 409 "Provider operation is terminal or its prior admission is unknown"
  time<-getCurrentTime
  deadline<-maybe (failure 409 "Saved provider deadline is invalid") pure (parseTime (at saved "deadline"))
  require (time<deadline) 409 "Original provider operation deadline expired"
  forM_ (parseTime (at saved "retryAt")) $ \due->when (time<due) (throwIO (RetryDeferred (at saved "id") (at saved "retryAt")))
  require (length (items saved "attempts")<3) 409 "Provider host-attempt limit exhausted"
  let number=length (items saved "attempts")+1
      attempt=obj [("number",num number),("status",str "reserved"),("started",str (timestamp time)),("usageUnknown",Bool True)]
      initial=mergeFields [("status",str "running"),("attempts",arr (items saved "attempts"++[attempt]))] saved
  current<-newIORef initial
  let update fields=do
       modifyIORef' current (`merge` fields)
       readIORef current >>= save
      observe evidence=do
       value<-readIORef current
       let snapshots=items value "attempts"
           changed=[if get entry "number"==num number then put "evidence" (toJSON evidence) entry else entry | entry<-snapshots]
       update (obj [("evidence",toJSON evidence),("attempts",arr changed)])
  save initial
  clock<-getMonotonicTimeNSec
  let untilDeadline=clock+floor (realToFrac (diffUTCTime deadline time)*1000000000::Double)
      capped=running {runDeadline=min (runDeadline running) untilDeadline}
  settings<-decodeValue 409 (get saved "modelSettings")
  outcome<-try (invokeWithSettingsFor True s capped EvidenceAnalysisResponseOnly settings (boolean (get saved "pinnedSettings")) (at saved "request") observe) :: IO (Either SomeException Text)
  finished<-getCurrentTime
  value<-readIORef current
  let evidence=get value "evidence"
      attempts result=[if get entry "number"==num number then mergeFields [("status",str result),("finished",str (timestamp finished)),("usageUnknown",Bool (get (get evidence "usage") "inputTokens"==Null || get (get evidence "usage") "outputTokens"==Null))] entry else entry | entry<-items value "attempts"]
  case outcome of
   Right _ | finished>=deadline->update (obj [("status",str "expired"),("reason",str "Original provider operation deadline expired"),("attempts",arr (attempts "completed"))]) >> failure 409 "Original provider operation deadline expired"
   Right _ | not (Effects.completeActionFree evidence)->update (obj [("status",str "unknown"),("reason",str "Provider response lacks complete action-free evidence"),("attempts",arr (attempts "unknown"))]) >> failure 409 "Provider response lacks complete action-free evidence"
   Right raw->update (obj [("status",str "completed"),("rawResponse",str raw),("attempts",arr (attempts "completed"))]) >> pure raw
   Left problem | isJust (fromException problem :: Maybe SomeAsyncException) || isJust (fromException problem :: Maybe AsyncCancelled)->do
    update (obj [("status",str "interrupted"),("reason",str "Provider process interrupted; admission and spend may be unknown"),("attempts",arr (attempts "interrupted"))])
    throwIO problem
   Left problem | Just (ProviderConfigurationFailure reason)<-fromException problem->do
    let unused=max 0 (floor (realToFrac (diffUTCTime deadline finished)*1000::Double) :: Int)
        prior=[if get entry "number"==num number then mergeFields [("status",str "not_admitted"),("finished",str (timestamp finished)),("usageUnknown",Bool False),("admission",obj [("schema",num 1),("status",str "not_started"),("authority",str "response-only-configuration"),("requestHash",get saved "promptHash"),("reason",str reason)])] entry else entry | entry<-items value "attempts"]
    update (obj [("status",str "configuration_blocked"),("unusedMilliseconds",num unused),("attempts",arr prior),("error",str reason)])
    throwIO problem
   Left problem->do
    stopped<-readIORef (runCancel running)
    let classified=case fromException problem of Just (ProviderFailure kind _)->kind; Nothing->UnknownProviderOutcome
        eligible=classified==Temporary503 && not stopped
        delay=if number==1 then 1 else 2
        due=addUTCTime delay finished
        waiting=eligible && number<3 && due<deadline
        terminal=if stopped then "cancelled" else if eligible && number>=3 then "exhausted" else if eligible && due>=deadline then "expired" else if classified==PermanentProviderFailure then "permanent" else "unknown"
        reason=if eligible then "The provider rejected this request with 503; model spend may be unknown" else if classified==PermanentProviderFailure then "Provider authentication or configuration was refused; no code repair is authorized" else "Provider outcome is unknown; automatic retry is unavailable"
    update (obj [("status",str (if waiting then "retry_wait" else terminal)),("reason",str reason),("retryAt",if waiting then str (timestamp due) else Null),("attempts",arr (attempts "failed")),("error",str reason)])
    if waiting then throwIO (RetryDeferred (at saved "id") (timestamp due)) else throwIO problem

-- Only an explicit response validator may request this single shape correction.
-- Semantic fields already present in the original response remain immutable.
validatedResponse :: Service -> Run -> Value -> (Value -> IO ()) -> (Value -> IO a) -> IO (Text,Value,a)
validatedResponse s running operation save validate = do
 latest<-newIORef operation
 let saveParent value=writeIORef latest value >> save value
 raw<-invokeRetained s running operation saveParent
 completed<-readIORef latest
 let original=parseJSONText raw
 checked<-try (validate original)
 case checked of
  Right value->pure (at operation "id",original,value)
  Left (APIError code reason) | code==422->do
   let knownCriteria=case get original "criteria" of
        Array values->not (V.null values) && all (\criterion->at criterion "status" `elem` ["pass","fail","unknown","not_applicable"] && has criterion "id") (V.toList values)
        _->False
       knownVerdict=at original "verdict" `elem` ["accepted","rejected","unknown"]
       authority=case at operation "node" of
        "PreparationReview"->case get original "accepted" of Bool _->True; _->False
        "PreparationRubricReview"->knownCriteria
        "CodeAssessment"->knownVerdict && at (get original "acceptance") "status" `elem` ["Completed","Unresolved"]
        "HybridAssessment"->knownVerdict && knownCriteria
        _->case original of Object _->True; _->False
   require authority 422 "Original semantic authority is missing; shape correction is unavailable"
   template<-prompt s "response-correction"
   let request=render template [("request",at operation "request"),("response",raw),("validation",reason)]
       stage=at operation "stageKey"<>":format-correction"
       previous=get completed "correction"
   correction<-if previous/=Null then pure previous else operationFor s running (at operation "node"<>"Correction") "format_correction" stage request Nothing []
   let pinned=put "deadline" (get operation "deadline") correction
       saveCorrection value=do
        current<-readIORef latest
        let changed=put "correction" value current
        writeIORef latest changed
        save changed
   fixedRaw<-invokeRetained s running pinned saveCorrection
   let fixed=parseJSONText fixedRaw
       protected key=key `elem` ["verdict","accepted","acceptance","activationAudit","criteria","evidence","result","outcome","status","action","source","fixtures","input","expected"]
       preserves before after=case before of
        Object fields->all (\(key,value)->if K.toText key=="reason" && value==str "" then True else if protected (K.toText key) then get after (K.toText key)==value else preserves value (get after (K.toText key))) (KM.toList fields)
          && all (\(key,value)->not (protected (K.toText key)) || get before (K.toText key)==value) (KM.toList (case after of Object fields->fields; _->KM.empty))
        Array values->case after of Array next->V.length values==V.length next && and (V.toList (V.zipWith preserves values next)); _->False
        _->before==after
   require (authority && preserves original fixed) 422 "Shape correction cannot create or change semantic authority or source facts"
   value<-validate fixed
   pure (at operation "id",fixed,value)
  Left problem->throwIO problem

waitProjection :: [Value] -> Value
waitProjection operations=fromMaybe Null (listToMaybe (reverse [mergeFields [("attemptCount",num (length (items op "attempts"))),("hostAttemptLimit",num 3)] (select ["id","node","status","retryAt","deadline","reason"] op) | op<-retainedOperations operations,at op "status" `elem` ["retry_wait","exhausted","expired","permanent","unknown","cancelled"]]))

retainedOperations :: [Value] -> [Value]
retainedOperations operations=concatMap (\op->op:retainedOperations [get op "correction" | get op "correction"/=Null]) operations

operationEvidence :: Value -> [Value]
operationEvidence operation=[get entry "evidence" | entry<-retainedOperations [operation]++concatMap (`items` "attempts") (retainedOperations [operation]),get entry "evidence"/=Null]

operationUsage :: Value -> [Value]
operationUsage operation=own++concatMap operationUsage [get operation "correction" | get operation "correction"/=Null]
 where own=case items operation "attempts" of
        []->[operation]
        attempts->[mergeFields [("id",str (at operation "id"<>":"<>T.pack (show (int (get attempt "number"))))),("operation",get operation "id"),("attempt",get attempt "number"),("status",get attempt "status"),("evidence",get attempt "evidence"),("usageUnknown",get attempt "usageUnknown")] operation | attempt<-attempts]
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
  SessionStarted session -> put "session" (str session) activity
  ToolCompleted _ label -> put "last_action" (str label) activity
  _ -> activity

modelDefaults :: IO (Text,Text)
modelDefaults = (,) <$> (maybe "gpt-6-sol" T.pack <$> lookupEnv "SAPIENS_CODEX_MODEL") <*> (maybe "high" T.pack <$> lookupEnv "SAPIENS_CODEX_REASONING_EFFORT")
codexBinary :: IO (Maybe FilePath)
codexBinary = codexBinaryFor Nothing

-- Ordinary work prefers the newest installed CLI. Response-only execution must
-- select a compatible candidate before applying its stricter verified policy.
codexBinaryFor :: Maybe Text -> IO (Maybe FilePath)
codexBinaryFor expected = do
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
         Right (Just (ExitSuccess,out,_)) | maybe True (==T.strip (T.pack out)) expected -> let (_,_,_,numbers) = out =~ ("codex-cli[[:space:]]+([0-9]+)\\.([0-9]+)\\.([0-9]+)" :: String) :: (String,String,String,[String]) in mapMaybe readMaybe numbers
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
analysisCatalog = do
 (model,effort)<-modelDefaults
 analysisCatalogFor Nothing (ModelSettings model effort)
analysisCatalogFor :: Maybe Run -> ModelSettings -> IO Value
analysisCatalogFor activeRun selectedSettings = snd <$> analysisProviderFor activeRun selectedSettings

analysisProviderFor :: Maybe Run -> ModelSettings -> IO (FilePath,Value)
analysisProviderFor activeRun selectedSettings = do
 binary <- codexBinaryFor (Just "codex-cli 0.162.1") >>= maybe (failure 503 "Verified Codex CLI is unavailable for analysis") pure
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
 let model=modelSlug selectedSettings
 selected <- maybe (failure 503 "Configured model has no verified response-only catalog entry") pure
  (listToMaybe [entry | entry<-items (parseJSONText catalog) "models",at entry "slug"==model])
 pure (binary,mergeFields [("shell_type",str "disabled"),("apply_patch_tool_type",Null),("experimental_supported_tools",arr []),("tool_mode",str "direct"),("supports_search_tool",Bool False),("include_skills_usage_instructions",Bool False),("include_plugin_usage_instructions",Bool False),("include_apps_usage_instructions",Bool False)] selected)

explanationTemplates :: Service -> Explanation -> IO (Text,Text)
explanationTemplates s selected = do
 template<-prompt s "hybrid-explanation"
 envelope<-prompt s "preparation-envelope"
 require (sourceHash template==explanationTemplateHash selected && sourceHash envelope==explanationEnvelopeHash selected) 409 "Pinned explanation prompt templates changed; prepare a new candidate"
 pure (template,envelope)

resolveModelSettings :: Service -> Run -> IO ModelSettings
resolveModelSettings s running = do
 (model,defaultEffort)<-modelDefaults
 settings<-executionSettings s (agentIdText (runOwner running))
 pure (ModelSettings model (if executionMode settings==Deep then "xhigh" else defaultEffort))

verifiedResponseSettings :: Service -> Run -> IO ModelSettings
verifiedResponseSettings s running = do
 settings<-resolveModelSettings s running
 catalog<-analysisCatalogFor (Just running) settings
 verifyReasoning settings catalog
 pure settings

verifyReasoning :: ModelSettings -> Value -> IO ()
verifyReasoning settings catalog = require (modelReasoning settings `elem` map (`at` "effort") (items catalog "supported_reasoning_levels")) 503 "Pinned reasoning effort is unavailable in the verified model catalog"

withInvocationPolicy :: Service -> Run -> InvocationPolicy -> ModelSettings -> Bool -> (FilePath -> FilePath -> [String] -> IO Text) -> IO Text
withInvocationPolicy s run policy settings pinned action = do
 let settingsArgs=["-c","model_reasoning_effort="++T.unpack (jsonText (str (modelReasoning settings))),"-c","model="++T.unpack (jsonText (str (modelSlug settings)))]
 case policy of
  OrdinaryInvocation -> do
   binary<-codexBinary >>= maybe (failure 503 "Codex CLI was not found") pure
   let directory=workspaceRoot s (agentIdText (runOwner run))
   action binary directory (["exec"]++settingsArgs++["--skip-git-repo-check","--json","--dangerously-bypass-approvals-and-sandbox","--cd",directory,"--color","never","-"])
  EvidenceAnalysisResponseOnly -> do
   (binary,catalog) <- analysisProviderFor (Just run) settings `catch` (\problem@(APIError code reason)->if code==503 then throwIO (ProviderConfigurationFailure reason) else throwIO problem)
   when pinned (verifyReasoning settings catalog)
   temporary <- getTemporaryDirectory
   identity <- newId
   let directory=temporary </> ("sapiens-analysis-"++T.unpack identity)
   bracket (createDirectory directory >> pure directory) removePathForcibly $ \folder -> do
    let catalogPath=folder </> "models.json"
    writeJSON catalogPath (obj [("models",arr [catalog])])
    let disabled=["shell_tool","shell_snapshot","apps","plugins","multi_agent","computer_use","browser_use","browser_use_external","view_image","image_generation","code_mode_host","hooks","memories","goals","sleep_tool","skill_mcp_dependency_install","tool_suggest","enable_request_compression"]
        config=[("model_catalog_json",str (T.pack catalogPath)),("approval_policy",str "never"),("web_search",str "disabled"),("project_doc_max_bytes",num 0),("mcp_servers",empty)]++[("features."<>feature,Bool False) | feature<-disabled]
        options=concat [["-c",T.unpack key++"="++T.unpack (jsonText value)] | (key,value)<-config]
    action binary folder (["exec","--ignore-user-config","--ignore-rules","--strict-config","--ephemeral","--skip-git-repo-check","--json","--sandbox","read-only","--cd",folder,"--color","never"]++settingsArgs++options++["-"])

invokeAnalysisObserved :: Service -> Run -> Text -> (EvidenceSnapshot -> IO ()) -> IO Text
invokeAnalysisObserved s run input observe = invokeModelWithPolicy s run EvidenceAnalysisResponseOnly input observe
invokeModel :: Service -> Run -> Text -> IO Text
invokeModel s run input = invokeModelObserved s run input (const (pure ()))

invokeModelObserved :: Service -> Run -> Text -> (EvidenceSnapshot -> IO ()) -> IO Text
invokeModelObserved s run input observe = invokeModelWithPolicy s run OrdinaryInvocation input observe

invokeModelWithPolicy :: Service -> Run -> InvocationPolicy -> Text -> (EvidenceSnapshot -> IO ()) -> IO Text
invokeModelWithPolicy s run policy input observe = do
 settings<-resolveModelSettings s run
 invokeWithSettings s run policy settings False input observe

invokeResponseOnlyObservedWith :: Service -> Run -> ModelSettings -> Text -> (EvidenceSnapshot -> IO ()) -> IO Text
invokeResponseOnlyObservedWith s run settings = invokeWithSettings s run EvidenceAnalysisResponseOnly settings True

invokeWithSettings :: Service -> Run -> InvocationPolicy -> ModelSettings -> Bool -> Text -> (EvidenceSnapshot -> IO ()) -> IO Text
invokeWithSettings s run policy settings pinned input observe = invokeWithSettingsFor False s run policy settings pinned input observe

invokeWithSettingsFor :: Bool -> Service -> Run -> InvocationPolicy -> ModelSettings -> Bool -> Text -> (EvidenceSnapshot -> IO ()) -> IO Text
invokeWithSettingsFor maintenance s run policy settings pinned input observe = withInvocationPolicy s run policy settings pinned $ \binary workdir args -> do
 cancelled <- readIORef (runCancel run)
 require (not cancelled) 409 "Stopped by Admin"
 before <- getMonotonicTimeNSec
 require (before < runDeadline run) 504 "Call time limit exhausted between decision nodes"
 let model=modelSlug settings
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
  stream <- newIORef (ProviderState M.empty Nothing)
  timedOut <- newIORef False
  terminalFailure <- newIORef Nothing
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
           ProviderFailed value | maintenance -> writeIORef terminalFailure (Just (failureClass (at (get value "error") "message")))
           OtherEvent | not maintenance && at rawEvent "type"=="error" -> failure 500 "Codex reported an error; provider details omitted"
           _ -> do
            state <- readIORef stream
            activity <- readIORef (runActivity run)
            time <- realToFrac . (*1000) <$> getPOSIXTime :: IO Double
            (updated,reported) <- validated (providerStep time event state activity)
            writeIORef stream updated
            writeIORef (runActivity run) reported
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
     terminal <- readIORef terminalFailure
     forM_ terminal $ \kind -> do
      snapshot<-captureSnapshot <$> readIORef capture
      let closed=snapshot {evidenceFinished=True,evidenceExit=Just (case exit of ExitSuccess->0; ExitFailure n->n),evidenceStreamEnd=True}
          noExecution=not (any (\receipt->receiptSummary receipt/=Nothing) (evidenceReceipts closed)) && not (evidenceFinal closed) && evidencePending closed==Just 0 && evidenceMalformed closed==0 && evidenceUnknown closed==0 && evidenceDropped closed==0 && evidenceOutOfOrder closed==0
          verified=case evidencePolicy closed of ResponseOnlyEvidence "codex-cli 0.162.1"->True; _->False
      throwIO (ProviderFailure (if verified && noExecution then kind else UnknownProviderOutcome) closed)
     require (exit == ExitSuccess) 500 "codex exec failed; provider stdout and stderr omitted"
     oversized <- captureOversized <$> readIORef capture
     require (not oversized) 500 "Provider event exceeded the 2 MiB transport limit; result is unresolved"
     (providerMessage <$> readIORef stream) >>= maybe (failure 500 "Codex completed without an agent message.") pure
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
