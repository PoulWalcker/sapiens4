{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Werror=incomplete-patterns #-}
-- Pure domain data and codecs shared by the host and native shells.
module Domain where

import Control.Monad
import Data.Aeson hiding (pairs)
import Data.Aeson.Types (Parser)
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as BL
import Data.Char (isAscii, isAlphaNum, isHexDigit)
import Data.List (find,sortOn,nub,stripPrefix)
import Crypto.Hash.SHA256 (hash)
import qualified Data.ByteString as BS
import Numeric (showHex)
import qualified Network.URI as URI
import Text.Read (readMaybe)
import Data.Maybe
import qualified Data.Map.Strict as M
import Data.Scientific (toBoundedInteger)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V

newtype AgentId = AgentId { agentIdText :: Text } deriving (Eq, Ord, Show, FromJSON, ToJSON)
newtype RunId = RunId { runIdText :: Text } deriving (Eq, Ord, Show, FromJSON, ToJSON)

-- This profile transports accepted content; it does not redefine Task completion.
newtype EffectEndpoint = EffectEndpoint { effectURL :: Text } deriving (Eq,Show)
instance ToJSON EffectEndpoint where toJSON=toJSON . effectURL
instance FromJSON EffectEndpoint where
 parseJSON value=do
  url<-parseJSON value
  unless (not (T.null url) && T.length url<=2000 && T.strip url==url) (fail "Invalid external endpoint URL")
  uri<-maybe (fail "Invalid external endpoint URL") pure (URI.parseURI (T.unpack url))
  authority<-maybe (fail "External endpoint needs an authority") pure (URI.uriAuthority uri)
  let scheme=URI.uriScheme uri; host=URI.uriRegName authority; port=URI.uriPort authority
      validPort=null port || case stripPrefix ":" port >>= (readMaybe :: String -> Maybe Int) of Just n->n>0 && n<=65535; Nothing->False
  unless (null (URI.uriUserInfo authority) && null (URI.uriQuery uri) && null (URI.uriFragment uri) && not (null host) && validPort
    && (scheme=="https:" || scheme=="http:" && host `elem` ["localhost","127.0.0.1","[::1]"])) (fail "Use a fixed HTTPS endpoint or exact loopback HTTP without credentials, query or fragment")
  pure (EffectEndpoint url)

data FeedSource = FeedSource { feedURL :: EffectEndpoint, feedIdentity :: Text, feedInitialCursor :: Text } deriving (Eq,Show)
instance FromJSON FeedSource where
 parseJSON value=do
  onlyFields ["url","identity","initialCursor"] value
  FeedSource <$> parseJSON (get value "url") <*> textFieldP "identity" 128 value <*> cursorP (get value "initialCursor")
instance ToJSON FeedSource where
 toJSON source=obj [("url",toJSON (feedURL source)),("identity",str (feedIdentity source)),("initialCursor",str (feedInitialCursor source))]
data DeliveryTarget = DeliveryTarget { deliveryURL :: EffectEndpoint, receiptURL :: EffectEndpoint, receiverIdentity :: Text } deriving (Eq,Show)
instance FromJSON DeliveryTarget where
 parseJSON value=do
  onlyFields ["url","receiptUrl","identity"] value
  DeliveryTarget <$> parseJSON (get value "url") <*> parseJSON (get value "receiptUrl") <*> textFieldP "identity" 128 value
instance ToJSON DeliveryTarget where
 toJSON target=obj [("url",toJSON (deliveryURL target)),("receiptUrl",toJSON (receiptURL target)),("identity",str (receiverIdentity target))]
data ExternalProfile = ExternalProfile { externalSource :: FeedSource, externalDelivery :: DeliveryTarget } deriving (Eq,Show)
instance FromJSON ExternalProfile where
 parseJSON value=do
  onlyFields ["kind","taskBoundary","executionPolicy","source","delivery"] value
  unless (at value "kind"=="receipt_feed_v1" && at value "taskBoundary"=="content_result" && at value "executionPolicy"=="response_only") (fail "Only the explicit response-only content_result receipt feed profile is supported")
  ExternalProfile <$> parseJSON (get value "source") <*> parseJSON (get value "delivery")
instance ToJSON ExternalProfile where
 toJSON profile=obj [("kind",str "receipt_feed_v1"),("taskBoundary",str "content_result"),("executionPolicy",str "response_only"),("source",toJSON (externalSource profile)),("delivery",toJSON (externalDelivery profile))]
data ProfileChange = PreserveProfile | RemoveProfile | ConfigureProfile ExternalProfile deriving (Eq,Show)
profileChange :: Value -> Parser ProfileChange
profileChange value
 | not (has value "externalProfile") = pure PreserveProfile
 | get value "externalProfile"==Null = pure RemoveProfile
 | otherwise = ConfigureProfile <$> parseJSON (get value "externalProfile")

cursorP :: Value -> Parser Text
cursorP value=do
 cursor<-parseJSON value
 unless (T.length cursor<=4000) (fail "Cursor exceeds the supported protocol size")
 pure cursor

data FeedPage = FeedPage { pageSource :: Text, pageCursor :: Text, pageNext :: Text, pageRecords :: [(Text,Value)] } deriving (Eq,Show)
instance FromJSON FeedPage where
 parseJSON value=do
  onlyFields ["schema","source","cursor","nextCursor","complete","records"] value
  unless (get value "schema"==num 1 && get value "complete"==Bool True) (fail "Feed page is incomplete or unsupported")
  rawRecords<-withArray "Feed records" (mapM record . V.toList) (get value "records")
  let records=nub rawRecords
  unless (length (nub (map fst records))==length records) (fail "Conflicting values for the same feed record ID")
  FeedPage <$> textFieldP "source" 128 value <*> cursorP (get value "cursor") <*> cursorP (get value "nextCursor") <*> pure records
  where record item=do
         onlyFields ["id","value"] item
         unless (has item "value") (fail "Feed record value is missing")
         (,) <$> textFieldP "id" 512 item <*> pure (get item "value")

-- Source progress is host authority, not a model-provided input default.
data FeedState = FeedState { feedStateRevision :: Int, feedStateCursor :: Text, feedStateSeen :: [Text] } deriving (Eq,Show)
feedState :: ExternalProfile -> Value -> Parser FeedState
feedState profile value = do
 revision <- withObject "Feed state" (.: "revision") value
 unless (revision > 0) (fail "Source state revision must be positive")
 unless (get value "profileHash" == str (canonicalHash (toJSON profile)) && get value "sourceHash" == str (canonicalHash (toJSON (externalSource profile)))) (fail "Source state identity differs from its profile")
 FeedState revision <$> cursorP (get value "cursor") <*> feedSeenIds (get value "seenIds")

feedSeenIds :: Value -> Parser [Text]
feedSeenIds value = do
 ids <- parseJSON value
 unless (all (\identity -> not (T.null (T.strip identity)) && T.length identity <= 512) ids && length (nub ids) == length ids) (fail "Source seen IDs must be unique bounded text")
 pure ids

data FeedContentInterface = ScriptText | HybridText deriving (Eq,Show)
instance ToJSON FeedContentInterface where
 toJSON value = str (case value of ScriptText -> "script-text-v1"; HybridText -> "hybrid-explanation-v1")
instance FromJSON FeedContentInterface where
 parseJSON value = case text value of
  "script-text-v1" -> pure ScriptText
  "hybrid-explanation-v1" -> pure HybridText
  _ -> fail "Unsupported feed content interface"

data FeedInputDescriptor = FeedInputDescriptor Text Int Text FeedContentInterface deriving (Eq,Show)
instance FromJSON FeedInputDescriptor where
 parseJSON value = do
  onlyFields ["kind","interface","profileHash","externalProfileRevision","sourceHash","contentInterface"] value
  unless (at value "kind" == "receipt_feed_v1" && at value "interface" == "receipt-feed-input-v1") (fail "Unsupported feed input interface")
  revision <- withObject "Feed input descriptor" (.: "externalProfileRevision") value
  unless (revision > 0) (fail "Invalid feed input profile revision")
  FeedInputDescriptor <$> hashField "profileHash" <*> pure revision <*> hashField "sourceHash" <*> parseJSON (get value "contentInterface")
  where hashField key = do
         digest <- textFieldP key 64 value
         unless (T.length digest == 64 && T.all isHexDigit digest) (fail "Invalid feed identity hash")
         pure digest
instance ToJSON FeedInputDescriptor where
 toJSON (FeedInputDescriptor profile revision source content) = obj [("kind",str "receipt_feed_v1"),("interface",str "receipt-feed-input-v1"),("profileHash",str profile),("externalProfileRevision",num revision),("sourceHash",str source),("contentInterface",toJSON content)]

feedDescriptor :: ExternalProfile -> Int -> FeedContentInterface -> FeedInputDescriptor
feedDescriptor profile revision = FeedInputDescriptor (canonicalHash (toJSON profile)) revision (canonicalHash (toJSON (externalSource profile)))

feedBusinessInput :: ExternalProfile -> Value -> Value -> [Text] -> Parser Value
feedBusinessInput profile parameters observation seen = do
 page <- parseJSON observation
 unless (pageSource page == feedIdentity (externalSource profile)) (fail "Feed observation belongs to another source")
 let records = pageRecords page
     fresh = [obj [("id",str identity),("value",value)] | (identity,value) <- records, identity `notElem` seen]
     relevant = [identity | (identity,_) <- records, identity `elem` seen]
     input = obj [("schema",num 1),("interface",str "receipt-feed-input-v1"),("taskInputs",parameters),("observation",observation),("newRecords",arr fresh),("previouslySeenIds",toJSON relevant)]
 unless (BL.length (encode input) <= 65536) (fail "Observed business input exceeds 64 KiB")
 pure input

feedContentText :: Value -> Parser Text
feedContentText value = do
 onlyFields ["text"] value
 result <- textFieldP "text" 65536 value
 unless (BS.length (TE.encodeUtf8 result) <= 65536) (fail "Feed content exceeds 64 KiB")
 pure result

data DeliveryState = DeliveryPrepared | DeliveryStarted | DeliveryUnknown | DeliveryConfirmed | DeliveryNotSent | DeliveryNotRequired deriving (Eq,Show)
instance FromJSON DeliveryState where
 parseJSON value=case text value of
  "prepared"->pure DeliveryPrepared; "started"->pure DeliveryStarted; "unknown"->pure DeliveryUnknown; "confirmed"->pure DeliveryConfirmed; "not_sent"->pure DeliveryNotSent; "not_required"->pure DeliveryNotRequired; _->fail "Unknown delivery state"
instance ToJSON DeliveryState where
 toJSON state=str (case state of DeliveryPrepared->"prepared"; DeliveryStarted->"started"; DeliveryUnknown->"unknown"; DeliveryConfirmed->"confirmed"; DeliveryNotSent->"not_sent"; DeliveryNotRequired->"not_required")

-- Requested configuration is pinned; remote model weights are not versioned here.
data ModelSettings = ModelSettings { modelSlug :: Text, modelReasoning :: Text } deriving (Eq,Show)
instance FromJSON ModelSettings where
 parseJSON value = do
  onlyFields ["slug","reasoning"] value
  ModelSettings <$> textFieldP "slug" 200 value <*> textFieldP "reasoning" 40 value
instance ToJSON ModelSettings where
 toJSON settings=obj [("slug",str (modelSlug settings)),("reasoning",str (modelReasoning settings))]

data CriterionKind = NumericCriterion | SemanticCriterion deriving (Eq,Show)
instance FromJSON CriterionKind where
 parseJSON value=case text value of "numeric"->pure NumericCriterion; "semantic"->pure SemanticCriterion; _->fail "Unknown criterion kind"
instance ToJSON CriterionKind where
 toJSON kind=str (case kind of NumericCriterion->"numeric"; SemanticCriterion->"semantic")
data RubricCriterion = RubricCriterion Text CriterionKind deriving (Eq,Show)
instance FromJSON RubricCriterion where
 parseJSON value=do
  onlyFields ["id","kind"] value
  RubricCriterion <$> textFieldP "id" 80 value <*> parseJSON (get value "kind")
instance ToJSON RubricCriterion where
 toJSON (RubricCriterion identity kind)=obj [("id",str identity),("kind",toJSON kind)]
data CriterionVerdict = CriterionPass | CriterionFail | CriterionUnknown | CriterionNotApplicable deriving (Eq,Show)
instance FromJSON CriterionVerdict where
 parseJSON value=case text value of "pass"->pure CriterionPass; "fail"->pure CriterionFail; "unknown"->pure CriterionUnknown; "not_applicable"->pure CriterionNotApplicable; _->fail "Unknown criterion verdict"
data CriterionAssessment = CriterionAssessment Text CriterionVerdict Text deriving (Eq,Show)
instance FromJSON CriterionAssessment where
 parseJSON value=do
  onlyFields ["id","status","evidence"] value
  CriterionAssessment <$> textFieldP "id" 80 value <*> parseJSON (get value "status") <*> textFieldP "evidence" 16000 value

data Explanation = Explanation { explanationInstruction :: Text, explanationModel :: ModelSettings, explanationTemplateHash :: Text, explanationEnvelopeHash :: Text } deriving (Eq,Show)
instance FromJSON Explanation where
 parseJSON value=do
  onlyFields ["instruction","model","policy","implementationHash","templateHash","envelopeHash"] value
  unless (get value "policy"==str "response_only" && get value "implementationHash"==str (canonicalHash (remove ["implementationHash"] value))) (fail "Explanation policy or immutable hash changed")
  Explanation <$> textFieldP "instruction" 16000 value <*> parseJSON (get value "model") <*> textFieldP "templateHash" 64 value <*> textFieldP "envelopeHash" 64 value

narrativeText :: Value -> Parser Text
narrativeText value=onlyFields ["text"] value >> textFieldP "text" 16000 value

hybridComposition :: Value
hybridComposition=obj [("schema",num 1),("steps",arr [
 obj [("id",str "calculate"),("kind",str "code"),("input",str "accepted-input-v1"),("output",str "json-result-facts-v1")],
 obj [("id",str "explain"),("kind",str "agent"),("input",str "accepted-input-and-facts-v1"),("output",str "narrative-text-v1")]])]

candidateExplanation :: Value -> Parser (Maybe Explanation)
candidateExplanation version
 | not (any (has version) ["explanation","composition","structureHash","rubric"]) = pure Nothing
 | not (has version "rubric") || get version "composition"/=hybridComposition || get version "structureHash"/=str (canonicalHash hybridComposition) = fail "Unsupported or changed hybrid composition"
 | otherwise = Just <$> parseJSON (get version "explanation")

data OrdinarySource = RoutineSnapshotSource | TaskDecisionSource Text deriving (Eq, Show)
data CandidateReference = CandidateReference AgentId RunId Text deriving (Eq,Show)
instance FromJSON CandidateReference where
 parseJSON value = do
  onlyFields ["owner","preparation","digest"] value
  owner<-AgentId <$> textFieldP "owner" 128 value
  preparation<-RunId <$> textFieldP "preparation" 128 value
  digest<-textFieldP "digest" 64 value
  unless (T.length digest==64 && T.all isHexDigit digest) (fail "Invalid candidate content identity")
  pure (CandidateReference owner preparation digest)
instance ToJSON CandidateReference where
 toJSON (CandidateReference owner preparation digest)=obj [("owner",toJSON owner),("preparation",toJSON preparation),("digest",str digest)]
data CodeRole = CandidateTrial | ActiveCode deriving (Eq,Show)
instance ToJSON CodeRole where
 toJSON value=str (case value of CandidateTrial->"trial"; ActiveCode->"active")
instance FromJSON CodeRole where
 parseJSON value=case text value of "trial"->pure CandidateTrial; "active"->pure ActiveCode; _->fail "Unknown code execution role"
data ExecutionBinding = OrdinaryBinding AgentId OrdinarySource | ScriptBinding AgentId OrdinarySource CandidateReference CodeRole deriving (Eq, Show)
bindingContract :: ExecutionBinding -> (AgentId,OrdinarySource)
bindingContract value=case value of OrdinaryBinding owner source->(owner,source); ScriptBinding owner source _ _->(owner,source)
instance FromJSON ExecutionBinding where
 parseJSON value = do
  let script=at value "mode"=="script"
  onlyFields (["schema","mode","owner","source"]++if script then ["reference","role"] else []) value
  unless (get value "schema"==num 1 && at value "mode" `elem` ["ordinary","script"]) (fail "Unsupported execution binding format")
  owner <- parseJSON (get value "owner")
  let source=get value "source"
  kind <- case at source "kind" of
   "routine_snapshot" -> onlyFields ["kind"] source >> pure RoutineSnapshotSource
   "task_decision" -> onlyFields ["kind","decisionId"] source >> TaskDecisionSource <$> textFieldP "decisionId" 128 source
   _ -> fail "Unknown accepted contract source"
  if script then ScriptBinding owner kind <$> parseJSON (get value "reference") <*> parseJSON (get value "role") else pure (OrdinaryBinding owner kind)
instance ToJSON ExecutionBinding where
 toJSON value = obj ([("schema",num 1),("mode",str mode),("owner",toJSON owner),("source",case source of
  RoutineSnapshotSource -> obj [("kind",str "routine_snapshot")]
  TaskDecisionSource identity -> obj [("kind",str "task_decision"),("decisionId",str identity)])]++extra)
  where
  (owner,source)=bindingContract value
  (mode,extra)=case value of OrdinaryBinding _ _->("ordinary",[]); ScriptBinding _ _ reference role->("script",[("reference",toJSON reference),("role",toJSON role)])

sourceHash :: Text -> Text
sourceHash = hex . hash . TE.encodeUtf8
 where hex = T.pack . concatMap (\b -> let v=showHex b "" in if length v==1 then '0':v else v) . BS.unpack
canonicalHash :: Value -> Text
canonicalHash = sourceHash . TE.decodeUtf8 . BL.toStrict . canonical
 where
 canonical (Object fields) = "{" <> BL.intercalate "," [encode (K.toText key)<> ":" <>canonical value | (key,value)<-sortOn (K.toText . fst) (KM.toList fields)] <> "}"
 canonical (Array values) = "[" <> BL.intercalate "," (map canonical (V.toList values)) <> "]"
 canonical value = encode value

newtype ContractGeneration = ContractGeneration Int deriving (Eq, Show, ToJSON)
instance FromJSON ContractGeneration where
 parseJSON value = do
  generation <- parseJSON value
  if generation > 0 then pure (ContractGeneration generation) else fail "Contract generation must be positive"

data AnalysisPin = AnalysisPin
 { analysisOwner :: AgentId, analysisRoutine :: Text, analysisGeneration :: ContractGeneration
 , analysisFeedback :: [Text], analysisSources :: [RunId]
 } deriving (Eq, Show)
instance FromJSON AnalysisPin where
 parseJSON = withObject "Analysis identity" $ \o -> do
  owner <- o .: "owner"
  routine <- o .: "routine"
  unless (not (T.null routine) && T.length routine <= 128) (fail "Invalid analysis consumer")
  AnalysisPin owner routine <$> o .: "contractGeneration" <*> o .: "feedback" <*> o .: "sources"
instance ToJSON AnalysisPin where
 toJSON pin = obj [("owner",toJSON (analysisOwner pin)),("routine",str (analysisRoutine pin)),("contractGeneration",toJSON (analysisGeneration pin)),("feedback",toJSON (analysisFeedback pin)),("sources",toJSON (analysisSources pin))]

data AnalysisParameter = AnalysisParameter Text Text deriving (Eq, Show)
instance FromJSON AnalysisParameter where
 parseJSON value = do
  onlyFields ["name","description"] value
  AnalysisParameter <$> textFieldP "name" 128 value <*> textFieldP "description" 2000 value
instance ToJSON AnalysisParameter where
 toJSON (AnalysisParameter name description) = obj [("name",str name),("description",str description)]
data AnalysisDecision
 = AnalysisReady Text [AnalysisParameter]
 | AnalysisNeedsEvidence [Text]
 | AnalysisDeclined Text
 deriving (Eq, Show)
instance FromJSON AnalysisDecision where
 parseJSON value = case at value "status" of
  "Ready" -> do
   onlyFields ["status","automatablePart","parameters"] value
   parameters <- fieldP "parameters" value
   unless (length parameters<=30) (fail "Too many analysis parameters")
   AnalysisReady <$> textFieldP "automatablePart" 8000 value <*> pure parameters
  "NeedMoreEvidence" -> do
   onlyFields ["status","missingObservations"] value
   missing <- fieldP "missingObservations" value
   unless (not (null missing) && length missing<=30 && all (\t -> not (T.null (T.strip t)) && T.length t<=2000) missing) (fail "Provide concrete missing observations")
   pure (AnalysisNeedsEvidence missing)
  "Decline" -> onlyFields ["status","reconsiderCondition"] value >> AnalysisDeclined <$> textFieldP "reconsiderCondition" 4000 value
  _ -> fail "Unknown automation analysis decision"
  where fieldP key = parseJSON . (`get` key)
instance ToJSON AnalysisDecision where
 toJSON decision = case decision of
  AnalysisReady part parameters -> obj [("status",str "Ready"),("automatablePart",str part),("parameters",toJSON parameters)]
  AnalysisNeedsEvidence missing -> obj [("status",str "NeedMoreEvidence"),("missingObservations",toJSON missing)]
  AnalysisDeclined condition -> obj [("status",str "Decline"),("reconsiderCondition",str condition)]

-- Wire spellings live at the codec boundary; domain code matches constructors.
class (Eq a, Enum a, Bounded a) => Wire a where
 wireName :: a -> Text
parseWire :: Wire a => Value -> Parser a
parseWire = withText "domain value" $ \name ->
 maybe (fail ("Unknown value: " ++ T.unpack name)) pure (find ((==name) . wireName) [minBound..maxBound])

-- Missing trigger is the explicitly supported pre-trigger interval format.
data RoutineTrigger = ManualTrigger | IntervalTrigger deriving (Eq, Show, Enum, Bounded)
instance Wire RoutineTrigger where
 wireName ManualTrigger = "manual"
 wireName IntervalTrigger = "interval"
instance FromJSON RoutineTrigger where parseJSON = parseWire
instance ToJSON RoutineTrigger where toJSON = str . wireName
routineTrigger :: Value -> Parser RoutineTrigger
routineTrigger = withObject "Routine" $ \o -> case KM.lookup "trigger" o of
 Nothing -> pure IntervalTrigger
 Just value -> parseJSON value

data ExecutionGate = ExecutionOpen | ExecutionStopped deriving (Eq, Show, Enum, Bounded)
instance Wire ExecutionGate where
 wireName ExecutionOpen = "open"
 wireName ExecutionStopped = "stopped"
instance FromJSON ExecutionGate where parseJSON = parseWire
instance ToJSON ExecutionGate where toJSON = str . wireName

data TurnStatus = Queued | Running | OutputPending | Done | Failed | Interrupted | Cancelled | Warning deriving (Eq, Ord, Show, Enum, Bounded)
instance Wire TurnStatus where
 wireName status = case status of Queued -> "queued"; Running -> "running"; OutputPending -> "output_pending"; Done -> "done"; Failed -> "failed"; Interrupted -> "interrupted"; Cancelled -> "cancelled"; Warning -> "warning"
instance FromJSON TurnStatus where parseJSON = parseWire
instance ToJSON TurnStatus where toJSON = String . wireName
data Flow = Chat | Computer deriving (Eq, Ord, Show, Enum, Bounded)
instance Wire Flow where wireName flow = case flow of Chat -> "chat"; Computer -> "computer"
instance FromJSON Flow where parseJSON = parseWire
instance ToJSON Flow where toJSON = String . wireName
data ExecutionMode = Normal | Deep deriving (Eq, Ord, Show, Enum, Bounded)
instance Wire ExecutionMode where wireName mode = case mode of Normal -> "normal"; Deep -> "deep"
instance FromJSON ExecutionMode where parseJSON = parseWire
instance ToJSON ExecutionMode where toJSON = String . wireName
newtype ExecutionSettings = ExecutionSettings { executionMode :: ExecutionMode } deriving (Eq, Show)
instance FromJSON ExecutionSettings where
 parseJSON = withObject "Execution settings" $ \o -> ExecutionSettings <$> o .:? "mode" .!= Normal
instance ToJSON ExecutionSettings where
 toJSON settings = object ["mode" .= executionMode settings, "timeout_seconds" .= (1800 :: Int)]
data CorporaStatus = Active | Paused | Archived deriving (Eq, Ord, Show, Enum, Bounded)
instance Wire CorporaStatus where wireName status = case status of Active -> "active"; Paused -> "paused"; Archived -> "archived"
instance FromJSON CorporaStatus where parseJSON = parseWire
instance ToJSON CorporaStatus where toJSON = String . wireName
data CorporaConfig = CorporaConfig
 { corporaName :: Maybe Text, corporaDevice :: Text, corporaScope :: Text
 , corporaStatus :: CorporaStatus, corporaRevision :: Int, corporaOperations :: M.Map Text Value, corporaDoctor :: Maybe AgentId
 } deriving (Eq, Show)
instance FromJSON CorporaConfig where
 parseJSON = withObject "Corpora settings" $ \o -> CorporaConfig <$> o .:? "name" <*> o .: "device" <*> o .: "scope" <*> o .: "status" <*> o .: "revision" <*> o .:? "operations" .!= M.empty <*> o .:? "doctor_id"
instance ToJSON CorporaConfig where
 toJSON config = object ["name" .= corporaName config, "device" .= corporaDevice config, "scope" .= corporaScope config, "status" .= corporaStatus config, "revision" .= corporaRevision config, "operations" .= corporaOperations config, "doctor_id" .= corporaDoctor config]
data Frequency = Bpm120 | Bpm60 | Bph60 deriving (Eq, Ord, Show, Enum, Bounded)
instance Wire Frequency where wireName frequency = case frequency of Bpm120 -> "bpm120"; Bpm60 -> "bpm60"; Bph60 -> "bph60"
instance FromJSON Frequency where parseJSON = parseWire
instance ToJSON Frequency where toJSON = String . wireName
periodMillis :: Frequency -> Int
periodMillis frequency = case frequency of Bpm120 -> 500; Bpm60 -> 1000; Bph60 -> 60000
data Pulse = Pulse { pulseName :: Text, pulseFrequency :: Frequency, pulseNumber :: Int } deriving (Eq, Show)
pulseIdentity :: Pulse -> Text
pulseIdentity pulse = pulseName pulse <> ":" <> wireName (pulseFrequency pulse) <> ":" <> T.pack (show (pulseNumber pulse))
instance ToJSON Pulse where
 toJSON pulse = obj [("pulse",str (pulseName pulse)),("frequency",toJSON (pulseFrequency pulse)),("num",num (pulseNumber pulse)),("pulseId",str (pulseIdentity pulse))]
data TaskState = Backlog | InProgress | TaskDone deriving (Eq, Ord, Show, Enum, Bounded)
instance Wire TaskState where wireName state = case state of Backlog -> "backlog"; InProgress -> "in_progress"; TaskDone -> "done"
instance FromJSON TaskState where parseJSON = parseWire
instance ToJSON TaskState where toJSON = String . wireName
data GroupAccess = ChiefOnly | Management | Member deriving (Eq, Show)
data BrowserAction = Navigate | Reload | Back | Forward deriving (Eq, Ord, Show, Enum, Bounded)
instance Wire BrowserAction where
 wireName action = case action of Navigate -> "navigate"; Reload -> "reload"; Back -> "back"; Forward -> "forward"
instance FromJSON BrowserAction where parseJSON = parseWire
instance ToJSON BrowserAction where toJSON = String . wireName
data BrowserCommand = BrowserCommand { browserSequence :: Text, browserAction :: BrowserAction } deriving (Eq, Show)
instance FromJSON BrowserCommand where
 parseJSON = withObject "Browser command" $ \o -> BrowserCommand <$> o .: "seq" <*> o .: "action"
instance ToJSON BrowserCommand where
 toJSON command = object ["seq" .= browserSequence command, "action" .= browserAction command]
data TurnAction = Retry | Cancel deriving (Eq, Ord, Show, Enum, Bounded)
instance Wire TurnAction where wireName action = case action of Retry -> "retry"; Cancel -> "cancel"
instance FromJSON TurnAction where parseJSON = parseWire
instance ToJSON TurnAction where toJSON = String . wireName

activeStatus, retryableStatus, completedStatus :: TurnStatus -> Bool
activeStatus status = status `elem` [Queued, Running, OutputPending]
retryableStatus status = status `elem` [Failed, Interrupted]
completedStatus status = status `elem` [Done, Warning]

-- Extra fields are retained for storage compatibility, but lifecycle fields have
-- one authoritative typed representation (they are removed from the extras).
data Turn = Turn { turnId :: RunId, turnFlow :: Flow, turnInput :: Text, turnStatus :: TurnStatus, turnAttempt :: Maybe Int, turnBatchId :: Maybe RunId, turnFields :: Object } deriving (Eq, Show)
instance FromJSON Turn where
 parseJSON = withObject "Turn" $ \o -> do
  attempt <- o .:? "attempt"
  unless (maybe True (>=0) attempt) (fail "Turn attempt cannot be negative")
  Turn <$> o .: "id" <*> o .: "flow" <*> o .: "input" <*> o .: "status" <*> pure attempt <*> o .:? "batch_id" <*> pure (foldr KM.delete o ["id","flow","input","status","attempt","batch_id"])
instance ToJSON Turn where
 toJSON t = Object $ KM.fromList ([("id",toJSON (turnId t)),("flow",toJSON (turnFlow t)),("input",toJSON (turnInput t)),("status",toJSON (turnStatus t))] ++ [("attempt",num n) | n <- maybeToList (turnAttempt t)] ++ [("batch_id",toJSON i) | i <- maybeToList (turnBatchId t)]) <> turnFields t
turnField :: Turn -> Text -> Value
turnField t key = get (Object (turnFields t)) key
withTurnFields :: (Value -> Value) -> Turn -> Turn
withTurnFields f t = t { turnFields = case f (Object (turnFields t)) of Object o -> foldr KM.delete o ["id","flow","input","status","attempt","batch_id"]; _ -> turnFields t }
batchLeader :: Turn -> RunId
batchLeader t = fromMaybe (turnId t) (turnBatchId t)
retryTurn :: Turn -> Turn
retryTurn t = (withTurnFields (remove ["error","batch_members","agency_kind","slot","pulseId"]) t) { turnStatus = Queued, turnAttempt=Just (fromMaybe 0 (turnAttempt t)+1), turnBatchId=Nothing }
newTurn :: RunId -> Flow -> Text -> Text -> Value -> Bool -> Turn
newTurn identity flow input created origin batchable = Turn identity flow input Queued (Just 1) Nothing (KM.fromList [("created",str created),("origin",origin),("batchable",Bool batchable)])

data AgentState = AgentState { agentTurns :: [Turn], agentFields :: Object } deriving (Eq, Show)
instance FromJSON AgentState where
 parseJSON = withObject "AgentState" $ \o -> AgentState <$> o .:? "turns" .!= [] <*> pure (KM.delete "turns" o)
instance ToJSON AgentState where toJSON state = Object (KM.insert "turns" (toJSON (agentTurns state)) (agentFields state))
stateJSON :: AgentState -> Value
stateJSON = toJSON
stateField :: AgentState -> Text -> Value
stateField state = get (Object (agentFields state))
mapTurns :: (Turn -> Turn) -> AgentState -> AgentState
mapTurns f state = state { agentTurns = map f (agentTurns state) }
mapStateFields :: (Value -> Value) -> AgentState -> AgentState
mapStateFields f state = state { agentFields = case f (Object (agentFields state)) of Object o -> KM.delete "turns" o; _ -> agentFields state }


select :: [Text] -> Value -> Value
select names value = obj [(key,get value key) | key <- names]
selectPresent :: [Text] -> Value -> Value
selectPresent names value = obj [(key,get value key) | key <- names, has value key]
mergeFields :: [(Text,Value)] -> Value -> Value
mergeFields fields value = merge value (obj fields)
appendItems :: Text -> [Value] -> Value -> Value
appendItems key values value = put key (arr (items value key ++ values)) value
replaceItem :: Text -> Value -> Value -> Value
replaceItem key value = modifyItems key (map (\item -> if get item "id" == get value "id" then value else item))
modifyItems :: Text -> ([Value] -> [Value]) -> Value -> Value
modifyItems key f value = put key (arr (f (items value key))) value

obj :: [(Text, Value)] -> Value
obj = Object . KM.fromList . map (\(k,v) -> (K.fromText k,v))
arr :: [Value] -> Value
arr = Array . V.fromList
str :: Text -> Value
str = String
num :: Int -> Value
num = toJSON
get :: Value -> Text -> Value
get (Object o) k = fromMaybe Null (KM.lookup (K.fromText k) o)
get _ _ = Null
text :: Value -> Text
text (String s) = s
text _ = ""
int :: Value -> Int
int (Number n) = fromMaybe 0 (toBoundedInteger n)
int _ = 0
boolean :: Value -> Bool
boolean (Bool b) = b
boolean _ = False
list :: Value -> [Value]
list (Array a) = V.toList a
list _ = []
pairs :: Value -> [(Text,Value)]
pairs (Object o) = map (\(k,v) -> (K.toText k,v)) (KM.toList o)
pairs _ = []
keys :: Value -> [Text]
keys = map fst . pairs
has :: Value -> Text -> Bool
has (Object o) k = KM.member (K.fromText k) o
has _ _ = False
put :: Text -> Value -> Value -> Value
put k v (Object o) = Object (KM.insert (K.fromText k) v o)
put k v _ = obj [(k,v)]
merge :: Value -> Value -> Value
merge a b = foldl (\x (k,v) -> put k v x) a (pairs b)
remove :: [Text] -> Value -> Value
remove ks = obj . filter ((`notElem` ks) . fst) . pairs
at :: Value -> Text -> Text
at v = text . get v
items :: Value -> Text -> [Value]
items v = list . get v
fallback :: Value -> Value -> Value
fallback Null b = b
fallback a _ = a
empty :: Value
empty = obj []
jsonText :: Value -> Text
jsonText = TE.decodeUtf8 . BL.toStrict . encode
parseJSONText :: Text -> Value
parseJSONText = fromMaybe Null . decode . BL.fromStrict . TE.encodeUtf8

textFieldP :: Text -> Int -> Value -> Parser Text
textFieldP k n v = case get v k of
 String s | not (T.null (T.strip s)) && T.length s <= n -> pure (T.strip s)
 _ -> fail (T.unpack (k <> " must be nonempty text, at most " <> T.pack (show n) <> " characters"))
stringsP :: Text -> Bool -> Value -> Parser [Text]
stringsP name nonempty value = do
 values <- parseJSON value
 unless (length values <= 50 && (not nonempty || not (null values)) && all (\t -> not (T.null (T.strip t)) && T.length t <= 16000) values) $ fail (T.unpack name ++ " must be a list of nonempty strings")
 pure values
nameP :: Value -> Parser Text
nameP value = do
 name <- textFieldP "name" 24 value
 unless (name == at value "name" && maybe False (\(c,_) -> c >= 'A' && c <= 'Z') (T.uncons name) && T.all (\c -> isAscii c && (isAlphaNum c || c `elem` ("_.:#+|()&$^-" :: String))) name) $ fail "Name must start with A–Z; use letters, numbers, or - _ . : # + | ( ) & $ ^ (no spaces)"
 pure name
onlyFields :: [Text] -> Value -> Parser ()
onlyFields allowed = withObject "record" $ \fields -> unless (all ((`elem` allowed) . K.toText) (KM.keys fields)) (fail "Unknown field in response")

-- Provider receipts describe delivery, not independently verified external effects.
data EvidenceUsage = EvidenceUsage
 { usageInput :: Maybe Integer, usageCached :: Maybe Integer, usageOutput :: Maybe Integer
 , usageDuplicates :: Int, usageConflict :: Bool, usageInvalid :: Int
 } deriving (Eq, Show)
instance ToJSON EvidenceUsage where
 toJSON u = obj [("inputTokens",toJSON (usageInput u)),("cachedInputTokens",toJSON (usageCached u)),("outputTokens",toJSON (usageOutput u)),("duplicates",toJSON (usageDuplicates u)),("conflict",Bool (usageConflict u)),("invalidReports",num (usageInvalid u)),("source",str "provider_report"),("price",Null)]

data EvidenceReceipt = EvidenceReceipt
 { receiptSequence :: Int, receiptTime :: Text, receiptKind :: Text
 , receiptItem :: Maybe Int, receiptStatus :: Maybe Text, receiptExit :: Maybe Int, receiptSummary :: Maybe ToolSummary
 } deriving (Eq, Show)
instance ToJSON EvidenceReceipt where
 toJSON r = obj [("sequence",toJSON (receiptSequence r)),("receivedAt",str (receiptTime r)),("kind",str (receiptKind r)),("item",toJSON (receiptItem r)),("status",toJSON (receiptStatus r)),("exitCode",toJSON (receiptExit r)),("source",str "provider_receipt"),("summary",toJSON (receiptSummary r))]

data EvidencePolicy = OrdinaryEvidence | ResponseOnlyEvidence Text deriving (Eq, Show)
data EvidenceSnapshot = EvidenceSnapshot
 { evidencePolicy :: EvidencePolicy, evidenceModel :: Text, evidenceStarted :: Text, evidenceDuration :: Maybe Double
 , evidenceExit :: Maybe Int, evidenceFinished :: Bool, evidenceReceipts :: [EvidenceReceipt]
 , evidenceUsage :: EvidenceUsage, evidenceFinal :: Bool, evidenceTurnEnd :: Bool, evidenceStreamEnd :: Bool
 , evidencePending :: Maybe Int, evidenceMalformed :: Int, evidenceUnknown :: Int
 , evidenceDropped :: Int, evidenceDroppedBytes :: Integer, evidenceOutOfOrder :: Int
 , evidenceOmittedFields :: Int, evidenceRedactedFields :: Int, evidenceTruncatedFields :: Int
 } deriving (Eq, Show)
instance ToJSON EvidenceSnapshot where
 toJSON e = obj
  [("schemaVersion",num 1),("provider",str "codex"),("model",str (evidenceModel e)),("policy",str (case evidencePolicy e of OrdinaryEvidence -> "unsafe_bypass"; ResponseOnlyEvidence _ -> "response_only")),("providerVersion",case evidencePolicy e of OrdinaryEvidence -> Null; ResponseOnlyEvidence version -> str version),("configuredExecutable",Bool True)
  ,("startedAt",str (evidenceStarted e)),("durationMs",toJSON (evidenceDuration e)),("processExit",toJSON (evidenceExit e)),("finished",Bool (evidenceFinished e))
  ,("events",toJSON (evidenceReceipts e)),("usage",toJSON (evidenceUsage e))
  ,("completeness",obj [("finalSeen",Bool (evidenceFinal e)),("turnEndSeen",Bool (evidenceTurnEnd e)),("streamEndSeen",Bool (evidenceStreamEnd e)),("pendingTools",toJSON (evidencePending e)),("malformedEvents",num (evidenceMalformed e)),("unknownEvents",num (evidenceUnknown e)),("droppedEvents",num (evidenceDropped e)),("droppedBytes",toJSON (evidenceDroppedBytes e)),("outOfOrderEvents",num (evidenceOutOfOrder e)),("contentOmitted",Bool True),("externalEffectsVerified",Bool False),("tailUnknown",Bool (not (evidenceFinished e) || not (evidenceStreamEnd e))),("observedEvents",num (length (evidenceReceipts e)+evidenceDropped e+evidenceMalformed e)),("omittedFields",num (evidenceOmittedFields e)),("redactedFields",num (evidenceRedactedFields e)),("truncatedFields",num (evidenceTruncatedFields e))])]

data AssessmentStatus = AssessmentAccepted | AssessmentRejected | AssessmentUnresolved deriving (Eq, Show)
instance ToJSON AssessmentStatus where
 toJSON status = str $ case status of AssessmentAccepted -> "Accepted"; AssessmentRejected -> "Rejected"; AssessmentUnresolved -> "Unresolved"
assessmentValue :: AssessmentStatus -> Text -> Text -> Value
assessmentValue status source reason = obj [("status",toJSON status),("source",str source),("reason",str reason),("independent",Bool False)]

-- Only the provider adapter constructs these allowlisted field names and values.
data SummaryField = SummaryField
 { summaryName :: Text, summaryValue :: Value, summaryRedacted :: Bool
 , summaryTruncated :: Bool, summaryOmitted :: Bool
 } deriving (Eq, Show)
instance ToJSON SummaryField where
 toJSON f = obj [("name",str (summaryName f)),("value",summaryValue f),("redacted",Bool (summaryRedacted f)),("truncated",Bool (summaryTruncated f)),("omitted",Bool (summaryOmitted f))]
data ToolSummary = ToolSummary
 { summaryFields :: [SummaryField], summaryOmittedFields :: Int
 , summaryRedactedFields :: Int, summaryTruncatedFields :: Int
 } deriving (Eq, Show)
instance ToJSON ToolSummary where
 toJSON s = obj [("fields",toJSON (summaryFields s)),("omittedFields",num (summaryOmittedFields s)),("redactedFields",num (summaryRedactedFields s)),("truncatedFields",num (summaryTruncatedFields s))]

-- Admission proof for the two repair activities with no ambient tool capability.
data RepairInvocationPolicy = DoctorScopedResponseOnly | RepairPreparationResponseOnly | SavedAuditResponseOnly | KitInspectionResponseOnly deriving (Eq,Show)
instance ToJSON RepairInvocationPolicy where
 toJSON policy=obj [("schema",num 1),("kind",str (case policy of DoctorScopedResponseOnly->"doctor_scoped_response_only"; RepairPreparationResponseOnly->"isolated_preparation_response_only"; SavedAuditResponseOnly->"saved_audit_response_only"; KitInspectionResponseOnly->"kit_inspection_response_only"))]
instance FromJSON RepairInvocationPolicy where
 parseJSON value=do
  onlyFields ["schema","kind"] value
  unless (get value "schema"==num 1) (fail "Unknown repair invocation admission schema")
  case at value "kind" of
   "doctor_scoped_response_only"->pure DoctorScopedResponseOnly
   "isolated_preparation_response_only"->pure RepairPreparationResponseOnly
   "saved_audit_response_only"->pure SavedAuditResponseOnly
   "kit_inspection_response_only"->pure KitInspectionResponseOnly
   _->fail "Unknown repair invocation admission policy"
