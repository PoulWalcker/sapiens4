{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Werror=incomplete-patterns #-}
module Delegation
 ( WorkCall(..), Workload, workloadId, workCalls, workOrigin, CallState(..), CallProgress(..)
 , callState, callNode, callOutput, begin, getWork, findWork, readWorks, saveWork, withCall, workItems
 , exceptionText, reconcileWorks, pendingWorks, runDecision, applyDecision, clarify
 , TaskSpec, templates, editTemplate, workTasks, taskList, workSummaries
 ) where

import Control.Concurrent
import qualified Data.Map.Strict as M
import Control.Exception
import Control.Monad
import Crypto.Hash.SHA256 (hash)
import Data.Aeson hiding (pairs)
import Data.Aeson.Types (Parser, parseEither)
import qualified Data.Bifunctor as B
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Aeson.Key
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import qualified Data.ByteString as BS
import Data.IORef
import Data.List (find,nub,sort,(\\))
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Numeric (showHex)
import System.Directory
import System.FilePath
import Types
import qualified Store as DB
import Population
import Memo
import Provider

data Node = Assessing | ChiefTriage | CreatingSapi | ReadyToWork | Delegation | Execution | OutcomeNode | WaitingForAdmin deriving (Eq, Ord, Show, Enum, Bounded)
instance Wire Node where
 wireName node = case node of Assessing -> "Assessing"; ChiefTriage -> "ChiefTriage"; CreatingSapi -> "CreatingSapi"; ReadyToWork -> "ReadyToWork"; Delegation -> "Delegation"; Execution -> "Execution"; OutcomeNode -> "Outcome"; WaitingForAdmin -> "WaitingForAdmin"
instance FromJSON Node where parseJSON = parseWire
instance ToJSON Node where toJSON = str . wireName

data CallState = CallQueued | CallRunning | CallCompleted | CallUnresolved | CallDelegated | CallWaiting | CallFailed | CallInterrupted | CallCancelled deriving (Eq, Ord, Show, Enum, Bounded)
instance Wire CallState where
 wireName state = case state of CallQueued -> "Queued"; CallRunning -> "Running"; CallCompleted -> "Completed"; CallUnresolved -> "Unresolved"; CallDelegated -> "Delegated"; CallWaiting -> "WaitingForAdmin"; CallFailed -> "Failed"; CallInterrupted -> "Interrupted"; CallCancelled -> "Cancelled"
instance FromJSON CallState where parseJSON = parseWire
instance ToJSON CallState where toJSON = str . wireName

-- A finished call owns its required reply and determines its terminal node.
-- Active/stopped calls retain the node needed for retry and have no output.
data CallResult = Answered Text | UnresolvedResult Text | HandedOff Text | NeedsClarification Text deriving (Eq, Show)
data CallProgress = QueuedAt Node | RunningAt Node | FailedAt Node | InterruptedAt Node | CancelledAt Node | FinishedCall CallResult deriving (Eq, Show)
data WorkCall = WorkCall
 { callIdentity :: RunId, callRecipient :: AgentId, callProgress :: CallProgress
 , callRequest :: Text, callParent :: Maybe RunId, callFields :: Object
 } deriving (Eq, Show)
callState :: WorkCall -> CallState
callState call = case callProgress call of
 QueuedAt _ -> CallQueued; RunningAt _ -> CallRunning; FailedAt _ -> CallFailed; InterruptedAt _ -> CallInterrupted; CancelledAt _ -> CallCancelled
 FinishedCall result -> case result of
  Answered _ -> CallCompleted; UnresolvedResult _ -> CallUnresolved; HandedOff _ -> CallDelegated; NeedsClarification _ -> CallWaiting
callNode :: WorkCall -> Node
callNode call = case callProgress call of
 QueuedAt node -> node; RunningAt node -> node; FailedAt node -> node; InterruptedAt node -> node; CancelledAt node -> node
 FinishedCall result -> case result of
  Answered _ -> OutcomeNode; UnresolvedResult _ -> OutcomeNode; HandedOff _ -> Delegation; NeedsClarification _ -> WaitingForAdmin
callOutput :: WorkCall -> Maybe Text
callOutput call = case callProgress call of
 QueuedAt _ -> Nothing; RunningAt _ -> Nothing; FailedAt _ -> Nothing; InterruptedAt _ -> Nothing; CancelledAt _ -> Nothing
 FinishedCall result -> Just $ case result of
  Answered output -> output; UnresolvedResult output -> output; HandedOff output -> output; NeedsClarification output -> output
instance FromJSON WorkCall where
 parseJSON = withObject "Workload call" $ \o -> do
  node <- o .: "node"
  state <- o .: "state"
  output <- o .:? "output"
  let unfinished constructor = do
       unless (isNothing output) (fail "Unfinished workload call cannot have output")
       pure (constructor node)
      finished constructor expected = do
       unless (node == expected) (fail "Workload call has an inconsistent terminal node")
       FinishedCall . constructor <$> maybe (fail "Finished workload call requires output") pure output
  progress <- case state of
   CallQueued -> unfinished QueuedAt; CallRunning -> unfinished RunningAt
   CallFailed -> unfinished FailedAt; CallInterrupted -> unfinished InterruptedAt; CallCancelled -> unfinished CancelledAt
   CallCompleted -> finished Answered OutcomeNode; CallUnresolved -> finished UnresolvedResult OutcomeNode
   CallDelegated -> finished HandedOff Delegation; CallWaiting -> finished NeedsClarification WaitingForAdmin
  WorkCall <$> o .: "callId" <*> o .: "addressedTo" <*> pure progress <*> o .: "request" <*> o .:? "causedBy" <*> pure (foldr KM.delete o ["callId","addressedTo","node","state","request","causedBy","output"])
instance ToJSON WorkCall where
 toJSON call = mergeFields ([("callId",toJSON (callIdentity call)),("addressedTo",toJSON (callRecipient call)),("node",toJSON (callNode call)),("state",toJSON (callState call)),("request",str (callRequest call)),("causedBy",toJSON (callParent call))] ++ [("output",str output) | output <- maybeToList (callOutput call)]) (Object (callFields call))
callField :: WorkCall -> Text -> Value
callField call = get (Object (callFields call))
setCallFields :: [(Text,Value)] -> WorkCall -> WorkCall
setCallFields fields call = call { callFields = KM.fromList [(Data.Aeson.Key.fromText key,value) | (key,value) <- fields] <> callFields call }
newCall :: RunId -> AgentId -> Maybe RunId -> CallProgress -> Text -> WorkCall
newCall identity owner parent progress request = WorkCall identity owner progress request parent (KM.singleton "attempt" (num 1))

data TaskContract
 = Unprepared | Accepted TaskSpec | UnsupportedLegacy Value | Corrupted Value Text
 deriving (Eq, Show)

-- Preserve wire shape and spelling without making them the accepted contract.
data TaskEncoding = TaskEncoding
 { taskEnvelope :: Maybe Value, taskSpecificationEncoding :: Maybe Value
 } deriving (Eq, Show)

decodeTask :: Maybe Value -> (TaskContract,TaskEncoding)
decodeTask rawTask = case rawTask of
 Just (Object fields) ->
  let envelope=Just (Object (KM.delete "specification" fields))
      encoding=TaskEncoding envelope
  in case KM.lookup "specification" fields of
   Nothing -> (Unprepared,encoding Nothing)
   Just Null -> (Unprepared,encoding (Just Null))
   Just value -> case parseEither parseJSON value of
    Right spec -> (Accepted spec,encoding (Just value))
    Left reason
     | hasStringCriteria value -> (UnsupportedLegacy value,encoding Nothing)
     | otherwise -> (Corrupted value (T.pack reason),encoding Nothing)
 Nothing -> (Unprepared,TaskEncoding Nothing Nothing)
 Just Null -> (Unprepared,TaskEncoding (Just Null) Nothing)
 Just value -> (Corrupted value "Task must be an object or null",TaskEncoding (Just value) Nothing)
 where
 hasStringCriteria value = case get value "completionCriteria" of
  Array criteria -> not (null criteria) && all isString criteria
  _ -> False
 isString (String _) = True
 isString _ = False

encodeTask :: TaskContract -> TaskEncoding -> Maybe Value
encodeTask contract encoding = case taskEnvelope encoding of
 Just envelope@(Object _) -> Just $ case specification of
  Nothing -> envelope
  Just value -> put "specification" value envelope
 envelope -> envelope
 where
 specification = case contract of
  Unprepared -> taskSpecificationEncoding encoding
  Accepted spec -> Just $ case taskSpecificationEncoding encoding of
   Just value | parseEither parseJSON value == Right spec -> value
   _ -> toJSON spec
  UnsupportedLegacy value -> Just value
  Corrupted value _ -> Just value

data Workload = Workload
 { workloadId :: Text, workTracksTask :: Bool, workCalls :: NonEmpty WorkCall
 , workTaskContract :: TaskContract, workTaskEncoding :: TaskEncoding, workOrigin :: StoredOrigin, workFields :: Object
 } deriving (Eq, Show)
instance FromJSON Workload where
 parseJSON = withObject "Workload" $ \o -> do
  let (contract,encoding)=decodeTask (KM.lookup "task" o)
  Workload <$> o .: "workloadId" <*> o .: "tracked" <*> o .: "calls" <*> pure contract <*> pure encoding <*> pure (decodeStoredOrigin (KM.lookup "origin" o))
   <*> pure (foldr KM.delete o ["workloadId","tracked","calls","task","origin"])
instance ToJSON Workload where
 toJSON work = mergeFields
  ([("workloadId",str (workloadId work)),("tracked",Bool (workTracksTask work)),("calls",toJSON (workCalls work))]
   ++ [("task",value) | value <- maybeToList (encodeTask (workTaskContract work) (workTaskEncoding work))]
   ++ [("origin",toJSON (workOrigin work)) | workOrigin work /= MissingOrigin])
  (Object (workFields work))
taskValue :: Workload -> Value
taskValue work = fromMaybe Null (encodeTask (workTaskContract work) (workTaskEncoding work))
acceptTaskSpec :: Text -> TaskSpec -> Workload -> Workload
acceptTaskSpec taskType spec work = work
 { workTaskContract=Accepted spec
 , workTaskEncoding=TaskEncoding
    (Just (put "taskType" (str taskType) (fromMaybe Null (taskEnvelope (workTaskEncoding work))))) Nothing }
workField :: Workload -> Text -> Value
workField work "origin" = toJSON (workOrigin work)
workField work key = get (Object (workFields work)) key
workItems :: Workload -> Text -> [Value]
workItems work = list . workField work
setWorkField :: Text -> Value -> Workload -> Workload
setWorkField key value work = work { workFields = KM.insert (Data.Aeson.Key.fromText key) value (workFields work) }
readWorks :: Service -> IO [Workload]
readWorks s = DB.values s DB.Workloads >>= traverse (decodeValue 500)

newtype CriterionId = CriterionId { criterionIdText :: Text } deriving (Eq, Ord, Show)
instance FromJSON CriterionId where
 parseJSON value = do
  identity <- parseJSON value
  unless (not (T.null identity) && T.length identity <= 64 && T.all (\c -> c `elem` ("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_-" :: String)) identity) (fail "Criterion ID must contain 1–64 letters, digits, _ or -")
  pure (CriterionId identity)
instance ToJSON CriterionId where toJSON = str . criterionIdText
newtype Requirement = Requirement { requirementText :: Text } deriving (Eq, Show)
newtype Condition = Condition { conditionText :: Text } deriving (Eq, Show)
data CompletionCriterion = Required CriterionId Requirement | Conditional CriterionId Condition Requirement deriving (Eq, Show)
criterionId :: CompletionCriterion -> CriterionId
criterionId criterion = case criterion of Required identity _ -> identity; Conditional identity _ _ -> identity
instance FromJSON CompletionCriterion where
 parseJSON value = do
  onlyFields ["id","requirement","applicability"] value
  identity <- parseJSON (get value "id")
  requirement <- Requirement <$> textFieldP "requirement" 16000 value
  let applicability=get value "applicability"
  case at applicability "kind" of
   "Required" -> onlyFields ["kind"] applicability >> pure (Required identity requirement)
   "Conditional" -> do
    onlyFields ["kind","condition"] applicability
    condition <- Condition <$> textFieldP "condition" 16000 applicability
    pure (Conditional identity condition requirement)
   _ -> fail "Applicability must be Required or Conditional"
instance ToJSON CompletionCriterion where
 toJSON criterion = case criterion of
  Required identity requirement -> fields identity requirement (obj [("kind",str "Required")])
  Conditional identity condition requirement -> fields identity requirement (obj [("kind",str "Conditional"),("condition",str (conditionText condition))])
  where fields identity requirement applicability = obj [("id",toJSON identity),("requirement",str (requirementText requirement)),("applicability",applicability)]
data TaskSpec = TaskSpec { specObjective :: Text, specInputs :: [Text], specOutputs :: NonEmpty Text, specCriteria :: NonEmpty CompletionCriterion } deriving (Eq, Show)
instance FromJSON TaskSpec where
 parseJSON value = do
  onlyFields ["objective","inputs","expectedOutputs","completionCriteria"] value
  objective <- textFieldP "objective" 16000 value
  inputs <- stringsP "inputs" False (get value "inputs")
  outputs <- stringsP "expectedOutputs" True (get value "expectedOutputs") >>= maybe (fail "Expected at least one output") pure . NE.nonEmpty
  criteria <- parseJSON (get value "completionCriteria") >>= maybe (fail "Expected at least one criterion") pure . NE.nonEmpty
  let ids=map criterionId (NE.toList criteria)
  unless (length ids <= 50 && length ids == length (nub ids)) (fail "Completion criteria require unique IDs and at most 50 entries")
  pure (TaskSpec objective inputs outputs criteria)
instance ToJSON TaskSpec where
 toJSON spec = obj [("objective",str (specObjective spec)),("inputs",toJSON (specInputs spec)),("expectedOutputs",toJSON (NE.toList (specOutputs spec))),("completionCriteria",toJSON (NE.toList (specCriteria spec)))]
data OutcomeStatus = Completed | Unresolved deriving (Eq, Ord, Show, Enum, Bounded)
instance Wire OutcomeStatus where wireName status = case status of Completed -> "Completed"; Unresolved -> "Unresolved"
instance FromJSON OutcomeStatus where parseJSON = parseWire
instance ToJSON OutcomeStatus where toJSON = str . wireName
data CriterionResult = Satisfied CriterionId Text | NotApplicable CriterionId Text deriving (Eq, Show)
resultId :: CriterionResult -> CriterionId
resultId result = case result of Satisfied identity _ -> identity; NotApplicable identity _ -> identity
instance FromJSON CriterionResult where
 parseJSON value = do
  onlyFields ["id","result","evidence"] value
  identity <- parseJSON (get value "id")
  evidence <- textFieldP "evidence" 4000 value
  case at value "result" of
   "Satisfied" -> pure (Satisfied identity evidence)
   "NotApplicable" -> pure (NotApplicable identity evidence)
   _ -> fail "Criterion result must be Satisfied or NotApplicable"
instance ToJSON CriterionResult where
 toJSON result = case result of
  Satisfied identity evidence -> fields identity "Satisfied" evidence
  NotApplicable identity evidence -> fields identity "NotApplicable" evidence
  where fields identity kind evidence = obj [("id",toJSON identity),("result",str kind),("evidence",str evidence)]
data Outcome = Outcome { outcomeStatus :: OutcomeStatus, outcomeResults :: [CriterionResult], outcomeArtifacts :: [Text] } deriving (Eq, Show)
instance FromJSON Outcome where
 parseJSON value = do
  onlyFields ["status","criterionResults","artifacts"] value
  Outcome <$> parseJSON (get value "status") <*> parseJSON (get value "criterionResults") <*> stringsP "artifacts" False (get value "artifacts")
instance ToJSON Outcome where
 toJSON outcome = obj [("status",toJSON (outcomeStatus outcome)),("criterionResults",toJSON (outcomeResults outcome)),("artifacts",toJSON (outcomeArtifacts outcome))]

data Decision
 = ReplyNow Text | ExecuteTask | OutsideSpecialization | SelectSpecialist Text
 | NeedSpecialist | AskAdmin Text | SpecifySapi Text Text | PrepareTask Text TaskSpec
 | Handoff Text | ReportOutcome Text Outcome
 deriving (Eq, Show)
data DecisionResponse = DecisionResponse { decisionBody :: Decision, decisionReason :: Text, decisionEvidence :: [Text], decisionRaw :: Value } deriving (Eq, Show)
parseDecision :: Node -> Value -> Parser DecisionResponse
parseDecision node value = do
 let event=at value "event"
     variant fields parser = onlyFields (fields ++ ["event","reason","evidence"]) value >> parser
     textAt key limit = textFieldP key limit value
 unless (event `elem` eventsFor node) (fail ("Unknown decision event for " ++ T.unpack (wireName node)))
 reason <- textAt "reason" 4000 >> parseJSON (get value "reason")
 evidence <- stringsP "evidence" False (get value "evidence")
 body <- case event of
  "FitsSpecialization" -> variant ["mode","reply"] $ case at value "mode" of
   "Repl" -> ReplyNow <$> textAt "reply" 64000
   "Exec" -> pure ExecuteTask
   _ -> fail "FitsSpecialization requires mode Repl or Exec"
  "OutsideSpecialization" -> variant [] (pure OutsideSpecialization)
  "SpecialistSelected" -> variant ["target"] (SelectSpecialist <$> parseJSON (get value "target"))
  "NewSpecialistNeeded" -> variant [] (pure NeedSpecialist)
  "RequestUnclear" -> variant ["question"] (AskAdmin <$> textAt "question" 4000)
  "SapiSpecified" -> variant ["name","role"] (SpecifySapi <$> nameP value <*> textAt "role" 60)
  "TaskPrepared" -> variant ["taskType","specification"] (PrepareTask <$> textAt "taskType" 60 <*> parseJSON (get value "specification"))
  "HandoffPrepared" -> variant ["request"] (Handoff <$> textAt "request" 16000)
  "Outcome" -> variant ["reply","outcome"] (ReportOutcome <$> textAt "reply" 64000 <*> parseJSON (get value "outcome"))
  _ -> fail "Unknown decision event"
 pure (DecisionResponse body reason evidence value)

promptNames :: [(Node,Text)]
promptNames = [(Assessing,"decision-triage"),(ChiefTriage,"decision-chief-triage"),(CreatingSapi,"decision-create-sapi"),(ReadyToWork,"decision-prepare-task"),(Delegation,"decision-delegation"),(Execution,"decision-execution")]
eventsFor :: Node -> [Text]
eventsFor node = case node of
 Assessing -> ["FitsSpecialization","OutsideSpecialization"]
 ChiefTriage -> ["FitsSpecialization","SpecialistSelected","NewSpecialistNeeded","RequestUnclear"]
 CreatingSapi -> ["SapiSpecified"]
 ReadyToWork -> ["TaskPrepared"]
 Delegation -> ["HandoffPrepared"]
 Execution -> ["Outcome","OutsideSpecialization"]
 OutcomeNode -> []
 WaitingForAdmin -> []
templates :: Service -> IO [Value]
templates s = forM promptNames $ \(node,name) -> do
 let local = serviceRoot s </> "decision-prompts" </> T.unpack name <.> "md"
 exists <- doesFileExist local
 let p = if exists then local else repositoryRoot s </> "corpora/prompts" </> T.unpack name <.> "md"
 content <- readText p
 pure $ obj [("node",toJSON node),("template",str (name<>".md")),("path",str (T.pack p)),("content",str content)]
editTemplate :: Service -> Text -> Value -> IO Value
editTemplate s rawNode d = do
 node <- decodeValue 400 (str rawNode)
 name <- maybe (failure 400 "Unknown decision node or field") pure (lookup node promptNames)
 require (keys d == ["content"]) 400 "Unknown decision node or field"
 content <- textField d "content" 20000
 let p = serviceRoot s </> "decision-prompts" </> T.unpack name <.> "md"
 writeText p content
 pure (obj [("saved",Bool True),("path",str (T.pack p))])
findWork :: Service -> RunId -> IO (Maybe (Workload,WorkCall))
findWork s i = do
 works <- readWorks s
 pure $ listToMaybe [(w,c) | w <- works,c <- NE.toList (workCalls w),callIdentity c == i]
getWork :: Service -> RunId -> IO (Workload,WorkCall)
getWork s i = findWork s i >>= maybe (failure 409 "Unknown workload call") pure
saveWork :: Service -> Workload -> IO ()
saveWork s w = do
 previous <- find ((==workloadId w) . workloadId) <$> readWorks s
 let call=NE.last (workCalls w)
     before=NE.last . workCalls <$> previous
     changed=maybe False (\old -> workTracksTask w && (not (workTracksTask old) || fmap (\c -> (callState c,callRecipient c)) before /= Just (callState call,callRecipient call))) previous
 when changed $ do
  (source,agency) <- currentActor s
  let state=callState call
      actor=if isJust agency || state==CallCancelled then source else if state==CallQueued then "system" else agentIdText (callRecipient call)
      detail=obj [("owner",toJSON (callRecipient call)),("task",taskValue w),("previous",maybe Null (toJSON . callState) before),("call",toJSON (callIdentity call)),("error",callField call "error")]
  DB.record s (map (agentIdText . callRecipient) (NE.toList (workCalls w))) (at (taskValue w) "taskId") ("task."<>T.toLower (wireName state)) detail actor (if state `elem` [CallQueued,CallCancelled] then Nothing else Just "chatInput")
 DB.putValue s DB.Workloads (workloadId w) (toJSON w)
withCall :: Workload -> WorkCall -> Workload
withCall w c = w { workCalls = fmap (\x -> if callIdentity x == callIdentity c then c else x) (workCalls w) }
data OutcomeError
 = UnknownCriterion (NonEmpty CriterionId)
 | DuplicateCriterionResult (NonEmpty CriterionId)
 | InvalidNotApplicableResult (NonEmpty CriterionId)
 | MissingCompletionCoverage (NonEmpty CriterionId)
 | MissingDecisionEvidence
 deriving (Eq, Show)
outcomeError :: OutcomeError -> Text
outcomeError problem = case problem of
 UnknownCriterion _ -> "Outcome references a criterion outside the accepted Task"
 DuplicateCriterionResult _ -> "Outcome reports the same criterion more than once"
 InvalidNotApplicableResult _ -> "Only an unsatisfied, explicitly conditional criterion can be not applicable"
 MissingCompletionCoverage _ -> completionEvidenceMessage
 MissingDecisionEvidence -> completionEvidenceMessage
 where completionEvidenceMessage = "Completed outcome requires evidence for all applicable completion criteria"
-- Structural coverage and evidence presence only; the host cannot prove an LLM's claims.
newtype StructurallyValidOutcome = StructurallyValidOutcome Outcome deriving (Eq, Show)
validateOutcome :: DecisionResponse -> Outcome -> TaskSpec -> Either OutcomeError StructurallyValidOutcome
validateOutcome response outcome spec
 | Just identities <- NE.nonEmpty (filter (`notElem` acceptedIds) reportedIds) = Left (UnknownCriterion identities)
 | Just identities <- NE.nonEmpty (nub (reportedIds \\ nub reportedIds)) = Left (DuplicateCriterionResult identities)
 | Just identities <- NE.nonEmpty (map resultId (filter invalidNotApplicable (outcomeResults outcome))) = Left (InvalidNotApplicableResult identities)
 | outcomeStatus outcome == Completed, null (decisionEvidence response) = Left MissingDecisionEvidence
 | outcomeStatus outcome == Completed, Just identities <- NE.nonEmpty (filter (`notElem` reportedIds) acceptedIds) = Left (MissingCompletionCoverage identities)
 | otherwise = Right (StructurallyValidOutcome outcome)
 where
 criteria=NE.toList (specCriteria spec)
 acceptedIds=map criterionId criteria
 reportedIds=map resultId (outcomeResults outcome)
 invalidNotApplicable result = case result of
  Satisfied _ _ -> False
  NotApplicable identity _ -> case find ((==identity) . criterionId) criteria of
   Just (Conditional _ _ _) -> False
   _ -> True
begin :: Service -> AgentId -> Turn -> Maybe TaskSpec -> IO ()
begin s a turn specification = do
 found <- findWork s (turnId turn)
 origin <- requireExecutionOrigin (turnOrigin turn)
 require (isNothing specification || originIsScheduled origin) 500 "Only Routine execution can supply an admitted Task specification"
 let routine=originIsScheduled origin
     node=if routine then (if isJust specification then Execution else ReadyToWork) else Assessing
     first=newCall (turnId turn) a Nothing (RunningAt node) (turnInput turn)
     task=TaskEncoding (Just (obj [("taskId",str ("task_"<>runIdText (turnId turn))),("taskType",str (if routine then "automated" else "work"))])) (Just Null)
     initial=Workload (runIdText (turnId turn)) routine (first :| []) Unprepared task
      (ValidOrigin (newOrigin (originPurpose origin) (originGroup origin)
       (originMetadata origin <> KM.fromList [("agent",toJSON a),("call",toJSON (turnId turn))])))
      (KM.fromList [("originalIntent",str (turnInput turn)),("decisions",arr []),("outcome",Null)])
     seeded=maybe initial (\spec -> acceptTaskSpec "automated" spec initial) specification
     (w,c)=fromMaybe (seeded,first) found
     running=if isNothing (callOutput c) then c { callProgress=RunningAt (callNode c), callFields=KM.delete "error" (callFields c) } else c
     updated=setCallFields [("attempt",num (fromMaybe 1 (turnAttempt turn)))] running
 acceptedOrigin <- requireExecutionOrigin (workOrigin w)
 require (originPurpose origin == originPurpose acceptedOrigin && originGroup origin == originGroup acceptedOrigin)
  500 "Saved Workload provenance does not match its Turn"
 saveWork s (withCall w updated)
conversation :: Service -> AgentId -> Turn -> AgentState -> IO Value
conversation s a turn state = do
 manifests <- fmap obj $ fmap catMaybes $ forM ["identity","computer-use","host-control","host-facts","corpora-state"] $ \name -> do
  let p = agentRoot s (agentIdText a) </> "manifests" </> T.unpack name <.> "md"
  exists <- doesFileExist p
  if exists then Just . (name,) . str <$> readText p else pure Nothing
 memo <- (get <$> readMemo (workspaceRoot s (agentIdText a)) <*> pure "content") `catch` (\(APIError _ e) -> pure (obj [("error",str e)]))
 let reviewing t = batchLeader t /= turnId turn && turnStatus t `elem` [Running,Failed,Interrupted]
     chat = takeLast 20 [let matching = find ((==get m "turn") . toJSON . turnId) (agentTurns state) in if at m "role" == "user" && maybe False reviewing matching then put "underPrevAgencyReview" (Bool True) m else m | m <- list (stateField state "chat"), personalMessage m]
 pure $ obj [("manifests",remove ["corpora-state"] manifests),("staticContext",get manifests "corpora-state" `fallback` str ""),("instantContext",obj [("chat",arr chat),("memo",memo)]),("inputs",stateField state "agency_batch" `fallback` arr [obj [("content",str (turnInput turn))]])]
 where
 takeLast n xs = drop (length xs-n) xs
 personalMessage entry = case decodeStoredOrigin (Just (get entry "origin")) of
  InvalidOrigin _ _ -> False
  MissingOrigin -> True
  ValidOrigin origin -> isNothing (originGroup origin)
recordDecision :: Service -> Text -> Value -> IO ()
recordDecision s a record = do
 writeJSON (archivePath s a ("decisions/"<>at record "decisionId")) record
 (w,_) <- field 500 record "callId" >>= getWork s
 let update d = if get d "decisionId" == get record "decisionId" then merge d (selectPresent ["accepted","response","error","transition"] record) else d
 saveWork s (setWorkField "decisions" (arr (map update (workItems w "decisions"))) w)
accept :: Service -> Text -> Value -> Text -> Text -> IO ()
accept s a record before after = recordDecision s a (merge record (obj [("accepted",Bool True),("transition",obj [("before",str before),("event",get record "hostEvent" `fallback` get (get record "response") "event"),("after",str (transition before (at (get record "response") "event") after)),("nextNode",str after)])]))
 where transition b e def = fromMaybe def $ lookup (b,e) [(("Assessing","OutsideSpecialization"),"ChiefTriage"),(("ChiefTriage","SpecialistSelected"),"Assessing"),(("ChiefTriage","NewSpecialistNeeded"),"CreatingSapi"),(("ChiefTriage","RequestUnclear"),"WaitingForAdmin")]
decide :: Service -> Run -> Turn -> AgentState -> Node -> IORef [Value] -> IO (DecisionResponse,Value)
decide s run turn state node logs = do
 (rendered,record) <- locked s $ do
  source <- templates s >>= maybe (failure 400 "Unknown decision node") pure . find ((==toJSON node) . (`get` "node"))
  (w,c) <- getWork s (turnId turn)
  void (either decisionFailure pure (acceptedTaskSpec w))
  origin <- either decisionFailure pure (checkedWorkOrigin w)
  conv <- conversation s (runOwner run) turn state
  chief <- DB.mainAgent s
  members <- team s
  extra <- if KM.member "agency_batch" (agentFields state) then ("\n\n" <>) <$> prompt s "agency-chat" else pure ""
  scheduled <- if originIsScheduled origin then ("\n\n" <>) <$> prompt s "scheduled-execution" else pure ""
  wiki <- prompt s "notes-wiki"
  envelope <- prompt s "decision-envelope"
  let instruction = at source "content" <> extra <> scheduled
      context = obj [("originalIntent",workField w "originalIntent"),("origin",workField w "origin"),("task",taskValue w),("tracked",Bool (workTracksTask w)),("currentCall",toJSON c),("selfId",str (agentIdText (runOwner run))),("chiefId",str chief),("team",arr members),("routingHistory",arr [select ["callId","addressedTo","node"] (toJSON call) | call <- NE.toList (workCalls w)]),("conversation",conv),("observation",callField c "observation")]
      rendered = render envelope [("node",wireName node),("instruction",instruction),("notes_wiki",render wiki [("notes_example",T.pack (repositoryRoot s </> "corpora/prompts/examples/jarvis-notes.html"))]),("context",jsonText context),("events",T.intercalate ", " (sort (eventsFor node)))]
  (model,effort) <- modelDefaults
  policy <- executionSettings s (agentIdText (runOwner run))
  i <- newId
  t <- now
  let digest = T.pack (concatMap (\b -> let v=showHex b "" in if length v==1 then '0':v else v) (BS.unpack (hash (TE.encodeUtf8 instruction))))
      record = obj [("decisionId",str i),("callId",toJSON (turnId turn)),("node",toJSON node),("template",get source "template"),("promptHash",str digest),("templatePath",get source "path"),("renderedPrompt",str rendered),("model",str model),("effort",str (if executionMode policy == Deep then "xhigh" else effort)),("created",str t),("accepted",Bool False)]
      summary = merge (select ["decisionId","callId","node","promptHash","accepted"] record) (obj [("agent",str (agentIdText (runOwner run))),("promptTemplate",get record "template")])
  saveWork s (setWorkField "decisions" (arr (workItems w "decisions"++[summary])) w)
  writeJSON (archivePath s (agentIdText (runOwner run)) ("decisions/"<>i)) record
  pure (rendered,record)
 logIndex <- length <$> readIORef logs
 modifyIORef' logs (++[obj [("role",toJSON node),("prompt",str rendered),("session",Null)]])
 let updateLog extra = modifyIORef' logs (\entries -> [if index==logIndex then merge entry extra else entry | (index,entry)<-zip [0::Int ..] entries])
 rawResponse <- newIORef Nothing
 result <- try $ do
  raw <- invoke s run rendered
  writeIORef rawResponse (Just raw)
  activity <- readIORef (runActivity run)
  updateLog (obj ([("session",get activity "session"),("answer",str raw)]++[("warning",get activity "warning") | has activity "warning"]))
  require (T.length raw <= 100000) 400 "Decision response exceeds 100000 characters"
  response <- parseValue 400 (parseDecision node) (parseJSONText raw)
  pure (response,merge record (obj [("rawResponse",str raw),("response",decisionRaw response)]))
 case result of
  Left (e::SomeException) -> do
   activity <- readIORef (runActivity run)
   updateLog (obj [("session",get activity "session")])
   raw <- readIORef rawResponse
   let failed=put "error" (str (exceptionText e)) (maybe record (\value -> put "rawResponse" (str value) record) raw)
   locked s (recordDecision s (agentIdText (runOwner run)) failed)
   throwIO e
  Right value@(_,record') -> locked s (recordDecision s (agentIdText (runOwner run)) record') >> pure value
exceptionText :: SomeException -> Text
exceptionText e = case fromException e of Just (APIError _ m) -> m; _ -> T.pack (displayException e)
validateTarget :: Service -> Workload -> Text -> Text -> IO ()
validateTarget s w source target = do
 either decisionFailure pure (policyDelegation w)
 chief <- DB.mainAgent s
 require (target /= source && (source == chief || target == chief)) 400 "Routes are Chief → Sapi or Sapi → Chief"
 void (resolve s target False)
 state <- agentState s target
 require (not (any ((`elem` [Queued,Running]) . turnStatus) (agentTurns state))) 409 "Recipient is busy; responsibility remains with the sender"
 works <- readWorks s
 require (not (any (\c -> callState c == CallQueued && callRecipient c == AgentId target) (concatMap (NE.toList . workCalls) works))) 409 "Recipient already has an accepted call"
 require (length (workCalls w) <= 8) 409 "Routing limit reached; Admin must resolve the request"
 require (target == chief || not (any ((==AgentId target) . callRecipient) (workCalls w))) 409 "This specialist already assessed the request; ask Admin to resolve routing"
transfer :: Service -> Run -> Text -> Text -> Value -> IO Text
transfer s run request reason record = do
 (w,c) <- getWork s (runId run)
 stopped <- readIORef (runCancel run)
 require (not stopped) 409 "Stopped before handoff acceptance"
 let target=text (callField c "target")
 validateTarget s w (agentIdText (runOwner run)) target
 i <- newId
 chief <- DB.mainAgent s
 let child=setCallFields [("handoffReason",str reason)] (newCall (RunId i) (AgentId target) (Just (callIdentity c)) (QueuedAt (if target==chief then ChiefTriage else Assessing)) request)
     reply="Delegated to "<>target<>"."
     updated=c { callProgress=FinishedCall (HandedOff reply) }
     accepted=withCall w updated
 saveWork s accepted { workTracksTask=True, workCalls=workCalls accepted <> (child :| []) }
 accept s (agentIdText (runOwner run)) record "Delegation" (wireName (callNode child))
 pure reply
applyDecision :: Service -> Text -> Maybe Text -> Text -> IO Value
applyDecision s owner current decision = do
 identity <- maybe (failure 409 "Delegation requires this call's recorded Delegation decision") pure current
 (w,c) <- getWork s (RunId identity)
 require (callNode c==Delegation && any (\d -> at d "decisionId"==decision && at d "callId"==identity && at d "node"=="Delegation") (workItems w "decisions")) 409 "Delegation requires this call's recorded Delegation decision"
 record <- readJSON (archivePath s owner ("decisions/"<>decision)) empty
 require (get record "error"==Null && at (get record "response") "event"=="HandoffPrepared") 409 "Delegation decision has not produced a valid response"
 existing <- case find ((==Just (RunId identity)) . callParent) (workCalls w) of
  Just child -> pure child
  Nothing -> do
   runs <- readMVar (serviceRuns s)
   run <- maybe (failure 409 "AgencyRun is no longer active") pure (M.lookup (RunId identity) runs)
   response <- parseValue 409 (parseDecision Delegation) (get record "response")
   case decisionBody response of
    Handoff request -> void (transfer s run request (decisionReason response) record)
    _ -> failure 409 "Delegation requires a handoff decision"
   (updated,_) <- getWork s (RunId identity)
   maybe (failure 409 "Delegation acceptance was not saved") pure (find ((==Just (RunId identity)) . callParent) (workCalls updated))
 pure (obj [("saved",Bool True),("callId",toJSON (callIdentity existing)),("target",toJSON (callRecipient existing))])
data DecisionError
 = TrackedTaskNeedsExecution | OnlyChiefCreates | CriteriaRemoved | RoutineTaskChanged
 | OwnerOnlyPolicySetup
 | InvalidOutcome OutcomeError | UnsupportedSavedTaskSpec | InvalidTaskContract Text | InvalidWorkOrigin Text
 deriving (Eq, Show)
data DecisionStep
 = SaveDecision Workload WorkCall
 | ResolveSpecialist Text
 | EnsureSpecialist Text Text
 | DelegateRequest Text
 deriving (Eq, Show)

-- Decide from an immutable snapshot. The runtime interprets only the resulting work.
advanceDecision :: AgentId -> DecisionResponse -> Workload -> WorkCall -> Either DecisionError DecisionStep
advanceDecision chief response work call = case decisionBody response of
 ReplyNow reply
  | workTracksTask work -> Left TrackedTaskNeedsExecution
  | otherwise -> Right (finish work (FinishedCall (Answered reply)))
 ExecuteTask -> do
  origin <- checkedWorkOrigin work
  accepted <- acceptedTaskSpec work
  let node=if originIsScheduled origin && isJust accepted then Execution else ReadyToWork
  Right (finish (work { workTracksTask=True }) (RunningAt node))
 OutsideSpecialization -> do
  when (callRecipient call /= chief) (policyDelegation work)
  Right $ SaveDecision work $
   if callRecipient call == chief then call { callProgress=RunningAt ChiefTriage }
   else (setCallFields [("target",toJSON chief)] call) { callProgress=RunningAt Delegation }
 SelectSpecialist name -> policyDelegation work >> Right (ResolveSpecialist name)
 NeedSpecialist -> policyDelegation work >> Right (finish work (RunningAt CreatingSapi))
 AskAdmin question -> Right (finish (work { workTracksTask=True }) (FinishedCall (NeedsClarification question)))
 SpecifySapi name role
  | callRecipient call /= chief -> Left OnlyChiefCreates
  | otherwise -> policyDelegation work >> Right (EnsureSpecialist name role)
 PrepareTask taskType spec -> do
   accepted <- acceptedTaskSpec work
   origin <- checkedWorkOrigin work
   let fixed=originIsScheduled origin && isJust accepted
   when (fixed && accepted /= Just spec) (Left RoutineTaskChanged)
   let inherited=maybe [] (NE.toList . specCriteria) accepted
   unless (all (`elem` NE.toList (specCriteria spec)) inherited) (Left CriteriaRemoved)
   let savedType=if originIsScheduled origin then "automated" else taskType
   Right (finish (if fixed then work else acceptTaskSpec savedType spec work) (RunningAt Execution))
 Handoff request -> policyDelegation work >> Right (DelegateRequest request)
 ReportOutcome reply outcome -> do
  spec <- acceptedTaskSpec work >>= maybe (Left (InvalidTaskContract "Task has no accepted specification")) Right
  StructurallyValidOutcome checked <- B.first InvalidOutcome (validateOutcome response outcome spec)
  let saved=mergeFields [("reply",str reply),("evidence",toJSON (decisionEvidence response)),("author",toJSON (callRecipient call))] (toJSON checked)
      result=case outcomeStatus outcome of Completed -> Answered reply; Unresolved -> UnresolvedResult reply
  pure (finish (setWorkField "outcome" saved work) (FinishedCall result))
 where
 finish changed progress = SaveDecision changed (call { callProgress=progress })

checkedWorkOrigin :: Workload -> Either DecisionError ExecutionOrigin
checkedWorkOrigin = B.first InvalidWorkOrigin . checkedOrigin . workOrigin

policyDelegation :: Workload -> Either DecisionError ()
policyDelegation work = do
 origin <- checkedWorkOrigin work
 case (originPurpose origin,originGroup origin) of
  (RoutinePolicySetup _,Nothing) -> Left OwnerOnlyPolicySetup
  _ -> Right ()

acceptedTaskSpec :: Workload -> Either DecisionError (Maybe TaskSpec)
acceptedTaskSpec work = case workTaskContract work of
 Unprepared -> Right Nothing
 Accepted spec -> Right (Just spec)
 UnsupportedLegacy _ -> Left UnsupportedSavedTaskSpec
 Corrupted _ reason -> Left (InvalidTaskContract reason)

decisionFailure :: DecisionError -> IO a
decisionFailure problem = case problem of
 TrackedTaskNeedsExecution -> failure 409 "An accepted Task must execute and report its completion criteria"
 OnlyChiefCreates -> failure 403 "Only Chief can create a Sapi"
 CriteriaRemoved -> failure 400 "A handoff cannot remove accepted completion criteria"
 RoutineTaskChanged -> failure 400 "A Routine Run cannot change its accepted specification; update the Routine and start a new Run"
 OwnerOnlyPolicySetup -> failure 409 "Personal routine policy setup must stay with its owner. Save a policy here, or retry this setup with its owner."
 InvalidOutcome reason -> failure 400 (outcomeError reason)
 UnsupportedSavedTaskSpec -> failure 409 "Saved Task specification must use structured completion criteria; create a new Task."
 InvalidTaskContract reason -> failure 500 ("Saved Task specification is malformed: "<>reason)
 InvalidWorkOrigin reason -> failure 500 reason

runDecision :: (AgentId -> IO ()) -> (Workload -> IO ()) -> Service -> Run -> Turn -> AgentState -> IO (Text,[Value])
runDecision prepareSpecialist verifyCompletion s run turn state = do
 locked s (begin s (runOwner run) turn Nothing)
 logs <- newIORef []
 let loop n = do
      require (n < 12) 409 "Decision transition limit reached; Admin must resolve the request"
      (existing,c) <- locked s (getWork s (turnId turn))
      case callOutput c of
       Just output -> do
        when (callState c `elem` [CallCompleted,CallUnresolved,CallWaiting]) (locked s (verifyCompletion existing))
        pure output
       Nothing -> do
        let node=callNode c
        (response,record) <- decide s run turn state node logs
        result <- locked s $ do
         (current,call) <- getWork s (turnId turn)
         stopped <- readIORef (runCancel run)
         require (not stopped) 409 "Stopped by Admin"
         chief <- DB.mainAgent s
         let owner=agentIdText (runOwner run)
             finish work updated = do
              let following=setCallFields [("observation",decisionRaw response)] updated
                  next=callNode following
              when (callState following `elem` [CallCompleted,CallUnresolved,CallWaiting]) (verifyCompletion work)
              saveWork s (withCall work following)
              accept s owner record (wireName node) (wireName next)
              pure (callOutput following)
         step <- either decisionFailure pure (advanceDecision (AgentId chief) response current call)
         case step of
          SaveDecision work updated -> finish work updated
          ResolveSpecialist name -> do
           target <- resolve s name False
           validateTarget s current owner target
           finish current ((setCallFields [("target",str target)] call) { callProgress=RunningAt Delegation })
          EnsureSpecialist name role -> do
           resolution <- ensureSpecialist s SpecialistRequest
            { specialistName=name, specialistRole=role, specialistManager=runOwner run }
           let (profile,created) = case resolution of
                ExistingSpecialist existing -> (existing,False)
                CreatedSpecialist new -> (new,True)
           prepareSpecialist (profileId profile)
           finish current ((setCallFields [("target",toJSON (profileId profile)),("creation",obj [("name",str name),("role",str role)]),("creationReceipt",obj [("agent",toJSON profile),("created",Bool created)])] call) { callProgress=RunningAt Delegation })
          DelegateRequest request -> Just <$> transfer s run request (decisionReason response) record
        maybe (loop (n+1)) pure result
 output <- loop (0::Int) `catch` (\(e::SomeException) -> do
  locked s $ do
   (work,_) <- getWork s (turnId turn)
   case listToMaybe (reverse [d | d<-workItems work "decisions",get d "callId"==toJSON (turnId turn)]) of
    Just summary | not (boolean (get summary "accepted")) -> do
     record <- readJSON (archivePath s (agentIdText (runOwner run)) ("decisions/"<>at summary "decisionId")) empty
     recordDecision s (agentIdText (runOwner run)) (put "error" (str (exceptionText e)) record)
    _ -> pure ()
  readIORef logs >>= throwIO . RunFailure (exceptionText e))
 (output,) <$> readIORef logs
reconcileCall :: [Value] -> WorkCall -> Validation WorkCall
reconcileCall turns call = case findId (runIdText (callIdentity call)) turns of
 Just turn | isNothing (callOutput call) -> do
  status <- validateValue 500 parseJSON (get turn "status")
  let failed progress = (setCallFields [("error",get turn "error")] call) { callProgress=progress (callNode call) }
  pure $ case status of
   Failed -> failed FailedAt
   Interrupted -> failed InterruptedAt
   Cancelled -> failed CancelledAt
   Queued -> call
   Running -> call
   OutputPending -> call
   Done -> call
   Warning -> call
 _ -> Right call
reconcileWorks :: Service -> IO ()
reconcileWorks s = do
 turns <- DB.turns s
 works <- readWorks s
 forM_ works $ \work -> do
  updated <- validated (traverse (reconcileCall turns) (workCalls work))
  when (updated /= workCalls work) $ saveWork s work { workCalls=updated }
pendingWorks :: Service -> IO ()
pendingWorks s = do
 works <- readWorks s
 existing <- DB.turns s
 forM_ works $ \w -> forM_ (workCalls w) $ \c -> when (callState c == CallQueued && isJust (callParent c) && isNothing (findId (runIdText (callIdentity c)) existing)) $
  case checkedOrigin (workOrigin w) of
   Left reason -> saveWork s (withCall w (setCallFields [("error",str reason)] c { callProgress=FailedAt (callNode c) }))
   Right parentOrigin -> do
    let a=agentIdText (callRecipient c)
        parent=find ((==callParent c) . Just . callIdentity) (workCalls w)
        metadata=KM.fromList [("workloadId",str (workloadId w)),("taskId",get (taskValue w) "taskId"),("caller",maybe Null (toJSON . callRecipient) parent),("parentCall",toJSON (callParent c))]
         <> KM.delete "agent" (KM.delete "call" (originMetadata parentOrigin))
        origin=newOrigin (originPurpose parentOrigin) (originGroup parentOrigin) metadata
    time <- now
    let turn=toJSON (newTurn (callIdentity c) Chat (callRequest c) time (toJSON origin) (originIsScheduled origin))
        message=obj [("role",str "user"),("content",str (callRequest c)),("turn",toJSON (callIdentity c)),("time",str time),("origin",toJSON origin)]
    modifyState s a $ \state -> if isJust (findId (runIdText (callIdentity c)) (items state "turns")) then state else appendItems "chat" [message] (appendItems "turns" [turn] state)
    DB.project s a

clarify :: Service -> Text -> Text -> Text -> IO Text
clarify s a wid message = do
 works <- readWorks s
 w <- maybe (failure 409 "Clarification requires the waiting Chief call") pure (find ((==wid) . workloadId) works)
 chief <- DB.mainAgent s
 let previous=NE.last (workCalls w)
 require (callState previous == CallWaiting && a==chief) 409 "Clarification requires the waiting Chief call"
 i <- newId
 let call=setCallFields [("activation",str "RequestClarified")] (newCall (RunId i) (AgentId a) (Just (callIdentity previous)) (QueuedAt ChiefTriage) message)
 saveWork s w { workCalls=workCalls w <> (call :| []) }
 pendingWorks s
 pure i
workTasks :: Service -> Text -> Bool -> IO Value
workTasks s a full = do
 works <- readWorks s
 rows <- forM [w | w <- works,workTracksTask w,any ((==AgentId a) . callRecipient) (workCalls w)] $ \w -> do
  let lastCall=NE.last (workCalls w)
  decisions <- forM (workItems w "decisions") $ \d -> if full then merge d <$> readJSON (archivePath s (at d "agent") ("decisions/"<>at d "decisionId")) empty else pure d
  pure $ mergeFields [("execution",obj [("state",toJSON (callState lastCall)),("owner",toJSON (callRecipient lastCall)),("currentCall",toJSON (callIdentity lastCall)),("attempt",callField lastCall "attempt")]),("decisions",arr decisions)] (select ["task","workloadId","originalIntent","calls","outcome"] (toJSON w))
 pure (obj [("tasks",arr rows)])
taskList :: Service -> Text -> IO Value
taskList s a = do
 tasks <- items <$> workTasks s a False <*> pure "tasks"
 pure $ obj [("tasks",arr (reverse [let body=get (get t "task") "specification" `fallback` obj [("objective",get t "originalIntent")]; execution=get t "execution"; calls=items t "calls" in obj [("id",get t "workloadId"),("title",get body "objective"),("taskType",get (get t "task") "taskType"),("state",get execution "state"),("owner",get execution "owner"),("sender",maybe Null (`get` "addressedTo") (listToMaybe calls)),("body",str (jsonText body <> "\n")),("result",get (get t "outcome") "reply"),("error",maybe Null (`get` "error") (listToMaybe (reverse calls)))] | t <- tasks]))]
workSummaries :: [Workload] -> [Value]
workSummaries works = [let c=NE.last (workCalls w) in obj [("taskType",get (taskValue w) "taskType"),("id",str (workloadId w)),("taskId",get (taskValue w) "taskId"),("origin",workField w "origin"),("callId",toJSON (callIdentity c)),("owner",toJSON (callRecipient c)),("state",toJSON (callState c)),("participants",toJSON (nub (map callRecipient (NE.toList (workCalls w))))),("output",toJSON (callOutput c)),("error",callField c "error"),("decisions",num (length (workItems w "decisions"))),("calls",num (length (workCalls w)))] | w <- works,workTracksTask w]
