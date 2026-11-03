{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Werror=incomplete-patterns #-}
module Delegation where

import Control.Concurrent
import qualified Data.Map.Strict as M
import Control.Exception
import Control.Monad
import Crypto.Hash.SHA256 (hash)
import Data.Aeson hiding (pairs)
import Data.Aeson.Types (Parser,parseEither)
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Aeson.Key
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import qualified Data.ByteString as BS
import Data.IORef
import Data.List (find,nub,sort)
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Database.SQLite.Simple as SQL
import Numeric (showHex)
import System.Directory
import System.FilePath
import Types
import qualified Store as DB
import qualified Effects
import qualified ManagedState as Managed
import Population
import Memo
import Provider

data Node = Assessing | ChiefTriage | CreatingSapi | ReadyToWork | Delegation | Execution | AutomationAnalysis | Preparation | OutcomeNode | WaitingForAdmin deriving (Eq, Ord, Show, Enum, Bounded)
instance Wire Node where
 wireName node = case node of Assessing -> "Assessing"; ChiefTriage -> "ChiefTriage"; CreatingSapi -> "CreatingSapi"; ReadyToWork -> "ReadyToWork"; Delegation -> "Delegation"; Execution -> "Execution"; AutomationAnalysis -> "AutomationAnalysis"; Preparation -> "Preparation"; OutcomeNode -> "Outcome"; WaitingForAdmin -> "WaitingForAdmin"
instance FromJSON Node where parseJSON = parseWire
instance ToJSON Node where toJSON = str . wireName

data CallState = CallQueued | CallRunning | CallCompleted | CallUnresolved | CallDelegated | CallWaiting | CallFailed | CallInterrupted | CallCancelled deriving (Eq, Ord, Show, Enum, Bounded)
instance Wire CallState where
 wireName state = case state of CallQueued -> "Queued"; CallRunning -> "Running"; CallCompleted -> "Completed"; CallUnresolved -> "Unresolved"; CallDelegated -> "Delegated"; CallWaiting -> "WaitingForAdmin"; CallFailed -> "Failed"; CallInterrupted -> "Interrupted"; CallCancelled -> "Cancelled"
instance FromJSON CallState where parseJSON = parseWire
instance ToJSON CallState where toJSON = str . wireName

data WorkCall = WorkCall
 { callIdentity :: RunId, callOwner :: AgentId, callNode :: Node, callState :: CallState
 , callRequest :: Text, callParent :: Maybe RunId, callOutput :: Maybe Text, callFields :: Object
 } deriving (Eq, Show)
instance FromJSON WorkCall where
 parseJSON = withObject "Workload call" $ \o -> WorkCall <$> o .: "callId" <*> o .: "addressedTo" <*> o .: "node" <*> o .: "state" <*> o .: "request" <*> o .:? "causedBy" <*> o .:? "output" <*> pure (foldr KM.delete o ["callId","addressedTo","node","state","request","causedBy","output"])
instance ToJSON WorkCall where
 toJSON call = mergeFields ([("callId",toJSON (callIdentity call)),("addressedTo",toJSON (callOwner call)),("node",toJSON (callNode call)),("state",toJSON (callState call)),("request",str (callRequest call)),("causedBy",toJSON (callParent call))] ++ [("output",str output) | output <- maybeToList (callOutput call)]) (Object (callFields call))
callField :: WorkCall -> Text -> Value
callField call = get (Object (callFields call))
setCallFields :: [(Text,Value)] -> WorkCall -> WorkCall
setCallFields fields call = call { callFields = KM.fromList [(Data.Aeson.Key.fromText key,value) | (key,value) <- fields] <> callFields call }
newCall :: RunId -> AgentId -> Maybe RunId -> Node -> CallState -> Text -> WorkCall
newCall identity owner parent node state request = WorkCall identity owner node state request parent Nothing (KM.singleton "attempt" (num 1))

data Workload = Workload { workloadId :: Text, workTracked :: Bool, workCalls :: NonEmpty WorkCall, workFields :: Object } deriving (Eq, Show)
instance FromJSON Workload where
 parseJSON = withObject "Workload" $ \o -> Workload <$> o .: "workloadId" <*> o .: "tracked" <*> o .: "calls" <*> pure (foldr KM.delete o ["workloadId","tracked","calls"])
instance ToJSON Workload where
 toJSON work = mergeFields [("workloadId",str (workloadId work)),("tracked",Bool (workTracked work)),("calls",toJSON (workCalls work))] (Object (workFields work))
workField :: Workload -> Text -> Value
workField work = get (Object (workFields work))
workItems :: Workload -> Text -> [Value]
workItems work = list . workField work
setWorkField :: Text -> Value -> Workload -> Workload
setWorkField key value work = work { workFields = KM.insert (Data.Aeson.Key.fromText key) value (workFields work) }
readWorks :: Service -> IO [Workload]
readWorks s = DB.values s DB.Workloads >>= traverse (decodeValue 500)
-- A call with a successor has handed off responsibility, including legacy
-- answered questions whose saved ancestor still says WaitingForAdmin.
frontierCalls :: Workload -> [WorkCall]
frontierCalls work = filter (\call -> not (any ((==Just (callIdentity call)) . callParent) calls)) calls
 where calls=NE.toList (workCalls work)

-- The caller owns the transaction that also saves its command or next deadline.
-- No files, model calls or nested transactions cross this admission interface.
admitRoutineOn :: SQL.Connection -> AgentId -> Turn -> Value -> Maybe TaskSpec -> IO ()
admitRoutineOn db owner turn snapshot accepted = do
 selected<-selectRoutineCodeOn db owner (routineWorkload owner turn snapshot accepted) snapshot accepted
 work<-Effects.pinOn db snapshot (toJSON selected) >>= decodeValue 500
 let message=obj [("role",str "user"),("content",str (turnInput turn)),("turn",toJSON (turnId turn)),("time",turnField turn "created"),("origin",turnField turn "origin")]
 SQL.execute db "INSERT INTO workloads(id,value) VALUES (?,?)" (workloadId work,jsonText (toJSON work))
 let marked=withTurnFields (\value -> if Effects.profileBound (toJSON work) then put "externalBound" (Bool True) value else value) turn
 Managed.enrollOn db (agentIdText owner) (if has (toJSON work) "codeExecution" then withTurnFields (put "codeBound" (Bool True)) marked else marked) message (turnInput turn) []
routineWorkload :: AgentId -> Turn -> Value -> Maybe TaskSpec -> Workload
routineWorkload owner turn snapshot accepted =
 let identity=runIdText (turnId turn)
     first=maybe base (const (setCallFields [("executionBinding",toJSON (OrdinaryBinding owner RoutineSnapshotSource))] base)) accepted
     base=newCall (turnId turn) owner Nothing (if isJust accepted then Execution else ReadyToWork) CallQueued (turnInput turn)
 in Workload identity True (first :| []) (KM.fromList
  [("bindingSchema",num 1),("originalIntent",str (turnInput turn)),("task",obj [("taskId",str ("task_"<>identity)),("taskType",str "automated"),("specification",maybe Null toJSON accepted)])
  ,("origin",turnField turn "origin"),("routineSnapshot",maybe snapshot (\spec -> put "acceptedTask" (toJSON spec) snapshot) accepted),("executionGate",toJSON ExecutionOpen)
  ,("created",turnField turn "created"),("decisions",arr []),("outcome",Null)])
-- Admission owns selection and the trial claim in its caller's existing SQL
-- transaction. The newest prepared watermark is independent of trial activity.
selectRoutineCodeOn :: SQL.Connection -> AgentId -> Workload -> Value -> Maybe TaskSpec -> IO Workload
selectRoutineCodeOn _ _ work _ Nothing = pure work
selectRoutineCodeOn db owner work snapshot (Just accepted) = do
 rows<-DB.queryRowsOn db "SELECT value FROM workloads ORDER BY rowid" ["value"] []
 works<-traverse (DB.decodeStored . (`at` "value") >=> decodeValue 500) rows
 let applicable prepared = let pin=workField prepared "preparationPin" in get pin "owner"==toJSON owner && get pin "routine"==get snapshot "id" && get pin "contractGeneration"==get snapshot "contractGeneration"
     prepared=[old | old<-works,applicable old,at (workField old "preparation") "status"=="awaiting_occurrence"]
     newest=listToMaybe (reverse prepared)
     activeTrial old=case find ((==at (get (workField old "preparation") "trial") "run") . workloadId) works of
      Just trial->any ((`notElem` [CallCompleted,CallFailed,CallUnresolved,CallInterrupted,CallCancelled]) . callState) (workCalls trial)
      Nothing->has (get (workField old "preparation") "trial") "run"
     busy=any activeTrial [old | old<-works,applicable old]
     active=get snapshot "activeCode"
     reference value=get (get (workField value "preparation") "version") "reference"
 operations<-DB.routineOperationsOn db
 eligible<-case newest of
  Nothing->pure False
  Just candidate->do
   pin<-decodeValue 409 (workField candidate "preparationPin")
   pure (DB.analysisFeedbackIds operations snapshot (map toJSON works) (analysisSources pin)==analysisFeedback pin)
 let chosen=case newest of
      Just candidate | eligible && not busy && not (has (get (workField candidate "preparation") "trial") "run") -> Just (reference candidate,CandidateTrial,Just candidate)
      _ | at active "status"=="active" && get active "contractGeneration"==get snapshot "contractGeneration" -> Just (get active "reference",ActiveCode,Nothing)
      _ -> Nothing
 case chosen of
  Nothing->pure work
  Just (raw,role,candidate)->do
   reference_<-decodeValue 409 raw
   version<-resolveCandidateFrom works owner snapshot accepted reference_
   let initial=NE.head (workCalls work)
       binding=ScriptBinding owner RoutineSnapshotSource reference_ role
       code=obj ([("reference",toJSON reference_),("role",toJSON role),("status",str "queued"),("operations",arr [])]++[("structureHash",get version "structureHash") | has version "structureHash"])
       changed=setWorkField "codeExecution" code work {workCalls=setCallFields [("executionBinding",toJSON binding)] initial :| NE.tail (workCalls work)}
   forM_ candidate $ \preparedWork->do
    let claim=obj [("run",str (workloadId work)),("reference",toJSON reference_),("status",str "claimed")]
    DB.putValueOn db DB.Workloads (workloadId preparedWork) (toJSON (setWorkField "preparation" (put "trial" claim (workField preparedWork "preparation")) preparedWork))
   pure changed

-- Both preparation checks and business execution resolve the exact persisted bytes.
resolveCandidateBytes :: AgentId -> Workload -> CandidateReference -> IO Value
resolveCandidateBytes owner source reference@(CandidateReference refOwner preparation digest) = do
 let version=get (workField source "preparation") "version"
 require (refOwner==owner && workloadId source==runIdText preparation && get (workField source "preparationPin") "owner"==toJSON owner) 409 "Candidate reference belongs to another preparation or owner"
 require (get version "reference"==toJSON reference && canonicalHash (remove ["reference"] version)==digest && sourceHash (at version "source")==at version "sourceHash") 409 "Pinned candidate content or reference changed"
 void (either (failure 409 . T.pack) pure (parseEither candidateExplanation version))
 pure version

resolveCandidateFrom :: [Workload] -> AgentId -> Value -> TaskSpec -> CandidateReference -> IO Value
resolveCandidateFrom works owner snapshot accepted reference@(CandidateReference _ preparation _) = do
 source<-maybe (failure 409 "Pinned candidate preparation is missing") pure (find ((==runIdText preparation) . workloadId) works)
 version<-resolveCandidateBytes owner source reference
 let prepared=workField source "preparation"
     pin=workField source "preparationPin"
 require (get pin "owner"==toJSON owner && get pin "routine"==get snapshot "id" && get pin "contractGeneration"==get snapshot "contractGeneration" && get version "contractGeneration"==get snapshot "contractGeneration") 409 "Candidate does not belong to this accepted consumer contract"
 original<-decodeValue 409 (get version "contract")
 explanation<-either (failure 409 . T.pack) pure (parseEither candidateExplanation version)
 forM_ explanation $ \_->do
  void (candidateRubric original (get version "rubric"))
  require (any (\check->get check "rubricAccepted"==Bool True && has check "explanationOperation") (items prepared "checks")) 409 "Hybrid candidate has no actual successful explanation"
  require (all (\check->at (get check "actual") "status"/="ok" || get check "rubricAccepted"==Bool True) (items prepared "checks")) 409 "Hybrid candidate lacks independent semantic checks"
 require (sameSemanticContract original accepted) 409 "Candidate cannot replace accepted task criteria"
 require (at version "interface"=="json-stdin-result-v1" && at version "language"=="python" && get version "dependencies"==arr [] && get version "capabilities"==arr []) 409 "Candidate execution interface is unsupported"
 require (at prepared "status"=="awaiting_occurrence" && get (get prepared "review") "accepted"==Bool True && not (null (items prepared "checks"))
   && all (\check->get check "matched"==Bool True && get check "version"==toJSON reference && get check "sourceHash"==get version "sourceHash") (items prepared "checks")) 409 "Candidate lacks completed checks for this exact version"
 pure version

criterionRows :: TaskSpec -> [(Text,Text)]
criterionRows spec=zip ["criterion-"<>T.pack (show n) | n<-[1::Int ..]] (NE.toList (specCriteria spec))

criterionContext :: TaskSpec -> Value
criterionContext spec=arr [obj [("id",str identity),("criterion",str criterion)] | (identity,criterion)<-criterionRows spec]

candidateRubric :: TaskSpec -> Value -> IO [RubricCriterion]
candidateRubric spec value = do
 rubric<-decodeValue 422 value
 let identities=[identity | RubricCriterion identity _<-rubric]
 require (length identities==length (criterionRows spec) && sort identities==sort (map fst (criterionRows spec))) 422 "Hybrid rubric must classify each original criterion exactly once"
 require (NumericCriterion `elem` [kind | RubricCriterion _ kind<-rubric] && SemanticCriterion `elem` [kind | RubricCriterion _ kind<-rubric]) 422 "Hybrid requires numerical and semantic criteria"
 pure rubric

criterionAssessments :: TaskSpec -> Value -> IO [CriterionAssessment]
criterionAssessments spec value = do
 verdicts<-decodeValue 422 value
 let identities=[identity | CriterionAssessment identity _ _<-verdicts]
 require (length identities==length (criterionRows spec) && sort identities==sort (map fst (criterionRows spec))) 422 "Assessment must cover each original criterion exactly once"
 pure verdicts

criteriaOutcome :: TaskSpec -> [CriterionAssessment] -> Value
criteriaOutcome spec verdicts = obj [("status",str (if all accepted verdicts then "Completed" else "Unresolved")),("satisfiedCriteria",arr [str criterion | (identity,criterion)<-criterionRows spec,CriterionAssessment key CriterionPass _<-verdicts,key==identity]),("notApplicableCriteria",obj [(criterion,str evidence) | (identity,criterion)<-criterionRows spec,CriterionAssessment key CriterionNotApplicable evidence<-verdicts,key==identity]),("artifacts",arr [])]
 where accepted (CriterionAssessment _ verdict _)=verdict `elem` [CriterionPass,CriterionNotApplicable]

sameSemanticContract :: TaskSpec -> TaskSpec -> Bool
sameSemanticContract left right = specObjective left==specObjective right && specOutputs left==specOutputs right && specCriteria left==specCriteria right

resolveCandidate :: Service -> Workload -> WorkCall -> IO (Value,CodeRole)
resolveCandidate s work call = do
 binding<-decodeValue 409 (callField call "executionBinding")
 case binding of
  OrdinaryBinding _ _->failure 409 "This occurrence is not pinned to code"
  ScriptBinding owner _ reference role->do
   require (owner==callOwner call) 409 "Code binding belongs to another call owner"
   accepted<-executionContract work call >>= maybe (failure 409 "Code execution requires a pinned task contract") pure
   works<-readWorks s
   version<-resolveCandidateFrom works owner (workField work "routineSnapshot") accepted reference
   require (get (workField work "codeExecution") "structureHash"==get version "structureHash") 409 "Code occurrence structure pin changed"
   require (get (workField work "codeExecution") "reference"==toJSON reference && get (workField work "codeExecution") "role"==toJSON role) 409 "Code occurrence metadata differs from its immutable binding"
   case role of
    CandidateTrial->do
     let CandidateReference _ preparation _=reference
     source<-maybe (failure 409 "Trial owner preparation is missing") pure (find ((==runIdText preparation) . workloadId) works)
     let claim=get (workField source "preparation") "trial"
     require (get claim "run"==str (workloadId work) && get claim "reference"==toJSON reference) 409 "This occurrence does not own the candidate trial"
    ActiveCode->do
     let admitted=get (workField work "routineSnapshot") "activeCode"
     require (get admitted "reference"==toJSON reference && at admitted "status"=="active") 409 "This occurrence has no accepted active-code binding"
   pure (version,role)

codeProjection :: Value -> Value
codeProjection Null = Null
codeProjection code = mergeFields ([("runtimeResult",runtime),("verification",codeVerification code)]++[("hybridResult",obj [("facts",get (get runtime "actual") "value"),("explanation",get (get operation "response") "text")]) | operation<-maybeToList explanation]) code
 where
 runtime=maybe Null (`get` "result") (listToMaybe (reverse [op | op<-items code "operations",at op "purpose"=="code_execution"]))
 explanation=find ((=="hybrid_explanation") . (`at` "purpose")) (items code "operations")

executionStages :: Value -> Value
executionStages code
 | not (has code "structureHash") = arr []
 | otherwise = arr [obj [("id",str identity),("status",maybe (str "not_started") (\op->if at op "status"=="validating" then str "running" else get op "status") (find ((==identity) . (`at` "stage")) (items code "operations")))] | identity<-["calculate","explain"]]

codeVerification :: Value -> Value
codeVerification code=fromMaybe Null (find (\op->get op "id"==get (get code "verification") "id") (items code "operations"))

generatedCleanupPending :: Workload -> Bool
generatedCleanupPending work=any (\key->at (workField work key) "status"=="cleanup_pending") ["preparation","codeExecution"]

routineSpecPinned :: Workload -> Bool
routineSpecPinned work = has (workField work "routineSnapshot") "criteria"

data TaskSpec = TaskSpec { specObjective :: Text, specInputs :: [Text], specOutputs :: NonEmpty Text, specCriteria :: NonEmpty Text } deriving (Eq, Show)
instance FromJSON TaskSpec where
 parseJSON value = do
  onlyFields ["objective","inputs","expectedOutputs","completionCriteria"] value
  void (textFieldP "objective" 16000 value)
  let nonempty key = stringsP key True (get value key) >>= maybe (fail "Expected at least one item") pure . NE.nonEmpty
  TaskSpec <$> parseJSON (get value "objective") <*> stringsP "inputs" False (get value "inputs") <*> nonempty "expectedOutputs" <*> nonempty "completionCriteria"
instance ToJSON TaskSpec where
 toJSON spec = obj [("objective",str (specObjective spec)),("inputs",toJSON (specInputs spec)),("expectedOutputs",toJSON (NE.toList (specOutputs spec))),("completionCriteria",toJSON (NE.toList (specCriteria spec)))]
-- Resolve only local durable sources. Mutable future Routine definitions are not
-- execution contracts, and old installations are not assigned invented versions.
executionContract :: Workload -> WorkCall -> IO (Maybe TaskSpec)
executionContract work call = do
 let marker=workField work "bindingSchema"
     acceptedDecisionForCall entry = get entry "callId"==toJSON (callIdentity call) && boolean (get entry "accepted") && at (get entry "response") "event"=="TaskPrepared"
     expected = callNode call==Execution || any acceptedDecisionForCall (workItems work "decisions")
       || (callIdentity call==callIdentity (NE.head (workCalls work)) && has (workField work "routineSnapshot") "acceptedTask")
 require (marker==Null || marker==num 1) 409 "Unsupported execution binding schema"
 if callField call "executionBinding"==Null then do
  require (marker==Null || not expected) 409 "Accepted execution binding is missing"
  pure Nothing
 else do
  binding <- decodeValue 409 (callField call "executionBinding")
  let (owner,source)=bindingContract binding
  require (owner==callOwner call) 409 "Execution binding belongs to another owner"
  case source of
   RoutineSnapshotSource -> do
    let snapshot=workField work "routineSnapshot"
    require (callIdentity call==callIdentity (NE.head (workCalls work)) && get snapshot "owner"==toJSON owner && has snapshot "criteria") 409 "Routine contract source does not belong to this call"
    Just <$> decodeValue 409 (get snapshot "acceptedTask")
   TaskDecisionSource identity -> do
    record <- maybe (failure 409 "Accepted task decision is missing") pure (find ((==identity) . (`at` "decisionId")) (workItems work "decisions"))
    require (get record "callId"==toJSON (callIdentity call) && get record "agent"==toJSON owner && boolean (get record "accepted") && at record "node"=="ReadyToWork" && at (get record "response") "event"=="TaskPrepared") 409 "Execution contract is not this call's accepted task decision"
    Just <$> decodeValue 409 (get (get record "response") "specification")

bindingDetail :: Workload -> WorkCall -> IO Value
bindingDetail work call = renderBinding `catch` (\(APIError _ reason) -> pure (obj [("status",str "invalid"),("reason",str reason)]))
 where
 renderBinding = do
  contract <- executionContract work call
  pure $ case contract of
   Nothing -> obj [("status",str (if workField work "bindingSchema"==Null then "legacy_unknown" else "not_selected"))]
   Just spec -> obj [("status",str "pinned"),("binding",callField call "executionBinding"),("contract",toJSON spec),("inputs",toJSON (specInputs spec)),("request",str (callRequest call))]

data OutcomeStatus = Completed | Unresolved deriving (Eq, Ord, Show, Enum, Bounded)
instance Wire OutcomeStatus where wireName status = case status of Completed -> "Completed"; Unresolved -> "Unresolved"
instance FromJSON OutcomeStatus where parseJSON = parseWire
instance ToJSON OutcomeStatus where toJSON = str . wireName
data Outcome = Outcome { outcomeStatus :: OutcomeStatus, satisfiedCriteria :: [Text], outcomeArtifacts :: [Text], notApplicable :: M.Map Text Text } deriving (Eq, Show)
instance FromJSON Outcome where
 parseJSON value = do
  onlyFields ["status","satisfiedCriteria","artifacts","notApplicableCriteria"] value
  skipped <- parseJSON (get value "notApplicableCriteria" `fallback` empty)
  forM_ (M.elems skipped) $ \reason -> void (textFieldP "reason" 4000 (obj [("reason",str reason)]))
  Outcome <$> parseJSON (get value "status") <*> stringsP "satisfiedCriteria" False (get value "satisfiedCriteria") <*> stringsP "artifacts" False (get value "artifacts") <*> pure skipped

data Decision
 = ReplyNow Text | ExecuteTask | OutsideSpecialization | SelectSpecialist Text
 | NeedSpecialist | AskAdmin Text | SpecifySapi Text Text | PrepareTask Text TaskSpec
 | Handoff Text | ReportOutcome Text Outcome | ReportAnalysis AnalysisDecision
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
  "AutomationAnalyzed" -> variant ["decision"] (ReportAnalysis <$> parseJSON (get value "decision"))
  _ -> fail "Unknown decision event"
 pure (DecisionResponse body reason evidence value)

promptNames :: [(Node,Text)]
promptNames = [(Assessing,"decision-triage"),(ChiefTriage,"decision-chief-triage"),(CreatingSapi,"decision-create-sapi"),(ReadyToWork,"decision-prepare-task"),(Delegation,"decision-delegation"),(Execution,"decision-execution"),(AutomationAnalysis,"decision-automation-analysis")]
eventsFor :: Node -> [Text]
eventsFor node = case node of
 Assessing -> ["FitsSpecialization","OutsideSpecialization"]
 ChiefTriage -> ["FitsSpecialization","SpecialistSelected","NewSpecialistNeeded","RequestUnclear"]
 CreatingSapi -> ["SapiSpecified"]
 ReadyToWork -> ["TaskPrepared"]
 Delegation -> ["HandoffPrepared"]
 Execution -> ["Outcome","OutsideSpecialization"]
 AutomationAnalysis -> ["AutomationAnalyzed"]
 Preparation -> []
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
-- Canonical membership prevents editable legacy child files from removing a stop fence.
callAllowed :: Service -> Turn -> IO Bool
callAllowed s turn = do
 found <- findWork s (turnId turn)
 DB.occurrenceAllowed s (maybe (turnField turn "origin") (\(work,_) -> workField work "origin") found)
requireCallAllowed :: Service -> Turn -> IO ()
requireCallAllowed s turn = do
 allowed <- callAllowed s turn
 unless allowed (failure 409 "Routine run is stopped")
getWork :: Service -> RunId -> IO (Workload,WorkCall)
getWork s i = findWork s i >>= maybe (failure 409 "Unknown workload call") pure
saveWork :: Service -> Workload -> IO ()
saveWork s w = DB.connection s (\db -> SQL.withTransaction db (saveWorkOn s db w))
saveWorkOn :: Service -> SQL.Connection -> Workload -> IO ()
saveWorkOn s db w = do
 rows<-DB.queryRowsOn db "SELECT value FROM workloads WHERE id=?" ["value"] [SQL.SQLText (workloadId w)]
 previous<-traverse (DB.decodeStored . (`at` "value") >=> decodeValue 500) (listToMaybe rows)
 let call=NE.last (workCalls w)
     before=NE.last . workCalls <$> previous
     changed=maybe False (\old -> workTracked w && (not (workTracked old) || fmap (\c -> (callState c,callOwner c)) before /= Just (callState call,callOwner call))) previous
 when changed $ do
  (source,agency) <- currentActor s
  let state=callState call
      actor=if isJust agency || state==CallCancelled then source else if state==CallQueued then "system" else agentIdText (callOwner call)
      detail=obj [("owner",toJSON (callOwner call)),("task",workField w "task"),("previous",maybe Null (toJSON . callState) before),("call",toJSON (callIdentity call)),("error",callField call "error")]
  DB.recordOn s db (map (agentIdText . callOwner) (NE.toList (workCalls w))) (at (workField w "task") "taskId") ("task."<>T.toLower (wireName state)) detail actor (if state `elem` [CallQueued,CallCancelled] then Nothing else Just "chatInput")
 DB.putValueOn db DB.Workloads (workloadId w) (toJSON w)
withCall :: Workload -> WorkCall -> Workload
withCall w c = w { workCalls = fmap (\x -> if callIdentity x == callIdentity c then c else x) (workCalls w) }
setAssessment :: Workload -> Value -> Value -> Workload
setAssessment work record assessment = setWorkField "decisions" (arr (map update (workItems work "decisions"))) work
 where update entry = if get entry "decisionId"==get record "decisionId" then put "assessment" assessment entry else entry

validateOutcome :: DecisionResponse -> Outcome -> [Text] -> IO ()
validateOutcome response outcome criteria = do
 forM_ (M.keys (notApplicable outcome)) $ \criterion ->
  require (criterion `elem` criteria && "If " `T.isPrefixOf` criterion && criterion `notElem` satisfiedCriteria outcome) 400 "Only an unsatisfied, explicit If criterion can be not applicable"
 when (outcomeStatus outcome == Completed) $
  require (not (null (decisionEvidence response)) && all (`elem` (satisfiedCriteria outcome ++ M.keys (notApplicable outcome))) criteria) 400 "Completed outcome requires evidence for all applicable completion criteria"
begin :: Service -> AgentId -> Turn -> IO ()
begin s a turn = do
 found <- findWork s (turnId turn)
 let routine=has (turnField turn "origin") "routine"
     first=newCall (turnId turn) a Nothing (if routine then ReadyToWork else Assessing) CallRunning (turnInput turn)
     initial=Workload (runIdText (turnId turn)) routine (first :| []) (KM.fromList [("bindingSchema",num 1),("originalIntent",str (turnInput turn)),("task",obj [("taskId",str ("task_"<>runIdText (turnId turn))),("taskType",str (if routine then "automated" else "work")),("specification",Null)]),("origin",merge (obj [("agent",toJSON a),("call",toJSON (turnId turn))]) (turnField turn "origin")),("decisions",arr []),("outcome",Null)])
     (w,c)=fromMaybe (initial,first) found
     running=if isNothing (callOutput c) then c { callState=CallRunning, callFields=KM.delete "error" (callFields c) } else c
     updated=setCallFields [("attempt",num (fromMaybe 1 (turnAttempt turn)))] running
 saveWork s (withCall w updated)
conversation :: Service -> AgentId -> Turn -> AgentState -> IO Value
conversation s a turn state = do
 manifests <- fmap obj $ fmap catMaybes $ forM ["identity","computer-use","host-control","host-facts","corpora-state"] $ \name -> do
  let p = agentRoot s (agentIdText a) </> "manifests" </> T.unpack name <.> "md"
  exists <- doesFileExist p
  if exists then Just . (name,) . str <$> readText p else pure Nothing
 memo <- (get <$> readMemo (workspaceRoot s (agentIdText a)) <*> pure "content") `catch` (\(APIError _ e) -> pure (obj [("error",str e)]))
 let reviewing t = batchLeader t /= turnId turn && turnStatus t `elem` [Running,Failed,Interrupted]
     chat = takeLast 20 [let matching = find ((==get m "turn") . toJSON . turnId) (agentTurns state) in if at m "role" == "user" && maybe False reviewing matching then put "underPrevAgencyReview" (Bool True) m else m | m <- list (stateField state "chat"), not (has (get m "origin") "group")]
 pure $ obj [("manifests",remove ["corpora-state"] manifests),("staticContext",get manifests "corpora-state" `fallback` str ""),("instantContext",obj [("chat",arr chat),("memo",memo)]),("inputs",stateField state "agency_batch" `fallback` arr [obj [("content",str (turnInput turn))]])]
 where takeLast n xs = drop (length xs-n) xs
recordDecision :: Service -> Text -> Value -> IO ()
recordDecision s a record = do
 writeJSON (archivePath s a ("decisions/"<>at record "decisionId")) record
 (w,_) <- field 500 record "callId" >>= getWork s
 saveWork s (withDecisionRecord record w)
withDecisionRecord :: Value -> Workload -> Workload
withDecisionRecord record work = setWorkField "decisions" (arr (map update (workItems work "decisions"))) work
 where update entry = if get entry "decisionId"==get record "decisionId" then merge entry (selectPresent ["accepted","response","error","transition"] record) else entry
accept :: Service -> Text -> Value -> Text -> Text -> IO ()
accept s a record before after = recordDecision s a (acceptedDecision record before after)
acceptedDecision :: Value -> Text -> Text -> Value
acceptedDecision record before after = merge record (obj [("accepted",Bool True),("transition",obj [("before",str before),("event",get record "hostEvent" `fallback` get (get record "response") "event"),("after",str (transition before (at (get record "response") "event") after)),("nextNode",str after)])])
 where transition b e def = fromMaybe def $ lookup (b,e) [(("Assessing","FitsSpecialization"),"ReadyToWork"),(("Assessing","OutsideSpecialization"),"ChiefTriage"),(("ChiefTriage","SpecialistSelected"),"Assessing"),(("ChiefTriage","NewSpecialistNeeded"),"CreatingSapi"),(("ChiefTriage","RequestUnclear"),"WaitingForAdmin"),(("ChiefTriage","FitsSpecialization"),"ReadyToWork")]
decide :: Service -> Run -> Turn -> AgentState -> Node -> IORef [Value] -> IO (DecisionResponse,Value)
decide s run turn state node logs = do
 (rendered,record,responseOnly) <- locked s $ do
  source <- templates s >>= maybe (failure 400 "Unknown decision node") pure . find ((==toJSON node) . (`get` "node"))
  (w,c) <- getWork s (turnId turn)
  DB.requireOccurrence s (workField w "origin")
  contract <- if node==Execution then executionContract w c else pure Nothing
  let taskContext=maybe (workField w "task") (\spec -> put "specification" (toJSON spec) (workField w "task")) contract
  conv <- if node==AutomationAnalysis then pure Null else conversation s (runOwner run) turn state
  chief <- DB.mainAgent s
  members <- team s
  extra <- if KM.member "agency_batch" (agentFields state) then ("\n\n" <>) <$> prompt s "agency-chat" else pure ""
  scheduled <- if has (workField w "origin") "routine" && at (workField w "origin") "trigger"/="manual" then ("\n\n" <>) <$> prompt s "scheduled-execution" else pure ""
  externalInstruction<-if Effects.profileBound (toJSON w) then ("\n\n" <>) <$> prompt s "receipt-feed-content" else pure ""
  pinned <- if node==ReadyToWork && routineSpecPinned w then ("\n\n" <>) <$> prompt s "decision-routine-specification" else pure ""
  routine <- if has (workField w "origin") "routine" then do
   definitions <- DB.values s DB.ScheduledRoutines
   pure (obj [("definition",fromMaybe Null (findId (at (workField w "origin") "routine") definitions)),("snapshot",workField w "routineSnapshot")])
   else pure Null
  wiki <- prompt s "notes-wiki"
  envelope <- prompt s "decision-envelope"
  let instruction = at source "content" <> extra <> scheduled <> pinned <> externalInstruction
      context = if node==AutomationAnalysis then obj [("selfId",toJSON (runOwner run)),("analysisEvidence",workField w "analysisEvidence")] else obj [("externalExecution",workField w "externalExecution"),("originalIntent",workField w "originalIntent"),("task",taskContext),("routine",routine),("tracked",Bool (workTracked w)),("currentCall",toJSON c),("selfId",str (agentIdText (runOwner run))),("chiefId",str chief),("team",arr members),("routingHistory",arr [select ["callId","addressedTo","node"] (toJSON call) | call <- NE.toList (workCalls w)]),("conversation",conv),("observation",callField c "observation")]
      rendered = render envelope [("node",wireName node),("instruction",instruction),("notes_wiki",if node==AutomationAnalysis then "" else render wiki [("notes_example",T.pack (repositoryRoot s </> "corpora/prompts/examples/jarvis-notes.html"))]),("context",jsonText context),("events",T.intercalate ", " (sort (eventsFor node)))]
  (model,effort) <- modelDefaults
  policy <- executionSettings s (agentIdText (runOwner run))
  i <- newId
  t <- now
  let digest = T.pack (concatMap (\b -> let v=showHex b "" in if length v==1 then '0':v else v) (BS.unpack (hash (TE.encodeUtf8 instruction))))
      record = obj [("decisionId",str i),("callId",toJSON (turnId turn)),("node",toJSON node),("template",get source "template"),("promptHash",str digest),("templatePath",get source "path"),("renderedPrompt",str rendered),("model",str model),("effort",str (if executionMode policy == Deep then "xhigh" else effort)),("created",str t),("accepted",Bool False)]
      summary = merge (select ["decisionId","callId","node","promptHash","accepted"] record) (obj [("purpose",str (if node==AutomationAnalysis then "automation_analysis" else "ordinary_execution")),("agent",str (agentIdText (runOwner run))),("promptTemplate",get record "template")])
  saveWork s (setWorkField "decisions" (arr (workItems w "decisions"++[summary])) w)
  writeJSON (archivePath s (agentIdText (runOwner run)) ("decisions/"<>i)) record
  pure (rendered,record,node==AutomationAnalysis || Effects.profileBound (toJSON w))
 logIndex <- length <$> readIORef logs
 modifyIORef' logs (++[obj [("role",toJSON node),("prompt",str rendered),("session",Null)]])
 let updateLog extra = modifyIORef' logs (\entries -> [if index==logIndex then merge entry extra else entry | (index,entry)<-zip [0::Int ..] entries])
 rawResponse <- newIORef Nothing
 result <- try $ do
  let observe snapshot = locked s $ do
       (current,_) <- getWork s (turnId turn)
       let update entry = if get entry "decisionId"==get record "decisionId" then put "evidence" (put "purpose" (str (if node==AutomationAnalysis then "automation_analysis" else "ordinary_execution")) (toJSON snapshot)) entry else entry
       let observed=Effects.observeEvidence (at record "decisionId") snapshot (toJSON (setWorkField "decisions" (arr (map update (workItems current "decisions"))) current))
       DB.putValue s DB.Workloads (workloadId current) observed
       require (get (Effects.external observed) "opaqueEffectUnknown"==Null) 409 "Response-only tool evidence is uncertain; external delivery is blocked"
  raw <- (if responseOnly then invokeAnalysisObserved else invokeObserved) s run rendered observe
  writeIORef rawResponse (Just raw)
  when responseOnly $ locked s $ do
   (current,_)<-getWork s (turnId turn)
   Effects.requireContentTrace (toJSON current) (at record "decisionId")
  activity <- readIORef (runActivity run)
  updateLog (obj ([("session",get activity "session"),("answer",str raw)]++[("warning",get activity "warning") | has activity "warning"]))
  require (T.length raw <= 100000) 400 "Decision response exceeds 100000 characters"
  parsed <- try (parseValue 400 (parseDecision node) (parseJSONText raw))
  response <- case parsed of
   Right value -> pure value
   Left (_::APIError) -> do
    when (at (get (parseJSONText raw) "outcome") "status"=="Completed") $ locked s $ do
     (current,_) <- getWork s (turnId turn)
     saveWork s (setAssessment current record (put "decisionId" (get record "decisionId") (assessmentValue AssessmentRejected "application_validation" "Claimed completion did not satisfy the response contract")))
    failure 400 (if at (parseJSONText raw) "event" `notElem` eventsFor node then "Unknown decision event; diagnostic payload omitted" else "Decision response did not satisfy its schema; diagnostic payload omitted")
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
 chief <- DB.mainAgent s
 require (target /= source && (source == chief || target == chief)) 400 "Routes are Chief → Sapi or Sapi → Chief"
 void (resolve s target False)
 state <- agentState s target
 require (not (any ((`elem` [Queued,Running]) . turnStatus) (agentTurns state))) 409 "Recipient is busy; responsibility remains with the sender"
 works <- readWorks s
 require (not (any (\c -> callState c == CallQueued && callOwner c == AgentId target) (concatMap (NE.toList . workCalls) works))) 409 "Recipient already has an accepted call"
 require (length (workCalls w) <= 8) 409 "Routing limit reached; Admin must resolve the request"
 require (target == chief || not (any ((==AgentId target) . callOwner) (workCalls w))) 409 "This specialist already assessed the request; ask Admin to resolve routing"
transfer :: Service -> Run -> Text -> Text -> Value -> IO Text
transfer s run request reason record = do
 (w,c) <- getWork s (runId run)
 DB.requireOccurrence s (workField w "origin")
 stopped <- readIORef (runCancel run)
 require (not stopped) 409 "Stopped before handoff acceptance"
 let target=text (callField c "target")
 validateTarget s w (agentIdText (runOwner run)) target
 i <- newId
 chief <- DB.mainAgent s
 let child=setCallFields [("handoffReason",str reason)] (newCall (RunId i) (AgentId target) (Just (callIdentity c)) (if target==chief then ChiefTriage else Assessing) CallQueued request)
     reply="Delegated to "<>target<>"."
     updated=c { callState=CallDelegated, callOutput=Just reply, callNode=Delegation }
     accepted=withCall w updated
 saveWork s accepted { workTracked=True, workCalls=workCalls accepted <> (child :| []) }
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
 pure (obj [("saved",Bool True),("callId",toJSON (callIdentity existing)),("target",toJSON (callOwner existing))])
runDecision :: Service -> Run -> Turn -> AgentState -> IO (Text,[Value])
runDecision s run turn state = do
 locked s (begin s (runOwner run) turn)
 logs <- newIORef []
 let loop n = do
      require (n < 12) 409 "Decision transition limit reached; Admin must resolve the request"
      (_,c) <- locked s (getWork s (turnId turn))
      case callOutput c of
       Just output -> pure output
       Nothing -> do
        let node=callNode c
        (response,record) <- decide s run turn state node logs
        result <- locked s $ do
         (current,call) <- getWork s (turnId turn)
         DB.requireOccurrence s (workField current "origin")
         stopped <- readIORef (runCancel run)
         require (not stopped) 409 "Stopped by Admin"
         chief <- DB.mainAgent s
         let owner=agentIdText (runOwner run)
             finish work updated next = do
              let following=(setCallFields [("observation",decisionRaw response)] updated) { callNode=next }
              saveWork s (withCall work following)
              accept s owner record (wireName node) (wireName next)
              pure (callOutput following)
         when (Effects.profileBound (toJSON current)) $ require (at (decisionRaw response) "event" `elem` ["Outcome","TaskPrepared","RequestUnclear"]) 409 "This response-only content profile does not support delegation or tool-capable follow-up"
         case decisionBody response of
          ReportAnalysis analysis -> do
           requireActive s owner
           let saved=mergeFields [("reason",str (decisionReason response)),("evidence",toJSON (decisionEvidence response))] (toJSON analysis)
           finish (setWorkField "analysisDecision" saved current) (call {callState=CallCompleted,callOutput=Just (decisionReason response)}) OutcomeNode
          ReplyNow reply -> do
           require (not (workTracked current)) 409 "An accepted Task must execute and report its completion criteria"
           finish current (call { callState=CallCompleted, callOutput=Just reply }) OutcomeNode
          ExecuteTask -> finish (current { workTracked=True }) call ReadyToWork
          OutsideSpecialization -> finish current (if owner==chief then call else setCallFields [("target",str chief)] call) (if owner==chief then ChiefTriage else Delegation)
          SelectSpecialist name -> do
           target <- resolve s name False
           validateTarget s current owner target
           finish current (setCallFields [("target",str target)] call) Delegation
          NeedSpecialist -> finish current call CreatingSapi
          AskAdmin question -> finish (current { workTracked=True }) (call { callState=CallWaiting, callOutput=Just question }) WaitingForAdmin
          SpecifySapi name role -> do
           require (owner==chief) 403 "Only Chief can create a Sapi"
           rows <- DB.agents s
           created <- case find ((==T.toCaseFold name) . T.toCaseFold . (`at` "name")) rows of
            Just row -> require (at row "role" == role) 409 "Name already exists with a different role" >> pure row
            Nothing -> createAgent s (obj [("name",str name),("role",str role),("manager",str owner)])
           finish current (setCallFields [("target",get created "id"),("creation",obj [("name",str name),("role",str role)]),("creationReceipt",obj [("agent",created),("created",Bool True)])] call) Delegation
          PrepareTask taskType spec -> do
           let previous=get (workField current "task") "specification"
           when (routineSpecPinned current) $ require (toJSON spec==previous) 409 "Routine goal, inputs and criteria are pinned for this run"
           require (all (`elem` map str (NE.toList (specCriteria spec))) (items previous "completionCriteria")) 400 "A handoff cannot remove accepted completion criteria"
           let task=mergeFields [("taskType",str (if has (workField current "origin") "routine" then "automated" else taskType)),("specification",toJSON spec)] (workField current "task")
           require (callField call "executionBinding"==Null) 409 "Execution binding cannot be replaced"
           let binding=OrdinaryBinding (callOwner call) (TaskDecisionSource (at record "decisionId"))
               following=(setCallFields [("executionBinding",toJSON binding),("observation",decisionRaw response)] call) {callNode=Execution}
               acceptedRecord=acceptedDecision record "ReadyToWork" "Execution"
               committed=withDecisionRecord acceptedRecord (setWorkField "task" task current)
           saveWork s (withCall committed following)
           writeJSON (archivePath s owner ("decisions/"<>at record "decisionId")) acceptedRecord
           pure Nothing
          Handoff request -> Just <$> transfer s run request (decisionReason response) record
          ReportOutcome reply outcome -> do
           pinnedContract <- executionContract current call
           criteria <- maybe (decodeValue 500 (get (get (workField current "task") "specification") "completionCriteria")) (pure . NE.toList . specCriteria) pinnedContract
           validation <- try (validateOutcome response outcome criteria)
           case validation of
            Left (APIError code _) -> do
             let assessment=put "decisionId" (get record "decisionId") (assessmentValue AssessmentRejected "application_validation" "Outcome did not satisfy the pinned criteria")
             saveWork s (setAssessment current record assessment)
             failure code "Outcome did not satisfy the pinned completion criteria"
            Right () -> pure ()
           let saved=mergeFields [("reply",str reply),("evidence",toJSON (decisionEvidence response)),("author",str owner)] (get (decisionRaw response) "outcome")
           let assessment=put "decisionId" (get record "decisionId") (assessmentValue (if outcomeStatus outcome==Completed then AssessmentAccepted else AssessmentUnresolved) "model_contract_assessment" "Model assessment against the pinned criteria; external effects are not independently verified")
           let assessed=setAssessment (setWorkField "outcome" saved current) record assessment
               completedCall=call { callState=case outcomeStatus outcome of Completed -> CallCompleted; Unresolved -> CallUnresolved, callOutput=Just reply }
           if Effects.profileBound (toJSON current) && outcomeStatus outcome==Completed then do
            let acceptedRecord=acceptedDecision record (wireName node) "Outcome"
                following=(setCallFields [("observation",decisionRaw response)] completedCall) {callNode=OutcomeNode}
                committed=withDecisionRecord acceptedRecord (withCall assessed following)
            DB.connection s $ \db -> SQL.withTransaction db $ do
             result<-Effects.acceptContentOn db (toJSON committed) (at record "decisionId") reply
             acceptedWork<-case result of
              Effects.ContentCommitted value->decodeValue 500 value
              Effects.ContentConflict value->do
               retained<-decodeValue 500 value
               let reason="Source cursor changed concurrently; model content retained without host acceptance or delivery"
                   conflictOutcome=put "hostReason" (str reason) (put "status" (toJSON Unresolved) (workField retained "outcome"))
                   conflictAssessment=put "decisionId" (get record "decisionId") (assessmentValue AssessmentUnresolved "cursor_conflict" reason)
               pure (withCall (setAssessment (setWorkField "outcome" conflictOutcome retained) record conflictAssessment) following {callState=CallUnresolved})
             saveWorkOn s db acceptedWork
            writeJSON (archivePath s owner ("decisions/"<>at record "decisionId")) acceptedRecord
            pure (Just reply)
           else finish assessed completedCall OutcomeNode
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
reconcileWorks :: Service -> IO ()
reconcileWorks s = do
 turns <- DB.turns s
 works <- readWorks s
 forM_ works $ \w -> do
  permitted <- DB.occurrenceAllowed s (workField w "origin")
  -- Canonical cancellation is terminal. A later process-interrupted projection
  -- must not replace its known cause with an uncertain execution failure.
  updated <- forM (workCalls w) $ \c -> if callState c==CallCancelled then pure c else if not permitted && callState c `elem` [CallQueued,CallRunning,CallWaiting] then pure (c {callState=CallCancelled,callOutput=Nothing}) else case findId (runIdText (callIdentity c)) turns of
   Just t | isNothing (callOutput c) -> do
    status <- field 500 t "status"
    pure $ case status of
     Failed -> (setCallFields [("error",get t "error")] c) { callState=CallFailed }
     Interrupted -> (setCallFields [("error",get t "error")] c) { callState=CallInterrupted }
     Cancelled -> (setCallFields [("error",get t "error")] c) { callState=CallCancelled }
     Queued -> c
     Running -> c
     OutputPending -> c
     Done -> c
     Warning -> c
   _ -> pure c
  when (updated /= workCalls w) $ saveWork s w { workCalls=updated }
pendingWorks :: Service -> IO ()
pendingWorks s = do
 works <- readWorks s
 existing <- DB.turns s
 forM_ works $ \w -> do
  permitted <- DB.occurrenceAllowed s (workField w "origin")
  forM_ (workCalls w) $ \c -> when (permitted) $ when (callState c == CallQueued && isJust (callParent c) && isNothing (findId (runIdText (callIdentity c)) existing)) $ do
   let a=agentIdText (callOwner c)
       parent=find ((==callParent c) . Just . callIdentity) (workCalls w)
       origin=mergeFields [("workloadId",str (workloadId w)),("taskId",get (workField w "task") "taskId"),("caller",maybe Null (toJSON . callOwner) parent),("parentCall",toJSON (callParent c))] (remove ["agent","call"] (workField w "origin"))
   time <- now
   let turn=toJSON (newTurn (callIdentity c) Chat (callRequest c) time origin (has origin "routine" && at origin "trigger"/="manual"))
       message=obj [("role",str "user"),("content",str (callRequest c)),("turn",toJSON (callIdentity c)),("time",str time),("origin",origin)]
   modifyState s a $ \state -> if isJust (findId (runIdText (callIdentity c)) (items state "turns")) then state else appendItems "chat" [message] (appendItems "turns" [turn] state)
   DB.project s a
clarify :: Service -> Text -> Text -> Text -> IO Text
clarify s a wid message = do
 works <- readWorks s
 w <- maybe (failure 409 "Clarification requires the waiting Chief call") pure (find ((==wid) . workloadId) works)
 DB.requireOccurrence s (workField w "origin")
 chief <- DB.mainAgent s
 let previous=NE.last (workCalls w)
 require (callState previous == CallWaiting && a==chief) 409 "Clarification requires the waiting Chief call"
 i <- newId
 let call=setCallFields [("activation",str "RequestClarified")] (newCall (RunId i) (AgentId a) (Just (callIdentity previous)) ChiefTriage CallQueued message)
 let consumed c=if callIdentity c==callIdentity previous then c {callState=CallDelegated} else c
 saveWork s w { workCalls=fmap consumed (workCalls w) <> (call :| []) }
 pendingWorks s
 pure i
workTasks :: Service -> Text -> Bool -> IO Value
workTasks s a full = do
 works <- readWorks s
 rows <- forM [w | w <- works,workTracked w,any ((==AgentId a) . callOwner) (workCalls w)] $ \w -> do
  let lastCall=NE.last (workCalls w)
  binding <- bindingDetail w lastCall
  decisions <- forM (workItems w "decisions") $ \d -> if full then merge d <$> readJSON (archivePath s (at d "agent") ("decisions/"<>at d "decisionId")) empty else pure d
  pure $ mergeFields [("executionBinding",binding),("execution",obj [("state",toJSON (callState lastCall)),("owner",toJSON (callOwner lastCall)),("currentCall",toJSON (callIdentity lastCall)),("attempt",callField lastCall "attempt")]),("decisions",arr decisions)] (select ["task","workloadId","originalIntent","calls","outcome"] (toJSON w))
 pure (obj [("tasks",arr rows)])
taskList :: Service -> Text -> IO Value
taskList s a = do
 tasks <- items <$> workTasks s a False <*> pure "tasks"
 pure $ obj [("tasks",arr (reverse [let body=get (get t "task") "specification" `fallback` obj [("objective",get t "originalIntent")]; execution=get t "execution"; calls=items t "calls" in obj [("id",get t "workloadId"),("title",get body "objective"),("taskType",get (get t "task") "taskType"),("state",get execution "state"),("owner",get execution "owner"),("sender",maybe Null (`get` "addressedTo") (listToMaybe calls)),("body",str (jsonText body <> "\n")),("result",get (get t "outcome") "reply"),("error",maybe Null (`get` "error") (listToMaybe (reverse calls)))] | t <- tasks]))]
workSummaries :: [Workload] -> [Value]
workSummaries works = [let c=NE.last (workCalls w) in obj [("taskType",get (workField w "task") "taskType"),("id",str (workloadId w)),("taskId",get (workField w "task") "taskId"),("origin",workField w "origin"),("callId",toJSON (callIdentity c)),("owner",toJSON (callOwner c)),("state",toJSON (callState c)),("participants",toJSON (nub (map callOwner (NE.toList (workCalls w))))),("output",toJSON (callOutput c)),("error",callField c "error"),("decisions",num (length (workItems w "decisions"))),("calls",num (length (workCalls w)))] | w <- works,workTracked w]
