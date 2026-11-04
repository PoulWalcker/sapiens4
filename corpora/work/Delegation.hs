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
import Data.Aeson.Types (Parser, parseEither)
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
import qualified Kit
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

-- A finished call owns its required reply and determines its terminal node.
-- Active/stopped calls retain the node needed for retry and have no output.
data CallResult = Answered Text | UnresolvedResult Text | HandedOff Text | NeedsClarification Text deriving (Eq, Show)
data CallProgress = QueuedAt Node | RunningAt Node | WaitingAt Node | FailedAt Node | InterruptedAt Node | CancelledAt Node | FinishedCall CallResult | RetainedCallResult Node CallResult | UnresolvedAt Node (Maybe Text) deriving (Eq, Show)
data WorkCall = WorkCall
 { callIdentity :: RunId, callOwner :: AgentId, callProgress :: CallProgress
 , callRequest :: Text, callParent :: Maybe RunId, callFields :: Object
 } deriving (Eq, Show)
callState :: WorkCall -> CallState
callState call = case callProgress call of
 QueuedAt _ -> CallQueued; RunningAt _ -> CallRunning; WaitingAt _ -> CallWaiting; FailedAt _ -> CallFailed; InterruptedAt _ -> CallInterrupted; CancelledAt _ -> CallCancelled
 UnresolvedAt _ _ -> CallUnresolved
 RetainedCallResult _ result -> callState (call {callProgress=FinishedCall result})
 FinishedCall result -> case result of
  Answered _ -> CallCompleted; UnresolvedResult _ -> CallUnresolved; HandedOff _ -> CallDelegated; NeedsClarification _ -> CallWaiting
callNode :: WorkCall -> Node
callNode call = case callProgress call of
 QueuedAt node -> node; RunningAt node -> node; WaitingAt node -> node; FailedAt node -> node; InterruptedAt node -> node; CancelledAt node -> node
 UnresolvedAt node _ -> node
 RetainedCallResult node _ -> node
 FinishedCall result -> case result of
  Answered _ -> OutcomeNode; UnresolvedResult _ -> OutcomeNode; HandedOff _ -> Delegation; NeedsClarification _ -> WaitingForAdmin
callOutput :: WorkCall -> Maybe Text
callOutput call = case callProgress call of
 QueuedAt _ -> Nothing; RunningAt _ -> Nothing; WaitingAt _ -> Nothing; FailedAt _ -> Nothing; InterruptedAt _ -> Nothing; CancelledAt _ -> Nothing
 UnresolvedAt _ output -> output
 RetainedCallResult _ result -> callOutput (call {callProgress=FinishedCall result})
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
       reply <- maybe (fail "Finished workload call requires output") pure output
       let result=constructor reply
           retained=node `elem` [Execution,Preparation,AutomationAnalysis] && state `elem` [CallCompleted,CallUnresolved]
             || node==WaitingForAdmin && state==CallDelegated
       if node==expected then pure (FinishedCall result)
       else if retained then pure (RetainedCallResult node result)
       else fail "Workload call has an inconsistent terminal node"
  progress <- case state of
   CallQueued -> unfinished QueuedAt; CallRunning -> unfinished RunningAt
   CallFailed -> unfinished FailedAt; CallInterrupted -> unfinished InterruptedAt; CallCancelled -> unfinished CancelledAt
   CallCompleted -> finished Answered OutcomeNode
   CallUnresolved -> if isNothing output then pure (UnresolvedAt node Nothing) else finished UnresolvedResult OutcomeNode
   CallDelegated -> finished HandedOff Delegation
   CallWaiting -> if isNothing output then unfinished WaitingAt else finished NeedsClarification WaitingForAdmin
  WorkCall <$> o .: "callId" <*> o .: "addressedTo" <*> pure progress <*> o .: "request" <*> o .:? "causedBy" <*> pure (foldr KM.delete o ["callId","addressedTo","node","state","request","causedBy","output"])
instance ToJSON WorkCall where
 toJSON call = mergeFields ([("callId",toJSON (callIdentity call)),("addressedTo",toJSON (callOwner call)),("node",toJSON (callNode call)),("state",toJSON (callState call)),("request",str (callRequest call)),("causedBy",toJSON (callParent call))] ++ [("output",str output) | output <- maybeToList (callOutput call)]) (Object (callFields call))
callField :: WorkCall -> Text -> Value
callField call = get (Object (callFields call))
setCallFields :: [(Text,Value)] -> WorkCall -> WorkCall
setCallFields fields call = call { callFields = KM.fromList [(Data.Aeson.Key.fromText key,value) | (key,value) <- fields] <> callFields call }
newCall :: RunId -> AgentId -> Maybe RunId -> CallProgress -> Text -> WorkCall
newCall identity owner parent progress request = WorkCall identity owner progress request parent (KM.singleton "attempt" (num 1))

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
admitRoutineOn db owner turn snapshot proposed = do
 accepted<-canonicalRoutineTask snapshot proposed
 selected<-selectRoutineCodeOn db owner (setWorkField "routineAttemptSchema" (num 1) (routineWorkload owner turn snapshot accepted)) snapshot accepted
 work<-Effects.pinOn db snapshot (toJSON selected) >>= decodeValue 500
 let message=obj [("role",str "user"),("content",str (turnInput turn)),("turn",toJSON (turnId turn)),("time",turnField turn "created"),("origin",turnField turn "origin")]
 SQL.execute db "INSERT INTO workloads(id,value) VALUES (?,?)" (workloadId work,jsonText (toJSON work))
 let marked=withTurnFields (\value -> if Effects.profileBound (toJSON work) then put "externalBound" (Bool True) value else value) turn
 Managed.enrollOn db (agentIdText owner) (if has (toJSON work) "codeExecution" then withTurnFields (put "codeBound" (Bool True)) marked else marked) message (turnInput turn) []
-- A compiled routine contract is scoped to the unchanged authored definition.
-- Trigger inputs belong to this occurrence, never to a previous result.
canonicalRoutineTask :: Value -> Maybe TaskSpec -> IO (Maybe TaskSpec)
canonicalRoutineTask definition proposed = do
 let source=get definition "acceptedTaskSource"
     current=get source "owner"==get definition "owner" && get source "contractGeneration"==get definition "contractGeneration"
 if get definition "acceptedTask"==Null || not current then pure proposed else do
  spec<-decodeValue 409 (get definition "acceptedTask")
  let inputs=at definition "inputs"
  pure (Just (if T.null (T.strip inputs) then spec else spec {specInputs=[inputs]}))

rememberRoutineTaskOn :: SQL.Connection -> Workload -> WorkCall -> TaskSpec -> Text -> IO ()
rememberRoutineTaskOn db work call spec decision = do
 let snapshot=workField work "routineSnapshot"
     defaultInput=T.null (T.strip (at snapshot "inputs"))
     eligible=callParent call==Nothing && at (toJSON call) "scope"/="part" && has snapshot "prompt" && not (has snapshot "criteria")
 when eligible $ do
  definition<-Effects.definitionOn db (at snapshot "id")
  require (get definition "owner"==toJSON (callOwner call) && get snapshot "owner"==get definition "owner" && get definition "contractGeneration"==get snapshot "contractGeneration" && get definition "prompt"==get snapshot "prompt" && not (boolean (get definition "deleted"))) 409 "Routine changed before accepting its task contract"
  require (workField work "executionGate"/=toJSON ExecutionStopped) 409 "Stopped before accepting the routine contract"
  operations<-DB.routineOperationsOn db
  require (null (DB.workFeedback operations (toJSON work))) 409 "Rejected source cannot establish a routine contract"
  Effects.requireDefinitionClearOn db definition
  prior<-canonicalRoutineTask definition Nothing
  case prior of
   Just accepted->require (if defaultInput then accepted==spec else sameSemanticContract accepted spec) 409 "Concurrent routine preparation changed the accepted contract"
   Nothing->when defaultInput $ do
    let source=obj [("owner",get definition "owner"),("contractGeneration",get definition "contractGeneration"),("run",str (workloadId work)),("decision",str decision)]
    void (DB.saveRoutineOn db (mergeFields [("acceptedTask",toJSON spec),("acceptedTaskSource",source)] definition))

routineWorkload :: AgentId -> Turn -> Value -> Maybe TaskSpec -> Workload
routineWorkload owner turn snapshot accepted =
 let identity=runIdText (turnId turn)
     first=maybe base (const (setCallFields [("executionBinding",toJSON (OrdinaryBinding owner RoutineSnapshotSource))] base)) accepted
     base=newCall (turnId turn) owner Nothing (QueuedAt (if isJust accepted then Execution else ReadyToWork)) (turnInput turn)
 in Workload identity True (first :| []) (KM.fromList
  [("responsible",toJSON owner),("bindingSchema",num 1),("originalIntent",str (turnInput turn)),("task",obj [("taskId",str ("task_"<>identity)),("taskType",str "automated"),("specification",maybe Null toJSON accepted)])
  ,("origin",turnField turn "origin"),("routineSnapshot",maybe (remove ["acceptedTask","acceptedTaskSource"] snapshot) (\spec -> put "acceptedTask" (toJSON spec) snapshot) accepted),("executionGate",toJSON ExecutionOpen)
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
   let casePin=workField candidate "repairPin"
       repairCase=find ((==get casePin "case") . (`get` "id")) (items snapshot "repairCases")
       repairEligible=casePin==Null || maybe False (\saved->at saved "status"=="awaiting_verification" && at saved "candidate"==workloadId candidate && int (get saved "spentMilliseconds")<int (get saved "limitMilliseconds")) repairCase
   pure (repairEligible && (not (has (get (workField candidate "preparation") "version") "expectedBinding") || Kit.bindingCurrent snapshot (get (get (workField candidate "preparation") "version") "expectedBinding")) && Kit.executionAllowed snapshot (get (workField candidate "preparation") "version") (map toJSON works) && DB.analysisFeedbackIds operations snapshot (map toJSON works) (analysisSources pin)==analysisFeedback pin)
 let chosen=case newest of
      Just candidate | eligible && not busy && not (has (get (workField candidate "preparation") "trial") "run") -> Just (reference candidate,CandidateTrial,Just candidate)
      _ | at active "status"=="active" && get active "contractGeneration"==get snapshot "contractGeneration" && maybe False (\source->Kit.executionAllowed snapshot (get (workField source "preparation") "version") (map toJSON works)) (find ((==at (get active "reference") "preparation") . workloadId) works) -> Just (get active "reference",ActiveCode,Nothing)
      _ -> Nothing
 case chosen of
  Nothing->pure work
  Just (raw,role,candidate)->do
   reference_<-decodeValue 409 raw
   version<-resolveCandidateFrom works owner snapshot accepted reference_
   let initial=NE.head (workCalls work)
       binding=ScriptBinding owner RoutineSnapshotSource reference_ role
       code=obj ([("upgradePin",get version "expectedBinding") | role==CandidateTrial && has (get version "expectedBinding") "case"]++[("reference",toJSON reference_),("role",toJSON role),("status",str "queued"),("operations",arr [])]++[("structureHash",get version "structureHash") | has version "structureHash"]++[("workflowDefinition",get version "workflow") | has version "workflow"])
       changed=setWorkField "codeExecution" code work {workCalls=setCallFields [("executionBinding",toJSON binding)] initial :| NE.tail (workCalls work)}
   forM_ candidate $ \preparedWork->do
    let claim=obj [("run",str (workloadId work)),("reference",toJSON reference_),("status",str "claimed")]
    DB.putValueOn db DB.Workloads (workloadId preparedWork) (toJSON (setWorkField "preparation" (put "trial" claim (workField preparedWork "preparation")) preparedWork))
   pure (maybe changed (\preparedWork->if has (toJSON preparedWork) "repairPin" then setWorkField "repairPin" (workField preparedWork "repairPin") changed else changed) candidate)

-- Both preparation checks and business execution resolve the exact persisted bytes.
resolveCandidateBytes :: AgentId -> Workload -> CandidateReference -> IO Value
resolveCandidateBytes owner source reference = Kit.resolvePreparation owner (toJSON source) reference

resolveCandidateFrom :: [Workload] -> AgentId -> Value -> TaskSpec -> CandidateReference -> IO Value
resolveCandidateFrom works owner snapshot accepted reference@(CandidateReference _ preparation _) = do
 source<-maybe (failure 409 "Pinned candidate preparation is missing") pure (find ((==runIdText preparation) . workloadId) works)
 version<-resolveCandidateBytes owner source reference
 let prepared=workField source "preparation"
     pin=workField source "preparationPin"
 if workField source "standalonePin"/=Null then do
  raw<-requireStandalonePinFrom works (workField source "standalonePin")
  require (get raw "owner"==toJSON owner && get version "standalonePin"==raw && workField source "executionGate"==toJSON ExecutionOpen) 409 "Standalone candidate consumer changed or stopped"
 else do
  require (get pin "owner"==toJSON owner && get pin "routine"==get snapshot "id" && get pin "contractGeneration"==get snapshot "contractGeneration" && get version "contractGeneration"==get snapshot "contractGeneration") 409 "Candidate does not belong to this accepted consumer contract"
  Effects.requireCandidateInput snapshot version
 original<-decodeValue 409 (get version "contract")
 explanation<-either (failure 409 . T.pack) pure (parseEither candidateExplanation version)
 forM_ explanation $ \_->do
  void (candidateRubric original (get version "rubric"))
  require (any (\check->get check "rubricAccepted"==Bool True && has check "explanationOperation") (items prepared "checks")) 409 "Hybrid candidate has no actual successful explanation"
  require (all (\check->at (get check "actual") "status"/="ok" || get check "rubricAccepted"==Bool True) (items prepared "checks")) 409 "Hybrid candidate lacks independent semantic checks"
 require (sameSemanticContract original accepted) 409 "Candidate cannot replace accepted task criteria"
 require (at version "interface"=="json-stdin-result-v1" && (at version "language"=="python" || at version "language"=="workflow" && has version "workflow") && get version "dependencies"==arr [] && get version "capabilities"==arr []) 409 "Candidate execution interface is unsupported"
 require (at prepared "status"=="awaiting_occurrence" && get (get prepared "review") "accepted"==Bool True && not (null (items prepared "checks"))
   && all (\check->get check "matched"==Bool True && get check "version"==toJSON reference && get check "sourceHash"==get version "sourceHash") (items prepared "checks")) 409 "Candidate lacks completed checks for this exact version"
 pure version

requireStandalonePinFrom :: [Workload] -> Value -> IO Value
requireStandalonePinFrom works raw=do
 StandaloneTaskPin owner workload identity decision digest<-decodeValue 409 raw
 source<-maybe (failure 409 "Original standalone Task disappeared") pure (find ((==workload) . workloadId) works)
 call<-maybe (failure 409 "Original standalone Call disappeared") pure (find ((==identity) . callIdentity) (workCalls source))
 spec<-executionContract source call >>= maybe (failure 409 "Original Task is not accepted") pure
 pin<-standalonePin source call spec
 require (toJSON pin==raw && canonicalHash (toJSON spec)==digest && owner==callOwner call && callState call `notElem` [CallCancelled,CallInterrupted,CallFailed] && workField source "executionGate"/=toJSON ExecutionStopped) 409 "Original standalone Task contract or authority changed"
 require (any (\entry->at entry "decisionId"==decision && boolean (get entry "accepted")) (workItems source "decisions")) 409 "Original accepted Task decision changed"
 pure raw

bindStandaloneOn :: SQL.Connection -> Workload -> WorkCall -> Value -> IO Workload
bindStandaloneOn db work call request=do
 works<-DB.queryRowsOn db "SELECT value FROM workloads ORDER BY rowid" ["value"] [] >>= traverse (DB.decodeStored . (`at` "value") >=> decodeValue 500)
 forM_ ["parent","consumer"] $ \key->void (requireStandalonePinFrom works (get request key))
 reference<-decodeValue 409 (get request "reference")
 spec<-executionContract work call >>= maybe (failure 409 "Child Task contract was not accepted") pure
 version<-resolveCandidateFrom works (callOwner call) Null spec reference
 role<-decodeValue 409 (get request "role")
 let CandidateReference _ preparation _=reference
 source<-maybe (failure 409 "Selected preparation disappeared") pure (find ((==runIdText preparation) . workloadId) works)
 let trial=get (workField source "preparation") "trial"
 require (if role==CandidateTrial then get trial "run"==str (workloadId work) && get trial "reference"==toJSON reference && at trial "status"=="claimed" else at trial "status"=="activated" && get trial "reference"==toJSON reference) 409 "Standalone trial or activation changed"
 original<-decodeValue 409 (callField call "executionBinding")
 let (owner,contractSource)=bindingContract original
     binding=ScriptBinding owner contractSource reference role
     code=obj ([("reference",toJSON reference),("role",toJSON role),("status",str "queued"),("operations",arr [])]++[("structureHash",get version "structureHash") | has version "structureHash"]++[("workflowDefinition",get version "workflow") | has version "workflow"]++[("standaloneActivation",trial) | role==ActiveCode])
 pure (setWorkField "codeExecution" code (withCall work (setCallFields [("executionBinding",toJSON binding)] call)))

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
sameSemanticContract left right = specObjective left==specObjective right && specOutputs left==specOutputs right && specCriteria left==specCriteria right && specConditions left==specConditions right

resolveCandidate :: Service -> Workload -> WorkCall -> IO (Value,CodeRole)
resolveCandidate s work call | has (toJSON call) "workflowChild" = do
 require (inheritedWorkflowCall work call) 409 "Workflow child does not belong to the canonical root cohort"
 (version,role)<-resolveCandidate s work (NE.head (workCalls work))
 require (version==get (callField call "workflowChild") "version") 409 "Workflow child version differs from its root binding"
 pure (version,role)
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
         standalone=get (workField work "codeExecution") "standaloneActivation"
     require (get admitted "reference"==toJSON reference && at admitted "status"=="active" || get standalone "reference"==toJSON reference && at standalone "status"=="activated") 409 "This occurrence has no accepted active-code binding"
     when (standalone/=Null) $ do
      let CandidateReference _ preparation _=reference
      source<-maybe (failure 409 "Activated standalone preparation disappeared") pure (find ((==runIdText preparation) . workloadId) works)
      require (at (get (workField source "preparation") "trial") "status"=="activated") 409 "Standalone version is no longer active"
   pure (version,role)

codeProjection :: Value -> Value
codeProjection Null = Null
codeProjection code = mergeFields ([("runtimeResult",runtime),("verification",codeVerification code)]++[("hybridResult",obj [("facts",get (get code "evaluatedResult" `fallback` get runtime "actual") "value"),("explanation",get code "evaluatedExplanation" `fallback` get (get operation "response") "text")]) | operation<-maybeToList explanation]) code
 where
 runtime=maybe Null (`get` "result") (listToMaybe (reverse [op | op<-items code "operations",at op "purpose"=="code_execution"]))
 explanation=find ((=="hybrid_explanation") . (`at` "purpose")) (items code "operations")

executionStages :: Value -> Value
executionStages code
 | has code "workflowDefinition" = arr [mergeFields [("id",str identity),("kind",get node "kind"),("input",get (get receipt "request") "input"),("status",if receipt/=Null then get receipt "status" else str (if finished then "skipped" else "not_started"))] (select ["output","selected","reason"] receipt) | (identity,node)<-pairs nodes,let receipt=get receipts identity]
 | not (has code "structureHash") = arr []
 | otherwise = arr [obj [("id",str identity),("status",maybe (str "not_started") (\op->if at op "status"=="validating" then str "running" else get op "status") (find ((==identity) . (`at` "stage")) (items code "operations")))] | identity<-["calculate","explain"]]

 where
 nodes=get (get code "workflowDefinition") "nodes"
 receipts=get (get code "workflowRun") "nodes"
 finished=any (\(identity,node)->at node "kind"=="finish" && at (get receipts identity) "status"=="completed") (pairs nodes)

codeVerification :: Value -> Value
codeVerification code=fromMaybe Null (find (\op->get op "id"==get (get code "verification") "id") (items code "operations"))

generatedCleanupPending :: Workload -> Bool
generatedCleanupPending work=any ((`elem` ["cleanup_pending","uncertain"]) . (`at` "status") . snd) (pairs (workField work "workflowContainers")) || any (\key->at (workField work key) "status"=="cleanup_pending") ["preparation","codeExecution"]

routineSpecPinned :: Workload -> Bool
routineSpecPinned work = has (workField work "routineSnapshot") "criteria" || has (workField work "routineSnapshot") "acceptedTask"

data TaskSpec = TaskSpec { specObjective :: Text, specInputs :: [Text], specOutputs :: NonEmpty Text, specCriteria :: NonEmpty Text, specConditions :: M.Map Text Text } deriving (Eq, Show)
instance FromJSON TaskSpec where
 parseJSON value = do
  onlyFields ["objective","inputs","expectedOutputs","completionCriteria","conditionalCriteria"] value
  void (textFieldP "objective" 16000 value)
  let nonempty key = stringsP key True (get value key) >>= maybe (fail "Expected at least one item") pure . NE.nonEmpty
  criteria <- nonempty "completionCriteria"
  conditions <- parseJSON (get value "conditionalCriteria" `fallback` empty)
  unless (all (`elem` NE.toList criteria) (M.keys conditions)) (fail "Conditions must identify exact completion criteria")
  forM_ (M.elems conditions) $ \condition -> void (textFieldP "condition" 16000 (obj [("condition",str condition)]))
  TaskSpec <$> parseJSON (get value "objective") <*> stringsP "inputs" False (get value "inputs") <*> nonempty "expectedOutputs" <*> pure criteria <*> pure conditions
instance ToJSON TaskSpec where
 toJSON spec = obj [("objective",str (specObjective spec)),("inputs",toJSON (specInputs spec)),("expectedOutputs",toJSON (NE.toList (specOutputs spec))),("completionCriteria",toJSON (NE.toList (specCriteria spec))),("conditionalCriteria",toJSON (specConditions spec))]
-- Resolve only local durable sources. Mutable future Routine definitions are not
-- execution contracts, and old installations are not assigned invented versions.
inheritedWorkflowCall :: Workload -> WorkCall -> Bool
inheritedWorkflowCall work call=walk [] call
 where
 root=NE.head (workCalls work)
 reference=get (workField work "codeExecution") "reference" `fallback` get (get (workField work "preparation") "version") "reference"
 walk seen current
  | callIdentity current `elem` seen=False
  | otherwise=
   let context=callField current "workflowChild"
       cohort=get (workField work "workflowCohorts") (at context "cohort")
       parent=find ((==callParent current) . Just . callIdentity) (workCalls work)
   in callIdentity current/=callIdentity root && callOwner current==callOwner root
    && callField current "executionBinding"==callField root "executionBinding"
    && at context "root"==workloadId work && get context "parent"==toJSON (callParent current)
    && get cohort "parent"==get context "parent" && toJSON (callIdentity current) `elem` items cohort "children"
    && get (get context "version") "reference"==reference
    && maybe False (\value->callOwner value==callOwner root && (callIdentity value==callIdentity root || walk (callIdentity current:seen) value)) parent

executionContract :: Workload -> WorkCall -> IO (Maybe TaskSpec)
executionContract work call | has (toJSON call) "workflowChild" = do
 require (inheritedWorkflowCall work call) 409 "Workflow child has no canonical inherited contract"
 executionContract work (NE.head (workCalls work))
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
    require (callIdentity call==callIdentity (NE.head (workCalls work)) && get snapshot "owner"==toJSON owner && has snapshot "acceptedTask") 409 "Routine contract source does not belong to this call"
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
 | ChooseWorkflow Text WorkflowPlan
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
  "HandoffPrepared" -> variant ["request","scope","specification"] $ do
   unless (at value "scope" `elem` ["","whole","part"]) (fail "Handoff scope must be whole or part")
   when (at value "scope"=="part") (void (parseJSON (get value "specification") :: Parser TaskSpec))
   Handoff <$> textAt "request" 16000
  "ComposeWorkflow" -> variant ["operation","previous"] (ChooseWorkflow <$> textAt "operation" 128 <*> (ComposeVersion <$> parseJSON (get value "previous")))
  "RunWorkflow" -> variant ["operation","reference"] (ChooseWorkflow <$> textAt "operation" 128 <*> (ExecuteVersion <$> parseJSON (get value "reference")))
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
 Execution -> ["Outcome","OutsideSpecialization","ComposeWorkflow","RunWorkflow"]
 AutomationAnalysis -> []
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

data OutcomeError = InvalidSkippedCriterion | MissingCompletionEvidence deriving (Eq, Show)
outcomeError :: OutcomeError -> Text
outcomeError problem = case problem of
 InvalidSkippedCriterion -> "Outcome criteria must belong to the contract; only an unsatisfied declared conditional criterion can be not applicable"
 MissingCompletionEvidence -> "Completed outcome requires evidence for all applicable completion criteria"
validateOutcome :: DecisionResponse -> Outcome -> TaskSpec -> Either OutcomeError ()
validateOutcome response outcome spec
 | any (`notElem` criteria) (satisfiedCriteria outcome) || any invalidSkip skipped = Left InvalidSkippedCriterion
 | not (null covered) && null (decisionEvidence response) = Left MissingCompletionEvidence
 | outcomeStatus outcome == Completed && any (`notElem` covered) criteria = Left MissingCompletionEvidence
 | otherwise = Right ()
 where
 criteria = NE.toList (specCriteria spec)
 skipped = M.keys (notApplicable outcome)
 covered = satisfiedCriteria outcome ++ skipped
 invalidSkip criterion = criterion `M.notMember` specConditions spec || criterion `elem` satisfiedCriteria outcome
begin :: Service -> AgentId -> Turn -> IO ()
begin s a turn = do
 found <- findWork s (turnId turn)
 let routine=has (turnField turn "origin") "routine"
     standalone=get (turnField turn "origin") "standaloneRun"
     first=newCall (turnId turn) a Nothing (RunningAt (if routine || standalone/=Null then ReadyToWork else Assessing)) (turnInput turn)
     initial=Workload (runIdText (turnId turn)) (routine || standalone/=Null) (first :| []) (KM.fromList [("responsible",toJSON a),("standaloneRun",standalone),("executionGate",toJSON ExecutionOpen),("bindingSchema",num 1),("originalIntent",str (turnInput turn)),("task",obj [("taskId",str ("task_"<>runIdText (turnId turn))),("taskType",str (if routine then "automated" else "work")),("specification",Null)]),("origin",merge (obj [("agent",toJSON a),("call",toJSON (turnId turn))]) (turnField turn "origin")),("decisions",arr []),("outcome",Null)])
     (w,c)=fromMaybe (initial,first) found
     running=if isNothing (callOutput c) then c { callProgress=RunningAt (callNode c), callFields=KM.delete "error" (callFields c) } else c
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
  let taskContext=if node==ReadyToWork && workField w "standaloneRun"/=Null then put "specification" (get (workField w "standaloneRun") "contract") (workField w "task") else workField w "task"
  conv <- conversation s (runOwner run) turn state
  chief <- DB.mainAgent s
  members <- team s
  extra <- if KM.member "agency_batch" (agentFields state) then ("\n\n" <>) <$> prompt s "agency-chat" else pure ""
  scheduled <- if has (workField w "origin") "routine" && at (workField w "origin") "trigger"/="manual" then ("\n\n" <>) <$> prompt s "scheduled-execution" else pure ""
  externalInstruction<-if Effects.profileBound (toJSON w) then ("\n\n" <>) <$> prompt s "receipt-feed-content" else pure ""
  pinned <- if node==ReadyToWork && routineSpecPinned w && callParent c==Nothing then ("\n\n" <>) <$> prompt s "decision-routine-specification" else if node==ReadyToWork && workField w "standaloneRun"/=Null then ("\n\n" <>) <$> prompt s "decision-standalone-specification" else pure ""
  routine <- if has (workField w "origin") "routine" then do
   definitions <- DB.values s DB.ScheduledRoutines
   pure (obj [("definition",fromMaybe Null (findId (at (workField w "origin") "routine") definitions)),("snapshot",workField w "routineSnapshot")])
   else pure Null
  wiki <- prompt s "notes-wiki"
  envelope <- prompt s "decision-envelope"
  feedInput<-if Effects.profileBound (toJSON w) then either (failure 409 . T.pack) (pure . (`get` "input")) (Effects.observedInput (toJSON w)) else pure Null
  works<-readWorks s
  let versions=[mergeFields [("reference",get version "reference"),("source",pin),("contract",get version "contract"),("activated",get (get prepared "trial") "status")] (select ["status","checks"] prepared) | candidate<-works,let pin=workField candidate "standalonePin",get pin "owner"==toJSON (runOwner run),let prepared=workField candidate "preparation",let version=get prepared "version",at prepared "status"=="awaiting_occurrence"]
  let instruction = at source "content" <> extra <> scheduled <> pinned <> externalInstruction
      context = obj [("executionSpecification",maybe Null toJSON contract),("responsible",workField w "responsible"),("remainingCriteria",workField w "remainingCriteria"),("partialResults",arr [obj [("callId",toJSON (callIdentity local)),("author",toJSON (callOwner local)),("outcome",callField local "outcome")] | local<-NE.toList (workCalls w),callField local "outcome"/=Null]),("preparationScope",str (if at (toJSON c) "afterPreparation"=="Delegation" then "root" else "assignment")),("availableWorkflows",arr versions),("acceptedInput",feedInput),("externalExecution",workField w "externalExecution"),("originalIntent",workField w "originalIntent"),("task",taskContext),("routine",routine),("tracked",Bool (workTracked w)),("currentCall",toJSON c),("selfId",str (agentIdText (runOwner run))),("chiefId",str chief),("team",arr members),("routingHistory",arr [select ["callId","addressedTo","node"] (toJSON call) | call <- NE.toList (workCalls w)]),("conversation",conv),("observation",callField c "observation")]
      rendered = render envelope [("node",wireName node),("instruction",instruction),("notes_wiki",render wiki [("notes_example",T.pack (repositoryRoot s </> "corpora/prompts/examples/jarvis-notes.html"))]),("context",jsonText context),("events",T.intercalate ", " (sort (eventsFor node)))]
  (model,effort) <- modelDefaults
  policy <- executionSettings s (agentIdText (runOwner run))
  i <- newId
  t <- now
  let digest = T.pack (concatMap (\b -> let v=showHex b "" in if length v==1 then '0':v else v) (BS.unpack (hash (TE.encodeUtf8 instruction))))
      record = obj [("decisionId",str i),("callId",toJSON (turnId turn)),("node",toJSON node),("template",get source "template"),("promptHash",str digest),("templatePath",get source "path"),("renderedPrompt",str rendered),("model",str model),("effort",str (if executionMode policy == Deep then "xhigh" else effort)),("created",str t),("accepted",Bool False)]
      summary = merge (select ["decisionId","callId","node","promptHash","accepted"] record) (obj [("purpose",str "ordinary_execution"),("agent",str (agentIdText (runOwner run))),("promptTemplate",get record "template")])
  preparedSummary<-DB.connection s (\db->Effects.prepareRoutineAttempt db (toJSON w) (Effects.profileBound (toJSON w)) summary)
  saveWork s (setWorkField "decisions" (arr (workItems w "decisions"++[preparedSummary])) w)
  writeJSON (archivePath s (agentIdText (runOwner run)) ("decisions/"<>i)) record
  pure (rendered,record,Effects.profileBound (toJSON w))
 logIndex <- length <$> readIORef logs
 modifyIORef' logs (++[obj [("role",toJSON node),("prompt",str rendered),("session",Null)]])
 let updateLog extra = modifyIORef' logs (\entries -> [if index==logIndex then merge entry extra else entry | (index,entry)<-zip [0::Int ..] entries])
 rawResponse <- newIORef Nothing
 result <- try $ do
  let observe snapshot = locked s $ do
       (current,_) <- getWork s (turnId turn)
       let update entry = if get entry "decisionId"==get record "decisionId" then Effects.admitRoutineAttempt (put "evidence" (put "purpose" (str "ordinary_execution") (toJSON snapshot)) entry) else entry
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
 require (target == chief || not (any (\call->callOwner call==AgentId target && callField call "outcome"==Null) (workCalls w))) 409 "This specialist already assessed the request; ask Admin to resolve routing"
assignmentSpecification :: Workload -> WorkCall -> Value
assignmentSpecification work call = callField call "specification" `fallback` get (workField work "task") "specification"

acceptedCriteria :: Workload -> [Text]
acceptedCriteria work = nub [criterion | call<-NE.toList (workCalls work),let result=callField call "outcome",criterion<-map text (items result "satisfiedCriteria") ++ map fst (pairs (get result "notApplicableCriteria"))]

handoffSpecification :: Workload -> WorkCall -> Value -> Validation (Text,TaskSpec)
handoffSpecification work call response = do
 original<-validateValue 409 parseJSON (assignmentSpecification work call)
 let partial=at response "scope"=="part"
     scope=if partial || at (toJSON call) "scope"=="part" then "part" else "whole"
 spec<-if partial then validateValue 400 parseJSON (get response "specification") else Right original
 let criteria=NE.toList (specCriteria spec)
 check (all (`elem` NE.toList (specCriteria original)) criteria) 400 "Partial assignment must use original criteria"
 check (all (\criterion->M.lookup criterion (specConditions spec)==M.lookup criterion (specConditions original)) criteria) 400 "Assignment cannot change accepted conditions"
 check (all (`notElem` acceptedCriteria work) criteria) 409 "Reuse accepted results instead of executing completed criteria again"
 check (partial || not (has response "specification") || get response "specification"==toJSON original) 400 "Whole handoff preserves its accepted contract"
 pure (scope,spec)

-- Save the local receipt and the owner's continuation in the same workload write.
-- The existing dispatcher admits that queued call under the original gate/resources.
scopedResult :: RunId -> Workload -> WorkCall -> Value -> Workload
scopedResult continuation work call result =
 let saved=withCall work (setCallFields [("outcome",result)] call)
     root=get (workField saved "task") "specification"
     covered=acceptedCriteria saved
     remaining=[criterion | criterion<-items root "completionCriteria",text criterion `notElem` covered]
     progress=setWorkField "remainingCriteria" (arr remaining) saved
 in if at (toJSON call) "scope"/="part" then setWorkField "outcome" result progress else
    let owner=AgentId (text (workField work "responsible"))
        request=text (workField work "originalIntent")
        next=setCallFields [("specification",root),("scope",str "whole"),("continuation",Bool True),("observation",obj [("partialResult",result),("remainingCriteria",arr remaining)])] (newCall continuation owner (Just (callIdentity call)) (QueuedAt Assessing) request)
        aggregate=mergeFields [("status",str "Unresolved"),("satisfiedCriteria",arr (nub [criterion | local<-NE.toList (workCalls saved),criterion<-items (callField local "outcome") "satisfiedCriteria"])),("notApplicableCriteria",obj [(criterion,reason) | local<-NE.toList (workCalls saved),(criterion,reason)<-pairs (get (callField local "outcome") "notApplicableCriteria")])] result
    in (setWorkField "outcome" aggregate progress) {workCalls=workCalls progress <> (next :| [])}

transfer :: Service -> Run -> Text -> Text -> Value -> IO Text
transfer s run request reason record = do
 (w,c) <- getWork s (runId run)
 DB.requireOccurrence s (workField w "origin")
 stopped <- readIORef (runCancel run)
 require (not stopped) 409 "Stopped before handoff acceptance"
 let target=text (callField c "target")
 validateTarget s w (agentIdText (runOwner run)) target
 (scope,spec)<-validated (handoffSpecification w c (get record "response"))
 i <- newId
 chief <- DB.mainAgent s
 let child=setCallFields [("handoffReason",str reason),("scope",str scope),("specification",toJSON spec)] (newCall (RunId i) (AgentId target) (Just (callIdentity c)) (QueuedAt (if target==chief then ChiefTriage else Assessing)) request)
     reply="Delegated to "<>target<>"."
     updated=c { callProgress=FinishedCall (HandedOff reply) }
     responsible=if scope=="whole" then AgentId target else AgentId (text (workField w "responsible" `fallback` toJSON (callOwner c)))
     accepted=setWorkField "responsible" (toJSON responsible) (withCall w updated)
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
data PreparedCodeReady = PreparedCodeReady deriving (Show)
instance Exception PreparedCodeReady

data WorkflowPlan = ComposeVersion (Maybe CandidateReference) | ExecuteVersion CandidateReference deriving (Eq,Show)
data WorkflowSelected = WorkflowSelected Text WorkflowPlan Value deriving (Show)
instance Exception WorkflowSelected

standalonePin :: Workload -> WorkCall -> TaskSpec -> IO StandaloneTaskPin
standalonePin work call spec=do
 binding<-decodeValue 409 (callField call "executionBinding")
 decision<-case bindingContract binding of
  (owner,TaskDecisionSource identity) | owner==callOwner call->pure identity
  _->failure 409 "Standalone composition requires an accepted ordinary Task"
 require (not (has (workField work "origin") "routine") && not (has (workField work "origin") "group")) 409 "Standalone composition requires a personal ordinary Task"
 pure (StandaloneTaskPin (callOwner call) (workloadId work) (callIdentity call) decision (canonicalHash (toJSON spec)))

acceptedInput :: TaskSpec -> Value
acceptedInput spec=case specInputs spec of
 []->Null
 [raw]->either (const (str raw)) id (eitherDecodeStrict (TE.encodeUtf8 raw))
 values->toJSON values
data DecisionError
 = AcceptedTaskNeedsExecution | OnlyChiefCreates | CriteriaRemoved
 | InvalidOutcome OutcomeError | InvalidAcceptedTask Text
 deriving (Eq, Show)
data DecisionStep
 = SaveDecision Workload WorkCall
 | ResolveSpecialist Text
 | EnsureSpecialist Text Text
 | DelegateRequest Text
 | PrepareAcceptedTask Text TaskSpec
 | AssessReportedOutcome Text Outcome
 | ComposeWorkflow Text WorkflowPlan

 deriving (Eq, Show)

-- Decide from an immutable snapshot. The runtime interprets only the resulting work.
advanceDecision :: AgentId -> DecisionResponse -> Workload -> WorkCall -> Either DecisionError DecisionStep
advanceDecision chief response work call = case decisionBody response of
 ReplyNow reply
  | workTracked work -> Left AcceptedTaskNeedsExecution
  | otherwise -> Right (finish work (FinishedCall (Answered reply)))
 ExecuteTask -> Right (finish (work { workTracked=True }) (RunningAt ReadyToWork))
 OutsideSpecialization -> Right $ SaveDecision work $
  if callOwner call == chief then call { callProgress=RunningAt ChiefTriage }
  else (setCallFields [("target",toJSON chief)] call) { callProgress=RunningAt Delegation }
 SelectSpecialist name -> Right (ResolveSpecialist name)
 NeedSpecialist -> Right (finish work (RunningAt CreatingSapi))
 AskAdmin question -> Right (finish (work { workTracked=True }) (FinishedCall (NeedsClarification question)))
 SpecifySapi name role
  | callOwner call /= chief -> Left OnlyChiefCreates
  | otherwise -> Right (EnsureSpecialist name role)
 PrepareTask taskType spec
  | any (`notElem` map str (NE.toList (specCriteria spec))) acceptedCriteria -> Left CriteriaRemoved
  | otherwise -> Right (PrepareAcceptedTask taskType spec)
 Handoff request -> Right (DelegateRequest request)
 ReportOutcome reply outcome -> Right (AssessReportedOutcome reply outcome)
 ChooseWorkflow operation plan -> Right (ComposeWorkflow operation plan)
 where
 finish changed progress = SaveDecision changed (call { callProgress=progress })
 acceptedCriteria = items (assignmentSpecification work call) "completionCriteria"

decisionFailure :: DecisionError -> IO a
decisionFailure problem = case problem of
 AcceptedTaskNeedsExecution -> failure 409 "An accepted Task must execute and report its completion criteria"
 OnlyChiefCreates -> failure 403 "Only Chief can create a Sapi"
 CriteriaRemoved -> failure 400 "A handoff cannot remove accepted completion criteria"
 InvalidOutcome reason -> failure 400 (outcomeError reason)
 InvalidAcceptedTask reason -> failure 500 reason

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
             finish work updated = do
              let routed=if callNode updated==Delegation && get (workField work "task") "specification"==Null then (setCallFields [("afterPreparation",str "Delegation")] updated) {callProgress=RunningAt ReadyToWork} else updated
                  following=setCallFields [("observation",decisionRaw response)] routed
                  next=callNode following
              saveWork s (withCall work following)
              accept s owner record (wireName node) (wireName next)
              pure (callOutput following)
         when (Effects.profileBound (toJSON current)) $ require (at (decisionRaw response) "event" `elem` ["Outcome","TaskPrepared","RequestUnclear"]) 409 "This response-only content profile does not support delegation or tool-capable follow-up"
         step <- either decisionFailure pure (advanceDecision (AgentId chief) response current call)
         case step of
          SaveDecision work updated -> finish work updated
          ComposeWorkflow operation reference -> do
           spec<-executionContract current call >>= maybe (failure 409 "Accept Task criteria before requesting Composer") pure
           void (standalonePin current call spec)
           let attempts=[entry | entry<-workItems current "decisions",at entry "node"=="Execution"]
           if length (items (toJSON turn) "batch_members")<=1 && not (null attempts) && all (Effects.completeActionFree . (`get` "evidence")) attempts then throwIO (WorkflowSelected operation reference record) else do
            let reason="Composer handoff refused: a multi-input batch, prior business actions or incomplete provider evidence cannot be replayed. Continue from retained work without repeating effects."
                refused=put "error" (str reason) record
            recordDecision s owner refused
            saveWork s (withCall (withDecisionRecord refused current) (setCallFields [("observation",obj [("workflowRefused",str reason),("decision",get record "decisionId")])] call))
            pure Nothing
          ResolveSpecialist name -> do
           target <- resolve s name False
           validateTarget s current owner target
           finish current ((setCallFields [("target",str target)] call) { callProgress=RunningAt Delegation })
          EnsureSpecialist name role -> do
           rows <- DB.agents s
           created <- case find ((==T.toCaseFold name) . T.toCaseFold . (`at` "name")) rows of
            Just row -> require (at row "role" == role) 409 "Name already exists with a different role" >> pure row
            Nothing -> createAgent s (obj [("name",str name),("role",str role),("manager",str owner)])
           finish current ((setCallFields [("target",get created "id"),("creation",obj [("name",str name),("role",str role)]),("creationReceipt",obj [("agent",created),("created",Bool True)])] call) {callProgress=RunningAt Delegation})
          PrepareAcceptedTask taskType spec -> do
           let previous=assignmentSpecification current call
               root=get (workField current "task") "specification"
               next=if at (toJSON call) "afterPreparation"=="Delegation" then Delegation else Execution
           when (routineSpecPinned current && callParent call==Nothing) $ require (toJSON spec==previous) 409 "Routine goal, inputs and criteria are pinned for this run"
           when (previous/=Null) $ do
            pinned<-decodeValue 400 previous
            require (specCriteria spec==specCriteria pinned && specConditions spec==specConditions pinned) 400 "Assignment must preserve every accepted criterion and condition"
           let task=if root/=Null then workField current "task" else mergeFields [("taskType",str (if has (workField current "origin") "routine" then "automated" else taskType)),("specification",toJSON spec)] (workField current "task")
           require (callField call "executionBinding"==Null) 409 "Execution binding cannot be replaced"
           let binding=OrdinaryBinding (callOwner call) (TaskDecisionSource (at record "decisionId"))
               following=(setCallFields [("executionBinding",toJSON binding),("specification",toJSON spec),("afterPreparation",Null),("observation",decisionRaw response)] call) {callProgress=RunningAt next}
               acceptedRecord=acceptedDecision record "ReadyToWork" (wireName next)
               committed=withDecisionRecord acceptedRecord (setWorkField "task" task current)
           selected<-if workField current "standaloneRun"==Null then pure (withCall committed following) else do
            let request=workField current "standaloneRun"
            original<-decodeValue 409 (get request "contract")
            require (sameSemanticContract spec original && acceptedInput spec==get request "input") 409 "Prepared Task changed the original criteria or authorized input"
            DB.connection s (\db->SQL.withTransaction db (bindStandaloneOn db (withCall committed following) following request))
           DB.connection s $ \db->SQL.withTransaction db $ do
            rememberRoutineTaskOn db selected following spec (at record "decisionId")
            saveWorkOn s db selected
           writeJSON (archivePath s owner ("decisions/"<>at record "decisionId")) acceptedRecord
           pure Nothing
          DelegateRequest request -> Just <$> transfer s run request (decisionReason response) record
          AssessReportedOutcome reply outcome -> do
           when (at (toJSON call) "scope"=="part") $ Effects.requireRunClear s (runId run)
           pinnedContract <- executionContract current call
           spec <- maybe (decodeValue 500 (assignmentSpecification current call)) pure pinnedContract
           let validation = validateOutcome response outcome spec
           case validation of
            Left _ -> do
             let assessment=put "decisionId" (get record "decisionId") (assessmentValue AssessmentRejected "application_validation" "Outcome did not satisfy the pinned criteria")
             saveWork s (setAssessment current record assessment)
             failure 400 "Outcome did not satisfy the pinned completion criteria"
            Right () -> pure ()
           let saved=mergeFields [("reply",str reply),("evidence",toJSON (decisionEvidence response)),("author",str owner)] (get (decisionRaw response) "outcome")
           let assessment=put "decisionId" (get record "decisionId") (assessmentValue (if outcomeStatus outcome==Completed then AssessmentAccepted else AssessmentUnresolved) "model_contract_assessment" "Model assessment against the pinned criteria; external effects are not independently verified")
           continuation<-RunId <$> newId
           let completedCall=setCallFields [("outcome",saved)] call { callProgress=FinishedCall (case outcomeStatus outcome of Completed -> Answered reply; Unresolved -> UnresolvedResult reply) }
               assessed=setAssessment (scopedResult continuation current completedCall saved) record assessment
           if Effects.profileBound (toJSON current) && outcomeStatus outcome==Completed then do
            let acceptedRecord=acceptedDecision record (wireName node) "Outcome"
                following=setCallFields [("observation",decisionRaw response)] completedCall
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
               pure (withCall (setAssessment (setWorkField "outcome" conflictOutcome retained) record conflictAssessment) following {callProgress=FinishedCall (UnresolvedResult reply)})
             saveWorkOn s db acceptedWork
            writeJSON (archivePath s owner ("decisions/"<>at record "decisionId")) acceptedRecord
            pure (Just reply)
           else finish assessed completedCall
        (_,after)<-getWork s (turnId turn)
        when (at (callField after "executionBinding") "mode"=="script" && isNothing (callOutput after)) (throwIO PreparedCodeReady)
        maybe (loop (n+1)) pure result
 output <- loop (0::Int) `catch` (\(e::SomeException) -> do
  when (isJust (fromException e :: Maybe PreparedCodeReady)) (throwIO e)
  when (isJust (fromException e :: Maybe WorkflowSelected)) (throwIO e)
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
  permitted <- DB.occurrenceAllowed s (workField work "origin")
  let reconcile call
       | callState call==CallCancelled = Right call
       | not permitted && callState call `elem` [CallQueued,CallRunning,CallWaiting] = Right (call {callProgress=CancelledAt (callNode call)})
       | otherwise = reconcileCall turns call
  updated <- validated (traverse reconcile (workCalls work))
  when (updated /= workCalls work) $ saveWork s work { workCalls=updated }
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
 require (callNode previous==WaitingForAdmin && isJust (callOutput previous) && callState previous == CallWaiting && a==chief) 409 "Clarification requires the waiting Chief call"
 i <- newId
 let call=setCallFields [("activation",str "RequestClarified")] (newCall (RunId i) (AgentId a) (Just (callIdentity previous)) (QueuedAt ChiefTriage) message)
 let consumed c=if callIdentity c==callIdentity previous then c {callProgress=FinishedCall (HandedOff (fromMaybe "" (callOutput c)))} else c
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
  pure $ mergeFields [("executionBinding",binding),("execution",obj [("state",toJSON (callState lastCall)),("owner",toJSON (callOwner lastCall)),("currentCall",toJSON (callIdentity lastCall)),("attempt",callField lastCall "attempt")]),("decisions",arr decisions)] (select ["task","workloadId","originalIntent","calls","outcome","standaloneRun","standaloneResult","standaloneHistory","workflowWaits"] (toJSON w))
 pure (obj [("tasks",arr rows)])
taskList :: Service -> Text -> IO Value
taskList s a = do
 tasks <- items <$> workTasks s a False <*> pure "tasks"
 pure $ obj [("tasks",arr (reverse [let body=get (get t "task") "specification" `fallback` obj [("objective",get t "originalIntent")]; execution=get t "execution"; calls=items t "calls" in obj [("id",get t "workloadId"),("title",get body "objective"),("taskType",get (get t "task") "taskType"),("state",get execution "state"),("owner",get execution "owner"),("sender",maybe Null (`get` "addressedTo") (listToMaybe calls)),("body",str (jsonText body <> "\n")),("result",get (get t "outcome") "reply"),("error",maybe Null (`get` "error") (listToMaybe (reverse calls)))] | t <- tasks, get t "standaloneRun"==Null]))]
workSummaries :: [Workload] -> [Value]
workSummaries works = [let c=NE.last (workCalls w) in obj [("taskType",get (workField w "task") "taskType"),("id",str (workloadId w)),("taskId",get (workField w "task") "taskId"),("origin",workField w "origin"),("callId",toJSON (callIdentity c)),("owner",toJSON (callOwner c)),("state",toJSON (callState c)),("participants",toJSON (nub (map callOwner (NE.toList (workCalls w))))),("output",toJSON (callOutput c)),("error",callField c "error"),("decisions",num (length (workItems w "decisions"))),("calls",num (length (workCalls w)))] | w <- works,workTracked w]
