{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Composer (preparationAuthority, repairOn, requestOn, stopOn, detail, projection, run, runWorkflowChild, recover, chooseStandalone, resumeStandalone, standaloneSnapshot ) where

import Control.Exception
import Control.Monad
import Data.Aeson hiding (pairs)
import Data.Aeson.Types (Parser,parseEither)
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Aeson.Key as Key
import qualified Data.ByteString.Lazy as BL
import Data.IORef
import Data.List (find)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString as B
import qualified Database.SQLite.Simple as SQL
import Data.Time.Clock (getCurrentTime,addUTCTime)
import GHC.Clock
import Types
import Population (requireActive)
import qualified Store as DB
import qualified ManagedState as Managed
import qualified Delegation as D
import qualified Routines as R
import qualified Provider
import qualified GeneratedCode as Code
import qualified Kit
import qualified Workflow
import qualified WorkflowCalls as WC
import qualified Effects

-- Stages have different admission/evidence requirements; a successful process
-- exit is deliberately not a preparation state.
data Phase = Fixtures | Generation | Environment | Checks | Review | AwaitingOccurrence deriving (Eq,Show)
phaseName :: Phase -> Text
phaseName value = case value of Fixtures->"fixtures"; Generation->"generation"; Environment->"environment"; Checks->"checks"; Review->"review"; AwaitingOccurrence->"awaiting_occurrence"

data Fixture = Fixture Value Value Value deriving (Eq,Show)
instance FromJSON Fixture where
 parseJSON value = do
  input <- withObject "Fixture" (.: "input") value
  expected <- withObject "Fixture" (.: "expected") value
  unless (BL.length (encode input)<=65536) (fail "Fixture input exceeds 64 KiB")
  unless ((at expected "status"=="ok" && keys expected `allIn` ["status","value"] && has expected "value")
    || (at expected "status"=="rejected" && keys expected `allIn` ["status","error"] && not (T.null (at expected "error")))) (fail "Independent expected result is invalid")
  pure (Fixture input expected (get value "modelStubs"))
  where allIn values allowed=all (`elem` allowed) values
instance ToJSON Fixture where
 toJSON (Fixture input expected stubs)=obj ([("input",input),("expected",expected)]++[("modelStubs",stubs) | stubs/=Null])

data Candidate = Candidate Text deriving (Eq,Show)
instance FromJSON Candidate where
 parseJSON value | has value "workflow" = do
  onlyFields ["workflow"] value
  either (fail . T.unpack) pure (Workflow.validate (get value "workflow"))
  pure (Candidate "")
 parseJSON value = do
  source <- withObject "Candidate" (.: "source") value :: Parser Text
  unless (keys value `allIn` ["source","language","profile","interface","dependencies","capabilities","explanation"]
    && at value "language"=="python" && at value "profile"=="python-3.13.15-linux-arm64"
    && at value "interface"=="json-stdin-result-v1" && get value "dependencies"==arr [] && get value "capabilities"==arr []) (fail "Unsupported candidate runtime, dependency or interface")
  unless (not (T.null source) && B.length (TE.encodeUtf8 source)<=65536) (fail "Candidate source must contain 1–65536 UTF8 bytes")
  when (has value "explanation") $ do
   onlyFields ["instruction"] (get value "explanation")
   void (textFieldP "instruction" 16000 (get value "explanation"))
  pure (Candidate source)
  where allIn values allowed=all (`elem` allowed) values

readOn :: SQL.Connection -> IO [D.Workload]
readOn db = DB.queryRowsOn db "SELECT value FROM workloads ORDER BY rowid" ["value"] [] >>= traverse (DB.decodeStored . (`at` "value") >=> decodeValue 500)
consumer :: R.Definition -> D.Workload -> Bool
consumer definition work = let pin=D.workField work "preparationPin" in at pin "routine"==R.definitionId definition && at pin "owner"==R.definitionOwner definition
-- Explicit re-preparation can learn from a rejected version under the same
-- evidence pin. Interrupted or uncertain provider work is never a retry source.
rejectedPreparation :: D.Workload -> Bool
rejectedPreparation work =
 let prepared=D.workField work "preparation"
     modelOperations=filter (\operation->has operation "node") (items prepared "operations")
     certain operation=let evidence=Provider.operationEvidence operation in not (null evidence) && all Effects.completeActionFree evidence
 in at prepared "status"=="failed" && D.workField work "executionGate"==toJSON ExecutionOpen
    && all ((==D.CallFailed) . D.callState) (D.workCalls work)
    && has prepared "version" && not (null modelOperations) && all certain modelOperations
    && (get (get prepared "review") "accepted"==Bool False || any ((==Bool False) . (`get` "matched")) (items prepared "checks"))

previousVersionFields :: AgentId -> D.Workload -> Value -> IO [(Text,Value)]
previousVersionFields owner work context = do
 reference<-decodeValue 409 (get (get (D.workField work "preparation") "version") "reference")
 version<-D.resolveCandidateBytes owner work reference
 pure [("previousVersion",version),("repairContext",context)]

-- Callers prove their own authority; construction and enrollment are shared.
enqueuePreparationOn :: SQL.Connection -> AgentId -> RunId -> Text -> [(Text,Value)] -> Value -> IO D.Workload
enqueuePreparationOn db owner identity time fields prepared = do
 let origin=fromMaybe (obj [("preparation",toJSON identity)]) (lookup "origin" fields)
     goal=at (get prepared "contract") "objective"
     work=D.Workload (runIdText identity) False (D.newCall identity owner Nothing (D.QueuedAt D.Preparation) goal :| [])
       (KM.fromList [(Key.fromText key,value) | (key,value)<-[("origin",origin),("created",str time),("originalIntent",str goal),("executionGate",toJSON ExecutionOpen),("preparation",prepared)]++[(key,value) | (key,value)<-fields,key/="origin"]])
 SQL.execute db "INSERT INTO workloads(id,value) VALUES (?,?)" (D.workloadId work,jsonText (toJSON work))
 Managed.enrollOn db (agentIdText owner) (newTurn identity Chat goal time origin False) Null goal []
 pure work

requestOn :: SQL.Connection -> R.Definition -> D.Workload -> Value -> RunId -> Text -> IO Value
requestOn db definition original context identity time = do
 works<-readOn db
 require (not (any D.generatedCleanupPending works)) 503 "Owned container cleanup remains unresolved"
 pin<-decodeValue 409 (get context "pin") :: IO AnalysisPin
 require (analysisOwner pin==AgentId (R.definitionOwner definition) && analysisRoutine pin==R.definitionId definition && analysisGeneration pin==R.contractGeneration definition && not (R.definitionDeleted definition)) 409 "Preparation contract changed"
 require (DB.sameRoutineContract (toJSON definition) (toJSON original)) 409 "Preparation needs the accepted current task contract"
 contract<-D.executionContract original (NE.head (D.workCalls original)) >>= maybe (failure 409 "Preparation requires an accepted pinned source task") pure
 sourceInput<-if get (toJSON definition) "externalProfile"==Null then pure Null else Effects.preparationInput (toJSON definition) (toJSON original)
 let lineage=[work | work<-works,consumer definition work,get (D.workField work "preparationPin") "contractGeneration"==toJSON (R.contractGeneration definition),not (has (toJSON work) "repairPin")]
     root=listToMaybe lineage
     rootId=maybe (runIdText identity) (\work->at (toJSON work) "preparationBudgetRoot" `orText` D.workloadId work) root
     authority=find ((==rootId) . D.workloadId) works
 require (all ((/=toJSON ExecutionStopped) . (`D.workField` "executionGate")) lineage) 409 "This preparation lineage was stopped; a new request cannot revive it"
 forM_ authority $ \work->do
  let budget=D.workField work "maintenanceBudget"
  require (D.workField work "executionGate"/=toJSON ExecutionStopped) 409 "This preparation lineage was stopped; a new request cannot revive it"
  require (not (boolean (get budget "usageUnknown")) && not (any ((`elem` ["reserved","uncertain"]) . (`at` "status")) (items budget "reservations"))) 409 "Prior preparation resource remains active or uncertain"
  require (budget==Null || int (get budget "spentMilliseconds")<int (get budget "limitMilliseconds")) 409 "This contract's preparation allowance is exhausted"
 let previous=listToMaybe [work | work<-reverse works,D.workField work "preparationPin"==toJSON pin,get (D.workField work "preparation") "contract"==toJSON contract,rejectedPreparation work]
 previousFields<-case previous of
  Nothing->pure []
  Just work->previousVersionFields (analysisOwner pin) work (obj [("previousAttempt",str (D.workloadId work)),("lastFailure",select ["checks","review","error"] (D.workField work "preparation"))])
 let prepared=mergeFields previousFields (obj [("expectedBinding",Kit.bindingPin (toJSON definition)),("deferEnabled",Bool True),("parallelEnabled",Bool True),("workflowEnabled",Bool True),("kitEnabled",Bool True),("feedProfile",get (toJSON definition) "externalProfile"),("sourceInput",sourceInput),("status",str "queued"),("contract",toJSON contract),("sourceEvidence",get context "evidence"),("operations",arr []),("checks",arr [])])
 void (enqueuePreparationOn db (analysisOwner pin) identity time [("preparationPin",toJSON pin),("preparationKey",get context "key"),("preparationBudgetRoot",str rootId)] prepared)
 pure (obj [("routine",str (R.definitionId definition)),("preparation",toJSON identity),("status",str "queued")])
 where orText value fallback_=if T.null value then fallback_ else value

stopOn :: SQL.Connection -> R.Definition -> RunId -> IO Value
stopOn db definition identity = do
 works<-readOn db
 work<-maybe (failure 404 "Unknown preparation") pure (find (\w->consumer definition w && D.workloadId w==runIdText identity) works)
 require (any ((`elem` [D.CallQueued,D.CallRunning,D.CallWaiting]) . D.callState) (D.workCalls work)) 409 "Preparation is already finished"
 let stopped=D.setWorkField "executionGate" (toJSON ExecutionStopped) work
 DB.putValueOn db DB.Workloads (D.workloadId work) (toJSON stopped)
 pure (obj [("preparation",toJSON identity),("status",str "stopped")])

load :: Service -> Text -> IO D.Workload
load s identity = D.readWorks s >>= maybe (failure 404 "Unknown preparation") pure . find ((==identity) . D.workloadId)
-- Never save a stale full Workload over Stop, feedback or another checkpoint.
update :: Service -> Text -> (Value -> Value) -> IO ()
update s identity transform = locked s $ do
 work<-load s identity
 D.saveWork s (D.setWorkField "preparation" (transform (D.workField work "preparation")) work)
checkGate :: Service -> Run -> IO ()
checkGate s running = do
 requireActive s (agentIdText (runOwner running))
 cancelled<-readIORef (runCancel running)
 require (not cancelled) 409 "Preparation stopped"
 (work,_)<-D.getWork s (runId running)
 DB.requireOccurrence s (D.workField work "origin")
phase :: Service -> Run -> Phase -> IO ()
phase s running value = phaseChecked s running value (pure ())
phaseChecked :: Service -> Run -> Phase -> IO () -> IO ()
phaseChecked s running value validate = locked s $ do
 validate
 checkGate s running
 work<-load s (runIdText (runId running))
 let changed=D.setWorkField "preparation" (put "status" (str (phaseName value)) (D.workField work "preparation")) work
     promoted=if value==AwaitingOccurrence then changed {D.workCalls=fmap (\call->call {D.callProgress=D.FinishedCall (D.Answered "Candidate awaits an authorized occurrence")}) (D.workCalls changed)} else changed
 DB.connection s $ \db->SQL.withTransaction db $ do
  DB.putValueOn db DB.Workloads (D.workloadId promoted) (toJSON promoted)
  when (value==AwaitingOccurrence && D.workField promoted "standalonePin"/=Null) (void (queueStandaloneOn db promoted))

ask :: Service -> Run -> Text -> Text -> Value -> IO Value
ask s running node purpose context = snd <$> askConfigured s running node purpose context Nothing

askConfigured :: Service -> Run -> Text -> Text -> Value -> Maybe Explanation -> IO (Text,Value)
askConfigured s running node purpose context explanation = do
 let settings=explanationModel <$> explanation
 checkGate s running
 (template,envelope)<-case explanation of
  Just selected->Provider.explanationTemplates s selected
  Nothing->(,) <$> prompt s (case node of "PreparationFixtures"->"preparation-fixtures"; "PreparationGeneration"->if has context "availableKit" then "preparation-generation-kit" else "preparation-generation"; "PreparationRubricReview"->"hybrid-rubric-review"; _->"preparation-review") <*> prompt s "preparation-envelope"
 workflowInstruction<-if boolean (get context "workflowEnabled") && node `elem` ["PreparationGeneration","PreparationFixtures"] then prompt s (if boolean (get context "parallelEnabled") then (if boolean (get context "deferEnabled") then "preparation-workflow-defer" else "preparation-workflow-parallel") else "preparation-workflow") else pure ""
 let rendered=render envelope [("node",node),("instruction",template<>workflowInstruction),("context",jsonText (put "selfId" (toJSON (runOwner running)) context))]
 owning<-load s (runIdText (runId running))
 let recordedPurpose=if has (toJSON owning) "repairPin" then "repair_"<>purpose else purpose
     stage=node<>":"<>canonicalHash context
 operation<-Provider.operationFor s running node recordedPurpose stage rendered settings (items (D.workField owning "preparation") "operations")
 let identity=at operation "id"
     saveOperation value=locked s $ do
      current<-load s (runIdText (runId running))
      let prepared=D.workField current "preparation"
          updated=D.setWorkField "preparation" (put "operations" (arr (filter ((/=identity) . (`at` "id")) (items prepared "operations")++[value])) prepared) current
          observed=if any Effects.positiveAnomaly (Provider.operationEvidence value) && D.workField updated "routineEffectUnknown"==Null then D.setWorkField "routineEffectUnknown" (obj [("decisionId",str identity),("reason",str "Preparation provider trace contains unexpected activity")]) updated else updated
      D.saveWork s observed
 saveOperation operation
 (_,result,())<-Provider.validatedResponse s running operation saveOperation $ \result->do
  require (case result of Object _->True; _->False) 422 "Preparation model response must be a JSON object"
  case node of
   "PreparationFixtures"->if at result "status" `elem` ["insufficient_evidence","unsuitable"] then void (parseValue 422 (textFieldP "reason" 8000) result) else void (field 422 result "fixtures" :: IO [Fixture])
   "PreparationGeneration"->if has result "reuse" && has context "availableKit" then do
    void (parseValue 422 (onlyFields ["reuse"]) result)
    void (decodeValue 422 (get result "reuse") :: IO CandidateReference)
    else void (decodeValue 422 result :: IO Candidate)
   "PreparationReview"->do
    void (field 422 result "accepted" :: IO Bool)
    void (parseValue 422 (textFieldP "reason" 8000) result)
   "PreparationRubricReview"->do
    current<-load s (runIdText (runId running))
    spec<-decodeValue 422 (get (D.workField current "preparation") "contract")
    void (D.criterionAssessments spec (get result "criteria"))
   "PreparationExplanation"->void (either (failure 422 . T.pack) pure (parseEither narrativeText result))
   _->pure ()
 update s (runIdText (runId running)) (\p->put "operations" (arr [if at op "id"==identity then mergeFields [("response",result)] op else op | op<-items p "operations"]) p)
 checkGate s running
 pure (identity,result)

run :: Service -> Run -> Turn -> IO (Text,[Value])
run s running _ = do
 let identity=runIdText (runId running)
 outcome<-try (prepare s running) :: IO (Either SomeException ())
 case outcome of
  Right () -> do
   current<-load s identity
   let prepared=D.workField current "preparation"
   pure (if at prepared "status" `elem` ["insufficient_evidence","unsuitable"] then at prepared "reason" else "Candidate awaits an authorized occurrence",[])
  Left exception | Just (_::Workflow.Yield)<-fromException exception -> do
   update s identity (put "status" (str "waiting_children"))
   throwIO exception
  Left exception | Just (_::Provider.RetryDeferred)<-fromException exception -> do
   update s identity (put "status" (str "waiting_provider"))
   throwIO exception
  Left exception -> do
   cancelled<-readIORef (runCancel running)
   permitted<-DB.occurrenceAllowed s (obj [("preparation",str identity)])
   update s identity (\p->let container=get p "container"; uncertain=has container "name" && at container "status"/="removed" in mergeFields [("status",str (if uncertain then "cleanup_pending" else if cancelled then "stopped" else if not permitted then "stale" else "failed")),("error",str (D.exceptionText exception))] p)
   throwIO exception

environment :: Service -> Run -> IO Code.Profile
environment s running = do
 phase s running Environment
 identity<-newId
 began<-getMonotonicTimeNSec
 let save fields=update s (runIdText (runId running)) (\p->put "operations" (arr [if at op "id"==identity then merge op fields else op | op<-items p "operations"]) p)
 update s (runIdText (runId running)) (appendItems "operations" [obj [("id",str identity),("purpose",str "environment"),("status",str "running")]])
 result<-try Code.available :: IO (Either APIError Code.Profile)
 ended<-getMonotonicTimeNSec
 let timing=[("durationMs",toJSON ((ended-began) `div` 1000000))]
 case result of
  Left problem@(APIError _ reason)->save (obj (timing++[("status",str "failed"),("error",str reason)])) >> throwIO problem
  Right profile->save (obj (timing++[("status",str "completed"),("runtime",Code.profileJSON profile)])) >> pure profile

requireCleanRuntime :: Service -> IO ()
requireCleanRuntime s = do
 works<-D.readWorks s
 require (not (any D.generatedCleanupPending works)) 503 "Owned container cleanup remains unresolved; restore the runtime and restart the app"

prepare :: Service -> Run -> IO ()
prepare s running = do
 let identity=runIdText (runId running)
 work<-load s identity
 let saved=D.workField work "preparation"
 require (at saved "status" `elem` ["queued","waiting_provider","waiting_children"]) 409 "Preparation already started; request a new explicit attempt"
 spec<-decodeValue 422 (get saved "contract")
 phase s running Fixtures
 oracle<-if has saved "fixtures" then pure (obj [("fixtures",get saved "sourceCases"),("contentInterface",get saved "contentInterface"),("rubric",get saved "rubric")]) else ask s running "PreparationFixtures" "fixture_design" (put "criteria" (D.criterionContext spec) (mergeFields (pairs (selectPresent ["workflowEnabled","parallelEnabled","deferEnabled"] saved)) (select ["contract","sourceEvidence","parameters","feedProfile","sourceInput","repairContext"] saved)))
 if at oracle "status" `elem` ["insufficient_evidence","unsuitable"] then do
  locked s $ do
   checkGate s running
   latest<-load s identity
   let completed=D.setWorkField "preparation" (mergeFields [("status",get oracle "status"),("reason",get oracle "reason")] (D.workField latest "preparation")) latest
   D.saveWork s (completed {D.workCalls=fmap (\call->call {D.callProgress=D.FinishedCall (D.Answered (at oracle "reason"))}) (D.workCalls completed)})
 else do
  rawFixtures<-field 422 oracle "fixtures" :: IO [Fixture]
  feedMode<-if get saved "feedProfile"==Null then pure Nothing else Just <$> field 422 oracle "contentInterface"
  typedFixtures<-forM rawFixtures $ \(Fixture input expected stubs)->case feedMode of
   Nothing->pure (Fixture input expected stubs)
   Just content->do
    profile<-decodeValue 422 (get saved "feedProfile")
    normalized<-parseValue 422 (Effects.fixtureInput profile) input
    when (content==ScriptText && at expected "status"=="ok") (void (parseValue 422 feedContentText (get expected "value")))
    pure (Fixture normalized expected stubs)
  let fixtures=map toJSON typedFixtures
      observedInputs=get (get (get saved "sourceEvidence") "source") "inputs"
      observedJSON=if get saved "sourceInput"/=Null then get (get saved "sourceInput") "input" else case observedInputs of String input->parseJSONText input; _->observedInputs
  when (has saved "fixtures") (require (get saved "fixtures"==arr fixtures) 409 "Retained fixture design changed")
  require (not (null fixtures) && any (\fixture->get fixture "input"/=observedJSON) fixtures) 422 "Preparation requires independent examples on new inputs"
  when (get oracle "rubric"/=Null) (void (D.candidateRubric spec (get oracle "rubric")))
  when (feedMode==Just HybridText) (void (D.candidateRubric spec (get oracle "rubric")))
  update s identity (mergeFields [("fixtures",arr fixtures),("sourceCases",toJSON rawFixtures),("contentInterface",maybe Null toJSON feedMode),("rubric",get oracle "rubric")])
  profile<-environment s running
  phase s running Generation
  let target=mergeFields [("contentInterface",maybe Null toJSON feedMode),("rubric",get oracle "rubric")] saved
  catalog_<-if not (boolean (get saved "kitEnabled")) then pure Null else if has saved "kitCatalog" then pure (get saved "kitCatalog") else do
   works<-map toJSON <$> D.readWorks s
   selected<-Kit.catalog (agentIdText (runOwner running)) target works
   update s identity (put "kitCatalog" selected)
   pure selected
  produced<-if has (get saved "version") "workflow" then pure (select ["workflow"] (get saved "version")) else if has saved "version" then pure (mergeFields [("profile",get (get (get saved "version") "runtime") "profile"),("explanation",select ["instruction"] (get (get saved "version") "explanation"))] (select ["source","language","interface","dependencies","capabilities"] (get saved "version"))) else ask s running "PreparationGeneration" "candidate_generation" (mergeFields ([("availableProfiles",arr [Code.profileJSON profile]),("interface",str "json-stdin-result-v1"),("contentInterface",maybe Null toJSON feedMode)]++[("availableKit",catalog_) | catalog_/=Null]) (mergeFields (pairs (selectPresent ["workflowEnabled","parallelEnabled","deferEnabled"] saved)) (select ["contract","sourceEvidence","parameters","feedProfile","sourceInput","repairContext","previousVersion"] saved)))
  (candidate,kitFields)<-if has saved "version" then pure (produced,pairs (selectPresent ["kitArtifact"] (get saved "version"))) else if not (has produced "reuse") then pure (produced,[]) else do
   checkGate s running
   require (any (\entry->get entry "reference"==get produced "reuse" && get entry "compatible"==Bool True) (items catalog_ "entries")) 409 "Selected Kit artifact is not an applicable entry in this preparation's catalog"
   works<-map toJSON <$> D.readWorks s
   (reused,artifact)<-Kit.resolve (agentIdText (runOwner running)) target (Code.profileJSON profile) works (get produced "reuse")
   instruction<-if has reused "explanation" then Just <$> prompt s "kit-explanation" else pure Nothing
   let candidate=mergeFields ([("profile",get (get reused "runtime") "profile")]++[("explanation",obj [("instruction",str text_)]) | text_<-maybeToList instruction]) (select ["source","language","interface","dependencies","capabilities"] reused)
   pure (candidate,[("kitArtifact",artifact)])
  let selectedCandidate=if get (get saved "version") "explanation"==Null && has saved "version" then remove ["explanation"] candidate else candidate
  Candidate source<-decodeValue 422 selectedCandidate
  when (has candidate "workflow") (require (isNothing feedMode) 422 "Prepared workflow feed composition is not supported")
  forM_ feedMode $ \content->require ((content==HybridText)==has selectedCandidate "explanation") 422 "Generated implementation differs from the frozen feed content interface"
  hybridFields<-if has saved "version" then pure (pairs (selectPresent ["composition","structureHash","rubric","explanation"] (get saved "version"))) else if not (has candidate "explanation") then pure [] else do
   rubric<-D.candidateRubric spec (get oracle "rubric")
   settings<-Provider.verifiedResponseSettings s running
   template<-prompt s "hybrid-explanation"
   envelope<-prompt s "preparation-envelope"
   let explanation=mergeFields [("templateHash",str (sourceHash template)),("envelopeHash",str (sourceHash envelope)),("model",toJSON settings),("policy",str "response_only")] (get candidate "explanation")
   pure [("composition",hybridComposition),("structureHash",str (canonicalHash hybridComposition)),("rubric",toJSON rubric),("explanation",put "implementationHash" (str (canonicalHash explanation)) explanation)]
  workflowFields<-if not (has candidate "workflow") then pure [] else if has saved "version" then pure (pairs (select ["workflowModel","workflowPrompt","structureHash"] (get saved "version"))) else do
   settings<-Provider.verifiedResponseSettings s running
   template<-prompt s "workflow-step"
   pure [("workflowPrompt",str template),("workflowModel",toJSON settings),("structureHash",str (Workflow.contentHash (get candidate "workflow")))]
  feedFields<-case feedMode of
   Nothing->pure []
   Just content->do
    profile<-decodeValue 422 (get saved "feedProfile")
    revision<-field 422 (get saved "sourceInput") "externalProfileRevision"
    pure [("inputBinding",toJSON (feedDescriptor profile revision content))]
  let content=mergeFields (pairs (selectPresent ["expectedBinding"] saved)++[("standalonePin",D.workField work "standalonePin") | D.workField work "standalonePin"/=Null]++kitFields++feedFields++hybridFields++workflowFields++ (if has candidate "workflow" then [("interface",str "json-stdin-result-v1"),("language",str "workflow"),("dependencies",arr []),("capabilities",arr [])] else [])++[("runtime",Code.profileJSON profile),("contract",get saved "contract"),("contractGeneration",get (D.workField work "preparationPin") "contractGeneration"),("sourceHash",str (if has candidate "workflow" then Workflow.contentHash (get candidate "workflow") else sourceHash source))]) (if has candidate "workflow" then select ["workflow"] candidate else select ["source","language","interface","dependencies","capabilities"] candidate)
      reference=obj [("owner",toJSON (runOwner running)),("preparation",str identity),("digest",str (canonicalHash content))]
      version=put "reference" reference content
  when (has saved "version") (require (version==get saved "version") 409 "Retained candidate identity or stage inputs changed")
  if has produced "reuse" && not (has saved "version") then locked s $ do
   checkGate s running
   DB.connection s $ \db->SQL.withTransaction db $ do
    works<-readOn db
    current<-maybe (failure 409 "Target preparation disappeared before Kit attachment") pure (find ((==identity) . D.workloadId) works)
    require (D.workField current "executionGate"==toJSON ExecutionOpen) 409 "Target preparation stopped before Kit attachment"
    (_,fresh)<-Kit.resolve (agentIdText (runOwner running)) target (Code.profileJSON profile) (map toJSON works) (get produced "reuse")
    require (fresh==get content "kitArtifact" && at fresh "id"==canonicalHash (Kit.descriptor version)) 409 "Selected Kit artifact changed before attachment"
    DB.putValueOn db DB.Workloads identity (toJSON (D.setWorkField "preparation" (put "version" version (D.workField current "preparation")) current))
   else update s identity (put "version" version)
  checkedProfile<-environment s running
  require (Code.profileJSON checkedProfile==get content "runtime") 409 "Candidate runtime identity changed before verification"
  phase s running Checks
  forM_ fixtures $ \fixture->do
   checkGate s running
   requireCleanRuntime s
   stored<-resolveVersion s running reference
   currentFixture<-load s identity
   let retained=find (\check->get check "inputHash"==str (canonicalHash (get fixture "input")) && get check "version"==reference && get check "sourceHash"==get stored "sourceHash" && get check "expected"==get fixture "expected" && get check "actual"==get fixture "expected" && at check "status"=="exited" && get check "exitCode"==num 0 && not (boolean (get check "wasRunning")) && get check "matched"==Bool True) (items (D.workField currentFixture "preparation") "checks")
   require (isJust retained || not (any ((==str (canonicalHash (get fixture "input"))) . (`get` "inputHash")) (items (D.workField currentFixture "preparation") "checks"))) 409 "Retained fixture receipt no longer matches the exact expected and actual result"
   (actual,receipt)<-case retained of
    Just receipt->pure (get receipt "actual",receipt)
    Nothing->do
     nonce<-newId
     let reserved=Code.reservation checkedProfile (agentIdText (runOwner running)) identity nonce
     started<-now
     let operation=obj [("id",str nonce),("purpose",str "fixture_check"),("status",str "running"),("started",str started),("inputHash",str (canonicalHash (get fixture "input"))),("sourceHash",get stored "sourceHash"),("version",reference),("durationMs",Null)]
         saveOperation fields=update s identity (\p->put "operations" (arr [if at op "id"==nonce then merge op fields else op | op<-items p "operations"]) p)
     update s identity ((if has stored "workflow" then id else put "container" reserved) . appendItems "operations" [operation])
     result<-(if has stored "workflow" then workflowFixture s running checkedProfile stored fixture else Code.runFixture checkedProfile running reserved (at stored "source") (get fixture "input") (\receipt->do
       when (at receipt "status"/="removed") $ checkGate s running >> requireCleanRuntime s >> void (resolveVersion s running reference)
       update s identity (put "container" receipt))) `catch` (\(exception::SomeException)->do
        cancelled<-readIORef (runCancel running)
        permitted<-DB.occurrenceAllowed s (obj [("preparation",str identity)])
        saveOperation (obj [("status",str (if cancelled || not permitted then "interrupted" else "failed")),("error",str (D.exceptionText exception))])
        throwIO exception)
     let actual=parseJSONText (at result "output")
         matched=at result "status"=="exited" && get result "exitCode"==num 0 && not (boolean (get result "wasRunning")) && actual==get fixture "expected"
         receipt=mergeFields [("inputHash",str (canonicalHash (get fixture "input"))),("sourceHash",get stored "sourceHash"),("version",reference),("actual",actual),("expected",get fixture "expected"),("matched",Bool matched)] result
     update s identity (\p->put "operations" (arr [if at op "id"==nonce then merge op (put "status" (str (if matched then "completed" else if at result "status"=="stopped" then "interrupted" else "failed")) receipt) else op | op<-items p "operations"]) (appendItems "checks" [receipt] p))
     require matched 422 "Candidate failed independent fixture comparison"
     pure (actual,receipt)
   explanation<-either (failure 422 . T.pack) pure (parseEither candidateExplanation stored)
   forM_ explanation $ \selected->when (at actual "status"=="ok") $ do
    let context=mergeFields [("contract",toJSON spec) | has stored "kitArtifact"] (obj [("instruction",str (explanationInstruction selected)),("acceptedInput",get fixture "input"),("facts",get actual "value"),("criteria",D.criterionContext spec),("reference",reference),("implementationHash",get (get stored "explanation") "implementationHash")])
    (explainId,narrative)<-askConfigured s running "PreparationExplanation" "fixture_explanation" context (Just selected)
    let parsedNarrative=parseEither narrativeText narrative
    update s identity (\p->put "operations" (arr [if at op "id"==explainId then mergeFields [("status",str (either (const "failed") (const "completed") parsedNarrative))] op else op | op<-items p "operations"]) p)
    text_<-either (failure 422 . T.pack) pure parsedNarrative
    (reviewId,assessment)<-askConfigured s running "PreparationRubricReview" "fixture_semantic_review" (mergeFields [("rubric",get stored "rubric"),("expected",get fixture "expected"),("explanation",str text_)] context) Nothing
    verdicts<-D.criterionAssessments spec (get assessment "criteria")
    decision<-parseValue 422 (D.parseDecision D.Execution) (obj [("event",str "Outcome"),("reply",str text_),("reason",str "Independent hybrid fixture rubric"),("evidence",arr [str evidence | CriterionAssessment _ _ evidence<-verdicts]),("outcome",D.criteriaOutcome spec verdicts)])
    accepted<-case D.decisionBody decision of
     D.ReportOutcome _ outcome->either (failure 400 . D.outcomeError) pure (D.validateOutcome decision outcome spec) >> pure (D.outcomeStatus outcome==D.Completed)
     _->failure 422 "Hybrid rubric has no criterion outcome"
    update s identity (\p->put "checks" (arr [if get check "inputHash"==get receipt "inputHash" then mergeFields [("explanationOperation",str explainId),("rubricReviewOperation",str reviewId),("rubricAccepted",Bool accepted)] check else check | check<-items p "checks"]) p)
    require accepted 422 "Hybrid explanation failed independent original-criteria rubric"
  current<-load s identity
  when (has version "explanation") $ require (any (\check->get check "rubricAccepted"==Bool True && has check "explanationOperation") (items (D.workField current "preparation") "checks")) 422 "Hybrid preparation requires an actual successful explanation and independent rubric check"
  phase s running Review
  let prepared=D.workField current "preparation"
  review<-ask s running "PreparationReview" "candidate_review" (select ["contract","version","fixtures","checks"] prepared)
  update s identity (put "review" review)
  require (get review "accepted"==Bool True && not (T.null (T.strip (at review "reason")))) 422 "Independent candidate review did not accept the preparation"
  phaseChecked s running AwaitingOccurrence (void (resolveVersion s running reference))

workflowFixture :: Service -> Run -> Code.Profile -> Value -> Value -> IO Value
workflowFixture s running profile version fixture = do
 (own,call)<-D.getWork s (runId running)
 let identity=D.workloadId own
     key=canonicalHash (get fixture "input")
     context=obj [("fixture",fixture),("fixtureKey",str key)]
     loadTrace=do current<-load s identity; pure (get (get (D.workField current "preparation") "workflowFixtures") key `fallback` obj [])
     saveTrace node receipt=locked s $ do
      checkGate s running
      current<-load s identity
      let prepared=D.workField current "preparation"
      D.saveWork s (D.setWorkField "preparation" (put "workflowFixtures" (put key (put "nodes" (put node receipt (get (get (get prepared "workflowFixtures") key) "nodes" `fallback` obj [])) (get (get prepared "workflowFixtures") key `fallback` obj [])) (get prepared "workflowFixtures" `fallback` obj [])) prepared) current)
     invoke nodeId node input=case at node "kind" of
      "defer"->do
       wall<-getCurrentTime
       snd <$> either (failure 422) pure (Workflow.waitSpec wall (addUTCTime 1800 wall) input)
      "fork"->WC.fork s running version (get fixture "input") context nodeId node
      "model"->do
       let stubs=get fixture "modelStubs"
       require (has stubs nodeId) 422 "Independent fixture has no response-only model stub for the selected node"
       pure (get stubs nodeId)
      _->do
       checkGate s running
       nonce<-newId
       let reserved=Code.reservation profile (agentIdText (runOwner running)) identity nonce
       locked s $ do (current,_)<-D.getWork s (runId running); D.saveWork s (WC.container running reserved current)
       result<-Code.runFixture profile running reserved (at node "source") input (\receipt->do
        when (at receipt "status"/="removed") (checkGate s running)
        locked s $ do (current,_)<-D.getWork s (runId running); D.saveWork s (WC.container running receipt current))
       require (at result "status"=="exited" && get result "exitCode"==num 0 && not (boolean (get result "wasRunning"))) 422 "Workflow fixture script failed"
       either (failure 422 . T.pack) pure (eitherDecodeStrict (TE.encodeUtf8 (at result "output")))
 let entry=if WC.child call then at (WC.childData call) "entry" else at (get version "workflow") "entry"
     stop=if WC.child call then Just (at (WC.childData call) "join") else Nothing
 value<-Workflow.runRegion (get version "workflow") (get fixture "input") (if any ((=="defer") . (`at` "kind") . snd) (pairs (get (get version "workflow") "nodes")) then put "fixtureTimer" (Bool True) (select ["reference","workflowModel","workflowPrompt","runtime"] version) else select ["reference","workflowModel","workflowPrompt","runtime"] version) entry stop loadTrace saveTrace invoke
 pure (obj [("status",str "exited"),("exitCode",num 0),("wasRunning",Bool False),("output",str (jsonText value)),("workflow",Bool True)])

runWorkflowChild :: Service -> Run -> Turn -> IO (Text,[Value])
runWorkflowChild s running turn=do
 locked s (D.begin s (runOwner running) turn)
 (_,call)<-D.getWork s (runId running)
 let context=WC.childData call
 profile<-Code.available
 require (Code.profileJSON profile==get (get context "version") "runtime") 409 "Pinned child runtime changed"
 void (workflowFixture s running profile (get context "version") (get context "fixture"))
 locked s $ do
  WC.allowed s running
  (work,current)<-D.getWork s (runId running)
  D.saveWork s (D.withCall work (current {D.callProgress=D.FinishedCall (D.Answered "Completed pinned fixture region; canonical outputs retained in the parent trace")}))
 pure ("Pinned workflow child region completed",[])

resolveVersion :: Service -> Run -> Value -> IO Value
resolveVersion s running expected = do
 current<-load s (runIdText (runId running))
 reference<-decodeValue 409 expected
 D.resolveCandidateBytes (runOwner running) current reference

preparationAuthority :: Service -> D.Workload -> IO D.Workload
preparationAuthority s work =
 let source=at (D.workField work "standalonePin") "workload"
     root=if T.null source then at (toJSON work) "preparationBudgetRoot" else source
 in if T.null root || root==D.workloadId work then pure work else load s root

recover :: Service -> IO ()
recover s = do
 works<-D.readWorks s
 forM_ works $ \work->when (has (toJSON work) "preparation") $ do
  let identity=D.workloadId work; prepared=D.workField work "preparation"; container=get prepared "container"
  authority<-preparationAuthority s work
  let budget=D.workField authority "maintenanceBudget"
      own receipt=at receipt "status"=="reserved" && (at receipt "operation"==identity || get receipt "operation"==Null && D.workloadId authority==identity)
  when (not (WC.safeIdle work) && any own (items budget "reservations")) $
   D.saveWork s (D.setWorkField "maintenanceBudget" (mergeFields [("usageUnknown",Bool True),("reservations",arr [if own receipt then put "status" (str "uncertain") receipt else receipt | receipt<-items budget "reservations"])] budget) authority)

  when (has container "name" && at container "status"/="removed") $ do
   result<-try (Code.reconcile container) :: IO (Either APIError ())
   update s identity (\p->case result of
    Right ()->mergeFields [("container",put "status" (str "removed") container),("status",str "interrupted"),("error",str "Host stopped during preparation; owned container removed without replay")] p
    Left (APIError _ reason)->mergeFields [("status",str "cleanup_pending"),("error",str reason)] p)
  when (at prepared "status" `elem` ["fixtures","generation","environment","checks","review","cleanup_pending"]) $
   update s identity finalizeInterrupted
 where
 finalizeInterrupted prepared =
  let terminal=if at prepared "status"=="cleanup_pending" then "uncertain" else "interrupted"
      operations=[if at op "status" `elem` ["running","validating"] then put "status" (str terminal) op else op | op<-items prepared "operations"]
      status=if at prepared "status"=="cleanup_pending" then prepared else put "status" (str "interrupted") prepared
  in put "operations" (arr operations) status

detail :: Service -> Text -> Text -> Text -> IO Value
detail s owner routine identity = do
 work<-load s identity
 let pin=D.workField work "preparationPin"
 require (at pin "owner"==owner && at pin "routine"==routine) 404 "Unknown preparation in this Sapi context"
 renderDetail s work
projection :: Service -> R.Definition -> [D.Workload] -> IO Value
projection s definition works = do
 projected<-maybe (pure Null) (fmap project . renderDetail s) (listToMaybe (reverse (filter (consumer definition) works)))
 let notice=get (toJSON definition) "preparationNotice"
 pure (if notice==Null then projected else mergeFields (pairs (remove ["key"] notice)) projected)
 where
 project detail_ = mergeFields [("timerFixturesSimulated",Bool (any ((=="defer") . (`at` "kind") . snd) (pairs (get (get (get detail_ "version") "workflow") "nodes")))),("expectedBinding",get (get detail_ "version") "expectedBinding"),("kitArtifact",get (get detail_ "version") "kitArtifact"),("reference",get (get detail_ "version") "reference"),("applicable",Bool (get detail_ "contractGeneration"==toJSON (R.contractGeneration definition) && at detail_ "status"/="stale"))] (select ["id","status","analysis","owner","routine","contractGeneration","error","reason","trial","providerWait","maintenanceBudget","contentInterface","rubric"] detail_)
preparationEvidence :: D.Workload -> Value
preparationEvidence work=
 let prepared=D.workField work "preparation"
     root=NE.head (D.workCalls work)
     terminal=case D.callState root of D.CallFailed->"failed"; D.CallInterrupted->"interrupted"; D.CallCancelled->"stopped"; _->""
     status=if at prepared "status"=="queued" && terminal/="" then terminal else at prepared "status"
 in mergeFields [("status",str status),("error",get prepared "error" `fallback` D.callField root "error"),("call",select ["state","error"] (toJSON root))] prepared

renderDetail :: Service -> D.Workload -> IO Value
renderDetail s work = do
 allowed<-DB.occurrenceAllowed s (D.workField work "origin")
 authority<-preparationAuthority s work
 let prepared=preparationEvidence work
     stopped=at (toJSON work) "executionGate"=="stopped"
     status=if at prepared "status"=="cleanup_pending" then "cleanup_pending" else if stopped then "stopped" else if not allowed then "stale" else at prepared "status"
 pure (mergeFields [("id",str (D.workloadId work)),("status",str status),("providerWait",Provider.waitProjection (items prepared "operations")),("maintenanceBudget",D.workField authority "maintenanceBudget")] (merge (select ["owner","routine","contractGeneration"] (D.workField work "preparationPin")) prepared))

-- Repair uses the existing candidate pipeline and immutable original contract.
-- Its caller atomically owns the case transition and this Workload admission.
repairOn :: SQL.Connection -> R.Definition -> Value -> Value -> Value -> RunId -> Text -> IO Value
repairOn db definition failedReference casePin context identity time = do
 works<-readOn db
 source<-maybe (failure 409 "Failed implementation preparation is unavailable") pure
  (find ((==at failedReference "preparation") . D.workloadId) works)
 let previous=listToMaybe [work | attempt<-reverse (items context "attempts"),at attempt "status"=="failed"
       ,work<-works,D.workloadId work==at attempt "preparation",D.workField work "repairPin"==casePin
       ,has (D.workField work "preparation") "version"]
     previousWork=fromMaybe source previous
     original=D.workField source "preparation"
     pin=D.workField source "preparationPin"
 previousFields<-previousVersionFields (AgentId (R.definitionOwner definition)) previousWork context
 require (get pin "owner"==str (R.definitionOwner definition) && get pin "routine"==str (R.definitionId definition)
  && get pin "contractGeneration"==toJSON (R.contractGeneration definition)) 409 "Repair contract changed"
 require (not (any D.generatedCleanupPending works)) 503 "Owned container cleanup remains unresolved"
 let origin=obj [("preparation",toJSON identity)]
     prepared=mergeFields (previousFields++[("expectedBinding",Kit.bindingPin (toJSON definition)),("deferEnabled",Bool True),("parallelEnabled",Bool True),("workflowEnabled",Bool True),("kitEnabled",Bool True),("status",str "queued"),("operations",arr []),("checks",arr [])])
       (select ["analysis","contract","sourceEvidence","parameters","feedProfile","sourceInput"] original)
 void (enqueuePreparationOn db (AgentId (R.definitionOwner definition)) identity time [("repairPin",casePin),("repairInvocationPolicy",toJSON RepairPreparationResponseOnly),("preparationPin",pin)] prepared)
 pure (toJSON identity)

findOn :: SQL.Connection -> Text -> IO D.Workload
findOn db identity=readOn db >>= maybe (failure 409 "Standalone work disappeared") pure . find ((==identity) . D.workloadId)

-- The decision has ended its provider invocation; publication and parking share
-- one transaction, so a crash cannot turn an unrecorded intention into work.
chooseStandalone :: Service -> Run -> Text -> D.WorkflowPlan -> Value -> IO ()
chooseStandalone s running operation plan record=do
 time<-now
 locked s $ do
  requireActive s (agentIdText (runOwner running))
  cancelled<-readIORef (runCancel running)
  require (not cancelled) 409 "Original Task stopped before continuation admission"
  state<-agentState s (agentIdText (runOwner running))
  turn<-maybe (failure 409 "Original input disappeared") pure (find ((==runId running) . turnId) (agentTurns state))
  DB.connection s $ \db->SQL.withTransaction db $ do
   works<-readOn db
   (source,call)<-maybe (failure 409 "Original Task disappeared") pure (listToMaybe [(work,current) | work<-works,current<-NE.toList (D.workCalls work),D.callIdentity current==runId running])
   spec<-D.executionContract source call >>= maybe (failure 409 "Composer requires accepted Task criteria") pure
   parent<-D.standalonePin source call spec
   require (D.workField source "executionGate"/=toJSON ExecutionStopped && D.callField call "outcome"==Null && at (D.workField source "outcome") "status"/="Completed" && D.callState call==D.CallRunning && D.callField call "standaloneWait"==Null) 409 "Original Task is not available for continuation"
   let attempts=[entry | entry<-D.workItems source "decisions",at entry "node"=="Execution"]
   require (not (null attempts) && all (Effects.completeActionFree . (`get` "evidence")) attempts && length (items (toJSON turn) "batch_members")<=1) 409 "Original Task has no complete action-free continuation proof"
   consumerPin<-case plan of
    D.ComposeVersion Nothing->pure (toJSON parent)
    D.ComposeVersion (Just previous)->lineage works spec previous
    D.ExecuteVersion reference->lineage works spec reference
   let reference=case plan of D.ComposeVersion _->Null; D.ExecuteVersion selected->toJSON selected
       prior=case plan of D.ComposeVersion previous->toJSON previous; D.ExecuteVersion _->Null
       request=obj [("parent",toJSON parent),("consumer",consumerPin),("input",D.acceptedInput spec),("contract",toJSON spec),("reference",reference),("previous",prior)]
       key=canonicalHash (obj [("workload",str (D.workloadId source)),("call",toJSON (runId running)),("operation",str operation)])
   receipts<-DB.queryRowsOn db "SELECT request,response FROM command_receipts WHERE scope='workflow' AND owner=? AND id=?" ["request","response"] [SQL.SQLText (agentIdText (runOwner running)),SQL.SQLText key]
   wait<-case receipts of
    [saved]->do
     original<-DB.decodeStored (at saved "request")
     require (original==request) 409 "Workflow operation belongs to another version, input or contract"
     DB.decodeStored (at saved "response")
    []->do
     identity<-RunId <$> newId
     let selected=put "operation" (str key) request
     result<-case plan of
      D.ExecuteVersion _->queueRunOn db source selected (runIdText identity) time
      D.ComposeVersion _->do
       let origin=obj [("preparation",toJSON identity),("standalonePreparation",Bool True)]
           prepared=obj [("status",str "queued"),("contract",toJSON spec),("sourceEvidence",obj [("source",obj [("id",str (D.workloadId source)),("inputs",D.acceptedInput spec)])]),("workflowEnabled",Bool True),("parallelEnabled",Bool True),("deferEnabled",Bool True),("operations",arr []),("checks",arr [])]
       void (enqueuePreparationOn db (runOwner running) identity time [("origin",origin),("standalonePin",consumerPin),("standaloneParent",selected)] prepared)
       pure (put "preparation" (toJSON identity) selected)
     SQL.execute db "INSERT INTO command_receipts(scope,owner,id,target,request,code,response,created) VALUES ('workflow',?,?,?,?,200,?,?)" (agentIdText (runOwner running),key,D.workloadId source,jsonText request,jsonText result,time)
     pure result
    _->failure 409 "Duplicate workflow operation receipts"
   Managed.adoptOn db (agentIdText (runOwner running)) turn [message | message<-items (stateJSON state) "chat",get message "turn"==toJSON (runId running)]
   let accepted=D.acceptedDecision record "Execution" "WaitingForWorkflow"
       parked=D.withCall (D.withDecisionRecord accepted source) (D.setCallFields [("standaloneWait",wait)] call {D.callProgress=D.QueuedAt (D.callNode call)})
   DB.putValueOn db DB.Workloads (D.workloadId source) (toJSON parked)
   Managed.transitionOn db (runIdText (runId running)) (put "status" (str "queued") . put "codeBound" (Bool True) . put "standaloneWait" (Bool True))
 DB.project s (agentIdText (runOwner running))
 throwIO (Workflow.Yield operation)
 where
 lineage works spec reference@(CandidateReference owner preparation _)=do
  require (owner==runOwner running) 403 "Workflow version belongs to another owner"
  void (D.resolveCandidateFrom works owner Null spec reference)
  prior<-maybe (failure 409 "Previous candidate disappeared") pure (find ((==runIdText preparation) . D.workloadId) works)
  require (D.workField prior "standalonePin"/=Null) 409 "Select a standalone version"
  pure (D.workField prior "standalonePin")

queueRunOn :: SQL.Connection -> D.Workload -> Value -> Text -> Text -> IO Value
queueRunOn db _source request identity time=do
 works<-readOn db
 forM_ ["parent","consumer"] $ \key->void (D.requireStandalonePinFrom works (get request key))
 reference<-decodeValue 409 (get request "reference")
 spec<-decodeValue 409 (get request "contract")
 void (D.resolveCandidateFrom works (AgentId (at (get request "parent") "owner")) Null spec reference)
 let CandidateReference owner preparation _=reference
 candidate<-findOn db (runIdText preparation)
 let prepared=D.workField candidate "preparation"; trial=get prepared "trial"
     active=at trial "status"=="activated"
 require (active || trial==Null || get trial "run"==str identity) 409 "This version's first authorized trial is already claimed or finished"
 let role=if active then ActiveCode else CandidateTrial
     selected=mergeFields [("child",str identity),("role",toJSON role),("reference",toJSON reference)] request
     origin=obj [("standaloneRun",selected)]
     turn=newTurn (RunId identity) Chat (D.specObjective spec) time origin False
 unless active $ DB.putValueOn db DB.Workloads (D.workloadId candidate) (toJSON (D.setWorkField "preparation" (put "trial" (obj [("run",str identity),("reference",toJSON reference),("status",str "claimed")]) prepared) candidate))
 Managed.enrollOn db (agentIdText owner) turn Null (D.specObjective spec) []
 -- The normal TaskPrepared decision creates this business Workload at admission.
 pure selected

queueStandaloneOn :: SQL.Connection -> D.Workload -> IO Value
queueStandaloneOn db preparation=do
 let request=D.workField preparation "standaloneParent"
     pin=get request "parent"
 source<-findOn db (at pin "workload")
 call<-maybe (failure 409 "Waiting original Call disappeared") pure (find ((==get pin "call") . toJSON . D.callIdentity) (D.workCalls source))
 let waiting=D.callField call "standaloneWait"
 require (get waiting "preparation"==str (D.workloadId preparation) && D.callState call==D.CallQueued && D.callField call "outcome"==Null && at (D.workField source "outcome") "status"/="Completed") 409 "Original Task no longer awaits this preparation"
 if has waiting "child" then pure waiting else do
  identity<-newId
  time<-now
  selected<-queueRunOn db source (put "reference" (get (get (D.workField preparation "preparation") "version") "reference") waiting) identity time
  DB.putValueOn db DB.Workloads (D.workloadId source) (toJSON (D.withCall source (D.setCallFields [("standaloneWait",selected)] call)))
  pure selected

resumeStandalone :: Service -> Run -> IO (Maybe Text)
resumeStandalone s running=locked s $ do
 requireActive s (agentIdText (runOwner running))
 cancelled<-readIORef (runCancel running)
 require (not cancelled) 409 "Original Task stopped before child settlement"
 DB.connection s $ \db->SQL.withTransaction db $ do
  works<-readOn db
  source<-maybe (failure 409 "Original Task disappeared") pure (find (any ((==runId running) . D.callIdentity) . D.workCalls) works)
  call<-maybe (failure 409 "Original Call disappeared") pure (find ((==runId running) . D.callIdentity) (D.workCalls source))
  let waiting=D.callField call "standaloneWait"
  if waiting==Null then pure Nothing else do
   void (D.requireStandalonePinFrom works (get waiting "parent"))
   require (D.callField call "outcome"==Null && at (D.workField source "outcome") "status"/="Completed") 409 "Original Task outcome is already settled"
   let child=find ((==at waiting "child") . D.workloadId) works
       prepared=find ((==at waiting "preparation") . D.workloadId) works
       childCode=maybe Null (`D.workField` "codeExecution") child
       childOutcome=maybe Null (`D.workField` "outcome") child
       successful=at childOutcome "status"=="Completed" && at (get childCode "acceptance") "verdict"=="accepted" && (at (get childCode "activation") "status"=="activated" || at childCode "role"=="active" && at (get childCode "standaloneActivation") "status"=="activated")
       history=D.workItems source "standaloneHistory"++[waiting]
       resumed=D.setWorkField "standaloneHistory" (arr history) source
       clear=D.setCallFields [("standaloneWait",Null)] call
   if successful then do
    result<-maybe (failure 409 "Accepted child disappeared") pure child
    let childCall=NE.head (D.workCalls result)
    original<-D.executionContract source call >>= maybe (failure 409 "Original accepted criteria disappeared") pure
    actual<-D.executionContract result childCall >>= maybe (failure 409 "Child criteria disappeared") pure
    require (D.sameSemanticContract original actual && D.acceptedInput actual==get waiting "input" && D.workField result "standaloneRun"==waiting && get childCode "reference"==get waiting "reference") 409 "Child proof no longer matches the original Task or exact run"
    void (D.resolveCandidateFrom works (runOwner running) Null actual =<< decodeValue 409 (get waiting "reference"))
    outcome<-decodeValue 409 (remove ["reply","evidence","author"] childOutcome)
    let proof=obj [("child",get waiting "child"),("reference",get waiting "reference"),("assessment",get (get childCode "acceptance") "invocation")]
        response=D.DecisionResponse (D.ReportOutcome (at childOutcome "reply") outcome) "Accepted exact child result against original criteria" [jsonText proof] childOutcome
    either (failure 400 . D.outcomeError) pure (D.validateOutcome response outcome original)
    continuation<-RunId <$> newId
    let completed=clear {D.callProgress=D.FinishedCall (D.Answered (at childOutcome "reply"))}
        finished=D.setWorkField "standaloneResult" proof (D.scopedResult continuation resumed completed childOutcome)
    D.saveWorkOn s db finished
    pure (Just (at childOutcome "reply"))
   else do
    let evidence=obj [("workflowResult",waiting),("preparation",maybe Null (select ["status","error","reason","call"] . preparationEvidence) prepared),("outcome",childOutcome),("code",select ["status","error","activation","acceptance"] childCode),("instruction",str "Continue the original Task from retained evidence. Do not repeat completed or uncertain actions.")]
    D.saveWorkOn s db (D.withCall resumed (D.setCallFields [("observation",evidence)] clear {D.callProgress=D.RunningAt D.Execution}))
    Managed.transitionOn db (runIdText (runId running)) (remove ["standaloneWait"])
    pure Nothing

standaloneSnapshot :: Service -> Text -> IO Value
standaloneSnapshot s owner=do
 works<-D.readWorks s
 rows<-forM [work | work<-works,D.workField work "standalonePin"/=Null,T.null owner || at (D.workField work "standalonePin") "owner"==owner] $ \work->do
  detail_<-renderDetail s work
  let prepared=D.workField work "preparation"
      reference=get (get prepared "version") "reference"
      runs=[obj [("id",str (D.workloadId childWork)),("parent",get (D.workField childWork "standaloneRun") "parent"),("outcome",D.workField childWork "outcome"),("execution",select ["status","activation","acceptance","error"] (D.workField childWork "codeExecution"))] | childWork<-works,get (D.workField childWork "standaloneRun") "reference"==reference,reference/=Null]
      requests=[request | parentWork<-works,request<-D.workItems parentWork "standaloneHistory"++[D.callField call "standaloneWait" | call<-NE.toList (D.workCalls parentWork)],get request "reference"==reference,reference/=Null]
  pure (mergeFields [("requests",arr requests),("id",str (D.workloadId work)),("source",D.workField work "standalonePin"),("parent",get (D.workField work "standaloneParent") "parent"),("reference",reference),("runs",arr runs),("maintenanceBudget",maybe Null (`D.workField` "maintenanceBudget") (find ((==at (D.workField work "standalonePin") "workload") . D.workloadId) works))] (select ["status","checks","review","trial","error","reason"] detail_))
 pure (arr rows)
