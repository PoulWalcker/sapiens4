{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Composer (requestOn, stopOn, detail, projection, run, recover ) where

import Control.Exception
import Control.Monad
import Data.Aeson
import Data.Aeson.Types (Parser,parseEither)
import qualified Data.Aeson.KeyMap as KM
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
import GHC.Clock
import Types
import Population (requireActive)
import qualified Store as DB
import qualified ManagedState as Managed
import qualified Delegation as D
import qualified Routines as R
import qualified Provider
import qualified GeneratedCode as Code

-- Stages have different admission/evidence requirements; a successful process
-- exit is deliberately not a preparation state.
data Phase = Fixtures | Generation | Environment | Checks | Review | AwaitingOccurrence deriving (Eq,Show)
phaseName :: Phase -> Text
phaseName value = case value of Fixtures->"fixtures"; Generation->"generation"; Environment->"environment"; Checks->"checks"; Review->"review"; AwaitingOccurrence->"awaiting_occurrence"

data Fixture = Fixture Value Value deriving (Eq,Show)
instance FromJSON Fixture where
 parseJSON value = do
  input <- withObject "Fixture" (.: "input") value
  expected <- withObject "Fixture" (.: "expected") value
  unless (BL.length (encode input)<=65536) (fail "Fixture input exceeds 64 KiB")
  unless ((at expected "status"=="ok" && keys expected `allIn` ["status","value"] && has expected "value")
    || (at expected "status"=="rejected" && keys expected `allIn` ["status","error"] && not (T.null (at expected "error")))) (fail "Independent expected result is invalid")
  pure (Fixture input expected)
  where allIn values allowed=all (`elem` allowed) values
instance ToJSON Fixture where
 toJSON (Fixture input expected)=obj [("input",input),("expected",expected)]

data Candidate = Candidate Text deriving (Eq,Show)
instance FromJSON Candidate where
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
requestOn :: SQL.Connection -> R.Definition -> RunId -> RunId -> Text -> IO Value
requestOn db definition analysis identity time = do
 require (get (toJSON definition) "externalProfile"==Null) 409 "Receipt-feed profile preparation requires observed-input integration; script/hybrid preparation is not yet supported for this profile"
 works<-readOn db
 require (not (any D.generatedCleanupPending works)) 503 "Owned container cleanup remains unresolved; restore the runtime and restart the app before another preparation"
 source<-maybe (failure 404 "Unknown Ready analysis") pure (find ((==runIdText analysis) . D.workloadId) works)
 pin<-decodeValue 409 (D.workField source "analysisPin") :: IO AnalysisPin
 require (analysisOwner pin==AgentId (R.definitionOwner definition) && analysisRoutine pin==R.definitionId definition && analysisGeneration pin==R.contractGeneration definition && not (R.definitionDeleted definition)) 409 "Analysis does not belong to the current Routine contract"
 operations<-DB.routineOperationsOn db
 require (DB.analysisFeedbackIds operations (toJSON definition) (map toJSON works) (analysisSources pin)==analysisFeedback pin && at (D.workField source "analysisDecision") "status"=="Ready" && all ((==D.CallCompleted) . D.callState) (D.workCalls source) && at (toJSON source) "executionGate"=="open") 409 "Preparation requires a current completed Ready analysis"
 require (not (any (\work->consumer definition work && any ((`elem` [D.CallQueued,D.CallRunning]) . D.callState) (D.workCalls work)) works)) 409 "A preparation is already active for this Routine"
 original<-maybe (failure 409 "The accepted source task is unavailable") pure (find (\work->D.workloadId work==at (get (D.workField source "analysisEvidence") "source") "id") works)
 require (DB.sameRoutineContract (toJSON definition) (toJSON original)) 409 "Preparation needs an accepted task under the current Routine instructions; run it ordinarily first"
 contract<-D.executionContract original (NE.head (D.workCalls original)) >>= maybe (failure 409 "Preparation requires an accepted pinned source task") pure
 let origin=obj [("preparation",toJSON identity)]
     preparation=obj [("status",str "queued"),("analysis",toJSON analysis),("contract",toJSON contract),("sourceEvidence",D.workField source "analysisEvidence"),("parameters",get (D.workField source "analysisDecision") "parameters"),("operations",arr []),("checks",arr [])]
     call=D.newCall identity (analysisOwner pin) Nothing D.Preparation D.CallQueued (R.definitionGoal definition)
     work=D.Workload (runIdText identity) False (call :| []) (KM.fromList [("origin",origin),("created",str time),("originalIntent",str (R.definitionGoal definition)),("executionGate",toJSON ExecutionOpen),("preparationPin",toJSON pin),("preparation",preparation)])
     turn=newTurn identity Chat (R.definitionGoal definition) time origin False
 SQL.execute db "INSERT INTO workloads(id,value) VALUES (?,?)" (D.workloadId work,jsonText (toJSON work))
 Managed.enrollOn db (R.definitionOwner definition) turn Null (R.definitionGoal definition) []
 pure (obj [("routine",str (R.definitionId definition)),("preparation",toJSON identity),("status",str "queued")])

stopOn :: SQL.Connection -> R.Definition -> RunId -> IO Value
stopOn db definition identity = do
 works<-readOn db
 work<-maybe (failure 404 "Unknown preparation") pure (find (\w->consumer definition w && D.workloadId w==runIdText identity) works)
 require (any ((`elem` [D.CallQueued,D.CallRunning]) . D.callState) (D.workCalls work)) 409 "Preparation is already finished"
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
 work<-load s (runIdText (runId running))
 DB.requireOccurrence s (D.workField work "origin")
phase :: Service -> Run -> Phase -> IO ()
phase s running value = phaseChecked s running value (pure ())
phaseChecked :: Service -> Run -> Phase -> IO () -> IO ()
phaseChecked s running value validate = locked s $ do
 validate
 checkGate s running
 work<-load s (runIdText (runId running))
 let changed=D.setWorkField "preparation" (put "status" (str (phaseName value)) (D.workField work "preparation")) work
     promoted=if value==AwaitingOccurrence then changed {D.workCalls=fmap (\call->call {D.callState=D.CallCompleted,D.callOutput=Just "Candidate awaits an authorized occurrence"}) (D.workCalls changed)} else changed
 D.saveWork s promoted

ask :: Service -> Run -> Text -> Text -> Value -> IO Value
ask s running node purpose context = snd <$> askConfigured s running node purpose context Nothing

askConfigured :: Service -> Run -> Text -> Text -> Value -> Maybe Explanation -> IO (Text,Value)
askConfigured s running node purpose context explanation = do
 let settings=explanationModel <$> explanation
 checkGate s running
 (template,envelope)<-case explanation of
  Just selected->Provider.explanationTemplates s selected
  Nothing->(,) <$> prompt s (case node of "PreparationFixtures"->"preparation-fixtures"; "PreparationGeneration"->"preparation-generation"; "PreparationRubricReview"->"hybrid-rubric-review"; _->"preparation-review") <*> prompt s "preparation-envelope"
 let rendered=render envelope [("node",node),("instruction",template),("context",jsonText (put "selfId" (toJSON (runOwner running)) context))]
 identity<-newId
 started<-now
 let operation=obj (concat [[("modelSettings",toJSON selected),("input",context),("inputHash",str (canonicalHash context)),("reference",get context "reference"),("implementationHash",get context "implementationHash")] | selected<-maybeToList settings]++[("id",str identity),("purpose",str purpose),("status",str "running"),("started",str started),("promptHash",str (sourceHash rendered))])
     save fields=update s (runIdText (runId running)) (\p->put "operations" (arr [if at op "id"==identity then merge op fields else op | op<-items p "operations"]) p)
 update s (runIdText (runId running)) (appendItems "operations" [operation])
 (do
  raw<-(maybe (Provider.invokeAnalysisObserved s running) (Provider.invokeResponseOnlyObservedWith s running) settings) rendered (\evidence->save (obj [("evidence",toJSON evidence)]))
  let result=parseJSONText raw
  require (case result of Object _->True; _->False) 422 "Preparation model response must be a JSON object"
  save (obj [("status",str (if isJust explanation then "validating" else "completed")),("response",result)])
  checkGate s running
  pure (identity,result)) `catch` (\(exception::SomeException)->do
   cancelled<-readIORef (runCancel running)
   permitted<-DB.occurrenceAllowed s (obj [("preparation",toJSON (runId running))])
   save (obj [("status",str (if cancelled || not permitted then "interrupted" else "failed")),("error",str (D.exceptionText exception))])
   throwIO exception)

run :: Service -> Run -> Turn -> IO (Text,[Value])
run s running _ = do
 let identity=runIdText (runId running)
 outcome<-try (prepare s running) :: IO (Either SomeException ())
 case outcome of
  Right () -> pure ("Candidate awaits an authorized occurrence",[])
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
 require (at saved "status"=="queued") 409 "Preparation already started; request a new explicit attempt"
 spec<-decodeValue 422 (get saved "contract")
 phase s running Fixtures
 oracle<-ask s running "PreparationFixtures" "fixture_design" (put "criteria" (D.criterionContext spec) (select ["contract","sourceEvidence","parameters"] saved))
 typedFixtures<-field 422 oracle "fixtures" :: IO [Fixture]
 let fixtures=map toJSON typedFixtures
     observedInputs=get (get (get saved "sourceEvidence") "source") "inputs"
     observedJSON=case observedInputs of String input->parseJSONText input; _->observedInputs
 require (not (null fixtures) && any (\fixture->get fixture "input"/=observedJSON) fixtures) 422 "Preparation requires independent examples on new inputs"
 when (has oracle "rubric") (void (D.candidateRubric spec (get oracle "rubric")))
 update s identity (mergeFields [("fixtures",arr fixtures),("rubric",get oracle "rubric")])
 profile<-environment s running
 phase s running Generation
 candidate<-ask s running "PreparationGeneration" "candidate_generation" (mergeFields [("availableProfiles",arr [Code.profileJSON profile]),("interface",str "json-stdin-result-v1")] (select ["contract","sourceEvidence","parameters"] saved))
 Candidate source<-decodeValue 422 candidate
 hybridFields<-if not (has candidate "explanation") then pure [] else do
  rubric<-D.candidateRubric spec (get oracle "rubric")
  settings<-Provider.verifiedResponseSettings s running
  template<-prompt s "hybrid-explanation"
  envelope<-prompt s "preparation-envelope"
  let explanation=mergeFields [("templateHash",str (sourceHash template)),("envelopeHash",str (sourceHash envelope)),("model",toJSON settings),("policy",str "response_only")] (get candidate "explanation")
  pure [("composition",hybridComposition),("structureHash",str (canonicalHash hybridComposition)),("rubric",toJSON rubric),("explanation",put "implementationHash" (str (canonicalHash explanation)) explanation)]
 let content=mergeFields (hybridFields++ [("runtime",Code.profileJSON profile),("contract",get saved "contract"),("contractGeneration",get (D.workField work "preparationPin") "contractGeneration"),("sourceHash",str (sourceHash source))]) (select ["source","language","interface","dependencies","capabilities"] candidate)
     reference=obj [("owner",toJSON (runOwner running)),("preparation",str identity),("digest",str (canonicalHash content))]
     version=put "reference" reference content
 update s identity (put "version" version)
 checkedProfile<-environment s running
 require (Code.profileJSON checkedProfile==get content "runtime") 409 "Candidate runtime identity changed before verification"
 phase s running Checks
 forM_ fixtures $ \fixture->do
  checkGate s running
  requireCleanRuntime s
  stored<-resolveVersion s running reference
  nonce<-newId
  let reserved=Code.reservation checkedProfile (agentIdText (runOwner running)) identity nonce
  started<-now
  let operation=obj [("id",str nonce),("purpose",str "fixture_check"),("status",str "running"),("started",str started),("inputHash",str (canonicalHash (get fixture "input"))),("sourceHash",get stored "sourceHash"),("version",reference),("durationMs",Null)]
      saveOperation fields=update s identity (\p->put "operations" (arr [if at op "id"==nonce then merge op fields else op | op<-items p "operations"]) p)
  update s identity (put "container" reserved . appendItems "operations" [operation])
  result<-(Code.runFixture checkedProfile running reserved (at stored "source") (get fixture "input") (\receipt->do
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
  explanation<-either (failure 422 . T.pack) pure (parseEither candidateExplanation stored)
  forM_ explanation $ \selected->when (at actual "status"=="ok") $ do
   let context=obj [("instruction",str (explanationInstruction selected)),("acceptedInput",get fixture "input"),("facts",get actual "value"),("criteria",D.criterionContext spec),("reference",reference),("implementationHash",get (get stored "explanation") "implementationHash")]
   (explainId,narrative)<-askConfigured s running "PreparationExplanation" "fixture_explanation" context (Just selected)
   let parsedNarrative=parseEither narrativeText narrative
   update s identity (\p->put "operations" (arr [if at op "id"==explainId then mergeFields [("status",str (either (const "failed") (const "completed") parsedNarrative))] op else op | op<-items p "operations"]) p)
   text_<-either (failure 422 . T.pack) pure parsedNarrative
   (reviewId,assessment)<-askConfigured s running "PreparationRubricReview" "fixture_semantic_review" (mergeFields [("rubric",get stored "rubric"),("expected",get fixture "expected"),("explanation",str text_)] context) Nothing
   verdicts<-D.criterionAssessments spec (get assessment "criteria")
   decision<-parseValue 422 (D.parseDecision D.Execution) (obj [("event",str "Outcome"),("reply",str text_),("reason",str "Independent hybrid fixture rubric"),("evidence",arr [str evidence | CriterionAssessment _ _ evidence<-verdicts]),("outcome",D.criteriaOutcome spec verdicts)])
   accepted<-case D.decisionBody decision of
    D.ReportOutcome _ outcome->D.validateOutcome decision outcome (NE.toList (D.specCriteria spec)) >> pure (D.outcomeStatus outcome==D.Completed)
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

resolveVersion :: Service -> Run -> Value -> IO Value
resolveVersion s running expected = do
 current<-load s (runIdText (runId running))
 reference<-decodeValue 409 expected
 D.resolveCandidateBytes (runOwner running) current reference

recover :: Service -> IO ()
recover s = do
 works<-D.readWorks s
 forM_ works $ \work->when (has (toJSON work) "preparation") $ do
  let identity=D.workloadId work; prepared=D.workField work "preparation"; container=get prepared "container"
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
projection s definition works = maybe (pure Null) (fmap (\detail_ -> put "reference" (get (get detail_ "version") "reference") (select ["id","status","analysis","owner","routine","contractGeneration","error","trial"] detail_)) . renderDetail s) (listToMaybe (reverse (filter (consumer definition) works)))
renderDetail :: Service -> D.Workload -> IO Value
renderDetail s work = do
 allowed<-DB.occurrenceAllowed s (D.workField work "origin")
 let prepared=D.workField work "preparation"
     stopped=at (toJSON work) "executionGate"=="stopped"
     status=if at prepared "status"=="cleanup_pending" then "cleanup_pending" else if stopped then "stopped" else if not allowed then "stale" else at prepared "status"
 pure (mergeFields [("id",str (D.workloadId work)),("status",str status)] (merge (select ["owner","routine","contractGeneration"] (D.workField work "preparationPin")) prepared))
