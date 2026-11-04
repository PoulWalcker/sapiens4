{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Automation (dispatch, requestOn, preflight, recordRepairFaultOn, withRepairBudget, dispatchRepairs, recoverRepairs, repairProjection, recordAuditFailure, savedAuditSourceOn, providerDue, deferProvider, cancelProviderWait, grantRepairOn, stopRepairOn, runKitInspection, requestRecovery, runRecovery, configurationBlocked) where

import Control.Concurrent (readMVar)
import qualified Data.Map.Strict as M
import Control.Exception hiding (handle)
import Control.Monad
import Data.Aeson hiding (pairs)
import qualified Workflow
import qualified WorkflowCalls as WC
import qualified Data.ByteString.Lazy as BL
import qualified Data.Aeson.KeyMap as KM
import Data.List (find,nubBy)
import Data.IORef
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Database.SQLite.Simple as SQL
import Types
import qualified Store as DB
import qualified ManagedState as Managed
import qualified Effects
import qualified Delegation as D
import qualified Routines as R
import qualified Kit
import qualified Provider
import Population (requireActive)
import qualified Composer
import GHC.Clock
import Data.Time.Clock (getCurrentTime,addUTCTime,diffUTCTime)

preflight :: IO ()
preflight = void Provider.analysisCatalog

-- Owner recovery uses the ordinary durable call runtime. The requesting tool
-- returns before any independent model is started; no nested provider process.
requestRecovery :: Service -> Text -> Text -> IO Value
requestRecovery s owner identity = do
 instruction<-prompt s "routine-effect-review"
 prior<-D.readWorks s
 let canResume=any (\work->at (D.workField work "routineRecovery") "owner"==owner && at (D.workField work "routineRecovery") "routine"==identity && configurationBlocked work) prior
 when canResume (void Provider.analysisCatalog)
 time<-now
 run<-RunId <$> newId
 DB.connection s $ \db->SQL.withTransaction db $ do
  definition<-Effects.definitionOn db identity
  require (at definition "owner"==owner) 404 "Unknown personal Routine"
  require (not (boolean (get definition "deleted"))) 409 "Routine is deleted"
  sources<-Effects.routineRecoveryCandidatesOn db owner identity
  works<-readWorksOn db
  let previous=[work | work<-works,at (D.workField work "routineRecovery") "routine"==identity,at (D.workField work "routineRecovery") "owner"==owner]
      receipt work=obj [("recovery",obj [("run",str (D.workloadId work)),("status",str (T.toLower (wireName (D.callState (NE.last (D.workCalls work)))))),("result",D.workField work "recoveryResult"),("canResume",Bool (configurationBlocked work)),("error",D.callField (NE.last (D.workCalls work)) "error")])]
  if null sources then case listToMaybe (reverse previous) of
   Just work->pure (receipt work)
   Nothing->do
    blockers<-Effects.routineBlockersOn db owner identity
    pure (obj [("recovery",obj [("status",str (if null blockers then "clear" else "blocked")),("blockers",arr blockers)])])
  else do
   contexts<-traverse (either (failure 409 . T.pack) pure . Effects.routineReviewContext) sources
   let key=canonicalHash (obj [("owner",str owner),("routine",str identity),("sources",arr contexts),("policy",str (sourceHash instruction))])
   case find ((==key) . (`at` "key") . (`D.workField` "routineRecovery")) previous of
    Just work->receipt <$> (if canResume then resumeUnadmittedRecoveryOn db work else pure work)
    Nothing->do
     let request=obj [("owner",str owner),("routine",str identity),("contractGeneration",get definition "contractGeneration"),("key",str key),("policyHash",str (sourceHash instruction)),("contexts",arr contexts)]
         origin=obj [("routineRecovery",toJSON run)]
         input="Review retained Routine actions without replaying them"
         call=D.newCall run (AgentId owner) Nothing (D.QueuedAt D.Preparation) input
         work=D.Workload (runIdText run) False (call :| []) (KM.fromList [("origin",origin),("routineRecovery",request),("executionGate",toJSON ExecutionOpen),("created",str time)])
     require (BL.length (encode request)<=262144) 413 "Retained recovery evidence exceeds the bounded review context"
     DB.putValueOn db DB.Workloads (D.workloadId work) (toJSON work)
     Managed.enrollOn db owner (newTurn run Chat input time origin False) Null input []
     pure (receipt work)

-- A pre-exec configuration refusal has not spent the model review allowance.
-- Resume its original call and retained operation, never a negative/unknown
-- admitted review or a stopped call. Waiting wall time is not active execution.
resumeUnadmittedRecoveryOn :: SQL.Connection -> D.Workload -> IO D.Workload
resumeUnadmittedRecoveryOn db work = do
 let operation=D.workField work "recoveryOperation"
     attempts=items operation "attempts"
     failed=all ((==D.CallFailed) . D.callState) (D.workCalls work)
     versionFailure="Automation analysis requires verified Codex CLI 0.162.1"
     -- This exact host error was emitted only by the pre-callback version gate
     -- in the first owner-review release, before capture or process creation.
     oldGate=failed && at operation "status"=="unknown" && length attempts==1
       && all ((==str versionFailure) . (`D.callField` "error")) (D.workCalls work)
       && at operation "node"=="RoutineEffectReview" && at operation "purpose"=="routine_effect_review"
       && at operation "policy"=="response_only" && at operation "promptHash"==sourceHash (at operation "request")
       && get operation "rawResponse"==Null && get operation "evidence"==Null
       && all (\attempt->at attempt "status"=="failed" && get attempt "evidence"==Null) attempts
     known=configurationBlocked work
 if not (failed && known && D.workField work "executionGate"==toJSON ExecutionOpen) then pure work else do
  rows<-DB.queryRowsOn db "SELECT value FROM managed_turns WHERE id=?" ["value"] [SQL.SQLText (D.workloadId work)]
  turn<-maybe (failure 409 "Recovery authority disappeared") (DB.decodeStored . (`at` "value")) (listToMaybe rows)
  require (not (any ((=="cancel") . (`at` "action")) (items (get turn "managed") "commands"))) 409 "Stopped recovery cannot resume"
  let legacyUnused=case (parseTime (at operation "deadline"),listToMaybe attempts >>= parseTime . (`at` "finished")) of
       (Just deadline,Just finished)->max 0 (floor (realToFrac (diffUTCTime deadline finished)*1000::Double) :: Int)
       _->0
      unused=if oldGate then legacyUnused else int (get operation "unusedMilliseconds")
      retained=if oldGate then [mergeFields [("status",str "not_admitted"),("usageUnknown",Bool False),("admission",obj [("schema",num 1),("status",str "not_started"),("authority",str "host-version-gate"),("gateHash",str (sourceHash versionFailure)),("requestHash",get operation "promptHash"),("reason",str versionFailure)])] attempt | attempt<-attempts] else attempts
  require (unused>0 && unused<=90000 && all (\attempt->at attempt "status"=="not_admitted" && at (get attempt "admission") "status"=="not_started" && get attempt "evidence"==Null) retained) 409 "No proven unused review allowance remains"
  clock<-getCurrentTime
  let resumed=mergeFields [("status",str "ready"),("attempts",arr retained),("unusedMilliseconds",num unused),("deadline",str (timestamp (addUTCTime (fromIntegral unused/1000) clock)))] operation
      queued=(D.setWorkField "recoveryOperation" resumed work) {D.workCalls=fmap (\call->call {D.callProgress=D.QueuedAt D.Preparation,D.callFields=KM.delete "error" (D.callFields call)}) (D.workCalls work)}
  DB.putValueOn db DB.Workloads (D.workloadId queued) (toJSON queued)
  Managed.transitionOn db (D.workloadId queued) (put "status" (str "queued") . remove ["error","managed_result"])
  pure queued

configurationBlocked :: D.Workload -> Bool
configurationBlocked work =
 let operation=D.workField work "recoveryOperation"; attempts=items operation "attempts"
     oldGate=at operation "status"=="unknown" && length attempts==1
       && all ((==str "Automation analysis requires verified Codex CLI 0.162.1") . (`D.callField` "error")) (D.workCalls work)
       && at operation "node"=="RoutineEffectReview" && at operation "purpose"=="routine_effect_review"
       && at operation "policy"=="response_only" && at operation "promptHash"==sourceHash (at operation "request")
       && get operation "rawResponse"==Null && get operation "evidence"==Null
       && all (\attempt->at attempt "status"=="failed" && get attempt "evidence"==Null) attempts
 in all ((==D.CallFailed) . D.callState) (D.workCalls work)
   && D.workField work "executionGate"==toJSON ExecutionOpen
   && (at operation "status"=="configuration_blocked" || oldGate
    || at operation "status"=="ready" && not (null attempts) && all (\attempt->at attempt "status"=="not_admitted" && get attempt "evidence"==Null && at (get attempt "admission") "status"=="not_started") attempts)

runRecovery :: Service -> Run -> Turn -> IO (Text,[Value])
runRecovery s running turn = do
 locked s (D.begin s (runOwner running) turn)
 (work,_)<-D.getWork s (runId running)
 let request=D.workField work "routineRecovery"
     owner=agentIdText (runOwner running)
     identity=at request "routine"
     checkOn db = do
      current<-loadRecovery db
      definition<-Effects.definitionOn db identity
      require (at definition "owner"==owner && not (boolean (get definition "deleted"))
        && get definition "contractGeneration"==get request "contractGeneration") 409 "Routine owner or contract changed during recovery"
      require (D.workField current "executionGate"==toJSON ExecutionOpen && all ((`elem` [D.CallQueued,D.CallRunning]) . D.callState) (D.workCalls current)) 409 "Recovery was stopped"
      candidates<-Effects.routineRecoveryCandidatesOn db owner identity
      require (map (`get` "workloadId") candidates==map (`get` "run") (items request "contexts")) 409 "The Routine's unresolved source set changed during recovery"
      sources<-forM (items request "contexts") $ \context->do
       rows<-DB.queryRowsOn db "SELECT value FROM workloads WHERE id=?" ["value"] [SQL.SQLText (at context "run")]
       value<-maybe (failure 409 "Recovery source disappeared") (DB.decodeStored . (`at` "value")) (listToMaybe rows)
       actual<-either (failure 409 . T.pack) pure (Effects.routineReviewContext value)
       require (actual==context) 409 "Retained invocation evidence changed during recovery"
       pure value
      pure (current,sources)
     loadRecovery db=do
      works<-readWorksOn db
      maybe (failure 409 "Recovery call disappeared") pure (find ((==runIdText (runId running)) . D.workloadId) works)
     guard=do
      requireActive s owner
      cancelled<-readIORef (runCancel running)
      require (not cancelled) 409 "Recovery stopped by Admin"
      DB.requireOccurrence s (D.workField work "origin")
      void (DB.connection s checkOn)
     save operation=locked s $ DB.connection s $ \db->do
      current<-loadRecovery db
      DB.putValueOn db DB.Workloads (D.workloadId current) (toJSON (D.setWorkField "recoveryOperation" operation current))
 instruction<-prompt s "routine-effect-review"
 require (sourceHash instruction==at request "policyHash") 409 "Recovery review policy changed"
 envelope<-prompt s "preparation-envelope"
 let context=obj [("owner",str owner),("routine",str identity),("sources",arr [source | selected<-items request "contexts",source<-items selected "sources"]),("runs",get request "contexts")]
     savedOperation=D.workField work "recoveryOperation"
     rendered=if savedOperation/=Null then at savedOperation "request" else render envelope [("node","RoutineEffectReview"),("instruction",instruction),("context",jsonText context)]
 clock<-getMonotonicTimeNSec
 let capped=running {runDeadline=min (runDeadline running) (clock+90000000000)}
 guard
 operation<-Provider.operationFor s capped "RoutineEffectReview" "routine_effect_review" (at request "key") rendered Nothing [value | let value=D.workField work "recoveryOperation",value/=Null]
 -- This is one bounded review, never an automatic repair/replay loop.
 raw<-Provider.invokeRetained s capped operation save `catch` (\(_::Provider.RetryDeferred)->failure 409 "Recovery review provider unavailable; its original operation is retained")
 let verdict=parseJSONText raw
 void (parseValue 422 (onlyFields ["readOnly","reviewedActions","reason"]) verdict)
 readOnly<-field 422 verdict "readOnly" :: IO Bool
 reason<-parseValue 422 (textFieldP "reason" 8000) verdict
 let ids=[get action "id" | source<-items context "sources",action<-items source "actions"]
 require (length (items verdict "reviewedActions")==length ids && all (`elem` items verdict "reviewedActions") ids) 422 "Recovery review must account for every retained action"
 guard
 locked s $ do
  guard
  DB.connection s $ \db->SQL.withTransaction db $ do
   (current,sources)<-checkOn db
   let retained=D.workField current "recoveryOperation"
       coverage=[obj [("run",get selected "run"),("sourceHash",get selected "sourceHash"),("actions",arr [get action "id" | source<-items selected "sources",action<-items source "actions"])] | selected<-items request "contexts"]
       proof=obj [("authority",str "independent-review"),("reviewRun",toJSON (runId running)),("policyHash",get request "policyHash"),("operation",retained),("coverage",arr coverage)]
       supplemented=[put "routineEffectReviews" (arr (items source "routineEffectReviews"++[put "sourceHash" (get selected "sourceHash") proof])) source | (source,selected)<-zip sources (items request "contexts")]
   when readOnly $ do
    require (all Effects.routineReviewAccepted supplemented) 409 "Independent recovery proof is incomplete"
    forM_ supplemented $ \source->DB.putValueOn db DB.Workloads (at source "workloadId") source
   let result=obj [("status",str (if readOnly then "reconciled" else "blocked")),("reason",str reason),("originalRuns",arr (map (`get` "workloadId") sources)),("replayed",Bool False)]
       finished=(D.setWorkField "recoveryResult" result current) {D.workCalls=fmap (\call->call {D.callProgress=D.FinishedCall (D.Answered reason)}) (D.workCalls current)}
   DB.putValueOn db DB.Workloads (D.workloadId current) (toJSON finished)
 pure ((if readOnly then "Retained actions were independently verified as reads. Future authorized Routine runs may continue; the old failed result was not replayed or accepted. " else "Recovery remains blocked. ")<>reason,[])

consumer :: R.Definition -> D.Workload -> Bool
consumer definition work = at (D.workField work "preparationPin") "routine"==R.definitionId definition
 && get (D.workField work "preparationPin") "owner"==str (R.definitionOwner definition)
currentPreparation :: R.Definition -> D.Workload -> Bool
currentPreparation definition work = consumer definition work && get (D.workField work "preparationPin") "contractGeneration"==toJSON (R.contractGeneration definition)
latestUnsuitable :: [D.Workload] -> Bool
latestUnsuitable preparations = maybe False ((=="unsuitable") . (`at` "status") . (`D.workField` "preparation")) (listToMaybe (reverse preparations))
ordinary :: R.Definition -> D.Workload -> Bool
ordinary definition work = at (D.workField work "origin") "routine"==R.definitionId definition
 && D.callOwner (NE.head (D.workCalls work))==AgentId (R.definitionOwner definition)
 && not (has (toJSON work) "preparationPin")
 && not (has (toJSON work) "analysisPin")
 && not (has (toJSON work) "codeExecution")
currentOrdinary :: R.Definition -> D.Workload -> Bool
currentOrdinary definition work = ordinary definition work && DB.sameRoutineContract (toJSON definition) (toJSON work)
terminal :: D.Workload -> Bool
terminal work = all ((`elem` [D.CallCompleted,D.CallFailed,D.CallUnresolved,D.CallInterrupted,D.CallCancelled]) . D.callState) (D.frontierCalls work)
positive :: [Value] -> D.Workload -> Bool
positive operations work = terminal work && at (DB.workAssessment operations (toJSON work)) "status"=="Accepted"
 && at (D.workField work "outcome") "status"=="Completed"
active :: D.Workload -> Bool
active work = any ((`elem` [D.CallQueued,D.CallRunning,D.CallWaiting]) . D.callState) (D.frontierCalls work)
 || (at (D.workField work "preparation") "status" `elem` ["waiting_provider","waiting_children"]
     && D.workField work "executionGate"/=toJSON ExecutionStopped
     && not (any ((==D.CallCancelled) . D.callState) (D.frontierCalls work)))

evidenceKey :: R.Definition -> [Value] -> [D.Workload] -> Value
evidenceKey definition operations works = obj
 [("contractGeneration",toJSON (R.contractGeneration definition))
 ,("positiveRuns",arr [obj [("run",str (D.workloadId work)),("assessment",get (DB.workAssessment operations (toJSON work)) "decisionId")] | work<-works,currentOrdinary definition work,positive operations work])
 ,("feedback",toJSON (DB.effectiveFeedbackIds operations (toJSON definition) (map toJSON works)))]

-- Selection only changes the supplied context, never retained evidence. Full
-- required facts fail visibly when oversized; optional values are kept whole.
selectEvidence :: R.Definition -> [Value] -> [D.Workload] -> D.Workload -> Maybe D.Workload -> Either Text Value
selectEvidence definition operations works source prior = do
 sourceInput<-if Effects.profileBound (toJSON source) then either (Left . T.pack) Right (Effects.observedInput (toJSON source)) else Right Null
 let matching=filter (ordinary definition) works
     current=filter (currentOrdinary definition) matching
     accepted=filter (positive operations) current
     feedback=concat [take 1 (DB.workFeedback operations (toJSON work)) | work<-matching,currentOrdinary definition work || D.workloadId work==D.workloadId source]
     summary work = obj [("id",str (D.workloadId work)),("inputs",get (D.workField work "routineSnapshot") "inputs")
       ,("positiveCurrent",Bool (currentOrdinary definition work && positive operations work))
       ,("applicability",str (if currentOrdinary definition work then "current" else "historical_unknown"))
       ,("savedContract",if currentOrdinary definition work then Null else selectPresent ["goal","prompt","criteria","contractGeneration"] (D.workField work "routineSnapshot"))
       ,("assessment",DB.workAssessment operations (toJSON work)),("outcome",D.workField work "outcome")
       ,("status",toJSON (D.callState (NE.last (D.workCalls work)))),("error",D.callField (NE.last (D.workCalls work)) "error")]
     invocation work entry = mergeFields [("run",str (D.workloadId work)),("evidence",remove ["events"] (get entry "evidence"))] (selectPresent ["decisionId","callId","node","purpose"] entry)
     metadata=[("invocations",invocation work entry) | work<-source:filter ((/=D.workloadId source) . D.workloadId) (reverse matching),entry<-D.workItems work "decisions",has entry "evidence"]
     receipts work=[("receipts",obj [("run",str (D.workloadId work)),("decisionId",get entry "decisionId"),("receipt",event)]) | entry<-D.workItems work "decisions",event<-items (get entry "evidence") "events"]
     others=filter ((/=D.workloadId source) . D.workloadId) (reverse matching)
     negative=[("history",summary work) | work<-others,terminal work,not (currentOrdinary definition work && positive operations work)]
     successes=[("history",summary work) | work<-others,currentOrdinary definition work,positive operations work]
     optional=metadata++negative++receipts source++successes++concatMap receipts others
     accounting=obj [("recordedRuns",num (length matching)),("acceptedCurrentRuns",num (length accepted)),("negativeRuns",num (length [work | work<-current,terminal work,not (positive operations work)]))
       ,("omittedRuns",num 0),("omittedReceipts",num 0),("omittedInvocations",num 0),("omittedBytes",num 0),("contextLimitBytes",num 262144)]
     required=obj [("consumer",obj [("owner",str (R.definitionOwner definition)),("routine",str (R.definitionId definition)),("contractGeneration",toJSON (R.contractGeneration definition))])
       ,("contract",obj [("goal",str (R.definitionGoal definition)),("criteria",maybe Null toJSON (R.definitionCriteria definition))])
       ,("source",put "inputBinding" sourceInput (summary source)),("positiveRuns",toJSON (map D.workloadId accepted)),("negativeFeedback",arr feedback)
       ,("previousDecision",maybe Null (selectPresent ["status","reason","error"] . (`D.workField` "preparation")) prior)
       ,("history",arr []),("invocations",arr []),("receipts",arr []),("coverage",accounting)]
     bytes=BL.length . encode
     -- Leave room for final decimal omission counters inside the strict cap.
     append context (key,value)=put key (arr (items context key++[value])) context
     fit (context,omitted) item = let next=append context item in
      if bytes next<=261120 then (next,omitted) else (context,item:omitted)
 if bytes required>261120 then Left "Saved analysis context exceeds 256 KiB; required criteria, outcome or feedback cannot be omitted"
 else let (selected,omitted)=foldl fit (required,[]) optional
          count key=length [() | (category,_)<-omitted,category==key]
          coverage=mergeFields [("omittedRuns",num (count "history")),("omittedReceipts",num (count "receipts")),("omittedInvocations",num (count "invocations")),("omittedBytes",toJSON (sum [bytes value | (_,value)<-omitted]))] accounting
      in Right (put "coverage" coverage selected)

readWorksOn :: SQL.Connection -> IO [D.Workload]
readWorksOn db = DB.queryRowsOn db "SELECT value FROM workloads ORDER BY rowid" ["value"] [] >>= traverse (DB.decodeStored . (`at` "value") >=> decodeValue 500)

-- Evidence admission and the candidate share one durable Workload. A timer
-- dispatches new observations; it does not buy another suitability decision.
requestOn :: SQL.Connection -> R.Definition -> RunId -> Text -> Bool -> IO Value
requestOn db candidate identity time explicit = do
 current<-Effects.definitionOn db (R.definitionId candidate)
 definition<-decodeValue 500 current
 require (R.contractGeneration definition==R.contractGeneration candidate) 409 "Routine contract changed before preparation admission"
 works<-readWorksOn db
 operations<-DB.routineOperationsOn db
 let preparations=filter (currentPreparation definition) works
     positives=[work | work<-works,currentOrdinary definition work,positive operations work]
     key=evidenceKey definition operations works
 require (not (any (\work->D.workField work "preparationKey"==key && at (D.workField work "preparation") "status"=="awaiting_occurrence") preparations)) 409 "Current checked candidate already awaits an authorized run"
 require (not (R.definitionDeleted definition)) 409 "Routine is deleted"
 require (not (any active (filter (consumer definition) works))) 409 "Preparation is already active"
 unless explicit $ do
  require (not (latestUnsuitable preparations)) 409 "Current contract was unsuitable for preparation; change the contract or explicitly reconsider preparation"
  require (not (any ((==key) . (`D.workField` "preparationKey")) preparations)) 409 "These observations already have a preparation"
 source<-maybe (failure 409 "Preparation needs a saved accepted ordinary result under current instructions") pure (listToMaybe (reverse positives))
 when (Effects.profileBound (toJSON source)) $ void (Effects.preparationInput (toJSON definition) (toJSON source))
 evidence<-either (failure 413) pure (selectEvidence definition operations works source (listToMaybe (reverse preparations)))
 let pin=AnalysisPin (AgentId (R.definitionOwner definition)) (R.definitionId definition) (R.contractGeneration definition)
       (DB.analysisFeedbackIds operations (toJSON definition) (map toJSON works) [RunId (D.workloadId source)]) [RunId (D.workloadId source)]
 when (has current "preparationNotice") $ void (DB.saveRoutineOn db (remove ["preparationNotice"] current))
 Composer.requestOn db definition source (obj [("pin",toJSON pin),("evidence",evidence),("key",key)]) identity time

dispatch :: Service -> IO ()
dispatch s = do
 definitions<-R.readDefinitions s
 works<-D.readWorks s
 operations<-DB.routineOperations s
 forM_ definitions $ \definition->when (not (R.definitionDeleted definition) && not ("group_" `T.isPrefixOf` R.definitionOwner definition)) $ do
  let preparations=filter (currentPreparation definition) works
      key=evidenceKey definition operations works
      activeCode=get (toJSON definition) "activeCode"
      activeSource=find ((==at (get activeCode "reference") "preparation") . D.workloadId) works
      currentPin work=case fromJSON (D.workField work "preparationPin") of
       Success pin->DB.analysisFeedbackIds operations (toJSON definition) (map toJSON works) (analysisSources pin)==analysisFeedback pin
       Error _->False
      healthy=at activeCode "status"=="active" && get activeCode "contractGeneration"==toJSON (R.contractGeneration definition) && maybe False currentPin activeSource
      already=any ((==key) . (`D.workField` "preparationKey")) preparations
      ready=any (\work->at (D.workField work "preparation") "status"=="awaiting_occurrence" && D.workField work "preparationKey"==key) preparations
      observations=any (\work->currentOrdinary definition work && positive operations work) works
      notice=get (toJSON definition) "preparationNotice"
  when (observations && not (latestUnsuitable preparations) && not healthy && not already && not ready && not (any active (filter (consumer definition) works)) && get notice "key"/=key) $ do
   result<-try $ do
    requireActive s (R.definitionOwner definition)
    preflight
    identity<-RunId <$> newId
    time<-now
    DB.connection s $ \db->SQL.withTransaction db $ void (requestOn db definition identity time False)
   case result of
    Right ()->pure ()
    Left (APIError _ reason)->unless (reason=="Preparation is already active") $ DB.connection s $ \db->SQL.withTransaction db $ do
     current<-Effects.definitionOn db (R.definitionId definition)
     when (get current "contractGeneration"==toJSON (R.contractGeneration definition)) $
      void (DB.saveRoutineOn db (put "preparationNotice" (obj [("key",key),("status",str "blocked"),("reason",str reason)]) current))

-- A case retains one aggregate allowance. Workloads remain the only executable
-- operations; the case is consumer-owned evidence and authorization history.
repairTerminal :: Value -> Bool
repairTerminal saved = at saved "status" `elem` ["recovered","exhausted","interrupted","stopped","structure_revision","declined","stale","inspection_complete","kit_inspection_unknown"]
repairCasesOn :: SQL.Connection -> IO [Value]
repairCasesOn db = DB.queryRowsOn db "SELECT value FROM scheduled_routines ORDER BY sequence" ["value"] [] >>= traverse (DB.decodeStored . (`at` "value"))
repairCaseIn :: Value -> Value -> Maybe Value
repairCaseIn definition pin = find ((==get pin "case") . (`get` "id")) (items definition "repairCases")
putRepair :: Value -> Value -> Value
putRepair definition saved = put "repairCase" (get saved "id") $ put "repairCases"
 (arr (if any ((==get saved "id") . (`get` "id")) (items definition "repairCases")
  then [if get old "id"==get saved "id" then saved else old | old<-items definition "repairCases"]
  else items definition "repairCases"++[saved])) definition
-- Keep the consumer's Routine identity distinct from its case identity.
casePin :: Value -> Value -> Value
casePin definition saved = obj [("owner",get definition "owner"),("routine",get definition "id"),("contractGeneration",get saved "contractGeneration"),("case",get saved "id")]
stopRepair :: Text -> Text -> Value -> Value
stopRepair status reason = mergeFields [("status",str status),("terminalReason",str reason)]

preparationPending :: Value -> Text
preparationPending saved = if at saved "kind"=="saved_result_audit" then "waiting_audit" else "waiting_preparation"

recordRepairFaultOn :: SQL.Connection -> D.Workload -> Value -> Text -> IO ()
recordRepairFaultOn db work reference reason = do
 definitions<-repairCasesOn db
 let snapshot=D.workField work "routineSnapshot"
 forM_ [definition | definition<-definitions,get definition "id"==get snapshot "id",get definition "owner"==get snapshot "owner",not ("group_" `T.isPrefixOf` at definition "owner")] $ \definition->do
  let previousCases=items definition "repairCases"
      historical=find (\saved->reference `elem` items saved "references" && get saved "recoveredReference"/=reference) (reverse previousCases)
      activeCode=get definition "activeCode"
  works<-readWorksOn db
  let adoption=find ((==at activeCode "run") . D.workloadId) works
      verifiedAfter saved proof =
       let code=D.workField proof "codeExecution"; adoptedSnapshot=D.workField proof "routineSnapshot"
       in at activeCode "status"=="active" && get activeCode "reference"==reference
        && get adoptedSnapshot "id"==get definition "id" && get adoptedSnapshot "owner"==get definition "owner"
        && get adoptedSnapshot "contractGeneration"==get definition "contractGeneration"
        && get code "reference"==reference && at (get code "activation") "status"=="activated"
        && get (get code "activationAudit") "accepted"==Bool True && not (boolean (get code "completedViaFallback"))
        && at (D.workField proof "outcome") "status"=="Completed" && all ((==D.CallCompleted) . D.callState) (D.workCalls proof)
        && case (parseTime (at activeCode "decidedAt"),parseTime (at saved "created")) of (Just adopted,Just created)->adopted>created; _->False
      closed=case (historical,listToMaybe (reverse previousCases),adoption) of
       (Nothing,Just latest,Just proof) | verifiedAfter latest proof->Just (mergeFields [("status",str "recovered"),("recoveredReference",reference)
         ,("references",arr (if reference `elem` items latest "references" then items latest "references" else items latest "references"++[reference]))
         ,("terminalReason",str "An independently checked authorized adoption closed this case before the later fault")] latest)
       _->Nothing
      base=maybe definition (putRepair definition) closed
      cases=items base "repairCases"
      pending=listToMaybe (reverse cases) >>= \latest->if at latest "status"/="recovered" || get latest "recoveredReference"/=reference then Just latest else Nothing
      prior=historical `mplus` pending
  identity<-newId
  time<-now
  let symptoms=items (D.workField work "codeExecution") "faults"
      saved=fromMaybe (obj [("id",str identity),("status",str "waiting_preparation"),("created",str time)
        ,("contractGeneration",get snapshot "contractGeneration"),("rootReference",reference),("references",arr [reference])
        ,("limitMilliseconds",num 1800000),("spentMilliseconds",num 0),("reservations",arr []),("attempts",arr []),("operations",arr []),("evidence",arr [])]) prior
      evidence=obj [("run",str (D.workloadId work)),("reference",reference),("reason",str reason),("faults",arr symptoms),("code",select ["input","runtimeResult","acceptance","hybridResult","evaluatedResult","evaluatedExplanation","fallbacks"] (D.codeProjection (D.workField work "codeExecution"))),("externalExecution",D.workField work "externalExecution")]
      key=canonicalHash evidence
      updated=put "evidence" (arr (if any ((==key) . (`at` "key")) (items saved "evidence") then items saved "evidence" else items saved "evidence"++[put "key" (str key) evidence]))
        (put "references" (arr (if reference `elem` items saved "references" then items saved "references" else items saved "references"++[reference])) saved)
  void (DB.saveRoutineOn db (putRepair base updated))

-- Audit recovery retains the original business identity; it does not quarantine
-- healthy implementation code or create a replacement business occurrence.
recordAuditFailure :: Service -> Run -> Text -> IO ()
recordAuditFailure s running reason = locked s $ DB.connection s $ \db->SQL.withTransaction db $ do
 works<-readWorksOn db
 forM_ (find ((==runIdText (runId running)) . D.workloadId) works) $ \work->do
  let code=D.workField work "codeExecution"
      assessment=D.codeVerification code
      runtime=get (D.codeProjection code) "runtimeResult"
      atAssessment=at assessment "node" `elem` ["CodeAssessment","HybridAssessment"]
      successful=get runtime "exitCode"==num 0 && at runtime "status"=="exited" && not (boolean (get runtime "wasRunning")) && get runtime "cleanupConfirmed"==Bool True
  when (atAssessment && successful && has code "evaluatedResult" && null (items code "faults") && null (pairs (get code "fallbacks"))) $ do
   recordRepairFaultOn db work (get code "reference") reason
   definitions<-repairCasesOn db
   forM_ (find (\definition->get definition "id"==get (D.workField work "routineSnapshot") "id") definitions) $ \definition->do
    forM_ (find ((==get definition "repairCase") . (`get` "id")) (items definition "repairCases")) $ \saved->
     unless (repairTerminal saved) $ void (DB.saveRoutineOn db (putRepair definition (mergeFields [("status",str "waiting_audit"),("kind",str "saved_result_audit"),("auditSource",str (D.workloadId work)),("lastFailure",obj [("stage",str "assessment"),("reason",str reason)])] saved)))

savedAuditSourceOn :: SQL.Connection -> Value -> Text -> IO (D.Workload,Value,Value)
savedAuditSourceOn db pin identity = do
 (definition,saved)<-loadRepairOn db pin
 require (not (repairTerminal saved) && not (boolean (get definition "deleted")) && get definition "contractGeneration"==get pin "contractGeneration") 409 "Saved audit case is stopped or its contract changed"
 require (at saved "auditSource"==identity) 409 "This is not the case's current saved audit source"
 works<-readWorksOn db
 source<-maybe (failure 409 "Saved audit source disappeared") pure (find ((==identity) . D.workloadId) works)
 _<-either (failure 409 . T.pack) pure (Effects.auditPublication (toJSON source))
 let snapshot=D.workField source "routineSnapshot"
     relevant=[work | work<-works,at (D.workField work "origin") "routine"==at definition "id",get (D.workField work "routineSnapshot") "owner"==get definition "owner"]
 require (maybe False ((==identity) . D.workloadId) (listToMaybe (reverse relevant))) 409 "A newer business occurrence supersedes this retained audit source"
 require (get snapshot "contractGeneration"==get definition "contractGeneration" && D.workField source "executionGate"==toJSON ExecutionOpen) 409 "Original occurrence was stopped or its contract changed"
 let code=D.workField source "codeExecution"
     runtime=get (D.codeProjection code) "runtimeResult"
     result=get code "evaluatedResult"
     responses=[get op "response" | op<-items code "operations",at op "node" `elem` ["CodeAssessment","HybridAssessment"]]
     negative value=at value "verdict" `elem` ["rejected","unknown"] || (has value "accepted" && get value "accepted"/=Bool True)
      || at (get value "acceptance") "status"=="Unresolved" || at (get value "outcome") "status"=="Unresolved"
      || (has (get value "activationAudit") "accepted" && get (get value "activationAudit") "accepted"/=Bool True)
      || any (\criterion->at criterion "verdict" `elem` ["fail","unknown"] || at criterion "status" `elem` ["fail","unknown"]) (items value "criteria")
 require (null (items code "faults") && null (pairs (get code "fallbacks")) && not (boolean (get code "completedViaFallback"))
  && get runtime "exitCode"==num 0 && at runtime "status"=="exited" && get runtime "cleanupConfirmed"==Bool True && not (boolean (get runtime "wasRunning"))
  && result/=Null && get runtime "actual"==result && at runtime "inputHash"==canonicalHash (get code "input")
  && not (any negative responses) && at (get code "acceptance") "verdict" `notElem` ["rejected","unknown"]
  && D.workField source "routineEffectUnknown"==Null) 409 "Saved result is incomplete, rejected, uncertain or replaced by fallback; audit-only recovery is unavailable"
 let call=NE.head (D.workCalls source)
 spec<-D.executionContract source call >>= maybe (failure 409 "Saved audit has no original task criteria") pure
 reference<-decodeValue 409 (get code "reference")
 version<-D.resolveCandidateFrom works (AgentId (at definition "owner")) snapshot spec reference
 require (get runtime "sourceHash"==get version "sourceHash" && get runtime "reference"==get version "reference") 409 "Saved runtime identity contradicts its immutable version"
 binding<-decodeValue 409 (D.callField call "executionBinding")
 case binding of
  OrdinaryBinding _ _->failure 409 "Saved audit has no immutable code binding"
  ScriptBinding owner _ bound role->do
   require (owner==AgentId (at definition "owner") && toJSON bound==get code "reference" && get code "role"==toJSON role && get code "structureHash"==get version "structureHash") 409 "Original code binding or structure changed"
   case role of
    ActiveCode->require (at (get definition "activeCode") "status"=="active" && get (get definition "activeCode") "reference"==get code "reference") 409 "Original active binding was superseded or quarantined"
    CandidateTrial->do
     let CandidateReference _ preparation _=bound
     prepared<-maybe (failure 409 "Original trial preparation disappeared") pure (find ((==runIdText preparation) . D.workloadId) works)
     require (get (get (D.workField prepared "preparation") "trial") "run"==str identity && get (get (D.workField prepared "preparation") "trial") "reference"==get code "reference") 409 "Original trial claim changed"
 require (not (has version "explanation") || not (T.null (at code "evaluatedExplanation"))) 409 "Saved explanation is missing; audit recovery cannot regenerate it"
 let proof=obj [("source",str identity),("snapshot",snapshot),("task",D.workField source "task"),("code",code),("version",version),("executionGate",D.workField source "executionGate"),("externalExecution",D.workField source "externalExecution")]
 pure (source,version,obj [("source",str identity),("hash",str (canonicalHash proof)),("reference",get code "reference")])

loadRepairOn :: SQL.Connection -> Value -> IO (Value,Value)
loadRepairOn db pin = do
 definitions<-repairCasesOn db
 definition<-maybe (failure 409 "Repair consumer is unavailable") pure (find (\d->get d "id"==get pin "routine" && get d "owner"==get pin "owner") definitions)
 saved<-maybe (failure 409 "Repair case is unavailable") pure (repairCaseIn definition pin)
 pure (definition,saved)
updateRepair :: Service -> Value -> (Value -> Value) -> IO ()
updateRepair s pin transform = locked s (updateRepairUnlocked s pin transform)
updateRepairUnlocked :: Service -> Value -> (Value -> Value) -> IO ()
updateRepairUnlocked s pin transform = DB.connection s $ \db->SQL.withTransaction db $ do
 (definition,saved)<-loadRepairOn db pin
 void (DB.saveRoutineOn db (putRepair definition (transform saved)))

-- Reserving before IO makes a crash non-refundable. Only measured completion
-- returns the proven unused part. Waiting for a queue or occurrence is free.
withRepairBudget :: Service -> Run -> (Run -> IO a) -> IO a
withRepairBudget s running action = do
 found<-D.findWork s (runId running)
 case found of
  Nothing->action running
  Just (_,call) | WC.child call->(WC.cap s running >>= action) `finally` WC.activity s running False
  Just (work,_) | not (has (toJSON work) "repairPin")->if has (toJSON work) "preparation" then withPreparationBudget s running action else if has (D.workField work "codeExecution") "workflowDefinition" then do
   WC.openBudget s running 1800000
   runBudgetAction s running (WC.cap s running >>= action) (WC.quiesce s running >> WC.finishBudget s running)
   else action running
  Just (work,_)->mask $ \restore->do
   let pin=D.workField work "repairPin"; token=runIdText (runId running)
   started<-getMonotonicTimeNSec
   allowance<-locked s $ DB.connection s $ \db->SQL.withTransaction db $ do
    (definition,saved)<-loadRepairOn db pin
    require (not (repairTerminal saved) && get definition "contractGeneration"==get pin "contractGeneration" && not (boolean (get definition "deleted"))) 409 "Repair is stopped or its consumer changed"
    let own=find (\receipt->at receipt "operation"==token && at receipt "status"=="reserved") (items saved "reservations")
    case own of
     Just receipt | at (D.workField work "workflowBudget") "status"=="reserved"->pure (int (get receipt "milliseconds"))
     _->do
      require (not (any ((=="reserved") . (`at` "status")) (items saved "reservations"))) 409 "Another repair activity already owns the allowance"
      let remaining=int (get saved "limitMilliseconds")-int (get saved "spentMilliseconds")
      require (remaining>0) 409 "Repair active allowance is exhausted"
      let reservation=obj [("operation",str token),("status",str "reserved"),("milliseconds",num remaining)]
      void (DB.saveRoutineOn db (putRepair definition (appendItems "reservations" [reservation] (put "spentMilliseconds" (get saved "limitMilliseconds") saved))))
      pure remaining
   let parallel=has (D.workField work "codeExecution") "workflowDefinition" || boolean (get (D.workField work "preparation") "workflowEnabled")
   when parallel (WC.openBudget s running allowance)
   let available=if runDeadline running>started then runDeadline running-started else 0
       capped=running {runDeadline=started+fromInteger (min (toInteger available) (toInteger allowance*1000000))}
       settle=do
        ended<-getMonotonicTimeNSec
        measured<-if parallel then WC.quiesce s running >> WC.used s running else pure (fromIntegral ((ended-started+999999) `div` 1000000))
        let used=min allowance (max 1 measured)
        when parallel (WC.finishBudget s running)
        updateRepair s pin $ \saved->
         let reservations=[if at r "operation"==token && at r "status"=="reserved" then mergeFields [("status",str "settled"),("usedMilliseconds",num used)] r else r | r<-items saved "reservations"]
             measured=put "reservations" (arr reservations) (put "spentMilliseconds" (num (int (get saved "spentMilliseconds")-allowance+used)) saved)
         in if used>=allowance && at saved "status" `notElem` ["recovered","stopped"] && not (has saved "userStop") then stopRepair "exhausted" "The case's aggregate active repair allowance is exhausted" measured else measured
   runBudgetAction s running (do activeRun<-if parallel then WC.cap s capped else pure capped; restore (action activeRun)) settle

withPreparationBudget :: Service -> Run -> (Run -> IO a) -> IO a
withPreparationBudget s running action=mask $ \restore->do
 started<-getMonotonicTimeNSec
 let identity=runId running
     authority=do
      (own,_)<-D.getWork s identity
      Composer.preparationAuthority s own
     change transform=locked s $ do
      work<-authority
      let budget=D.workField work "maintenanceBudget" `fallback` obj [("limitMilliseconds",num 1800000),("spentMilliseconds",num 0),("reservations",arr [])]
      D.saveWork s (D.setWorkField "maintenanceBudget" (transform budget) work)
 allowance<-locked s $ do
  work<-authority
  require (D.workField work "executionGate"/=toJSON ExecutionStopped) 409 "Preparation lineage was stopped"
  let budget=D.workField work "maintenanceBudget" `fallback` obj [("limitMilliseconds",num 1800000),("spentMilliseconds",num 0),("reservations",arr [])]
      remaining=int (get budget "limitMilliseconds")-int (get budget "spentMilliseconds")
  (own,_)<-D.getWork s identity
  case find ((=="reserved") . (`at` "status")) (items budget "reservations") of
   Just receipt | at receipt "operation"==runIdText identity && at (D.workField own "workflowBudget") "status"=="reserved"->pure (int (get receipt "milliseconds"))
   _->do
    require (remaining>0 && not (any ((=="reserved") . (`at` "status")) (items budget "reservations"))) 409 "Original preparation active allowance is exhausted or uncertain"
    let reservation=obj [("operation",toJSON identity),("status",str "reserved"),("milliseconds",num remaining)]
    D.saveWork s (D.setWorkField "maintenanceBudget" (appendItems "reservations" [reservation] (put "spentMilliseconds" (get budget "limitMilliseconds") budget)) work)
    pure remaining
 (own,_)<-D.getWork s identity
 let parallel=boolean (get (D.workField own "preparation") "workflowEnabled")
 when parallel (WC.openBudget s running allowance)
 let available=if runDeadline running>started then runDeadline running-started else 0
     capped=running {runDeadline=started+fromInteger (min (toInteger available) (toInteger allowance*1000000))}
     settle=do
      ended<-getMonotonicTimeNSec
      measured<-if parallel then WC.quiesce s running >> WC.used s running else pure (fromIntegral ((ended-started+999999) `div` 1000000))
      let used=min allowance (max 1 measured)
      when parallel (WC.finishBudget s running)
      change (\budget->put "spentMilliseconds" (num (int (get budget "spentMilliseconds")-allowance+used)) (put "reservations" (arr [if at receipt "status"=="reserved" then mergeFields [("status",str "settled"),("usedMilliseconds",num used)] receipt else receipt | receipt<-items budget "reservations"]) budget))
 runBudgetAction s running (do activeRun<-if parallel then WC.cap s capped else pure capped; restore (action activeRun)) settle

runBudgetAction :: Service -> Run -> IO a -> IO () -> IO a
runBudgetAction s running action settle=do
 result<-try action
 case result of
  Left problem | Just (_::Workflow.Yield)<-fromException problem->throwIO (problem::SomeException)
  _->do
   settle
   case result of
    Left problem | isNothing (fromException problem :: Maybe Provider.RetryDeferred)->locked s $ do
     cancelled<-readIORef (runCancel running)
     (work,_)<-D.getWork s (runId running)
     when (has (toJSON work) "preparation" && at (D.workField work "preparation") "status" `notElem` ["failed","stopped","stale","interrupted","cleanup_pending","awaiting_occurrence"]) $
      D.saveWork s (D.setWorkField "preparation" (mergeFields [("status",str (if cancelled || D.workField work "executionGate"==toJSON ExecutionStopped then "stopped" else if at (D.workField work "workflowBudget") "status"=="uncertain" then "interrupted" else "failed")),("error",str (D.exceptionText problem))] (D.workField work "preparation")) work)
    _->pure ()
   either throwIO pure result

settleStoppedWorkflows :: Service -> IO ()
settleStoppedWorkflows s=do
 works<-D.readWorks s
 live<-readMVar (serviceRuns s)
 forM_ works $ \work->when (has (toJSON work) "workflowHalt" && at (D.workField work "workflowBudget") "status"=="reserved" && not (any (\call->M.member (D.callIdentity call) live) (D.workCalls work)) && not (D.generatedCleanupPending work)) $ DB.connection s $ \db->SQL.withTransaction db $ do
  let budget=D.workField work "workflowBudget"
      allowance=int (get budget "allowance")
      used=min allowance ((int (get budget "usedNanoseconds")+999999) `div` 1000000)
      settle value=put "spentMilliseconds" (num (max 0 (int (get value "spentMilliseconds")-allowance+used))) (put "reservations" (arr [if at receipt "status"=="reserved" && (get receipt "operation"==Null || at receipt "operation"==D.workloadId work) then mergeFields [("status",str "settled"),("usedMilliseconds",num used)] receipt else receipt | receipt<-items value "reservations"]) value)
      changed=D.setWorkField "workflowBudget" (put "status" (str "settled") budget) work
  if has (toJSON work) "repairPin" then do
   (definition,saved)<-loadRepairOn db (D.workField work "repairPin")
   void (DB.saveRoutineOn db (putRepair definition (settle saved)))
   DB.putValueOn db DB.Workloads (D.workloadId work) (toJSON changed)
   else do
    authority<-Composer.preparationAuthority s work
    if D.workloadId authority==D.workloadId work then DB.putValueOn db DB.Workloads (D.workloadId work) (toJSON (if has (toJSON work) "maintenanceBudget" then D.setWorkField "maintenanceBudget" (settle (D.workField work "maintenanceBudget")) changed else changed))
    else do
     DB.putValueOn db DB.Workloads (D.workloadId authority) (toJSON (D.setWorkField "maintenanceBudget" (settle (D.workField authority "maintenanceBudget")) authority))
     DB.putValueOn db DB.Workloads (D.workloadId work) (toJSON changed)

providerWaitProjection :: [Value] -> Value
providerWaitProjection=Provider.waitProjection

providerOperations :: Service -> D.Workload -> IO [Value]
providerOperations s work
 | has (toJSON work) "preparation"=pure (items (D.workField work "preparation") "operations")
 | has (toJSON work) "savedAudit"=pure (items (D.workField work "codeExecution") "operations")
 | has (toJSON work) "repairPin"=do
    (_,saved)<-DB.connection s (\db->loadRepairOn db (D.workField work "repairPin"))
    pure (items saved "operations")
 | otherwise=pure []

providerDue :: Service -> Turn -> IO Bool
providerDue s turn=do
 found<-D.findWork s (turnId turn)
 case found of
  Nothing->pure True
  Just (work,_) | not (has (toJSON work) "providerWait")->pure True
  Just (work,_)->do
   operations<-Provider.retainedOperations <$> providerOperations s work
   time<-now
   pure $ case find ((==get (D.workField work "providerWait") "operation") . (`get` "id")) operations of
    Just op->case (parseTime (at op "retryAt"),parseTime time) of (Just due,Just clock)->clock>=due; _->True
    Nothing->True

deferProvider :: Service -> Run -> Text -> Text -> IO ()
deferProvider s running operation due=do
 (work,_)<-D.getWork s (runId running)
 cancelled<-readIORef (runCancel running)
 allowed<-DB.occurrenceAllowed s (D.workField work "origin")
 require (not cancelled && allowed && D.workField work "executionGate"==toJSON ExecutionOpen) 409 "Provider wait was stopped before publication"
 let queued=(D.setWorkField "providerWait" (obj [("operation",str operation)]) work) {D.workCalls=fmap (\call->if D.callIdentity call==runId running then call {D.callProgress=D.QueuedAt (D.callNode call)} else call) (D.workCalls work)}
 D.saveWork s queued
 state<-agentState s (agentIdText (runOwner running))
 let changed=mapTurns (\turn->if turnId turn==runId running then (withTurnFields (put "providerWait" (obj [("operation",str operation),("retryAt",str due)]) . remove ["error"]) turn) {turnStatus=Queued} else turn) state
 saveState s (agentIdText (runOwner running)) changed
 DB.project s (agentIdText (runOwner running))

cancelProviderWait :: Service -> RunId -> IO ()
cancelProviderWait s identity=do
 found<-D.findWork s identity
 forM_ found $ \(work,_)->do
  when (has (toJSON work) "repairPin") $ do
   time<-now
   updateRepairUnlocked s (D.workField work "repairPin") (\saved->if at saved "status"=="recovered" then saved else put "userStop" (obj [("at",str time),("reason",str "Repair stage stopped by Admin"),("authority",str "admin")]) (stopRepair "stopped" "Repair stage stopped by Admin" saved))
  when (has (toJSON work) "providerWait") $ do
   let operation=at (D.workField work "providerWait") "operation"
       cancel=map cancelOne
       cancelOne op=let nested=if get op "correction"/=Null then put "correction" (cancelOne (get op "correction")) op else op
                    in if at op "id"==operation && at op "status"=="retry_wait" then mergeFields [("status",str "cancelled"),("reason",str "Stopped by Admin")] nested else nested
   if has (toJSON work) "preparation" then D.saveWork s (D.setWorkField "preparation" (put "operations" (arr (cancel (items (D.workField work "preparation") "operations"))) (D.workField work "preparation")) work)
   else if has (toJSON work) "savedAudit" then D.saveWork s (D.setWorkField "codeExecution" (put "operations" (arr (cancel (items (D.workField work "codeExecution") "operations"))) (D.workField work "codeExecution")) work)
   else when (has (toJSON work) "repairPin") (updateRepairUnlocked s (D.workField work "repairPin") (\saved->put "operations" (arr (cancel (items saved "operations"))) saved))

-- Explicit Admin grants extend only this case's ceiling. They never reopen an
-- old operation, reset its deadline/count, or clear an effect/usage receipt.
grantRepairOn :: SQL.Connection -> Value -> Text -> Text -> Int -> Text -> Text -> IO Value
grantRepairOn db definition identity operation minutes reason time=do
 saved<-currentRepair definition identity
 works<-readWorksOn db
 let related=[work | work<-works,D.workField work "repairPin"==casePin definition saved]
     remaining=int (get saved "limitMilliseconds")-int (get saved "spentMilliseconds")
 require (int (get saved "limitMilliseconds")>0 && int (get saved "spentMilliseconds")>=0 && at saved "status"=="exhausted" && remaining<=0 && not (has saved "userStop")) 409 "Only this case's exhausted active resource can be continued; Stop and provider-attempt exhaustion are not grants"
 require (not (any ((`elem` ["reserved","uncertain"]) . (`at` "status")) (items saved "reservations")) && not (any repairWorkActive related)) 409 "Repair work is active or its prior reservation is uncertain; wait for settlement before granting resource"
 Effects.requireDefinitionClearOn db definition
 let additions=toInteger minutes*60000
     previous=toInteger (int (get saved "limitMilliseconds"))
 require (minutes>0 && additions>0 && previous+additions<=9007199254740991 && previous+additions<=toInteger (maxBound::Int)) 400 "Additional repair minutes must be a positive whole number within the accounting range"
 require (not (any ((==operation) . (`at` "id")) (items saved "grants"))) 409 "This grant operation already exists"
 let grant=obj [("id",str operation),("case",str identity),("owner",get definition "owner"),("routine",get definition "id"),("contractGeneration",get saved "contractGeneration"),("authority",str "admin"),("reason",str reason),("created",str time),("additionalMilliseconds",toJSON additions),("previousLimitMilliseconds",get saved "limitMilliseconds"),("previousSpentMilliseconds",get saved "spentMilliseconds")]
     updated=mergeFields [("baseLimitMilliseconds",get saved "baseLimitMilliseconds" `fallback` get saved "limitMilliseconds"),("limitMilliseconds",toJSON (previous+additions)),("status",str (preparationPending saved)),("terminalReason",Null),("waitingReason",str "Explicit finite continuation authorized"),("continuation",obj [("grant",str operation),("authorized",str time),("kind",str (if preparationPending saved=="waiting_preparation" then "new_preparation" else "saved_result_audit"))])]
       (appendItems "grants" [grant] saved)
 void (DB.saveRoutineOn db (put "revision" (num (int (get definition "revision")+1)) (putRepair definition updated)))
 pure (obj [("routine",get definition "id"),("case",str identity),("grant",grant),("status",str (preparationPending saved)),("remainingMilliseconds",toJSON additions)])

currentRepair :: Value -> Text -> IO Value
currentRepair definition identity=do
 require (get definition "repairCase"==str identity && not (boolean (get definition "deleted"))) 409 "The current repair case is required"
 saved<-maybe (failure 409 "Unknown repair case") pure (find ((==identity) . (`at` "id")) (items definition "repairCases"))
 require (get saved "contractGeneration"==get definition "contractGeneration") 409 "Repair contract changed; this grant cannot change its pins"
 pure saved

repairWorkActive :: D.Workload -> Bool
repairWorkActive work=any ((`elem` [D.CallQueued,D.CallRunning,D.CallWaiting]) . D.callState) (D.workCalls work)

-- A case-wide Stop revokes future admission; unspent grants and old receipts
-- stay visible. Existing occurrence gates stop queued and active processes.
stopRepairOn :: SQL.Connection -> Value -> Text -> Text -> Text -> IO Value
stopRepairOn db definition identity reason time=do
 saved<-currentRepair definition identity
 require (at saved "status" `notElem` ["recovered","stale","stopped"]) 409 "Repair case is already closed or stopped"
 works<-readWorksOn db
 forM_ [work | work<-works,D.workField work "repairPin"==casePin definition saved,repairWorkActive work] $ \work->
  DB.putValueOn db DB.Workloads (D.workloadId work) (toJSON (D.setWorkField "executionGate" (toJSON ExecutionStopped) work))
 let stopped=put "kitInspections" (arr [if at record "status" `elem` ["queued","running"] then put "status" (str "stopped") record else record | record<-items saved "kitInspections"]) $ put "userStop" (obj [("at",str time),("reason",str reason),("authority",str "admin")]) (stopRepair "stopped" reason saved)
 void (DB.saveRoutineOn db (put "revision" (num (int (get definition "revision")+1)) (putRepair definition stopped)))
 pure (obj [("routine",get definition "id"),("case",str identity),("status",str "stopped")])

recoverRepairs :: Service -> IO ()
recoverRepairs s = do
 definitions<-R.readDefinitions s
 forM_ definitions $ \definition->forM_ (items (toJSON definition) "repairCases") $ \saved->do
  let pin=casePin (toJSON definition) saved
  relatedWorkflow<-D.readWorks s
  let safelyWaiting=any (\work->D.workField work "repairPin"==pin && WC.safeIdle work) relatedWorkflow
  when (not safelyWaiting && any ((=="reserved") . (`at` "status")) (items saved "reservations")) $
   updateRepairUnlocked s pin $ \current->
    stopRepair "interrupted" "Host stopped during active repair; unfinished reservation remains consumed and spend is unknown"
     (mergeFields [("usageUnknown",Bool True),("reservations",arr [if at r "status"=="reserved" then put "status" (str "uncertain") r else r | r<-items current "reservations"])
      ,("operations",arr [if at op "status" `elem` ["running","reserved","admitted"] then mergeFields [("status",str "uncertain"),("usageUnknown",Bool True)] op else op | op<-items current "operations"])] current)


 works<-D.readWorks s
 forM_ works $ \work->when (has (D.workField work "origin") "analysis" && repairWorkActive work) $
  D.saveWork s ((D.setWorkField "executionGate" (toJSON ExecutionStopped) work) {D.workCalls=fmap (\call->if D.callState call `elem` [D.CallQueued,D.CallRunning,D.CallWaiting] then (D.setCallFields [("error",str "Separate analysis was retired; request preparation explicitly if needed")] call) {D.callProgress=D.CancelledAt (D.callNode call)} else call) (D.workCalls work)})


-- Same independently audited adoption proof used by repair lineage admission.
kitIndependentAdoption :: Value -> Value -> [D.Workload] -> Bool
kitIndependentAdoption definition saved works=case find ((==at (get definition "activeCode") "run") . D.workloadId) works of
 Nothing->False
 Just proof->let activeCode=get definition "activeCode"; code=D.workField proof "codeExecution"; snapshot=D.workField proof "routineSnapshot" in
  at activeCode "status"=="active" && get code "reference"==get activeCode "reference" && get snapshot "id"==get definition "id" && get snapshot "owner"==get definition "owner" && get snapshot "contractGeneration"==get definition "contractGeneration"
  && at (get code "activation") "status"=="activated" && get (get code "activationAudit") "accepted"==Bool True && not (boolean (get code "completedViaFallback")) && at (D.workField proof "outcome") "status"=="Completed" && all ((==D.CallCompleted) . D.callState) (D.workCalls proof)
  && case (parseTime (at activeCode "decidedAt"),parseTime (at saved "created")) of (Just adopted,Just created)->adopted>created; _->False

kitLocalEvidence :: Value -> Value -> Value -> [D.Workload] -> Text
kitLocalEvidence definition saved reference works=canonicalHash (obj [("active",select ["status","reference","reason","failedRun"] (get definition "activeCode")),("evidence",get saved "evidence"),("faults",arr [obj [("run",str (D.workloadId work)),("faults",get code "faults")] | work<-works,let code=D.workField work "codeExecution",get code "reference"==reference,not (null (items code "faults"))])])

-- One persisted, response-only inspection uses the consumer's existing case.
-- The issue key is stable across new failures of the same source bytes.
ensureKitInspections :: Service -> IO ()
ensureKitInspections s=do
 definitions<-R.readDefinitions s
 works<-D.readWorks s
 forM_ definitions $ \typed->do
  let definition=toJSON typed; owner=R.definitionOwner typed; activeCode=get definition "activeCode"; reference=get activeCode "reference"
      source=find ((==at reference "preparation") . D.workloadId) works
  forM_ source $ \prepared->forM_ (Kit.issueFor owner (get (D.workField prepared "preparation") "version") (map toJSON works)) $ \issue->do
   let key=canonicalHash (obj [("issue",get issue "id"),("routine",get definition "id"),("reference",reference),("contractGeneration",get definition "contractGeneration")])
       existing=any (any ((==key) . (`at` "id")) . (`items` "kitInspections")) (items definition "repairCases")
   unless (existing || get issue "reference"==reference || R.definitionDeleted typed || "group_" `T.isPrefixOf` owner) $ do
    time<-now
    identity<-newId
    DB.connection s $ \db->SQL.withTransaction db $ do
     current<-Effects.definitionOn db (R.definitionId typed)
     let prior_=find ((==get current "repairCase") . (`get` "id")) (items current "repairCases")
         newLineage=maybe False (\old->at old "status" `elem` ["recovered","inspection_complete"] && kitIndependentAdoption current old works) prior_
         prior=if newLineage then Nothing else prior_
         base=fromMaybe (obj [("id",str identity),("status",str "waiting_kit_inspection"),("kind",str "kit_inspection"),("created",str time),("contractGeneration",get current "contractGeneration"),("rootReference",reference),("references",arr [reference]),("limitMilliseconds",num 1800000),("spentMilliseconds",num 0),("reservations",arr []),("attempts",arr []),("operations",arr []),("evidence",arr [])]) prior
         localFault=at activeCode "status"=="suspect" || at base "kind"=="implementation_defect" && at base "status" `notElem` ["recovered","inspection_complete"]
         blocked=(repairTerminal base && at base "status" `notElem` ["recovered","inspection_complete"]) || int (get base "spentMilliseconds")>=int (get base "limitMilliseconds") || has base "userStop" || get base "contractGeneration"/=get current "contractGeneration" || reference `notElem` items base "references"
         record=obj [("id",str key),("issue",get issue "id"),("cause",issue),("reference",reference),("contractGeneration",get current "contractGeneration"),("revision",get current "revision"),("status",str (if localFault then "already_affected" else if blocked then "blocked" else "queued")),("created",str time),("reason",str (if localFault then "This consumer already has its own implementation fault" else if blocked then "Existing repair lineage is stopped, exhausted, stale or owns another binding; no fresh allowance" else "Shared suspect artifact requires this consumer's own applicability inspection"))]
         appended=appendItems "kitInspections" [record] base
         ready=if not localFault && not blocked && at base "kind"=="kit_inspection" && not (any ((=="reserved") . (`at` "status")) (items base "reservations")) then put "status" (str "waiting_kit_inspection") appended else appended
     when (get current "contractGeneration"==get definition "contractGeneration" && get (get current "activeCode") "reference"==reference) (void (DB.saveRoutineOn db (putRepair current ready)))

-- A queued inspection never replaces an unrelated active repair/audit phase.
promoteKitInspections :: Service -> IO ()
promoteKitInspections s=do
 definitions<-R.readDefinitions s
 forM_ definitions $ \definition->forM_ (items (toJSON definition) "repairCases") $ \saved->
  when (at saved "status" `elem` ["recovered","inspection_complete"] && any ((=="queued") . (`at` "status")) (items saved "kitInspections") && not (has saved "userStop") && not (any ((`elem` ["reserved","uncertain"]) . (`at` "status")) (items saved "reservations"))) $
   updateRepairUnlocked s (casePin (toJSON definition) saved) (\latest->if at latest "status"==at saved "status" then mergeFields [("kitReturnPhase",get latest "status"),("status",str "waiting_kit_inspection")] latest else latest)

inspectionOn :: SQL.Connection -> Value -> Text -> IO (Value,Value,Value)
inspectionOn db pin identity=do
 (definition,saved)<-loadRepairOn db pin
 inspection<-maybe (failure 409 "Kit inspection disappeared") pure (find ((==identity) . (`at` "id")) (items saved "kitInspections"))
 require (not (boolean (get definition "deleted")) && get definition "contractGeneration"==get inspection "contractGeneration" && get definition "revision"==get inspection "revision" && get (get definition "activeCode") "reference"==get inspection "reference") 409 "Kit consumer binding or contract changed"
 require (not (has saved "userStop") && at saved "status"=="inspecting_kit" && at inspection "status" `elem` ["queued","running"]) 409 "Kit inspection is stopped or no longer current"
 pure (definition,saved,inspection)

saveKitOperation :: Service -> Run -> Value -> Text -> Value -> IO ()
saveKitOperation s running pin identity operation=locked s $ DB.connection s $ \db->SQL.withTransaction db $ do
 (definition,saved)<-loadRepairOn db pin
 works<-readWorksOn db
 work<-maybe (failure 409 "Kit inspection authority disappeared") pure (find ((==runIdText (runId running)) . D.workloadId) works)
 let entries=filter ((/=at operation "id") . (`at` "id")) (items saved "operations")++[operation]
     inspections=[if at record "id"==identity then mergeFields [("status",str (if at record "status" `elem` ["queued","running"] then "running" else at record "status")),("deadline",get operation "deadline"),("operation",get operation "id")] record else record | record<-items saved "kitInspections"]
     updated=D.setWorkField "kitInspection" (put "operation" operation (D.workField work "kitInspection")) work
     observed=if any Effects.positiveAnomaly (Provider.operationEvidence operation) && D.workField updated "routineEffectUnknown"==Null then D.setWorkField "routineEffectUnknown" (obj [("decisionId",get operation "id"),("reason",str "Kit inspection trace contains unexpected activity")]) updated else updated
 void (DB.saveRoutineOn db (putRepair definition (mergeFields [("operations",arr entries),("kitInspections",arr inspections)] saved)))
 DB.putValueOn db DB.Workloads (D.workloadId work) (toJSON observed)

runKitInspection :: Service -> Run -> Turn -> IO (Text,[Value])
runKitInspection s running _=do
 (work,_)<-D.getWork s (runId running)
 let pin=D.workField work "repairPin"; request=D.workField work "kitInspection"; identity=at request "id"
     guard=do
      requireActive s (agentIdText (runOwner running))
      cancelled<-readIORef (runCancel running)
      require (not cancelled) 409 "Kit inspection stopped"
      Effects.requireRunClear s (runId running)
      (latest,_)<-D.getWork s (runId running)
      require (D.workField latest "executionGate"==toJSON ExecutionOpen) 409 "Kit inspection authority stopped"
      (definition,saved,_)<-DB.connection s (\db->inspectionOn db pin identity)
      works<-D.readWorks s
      require (kitLocalEvidence definition saved (get request "reference") works==at request "localEvidence") 409 "Local Kit evidence changed before inspection admission"
     finishUnknown problem=do
      locked s $ DB.connection s $ \db->SQL.withTransaction db $ do
       (definition,saved)<-loadRepairOn db pin
       works<-readWorksOn db
       let changed=kitLocalEvidence definition saved (get request "reference") works/=at request "localEvidence"
           localFault=changed && get (get definition "activeCode") "reference"==get request "reference" && get definition "contractGeneration"==get request "contractGeneration" && (at (get definition "activeCode") "status"=="suspect" || not (null (items saved "evidence")))
           phase=if localFault then "waiting_preparation" else if at saved "kind"/="kit_inspection" && has saved "kitReturnPhase" then at saved "kitReturnPhase" else "kit_inspection_unknown"
           updated=mergeFields ([("status",str phase),("terminalReason",str (D.exceptionText problem)),("kitInspections",arr [if at record "id"==identity then mergeFields [("status",str (if localFault then "stale" else "unknown")),("reason",str (D.exceptionText problem))] record else record | record<-items saved "kitInspections"])]++[("kind",str "implementation_defect") | localFault]) saved
       unless (repairTerminal saved) (void (DB.saveRoutineOn db (putRepair definition updated)))
      throwIO problem
     execute=do
      guard
      instruction<-prompt s "kit-inspection"
      envelope<-prompt s "preparation-envelope"
      let context=get request "context"
          rendered=render envelope [("node","KitInspection"),("instruction",instruction),("context",jsonText context)]
      require (BL.length (encode context)<=262144) 413 "Kit inspection evidence exceeds the bounded context; no automatic retry"
      (_,saved)<-DB.connection s (\db->loadRepairOn db pin)
      clock<-getMonotonicTimeNSec
      let capped=running {runDeadline=min (runDeadline running) (clock+90000000000)}
      operation<-Provider.operationFor s capped "KitInspection" "kit_inspection" ("KitInspection:"<>runIdText (runId running)) rendered Nothing (items saved "operations")
      raw<-Provider.invokeRetained s capped operation (saveKitOperation s running pin identity)
      verdict<-pure (parseJSONText raw)
      void (parseValue 422 (onlyFields ["affected","reason"]) verdict)
      affected<-field 422 verdict "affected" :: IO Bool
      reason<-parseValue 422 (textFieldP "reason" 8000) verdict
      guard
      locked s $ DB.connection s $ \db->SQL.withTransaction db $ do
       (definition,latest,inspection)<-inspectionOn db pin identity
       currentWorks<-readWorksOn db
       current<-maybe (failure 409 "Kit inspection Workload disappeared") pure (find ((==runIdText (runId running)) . D.workloadId) currentWorks)
       require (D.workField current "executionGate"==toJSON ExecutionOpen) 409 "Kit inspection stopped before receipt"
       Effects.requireDefinitionClearOn db definition
       require (kitLocalEvidence definition latest (get inspection "reference") currentWorks==at request "localEvidence") 409 "Local fault evidence changed during Kit inspection; the verdict is stale"
       let records=[if at record "id"==identity then mergeFields [("status",str "completed"),("verdict",verdict),("reason",str reason)] record else record | record<-items latest "kitInspections"]
           ownKind=at latest "kind"=="kit_inspection"
           phase=if affected then "waiting_preparation" else if ownKind then "inspection_complete" else at latest "kitReturnPhase"
           assessed=mergeFields [("kitInspections",arr records),("status",str phase)] latest
           repaired=if affected then mergeFields [("kind",str "implementation_defect"),("lastFailure",obj [("kitIssue",get inspection "issue"),("reason",str reason),("scope",str "consumer_applicability")]) ] assessed else assessed
           recordDefinition=if affected then put "activeCode" (mergeFields [("status",str "suspect"),("reason",str reason)] (get definition "activeCode")) definition else definition
       void (DB.saveRoutineOn db (putRepair recordDefinition repaired))
       DB.putValueOn db DB.Workloads (D.workloadId current) (toJSON current {D.workCalls=fmap (\call->call {D.callProgress=D.FinishedCall (D.Answered reason)}) (D.workCalls current)})
      pure (reason,[])
 execute `catch` (\(problem::SomeException)->case fromException problem :: Maybe Provider.RetryDeferred of Just _->throwIO problem; Nothing->finishUnknown problem)

admitKitInspection :: Service -> R.Definition -> Value -> IO ()
admitKitInspection s definition saved=do
 works<-D.readWorks s
 let waiting=find ((=="queued") . (`at` "status")) (items saved "kitInspections")
 forM_ waiting $ \inspection->do
  let reference=get inspection "reference"
  source<-maybe (failure 409 "Pinned Kit consumer preparation disappeared") pure (find ((==at reference "preparation") . D.workloadId) works)
  version<-decodeValue 409 reference >>= D.resolveCandidateBytes (AgentId (R.definitionOwner definition)) source
  identity<-RunId <$> newId
  time<-now
  let context=obj [("selfId",str (R.definitionOwner definition)),("consumer",select ["id","owner","goal","criteria","contractGeneration"] (toJSON definition)),("reference",reference),("version",select ["source","sourceHash","language","runtime","dependencies","capabilities","inputBinding"] version),("fixtures",get (D.workField source "preparation") "fixtures"),("issue",get inspection "cause")]
      pin=casePin (toJSON definition) saved
      request=mergeFields [("context",context),("localEvidence",str (kitLocalEvidence (toJSON definition) saved reference works))] (select ["id","issue","reference","contractGeneration","revision"] inspection)
  DB.connection s $ \db->SQL.withTransaction db $ do
   (current,latest)<-loadRepairOn db pin
   require (at latest "status"=="waiting_kit_inspection" && get (get current "activeCode") "reference"==reference && not (has latest "userStop")) 409 "Kit inspection changed before admission"
   require (not (any ((=="reserved") . (`at` "status")) (items latest "reservations"))) 409 "Existing repair work owns the allowance"
   let call=D.newCall identity (AgentId (R.definitionOwner definition)) Nothing (D.QueuedAt D.Preparation) "Inspect this consumer's applicability of a shared suspect artifact"
       origin=obj [("kitInspection",toJSON identity)]
       work=D.Workload (runIdText identity) False (call :| []) (KM.fromList [("origin",origin),("repairPin",pin),("repairInvocationPolicy",toJSON KitInspectionResponseOnly),("kitInspection",request),("executionGate",toJSON ExecutionOpen),("created",str time)])
       records=[if get record "id"==get inspection "id" then put "run" (toJSON identity) record else record | record<-items latest "kitInspections"]
   SQL.execute db "INSERT INTO workloads(id,value) VALUES (?,?)" (D.workloadId work,jsonText (toJSON work))
   Managed.enrollOn db (R.definitionOwner definition) (newTurn identity Chat "Inspect shared suspect Kit applicability" time origin False) Null "Inspect shared suspect Kit applicability" []
   void (DB.saveRoutineOn db (putRepair current (mergeFields [("status",str "inspecting_kit"),("kitInspections",arr records)] latest)))

-- Offers are provenance, not activation or another consumer's accepted result.
refreshKitOffers :: Service -> IO ()
refreshKitOffers s=do
 definitions<-R.readDefinitions s
 works<-D.readWorks s
 forM_ definitions $ \target->forM_ (items (toJSON target) "repairCases") $ \saved->forM_ (items saved "kitInspections") $ \inspection->do
  let issue=get inspection "cause"; owner=R.definitionOwner target
  forM_ [source | source<-definitions,R.definitionOwner source==owner] $ \source->forM_ [repaired | repaired<-items (toJSON source) "repairCases",at repaired "status"=="recovered",get issue "reference" `elem` items repaired "references",get repaired "recoveredReference"/=get issue "reference"] $ \repaired->do
   let offered=get repaired "recoveredReference"
       key=canonicalHash (obj [("consumer",str (R.definitionId target)),("binding",get inspection "reference"),("contractGeneration",get inspection "contractGeneration"),("offered",offered),("issue",get inspection "issue")])
   unless (any ((==key) . (`at` "id")) (items saved "kitOffers")) $ do
    let offer=obj [("id",str key),("reference",offered),("expectedReference",get inspection "reference"),("contractGeneration",get inspection "contractGeneration"),("issue",get inspection "issue"),("sourceRoutine",str (R.definitionId source)),("sourceCase",get repaired "id"),("reason",str "The source consumer independently accepted this repair; this consumer still requires its own checks and authorized audit"),("status",str "offered")]
    forM_ (find ((==at offered "preparation") . D.workloadId) works) $ \candidate->do
     version<-decodeValue 409 offered >>= D.resolveCandidateBytes (AgentId owner) candidate
     unless (isJust (Kit.issueFor owner version (map toJSON works))) (updateRepairUnlocked s (casePin (toJSON target) saved) (appendItems "kitOffers" [offer]))

-- Called from the existing pulse dispatcher, never from a read/projection.
dispatchRepairs :: Service -> IO ()
dispatchRepairs s = do
 settleStoppedWorkflows s
 ensureKitInspections s
 promoteKitInspections s
 refreshKitOffers s
 definitions<-R.readDefinitions s
 forM_ definitions $ \definition->forM_ (filter (not . repairTerminal) (items (toJSON definition) "repairCases")) $ \saved->do
  let pin=casePin (toJSON definition) saved
  result<-try (advanceRepair s definition saved) :: IO (Either APIError ())
  case result of
   Right ()->pure ()
   Left (APIError _ reason)->updateRepairUnlocked s pin (stopRepair "interrupted" reason)

advanceRepair :: Service -> R.Definition -> Value -> IO ()
advanceRepair s definition saved = do
 let raw=toJSON definition; pin=casePin raw saved
 works<-D.readWorks s
 let findWork field_=find ((==at saved field_) . D.workloadId) works
     finished work=all ((`elem` [D.CallCompleted,D.CallFailed,D.CallUnresolved,D.CallInterrupted,D.CallCancelled]) . D.callState) (D.workCalls work)
     success work=all ((==D.CallCompleted) . D.callState) (D.workCalls work)
     ongoing=any ((=="reserved") . (`at` "status")) (items saved "reservations")
     sources=[find ((==at evidence "run") . D.workloadId) works | evidence<-items saved "evidence"]
     ready=all (maybe False terminal) sources && not (any repairWorkActive [work | work<-works,D.workField work "repairPin"==pin])
 if R.definitionDeleted definition || get raw "contractGeneration"/=get saved "contractGeneration" then updateRepairUnlocked s pin (stopRepair "stale" "Routine contract or source profile changed; retained repair allowance is not reset")
 else unless ongoing $ if int (get saved "spentMilliseconds")>=int (get saved "limitMilliseconds") then updateRepairUnlocked s pin (stopRepair "exhausted" "The case's aggregate active repair allowance is exhausted") else case at saved "status" of
  "waiting_preparation"->when ready (admitReplacement s definition saved)
  "waiting_audit"->when ready (admitSavedAudit s definition saved)
  "waiting_kit_inspection"->admitKitInspection s definition saved
  "auditing_saved_result"->forM_ (findWork "auditRun") $ \work->when (finished work) $ do
   let result=D.workField work "savedAuditResult"
   if success work && get result "accepted"==Bool True then updateRepairUnlocked s pin (mergeFields [("status",str "recovered"),("recoveredReference",get result "reference"),("verification",mergeFields [("kind",str "saved_result_audit"),("auditRun",str (D.workloadId work))] result),("terminalReason",str "The exact original saved result passed independent assessment without re-execution or delivery replay")])
   else if at result "verdict"=="rejected" then updateRepairUnlocked s pin (mergeFields [("status",str "waiting_preparation"),("kind",str "implementation_defect"),("lastFailure",result)])
   else updateRepairUnlocked s pin (stopRepair "interrupted" "Saved result audit did not establish recovery; no provider replay is authorized")
  "repairing"->forM_ (findWork "candidate") $ \work->when (finished work) $ do
   let prepared=D.workField work "preparation"; reference=get (get prepared "version") "reference"
   if at prepared "status"=="awaiting_occurrence" && success work then updateRepairUnlocked s pin $ \latest->
    mergeFields [("status",str "awaiting_verification"),("currentCandidate",reference),("references",arr (items latest "references"++[reference]))] latest
   else failedIteration s pin work (obj [("preparation",str (D.workloadId work)),("status",get prepared "status"),("error",get prepared "error"),("checks",get prepared "checks"),("review",get prepared "review")])
  "awaiting_verification"->forM_ (findWork "candidate") $ \candidate->do
   let trial=get (D.workField candidate "preparation") "trial"
   forM_ (find ((==at trial "run") . D.workloadId) works) $ \work->do
    updateRepairUnlocked s pin (mergeFields [("verificationRun",str (D.workloadId work)),("status",str "verifying")])
  "verifying"->forM_ (findWork "verificationRun") $ \work->when (finished work) $ do
   let code=D.workField work "codeExecution"; activation=get code "activation"
   if at saved "auditSource"==D.workloadId work && null (items code "faults") && at (get code "acceptance") "verdict" `notElem` ["rejected","unknown"] then updateRepairUnlocked s pin (put "status" (str "waiting_audit"))
   else if at activation "status"=="activated" && not (boolean (get code "completedViaFallback")) then updateRepairUnlocked s pin
     (mergeFields [("status",str "recovered"),("recoveredReference",get code "reference"),("terminalReason",str "Checked replacement passed an authorized occurrence and its activation audit")])
   else failedIteration s pin work (obj [("verification",str (D.workloadId work)),("acceptance",get code "acceptance"),("activation",activation),("faults",get code "faults")])
  _->pure ()

admitSavedAudit :: Service -> R.Definition -> Value -> IO ()
admitSavedAudit s definition saved = do
 let pin=casePin (toJSON definition) saved
 requireActive s (R.definitionOwner definition)
 forM_ (items saved "evidence") $ \evidence->Effects.requireRunClear s (RunId (at evidence "run"))
 identity<-RunId <$> newId
 time<-now
 DB.connection s $ \db->SQL.withTransaction db $ do
  (current,latest)<-loadRepairOn db pin
  require (at latest "status"=="waiting_audit" && not (has latest "userStop")
   && not (any ((`elem` ["reserved","uncertain"]) . (`at` "status")) (items latest "reservations"))) 409 "Saved audit authority changed or prior resource is uncertain"
  (_,_,proof)<-savedAuditSourceOn db pin (at latest "auditSource")
  let owner=AgentId (R.definitionOwner definition)
      origin=obj [("repairAudit",toJSON identity)]
      call=D.newCall identity owner Nothing (D.QueuedAt D.Preparation) "Audit the exact retained original result without re-execution"
      work=D.Workload (runIdText identity) False (call :| []) (KM.fromList [("origin",origin),("repairPin",pin),("savedAudit",proof),("repairInvocationPolicy",toJSON SavedAuditResponseOnly),("created",str time),("executionGate",toJSON ExecutionOpen),("codeExecution",obj [("operations",arr [])])])
      attempt=obj [("number",num (length (items latest "attempts")+1)),("kind",str "saved_result_audit"),("audit",toJSON identity),("status",str "auditing")]
  SQL.execute db "INSERT INTO workloads(id,value) VALUES (?,?)" (D.workloadId work,jsonText (toJSON work))
  Managed.enrollOn db (agentIdText owner) (newTurn identity Chat "Audit the retained original result" time origin False) Null "Audit the retained original result" []
  void (DB.saveRoutineOn db (putRepair current (appendItems "attempts" [attempt] (mergeFields [("status",str "auditing_saved_result"),("auditRun",toJSON identity),("phase",str "saved_result_audit")] latest))))

-- Regeneration enters the same checked preparation pipeline as a new version.
-- The source occurrence must finish before its retained effects authorize reuse.
admitReplacement :: Service -> R.Definition -> Value -> IO ()
admitReplacement s definition saved = do
 let pin=casePin (toJSON definition) saved
 requireActive s (R.definitionOwner definition)
 forM_ (items saved "evidence") $ \evidence->Effects.requireRunClear s (RunId (at evidence "run"))
 identity<-RunId <$> newId
 time<-now
 DB.connection s $ \db->SQL.withTransaction db $ do
  (current,latest)<-loadRepairOn db pin
  require (at latest "status"==at saved "status" && at latest "status"=="waiting_preparation" && not (has latest "userStop")) 409 "Preparation authority changed"
  Effects.requireDefinitionClearOn db current
  works<-readWorksOn db
  require (not (any repairWorkActive [work | work<-works,D.workField work "repairPin"==pin])
   && not (any ((`elem` ["reserved","uncertain"]) . (`at` "status")) (items latest "reservations"))) 409 "Previous preparation is active or uncertain"
  typed<-decodeValue 500 current
  candidate<-Composer.repairOn db typed (get latest "rootReference") pin (select ["evidence","attempts","lastFailure","kitOffers"] latest) identity time
  let attempt=obj [("number",num (length (items latest "attempts")+1)),("preparation",candidate),("status",str "preparing")]
  void (DB.saveRoutineOn db (putRepair current (appendItems "attempts" [attempt] (mergeFields [("status",str "repairing"),("candidate",candidate),("waitingReason",Null)] latest))))

failedIteration :: Service -> Value -> D.Workload -> Value -> IO ()
failedIteration s pin work evidence = do
 let prepared=D.workField work "preparation"; code=D.workField work "codeExecution"
     operations=items prepared "operations"++items code "operations"
     unknown=any (\op->at op "status" `elem` ["failed","interrupted","uncertain","running"] && (get op "purpose"/=str "fixture_check" && get op "purpose"/=str "repair_fixture_check") && not (Effects.completeActionFree (get op "evidence"))) operations
     proven=any ((==Bool False) . (`get` "matched")) (items prepared "checks") || get (get prepared "review") "accepted"==Bool False
       || at (get code "acceptance") "verdict"=="rejected" || get (get code "activationAudit") "accepted"==Bool False
     stopped=at (toJSON work) "executionGate"=="stopped" || any ((`elem` [D.CallCancelled,D.CallInterrupted]) . D.callState) (D.workCalls work)
 updateRepairUnlocked s pin $ \saved->
  let attemptCount=length (items saved "attempts")
      attempts=[if index==attemptCount then mergeFields [("status",str "failed"),("failure",evidence)] attempt else attempt | (index,attempt)<-zip [1..] (items saved "attempts")]
      retained=put "attempts" (arr attempts) saved
  in if stopped || unknown || not proven then stopRepair "interrupted" "Repair stopped or model response is uncertain; no automatic provider retry is authorized" retained
     else mergeFields [("status",str (preparationPending saved)),("lastFailure",evidence)] retained

-- Purpose-indexed receipts are a read projection of existing operation owners.
repairProjection :: R.Definition -> [D.Workload] -> Value
repairProjection definition works = case find ((==get raw "repairCase") . (`get` "id")) (items raw "repairCases") of
 Nothing->Null
 Just saved->
  let pin=casePin raw saved
      owned=[work | work<-works,get (D.workField work "repairPin") "case"==get pin "case",get (D.workField work "repairPin") "routine"==get pin "routine"]
      entries=items saved "operations"++[mergeFields [("run",str (D.workloadId work)),("purpose",str (if "repair_" `T.isPrefixOf` at op "purpose" then at op "purpose" else (if has (toJSON work) "preparation" then "repair_" else "repair_verification_")<>at op "purpose"))] op
       | work<-owned,op<-items (D.workField work "preparation") "operations"++items (D.workField work "codeExecution") "operations"]
      measured=sum [int (get reservation "usedMilliseconds") | reservation<-items saved "reservations",at reservation "status"=="settled"]
      reserved=sum [int (get reservation "milliseconds") | reservation<-items saved "reservations",at reservation "status" `elem` ["reserved","uncertain"]]
      remaining=max 0 (int (get saved "limitMilliseconds")-int (get saved "spentMilliseconds"))
      grantBlocker=if at saved "status"/="exhausted" || remaining>0 then "Only exhausted active repair resource is eligible" else if has saved "userStop" then "Repair was explicitly stopped" else if any ((`elem` ["reserved","uncertain"]) . (`at` "status")) (items saved "reservations") || any repairWorkActive owned then "Prior repair work is active or uncertain" else if get saved "contractGeneration"/=get raw "contractGeneration" then "Routine instructions changed" else if any (\work->D.workField work "routineEffectUnknown"/=Null) [work | work<-works,at (D.workField work "repairPin") "routine"==R.definitionId definition || at (D.workField work "origin") "routine"==R.definitionId definition] then "Unknown consumer effects require reconciliation" else ""
  in mergeFields [("remainingMilliseconds",num remaining),("grantBlocker",str grantBlocker),("canGrant",Bool (T.null grantBlocker)),("providerWait",providerWaitProjection entries),("usage",arr (concatMap Provider.operationUsage (nubBy (\left right->get left "id"==get right "id") entries))),("measuredMilliseconds",num measured),("reservedMilliseconds",num reserved)] saved
 where raw=toJSON definition
