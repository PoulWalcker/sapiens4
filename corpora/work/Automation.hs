{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Automation (dispatch, requestOn, stopOn, projection, detail, preflight, recordRepairFaultOn, withRepairBudget, runDiagnosis, dispatchRepairs, recoverRepairs, repairProjection) where

import Control.Exception
import Control.Monad
import Data.Aeson
import qualified Data.ByteString.Lazy as BL
import qualified Data.Aeson.KeyMap as KM
import Data.List (find)
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
import qualified Provider
import Population (requireActive)
import qualified Population
import qualified Composer
import GHC.Clock

preflight :: IO ()
preflight = void Provider.analysisCatalog

consumer :: R.Definition -> D.Workload -> Bool
consumer definition work = at (D.workField work "analysisPin") "routine"==R.definitionId definition
 && get (D.workField work "analysisPin") "owner"==str (R.definitionOwner definition)
currentAnalysis :: R.Definition -> D.Workload -> Bool
currentAnalysis definition work = consumer definition work && get (D.workField work "analysisPin") "contractGeneration"==toJSON (R.contractGeneration definition)
ordinary :: R.Definition -> D.Workload -> Bool
ordinary definition work = at (D.workField work "origin") "routine"==R.definitionId definition
 && D.callOwner (NE.head (D.workCalls work))==AgentId (R.definitionOwner definition)
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
active work = any ((`elem` [D.CallQueued,D.CallRunning]) . D.callState) (D.frontierCalls work)

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
       ,("previousDecision",maybe Null (`D.workField` "analysisDecision") prior)
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

requestOn :: SQL.Connection -> R.Definition -> RunId -> Text -> Bool -> IO Value
requestOn db candidate identity time explicit = do
 definitions <- DB.queryRowsOn db "SELECT value FROM scheduled_routines ORDER BY sequence" ["value"] [] >>= traverse (DB.decodeStored . (`at` "value") >=> decodeValue 500)
 definition <- maybe (failure 409 "Routine is no longer available for analysis") pure
  (find (\value -> R.definitionId value==R.definitionId candidate && R.definitionOwner value==R.definitionOwner candidate && not (R.definitionDeleted value)) definitions)
 require (R.contractGeneration definition==R.contractGeneration candidate) 409 "Routine instructions changed before analysis admission"
 works <- readWorksOn db
 operations <- DB.routineOperationsOn db
 let analyses=filter (currentAnalysis definition) works
     allAnalyses=filter (consumer definition) works
 require (not (any active allAnalyses)) 409 "An analysis is already queued or running for this Routine"
 let positives=[work | work<-works,currentOrdinary definition work,positive operations work]
     prior=listToMaybe (reverse analyses)
     historical=[work | work<-works,ordinary definition work,at (D.workField work "outcome") "status"=="Completed"]
     priorSource=prior >>= (\work -> find ((==at (get (D.workField work "analysisEvidence") "source") "id") . D.workloadId) works)
     candidates=reverse positives++maybeToList priorSource++[work | explicit,work<-reverse historical]
 source <- maybe (failure 409 "Analysis needs a saved ordinary result; run this Routine first") pure (listToMaybe candidates)
 let key=evidenceKey definition operations works
 unless explicit $ require (not (any (\work -> D.workField work "analysisKey"==key) analyses)) 409 "This evidence already has an analysis reservation"
 evidence <- either (failure 413) pure (selectEvidence definition operations works source prior)
 let pin=AnalysisPin (AgentId (R.definitionOwner definition)) (R.definitionId definition) (R.contractGeneration definition)
       (DB.analysisFeedbackIds operations (toJSON definition) (map toJSON works) [RunId (D.workloadId source)]) [RunId (D.workloadId source)]
     origin=obj [("analysis",toJSON identity)]
     turn=newTurn identity Chat (R.definitionGoal definition) time origin False
     call=D.newCall identity (analysisOwner pin) Nothing D.AutomationAnalysis D.CallQueued (R.definitionGoal definition)
     work=D.Workload (runIdText identity) False (call :| []) (KM.fromList
       [("origin",origin),("created",str time),("originalIntent",str (R.definitionGoal definition)),("executionGate",toJSON ExecutionOpen)
       ,("analysisPin",toJSON pin),("analysisKey",key),("analysisEvidence",evidence),("analysisExplicit",Bool explicit),("decisions",arr [])])
 SQL.execute db "INSERT INTO workloads(id,value) VALUES (?,?)" (D.workloadId work,jsonText (toJSON work))
 Managed.enrollOn db (R.definitionOwner definition) turn Null (R.definitionGoal definition) []
 when (has (toJSON definition) "analysisNotice") $ void (DB.saveRoutineOn db (remove ["analysisNotice"] (toJSON definition)))
 pure (obj [("routine",str (R.definitionId definition)),("analysis",toJSON identity),("status",str "queued")])

stopOn :: SQL.Connection -> R.Definition -> RunId -> IO Value
stopOn db definition identity = do
 works <- readWorksOn db
 work <- maybe (failure 404 "Unknown Routine analysis") pure (find (\work -> consumer definition work && D.workloadId work==runIdText identity) works)
 require (active work) 409 "Analysis is already finished"
 let stopped=(D.setWorkField "executionGate" (toJSON ExecutionStopped) work)
       {D.workCalls=fmap (\call -> if D.callState call `elem` [D.CallQueued,D.CallRunning] then call {D.callState=D.CallCancelled} else call) (D.workCalls work)}
 DB.putValueOn db DB.Workloads (D.workloadId work) (toJSON stopped)
 pure (obj [("routine",str (R.definitionId definition)),("analysis",toJSON identity),("status",str "stopped")])

dispatch :: Service -> IO ()
dispatch s = do
 definitions <- R.readDefinitions s
 works <- D.readWorks s
 operations <- DB.routineOperations s
 verification <- newIORef Nothing
 let verified = do
      prior <- readIORef verification
      result <- case prior of
       Just saved -> pure saved
       Nothing -> do
        checked <- try preflight
        writeIORef verification (Just checked)
        pure checked
      either (throwIO :: APIError -> IO ()) pure result
 forM_ definitions $ \definition -> when (not (R.definitionDeleted definition) && not ("group_" `T.isPrefixOf` R.definitionOwner definition)) $ do
  let analyses=filter (currentAnalysis definition) works
      latest=listToMaybe (reverse analyses)
      key=evidenceKey definition operations works
      observations=[work | work<-works,currentOrdinary definition work,positive operations work]
      previousDecision=listToMaybe (reverse [work | work<-analyses,has (toJSON work) "analysisDecision"])
      needsMore=maybe False ((=="NeedMoreEvidence") . (`at` "status") . (`D.workField` "analysisDecision")) previousDecision
      latestFailed=maybe False (\work -> D.callState (NE.last (D.workCalls work)) `elem` [D.CallFailed,D.CallInterrupted,D.CallUnresolved]
        || at (toJSON work) "executionGate"=="stopped") latest
      already=any ((==key) . (`D.workField` "analysisKey")) analyses
      notice=get (toJSON definition) "analysisNotice"
      eligible=(null analyses && not (null observations)) || (not (null analyses) && needsMore && not latestFailed)
  when (eligible && not already && not (any active (filter (consumer definition) works)) && get notice "key"/=key) $ do
   result <- try $ do
    requireActive s (R.definitionOwner definition)
    verified
    identity <- RunId <$> newId
    time <- now
    DB.connection s $ \db -> SQL.withTransaction db $ void (requestOn db definition identity time False)
   case result of
    Right () -> pure ()
    Left (APIError code reason) -> unless (code==409) $ DB.connection s $ \db -> SQL.withTransaction db $ do
     stored <- DB.queryRowsOn db "SELECT value FROM scheduled_routines ORDER BY sequence" ["value"] [] >>= traverse (DB.decodeStored . (`at` "value"))
     forM_ (findId (R.definitionId definition) stored) $ \current -> do
      latestWorks <- readWorksOn db
      latestOperations <- DB.routineOperationsOn db
      currentDefinition <- decodeValue 500 current
      when (not (R.definitionDeleted currentDefinition) && evidenceKey currentDefinition latestOperations latestWorks==key) $
       void (DB.saveRoutineOn db (put "analysisNotice" (obj [("key",key),("status",str "blocked"),("reason",str reason)]) current))

projection :: Service -> R.Definition -> [D.Workload] -> IO Value
projection s definition works = do
 operations <- DB.routineOperations s
 let notice=get (toJSON definition) "analysisNotice"
     currentKey=evidenceKey definition operations works
 if get notice "key"==currentKey then pure (remove ["key"] notice) else
  case listToMaybe (reverse (filter (consumer definition) works)) of
   Nothing -> pure Null
   Just work -> renderAnalysis s definition work False

detail :: Service -> Text -> Text -> Text -> IO Value
detail s owner routine identity = do
 definitions <- R.readDefinitions s
 definition <- maybe (failure 404 "Unknown personal Routine") pure (find (\d -> R.definitionOwner d==owner && R.definitionId d==routine) definitions)
 works <- D.readWorks s
 work <- maybe (failure 404 "Unknown Routine analysis") pure (find (\w -> consumer definition w && D.workloadId w==identity) works)
 renderAnalysis s definition work True

renderAnalysis :: Service -> R.Definition -> D.Workload -> Bool -> IO Value
renderAnalysis s definition work full = do
 allowed <- DB.occurrenceAllowed s (D.workField work "origin")
 let call=NE.last (D.workCalls work)
     decision=D.workField work "analysisDecision"
     stopped=at (toJSON work) "executionGate"=="stopped"
     stale=not allowed && not stopped
     status | stopped = "stopped"
            | stale = "stale"
            | otherwise = case D.callState call of
               D.CallQueued -> "queued"
               D.CallRunning -> "running"
               D.CallCompleted -> at decision "status"
               D.CallFailed -> "failed"
               D.CallInterrupted -> "interrupted"
               _ -> "stopped"
     reason | stopped = "Analysis stopped; saved evidence is preserved"
            | stale = "Analysis belongs to changed instructions or feedback; reconsider using current evidence"
            | D.callState call `elem` [D.CallFailed,D.CallInterrupted] = at (toJSON call) "error"
            | otherwise = at decision "reason"
     evidence=D.workField work "analysisEvidence"
     invocationEvidence=maybe Null (`get` "evidence") (listToMaybe (reverse (D.workItems work "decisions")))
     fields=[("id",str (D.workloadId work)),("status",str status),("reason",str reason),("decision",decision),("purpose",str "automation_analysis")
       ,("owner",str (R.definitionOwner definition)),("routine",str (R.definitionId definition)),("contractGeneration",get (D.workField work "analysisPin") "contractGeneration")
       ,("sourceRuns",arr [get (get evidence "source") "id"]),("created",D.workField work "created")
       ,("usage",get invocationEvidence "usage"),("durationMs",get invocationEvidence "durationMs")]
     details=if full then [("evidence",evidence),("invocations",arr [selectPresent ["decisionId","callId","node","purpose","evidence","error"] entry | entry<-D.workItems work "decisions"])] else []
 pure (mergeFields (fields++details) (selectPresent ["automatablePart","parameters","missingObservations","reconsiderCondition"] decision))

-- A case retains one aggregate allowance. Workloads remain the only executable
-- operations; the case is consumer-owned evidence and authorization history.
repairTerminal :: Value -> Bool
repairTerminal saved = at saved "status" `elem` ["recovered","exhausted","interrupted","stopped","structure_revision","declined","stale"]
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
  let saved=fromMaybe (obj [("id",str identity),("status",str "waiting_doctor"),("created",str time)
        ,("contractGeneration",get snapshot "contractGeneration"),("rootReference",reference),("references",arr [reference])
        ,("limitMilliseconds",num 1800000),("spentMilliseconds",num 0),("reservations",arr []),("attempts",arr []),("operations",arr []),("evidence",arr [])]) prior
      symptoms=items (D.workField work "codeExecution") "faults"
      evidence=obj [("run",str (D.workloadId work)),("reference",reference),("reason",str reason),("faults",arr symptoms),("code",select ["input","runtimeResult","acceptance","hybridResult","evaluatedResult","evaluatedExplanation","fallbacks"] (D.codeProjection (D.workField work "codeExecution"))),("externalExecution",D.workField work "externalExecution")]
      key=canonicalHash evidence
      updated=put "evidence" (arr (if any ((==key) . (`at` "key")) (items saved "evidence") then items saved "evidence" else items saved "evidence"++[put "key" (str key) evidence]))
        (put "references" (arr (if reference `elem` items saved "references" then items saved "references" else items saved "references"++[reference])) saved)
  void (DB.saveRoutineOn db (putRepair base updated))

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
  Just (work,_) | not (has (toJSON work) "repairPin")->action running
  Just (work,_)->mask $ \restore->do
   let pin=D.workField work "repairPin"; token=runIdText (runId running)
   started<-getMonotonicTimeNSec
   allowance<-locked s $ DB.connection s $ \db->SQL.withTransaction db $ do
    (definition,saved)<-loadRepairOn db pin
    require (not (repairTerminal saved) && get definition "contractGeneration"==get pin "contractGeneration" && not (boolean (get definition "deleted"))) 409 "Repair is stopped or its consumer changed"
    require (not (any ((=="reserved") . (`at` "status")) (items saved "reservations"))) 409 "Another repair activity already owns the allowance"
    let remaining=int (get saved "limitMilliseconds")-int (get saved "spentMilliseconds")
    require (remaining>0) 409 "Repair active allowance is exhausted"
    let reservation=obj [("operation",str token),("status",str "reserved"),("milliseconds",num remaining)]
    void (DB.saveRoutineOn db (putRepair definition (appendItems "reservations" [reservation] (put "spentMilliseconds" (get saved "limitMilliseconds") saved))))
    pure remaining
   let capped=running {runDeadline=min (runDeadline running) (started+fromIntegral allowance*1000000)}
       settle=do
        ended<-getMonotonicTimeNSec
        let used=min allowance (max 1 (fromIntegral ((ended-started+999999) `div` 1000000)))
        updateRepair s pin $ \saved->
         let reservations=[if at r "operation"==token && at r "status"=="reserved" then mergeFields [("status",str "settled"),("usedMilliseconds",num used)] r else r | r<-items saved "reservations"]
             measured=put "reservations" (arr reservations) (put "spentMilliseconds" (num (int (get saved "spentMilliseconds")-allowance+used)) saved)
         in if used>=allowance && not (at saved "status"=="recovered") then stopRepair "exhausted" "The case's aggregate active repair allowance is exhausted" measured else measured
   restore (action capped) `finally` settle

-- External diagnosis is parsed into an explicit verdict before any transition.
data RepairVerdict = ImplementationDefect | StructureRevision | RepairDeclined deriving (Eq,Show)
instance FromJSON RepairVerdict where
 parseJSON value = case text value of
  "implementation_defect"->pure ImplementationDefect
  "structure_revision"->pure StructureRevision
  "stop"->pure RepairDeclined
  _->fail "Expected implementation_defect, structure_revision or stop"

runDiagnosis :: Service -> Run -> Turn -> IO (Text,[Value])
runDiagnosis s running _ = do
 (work,_)<-D.getWork s (runId running)
 let pin=D.workField work "repairPin"
 (definition,saved)<-DB.connection s (\db->loadRepairOn db pin)
 works<-D.readWorks s
 let failedImplementation=maybe Null (\source->get (D.workField source "preparation") "version") (find ((==at (get saved "rootReference") "preparation") . D.workloadId) works)
 template<-prompt s "repair-diagnosis"
 envelope<-prompt s "preparation-envelope"
 let context=obj [("selfId",toJSON (runOwner running)),("consumer",select ["owner","id","goal","criteria"] definition),("case",mergeFields [("latestEvidence",fromMaybe Null (listToMaybe (reverse (items saved "evidence")))),("previousHypotheses",arr [get attempt "hypothesis" | attempt<-items saved "attempts"])] (select ["id","rootReference","limitMilliseconds","spentMilliseconds","lastFailure"] saved)),("failedImplementation",failedImplementation)]
     rendered=render envelope [("node","DoctorDiagnosis"),("instruction",template),("context",jsonText context)]
 require (BL.length (encode context)<=262144) 413 "Required repair evidence exceeds 256 KiB; inspect retained evidence before another repair"
 identity<-newId
 started<-now
 let operation=obj [("id",str identity),("purpose",str "repair_diagnosis"),("status",str "running"),("started",str started),("doctor",toJSON (runOwner running))]
     save fields=updateRepair s pin (\current->put "operations" (arr [if at op "id"==identity then merge op fields else op | op<-items current "operations"]) current)
 captured<-newIORef Null
 updateRepair s pin (appendItems "operations" [operation])
 (do
  raw<-Provider.invokeAnalysisObserved s running rendered (\evidence->writeIORef captured (toJSON evidence) >> save (obj [("evidence",toJSON evidence)]))
  trace<-readIORef captured
  when (Effects.positiveAnomaly trace) $ locked s $ do
   forM_ (listToMaybe (items saved "evidence")) $ \fault->do
    (failedWork,_)<-D.getWork s (RunId (at fault "run"))
    D.saveWork s (D.setWorkField "routineEffectUnknown" (obj [("decisionId",str identity),("reason",str "Doctor response-only invocation contains unexpected activity"),("evidence",trace)]) failedWork)
  require (Effects.completeActionFree trace) 409 "Doctor response lacks complete action-free terminal evidence; no automatic replay is authorized"
  DB.requireOccurrence s (D.workField work "origin")
  let response=parseJSONText raw
  verdict<-field 422 response "verdict" :: IO RepairVerdict
  diagnosis<-textField response "diagnosis" 4000
  recommendation<-textField response "recommendation" 8000
  hypothesis<-if verdict==ImplementationDefect then textField response "hypothesis" 8000 else pure ""
  let previous=[at attempt "hypothesis" | attempt<-items saved "attempts",has attempt "hypothesis"]
  require (verdict/=ImplementationDefect || hypothesis `notElem` previous) 422 "A new repair iteration needs a changed hypothesis from the retained failed evidence"
  locked s $ do
   cancelled<-readIORef (runCancel running)
   require (not cancelled) 409 "Doctor diagnosis was stopped"
   DB.requireOccurrence s (D.workField work "origin")
   (current,_)<-D.getWork s (runId running)
   DB.connection s $ \db->SQL.withTransaction db $ do
    (consumerDefinition,latest)<-loadRepairOn db pin
    require (at latest "status"=="diagnosing" && get latest "diagnosisRun"==toJSON (runId running)
     && get consumerDefinition "contractGeneration"==get pin "contractGeneration" && not (boolean (get consumerDefinition "deleted"))
     && at (toJSON current) "executionGate"=="open") 409 "Doctor result is stale or its case stopped"
    let recorded=put "operations" (arr [if at op "id"==identity then mergeFields [("status",str "completed"),("response",response)] op else op | op<-items latest "operations"]) latest
        diagnosed=case verdict of
         StructureRevision->stopRepair "structure_revision" recommendation (put "diagnosis" response recorded)
         RepairDeclined->stopRepair "declined" recommendation (put "diagnosis" response recorded)
         ImplementationDefect->mergeFields [("status",str "diagnosed"),("diagnosis",response),("hypothesis",str hypothesis)] recorded
    void (DB.saveRoutineOn db (putRepair consumerDefinition diagnosed))
    DB.putValueOn db DB.Workloads (D.workloadId current) (toJSON current {D.workCalls=fmap (\call->call {D.callState=D.CallCompleted,D.callOutput=Just diagnosis}) (D.workCalls current)})
  pure (diagnosis,[])) `catch` (\(problem::SomeException)->do
   trace<-readIORef captured
   let unknownUsage=get (get trace "usage") "inputTokens"==Null || get (get trace "usage") "outputTokens"==Null
   save (obj [("status",str "failed"),("error",str (D.exceptionText problem)),("usageUnknown",Bool unknownUsage)])
   updateRepair s pin (\current->if repairTerminal current then current else stopRepair "interrupted" (D.exceptionText problem) current)
   throwIO problem)

recoverRepairs :: Service -> IO ()
recoverRepairs s = do
 definitions<-R.readDefinitions s
 forM_ definitions $ \definition->forM_ (items (toJSON definition) "repairCases") $ \saved->
  when (any ((=="reserved") . (`at` "status")) (items saved "reservations")) $
   updateRepairUnlocked s (casePin (toJSON definition) saved) $ \current->
    stopRepair "interrupted" "Host stopped during active repair; unfinished reservation remains consumed and spend is unknown"
     (mergeFields [("usageUnknown",Bool True),("reservations",arr [if at r "status"=="reserved" then put "status" (str "uncertain") r else r | r<-items current "reservations"])
      ,("operations",arr [if at op "status"=="running" then mergeFields [("status",str "uncertain"),("usageUnknown",Bool True)] op else op | op<-items current "operations"])] current)


 -- Upgrade existing broken bindings without replaying any historical work.
 works<-D.readWorks s
 forM_ definitions $ \definition->do
  let activeCode=get (toJSON definition) "activeCode"
      faults=[work | work<-works,at (D.workField work "origin") "routine"==R.definitionId definition
       ,get (D.workField work "routineSnapshot") "owner"==str (R.definitionOwner definition)
       ,not (null (items (D.workField work "codeExecution") "faults"))
       ,get activeCode "reference"==get (D.workField work "codeExecution") "reference" && at activeCode "status"=="suspect"
       ,not (any (\saved->get activeCode "reference" `elem` items saved "references") (items (toJSON definition) "repairCases"))]
  forM_ faults $ \work->DB.connection s $ \db->SQL.withTransaction db $
   recordRepairFaultOn db work (get (D.workField work "codeExecution") "reference") "Retained implementation fault from an earlier application version"

-- Called from the existing pulse dispatcher, never from a read/projection.
dispatchRepairs :: Service -> IO ()
dispatchRepairs s = do
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
 if R.definitionDeleted definition || get raw "contractGeneration"/=get saved "contractGeneration" then updateRepairUnlocked s pin (stopRepair "stale" "Routine contract or source profile changed; retained repair allowance is not reset")
 else unless ongoing $ if int (get saved "spentMilliseconds")>=int (get saved "limitMilliseconds") then updateRepairUnlocked s pin (stopRepair "exhausted" "The case's aggregate active repair allowance is exhausted") else case at saved "status" of
  "waiting_doctor"->do
   config<-Population.corporaSettings s
   case corporaDoctor config of
    Nothing->updateRepairUnlocked s pin (put "waitingReason" (str "No Doctor is assigned; safe ordinary execution remains available"))
    Just doctor->do
     chief<-DB.mainAgent s
     require (agentIdText doctor/=chief && not ("group_" `T.isPrefixOf` agentIdText doctor)) 400 "Assign an active personal specialist as Doctor"
     requireActive s (agentIdText doctor)
     identity<-RunId <$> newId
     time<-now
     DB.connection s $ \db->SQL.withTransaction db $ do
      (current,latest)<-loadRepairOn db pin
      when (at latest "status"=="waiting_doctor") $ do
       let origin=obj [("repair",toJSON identity)]
           call=D.newCall identity doctor Nothing D.Preparation D.CallQueued "Diagnose the retained Routine implementation fault"
           diagnosisWork=D.Workload (runIdText identity) False (call :| []) (KM.fromList [("origin",origin),("repairPin",pin),("created",str time),("executionGate",toJSON ExecutionOpen)])
       SQL.execute db "INSERT INTO workloads(id,value) VALUES (?,?)" (D.workloadId diagnosisWork,jsonText (toJSON diagnosisWork))
       Managed.enrollOn db (agentIdText doctor) (newTurn identity Chat "Diagnose the retained Routine implementation fault" time origin False) Null "Diagnose the retained Routine implementation fault" []
       void (DB.saveRoutineOn db (putRepair current (mergeFields [("status",str "diagnosing"),("doctor",toJSON doctor),("diagnosisRun",toJSON identity),("waitingReason",Null)] latest)))
  "diagnosing"->forM_ (findWork "diagnosisRun") $ \work->when (finished work && not (success work)) (updateRepairUnlocked s pin (stopRepair "interrupted" "Diagnosis did not finish; no provider replay is authorized"))
  "diagnosed"->case findWork "diagnosisRun" of
   Just diagnosisWork | finished diagnosisWork && success diagnosisWork->do
    Effects.requireRunClear s (RunId (D.workloadId diagnosisWork))
    forM_ (items saved "evidence") $ \evidence->Effects.requireRunClear s (RunId (at evidence "run"))
    identity<-RunId <$> newId
    time<-now
    DB.connection s $ \db->SQL.withTransaction db $ do
     (current,latest)<-loadRepairOn db pin
     when (at latest "status"=="diagnosed") $ do
      typed<-decodeValue 500 current
      candidate<-Composer.repairOn db typed (get latest "rootReference") pin (select ["diagnosis","evidence","attempts"] latest) identity time
      let attempt=obj [("number",num (length (items latest "attempts")+1)),("hypothesis",get latest "hypothesis"),("preparation",candidate),("status",str "preparing")]
      void (DB.saveRoutineOn db (putRepair current (appendItems "attempts" [attempt] (mergeFields [("status",str "repairing"),("candidate",candidate)] latest))))
   _->pure ()
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
   if at activation "status"=="activated" && not (boolean (get code "completedViaFallback")) then updateRepairUnlocked s pin
     (mergeFields [("status",str "recovered"),("recoveredReference",get code "reference"),("terminalReason",str "Checked replacement passed an authorized occurrence and its activation audit")])
   else failedIteration s pin work (obj [("verification",str (D.workloadId work)),("acceptance",get code "acceptance"),("activation",activation),("faults",get code "faults")])
  _->pure ()

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
     else mergeFields [("status",str "waiting_doctor"),("lastFailure",evidence)] retained

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
  in mergeFields [("usage",arr entries),("measuredMilliseconds",num measured),("reservedMilliseconds",num reserved)] saved
 where raw=toJSON definition
