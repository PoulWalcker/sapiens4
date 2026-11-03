{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}
module Automation (dispatch, requestOn, stopOn, projection, detail, preflight, recordRepairFaultOn, withRepairBudget, runDiagnosis, dispatchRepairs, recoverRepairs, repairProjection, selectDoctorResources, configureDoctorAccessOn, recordAuditFailure, savedAuditSourceOn) where

import Control.Exception hiding (handle)
import Control.Monad
import Data.Aeson hiding (pairs)
import Data.Aeson.Types (Parser)
import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as TE
import System.Directory
import System.FilePath
import System.IO
#ifndef mingw32_HOST_OS
import qualified System.Posix.IO as Posix
import qualified System.Posix.Files as Posix
import System.Posix.Types (Fd)
#endif
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
import qualified Provider
import Population (requireActive)
import qualified Population
import qualified GeneratedCode as Code
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
     unless (repairTerminal saved) $ void (DB.saveRoutineOn db (putRepair definition (mergeFields [("kind",str "saved_result_audit"),("auditSource",str (D.workloadId work)),("lastFailure",obj [("stage",str "assessment"),("reason",str reason)])] saved)))

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

-- Resource authority comes only from the explicit Admin command, never model paths.
selectDoctorResources :: Service -> Value -> IO Value
selectDoctorResources s value = do
 paths<-field 400 value "resources" :: IO [Value]
 arr <$> forM paths (\resource->do
  _<-parseValue 400 (onlyFields ["path","access"]) resource
  path<-T.unpack <$> textField resource "path" 8192
  access<-textField resource "access" 10
  require (access `elem` ["read","write"] && isAbsolute path) 400 "Doctor resources need an absolute file path and read or write access"
  let canonical=normalise path
  require (all (`notElem` [".",".."]) (splitDirectories path)) 400 "Doctor resources need a normalized absolute path"
  home<-getHomeDirectory
  protected<-mapM canonicalizePath ([serviceRoot s,home </> ".codex",home </> ".ssh",home </> ".aws",repositoryRoot s </> ".git"]++[repositoryRoot s | access=="write"])
  require (not (any (\root->canonical==root || addTrailingPathSeparator root `isPrefix` canonical) protected)) 403 "Application state and implementation artifacts cannot be Doctor resources"
#ifndef mingw32_HOST_OS
  existing<-filterM doesDirectoryExist protected
  denied<-mapM Posix.getFileStatus existing
  let protect fd=do
       status<-Posix.getFdStatus fd
       require (not (any (\entry->Posix.deviceID entry==Posix.deviceID status && Posix.fileID entry==Posix.fileID status) denied)) 403 "Protected application resources cannot be selected"
  withDoctorDescriptor canonical Posix.ReadOnly protect $ \fd->do
   status<-Posix.getFdStatus fd
   require (Posix.isRegularFile status && Posix.linkCount status==1) 403 "Doctor resources must be regular files without hard links"
   identity<-newId
   handle<-Posix.dup fd >>= Posix.fdToHandle
   _<-bracket (pure handle) hClose readDoctorText
   pure (obj [("id",str identity),("path",str (T.pack canonical)),("access",str access),("device",str (T.pack (show (Posix.deviceID status)))),("inode",str (T.pack (show (Posix.fileID status))))])
#else
  failure 503 "Doctor file access is unsupported on this platform"
#endif
  )
 where isPrefix prefix candidate=take (length prefix) candidate==prefix

#ifndef mingw32_HOST_OS
-- Anchor every path component to its already-open parent; no symlink or
-- renamed pathname can redirect selection into a different directory tree.
withDoctorDescriptor :: FilePath -> Posix.OpenMode -> (Fd -> IO ()) -> (Fd -> IO a) -> IO a
withDoctorDescriptor path mode guardDirectory action =
 (bracket (Posix.openFd "/" Posix.ReadOnly directoryFlags) Posix.closeFd $ \root->walk root (filter (/="/") (splitDirectories path)))
 `catch` (\(_::IOException)->failure 409 "Doctor resource path is unavailable or changed")
 where
 directoryFlags=Posix.defaultFileFlags {Posix.nonBlock=True,Posix.nofollow=True,Posix.directory=True}
 walk parent components=do
  guardDirectory parent
  case components of
   []->failure 400 "Select an existing text file, not a directory"
   [name]->bracket (Posix.openFdAt (Just parent) name mode Posix.defaultFileFlags {Posix.nonBlock=True,Posix.nofollow=True}) Posix.closeFd action
   name:rest->bracket (Posix.openFdAt (Just parent) name Posix.ReadOnly directoryFlags) Posix.closeFd (\child->walk child rest)
#endif

configureDoctorAccessOn :: SQL.Connection -> Value -> Text -> Value -> IO Value
configureDoctorAccessOn db definition identity resources = do
 saved<-maybe (failure 409 "Unknown Doctor case") pure (find ((==identity) . (`at` "id")) (items definition "repairCases"))
 require (at saved "status" `elem` ["waiting_access","waiting_doctor"] && not (any ((=="reserved") . (`at` "status")) (items saved "reservations"))) 409 "Doctor access can change only while this case waits without active work"
 let revision=int (get (get saved "access") "revision")+1
     updated=mergeFields [("access",obj [("revision",num revision),("resources",resources)]),("status",str "waiting_doctor"),("waitingReason",Null)] saved
 void (DB.saveRoutineOn db (put "revision" (num (int (get definition "revision")+1)) (putRepair definition updated)))
 pure (obj [("routine",get definition "id"),("case",str identity),("access",get updated "access")])

-- Opening without truncation and checking the descriptor before content IO
-- preserves the selected resource identity even if its pathname is replaced.
withDoctorFile :: Value -> Bool -> (Handle -> IO a) -> IO a
withDoctorFile resource writing action = do
 require (not writing || at resource "access"=="write") 403 "Doctor has no write authority for this resource"
#ifndef mingw32_HOST_OS
 withDoctorDescriptor (T.unpack (at resource "path")) (if writing then Posix.ReadWrite else Posix.ReadOnly) (const (pure ())) $ \fd->do
  status<-Posix.getFdStatus fd
  require (Posix.isRegularFile status && Posix.linkCount status==1
    && at resource "device"==T.pack (show (Posix.deviceID status)) && at resource "inode"==T.pack (show (Posix.fileID status))) 409 "Doctor resource identity changed; select the file again"
  bracket (Posix.dup fd >>= Posix.fdToHandle) hClose action
#else
 failure 503 "Doctor file access is unsupported on this platform"
#endif

readDoctorText :: Handle -> IO Text
readDoctorText handle = do
 hSeek handle AbsoluteSeek 0
 bytes<-BS.hGet handle 262145
 require (BS.length bytes<=262144) 413 "Doctor file exceeds 256 KiB"
 content<-either (const (failure 415 "Doctor resources must be UTF-8 text")) pure (TE.decodeUtf8' bytes)
 require (not (T.any (=='\0') content)) 415 "Doctor resources must be UTF-8 text"
 pure content

data DoctorAction = ReadDoctorFile Text | StageDoctorFile Text Text | CheckDoctorStage Text Text | ApplyDoctorFile Text Text Text | InspectDoctorRuntime | InspectDoctorCase deriving (Eq,Show)
instance FromJSON DoctorAction where
 parseJSON value=do
  kind<-withObject "Doctor action" (.: "tool") value :: Parser Text
  let identifier key=withObject "Doctor action" (.: key) value
  case kind of
   "read_file"->onlyFields ["tool","resource"] value >> ReadDoctorFile <$> identifier "resource"
   "stage_file"->onlyFields ["tool","name","content"] value >> StageDoctorFile <$> identifier "name" <*> identifier "content"
   "check_stage"->onlyFields ["tool","stage","fixture"] value >> CheckDoctorStage <$> identifier "stage" <*> identifier "fixture"
   "apply_file"->onlyFields ["tool","resource","stage","expectedHash"] value >> ApplyDoctorFile <$> identifier "resource" <*> identifier "stage" <*> identifier "expectedHash"
   "inspect_runtime"->onlyFields ["tool"] value >> pure InspectDoctorRuntime
   "inspect_case"->onlyFields ["tool"] value >> pure InspectDoctorCase
   _->fail "Unknown Doctor tool"

runDoctorAction :: Service -> Run -> Value -> Value -> DoctorAction -> (Value -> IO ()) -> IO Value
runDoctorAction s running pin request action save = do
 (_,saved)<-DB.connection s (\db->loadRepairOn db pin)
 let resource identity=maybe (failure 403 "Doctor resource is not authorized") pure (find ((==identity) . (`at` "id")) (items (get saved "access") "resources"))
     stage identity=maybe (failure 404 "Unknown isolated Doctor stage") pure (find ((==identity) . (`at` "id")) (items saved "stages"))
 case action of
  InspectDoctorCase->pure (select ["id","evidence","attempts","lastFailure"] saved)
  InspectDoctorRuntime->Code.profileJSON <$> Code.available
  ReadDoctorFile identity->do
   selected<-resource identity
   withDoctorFile selected False $ \handle->do
    content<-readDoctorText handle
    pure (obj [("resource",str identity),("content",str content),("hash",str (sourceHash content))])
  StageDoctorFile name content->do
   require (not (T.null name) && T.unpack name==takeFileName (T.unpack name) && name `notElem` [".",".."] && not (T.any (`elem` ['\\','/','\0']) name) && not (T.any (=='\0') content) && BS.length (TE.encodeUtf8 content)<=65536) 400 "An isolated stage needs a filename and at most 64 KiB of UTF-8 content"
   let identity=canonicalHash (obj [("name",str name),("content",str content)])
       staged=obj [("id",str identity),("name",str name),("content",str content),("hash",str (sourceHash content))]
   updateRepair s pin (\current->put "stages" (arr (filter ((/=identity) . (`at` "id")) (items current "stages")++[staged])) current)
   pure (remove ["content"] staged)
  CheckDoctorStage identity fixtureHash->do
   selected<-stage identity
   works<-D.readWorks s
   original<-maybe (failure 404 "Original preparation is unavailable") pure (find ((==at (get saved "rootReference") "preparation") . D.workloadId) works)
   fixture<-maybe (failure 403 "Only the saved preparation fixtures are available") pure (find ((==fixtureHash) . canonicalHash) (items (D.workField original "preparation") "fixtures"))
   profile<-Code.available
   nonce<-newId
   let reserved=Code.reservation profile (agentIdText (runOwner running)) (at saved "id") nonce
   save (obj [("runtime",reserved)])
   result<-Code.runFixture profile running reserved (at selected "content") (get fixture "input") (\receipt->save (obj [("runtime",receipt)]))
   pure (obj [("fixture",str fixtureHash),("runtimeResult",result),("expected",get fixture "expected")])
  ApplyDoctorFile identity stageId expectedHash->do
   selected<-resource identity
   staged<-stage stageId
   require (at selected "access"=="write") 403 "Doctor has no write authority for this resource"
   withDoctorFile selected True $ \handle->do
    prior<-readDoctorText handle
    require (sourceHash prior==expectedHash) 409 "The live file changed; read its current content before proposing a write"
    -- This is optimistic conflict detection, not atomic CAS against outside editors.
    locked s $ do
     requireDoctorCurrent s running pin
     Effects.requireRunClear s (runId running)
     stopped<-readIORef (runCancel running)
     require (not stopped) 409 "Doctor stopped before live file admission"
     (current,_)<-D.getWork s (runId running)
     updateRepairUnlocked s pin (\savedCase->put "operations" (arr [if get op "id"==get request "id" then mergeFields [("status",str "admitted"),("liveEffect",Bool True)] op else op | op<-items savedCase "operations"]) savedCase)
     D.saveWork s (D.setWorkField "routineEffectUnknown" (obj [("decisionId",get request "id"),("reason",str "Doctor live file action awaits a confirmed outcome")]) current)
    hSeek handle AbsoluteSeek 0
    BS.hPut handle (TE.encodeUtf8 (at staged "content"))
    hSetFileSize handle (fromIntegral (BS.length (TE.encodeUtf8 (at staged "content"))))
    hFlush handle
    actual<-readDoctorText handle
    require (sourceHash actual==at staged "hash") 409 "Doctor write outcome could not be independently read back"
    let result=obj [("resource",str identity),("hash",str (sourceHash actual)),("confirmed",Bool True)]
    locked s $ DB.connection s $ \db->SQL.withTransaction db $ do
     (currentDefinition,latest)<-loadRepairOn db pin
     void (DB.saveRoutineOn db (putRepair currentDefinition (put "operations" (arr [if get op "id"==get request "id" then mergeFields [("status",str "completed"),("result",result)] op else op | op<-items latest "operations"]) latest)))
     rows<-DB.queryRowsOn db "SELECT value FROM workloads WHERE id=?" ["value"] [SQL.SQLText (runIdText (runId running))]
     current<-maybe (failure 409 "Doctor action owner disappeared") (DB.decodeStored . (`at` "value")) (listToMaybe rows)
     when (get (get current "routineEffectUnknown") "decisionId"==get request "id") $
      DB.putValueOn db DB.Workloads (runIdText (runId running)) (remove ["routineEffectUnknown"] current)
    pure result

-- External diagnosis is parsed into an explicit verdict before any transition.
data RepairVerdict = ImplementationDefect | StructureRevision | RepairDeclined | RepairNeedsAccess deriving (Eq,Show)
instance FromJSON RepairVerdict where
 parseJSON value = case text value of
  "implementation_defect"->pure ImplementationDefect
  "structure_revision"->pure StructureRevision
  "request_access"->pure RepairNeedsAccess
  "stop"->pure RepairDeclined
  _->fail "Expected implementation_defect, structure_revision or stop"

requireDoctorCurrent :: Service -> Run -> Value -> IO ()
requireDoctorCurrent s running pin = do
 stopped<-readIORef (runCancel running)
 require (not stopped) 409 "Doctor was stopped before action admission"
 (definition,saved)<-DB.connection s (\db->loadRepairOn db pin)
 (work,_)<-D.getWork s (runId running)
 require (at saved "status"=="diagnosing" && get saved "diagnosisRun"==toJSON (runId running)
  && get definition "contractGeneration"==get pin "contractGeneration" && not (boolean (get definition "deleted"))
  && at (toJSON work) "executionGate"=="open") 409 "Doctor action is stale or its case stopped"
 config<-Population.corporaSettings s
 require (corporaDoctor config==Just (runOwner running)) 409 "Doctor assignment changed before action admission"
 requireActive s (agentIdText (runOwner running))
 clock<-getMonotonicTimeNSec
 require (clock<runDeadline running) 409 "Doctor active allowance is exhausted"

saveDoctorOperation :: Service -> Value -> Text -> Value -> IO ()
saveDoctorOperation s pin identity fields=updateRepair s pin (\current->put "operations" (arr [if at op "id"==identity then merge op fields else op | op<-items current "operations"]) current)

performDoctorAction :: Service -> Run -> Value -> Value -> IO Value
performDoctorAction s running pin request = do
 action<-decodeValue 422 request
 (_,saved)<-DB.connection s (\db->loadRepairOn db pin)
 let stageHash=maybe Null (`get` "hash") (find ((==at request "stage") . (`at` "id")) (items saved "stages"))
     digest=canonicalHash (obj [("request",request),("access",get saved "access"),("stageHash",stageHash)])
 case find (\op->at request "tool" `elem` ["stage_file","check_stage","apply_file"] && at op "digest"==digest && at op "status"=="completed") (items saved "operations") of
  Just receipt->pure (put "retainedReceipt" (get receipt "id") (get receipt "result"))
  Nothing->do
   identity<-newId
   time<-now
   let operation=obj [("id",str identity),("purpose",str "repair_tool"),("tool",get request "tool"),("request",request),("digest",str digest),("status",str "reserved"),("started",str time),("authorityRevision",get (get saved "access") "revision")]
       save=saveDoctorOperation s pin identity
   locked s $ do
    requireDoctorCurrent s running pin
    updateRepairUnlocked s pin (appendItems "operations" [operation] . put "phase" (str "tool_work"))
   outcome<-try (runDoctorAction s running pin (put "id" (str identity) request) action save) :: IO (Either SomeException Value)
   case outcome of
    Right result->save (obj [("status",str "completed"),("result",result)]) >> pure (put "tool" (get request "tool") result)
    Left problem->do
     (_,latest)<-DB.connection s (\db->loadRepairOn db pin)
     let current=find ((==identity) . (`at` "id")) (items latest "operations")
         live=maybe False (boolean . (`get` "liveEffect")) current
         message=D.exceptionText problem
     save (obj [("status",str (if live then "uncertain" else "denied")),("error",str message)])
     case fromException problem :: Maybe APIError of
      Just _ | not live->pure (obj [("tool",get request "tool"),("denied",Bool True),("reason",str message)])
      _->throwIO problem

runDiagnosis :: Service -> Run -> Turn -> IO (Text,[Value])
runDiagnosis s running _ = do
 (work,_)<-D.getWork s (runId running)
 let pin=D.workField work "repairPin"
 template<-prompt s "repair-diagnosis"
 envelope<-prompt s "preparation-envelope"
 let loop observations = do
      locked s (requireDoctorCurrent s running pin)
      (definition,saved)<-DB.connection s (\db->loadRepairOn db pin)
      works<-D.readWorks s
      let original=find ((==at (get saved "rootReference") "preparation") . D.workloadId) works
          prepared=maybe Null (`D.workField` "preparation") original
          context=obj [("selfId",toJSON (runOwner running)),("consumer",select ["owner","id","goal","criteria"] definition)
           ,("case",mergeFields [("latestEvidence",fromMaybe Null (listToMaybe (reverse (items saved "evidence")))),("previousHypotheses",arr [get attempt "hypothesis" | attempt<-items saved "attempts"])] (select ["id","rootReference","limitMilliseconds","spentMilliseconds","lastFailure","access"] saved))
           ,("failedImplementation",get prepared "version"),("fixtures",arr [put "id" (str (canonicalHash fixture)) fixture | fixture<-items prepared "fixtures"])
           ,("stages",arr [remove ["content"] staged | staged<-items saved "stages"]),("observations",arr observations)]
          rendered=render envelope [("node","DoctorDiagnosis"),("instruction",template),("context",jsonText context)]
      require (BL.length (encode context)<=262144) 413 "Required repair evidence exceeds 256 KiB; inspect retained evidence before another repair"
      identity<-newId
      started<-now
      let operation=obj [("id",str identity),("purpose",str "repair_diagnosis"),("status",str "running"),("started",str started),("doctor",toJSON (runOwner running))]
          save=saveDoctorOperation s pin identity
      captured<-newIORef Null
      updateRepair s pin (appendItems "operations" [operation] . put "phase" (str "saved_data_diagnosis"))
      raw<-(Provider.invokeAnalysisObserved s running rendered (\evidence->do
       let trace=toJSON evidence
       writeIORef captured trace
       save (obj [("evidence",trace)])
       when (Effects.positiveAnomaly trace) $ locked s $ do
        (current,_)<-D.getWork s (runId running)
        D.saveWork s (D.setWorkField "routineEffectUnknown" (obj [("decisionId",str identity),("reason",str "Doctor provider trace contains unexpected activity"),("evidence",trace)]) current))) `catch` (\(problem::SomeException)->do
       trace<-readIORef captured
       save (obj [("status",str "failed"),("error",str (D.exceptionText problem)),("usageUnknown",Bool (get (get trace "usage") "inputTokens"==Null || get (get trace "usage") "outputTokens"==Null))])
       throwIO problem)
      trace<-readIORef captured
      when (Effects.positiveAnomaly trace) $ locked s $ do
       (current,_)<-D.getWork s (runId running)
       D.saveWork s (D.setWorkField "routineEffectUnknown" (obj [("decisionId",str identity),("reason",str "Doctor response-only invocation contains unexpected activity"),("evidence",trace)]) current)
      require (Effects.completeActionFree trace) 409 "Doctor response lacks complete action-free terminal evidence; no automatic replay is authorized"
      locked s (requireDoctorCurrent s running pin)
      let response=parseJSONText raw
      save (obj [("status",str "completed"),("response",response)])
      if has response "action" then do
       _<-parseValue 422 (onlyFields ["action"]) response
       observation<-performDoctorAction s running pin (get response "action")
       loop (observations++[observation])
      else do
       verdict<-field 422 response "verdict" :: IO RepairVerdict
       diagnosis<-textField response "diagnosis" 4000
       recommendation<-textField response "recommendation" 8000
       hypothesis<-if verdict==ImplementationDefect then textField response "hypothesis" 8000 else pure ""
       let previous=[at attempt "hypothesis" | attempt<-items saved "attempts",has attempt "hypothesis"]
       require (verdict/=ImplementationDefect || hypothesis `notElem` previous) 422 "A new repair iteration needs a changed hypothesis from the retained failed evidence"
       locked s $ do
        requireDoctorCurrent s running pin
        (current,_)<-D.getWork s (runId running)
        DB.connection s $ \db->SQL.withTransaction db $ do
         (consumerDefinition,latest)<-loadRepairOn db pin
         let diagnosed=case verdict of
              StructureRevision->stopRepair "structure_revision" recommendation (put "diagnosis" response latest)
              RepairDeclined->stopRepair "declined" recommendation (put "diagnosis" response latest)
              RepairNeedsAccess->mergeFields [("status",str "waiting_access"),("diagnosis",response),("waitingReason",str recommendation),("phase",str "waiting_independent_confirmation")] latest
              ImplementationDefect->mergeFields [("status",str "diagnosed"),("diagnosis",response),("hypothesis",str hypothesis)] latest
         void (DB.saveRoutineOn db (putRepair consumerDefinition diagnosed))
         DB.putValueOn db DB.Workloads (D.workloadId current) (toJSON current {D.workCalls=fmap (\call->call {D.callState=D.CallCompleted,D.callOutput=Just diagnosis}) (D.workCalls current)})
       pure (diagnosis,[])
 (_,initial)<-DB.connection s (\db->loadRepairOn db pin)
 let retained=[put "tool" (get op "tool") (get op "result") | op<-items initial "operations",at op "purpose"=="repair_tool",at op "status"=="completed"]
 loop retained `catch` (\(problem::SomeException)->do
  updateRepair s pin (\current->if repairTerminal current then current else stopRepair "interrupted" (D.exceptionText problem) current)
  throwIO problem)

recoverRepairs :: Service -> IO ()
recoverRepairs s = do
 definitions<-R.readDefinitions s
 forM_ definitions $ \definition->forM_ (items (toJSON definition) "repairCases") $ \saved->do
  let pin=casePin (toJSON definition) saved
  -- Only authoritative historical case links with verified response-only
  -- receipts establish absence proof; a bare repairPin never grants it.
  forM_ ((at saved "diagnosisRun",DoctorScopedResponseOnly):[(at attempt "preparation",RepairPreparationResponseOnly) | attempt<-items saved "attempts"]) $ \(identity,policy)->do
   found<-D.findWork s (RunId identity)
   forM_ found $ \(work,_)->when (not (has (toJSON work) "repairInvocationPolicy") && D.workField work "repairPin"==pin) $ do
    let operations=case policy of
         DoctorScopedResponseOnly->[op | op<-items saved "operations",at op "purpose"=="repair_diagnosis"]
         RepairPreparationResponseOnly->[op | op<-items (D.workField work "preparation") "operations",at op "purpose"/= "fixture_check"]
         SavedAuditResponseOnly->[]
        proven op=let evidence=get op "evidence" in at evidence "policy"=="response_only" && at evidence "providerVersion"=="codex-cli 0.162.1" && not (Effects.positiveAnomaly evidence)
    when (not (null operations) && all proven operations) (D.saveWork s (D.setWorkField "repairInvocationPolicy" (toJSON policy) work))
  forM_ [op | op<-items saved "operations",has op "runtime",at (get op "runtime") "status"/="removed"] $ \op->do
   Code.reconcile (get op "runtime")
   updateRepairUnlocked s pin (\current->put "operations" (arr [if get entry "id"==get op "id" then put "runtime" (put "status" (str "removed") (get entry "runtime")) entry else entry | entry<-items current "operations"]) current)
  let unfinished=[op | op<-items saved "operations",at op "status" `elem` ["running","reserved","admitted"]]
  when (any (Effects.positiveAnomaly . (`get` "evidence")) (items saved "operations")) $ do
   found<-D.findWork s (RunId (at saved "diagnosisRun"))
   forM_ found $ \(work,_)->when (D.workField work "routineEffectUnknown"==Null) (D.saveWork s (D.setWorkField "routineEffectUnknown" (obj [("reason",str "Doctor provider trace contains unexpected or incomplete activity")]) work))
  forM_ [op | op<-unfinished,boolean (get op "liveEffect")] $ \op->do
   found<-D.findWork s (RunId (at saved "diagnosisRun"))
   forM_ found $ \(work,_)->D.saveWork s (D.setWorkField "routineEffectUnknown" (obj [("decisionId",get op "id"),("reason",str "Doctor live file action was interrupted without a confirmed outcome")]) work)
  when (any ((=="reserved") . (`at` "status")) (items saved "reservations")) $
   updateRepairUnlocked s pin $ \current->
    stopRepair "interrupted" "Host stopped during active repair; unfinished reservation remains consumed and spend is unknown"
     (mergeFields [("usageUnknown",Bool True),("reservations",arr [if at r "status"=="reserved" then put "status" (str "uncertain") r else r | r<-items current "reservations"])
      ,("operations",arr [if at op "status" `elem` ["running","reserved","admitted"] then mergeFields [("status",str "uncertain"),("usageUnknown",Bool True)] op else op | op<-items current "operations"])] current)


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
           diagnosisWork=D.Workload (runIdText identity) False (call :| []) (KM.fromList [("origin",origin),("repairPin",pin),("repairInvocationPolicy",toJSON DoctorScopedResponseOnly),("created",str time),("executionGate",toJSON ExecutionOpen)])
       SQL.execute db "INSERT INTO workloads(id,value) VALUES (?,?)" (D.workloadId diagnosisWork,jsonText (toJSON diagnosisWork))
       Managed.enrollOn db (agentIdText doctor) (newTurn identity Chat "Diagnose the retained Routine implementation fault" time origin False) Null "Diagnose the retained Routine implementation fault" []
       void (DB.saveRoutineOn db (putRepair current (mergeFields [("status",str "diagnosing"),("doctor",toJSON doctor),("diagnosisRun",toJSON identity),("waitingReason",Null)] latest)))
  "diagnosing"->forM_ (findWork "diagnosisRun") $ \work->when (finished work && not (success work)) (updateRepairUnlocked s pin (stopRepair "interrupted" "Diagnosis did not finish; no provider replay is authorized"))
  "diagnosed"->case findWork "diagnosisRun" of
   Just diagnosisWork | finished diagnosisWork && success diagnosisWork->do
    Effects.requireRunClear s (RunId (D.workloadId diagnosisWork))
    forM_ (items saved "evidence") $ \evidence->Effects.requireRunClear s (RunId (at evidence "run"))
    audit<-try (DB.connection s (\db->savedAuditSourceOn db pin (at saved "auditSource"))) :: IO (Either APIError (D.Workload,Value,Value))
    case audit of
     Right (source,_,proof)->do
      identity<-RunId <$> newId
      time<-now
      DB.connection s $ \db->SQL.withTransaction db $ do
       (current,latest)<-loadRepairOn db pin
       (_,_,currentProof)<-savedAuditSourceOn db pin (D.workloadId source)
       require (currentProof==proof && at latest "status"=="diagnosed") 409 "Saved audit changed before admission"
       let owner=AgentId (R.definitionOwner definition)
           origin=obj [("repairAudit",toJSON identity)]
           call=D.newCall identity owner Nothing D.Preparation D.CallQueued "Audit the exact retained original result without re-execution"
           work=D.Workload (runIdText identity) False (call :| []) (KM.fromList [("origin",origin),("repairPin",pin),("savedAudit",proof),("repairInvocationPolicy",toJSON SavedAuditResponseOnly),("created",str time),("executionGate",toJSON ExecutionOpen),("codeExecution",obj [("operations",arr [])])])
           attempt=obj [("number",num (length (items latest "attempts")+1)),("hypothesis",get latest "hypothesis"),("kind",str "saved_result_audit"),("audit",toJSON identity),("status",str "auditing")]
       SQL.execute db "INSERT INTO workloads(id,value) VALUES (?,?)" (D.workloadId work,jsonText (toJSON work))
       Managed.enrollOn db (agentIdText owner) (newTurn identity Chat "Audit the retained original result" time origin False) Null "Audit the retained original result" []
       void (DB.saveRoutineOn db (putRepair current (appendItems "attempts" [attempt] (mergeFields [("status",str "auditing_saved_result"),("auditRun",toJSON identity),("phase",str "saved_result_audit")] latest))))
     Left (APIError _ reason) | at saved "kind"=="saved_result_audit"->updateRepairUnlocked s pin (stopRepair "interrupted" reason)
     Left _->do
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
  "auditing_saved_result"->forM_ (findWork "auditRun") $ \work->when (finished work) $ do
   let result=D.workField work "savedAuditResult"
   if success work && get result "accepted"==Bool True then updateRepairUnlocked s pin (mergeFields [("status",str "recovered"),("recoveredReference",get result "reference"),("verification",mergeFields [("kind",str "saved_result_audit"),("auditRun",str (D.workloadId work))] result),("terminalReason",str "The exact original saved result passed independent assessment without re-execution or delivery replay")])
   else if at result "verdict"=="rejected" then updateRepairUnlocked s pin (mergeFields [("status",str "waiting_doctor"),("kind",str "implementation_defect"),("lastFailure",result)])
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
   if at saved "auditSource"==D.workloadId work && null (items code "faults") && at (get code "acceptance") "verdict" `notElem` ["rejected","unknown"] then updateRepairUnlocked s pin (put "status" (str "waiting_doctor"))
   else if at activation "status"=="activated" && not (boolean (get code "completedViaFallback")) then updateRepairUnlocked s pin
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
  in mergeFields [("usage",arr (nubBy (\left right->get left "id"==get right "id") entries)),("measuredMilliseconds",num measured),("reservedMilliseconds",num reserved)] saved
 where raw=toJSON definition
