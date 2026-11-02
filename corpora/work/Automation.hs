{-# LANGUAGE OverloadedStrings #-}
module Automation (dispatch, requestOn, stopOn, projection, detail, preflight) where

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
import qualified Delegation as D
import qualified Routines as R
import qualified Provider
import Population (requireActive)

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
       ,("source",summary source),("positiveRuns",toJSON (map D.workloadId accepted)),("negativeFeedback",arr feedback)
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
