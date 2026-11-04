{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module WorkflowCalls (fork, ready, child, childData, yielding, openBudget, activity, used, cap, finishBudget, quiesce, allowed, trace, publish, container, safeIdle, recover, stop, defer, dispatchWaits) where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.Aeson hiding (pairs)
import qualified Database.SQLite.Simple as SQL
import Data.IORef
import Data.Time.Clock (getCurrentTime,diffUTCTime)
import Data.List (find)
import qualified Data.List.NonEmpty as NE
import Data.Maybe
import qualified Data.Map.Strict as M
import Data.Text (Text)
import GHC.Clock
import Types
import qualified Delegation as D
import qualified Store as DB
import qualified ManagedState as Managed
import qualified GeneratedCode as Code
import qualified Population
import qualified Effects
import qualified Workflow

childData :: D.WorkCall -> Value
childData call=D.callField call "workflowChild"
child :: D.WorkCall -> Bool
child call=childData call/=Null

change :: Service -> RunId -> (D.Workload -> D.Workload) -> IO ()
change s identity transform=locked s $ do
 (work,_)<-D.getWork s identity
 D.saveWork s (transform work)

trace :: D.Workload -> Value -> Value
trace work context=if get context "fixture"==Null then get (D.workField work "codeExecution") "workflowRun" `fallback` obj [] else
 get (get (D.workField work "preparation") "workflowFixtures") (at context "fixtureKey") `fallback` obj []

publish :: Value -> Text -> Value -> D.Workload -> D.Workload
publish context identity receipt work=
 let current=trace work context
     updated=put "nodes" (put identity receipt (get current "nodes" `fallback` obj [])) current
 in if get context "fixture"==Null then D.setWorkField "codeExecution" (put "workflowRun" updated (D.workField work "codeExecution")) work else
 let prepared=D.workField work "preparation" in D.setWorkField "preparation" (put "workflowFixtures" (put (at context "fixtureKey") updated (get prepared "workflowFixtures" `fallback` obj [])) prepared) work

container :: Run -> Value -> D.Workload -> D.Workload
container running receipt work=D.setWorkField "workflowContainers" (put (runIdText (runId running)) receipt (D.workField work "workflowContainers" `fallback` obj [])) work

allowed :: Service -> Run -> IO ()
allowed s running=do
 (work,_)<-D.getWork s (runId running)
 stopped<-readIORef (runCancel running)
 require (not stopped && D.workField work "executionGate"==toJSON ExecutionOpen && not (has (toJSON work) "workflowHalt")) 409 "Responsible workflow is stopped"
 DB.requireOccurrence s (D.workField work "origin")
 when (has (toJSON work) "repairPin") $ DB.repairPinAllowed s (D.workField work "repairPin") >>= \clear->require clear 409 "Responsible repair case changed"
 let budget=D.workField work "workflowBudget"
 require (at budget "status"=="reserved") 409 "Inherited workflow resource is uncertain or settled"

fork :: Service -> Run -> Value -> Value -> Value -> Text -> Value -> IO Value
fork s running version input context nodeId node=do
 allowed s running
 identities<-mapM (const newId) (items node "children")
 time<-now
 (original,_)<-D.getWork s (runId running)
 result<-locked s $ DB.connection s $ \db->SQL.withTransaction db $ do
  rows<-DB.queryRowsOn db "SELECT value FROM workloads WHERE id=?" ["value"] [SQL.SQLText (D.workloadId original)]
  work<-case rows of [row]->DB.decodeStored (at row "value") >>= decodeValue 500; _->failure 409 "Workflow root is unavailable"
  validateOn db work version context
  parent<-maybe (failure 409 "Workflow parent disappeared") pure (find ((==runId running) . D.callIdentity) (D.workCalls work))
  let key=canonicalHash (obj [("call",toJSON (runId running)),("fixture",get context "fixtureKey"),("node",str nodeId)])
      cohorts=D.workField work "workflowCohorts" `fallback` obj []
      previous=get cohorts key
      request=obj [("version",get version "reference"),("input",input),("context",context),("node",node)]
  if previous/=Null then do
   require (get previous "requestHash"==str (canonicalHash request)) 409 "Fork cohort authority changed"
   let calls=[call | call<-NE.toList (D.workCalls work),toJSON (D.callIdentity call) `elem` items previous "children"]
   require (length calls==length (items previous "children")) 409 "Fork cohort child identity is missing"
   require (all ((`notElem` [D.CallFailed,D.CallInterrupted,D.CallCancelled,D.CallUnresolved]) . D.callState) calls) 409 "Required workflow child is failed or unknown"
   pure (key,all ((==D.CallCompleted) . D.callState) calls)
   else do
    require (D.workField work "executionGate"==toJSON ExecutionOpen && not (has (toJSON work) "workflowHalt")) 409 "Workflow stopped before fork publication"
    let children=zip (map text (items node "children")) identities
        cohort=obj [("parent",toJSON (runId running)),("fork",str nodeId),("join",get node "join"),("children",arr (map (str . snd) children)),("requestHash",str (canonicalHash request)),("status",str "waiting")]
        makeCall (entry,identity)=D.setCallFields [("executionBinding",D.callField parent "executionBinding"),("workflowChild",mergeFields [("entry",str entry),("join",get node "join"),("cohort",str key),("parent",toJSON (runId running)),("version",version),("input",input),("root",str (D.workloadId work))] context)] (D.newCall (RunId identity) (runOwner running) (Just (runId running)) D.Execution D.CallQueued "Run the pinned workflow child region")
        following=map makeCall children
        updated=D.setWorkField "workflowCohorts" (put key cohort cohorts) work {D.workCalls=fmap (\call->if D.callIdentity call==runId running then D.setCallFields [("workflowWait",str key)] (call {D.callState=D.CallQueued}) else call) (D.workCalls work) <> NE.fromList following}
    DB.putValueOn db DB.Workloads (D.workloadId work) (toJSON updated)
    Managed.transitionOn db (runIdText (runId running)) (put "workflowWait" (str key) . put "status" (str "queued"))
    forM_ following $ \call->do
     let origin=put "workflowChild" (childData call) (D.workField work "origin")
         turn=withTurnFields (put "codeBound" (Bool True)) (newTurn (D.callIdentity call) Chat (D.callRequest call) time origin False)
     Managed.enrollOn db (agentIdText (runOwner running)) turn Null (D.callRequest call) []
    pure (key,False)
 activity s running False
 DB.project s (agentIdText (runOwner running))
 if snd result then activity s running True >> pure Null else throwIO (Workflow.Yield (fst result))

ready :: Service -> Turn -> IO Bool
ready s turn=do
 found<-D.findWork s (turnId turn)
 let business=get (turnField turn "origin") "standaloneRun"
     preparation=maybe Null ((`D.workField` "standaloneParent") . fst) found
     request=business `fallback` preparation
 released<-if request==Null then pure True else standaloneReleased s request (maybe (runIdText (turnId turn)) (D.workloadId . fst) found) (business/=Null)
 if not released then pure False else case found of
   Nothing->pure True
   Just (_,call) | D.callField call "standaloneWait"/=Null->do
    works<-D.readWorks s
    let waiting=D.callField call "standaloneWait"
        terminal current=all ((`elem` [D.CallCompleted,D.CallFailed,D.CallInterrupted,D.CallCancelled,D.CallUnresolved]) . D.callState) (D.workCalls current)
    if has waiting "child" then case find ((==at waiting "child") . D.workloadId) works of
     Just current->pure (terminal current)
     Nothing->do
      turns<-DB.turns s
      pure (any (\value->at value "id"==at waiting "child" && at value "status" `elem` ["failed","interrupted","cancelled"]) turns)
    else pure (any (\current->D.workloadId current==at waiting "preparation" && (terminal current || at (D.workField current "preparation") "status" `elem` ["failed","stopped","stale","interrupted","declined"])) works)
   Just (work,call)->
    if (child call || turnField turn "workflowWait"/=Null) && (at (D.workField work "workflowBudget") "status"/="reserved" || has (toJSON work) "workflowHalt") then pure False else case turnField turn "workflowWait" of
     Null->pure True
     key | has (D.workField work "workflowWaits") (text key)->timeReady s work call (text key)
     key->do
      let required=items (get (D.workField work "workflowCohorts") (text key)) "children"
          calls=[candidate | candidate<-NE.toList (D.workCalls work),toJSON (D.callIdentity candidate) `elem` required]
      pure (not (null calls) && all ((`notElem` [D.CallQueued,D.CallRunning,D.CallDelegated]) . D.callState) calls)

standaloneReleased :: Service -> Value -> Text -> Bool -> IO Bool
standaloneReleased s request identity business=do
 works<-D.readWorks s
 runs<-readMVar (serviceRuns s)
 let pin=get request "parent"
     sources=[call | work<-works,D.workloadId work==at pin "workload",call<-NE.toList (D.workCalls work),toJSON (D.callIdentity call)==get pin "call"]
 pure $ case sources of
  [call]->let waiting=D.callField call "standaloneWait" in D.callState call==D.CallQueued && get waiting "operation"==get request "operation" && at waiting (if business then "child" else "preparation")==identity && M.notMember (D.callIdentity call) runs
  _->False

defer :: Service -> Run -> Value -> Value -> Text -> Value -> Value -> IO Value
defer s running version context nodeId node selected=do
 allowed s running
 (original,_)<-D.getWork s (runId running)
 result<-locked s $ DB.connection s $ \db->SQL.withTransaction db $ do
  rows<-DB.queryRowsOn db "SELECT value FROM workloads WHERE id=?" ["value"] [SQL.SQLText (D.workloadId original)]
  work<-case rows of [row]->DB.decodeStored (at row "value") >>= decodeValue 500; _->failure 409 "Deferred root disappeared"
  validateOn db work version context
  wall<-getCurrentTime
  deadline<-maybe (failure 409 "Original business wall deadline is unavailable") pure (parseTime (at (D.workField work "codeExecution") "workflowDeadline"))
  require (wall<deadline) 409 "Original business wall deadline expired"
  let key=canonicalHash (obj [("call",toJSON (runId running)),("node",str nodeId),("structure",get version "structureHash")])
      waits=D.workField work "workflowWaits" `fallback` obj []
      previous=get waits key
      receipt=get (get (trace work context) "nodes") nodeId
      requestHash=get receipt "requestHash"
  require (not (boolean (get receipt "fixtureSimulation")) && requestHash/=Null) 409 "Business timer requires an actual retained admission"
  (due,value)<-if previous==Null then either (failure 422) pure (Workflow.waitSpec wall deadline selected) else do
   require (get previous "requestHash"==requestHash && get previous "selected"==selected && get previous "reference"==get version "reference" && get previous "next"==get node "next" && at previous "status" `elem` ["waiting","completed"]) 409 "Deferred request identity changed or was blocked"
   due<-maybe (failure 409 "Retained wake time is invalid") pure (parseTime (at previous "due"))
   pure (due,get selected "value")
  let record=mergeFields [("call",toJSON (runId running)),("node",str nodeId),("next",get node "next"),("due",str (timestamp due)),("progress",get selected "progress"),("selected",selected),("reference",get version "reference"),("requestHash",requestHash),("version",version)] (if previous==Null then obj [] else previous)
  if wall>=due then do
   let completed=mergeFields [("status",str "completed"),("resumedAt",str (timestamp wall))] record
       nodeReceipt=mergeFields [("status",str "completed"),("output",value),("outputHash",str (canonicalHash value)),("wait",str key)] receipt
   DB.putValueOn db DB.Workloads (D.workloadId work) (toJSON (publish context nodeId nodeReceipt (D.setWorkField "workflowWaits" (put key completed waits) work)))
   pure (Right value)
   else do
    clock<-toInteger <$> getMonotonicTimeNSec
    let budget=tick clock (D.workField work "workflowBudget")
        idle=put "activities" (put (runIdText (runId running)) (Bool False) (get budget "activities")) budget
        waiting=put "status" (str "waiting") record
        nodeReceipt=mergeFields [("status",str "waiting"),("wait",str key)] receipt
        changed=publish context nodeId nodeReceipt (D.setWorkField "workflowBudget" idle (D.setWorkField "workflowWaits" (put key waiting waits) work))
        queued=changed {D.workCalls=fmap (\call->if D.callIdentity call==runId running then D.setCallFields [("workflowWait",str key)] (call {D.callState=D.CallQueued}) else call) (D.workCalls changed)}
    DB.putValueOn db DB.Workloads (D.workloadId work) (toJSON queued)
    Managed.transitionOn db (runIdText (runId running)) (put "workflowWait" (str key) . put "status" (str "queued"))
    pure (Left key)
 DB.project s (agentIdText (runOwner running))
 either (throwIO . Workflow.Yield) pure result

-- Called by the existing Pulse/command lock; readiness is not another timer queue.
timeReady :: Service -> D.Workload -> D.WorkCall -> Text -> IO Bool
timeReady s work call key=do
 wall<-getCurrentTime
 let record=get (D.workField work "workflowWaits") key
     due=parseTime (at record "due")
     deadline=parseTime (at (D.workField work "codeExecution") "workflowDeadline")
 retired<-Population.retired s (agentIdText (D.callOwner call))
 let reason | retired="Owner permissions were revoked before wake"
            | maybe True (<=wall) deadline="Original business wall deadline expired before wake"
            | isNothing due="Retained wake time is invalid"
            | otherwise=""
 if at record "status"/="waiting" then pure False else if reason/="" then block record reason else if maybe False (>wall) due then pure False else do
  result<-try (DB.connection s (\db->SQL.withTransaction db (validateOn db work (get record "version") (obj [])))) :: IO (Either APIError ())
  case result of Right ()->pure True; Left (APIError _ message)->block record message
 where
 block record reason=do
  stop s (D.callIdentity call)
  DB.connection s $ \db->SQL.withTransaction db $ do
   rows<-DB.queryRowsOn db "SELECT value FROM workloads WHERE id=?" ["value"] [SQL.SQLText (D.workloadId work)]
   current<-case rows of [row]->DB.decodeStored (at row "value") >>= decodeValue 500; _->failure 409 "Waiting workflow disappeared"
   let updated=put "workflowWaits" (put key (mergeFields [("status",str "blocked"),("reason",str reason)] record) (D.workField current "workflowWaits")) (toJSON current)
   DB.putValueOn db DB.Workloads (D.workloadId current) updated
   Managed.transitionOn db (runIdText (D.callIdentity call)) (put "status" (str "cancelled") . put "error" (str reason))
  DB.project s (agentIdText (D.callOwner call))
  pure False

dispatchWaits :: Service -> IO ()
dispatchWaits s=do
 works<-D.readWorks s
 forM_ works $ \work->forM_ (pairs (D.workField work "workflowWaits")) $ \(key,record)->when (at record "status"=="waiting") $
  forM_ (find ((==get record "call") . toJSON . D.callIdentity) (D.workCalls work)) $ \call->when (D.callState call==D.CallQueued) (void (timeReady s work call key))

validateOn :: SQL.Connection -> D.Workload -> Value -> Value -> IO ()
validateOn db work version context=do
 require (D.workField work "executionGate"==toJSON ExecutionOpen && not (has (toJSON work) "workflowHalt")) 409 "Workflow stopped before cohort admission"
 let reference=get version "reference"
 rows<-DB.queryRowsOn db "SELECT value FROM workloads WHERE id=?" ["value"] [SQL.SQLText (at reference "preparation")]
 source<-case rows of [row]->DB.decodeStored (at row "value"); _->failure 409 "Pinned workflow preparation disappeared"
 require (get (get source "preparation") "version"==version && at reference "digest"==canonicalHash (remove ["reference"] version)) 409 "Pinned workflow version changed before fork admission"
 if get source "standalonePin"/=Null then do
  works<-DB.queryRowsOn db "SELECT value FROM workloads ORDER BY rowid" ["value"] [] >>= traverse (DB.decodeStored . (`at` "value") >=> decodeValue 500)
  void (D.requireStandalonePinFrom works (get source "standalonePin"))
  forM_ [D.workField work "standaloneRun",D.workField work "standaloneParent"] $ \request->when (request/=Null) (void (D.requireStandalonePinFrom works (get request "parent")))
  require (at source "executionGate"/="stopped") 409 "Standalone preparation stopped before child admission"
 else do
  definitions<-DB.queryRowsOn db "SELECT value FROM scheduled_routines ORDER BY sequence" ["value"] [] >>= mapM (DB.decodeStored . (`at` "value"))
  let pin=D.workField work "preparationPin" `fallback` D.workField work "routineSnapshot"
      routine=get pin "routine" `fallback` get pin "id"
  definition<-maybe (failure 409 "Workflow consumer disappeared") pure (find (\value->get value "id"==routine && get value "owner"==get reference "owner") definitions)
  require (not (boolean (get definition "deleted")) && get definition "contractGeneration"==get version "contractGeneration") 409 "Workflow consumer changed before child admission"
  Effects.requireDefinitionClearOn db definition
  when (has (toJSON work) "repairPin") $ do
   let repair=D.workField work "repairPin"
   saved<-maybe (failure 409 "Workflow repair case disappeared") pure (find ((==get repair "case") . (`get` "id")) (items definition "repairCases"))
   require (not (has saved "userStop") && at saved "status" `elem` ["repairing","verifying","awaiting_verification","diagnosing","diagnosed","waiting_doctor"] && any (\receipt->at receipt "operation"==D.workloadId work && at receipt "status"=="reserved") (items saved "reservations")) 409 "Shared repair reservation is no longer active"
 when (get context "fixture"==Null) $ do
  let prepared=get source "preparation"
  require (at prepared "status"=="awaiting_occurrence" && get (get prepared "review") "accepted"==Bool True && not (null (items prepared "checks")) && all ((==Bool True) . (`get` "matched")) (items prepared "checks")) 409 "Workflow no longer has its own checked acceptance"

yielding :: Service -> Run -> IO ()
yielding s running=DB.project s (agentIdText (runOwner running))

-- The case/preparation reservation remains the authority. This is its measured
-- activity union, scoped to one live host incarnation; crash never refunds it.
openBudget :: Service -> Run -> Int -> IO ()
openBudget s running allowance=do
 clock<-getMonotonicTimeNSec
 change s (runId running) $ \work->if at (D.workField work "workflowBudget") "status"=="reserved" then work else
  D.setWorkField "workflowBudget" (obj ([("history",arr (items (D.workField work "workflowBudget") "history"++[remove ["history"] (D.workField work "workflowBudget") | D.workField work "workflowBudget"/=Null])),("token",str (D.workloadId work)),("status",str "reserved"),("allowance",num allowance),("usedNanoseconds",num 0),("lastTick",toJSON clock),("activities",obj [])])) work
 activity s running True

tick :: Integer -> Value -> Value
tick clock budget=let previous=toInteger (int (get budget "lastTick")); active=any ((==Bool True) . snd) (pairs (get budget "activities"))
 in mergeFields [("lastTick",toJSON (max clock previous)),("usedNanoseconds",toJSON (toInteger (int (get budget "usedNanoseconds"))+if active then max 0 (clock-previous) else 0))] budget

activity :: Service -> Run -> Bool -> IO ()
activity s running active=locked s $ do
 clock<-toInteger <$> getMonotonicTimeNSec
 (work,_)<-D.getWork s (runId running)
 let budget=tick clock (D.workField work "workflowBudget")
 when (at budget "status"=="reserved") $ D.saveWork s (D.setWorkField "workflowBudget" (put "activities" (put (runIdText (runId running)) (Bool active) (get budget "activities" `fallback` obj [])) budget) work)

used :: Service -> Run -> IO Int
used s running=locked s $ do
 clock<-toInteger <$> getMonotonicTimeNSec
 (work,_)<-D.getWork s (runId running)
 pure ((int (get (tick clock (D.workField work "workflowBudget")) "usedNanoseconds")+999999) `div` 1000000)

cap :: Service -> Run -> IO Run
cap s running=do
 allowed s running
 activity s running True
 elapsed<-used s running
 (work,_)<-D.getWork s (runId running)
 let remaining=int (get (D.workField work "workflowBudget") "allowance")-elapsed
 require (remaining>0) 409 "Shared workflow active allowance exhausted"
 clock<-getMonotonicTimeNSec
 wall<-getCurrentTime
 let available=if runDeadline running>clock then runDeadline running-clock else 0
     savedDeadline=get (D.workField work "codeExecution") "workflowDeadline"
 wallAvailable<-if savedDeadline==Null then pure (toInteger available) else do
  deadline<-maybe (failure 409 "Invalid original workflow wall deadline") pure (parseTime (text savedDeadline))
  require (deadline>wall) 409 "Original workflow wall deadline expired"
  pure (floor (diffUTCTime deadline wall*1000000000))
 pure running {runDeadline=clock+fromInteger (minimum [toInteger available,toInteger remaining*1000000,wallAvailable])}

finishBudget :: Service -> Run -> IO ()
finishBudget s running=do
 activity s running False
 change s (runId running) (\work->D.setWorkField "workflowBudget" (put "status" (str "settled") (D.workField work "workflowBudget")) work)

safeIdle :: D.Workload -> Bool
safeIdle work=(not (null (pairs (D.workField work "workflowCohorts"))) || any ((=="waiting") . (`at` "status") . snd) (pairs (D.workField work "workflowWaits")))
 && at (D.workField work "workflowBudget") "status"=="reserved"
 && not (any ((==Bool True) . snd) (pairs (get (D.workField work "workflowBudget") "activities")))
 && all ((`elem` [D.CallQueued,D.CallCompleted]) . D.callState) (D.workCalls work)
 && all ((=="removed") . (`at` "status") . snd) (pairs (D.workField work "workflowContainers"))

recover :: Service -> IO ()
recover s=do
 works<-D.readWorks s
 forM_ works $ \work->do
  containers<-forM (pairs (D.workField work "workflowContainers")) $ \(key,value)->
   if at value "status"=="removed" then pure (key,value) else do
    outcome<-try (reconcileContainer value) :: IO (Either APIError ())
    pure (key,case outcome of Right ()->put "status" (str "removed") value; Left (APIError _ reason)->mergeFields [("status",str "cleanup_pending"),("error",str reason)] value)
  let budget=D.workField work "workflowBudget"
      unsafe=at budget "status"=="reserved" && not (safeIdle work)
      updated=D.setWorkField "workflowContainers" (obj containers) (if unsafe then D.setWorkField "workflowBudget" (mergeFields [("status",str "uncertain"),("reason",str "Host stopped with an open activity; resource remains consumed")] budget) work else work)
  when (not (null containers) || unsafe) (D.saveWork s updated)
 where reconcileContainer=Code.reconcile

quiesce :: Service -> Run -> IO ()
quiesce s running=do
 (original,_)<-D.getWork s (runId running)
 let pending=any (\call->child call && D.callState call/=D.CallCompleted) (D.workCalls original)
 when pending $ change s (runId running) $ \current->D.setWorkField "workflowHalt" (str "Parent finalization prevents descendant admission") current {D.workCalls=fmap (\call->if child call && D.callState call==D.CallQueued then call {D.callState=D.CallCancelled} else call) (D.workCalls current)}
 let waitForExit count=do
      (work,_)<-D.getWork s (runId running)
      let identities=[D.callIdentity call | call<-NE.toList (D.workCalls work),child call]
      runs<-readMVar (serviceRuns s)
      let live=[run | (identity,run)<-M.toList runs,identity `elem` identities,identity/=runId running]
      mapM_ (\run->writeIORef (runCancel run) True) live
      if null live then pure () else if count<=0 then failure 503 "Workflow descendants have not confirmed exit; reservation remains consumed" else threadDelay 20000 >> waitForExit (count-1)
 waitForExit (500::Int)

-- Caller owns the existing command lock. The durable gate is revoked before
-- live flags; queued descendants cannot slip into admission while it propagates.
stop :: Service -> RunId -> IO ()
stop s identity=do
 found<-D.findWork s identity
 forM_ found $ \(work,_)->unless (null (pairs (D.workField work "workflowCohorts")) && null (pairs (D.workField work "workflowWaits"))) $ do
  DB.connection s $ \db->SQL.withTransaction db $ do
   let waits=obj [(key,if at value "status"=="waiting" then mergeFields [("status",str "cancelled"),("reason",str "Stopped before wake")] value else value) | (key,value)<-pairs (D.workField work "workflowWaits")]
       changed=D.setWorkField "workflowWaits" waits (D.setWorkField "workflowHalt" (str "Stopped by Admin") (D.setWorkField "executionGate" (toJSON ExecutionStopped) work))
       stopped=changed {D.workCalls=fmap (\call->if D.callState call==D.CallQueued then call {D.callState=D.CallCancelled} else call) (D.workCalls changed)}
   DB.putValueOn db DB.Workloads (D.workloadId work) (toJSON stopped)
   forM_ [call | call<-NE.toList (D.workCalls stopped),child call,D.callState call==D.CallCancelled] $ \call->
    Managed.transitionOn db (runIdText (D.callIdentity call)) (put "status" (str "cancelled"))
  runs<-readMVar (serviceRuns s)
  forM_ (NE.toList (D.workCalls work)) $ \call->forM_ (M.lookup (D.callIdentity call) runs) $ \running->writeIORef (runCancel running) True
