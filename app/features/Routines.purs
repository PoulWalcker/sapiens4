module Sapiens.Routines (view) where

import Prelude
import Data.Argonaut.Core (Json)
import Data.Array as A
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String.CodeUnits as CU
import Libs4.Browser as B
import Sapiens.Action (Action(..))
import Sapiens.Data as D
import Sapiens.Html as V
import Sapiens.Model as M
import Sapiens.Text as T

data Routine = Managed Json | Legacy Json

record :: Routine -> Json
record (Managed r) = r
record (Legacy r) = r

managed :: Routine -> Boolean
managed (Managed _) = true
managed _ = false

title :: Routine -> String
title routine = D.fallback (D.s "prompt" $ record routine) $ D.s "title" $ record routine

active :: Json -> Boolean
active run = A.elem (D.s "status" run) ["queued","running","waiting"]

status :: String -> String
status = case _ of
  "done" -> "Completed"
  "running" -> "Running"
  "queued" -> "Queued"
  "waiting" -> "Needs your reply"
  "failed" -> "Needs attention"
  "interrupted" -> "Interrupted"
  "unresolved" -> "Unresolved"
  "cancelled" -> "Stopped"
  "stopped" -> "Stopped"
  other -> other

view :: M.Model -> V.Html
view m = V.node "section" [V.cls "routine-ui",V.label "Routines"] $
  if M.isGroup (M.selected m) then [V.textNode "h1" [] "Routines",V.p "routine-muted" "Personal routines belong to an individual Sapi."] else content <> scheduleFeedback
  where
  state = M.routineView m
  saved = A.filter (\r -> D.s "owner" r == m.selected) $ D.rows "personal_routines" m.snapshot
  old = A.filter (\r -> D.s "owner" r == m.selected && not (A.any (\s -> D.s "id" s == D.s "id" r) saved)) $ D.rows "routines" m.snapshot
  routines = map Managed saved <> map Legacy old
  find id = A.find ((id == _) <<< D.s "id" <<< record) routines
  link caption page = V.button "routine-link" caption $ RoutineNavigate page
  errors = map (\op -> V.p "routine-problem" $ D.s "error" $ D.get "response" op) $ A.filter (\op -> D.s "owner" op == m.selected) $ D.rows "routine_errors" m.snapshot
  badge value = V.span [V.cls "routine-status"] value
  interval routine = D.s "trigger" (record routine) == "interval" || not (managed routine)
  whenText routine = frequency <> next
    where
    r = record routine
    minutes = D.int $ D.get "minutes" r
    frequency = if interval routine then "Every " <> show minutes <> (if minutes == 1 then " minute" else " minutes") else "On request"
    next = if D.b "deleted" r then " · No future runs" else if not (interval routine) then "" else if D.b "paused" r then " · Schedule paused" else " · Next: " <> formatDate (D.s "nextDue" r)
  formatDate value = if value == "" then "Time unavailable" else B.formatDate "en-GB" (D.json {day:"numeric",month:"short",hour:"2-digit",minute:"2-digit",second:"2-digit"}) value
  timestamp run = D.fallback (D.s "created" $ fromMaybe D.empty $ A.find (\t -> D.s "id" t == D.s "id" run) $ D.rows "turns" m.snapshot) $ D.s "created" run
  triggerLabel run = case D.s "trigger" run of
    "manual" -> "Manual run"
    "interval" -> "Scheduled run"
    _ -> "Trigger not recorded"
  runs routine = case routine of
    Managed r -> D.rows "runs" r
    Legacy r -> map legacyRun $ A.filter (\w -> D.str (D.at ["origin","routine"] w) == D.s "id" r) $ D.rows "workloads" m.snapshot
  legacyRun work = D.json {id:D.get "id" work,status:case D.s "state" work of
    "Completed" -> "done"
    "Queued" -> "queued"
    "Running" -> "running"
    "WaitingForAdmin" -> "waiting"
    "Unresolved" -> "unresolved"
    "Cancelled" -> "cancelled"
    "Interrupted" -> "interrupted"
    _ -> "failed",result:D.get "output" work,error:D.get "error" work,currentOwner:D.get "owner" work}
  latest routine = case A.find active $ A.reverse $ runs routine of
    Just current -> Just current
    Nothing -> A.last $ runs routine
  routineStatus routine = if D.b "deleted" $ record routine then "Deleted" else case latest routine of
    Just run | active run || A.elem (D.s "status" run) ["failed","unresolved","interrupted"] -> status $ D.s "status" run
    _ -> if not (interval routine) then "Ready" else if D.b "paused" $ record routine then "Paused" else "Enabled"
  menu routine = V.node "details" [V.cls "routine-menu"]
    [V.node "summary" [V.label $ "Manage " <> title routine] [V.text "⋯"]
    ,V.div "" $ if D.b "deleted" $ record routine then [] else case routine of
      Managed r -> [link "Settings" $ M.RoutineSettings (D.s "id" r)] <> scheduleControl routine <> [V.button "routine-link" "Delete…" $ OpenDialog M.RoutineDelete r]
      Legacy r -> [V.button "routine-link" "Edit through chat" $ PrepareDraft ("Edit @" <> D.s "id" r <> " ( " <> D.s "title" r <> " )\nEdit Frequency (prev every " <> show (D.int $ D.get "minutes" r) <> " minutes): new every XXX minutes\nEdit Execution Prompt as: " <> D.s "prompt" r) "XXX",V.button "routine-link" (if D.b "paused" r then "Resume schedule through chat" else "Pause schedule through chat") $ PrepareDraft ((if D.b "paused" r then "Resume Scheduled Routine" else "Pause Scheduled Routine") <> "\n@" <> D.s "id" r) ""]]
  scheduleControl routine = if not (managed routine && interval routine) then [] else
    [V.control [V.cls "routine-link",V.disabled $ case state.scheduleRequest of
      Nothing -> false
      Just request -> request.busy || request.action /= action || D.s "id" request.record /= D.s "id" r]
      (if D.b "paused" r then "Resume schedule" else "Pause schedule") (RoutineScheduleAction action r)]
    where
    r = record routine
    action = if D.b "paused" r then M.ResumeSchedule else M.PauseSchedule
  scheduleFeedback = case state.scheduleRequest of
    Nothing -> []
    Just request -> [V.div "routine-notice" $
      [V.p "" $ "Schedule request: " <> D.s "title" request.record,V.p "" $ if request.busy then "Waiting for confirmation…" else request.error] <>
      if request.busy then [] else [V.button "button" "Retry unchanged" RoutineRetrySchedule,V.button "routine-link" "Refresh current schedule" RoutineRefreshSchedule]]
  runButton routine = case routine of
    Managed r -> V.button "button" "Run now" $ OpenDialog M.RoutineRun r
    Legacy r -> V.button "button" "Run through chat" $ PrepareDraft ("@" <> D.s "id" r <> "\nRun it now.") ""
  heading routine history single =
    [link (if single then "← History" else "← All routines") (if single then M.RoutineHistory id else M.RoutineList false)
    ,V.div "routine-heading routine-page-heading" [V.textNode "h1" [] $ title routine,V.div "routine-actions" $ if D.b "deleted" r then [badge "Deleted"] else [runButton routine] <> scheduleControl routine <> [menu routine]]
    ,V.p "routine-muted" $ whenText routine
    ,V.node "nav" [V.cls "routine-tabs",V.label "Routine views"] [V.control [V.cls $ "routine-link" <> if not history then " selected" else "",V.attr "aria-current" $ if not history then "page" else "false"] "Latest result" (RoutineNavigate $ M.RoutineLatest id),V.control [V.cls $ "routine-link" <> if history then " selected" else "",V.attr "aria-current" $ if history then "page" else "false"] "History" (RoutineNavigate $ M.RoutineHistory id)]
    ,V.when (managed routine) $ repairPanel routine
    ,V.when (managed routine) $ analysisPanel routine
    ,V.when (managed routine) $ codePanel routine]
    where r = record routine
          id = D.s "id" r
  analysisPanel routine = V.div "routine-notice" $
    [V.textNode "h3" [] "Automation analysis"
    ,V.p "" $ case current of
      "Ready" -> "Ready for later preparation"
      "NeedMoreEvidence" -> "More evidence needed"
      "Decline" -> "Automation declined"
      "queued" -> "Analysis queued"
      "running" -> "Analysis in progress"
      "stale" -> "Analysis needs reconsideration"
      "blocked" -> "Analysis unavailable"
      "failed" -> "Analysis failed"
      "interrupted" -> "Analysis interrupted"
      "stopped" -> "Analysis stopped"
      _ -> "Awaiting an accepted result"
    ,V.when (D.s "reason" analysis /= "") $ V.p "routine-muted" $ D.s "reason" analysis] <>
    (if current == "Ready" then [V.p "" $ D.s "automatablePart" analysis,V.p "routine-muted" "Preparation and activation are tracked separately below."] <> map (\parameter -> V.p "routine-muted" $ D.s "name" parameter <> ": " <> D.s "description" parameter) (D.rows "parameters" analysis) else []) <>
    (if current == "NeedMoreEvidence" then map (V.p "" <<< D.str) (D.rows "missingObservations" analysis) else []) <>
    (if current == "Decline" then [V.p "" $ D.s "reconsiderCondition" analysis] else []) <>
    [V.when (D.s "id" analysis /= "") $ V.node "details" [] [V.node "summary" [] [V.text "Analysis usage"],V.p "routine-muted" $ if D.b "conflict" usage then "Purpose: automation analysis · Usage unknown: provider reports conflict." else "Purpose: automation analysis · Input tokens: " <> known "inputTokens" usage <> " · Output tokens: " <> known "outputTokens" usage,V.p "routine-muted" $ "Duration (ms): " <> known "durationMs" analysis <> " · Price: Unknown"]] <>
    (if D.b "deleted" r then [] else if A.elem current ["queued","running"] then
      [V.button "routine-link" "Stop analysis…" $ OpenDialog M.RoutineStopAnalysis $ D.put "analysis" (D.get "id" analysis) r]
     else if A.null (runs routine) then [] else [V.button "routine-link" "Reconsider automation…" $ OpenDialog M.RoutineAnalyze r])
    where
    r = record routine
    analysis = D.get "analysis" r
    current = D.s "status" analysis
    usage = D.get "usage" analysis
  codePanel routine = V.when (D.s "id" prepared /= "" || D.s "digest" reference /= "") $ V.div "routine-notice"
    [V.textNode "h3" [] "Script automation"
    ,V.p "" $ case D.s "status" active of
      "active" -> "Script active · " <> CU.take 8 (D.s "digest" reference)
      "suspect" -> "Script suspended"
      "stale" -> "Script needs reconsideration"
      _ -> if D.s "id" prepared /= "" && not (D.b "applicable" prepared) then "Prepared version needs reconsideration" else if D.s "run" trial /= "" then if A.elem (D.s "status" trialRun) ["queued", "running", "waiting", "blocked"] then "Trial claimed" else "Trial finished without activation" else if D.s "status" prepared == "awaiting_occurrence" then "Prepared · awaiting an authorized run" else "Preparation: " <> D.s "status" prepared
    ,providerWaitPanel $ D.get "providerWait" prepared
    ,V.when (D.s "status" active == "stale" || D.s "id" prepared /= "" && not (D.b "applicable" prepared)) $ V.p "routine-muted" "This historical version no longer applies to the current task or source profile. Its history is retained."
    ,V.when (D.s "digest" (D.get "reference" prepared) /= "") $ V.p "routine-muted" $ "Prepared version · " <> CU.take 8 (D.s "digest" (D.get "reference" prepared))
    ,V.when (D.s "run" active /= "") $ V.p "routine-muted" $ "Activated by run " <> D.s "run" active <> " · " <> formatDate (D.s "decidedAt" active)
    ,V.when (D.s "reason" active /= "") $ V.p "routine-muted" $ D.s "reason" active
    ,V.when (D.s "status" trial == "not_activated") $ V.p "routine-muted" $ D.s "reason" trial]
    where
    r = record routine
    prepared = D.get "preparation" r
    active = D.get "activeCode" r
    reference = D.get "reference" active
    trial = D.get "trial" prepared
    trialRun = fromMaybe D.empty $ A.find (\run -> D.s "id" run == D.s "run" trial) (D.rows "runs" r)
  providerWaitPanel waiting = V.when (D.s "id" waiting /= "") $ V.div "routine-notice"
    [V.p "" $ "Provider: " <> D.s "status" waiting <> " · host attempts " <> show (D.int $ D.get "attemptCount" waiting) <> "/3"
    ,V.p "routine-muted" $ D.s "reason" waiting
    ,V.when (D.s "retryAt" waiting /= "") $ V.p "routine-muted" $ "Resume after " <> formatDate (D.s "retryAt" waiting)
    ,V.p "routine-muted" "The original maintenance allowance is retained. Waiting uses no active slot. Provider spend may be unknown."]
  repairPanel routine = V.when (D.s "id" repair /= "") $ V.div "routine-notice"
    [V.textNode "h3" [] "Doctor repair"
    ,V.p "routine-muted" "Doctor can inspect selected text files, stage isolated repairs and run isolated checks. Live writes require explicit file access and clear effect receipts."
    ,V.p "routine-muted" $ case D.s "phase" repair of
      "tool_work" -> "Doctor is performing a scoped tool operation."
      "waiting_independent_confirmation" -> "Waiting for file access or independent confirmation."
      _ -> "Doctor diagnoses saved evidence."
    ,V.when (A.elem (D.s "status" repair) ["waiting_access","waiting_doctor"]) $ V.button "routine-link" "Doctor file access…" $ OpenDialog M.DoctorAccess $ record routine
    ,V.node "details" [] ([V.node "summary" [] [V.text "Doctor access and actions"]] <> map (\resource -> V.p "routine-muted" $ D.s "access" resource <> " · " <> D.s "path" resource) (D.rows "resources" $ D.get "access" repair) <> map (\op -> V.p "routine-muted" $ D.s "tool" op <> " · " <> D.s "status" op <> " · " <> D.s "error" op) (A.filter (\op -> D.s "purpose" op == "repair_tool") $ D.rows "operations" repair))
    ,providerWaitPanel $ D.get "providerWait" repair
    ,V.p "" $ "Repair: " <> D.s "status" repair
    ,V.p "routine-muted" $ "Doctor: " <> D.fallback "Not assigned" (D.s "doctor" repair)
    ,V.p "" $ D.s "waitingReason" repair
    ,V.p "" $ D.s "diagnosis" (D.get "diagnosis" repair)
    ,V.p "routine-muted" $ D.s "recommendation" (D.get "diagnosis" repair)
    ,V.p "routine-muted" $ "Attempts: " <> show (A.length $ D.rows "attempts" repair) <> " · Measured active time (ms): " <> show (D.int $ D.get "measuredMilliseconds" repair) <> " · Reserved (ms): " <> show (D.int $ D.get "reservedMilliseconds" repair) <> " / " <> show (D.n "limitMilliseconds" repair)
    ,V.p "routine-muted" $ "Remaining active repair time (ms): " <> show (D.n "remainingMilliseconds" repair)
    ,V.when (D.s "status" repair == "exhausted") $ V.div "routine-actions"
      [V.control [V.cls "routine-link",V.disabled $ not $ D.b "canGrant" repair] "Continue repair…" $ OpenDialog M.RepairGrant $ record routine
      ,V.p "routine-muted" $ D.s "grantBlocker" repair]
    ,V.when (not $ A.elem (D.s "status" repair) ["recovered","stale","stopped"]) $ V.button "routine-link" "Stop repair…" $ OpenDialog M.RepairStop $ record routine
    ,V.node "details" [] ([V.node "summary" [] [V.text "Repair continuation grants"]] <> map (\grant -> V.p "routine-muted" $ "Added " <> show (D.n "additionalMilliseconds" grant / 60000.0) <> " active minutes · " <> D.s "reason" grant <> " · " <> formatDate (D.s "created" grant)) (D.rows "grants" repair))
    ,V.p "routine-muted" $ "Current candidate: " <> D.fallback "None" (D.s "candidate" repair)
    ,V.p "" $ D.s "terminalReason" repair
    ,V.when (D.b "usageUnknown" repair) $ V.p "routine-problem" "Repair spend is unknown for interrupted activity. Its reserved resource remains consumed."
    ,V.p "routine-muted" "Safe ordinary execution remains available while repair waits or stops."
    ,V.node "details" [] ([V.node "summary" [] [V.text "Repair usage"], V.p "routine-muted" "Unreported tokens and price are unknown."] <> map (\op -> V.p "routine-muted" $ D.s "purpose" op <> " · " <> D.s "status" op <> " · Input tokens: " <> known "inputTokens" (D.get "usage" $ D.get "evidence" op) <> " · Output tokens: " <> known "outputTokens" (D.get "usage" $ D.get "evidence" op) <> " · Duration (ms): " <> known "durationMs" (D.get "evidence" op)) (D.rows "usage" repair))]
    where
    repair = D.get "repair" $ record routine
  result routine run =
    [V.p "routine-muted" $ status (D.s "status" run) <> " · " <> formatDate (timestamp run) <> " · " <> triggerLabel run
    ,V.when (managed routine) $ V.p "routine-muted" $ "Assessment: " <> D.s "status" (D.get "assessment" run) <> if D.b "historical" run then " · Historical revision" else ""
    ,V.p "routine-muted" $ "Responsible Sapi: " <> D.s "name" (M.agent m $ D.s "currentOwner" run)
    ,V.when (D.s "message" run /= "") $ V.p "routine-notice" $ D.s "message" run
    ,V.when (D.s "result" run /= "") $ V.div "routine-result" $ T.paragraphs m $ D.s "result" run
    ,V.when (D.s "error" run /= "") $ V.p "routine-problem" $ D.s "error" run
    ,V.when (active run && D.s "message" run == "") $ V.p "routine-muted" $ if D.s "status" run == "queued" then "Waiting to start." else "Work is in progress."
    ,V.when (managed routine) $ V.div "routine-run-inputs" [V.p "routine-muted" $ if D.n "revision" (D.get "snapshot" run) > 0.0 then "Definition revision " <> show (D.int $ D.at ["snapshot","revision"] run) else "Definition revision not recorded",V.p "" $ D.str $ D.at ["snapshot","inputs"] run]
    ,V.when (D.valid $ D.get "fallbackSummary" run) $ V.div "routine-notice"
      [V.p "" $ if D.s "status" (D.get "fallbackSummary" run) == "completed" then "Completed through fallback" else "Fallback · " <> D.s "status" (D.get "fallbackSummary" run)
      ,V.when (D.b "faultyImplementation" $ D.get "fallbackSummary" run) $ V.p "routine-muted" "Automation needs repair; successful fallback does not repair its implementation."]
    ,externalPanel routine run
    ,stagePanel run
    ,V.when (managed routine) $ evidencePanel routine run
    ,V.when (managed routine && not (active run)) $ V.button "routine-link" "Reject result…" $ OpenDialog M.RoutineReject $ D.put "revision" (D.at ["snapshot","revision"] run) $ D.put "run" (D.get "id" run) (record routine)
    ,V.div "routine-actions" $ reply run <> if managed routine && active run then [V.button "button" "Stop this run…" $ OpenDialog M.RoutineStop $ D.put "run" (D.get "id" run) (record routine)] else []]
  externalPanel routine run = V.when (D.valid external) $ V.div "routine-notice" $
    [V.textNode "h3" [] "Content and delivery"
    ,V.p "" $ "Content · " <> if D.s "status" (D.get "contentAcceptance" external) == "accepted" then "Accepted" else if active run then "Pending" else "Unresolved"
    ,V.p "" $ "Delivery · " <> case deliveryStatus of
      "confirmed" -> "Confirmed"
      "unknown" -> "Outcome unknown"
      "started" -> "In progress"
      "prepared" -> "Not started"
      "not_sent" -> "Not sent"
      "not_required" -> "Not required — no new records"
      _ -> "Not started"
    ,V.when (D.b "possibleEffectUnknown" external) $ V.p "routine-problem" "Possible external effect · Unresolved"
    ,V.when (deliveryStatus == "unknown" || deliveryStatus == "started") $ V.p "routine-muted" "Stopping does not undo a delivery already started."
    ,V.when (D.s "error" delivery /= "") $ V.p "routine-problem" $ D.s "error" delivery
    ,V.when (D.s "lookupError" delivery /= "") $ V.p "routine-problem" $ D.s "lookupError" delivery] <>
    case state.reconciliation of
      Just request | D.s "run" request.record == D.s "id" run ->
        if request.busy then [V.p "routine-muted" "Checking delivery receipt…"]
        else [V.p "routine-problem" request.error,V.button "routine-link" "Retry receipt lookup" RoutineRetryReconciliation,V.button "routine-link" "Start new receipt lookup" $ RoutineReconcile request.record]
      _ -> if deliveryStatus == "unknown" then [V.button "routine-link" "Check delivery receipt" $ RoutineReconcile $ D.put "run" (D.get "id" run) (record routine)] else []
    where
    external = D.get "externalSummary" run
    delivery = D.get "delivery" external
    deliveryStatus = D.s "status" delivery
  stagePanel run = V.when (not (A.null stages)) $ V.div "routine-notice" $
    [V.textNode "h3" [] "Execution steps"] <> map (\stage -> V.p "" $ (if D.s "id" stage == "calculate" then "Calculation" else "Explanation") <> " · " <> stageStatus (D.s "status" stage)) stages
    where stages = D.rows "executionStages" run
  stageStatus value = case value of
    "completed" -> "Completed"
    "running" -> "In progress"
    "failed" -> "Failed"
    "interrupted" -> "Interrupted"
    "uncertain" -> "Outcome unknown"
    _ -> "Not started"
  reply run = case A.find (\w -> D.s "id" w == D.s "id" run && D.s "state" w == "WaitingForAdmin") $ D.rows "workloads" m.snapshot of
    Just work -> [V.button "button" ("Reply to " <> D.s "name" (M.agent m $ D.s "owner" work)) $ Clarify work]
    Nothing -> []
  evidencePanel routine run = V.div "routine-evidence" $
    [V.button "routine-link" "Evidence and usage" $ RoutineEvidence (D.s "id" $ record routine) (D.s "id" run)] <>
    case state.evidence of
      Just evidence | evidence.run == D.s "id" run && evidence.routine == D.s "id" (record routine) ->
        if evidence.busy then [V.p "routine-muted" "Loading evidence…"]
        else if evidence.error /= "" then [V.p "routine-problem" evidence.error,V.button "routine-link" "Retry evidence" $ RoutineEvidence evidence.routine evidence.run]
        else [evidenceDetail evidence.record,V.button "routine-link" "Refresh evidence" $ RoutineEvidence evidence.routine evidence.run]
      _ -> []
  evidenceDetail run = V.div "routine-evidence-detail" $
    [V.textNode "h3" [] "Evidence and usage"
    ,V.p "routine-muted" $ D.s "reason" $ D.get "assessment" run
    ,V.p "routine-muted" $ "Original assessment: " <> D.s "status" (D.get "originalAssessment" run)
    ,V.p "routine-muted" "Model statements and provider receipts do not independently verify external effects. Summaries may omit or shorten content; recognized credentials are removed."
    ,V.textNode "h3" [] "Pinned completion criteria"] <>
    map (\criterion -> V.p "" $ D.s "criterion" criterion <> " · " <> D.s "status" criterion <> if D.s "reason" criterion == "" then "" else " · " <> D.s "reason" criterion) (D.rows "criteriaAssessment" run) <>
    [V.textNode "h3" [] "Model statements"] <>
    map (V.p "" <<< D.str) (D.arr $ D.at ["observations","statements"] run) <>
    [V.textNode "h3" [] "Artifacts reported by the model"] <>
    map (V.p "" <<< D.str) (D.arr $ D.at ["outcome","artifacts"] run) <>
    map (\f -> V.p "routine-notice" $ "Admin feedback: " <> D.s "reason" f) (D.rows "feedback" run) <>
    (if A.null (D.rows "invocations" run) then [V.p "routine-muted" "Provider evidence and usage were not recorded."] else map invocation $ D.rows "invocations" run)
  invocation call = V.div "routine-run-inputs"
    [V.textNode "h3" [] $ D.s "node" call <> " · " <> D.s "provider" evidence <> " · " <> D.s "model" evidence
    ,V.when (D.valid $ D.get "assessment" call) $ V.p "routine-muted" $ "Invocation assessment: " <> D.s "status" (D.get "assessment" call)
    ,V.p "routine-muted" $ "Duration (ms): " <> known "durationMs" evidence <> " · Process exit: " <> known "processExit" evidence
    ,V.p "routine-muted" $ if D.b "conflict" usage then "Usage is unknown: provider reports conflict." else "Input tokens: " <> known "inputTokens" usage <> " · Cached: " <> known "cachedInputTokens" usage <> " · Output: " <> known "outputTokens" usage
    ,V.p "routine-muted" $ "First reported tokens: " <> known "inputTokens" usage <> " / " <> known "outputTokens" usage <> " · Repeated reports: " <> known "duplicates" usage <> " · Price: Unknown"
    ,V.p "routine-muted" $ "Capture: " <> (if D.b "tailUnknown" coverage then "unfinished; unrecorded tail unknown" else "finalized") <> " · Final message: " <> show (D.b "finalSeen" coverage) <> " · Stream end: " <> show (D.b "streamEndSeen" coverage) <> " · Turn end: " <> show (D.b "turnEndSeen" coverage)
    ,V.p "routine-muted" $ "Pending tools: " <> known "pendingTools" coverage <> " · Dropped events: " <> known "droppedEvents" coverage <> " · Dropped bytes: " <> known "droppedBytes" coverage <> " · Unknown events: " <> known "unknownEvents" coverage <> " · Malformed events: " <> known "malformedEvents" coverage
    ,V.p "routine-muted" "Process exit and elapsed time are application observations. Provider version and omitted event content are unknown."
    ,V.p "routine-muted" $ "Omitted fields: " <> known "omittedFields" coverage <> " · Redacted fields: " <> known "redactedFields" coverage <> " · Shortened fields: " <> known "truncatedFields" coverage
    ,V.node "details" [] [V.node "summary" [] [V.text "Provider receipts"],V.node "ul" [] $ map receipt $ D.rows "events" evidence]]
    where
    evidence = D.get "evidence" call
    usage = D.get "usage" evidence
    coverage = D.get "completeness" evidence
  receipt event = V.node "li" [] $
    [V.p "routine-muted" $ "Provider receipt " <> known "sequence" event <> ": " <> D.s "kind" event <> " · item " <> known "item" event <> " · status " <> (if D.s "status" event == "" then "Unknown" else D.s "status" event) <> " · exit " <> known "exitCode" event <> " · received " <> D.s "receivedAt" event] <>
    map (\field -> V.div "" [V.p "routine-muted" $ D.s "name" field <> (if D.b "redacted" field then " · credential content removed" else "") <> (if D.b "truncated" field then " · shortened" else "") <> (if D.b "omitted" field then " · some content omitted" else ""),V.textNode "pre" [] $ if D.valid $ D.get "value" field then D.encode $ D.get "value" field else "Not retained"]) (D.rows "fields" $ D.get "summary" event)
  known key value = if D.valid $ D.get key value then D.encode $ D.get key value else "Unknown"
  row routine = V.node "article" [V.cls "routine-row routine-list-row"]
    [V.div "routine-heading" [link (title routine) $ M.RoutineLatest $ D.s "id" $ record routine,badge $ routineStatus routine <> case latest routine of
      Just run | managed routine && not (active run) -> " · " <> D.s "status" (D.get "assessment" run)
      _ -> ""]
    ,V.p "routine-muted routine-schedule" $ whenText routine
    ,V.div "routine-actions" $ if D.b "deleted" $ record routine then [] else [menu routine]]
  content = case state.page of
    M.RoutineList deleted ->
      [V.div "routine-heading" [V.textNode "h1" [] "Routines",V.button "button" "+ New routine" $ RoutineNavigate $ M.RoutineSettings ""]
      ,V.node "nav" [V.cls "routine-tabs",V.label "Routine filter"] [link "Active" $ M.RoutineList false,link "Deleted" $ M.RoutineList true]] <>
      (let rows = A.filter (\r -> D.b "deleted" (record r) == deleted) routines in if A.null rows then [V.div "routine-empty" [V.textNode "h2" [] $ if deleted then "No deleted routines" else "No routines yet",V.p "routine-muted" $ if deleted then "Deleted routines keep their run history." else "Save work you want to repeat."]] else map row rows) <> errors
    M.RoutineSchedule _ -> case state.editor of
      Just editor -> scheduleEditor editor
      Nothing -> [link "← All routines" $ M.RoutineList false]
    M.RoutineSettings _ -> case state.editor of
      Just editor -> if D.s "id" editor.record /= "" && not (D.valid $ D.get "criteria" editor.record) then legacySettings editor.record else editorView editor
      Nothing -> [link "← All routines" $ M.RoutineList false]
    M.RoutineLatest id -> case find id of
      Nothing -> [link "← All routines" $ M.RoutineList false]
      Just routine -> heading routine false false <> case latest routine of
        Nothing -> [V.div "routine-empty" [V.textNode "h2" [] "No results yet",V.p "routine-muted" "Run it whenever you need it."]]
        Just run -> result routine run
    M.RoutineHistory id -> case find id of
      Nothing -> [link "← All routines" $ M.RoutineList false]
      Just routine -> heading routine true false <> (if A.null (runs routine) then [V.p "routine-muted" "No runs yet."] else A.mapWithIndex (\index run -> V.div "routine-history-row" [link ("Run " <> show (A.length (runs routine) - index) <> " · " <> formatDate (timestamp run)) $ M.RoutineOccurrence id (D.s "id" run),badge $ status (D.s "status" run) <> if managed routine then " · " <> D.s "status" (D.get "assessment" run) else ""]) $ A.reverse $ runs routine)
    M.RoutineOccurrence id runId -> case find id of
      Nothing -> [link "← All routines" $ M.RoutineList false]
      Just routine -> heading routine true true <> case A.find ((runId == _) <<< D.s "id") $ runs routine of
        Nothing -> [V.p "routine-muted" "Loading run…"]
        Just run -> result routine run
  editorView editor =
    [V.textNode "h1" [] $ if id == "" then "New routine" else "Routine settings"
    ,V.node "form" [V.cls "routine-editor",V.submit RoutineSave]
      [field "input" "Name (up to 60 characters)" "title",field "textarea" "What to do" "goal",field "textarea" "Completion criteria (one per line)" "criteria"
      ,V.p "routine-muted" $ "When: " <> whenText (Managed editor.record)
      ,V.when (id /= "") $ link "Schedule settings" $ M.RoutineSchedule id
      ,V.p "routine-muted" "Saving does not start work. Changes apply to future runs; accepted runs keep their original instructions."
      ,V.when (editor.error /= "") $ V.div "" [V.textNode "p" [V.cls "routine-problem",V.attr "role" "alert"] editor.error,V.button "routine-link" "Reload current settings" RoutineReloadEditor]
      ,V.div "routine-actions" [V.textNode "button" [V.attr "type" "submit",V.cls "button primary",V.disabled editor.busy] (if id == "" then "Create routine" else "Save changes"),link "Cancel" $ if id == "" then M.RoutineList false else M.RoutineLatest id]]]
    where
    id = D.s "id" editor.record
    field tag label key = V.node "label" [] [V.text label,V.node tag [V.label label,V.value $ D.s key editor.fields,V.input $ RoutineField key,V.bool "required" true,V.disabled editor.busy] []]

  legacySettings r =
    [V.textNode "h1" [] "Routine settings",V.p "" $ D.s "goal" r
    ,V.p "routine-muted" "This existing routine prepares its completion criteria when it runs. Edit its instructions through chat."
    ,V.button "button" "Edit instructions through chat" $ PrepareDraft ("Edit @" <> D.s "id" r <> " ( " <> D.s "title" r <> " )\nEdit Frequency (prev every " <> show (D.int $ D.get "minutes" r) <> " minutes): new every XXX minutes\nEdit Execution Prompt as: " <> D.s "goal" r) "XXX"
    ,link "Schedule settings" $ M.RoutineSchedule $ D.s "id" r
    ,link "← Latest result" $ M.RoutineLatest $ D.s "id" r]
  scheduleEditor editor =
    [link "← Settings" $ M.RoutineSettings id,V.textNode "h1" [] "Routine schedule"
    ,V.p "routine-muted" $ whenText (Managed editor.record)
    ,V.node "form" [V.cls "routine-editor",V.submit RoutineSave]
      [V.node "label" [] [V.text "Interval, minutes",V.node "input" [V.label "Interval, minutes",V.attr "type" "number",V.attr "min" "1",V.attr "max" "525600",V.attr "step" "1",V.bool "required" true,V.value $ D.s "minutes" editor.fields,V.input $ RoutineField "minutes",V.disabled editor.busy] []]
      ,V.p "routine-muted" "This changes future scheduled runs. Current runs keep their accepted instructions."
      ,V.when (editor.error /= "") $ V.div "" [V.textNode "p" [V.cls "routine-problem",V.attr "role" "alert"] editor.error,V.button "routine-link" "Reload current schedule" RoutineReloadEditor]
      ,V.div "routine-actions" [V.textNode "button" [V.attr "type" "submit",V.cls "button primary",V.disabled editor.busy] (if D.s "trigger" editor.record == "interval" then "Save interval" else "Enable interval"),link "Cancel" $ M.RoutineSettings id]]]
    where id = D.s "id" editor.record
