module Sapiens.Routine (viewList, view) where

import Prelude
import Data.Either (Either(..))
import Data.Array as A
import Data.Int as I
import Data.Maybe (Maybe(..), fromMaybe, isJust, maybe)
import Data.Set as Set
import Foreign.Object as O
import Libs4.Browser as B
import Sapiens.Action (Action(..))
import Sapiens.Data as D
import Sapiens.Form as F
import Sapiens.Domain as X
import Sapiens.Html as V
import Sapiens.Model as M
import Sapiens.Navigation as N
import Sapiens.RoutineData as RD
import Sapiens.RoutineRequest as RR
import Sapiens.Text as T


viewList :: M.Model -> V.Html
viewList m = V.node "section" [V.cls "routine-browser",V.label "Routines"]
  [V.div "routine-heading" [heading "Routines",V.when (not (M.selected m).archived) $ V.button "routine-link" "+ New" $ requestAction m $ RR.CreateRoutine $ M.selectedOwner m]
  ,V.div "activity-view-menu routine-filter-menu"
      [V.controlWith [V.cls "routine-filter-button",V.label "Filter routines",V.attr "aria-expanded" $ show $ m.menu == Just "routine-filter",V.attr "aria-controls" "routine-filter-options"] [V.text $ filterLabel selected,V.chevron false] (Menu "routine-filter")
      ,V.node "div" [V.id "routine-filter-options",V.cls "activity-menu routine-filter-options",V.hidden $ m.menu /= Just "routine-filter"] $ map (\f -> V.choice (selected == f) (filterLabel f) (SetRoutineFilter f)) [N.AllRoutines,N.ActiveRoutines,N.PausedRoutines]]
  ,V.node "div" [V.id "routine-rows",V.cls "routine-rows"] $
      if A.null rows then [V.p "routine-empty" emptyMessage] else map row rows]
  where
  selected = M.routineFilter m
  emptyMessage = case selected of
    N.AllRoutines -> "No routines yet."
    N.ActiveRoutines -> "No active routines."
    N.PausedRoutines -> "No paused routines."
  rows = A.filter (\r -> r.owner == m.selected && case selected of
    N.AllRoutines -> true
    N.ActiveRoutines -> not r.paused
    N.PausedRoutines -> r.paused) m.snapshot.routines
  row r =
    let ref = {owner:M.selectedOwner m,id:N.RoutineId r.id}
        title = M.routineDisplayTitle r
        status = routineStatus r
    in V.controlWith [V.cls "routine-row",V.attr "data-routine-anchor" r.id,V.label $ "Open " <> title]
      [V.node "span" [V.cls "routine-line"] [V.span [V.cls "routine-row-title",V.attr "title" title] title,badge status]
      ,V.span [V.cls "routine-meta",V.attr "title" $ date r.nextDue] $ if r.paused then "Every " <> show (I.floor r.minutes) <> " min" else "Next " <> shortDate r.nextDue]
      (OpenRoutine $ N.Overview ref)

filterLabel :: N.RoutineFilter -> String
filterLabel = case _ of
  N.AllRoutines -> "All routines"
  N.ActiveRoutines -> "Active"
  N.PausedRoutines -> "Paused"

view :: M.Model -> N.RoutineRoute -> V.Html
view m route = V.node "section" [V.cls "routine-browser",V.label "Routine details"] $
  [V.div "routine-toolbar"
    [V.button "routine-link routine-back" "← Back" RoutineBack
    ,V.control [V.cls "routine-refresh",V.label "Refresh routine view",V.attr "title" "Refresh"] "↻" RefreshRoutine]
  ,V.div "routine-heading" $ [heading title] <> case route of
      N.Overview _ | available -> case M.valueOf $ document m (M.overviewKey ref) of
        Just (X.RoutineHistoryDocument page) -> [controls m page.routine (isJust page.current)]
        _ -> []
      _ -> []] <> controlStatus m ref <>
  if not available then [V.p "routine-empty" "This routine is no longer available."] else case route of
    N.Overview owner -> overview m owner
    N.History owner position -> history m owner position
    N.RunDetails run _ -> runDetail m run
  where
  title = case route of
    N.History _ _ -> "Run history"
    _ -> M.routineTitle m ref
  ref = case route of
    N.Overview owner -> owner
    N.History owner _ -> owner
    N.RunDetails run _ -> run.routine
  available = ref.owner == M.selectedOwner m && A.any (\r -> r.owner == m.selected && r.id == N.routineId ref.id) m.snapshot.routines

heading :: String -> V.Html
heading title = V.textNode "h1" [V.id "routine-heading",V.attr "tabindex" "-1",V.attr "title" title] title

document :: M.Model -> String -> M.RemoteData String X.Document
document m key = fromMaybe M.NotAsked $ O.lookup key m.documents

loading :: String -> V.Html
loading caption = V.node "p" [V.cls "routine-empty",V.attr "role" "status"] [V.text caption]

failed :: String -> String -> String -> V.Html
failed key caption message = V.node "div" [V.cls "routine-note",V.attr "role" "alert"]
  [V.p "" caption,V.p "routine-meta" message,V.button "routine-link" "Try again" $ RetryDocument key]

overview :: M.Model -> N.RoutineRef -> Array V.Html
overview m ref = case document m key of
  M.Failed _ message -> [failed key "Could not load this routine." message]
  resource -> case M.valueOf resource of
    Just (X.RoutineHistoryDocument page) | not (isJust page.routine.deletedAt) ->
      [schedule m page.routine
      ,maybe V.empty (currentRun m ref) page.current
      ,V.node "section" [V.cls "routine-result",V.label "Last finished run"] $
          case page.latest of
            Nothing -> [V.p "routine-meta" "Last run",V.textNode "h2" [] $ if isJust page.current then "First run in progress" else "No runs yet"
              ,V.p "routine-meta" $ if isJust page.current then "The result will appear when the run finishes." else "There are no saved runs for this routine."]
            Just latest -> [V.div "routine-result-head" [V.textNode "p" [V.cls "routine-meta",V.attr "title" $ date latest.created] $ "Last run · " <> shortDate latest.created,badge $ runStatus latest]] <> latestResult m ref latest
      ,conditionView m ref
      ,V.controlWith [V.cls "routine-navrow"] [V.text "Run history",V.chevron true] $ OpenRoutineHistory ref
      ,assignment m ("assignment:" <> M.overviewKey ref) "Assignment" page.routine]
    Just (X.RoutineHistoryDocument _) -> [V.p "routine-empty" "This routine is no longer available."]
    _ -> [loading "Loading routine…"]
  where key = M.overviewKey ref

schedule :: M.Model -> RD.RoutineDefinition -> V.Html
schedule m routine = V.div "routine-schedule"
  [V.span [] $ "Every " <> show routine.minutes <> " min"
  ,V.span [V.attr "title" $ date routine.nextDue] $ if routine.paused then "Schedule paused" else "Next " <> shortDate routine.nextDue
  ,maybe V.empty (\r -> V.when (r.deferredCount > 0.0) $ V.p "routine-meta" $ show (I.floor r.deferredCount) <> " scheduled runs waiting since " <> date r.oldestDeferredDue)
      $ A.find (\r -> r.id == N.routineId routine.ref.id && r.owner == N.ownerId routine.ref.owner) m.snapshot.routines]

controls :: M.Model -> RD.RoutineDefinition -> Boolean -> V.Html
controls m routine running = V.when (not (M.selected m).archived) $ V.div "routine-actions"
  [V.when (not running) $ V.button "routine-button" "Ask Sapi to run" $ requestAction m $ RR.RequestRun routine.ref
  ,V.node "details" [V.cls "routine-menu"]
    [V.textNode "summary" [V.cls "routine-button routine-icon-button",V.label "Routine actions",V.attr "title" "Routine actions"] "···"
    ,V.div "routine-menu-box"
      [V.control [V.cls "routine-button",V.disabled $ M.routineControlBusy m routine.ref] (if M.routineControlBusy m routine.ref then "Updating schedule…" else if routine.paused then "Resume schedule" else "Pause schedule") $ ControlRoutine $ (if routine.paused then RD.ResumeSchedule else RD.PauseSchedule) routine.ref
      ,V.control [V.cls "routine-button danger",V.disabled $ M.routineControlBusy m routine.ref] "Delete routine" $ OpenDialog M.RoutineControl $ F.RoutineControlRecord {control:RD.DeleteRoutine routine.ref,title:M.routineTitle m routine.ref,running,paused:routine.paused}
      ,V.button "routine-button" "Request a change" $ requestAction m $ RR.ChangeRoutine routine.ref]]]

currentRun :: M.Model -> N.RoutineRef -> RD.RunSummary -> V.Html
currentRun m ref run = V.div "routine-current"
  [V.p "routine-meta" "Current run"
  ,V.controlWith [V.cls "routine-link",V.label "View current run"] [badge $ runStatus run] $ OpenRoutine $ N.RunDetails {routine:ref,id:run.id} N.BackToOverview
  ,stopButton m ref run]

stopButton :: M.Model -> N.RoutineRef -> RD.RunSummary -> V.Html
stopButton m ref run = V.when (not run.finished && not (isJust run.stoppedAt)) $ V.control [V.cls "routine-button",V.disabled $ M.routineControlBusy m ref] (if M.routineControlBusy m ref then "Sending…" else "Stop run") $
  OpenDialog M.RoutineControl $ F.RoutineControlRecord {control:RD.StopRun {routine:ref,id:run.id},title:M.routineTitle m ref,running:true,paused:maybe false _.paused $ A.find (\r -> r.owner == N.ownerId ref.owner && r.id == N.routineId ref.id) m.snapshot.routines}

latestResult :: M.Model -> N.RoutineRef -> RD.RunSummary -> Array V.Html
latestResult m ref latest =
  (case document m key of
    M.Failed _ message -> [failed key "Could not load the saved result." message]
    resource -> case M.valueOf resource of
      Just (X.RoutineRunDocument detail) | sameResult detail.summary latest -> resultBody m key detail
      _ -> [loading "Loading saved result…"])
  <> [V.button "routine-link" "Run details ›" $ OpenRoutine $ N.RunDetails target N.BackToOverview]
  where
  target = {routine:ref,id:latest.id}
  key = M.runKey target

-- The same Run may acquire a new result or review while its detail is loading.
sameResult :: RD.RunSummary -> RD.RunSummary -> Boolean
sameResult saved current = saved.id == current.id && saved.resultTarget == current.resultTarget
  && saved.taskRevision == current.taskRevision && saved.attempt == current.attempt
  && saved.outcome == current.outcome && saved.rejection == current.rejection
  && saved.stale == current.stale && saved.stoppedAt == current.stoppedAt
  && saved.executionStatus == current.executionStatus && saved.callState == current.callState
  && saved.finished == current.finished

history :: M.Model -> N.RoutineRef -> N.HistoryPosition -> Array V.Html
history m ref position = [V.textNode "p" [V.cls "routine-meta routine-history-title",V.attr "title" $ M.routineTitle m ref] $ M.routineTitle m ref] <> case document m key of
  M.Failed _ message -> [failed key "Could not load run history." message]
  resource -> case M.valueOf resource of
    Just (X.RoutineHistoryDocument page) | not (isJust page.routine.deletedAt) ->
      let offset = if position.half == N.FirstTen then 0 else 10
          rows = A.take 10 $ A.drop offset page.runs
          index = fromMaybe 0 $ A.elemIndex position $ M.historyPositions m ref
          next = if position.half == N.FirstTen && A.length page.runs > 10 then true else isJust page.next
      in if A.null rows then [V.p "routine-empty" "No runs yet."] else
        [V.div "routine-pagination"
          [V.span [V.cls "routine-meta",V.attr "aria-live" "polite"] $ show (index * 10 + 1) <> "–" <> show (index * 10 + A.length rows)
          ,V.div "routine-arrows"
            [V.controlWith [V.cls "routine-page-arrow routine-previous",V.label "Previous page",V.disabled $ index == 0] [V.chevron true] $ RoutineHistoryPage false
            ,V.controlWith [V.cls "routine-page-arrow",V.label "Next page",V.disabled $ not next] [V.chevron true] $ RoutineHistoryPage true]]
        ,V.node "div" [V.id "routine-rows",V.cls "routine-rows"] $ map row rows]
    Just (X.RoutineHistoryDocument _) -> [V.p "routine-empty" "This routine is no longer available."]
    _ -> [loading "Loading history…"]
  where
  key = M.historyKey ref position.before
  row run = V.controlWith [V.cls "routine-row routine-history-row",V.attr "data-routine-anchor" $ N.runId run.id]
    [V.node "span" [V.cls "routine-line"] [V.span [V.attr "title" $ date run.created] $ shortDate run.created <> " · " <> RD.triggerLabel run.trigger,badge $ runStatus run]
    ,maybe V.empty (V.span [V.cls "routine-meta routine-error"]) run.error]
    $ OpenRoutine $ N.RunDetails {routine:ref,id:run.id} (N.BackToHistory position)

runDetail :: M.Model -> N.RunRef -> Array V.Html
runDetail m ref = case document m key of
  M.Failed _ message -> [failed key "Could not load this run." message]
  resource -> case M.valueOf resource of
    Just (X.RoutineRunDocument detail) ->
      [V.p "routine-meta routine-run-meta" $ "Created " <> date detail.summary.created <> " · " <> RD.triggerLabel detail.summary.trigger
      ,V.node "section" [V.cls "routine-result",V.label "Run result"] $
        case selectedResult m ref detail of
          Nothing -> [V.div "routine-result-head" [badge $ runStatus detail.summary,stopButton m ref.routine detail.summary]] <> resultBody m key detail
          Just result -> savedResultBody m result
      ,V.button "routine-link" "Reply to Sapi" $ requestAction m $ RR.ReplyToRun ref (case selectedResult m ref detail of
          Just result -> Just result.reference
          Nothing -> detail.summary.resultTarget)
      ,V.div "routine-repairs" $ map (repairView m ref) detail.repairs
      ,savedResults m ref detail
      ,disclosure m ("assignment:" <> key) "Run details" $
          [V.p "routine-meta" $ (if assignmentChanged detail.snapshot then "The assignment has since changed. " else "") <> "These are the instructions saved for this run."
          ,case selectedResult m ref detail of
              Nothing -> maybe V.empty (outcomeView m ("outcome:" <> key) detail.snapshot.specification) detail.summary.outcome
              Just result -> maybe V.empty (outcomeView m ("outcome:" <> key <> ":" <> resultKey result.reference) detail.snapshot.specification) result.outcome]
          <> (if isJust detail.summary.stoppedAt && not (isJust $ selectedResult m ref detail)
              then maybe [] (\error -> [V.textNode "h3" [] "Execution details",V.div "routine-meta" $ T.paragraphs m error]) detail.summary.error
              else [])
          <> assignmentContent m detail.snapshot]
    _ -> [loading "Loading run…"]
  where
  key = M.runKey ref
  assignmentChanged snapshot = A.any (\r -> r.owner == N.ownerId ref.routine.owner && r.id == N.routineId ref.routine.id && r.prompt /= snapshot.prompt) m.snapshot.routines

type Badge = {label :: String, tone :: String}

badge :: Badge -> V.Html
badge status = V.span [V.cls $ "routine-badge " <> status.tone] status.label

runStatus :: RD.RunSummary -> Badge
runStatus run
  | isJust run.stoppedAt && not run.finished = {label:"Stopping",tone:"active"}
  | isJust run.stoppedAt = {label:"Stopped",tone:"neutral"}
  | not run.finished = {label:inProgress run,tone:"active"}
  | physicalFailure run || isJust run.error = {label:"Needs attention",tone:"attention"}
  | run.stale || isJust run.rejection = {label:"Needs attention",tone:"attention"}
  | otherwise = case _.status <$> run.outcome of
      Just RD.Completed -> {label:if isJust run.result then "Reported complete" else "Completed",tone:"complete"}
      Just RD.Unresolved -> {label:"Needs attention",tone:"attention"}
      Nothing -> {label:"Needs attention",tone:"attention"}

physicalFailure :: RD.RunSummary -> Boolean
physicalFailure run = A.elem run.executionStatus (map Just [RD.ExecutionFailed,RD.ExecutionInterrupted,RD.ExecutionCancelled]) || A.elem run.callState (map Just [RD.CallFailed,RD.CallInterrupted,RD.CallCancelled])

inProgress :: RD.RunSummary -> String
inProgress run
  | run.pending || run.executionStatus == Just RD.ExecutionQueued || run.callState == Just RD.CallQueued = "Queued"
  | run.callState == Just RD.CallWaiting = "Needs input"
  | run.callState == Just RD.CallDelegated = "Delegated"
  | run.executionStatus == Just RD.ExecutionOutputPending || run.groupSettled == Just false && A.elem run.executionStatus (map Just [RD.ExecutionDone,RD.ExecutionWarning]) = "Finishing"
  | otherwise = "Running"

date :: String -> String
date value = B.formatDate "" (D.json {year:"numeric",month:"short",day:"numeric",hour:"2-digit",minute:"2-digit",timeZoneName:"short"}) value

shortDate :: String -> String
shortDate value = B.formatDate "" (D.json {month:"short",day:"numeric",hour:"2-digit",minute:"2-digit",hour12:false}) value

resultBody :: M.Model -> String -> RD.RoutineRunDetail -> Array V.Html
resultBody m _ detail =
  [V.when (isJust run.stoppedAt) $ V.p "routine-meta" $
      (if run.finished then "Stopped by request." else "Stop requested.") <> " Actions already completed are preserved."
  ,V.when (not run.finished && not (isJust run.stoppedAt)) $ V.p "routine-meta" "This run has not finished."
  ,V.when (physicalFailure run && not (isJust run.stoppedAt)) $ V.p "routine-error" $
      maybe "Execution did not complete." (\state -> "Execution: " <> RD.executionLabel state) run.executionStatus
  ,V.when (run.stale && (isJust run.result || isJust run.outcome)) $ V.p "routine-note" "This result belongs to an earlier task revision."
  ,V.when (not $ isJust run.stoppedAt) $ maybe V.empty (\error -> V.div "routine-error" $ T.paragraphs m error) run.error
  ,maybe V.empty (\text -> V.div "routine-result-text" $ T.paragraphs m text) run.result
  ,case run.outcome of
      Nothing -> V.when (run.finished && not (isJust run.stoppedAt)) $ V.p "routine-note" "Outcome not recorded. Review the saved result before relying on it."
      Just _ -> V.empty
  ,maybe V.empty (rejectionView m) run.rejection
  ,V.when (run.notification == RD.Quiet || maybe false ((_ == RD.Quiet) <<< _.notification) selected) $ V.p "routine-meta" "Saved in history · No chat notification."]
  where
  run = detail.summary
  selected = run.resultTarget >>= \ref -> A.find ((ref == _) <<< _.reference) detail.results

selectedResult :: M.Model -> N.RunRef -> RD.RoutineRunDetail -> Maybe RD.SavedRunResult
selectedResult m ref detail = O.lookup (M.runKey ref) m.routineResultSelections >>= \selected ->
  A.find (\result -> result.reference == selected && Just selected /= detail.summary.resultTarget) detail.results

savedResults :: M.Model -> N.RunRef -> RD.RoutineRunDetail -> V.Html
savedResults m ref detail = V.when (not $ A.null previous) $ disclosure m ("saved:" <> M.runKey ref) "Previous saved results"
  [V.node "label" [V.cls "routine-result-picker"]
    [V.span [V.cls "routine-meta"] "Selected result"
    ,V.node "select" [V.cls "routine-filter",V.label "Selected result",V.value selected,V.change choose] $
      [V.textNode "option" [V.value "",V.bool "selected" $ selected == ""] "Latest result"] <>
      A.mapWithIndex (\index result -> V.textNode "option" [V.value $ resultKey result.reference,V.bool "selected" $ selected == resultKey result.reference] $ "Earlier result " <> show (A.length previous - index)) previous]]
  where
  previous = A.reverse $ A.filter (\result -> Just result.reference /= detail.summary.resultTarget) detail.results
  selected = maybe "" (resultKey <<< _.reference) $ selectedResult m ref detail
  choose value = SelectRoutineResult ref $ _.reference <$> A.find ((value == _) <<< resultKey <<< _.reference) previous

savedResultBody :: M.Model -> RD.SavedRunResult -> Array V.Html
savedResultBody m result =
  [V.div "routine-result-head" [badge status]
  ,V.p "routine-note" "Earlier saved result."
  ,V.when result.stale $ V.p "routine-note" "Earlier task revision"
  ,maybe V.empty (\error -> V.div "routine-error" $ T.paragraphs m error) result.error
  ,maybe V.empty (\text -> V.div "routine-result-text" $ T.paragraphs m text) result.reply
  ,V.when (not $ isJust result.outcome) $ V.p "routine-meta" "No structured outcome was recorded."
  ,maybe V.empty (rejectionView m) result.rejection
  ,V.when (result.notification == RD.Quiet) $ V.p "routine-meta" "Saved without a chat notification."]
  where
  status = if result.stale || isJust result.error || isJust result.rejection then {label:"Needs attention",tone:"attention"}
    else case _.status <$> result.outcome of
      Just RD.Completed -> {label:"Reported complete",tone:"complete"}
      Just RD.Unresolved -> {label:"Needs attention",tone:"attention"}
      Nothing -> {label:"Needs attention",tone:"attention"}

resultKey :: RD.ExactResultRef -> String
resultKey = case _ of
  RD.PersonalDecision decision -> "decision:" <> decision
  RD.GroupResult request revision -> "request:" <> N.runId request <> ":" <> show revision

rejectionView :: M.Model -> RD.ResultRejection -> V.Html
rejectionView m rejected = V.div "routine-note"
  ([V.textNode "strong" [] "Reported issue",V.p "routine-meta" $ date rejected.rejectedAt] <> T.paragraphs m rejected.reason <> [V.p "routine-meta" "The original recorded outcome is preserved."])

assignment :: M.Model -> String -> String -> RD.RoutineDefinition -> V.Html
assignment m key title routine = disclosure m key title $
  assignmentContent m routine <> [V.p "routine-meta" $ RD.policyLabel routine.policy,V.p "routine-meta" "Changes apply to future runs."]

assignmentContent :: M.Model -> RD.RoutineDefinition -> Array V.Html
assignmentContent m routine = case routine.specification of
    Nothing -> instructions <> [V.p "routine-meta" "No structured criteria were saved for this legacy routine."]
    Just spec ->
      [V.textNode "h3" [] "Objective",V.div "routine-prose" $ T.paragraphs m spec.objective
      ] <> instructions <> [textList "Inputs" spec.inputs,textList "Expected outputs" spec.expectedOutputs
      ,V.textNode "h3" [] "Completion criteria",V.node "ul" [V.cls "routine-criteria"] $ map criterion spec.completionCriteria]
  where
  instructions = [V.textNode "h3" [] "Instructions",V.div "routine-prose" $ T.paragraphs m routine.prompt]
  criterion item = V.node "li" [] $ case item of
    RD.Required _ requirement -> [V.p "" requirement,V.p "routine-meta" "Required"]
    RD.Conditional _ condition requirement -> [V.p "" requirement,V.p "routine-meta" $ "When: " <> condition]

textList :: String -> Array String -> V.Html
textList title rows = V.when (not $ A.null rows) $ V.div "routine-list-section" [V.textNode "h3" [] title,V.node "ul" [] $ map (V.textNode "li" []) rows]

outcomeView :: M.Model -> String -> Maybe RD.TaskSpec -> RD.Outcome -> V.Html
outcomeView m key spec outcome = disclosure m key "Completion criteria and evidence" $
  [V.p "routine-meta" $ "Recorded outcome: " <> RD.outcomeLabel outcome.status
  ,V.node "ul" [V.cls "routine-criteria"] $ case spec of
      Just task -> map criterion task.completionCriteria
      Nothing -> map (\result -> V.node "li" [] [resultContent m result]) outcome.criterionResults
  ,V.when (not $ A.null outcome.artifacts) $ V.div "routine-list-section" [V.textNode "h3" [] "Saved artifacts",V.node "ul" [] $ map (\path -> V.node "li" [] [T.link path ""]) outcome.artifacts]]
  where
  criterion = case _ of
    RD.Required id requirement -> criterionEntry id requirement "Required"
    RD.Conditional id condition requirement -> criterionEntry id requirement ("When: " <> condition)
  criterionEntry id requirement condition =
    let matches = case _ of
          RD.Satisfied identity _ -> identity == id
          RD.NotApplicable identity _ -> identity == id
        result = A.find matches outcome.criterionResults
    in V.node "li" [] [V.p "" requirement,V.p "routine-meta" condition,maybe (V.p "routine-meta" "Not reported") (resultContent m) result]

resultContent :: M.Model -> RD.CriterionResult -> V.Html
resultContent m = case _ of
  RD.Satisfied _ evidence -> V.div "routine-evidence" $ [V.p "routine-meta" "Satisfied"] <> T.paragraphs m evidence
  RD.NotApplicable _ evidence -> V.div "routine-evidence" $ [V.p "routine-meta" "Not applicable"] <> T.paragraphs m evidence

disclosure :: M.Model -> String -> String -> Array V.Html -> V.Html
disclosure m key title content = V.node "details" [V.cls "routine-disclosure",V.bool "open" $ Set.member key m.expanded]
  [V.node "summary" [V.on "click" \event -> Link event (ToggleExpanded key)] [V.text title],V.div "routine-disclosure-body" content]

controlStatus :: M.Model -> N.RoutineRef -> Array V.Html
controlStatus m ref = case O.lookup (M.routineKey ref) m.routineControls of
  Just (M.Loading _ _) -> [V.node "p" [V.cls "routine-meta",V.attr "role" "status"] [V.text "Sending change…"]]
  Just (M.Failed _ message) -> [V.node "div" [V.cls "routine-note",V.attr "role" "alert"] [V.p "" message,V.button "routine-link" "Refresh status" RefreshRoutine]]
  _ -> []

requestAction :: M.Model -> RR.Intent -> Action
requestAction m intent = OpenDialog M.RoutineRequest $ F.RoutineRequestRecord {intent,origin:m.activity}

routineStatus :: X.Routine -> Badge
routineStatus routine
  | isJust routine.activeRun = {label:"Running",tone:"active"}
  | otherwise = case routine.condition of
      Left _ -> {label:"Status unavailable",tone:"attention"}
      Right (RD.AttentionRequired _ _) -> {label:"Needs attention",tone:"attention"}
      Right (RD.RepairPending _) -> {label:"Repairing",tone:"active"}
      Right RD.NoIssue -> {label:if routine.paused then "Paused" else "Active",tone:"neutral"}

conditionView :: M.Model -> N.RoutineRef -> V.Html
conditionView m ref = case A.find (\r -> r.id == N.routineId ref.id && r.owner == N.ownerId ref.owner) m.snapshot.routines of
  Just routine -> case routine.condition of
    Left error -> V.p "routine-error" error
    Right (RD.AttentionRequired problem reason) -> V.div "routine-note"
      [V.p "" reason.message,V.button "routine-link" "Review affected run" $ OpenRoutine $ N.RunDetails {routine:ref,id:problem.run} N.BackToOverview]
    Right (RD.RepairPending repair) -> V.div "routine-note"
      [V.textNode "strong" [] "Repairing",V.p "" repair.description,V.p "routine-meta" "Fix applied · Awaiting verification by a new run.",V.button "routine-link" "View repair history" $ OpenRoutine $ N.RunDetails {routine:ref,id:repair.problem.run} N.BackToOverview]
    Right RD.NoIssue -> V.empty
  Nothing -> V.empty

repairView :: M.Model -> N.RunRef -> RD.RepairEntry -> V.Html
repairView _ ref entry = V.div "routine-note"
  [V.textNode "strong" [] "Fix applied",V.p "" entry.repair.description,V.p "routine-meta" $ date entry.repair.confirmedAt
  ,case entry.verification of
      RD.AwaitingVerification -> V.p "routine-meta" "Awaiting verification by a new run."
      RD.VerificationRunning run -> link run "Verification in progress"
      RD.VerificationPassed run -> link run "Verified by a successful run"
      RD.VerificationFailed run -> link run "Verification needs attention"]
  where
  link run text = V.button "routine-link" text $ OpenRoutine $ N.RunDetails {routine:ref.routine,id:run} N.BackToOverview
