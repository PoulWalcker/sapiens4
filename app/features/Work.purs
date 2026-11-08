module Sapiens.Work where

import Sapiens.Form as F
import Prelude
import Data.Array as A
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set as Set
import Data.String as S
import Foreign.Object as O
import Libs4.Browser as B
import Sapiens.Action (Action(..), Success(..))
import Sapiens.Conversation as C
import Sapiens.Data as D
import Sapiens.Domain as X
import Sapiens.Html as V
import Sapiens.Model as M
import Sapiens.Navigation as N
import Sapiens.Routine as Routine
import Sapiens.Text as T

emptyTasks :: String -> String -> V.Html
emptyTasks title detail = V.div "task-empty" [V.p "" title,V.textNode "small" [] (detail)]

disclosure :: M.Model -> String -> String -> Array V.Html -> Array V.Html -> V.Html
disclosure m key classes summary content = V.node "details" [V.cls classes,V.bool "open" $ Set.member key m.expanded]
  [V.node "summary" [V.cls "task-summary",V.on "click" \e -> Link e (ToggleExpanded key)] summary,V.div "task-content" content]

taskRow :: M.Model -> X.Task -> V.Html
taskRow m task = disclosure m ("task:" <> m.selected <> ":" <> task.id) ("task-row " <> if routine then "task-routine" else "")
  ([V.span [V.cls $ "task-mark " <> tone,V.attr "aria-hidden" "true"] (if state == X.WorkCompleted then "✓" else if tone == "attention" then "!" else "·")
   ,V.node "span" [V.cls "task-row-copy"] [V.span [V.cls "task-title",V.attr "title" $ task.title] (task.title),V.node "span" [V.cls "task-meta"] [V.span [V.cls $ "task-state " <> tone] (if state == X.WorkWaitingForAdmin then "Needs input" else X.workName state),V.span [V.cls "task-assignee"] (D.fallback route $ task.meta)]]]
   <> (if routine then stopControl <> [control "Run routine" "Run" $ PrepareDraft ("@" <> task.id <> "\nRun it now.") "",control "Edit routine" "Edit" $ PrepareDraft ("Edit @" <> task.id <> " ( " <> task.title <> " )\nEdit Frequency (prev every " <> task.minutes <> " minutes): new every XXX minutes\nEdit Execution Prompt as: " <> task.prompt) "XXX",control (if state == X.WorkPaused then "Resume routine" else "Pause routine") (if state == X.WorkPaused then "▶" else "Ⅱ") $ PrepareDraft ((if state == X.WorkPaused then "Resume Scheduled Routine" else "Pause Scheduled Routine") <> "\n@" <> task.id) "",V.node "span" [V.cls "task-reference",V.attr "title" "Scheduled tasks queued in this allowance; pauses automatically at 1000."] [V.textNode "code" [] ("@" <> task.id),V.span [] (show task.runCount <> "/1000")]] else []) <> [V.span [V.cls "task-chevron",V.attr "aria-hidden" "true"] "›"])
  [V.div "task-body-heading" [V.span [] (if routine then "Execution prompt" else "Task body"),V.span [V.cls "task-format"] (if routine then "" else "YAML"),V.control [V.cls "task-copy",V.label $ "Copy " <> if routine then "execution prompt" else "task YAML"] "Copy" (Copy (task.body) (if routine then "Execution prompt copied." else "Task YAML copied."))]
  ,V.textNode "pre" [V.cls "task-body-yaml",V.label $ if routine then "Execution prompt" else "Task body YAML"] (task.body)
  ,V.when (task.result /= "") $ V.div "task-result" $ [V.textNode "h3" [V.cls "task-result-heading"] "Result"] <> T.paragraphs m (task.result)
  ,V.when (task.error /= "") $ V.p "task-error" $ task.error]
  where
  state = task.state
  tone = if state == X.WorkCompleted then "complete" else if A.elem state [X.WorkFailed,X.WorkInterrupted,X.WorkUnresolved,X.WorkWaitingForAdmin] then "attention" else if A.elem state [X.WorkRunning,X.WorkQueued] then "active" else "neutral"
  name id = (M.agent m id).name
  route = (if task.sender == task.owner then "" else name (task.sender) <> " → ") <> name (task.owner)
  routine = task.routine
  stopControl = case A.find (\r -> r.id == task.id) m.snapshot.routines >>= _.activeRun of
    Nothing -> []
    Just run -> [control "Stop current run; keep schedule" "Stop" $ Mutate ("/api/routines/" <> B.encodeURI task.id <> "/runs/" <> B.encodeURI run <> "/stop") "POST" D.empty Refresh]
  control caption glyph action = V.textNode "button" [V.attr "type" "button",V.cls "task-copy task-routine-action",V.label caption,V.on "click" \e -> Link e action] (glyph)

groupCalls :: M.Model -> X.Entity -> Array X.Task
groupCalls m group = map row $ A.reverse $ A.filter (not <<< _.hasTask) group.requests
  where
  row r =
    let message = A.find ((r.message == _) <<< _.id) group.messages
        result = A.find ((r.id == _) <<< _.turn) group.messages
        title = if r.memo then "Update group Memo" else D.fallback "Group call" $ fromMaybe "" $ _.text <$> message
        state = case r.status of
          X.Warning -> X.WorkCompleted
          X.OutputPending -> X.WorkRunning
          X.Cancelled -> X.WorkCancelled
          other -> X.workState $ C.statusName other
    in {id:r.id,title,state,owner:r.target,sender:r.target,body:"objective: " <> D.encode (D.text title) <> "\nassignee: " <> D.encode (D.text (M.agent m r.target).name) <> "\n",result:fromMaybe "" $ _.text <$> result,error:r.error,taskType:"",routine:false,minutes:"",prompt:"",meta:"",runCount:0}

defaultActivity :: M.Model -> N.Activity
defaultActivity m = if A.any (not <<< M.past) states then N.Work N.Tasks N.Planned else if A.any (\r -> r.owner == m.selected && not (r.paused)) (m.snapshot.routines) then N.Work N.Scheduled N.Planned else if A.null states then N.Work N.Memo N.Planned else N.Work N.Tasks N.Executed
  where
  group = M.selected m
  states = if M.isGroup group then map _.state (groupCalls m group) <> map (\t -> if t.state == X.GroupDone then X.WorkCompleted else X.WorkQueued) (A.filter (not <<< _.deleted) $ group.tasks) else map _.state $ A.filter (\w -> w.origin.group == "" && A.elem m.selected w.participants) $ m.snapshot.workloads

groupTask :: M.Model -> X.Entity -> X.GroupTask -> V.Html
groupTask m group task = disclosure m ("group-task:" <> task.id) "task-row group-task"
  [V.node "span" [V.cls "task-row-copy"] [V.span [V.cls "task-title"] (task.title),V.span [V.cls "task-meta"] ((M.agent m task.assignee).name <> " · " <> if task.deleted then "Deleted" else taskState $ task.state)],V.span [V.cls "task-chevron"] "›"]
  [V.div "group-task-body" $ T.paragraphs m $ task.body
  ,V.when (not $ group.archived) $ V.div "actions" [V.button "button" (if task.deleted then "Restore / edit" else "Edit") $ OpenDialog M.GroupTask (F.TaskRecord task),V.when (not $ task.deleted) $ V.control [V.cls "button",V.disabled running] (if running then "Running / queued" else if task.state == X.GroupDone then "Run again" else "Run task") (Mutate ("/api/groups/" <> group.id <> "/tasks/" <> task.id <> "/run") "POST" (D.json {revision: D.json task.revision}) Refresh)]
  ,V.when (not $ A.null $ task.results) $ V.node "details" [] [V.textNode "summary" [] ("Results · " <> show (A.length $ task.results)),V.div "" $ map (\r -> V.div "group-task-result" $ [V.textNode "strong" [] ((M.agent m r.author).name <> if r.stale then " · Earlier revision" else "")] <> T.paragraphs m (D.fallback (D.fallback "No reply saved." $ r.error) $ r.text)) $ task.results]
  ,V.when (not $ A.null $ task.history) $ V.node "details" [] [V.textNode "summary" [] ("Change history · " <> show (A.length $ task.history)),V.div "" $ map (\h -> V.div "group-task-result" [V.textNode "strong" [] (h.title),V.p "" $ (M.agent m h.assignee).name <> " · " <> taskState (h.state) <> if h.deleted then " · Deleted" else "",V.p "" $ h.body]) $ A.reverse $ task.history]]
  where
  running = A.any (\r -> r.task == task.id && A.elem r.status [X.Queued,X.Running]) $ group.requests

taskState :: X.GroupState -> String
taskState = case _ of
  X.GroupBacklog -> "Backlog"
  X.GroupInProgress -> "In progress"
  X.GroupDone -> "Done"
  X.OtherGroupState other -> other

tasks :: M.Model -> N.WorkView -> N.Period -> V.Html
tasks m view period = V.node "section" [V.cls "task-browser"] [V.node "div" [V.id "task-list",V.cls "task-list",V.label $ N.periodLabel period <> " tasks",V.attr "aria-busy" $ show loading] content]
  where
  past = period == N.Executed
  key = "tasks:" <> m.selected
  document = fromMaybe M.NotAsked $ O.lookup key m.documents
  loading = case document of
    M.Loading _ _ -> true
    _ -> false
  rows = A.filter (\t -> M.past (t.state) == past && (view /= N.Automated || t.taskType == "automated")) $ case M.valueOf document of
    Just (X.TaskDocument items) -> items
    _ -> []
  content = if M.isGroup (M.selected m) then groupContent else case document of
    M.Failed _ _ -> [V.div "task-empty" [V.p "" "Could not load tasks.",V.button "task-copy" "Try again" $ RetryDocument key]]
    _ -> if M.valueOf document == Nothing then [V.p "task-empty" "Loading tasks…"] else if A.null rows then [emptyTasks (if past then "No executed tasks" else "No planned tasks") (if past then "Completed and stopped tasks will appear here." else "Queued, running and waiting tasks will appear here.")] else map (taskRow m) rows
  group = M.selected m
  groupRows = A.filter (\t -> (view /= N.Automated || t.taskType == "automated") && if period == N.Deleted then t.deleted else not (t.deleted) && (t.state == X.GroupDone) == past) $ group.tasks
  calls = if view == N.Automated || period == N.Deleted then [] else A.filter (\t -> M.past (t.state) == past) $ groupCalls m group
  groupContent = if A.null groupRows && A.null calls then [V.p "empty" "No tasks here yet."] else map (groupTask m group) groupRows <> map (taskRow m) calls

routines :: M.Model -> N.Period -> V.Html
routines m _ = Routine.viewList m

events :: M.Model -> V.Html
events m = V.node "section" [V.cls "event-log"] [V.div "event-columns event-head" $ map (\s -> V.span [] (s)) ["Timestamp","Actor","Entity","Event","Agency",""],V.div "event-rows" $ if A.null rows then [V.p "empty" "No events yet."] else map event rows]
  where
  rows = A.take 100 $ fromMaybe [] $ O.lookup m.selected m.snapshot.events
  name id = case id of
    "admin" -> "Admin"
    "system" -> "System"
    "corpora" -> "Corpora"
    _ -> D.fallback (if S.take 12 id == "routine_sch_" then "@" <> id else id) $ fromMaybe "" $ _.name <$> X.findEntity id m.agents
  event row = let key = "event:" <> row.id in V.node "details" [V.cls "task-row event-row",V.bool "open" $ Set.member key m.expanded]
    [V.node "summary" [V.cls "event-columns",V.on "click" \e -> Link e (ToggleExpanded key)] (map (\v -> V.span [V.attr "title" v] (v)) [B.formatDate "en-GB" (D.json {day: "2-digit", month: "short", hour: "2-digit", minute: "2-digit", second: "2-digit", fractionalSecondDigits: 3}) (row.timestamp),name $ row.actor,name $ row.entity,row.event,D.fallback "—" $ row.agency,"›"])
    ,V.when (Set.member key m.expanded) $ V.textNode "pre" [] (row.details)]
