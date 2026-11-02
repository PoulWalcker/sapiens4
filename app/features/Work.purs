module Sapiens.Work where

import Prelude
import Data.Argonaut.Core (Json)
import Data.Array as A
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set as Set
import Data.String as S
import Foreign.Object as O
import Libs4.Browser as B
import Sapiens.Action (Action(..), Success(..))
import Sapiens.Conversation as C
import Sapiens.Data as D
import Sapiens.Html as V
import Sapiens.Model as M
import Sapiens.Navigation as N
import Sapiens.Text as T
import Sapiens.Routines as Routines

emptyTasks :: String -> String -> V.Html
emptyTasks title detail = V.div "task-empty" [V.p "" title,V.textNode "small" [] (detail)]

disclosure :: M.Model -> String -> String -> Array V.Html -> Array V.Html -> V.Html
disclosure m key classes summary content = V.node "details" [V.cls classes,V.bool "open" $ Set.member key m.expanded]
  [V.node "summary" [V.cls "task-summary",V.on "click" \e -> Link e (ToggleExpanded key)] summary,V.div "task-content" content]

taskRow :: M.Model -> Json -> V.Html
taskRow m task = disclosure m ("task:" <> m.selected <> ":" <> D.s "id" task) ("task-row " <> if routine then "task-routine" else "")
  ([V.span [V.cls $ "task-mark " <> tone,V.attr "aria-hidden" "true"] (if state == "Completed" then "✓" else if tone == "attention" then "!" else "·")
   ,V.node "span" [V.cls "task-row-copy"] [V.span [V.cls "task-title",V.attr "title" $ D.s "title" task] (D.s "title" task),V.node "span" [V.cls "task-meta"] [V.span [V.cls $ "task-state " <> tone] (if state == "WaitingForAdmin" then "Needs input" else state),V.span [V.cls "task-assignee"] (D.fallback route $ D.s "meta" task)]]]
   <> (if routine then [control "Run routine" "Run" $ PrepareDraft ("@" <> D.s "id" task <> "\nRun it now.") "",control "Edit routine" "Edit" $ PrepareDraft ("Edit @" <> D.s "id" task <> " ( " <> D.s "title" task <> " )\nEdit Frequency (prev every " <> D.s "minutes" task <> " minutes): new every XXX minutes\nEdit Execution Prompt as: " <> D.s "prompt" task) "XXX",control (if state == "Paused" then "Resume routine" else "Pause routine") (if state == "Paused" then "▶" else "Ⅱ") $ PrepareDraft ((if state == "Paused" then "Resume Scheduled Routine" else "Pause Scheduled Routine") <> "\n@" <> D.s "id" task) "",V.node "span" [V.cls "task-reference",V.attr "title" "Scheduled tasks queued in this allowance; pauses automatically at 1000."] [V.textNode "code" [] ("@" <> D.s "id" task),V.span [] (show (D.int $ D.get "runCount" task) <> "/1000")]] else []) <> [V.span [V.cls "task-chevron",V.attr "aria-hidden" "true"] "›"])
  [V.div "task-body-heading" [V.span [] (if routine then "Execution prompt" else "Task body"),V.span [V.cls "task-format"] (if routine then "" else "YAML"),V.control [V.cls "task-copy",V.label $ "Copy " <> if routine then "execution prompt" else "task YAML"] "Copy" (Copy (D.s "body" task) (if routine then "Execution prompt copied." else "Task YAML copied."))]
  ,V.textNode "pre" [V.cls "task-body-yaml",V.label $ if routine then "Execution prompt" else "Task body YAML"] (D.s "body" task)
  ,V.when (D.s "result" task /= "") $ V.div "task-result" $ [V.textNode "h3" [V.cls "task-result-heading"] "Result"] <> T.paragraphs m (D.s "result" task)
  ,V.when (D.s "error" task /= "") $ V.p "task-error" $ D.s "error" task]
  where
  state = D.s "state" task
  tone = if state == "Completed" then "complete" else if A.elem state ["Failed","Interrupted","Unresolved","WaitingForAdmin"] then "attention" else if A.elem state ["Running","Queued"] then "active" else "neutral"
  name id = D.s "name" $ M.agent m id
  route = (if D.s "sender" task == D.s "owner" task then "" else name (D.s "sender" task) <> " → ") <> name (D.s "owner" task)
  routine = D.b "routine" task
  control caption glyph action = V.textNode "button" [V.attr "type" "button",V.cls "task-copy task-routine-action",V.label caption,V.on "click" \e -> Link e action] (glyph)

groupCalls :: M.Model -> Json -> Array Json
groupCalls m group = map row $ A.reverse $ A.filter (not <<< D.valid <<< D.get "task") $ D.rows "requests" group
  where
  row r =
    let message = D.find (D.s "message" r) $ D.rows "messages" group
        result = fromMaybe D.null $ A.find (( _ == D.s "id" r) <<< D.s "turn") $ D.rows "messages" group
        title = if D.b "memo" r then "Update group Memo" else D.fallback "Group call" $ D.s "text" message
        state = if D.s "status" r == "warning" then "Completed" else if D.s "status" r == "output_pending" then "Running" else if D.s "status" r == "cancelled" then "Cancelled" else C.statusName $ D.s "status" r
    in D.json {id: D.get "id" r, title, state, owner: D.get "target" r, sender: D.get "target" r, body: "objective: " <> D.encode (D.text title) <> "\nassignee: " <> D.encode (D.text $ D.s "name" $ M.agent m $ D.s "target" r) <> "\n", result: D.get "text" result, error: D.get "error" r}

defaultActivity :: M.Model -> N.Activity
defaultActivity m = if A.any (not <<< M.past) states then N.Work N.Tasks N.Planned else if A.any (\r -> D.s "owner" r == m.selected && not (D.b "paused" r)) (D.rows "routines" m.snapshot) then N.Work N.Scheduled N.Planned else if A.null states then N.Work N.Memo N.Planned else N.Work N.Tasks N.Executed
  where
  group = M.selected m
  states = if M.isGroup group then map (D.s "state") (groupCalls m group) <> map (\t -> if D.s "state" t == "done" then "Completed" else "Queued") (A.filter (not <<< D.b "deleted") $ D.rows "tasks" group) else map (D.s "state") $ A.filter (\w -> not (D.valid $ D.at ["origin","group"] w) && D.has m.selected (D.get "participants" w)) $ D.rows "workloads" m.snapshot

groupTask :: M.Model -> Json -> Json -> V.Html
groupTask m group task = disclosure m ("group-task:" <> D.s "id" task) "task-row group-task"
  [V.node "span" [V.cls "task-row-copy"] [V.span [V.cls "task-title"] (D.s "title" task),V.span [V.cls "task-meta"] (D.s "name" (M.agent m $ D.s "assignee" task) <> " · " <> if D.b "deleted" task then "Deleted" else taskState $ D.s "state" task)],V.span [V.cls "task-chevron"] "›"]
  [V.div "group-task-body" $ T.paragraphs m $ D.s "body" task
  ,V.when (not $ D.b "archived" group) $ V.div "actions" [V.button "button" (if D.b "deleted" task then "Restore / edit" else "Edit") $ OpenDialog M.GroupTask task,V.when (not $ D.b "deleted" task) $ V.control [V.cls "button",V.disabled running] (if running then "Running / queued" else if D.s "state" task == "done" then "Run again" else "Run task") (Mutate ("/api/groups/" <> D.s "id" group <> "/tasks/" <> D.s "id" task <> "/run") "POST" (D.json {revision: D.get "revision" task}) Refresh)]
  ,V.when (not $ A.null $ D.rows "results" task) $ V.node "details" [] [V.textNode "summary" [] ("Results · " <> show (A.length $ D.rows "results" task)),V.div "" $ map (\r -> V.div "group-task-result" $ [V.textNode "strong" [] (D.s "name" (M.agent m $ D.s "author" r) <> if D.b "stale" r then " · Earlier revision" else "")] <> T.paragraphs m (D.fallback (D.fallback "No reply saved." $ D.s "error" r) $ D.s "text" r)) $ D.rows "results" task]
  ,V.when (not $ A.null $ D.rows "history" task) $ V.node "details" [] [V.textNode "summary" [] ("Change history · " <> show (A.length $ D.rows "history" task)),V.div "" $ map (\h -> V.div "group-task-result" [V.textNode "strong" [] (D.s "title" h),V.p "" $ D.s "name" (M.agent m $ D.s "assignee" h) <> " · " <> taskState (D.s "state" h) <> if D.b "deleted" h then " · Deleted" else "",V.p "" $ D.s "body" h]) $ A.reverse $ D.rows "history" task]]
  where
  running = A.any (\r -> D.s "task" r == D.s "id" task && A.elem (D.s "status" r) ["queued","running"]) $ D.rows "requests" group

taskState :: String -> String
taskState = case _ of
  "backlog" -> "Backlog"
  "in_progress" -> "In progress"
  "done" -> "Done"
  other -> other

tasks :: M.Model -> N.WorkView -> N.Period -> V.Html
tasks m view period = V.node "section" [V.cls "task-browser"] [V.node "div" [V.id "task-list",V.cls "task-list",V.label $ N.periodLabel period <> " tasks",V.attr "aria-busy" $ show loading] content]
  where
  past = period == N.Executed
  key = "tasks:" <> m.selected
  document = fromMaybe M.NotAsked $ O.lookup key m.documents
  loading = case document of
    M.Loading _ _ -> true
    _ -> false
  rows = A.filter (\t -> M.past (D.s "state" t) == past && (view /= N.Automated || D.s "taskType" t == "automated")) $ D.arr $ fromMaybe D.null $ M.valueOf document
  content = if M.isGroup (M.selected m) then groupContent else case document of
    M.Failed _ _ -> [V.div "task-empty" [V.p "" "Could not load tasks.",V.button "task-copy" "Try again" $ RetryDocument key]]
    _ -> if M.valueOf document == Nothing then [V.p "task-empty" "Loading tasks…"] else if A.null rows then [emptyTasks (if past then "No executed tasks" else "No planned tasks") (if past then "Completed and stopped tasks will appear here." else "Queued, running and waiting tasks will appear here.")] else map (taskRow m) rows
  group = M.selected m
  groupRows = A.filter (\t -> (view /= N.Automated || D.s "taskType" t == "automated") && if period == N.Deleted then D.b "deleted" t else not (D.b "deleted" t) && (D.s "state" t == "done") == past) $ D.rows "tasks" group
  calls = if view == N.Automated || period == N.Deleted then [] else A.filter (\t -> M.past (D.s "state" t) == past) $ groupCalls m group
  groupContent = if A.null groupRows && A.null calls then [V.p "empty" "No tasks here yet."] else map (groupTask m group) groupRows <> map (taskRow m) calls

routines :: M.Model -> N.Period -> V.Html
routines m period = V.node "section" [V.cls "task-browser"] [V.node "div" [V.cls "task-list",V.label $ N.periodLabel period <> " scheduled routines"] $ if A.null rows then [emptyTasks (if period == N.Paused then "No paused routines" else "No planned routines") (if period == N.Paused then "Paused routines will appear here." else "Create a scheduled routine through chat.")] else map (taskRow m) rows]
  where
  rows = map (\r -> D.merge r $ D.json {routine: true, sender: D.get "owner" r, state: if D.b "paused" r then "Paused" else "Planned", minutes: show $ D.int $ D.get "minutes" r, meta: "Every " <> show (D.int $ D.get "minutes" r) <> " minutes" <> if D.b "paused" r then "" else " · " <> B.formatDate "" D.empty (D.s "nextDue" r), body: "Frequency: every " <> show (D.int $ D.get "minutes" r) <> " minutes\nExecute: " <> D.s "prompt" r}) $ A.filter (\r -> D.s "owner" r == m.selected && D.b "paused" r == (period == N.Paused)) $ D.rows "routines" m.snapshot

personalRoutines :: M.Model -> V.Html
personalRoutines = Routines.view

events :: M.Model -> V.Html
events m = V.node "section" [V.cls "event-log"] [V.div "event-columns event-head" $ map (\s -> V.span [] (s)) ["Timestamp","Actor","Entity","Event","Agency",""],V.div "event-rows" $ if A.null rows then [V.p "empty" "No events yet."] else map event rows]
  where
  rows = A.take 100 $ D.arr $ D.at ["events",m.selected] m.snapshot
  name id = case id of
    "admin" -> "Admin"
    "system" -> "System"
    "corpora" -> "Corpora"
    _ -> D.fallback (if S.take 12 id == "routine_sch_" then "@" <> id else id) $ D.s "name" $ D.find id m.agents
  event row = let key = "event:" <> D.encode (D.get "id" row) in V.node "details" [V.cls "task-row event-row",V.bool "open" $ Set.member key m.expanded]
    [V.node "summary" [V.cls "event-columns",V.on "click" \e -> Link e (ToggleExpanded key)] (map (\v -> V.span [V.attr "title" v] (v)) [B.formatDate "en-GB" (D.json {day: "2-digit", month: "short", hour: "2-digit", minute: "2-digit", second: "2-digit", fractionalSecondDigits: 3}) (D.s "timestamp" row),name $ D.s "actor" row,name $ D.s "entity" row,D.s "event" row,D.fallback "—" $ D.s "agency" row,"›"])
    ,V.when (Set.member key m.expanded) $ V.textNode "pre" [] (D.encode $ D.get "details" row)]
