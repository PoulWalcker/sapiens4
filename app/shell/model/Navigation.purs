module Sapiens.Navigation where

import Prelude
import Sapiens.Environment (Environment)
import Data.Array as A
import Data.Maybe (fromMaybe)
import Data.Argonaut.Core (Json)
import Sapiens.Data as D

data Pane = Sidebar | ChatPane | Workspace

derive instance eqPane :: Eq Pane

data ChatView = Conversation | Pins | Threads | Comments

derive instance eqChatView :: Eq ChatView

data WorkView = Memo | Tasks | TaskCall | Backlog | Automated | Automation | Routines | Scheduled | Callbacks | Jobs | Agentic | Pipelines | Composers | ComposeWorkflow | ComposeRoutine | ComposeCall

derive instance eqWorkView :: Eq WorkView

data Period = Planned | Executed | Paused | Deleted

derive instance eqPeriod :: Eq Period

data Activity = Chat ChatView | Work WorkView Period | Updates

derive instance eqActivity :: Eq Activity

data Scope = All | Sapis | Groups

derive instance eqScope :: Eq Scope

data NavTree = Branch String String WorkView (Array NavTree) | Leaf WorkView

workTree :: Array NavTree
workTree = [Leaf Memo, Branch "tasks" "Tasks" Tasks [Leaf TaskCall, Leaf Backlog, Leaf Automated], Branch "automation" "Automation" Automation [Branch "routine" "Routines" Routines [Leaf Scheduled, Leaf Callbacks, Leaf Jobs], Branch "workflows" "Workflows" Agentic [Leaf Agentic, Leaf Pipelines, Leaf Composers], Branch "composer" "Composers" ComposeWorkflow [Leaf ComposeWorkflow, Leaf ComposeRoutine, Leaf ComposeCall]]]

workId :: WorkView -> String
workId = case _ of
  Memo -> "memo"
  Tasks -> "tasks"
  TaskCall -> "tasks-call"
  Backlog -> "tasks-backlog"
  Automated -> "tasks-automated"
  Automation -> "automation"
  Routines -> "routines"
  Scheduled -> "routine-scheduled"
  Callbacks -> "routine-callback"
  Jobs -> "routine-job"
  Agentic -> "workflow-agentic"
  Pipelines -> "workflow-pipeline"
  Composers -> "workflow-composer"
  ComposeWorkflow -> "composer-workflow"
  ComposeRoutine -> "composer-routine"
  ComposeCall -> "composer-call"

workLabel :: WorkView -> String
workLabel = case _ of
  Memo -> "Memo"
  Tasks -> "Tasks"
  TaskCall -> "Call"
  Backlog -> "Backlog"
  Automated -> "Automated"
  Automation -> "Automation"
  Routines -> "Routines"
  Scheduled -> "Scheduled"
  Callbacks -> "Callbacks"
  Jobs -> "Jobs"
  Agentic -> "Agentic"
  Pipelines -> "Pipelines"
  Composers -> "Composers"
  ComposeWorkflow -> "Workflow"
  ComposeRoutine -> "Routine"
  ComposeCall -> "Call"

chatId :: ChatView -> String
chatId = case _ of
  Conversation -> "conversation"
  Pins -> "pins"
  Threads -> "threads"
  Comments -> "comments"

chatLabel :: ChatView -> String
chatLabel = case _ of
  Conversation -> "Conversation"
  Pins -> "Pins"
  Threads -> "Threads"
  Comments -> "Comments"

periodId :: Period -> String
periodId = case _ of
  Planned -> "upcoming"
  Executed -> "past"
  Paused -> "paused"
  Deleted -> "deleted"

periodLabel :: Period -> String
periodLabel = case _ of
  Planned -> "Planned"
  Executed -> "Executed"
  Paused -> "Paused"
  Deleted -> "Deleted"

paneId :: Pane -> String
paneId = case _ of
  Sidebar -> "sidebar"
  ChatPane -> "chat"
  Workspace -> "workspace"

scopeId :: Scope -> String
scopeId = case _ of
  All -> "all"
  Sapis -> "sapis"
  Groups -> "groups"

panel :: Activity -> String
panel = case _ of
  Chat _ -> "chat"
  Work _ _ -> "work"
  Updates -> "updates"

view :: Activity -> String
view = case _ of
  Chat c -> chatId c
  Work w _ -> workId w
  Updates -> "events"

period :: Activity -> Period
period = case _ of
  Work _ p -> p
  _ -> Planned

readActivity :: Boolean -> Json -> Activity
readActivity group prefs =
  let p = case D.s "period" prefs of
        "past" -> Executed
        "paused" -> Paused
        "deleted" | group -> Deleted
        _ -> Planned
      w = fromMaybe Memo $ A.find ((_ == D.s "view" prefs) <<< workId) [Memo,Tasks,TaskCall,Backlog,Automated,Automation,Routines,Scheduled,Callbacks,Jobs,Agentic,Pipelines,Composers,ComposeWorkflow,ComposeRoutine,ComposeCall]
      work v = Work v (if v == Scheduled && p == Paused || A.elem v [Tasks,Automated] && A.elem p [Executed,Deleted] then p else Planned)
  in case D.s "panel" prefs of
    "notes" -> work Memo
    "tasks" -> work Tasks
    "work" -> work w
    "updates" -> Updates
    _ -> Chat $ fromMaybe Conversation $ A.find ((_ == D.s "view" prefs) <<< chatId) [Conversation,Pins,Threads,Comments]

encodeActivity :: Activity -> Json
encodeActivity a = D.json {panel: panel a, view: view a, period: periodId $ period a}

ios :: Environment -> Boolean
ios env = D.matches "iPad|iPhone|iPod" (env.userAgent) || env.platform == "MacIntel" && env.touchPoints > 1.0

limit :: Environment -> Int
limit env
  | not (env.touch) && env.width > 600.0 = 3
  | env.orientation == "portrait-primary" || env.orientation == "portrait-secondary" = 1
  | env.orientation == "landscape-primary" || env.orientation == "landscape-secondary" = 2
  | env.height >= env.width = 1
  | otherwise = 2

visiblePanes :: Int -> Boolean -> Array Pane -> Array Pane
visiblePanes maxPanes workspace panes =
  let visible = A.take maxPanes $ A.filter (\p -> p /= Workspace || workspace) panes
  in if A.null visible then [ChatPane] else visible

togglePane :: Boolean -> Int -> Boolean -> Pane -> Array Pane -> Array Pane
togglePane toggle maxPanes workspace pane panes =
  let current = if maxPanes == 3 then panes else visiblePanes maxPanes workspace panes
      hide = toggle && A.elem pane current && (maxPanes == 3 || maxPanes == 2 && A.length current > 1)
      next = A.filter (_ /= pane) current
      result = if hide then next else A.cons pane next
  in if maxPanes == 3 then result else visiblePanes maxPanes workspace result
