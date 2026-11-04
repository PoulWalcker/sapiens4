module Sapiens.Domain where

import Prelude
import Data.Argonaut.Encode (class EncodeJson)
import Data.Argonaut.Core (Json, toObject, toArray, toString, toBoolean, toNumber)
import Data.Array as A
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Traversable (traverse)
import Foreign.Object as O
import Sapiens.Data as D
import Sapiens.Navigation as N
import Sapiens.Browser as Browser

data EntityKind = Sapi | Group
derive instance eqEntityKind :: Eq EntityKind

data CallState = Queued | Running | OutputPending | Done | Warning | Failed | Interrupted | Conflict | Cancelled | OtherCallState String
derive instance eqCallState :: Eq CallState

callState :: String -> CallState
callState = case _ of
  "queued" -> Queued
  "running" -> Running
  "output_pending" -> OutputPending
  "done" -> Done
  "warning" -> Warning
  "failed" -> Failed
  "interrupted" -> Interrupted
  "conflict" -> Conflict
  "cancelled" -> Cancelled
  value -> OtherCallState value

callName :: CallState -> String
callName = case _ of
  Queued -> "Queued"
  Running -> "Running"
  OutputPending -> "Awaiting output pulse"
  Done -> "Completed"
  Warning -> "Warning"
  Failed -> "Failed"
  Interrupted -> "Interrupted"
  Conflict -> "Needs review"
  Cancelled -> "Dismissed"
  OtherCallState value -> value

blocksChat :: Turn -> Boolean
blocksChat request = A.elem request.status [Queued,Running,OutputPending]

type Attachment =
  { id :: String, kind :: AttachmentKind, name :: String, value :: String }

attachment :: Json -> Attachment
attachment value =
  { id: D.s "id" value
  , kind: attachmentKind $ D.s "kind" value
  , name: D.s "name" value
  , value: D.s "value" value
  }

type Origin =
  { agent :: String, call :: String, group :: String, caller :: String, routine :: String, maintenance :: Boolean }

origin :: Json -> Origin
origin value =
  { agent: D.s "agent" value
  , call: D.s "call" value
  , group: D.s "group" value
  , caller: D.s "caller" value
  , routine: D.s "routine" value
  , maintenance: A.any (\key -> D.valid $ D.get key value) ["preparation","analysis","repairAudit","kitInspection","standaloneRun"]
  }

type Turn =
  { id :: String, agent :: String, group :: String, flow :: String, status :: CallState, input :: String, output :: String, hasOutput :: Boolean, created :: String, output_at :: String, output_order :: Number, error :: String, batch_id :: String, origin :: Origin, attachments :: Array Attachment }

turn :: Json -> Turn
turn value =
  { id: D.s "id" value
  , agent: D.s "agent" value
  , group: D.s "group" value
  , flow: D.s "flow" value
  , status: callState $ D.s "status" value
  , input: D.s "input" value
  , output: D.s "output" value
  , hasOutput: D.valid $ D.get "output" value
  , created: D.s "created" value
  , output_at: D.s "output_at" value
  , output_order: D.n "output_order" value
  , error: D.s "error" value
  , batch_id: D.s "batch_id" value
  , origin: origin $ D.get "origin" value
  , attachments: map attachment $ D.rows "attachments" value
  }

type Workload =
  { revision :: String, id :: String, taskId :: String, taskType :: String, callId :: String, owner :: String, state :: WorkState, output :: String, error :: String, origin :: Origin, participants :: Array String }

workload :: Json -> Workload
workload value =
  { revision: D.encode value
  , id: D.s "id" value
  , taskId: D.s "taskId" value
  , taskType: D.s "taskType" value
  , callId: D.s "callId" value
  , owner: D.s "owner" value
  , state: workState $ D.s "state" value
  , output: D.s "output" value
  , error: D.s "error" value
  , origin: origin $ D.get "origin" value
  , participants: D.strings $ D.get "participants" value
  }

type GroupMessage =
  { id :: String, author :: String, text :: String, created :: String, turn :: String, stale :: Boolean, attachments :: Array Attachment }

groupMessage :: Json -> GroupMessage
groupMessage value =
  { id: D.s "id" value
  , author: D.s "author" value
  , text: D.s "text" value
  , created: D.s "created" value
  , turn: D.s "turn" value
  , stale: D.b "stale" value
  , attachments: map attachment $ D.rows "attachments" value
  }

type GroupRequest =
  { id :: String, message :: String, target :: String, task :: String, hasTask :: Boolean, memo :: Boolean, status :: CallState, error :: String }

groupRequest :: Json -> GroupRequest
groupRequest value =
  { id: D.s "id" value
  , message: D.s "message" value
  , target: D.s "target" value
  , task: D.s "task" value
  , hasTask: D.valid $ D.get "task" value
  , memo: D.b "memo" value
  , status: callState $ D.s "status" value
  , error: D.s "error" value
  }

type TaskRevision =
  { title :: String, body :: String, assignee :: String, state :: GroupState, deleted :: Boolean }

taskRevision :: Json -> TaskRevision
taskRevision value =
  { title: D.s "title" value
  , body: D.s "body" value
  , assignee: D.s "assignee" value
  , state: groupState $ D.s "state" value
  , deleted: D.b "deleted" value
  }

type TaskResult =
  { author :: String, stale :: Boolean, text :: String, error :: String }

taskResult :: Json -> TaskResult
taskResult value =
  { author: D.s "author" value
  , stale: D.b "stale" value
  , text: D.s "text" value
  , error: D.s "error" value
  }

type GroupTask =
  { id :: String, title :: String, body :: String, assignee :: String, state :: GroupState, deleted :: Boolean, taskType :: String, revision :: Number, results :: Array TaskResult, history :: Array TaskRevision }

groupTask :: Json -> GroupTask
groupTask value =
  { id: D.s "id" value
  , title: D.s "title" value
  , body: D.s "body" value
  , assignee: D.s "assignee" value
  , state: groupState $ D.s "state" value
  , deleted: D.b "deleted" value
  , taskType: D.s "taskType" value
  , revision: D.n "revision" value
  , results: map taskResult $ D.rows "results" value
  , history: map taskRevision $ D.rows "history" value
  }

type Notes =
  { revision :: String, path :: String, workspace :: String, modified_at :: String, modified_by :: String, created_at :: String, created_by :: String }

notes :: Json -> Notes
notes value =
  { revision: D.s "revision" value
  , path: D.s "path" value
  , workspace: D.s "workspace" value
  , modified_at: D.s "modified_at" value
  , modified_by: D.s "modified_by" value
  , created_at: D.s "created_at" value
  , created_by: D.s "created_by" value
  }

type Entity =
  { id :: String, name :: String, role :: String, color :: String, face :: String, retired :: Boolean, archived :: Boolean, created :: String, updated :: String, description :: String, lead :: String, workspace :: String, revision :: Number, kind :: EntityKind, busy :: Boolean, lastActivity :: Number, preview :: String, stripes :: Array String, members :: Array String, messages :: Array GroupMessage, requests :: Array GroupRequest, tasks :: Array GroupTask, notes :: Notes }

entity :: Json -> Entity
entity value =
  { id: D.s "id" value
  , name: D.s "name" value
  , role: D.s "role" value
  , color: D.s "color" value
  , face: D.s "face" value
  , retired: D.b "retired" value
  , archived: D.b "archived" value
  , created: D.s "created" value
  , updated: D.s "updated" value
  , description: D.s "description" value
  , lead: D.s "lead" value
  , workspace: D.s "workspace" value
  , revision: D.n "revision" value
  , kind: Sapi
  , busy: false
  , lastActivity: 0.0
  , preview: ""
  , stripes: D.strings $ D.get "stripes" value
  , members: D.strings $ D.get "members" value
  , messages: map groupMessage $ D.rows "messages" value
  , requests: map groupRequest $ D.rows "requests" value
  , tasks: map groupTask $ D.rows "tasks" value
  , notes: notes $ D.get "notes" value
  }

type Activity =
  { turn :: String, stopping :: Boolean, started :: Number, updated :: Number, phase :: String, tool :: String, last_action :: String }

activity :: Json -> Activity
activity value =
  { turn: D.s "turn" value
  , stopping: D.b "stopping" value
  , started: D.n "started" value
  , updated: D.n "updated" value
  , phase: D.s "phase" value
  , tool: D.s "tool" value
  , last_action: D.s "last_action" value
  }

type Orchestration =
  { manager :: String, notes :: Notes, mode :: String, timeout :: Number }

orchestration :: Json -> Orchestration
orchestration value =
  { manager: D.s "manager" value
  , notes: notes $ D.get "notes" value
  , mode: D.str $ D.at ["execution","mode"] value
  , timeout: D.num $ D.at ["execution","timeout_seconds"] value
  }

type Routine =
  { id :: String, owner :: String, title :: String, prompt :: String, minutes :: Number, frequency :: String, nextDue :: String, runCount :: Number, paused :: Boolean }

routine :: Json -> Routine
routine value =
  { id: D.s "id" value
  , owner: D.s "owner" value
  , title: D.s "title" value
  , prompt: D.s "prompt" value
  , minutes: D.n "minutes" value
  , frequency: if D.s "kind" (D.get "schedule" value) == "daily" then "Daily at " <> D.s "time" (D.get "schedule" value) <> " · " <> D.s "timezone" (D.get "schedule" value) else "Every " <> show (D.int $ D.get "minutes" value) <> " minutes"
  , nextDue: D.s "nextDue" value
  , runCount: D.n "runCount" value
  , paused: D.b "paused" value
  }

type Event =
  { id :: String, timestamp :: String, actor :: String, entity :: String, event :: String, agency :: String, details :: String }

event :: Json -> Event
event value =
  { id: D.encode $ D.get "id" value
  , timestamp: D.s "timestamp" value
  , actor: D.s "actor" value
  , entity: D.s "entity" value
  , event: D.s "event" value
  , agency: D.s "agency" value
  , details: D.encode $ D.get "details" value
  }

type Task =
  { id :: String, title :: String, body :: String, state :: WorkState, owner :: String, sender :: String, taskType :: String, result :: String, error :: String, routine :: Boolean, minutes :: String, prompt :: String, meta :: String, runCount :: Int }

task :: Json -> Task
task value =
  { id: D.s "id" value
  , title: D.s "title" value
  , body: D.s "body" value
  , state: workState $ D.s "state" value
  , owner: D.s "owner" value
  , sender: D.s "sender" value
  , taskType: D.s "taskType" value
  , result: D.s "result" value
  , error: D.s "error" value
  , routine: false
  , minutes: ""
  , prompt: ""
  , meta: ""
  , runCount: 0
  }

type Message =
  { chatKey :: String, user :: Boolean, author :: String, text :: String, created :: String
  , attachments :: Array Attachment, request :: Maybe Turn, delegated :: Boolean, warning :: String
  , turnId :: String, stale :: Boolean, outputOrder :: Number }

-- Retain the automation wire payload while its versioned evidence is rendered by
-- the Routine UI; the rest of the shell uses the decoded typed projection.
type Snapshot =
  { automation :: Json, agents :: Array Entity, groups :: Array Entity, turns :: Array Turn, workloads :: Array Workload
  , routines :: Array Routine, events :: O.Object (Array Event), activity :: O.Object Activity
  , run_activity :: O.Object Activity, orchestration :: O.Object Orchestration
  , main_agent_id :: String, creation_template :: String, corpora :: Corpora, preferences :: Preferences
  , attachment_drafts :: O.Object (Array Attachment), computer :: {owner :: String, built :: Boolean, platform :: String} }

snapshot :: Json -> Snapshot
snapshot value =
  { automation:value, agents: map entity $ D.rows "agents" value
  , groups: map (\g -> (entity g) {kind=Group}) $ D.rows "groups" value
  , turns: map turn $ D.rows "turns" value
  , workloads: map workload $ D.rows "workloads" value
  , routines: map routine $ D.rows "routines" value
  , events: map (map event <<< D.arr) $ object "events" value
  , activity: map activity $ object "activity" value
  , run_activity: map activity $ object "run_activity" value
  , orchestration: map orchestration $ object "orchestration" value
  , attachment_drafts: map (map attachment <<< D.arr) $ object "attachment_drafts" value
  , main_agent_id: D.s "main_agent_id" value, creation_template: D.s "creation_template" value
  , corpora: corpora $ D.get "corpora" value, preferences: preferences (A.any (\g -> D.s "id" g == D.str (D.at ["preferences","selected"] value)) $ D.rows "groups" value) $ D.get "preferences" value
  , computer: {owner:D.str $ D.at ["computer","owner"] value,built:D.flag $ D.at ["computer","built"] value,platform:D.str $ D.at ["computer","platform"] value}
  }

object :: String -> Json -> O.Object Json
object key = fromMaybe O.empty <<< toObject <<< D.get key

findEntity :: String -> Array Entity -> Maybe Entity
findEntity id = A.find ((id == _) <<< _.id)

emptyEntity :: Entity
emptyEntity = entity D.empty

emptySnapshot :: Snapshot
emptySnapshot = snapshot D.empty

data Document = TaskDocument (Array Task) | MemoDocument MemoDocument
derive instance eqDocument :: Eq Document

decodeDocument :: Boolean -> Json -> Either String Document
decodeDocument tasks value
  | tasks = case toArray $ D.get "tasks" value of
      Nothing -> Left "Invalid tasks response"
      Just rows -> TaskDocument <$> traverse decodeTask rows
  | isJust (toString $ D.get "content" value) && isJust (toString $ D.get "path" value) =
      Right $ MemoDocument {content:D.s "content" value,path:D.s "path" value,metadata:noteMetadata $ D.get "metadata" value}
  | otherwise = Left "Invalid Memo response"

decodeTask :: Json -> Either String Task
decodeTask row = if A.all (\key -> isJust $ toString $ D.get key row) ["id","title","body","state","owner","sender"]
    then Right $ task row else Left "Invalid tasks response"

instance encodeEntityKind :: EncodeJson EntityKind where
  encodeJson kind = D.text $ if kind == Group then "group" else "sapi"

instance encodeCallState :: EncodeJson CallState where
  encodeJson = D.text <<< case _ of
    Queued -> "queued"
    Running -> "running"
    OutputPending -> "output_pending"
    Done -> "done"
    Warning -> "warning"
    Failed -> "failed"
    Interrupted -> "interrupted"
    Conflict -> "conflict"
    Cancelled -> "cancelled"
    OtherCallState value -> value

data WorkState = WorkQueued | WorkRunning | WorkCompleted | WorkUnresolved | WorkDelegated | WorkWaitingForAdmin | WorkFailed | WorkInterrupted | WorkCancelled | WorkPaused | WorkPlanned | OtherWorkState String
derive instance eqWorkState :: Eq WorkState

workState :: String -> WorkState
workState = case _ of
  "Queued" -> WorkQueued
  "Running" -> WorkRunning
  "Completed" -> WorkCompleted
  "Unresolved" -> WorkUnresolved
  "Delegated" -> WorkDelegated
  "WaitingForAdmin" -> WorkWaitingForAdmin
  "Failed" -> WorkFailed
  "Interrupted" -> WorkInterrupted
  "Cancelled" -> WorkCancelled
  "Paused" -> WorkPaused
  "Planned" -> WorkPlanned
  value -> OtherWorkState value

workName :: WorkState -> String
workName = case _ of
  WorkQueued -> "Queued"
  WorkRunning -> "Running"
  WorkCompleted -> "Completed"
  WorkUnresolved -> "Unresolved"
  WorkDelegated -> "Delegated"
  WorkWaitingForAdmin -> "WaitingForAdmin"
  WorkFailed -> "Failed"
  WorkInterrupted -> "Interrupted"
  WorkCancelled -> "Cancelled"
  WorkPaused -> "Paused"
  WorkPlanned -> "Planned"
  OtherWorkState value -> value

instance encodeWorkState :: EncodeJson WorkState where
  encodeJson = D.text <<< workName

data GroupState = GroupBacklog | GroupInProgress | GroupDone | OtherGroupState String
derive instance eqGroupState :: Eq GroupState

groupState :: String -> GroupState
groupState = case _ of
  "backlog" -> GroupBacklog
  "in_progress" -> GroupInProgress
  "done" -> GroupDone
  value -> OtherGroupState value

groupStateName :: GroupState -> String
groupStateName = case _ of
  GroupBacklog -> "backlog"
  GroupInProgress -> "in_progress"
  GroupDone -> "done"
  OtherGroupState value -> value

instance encodeGroupState :: EncodeJson GroupState where
  encodeJson = D.text <<< groupStateName

type Tab = {id :: String, url :: String, path :: String, title :: String, zoom :: Number, can_back :: Boolean, can_forward :: Boolean, loading :: Boolean, error :: String, command :: Maybe {seq :: String, action :: String}}
type Bookmark = {id :: String, url :: String, title :: String, tab_id :: String}
type Workspace = {tabs :: Array Tab, bookmarks :: Array Bookmark, activeTab :: String}

tab :: Json -> Tab
tab value = {id:D.s "id" value,url:D.s "url" value,path:D.s "path" value,title:D.s "title" value,zoom:D.n "zoom" value,can_back:D.b "can_back" value,can_forward:D.b "can_forward" value,loading:D.b "loading" value,error:D.s "error" value,command:if D.valid $ D.get "command" value then Just {seq:D.str $ D.at ["command","seq"] value,action:D.str $ D.at ["command","action"] value} else Nothing}

bookmark :: Json -> Bookmark
bookmark value = {id:D.s "id" value,url:D.s "url" value,title:D.s "title" value,tab_id:D.s "tab_id" value}

workspace :: Json -> Workspace
workspace value = {tabs:map tab $ D.rows "tabs" value,bookmarks:map bookmark $ D.rows "bookmarks" value,activeTab:D.s "activeTab" value}

workspaces :: Json -> O.Object Workspace
workspaces = map workspace <<< fromMaybe O.empty <<< toObject

type Invitation = {v :: String, relay :: String, room :: String, token :: String, secret :: String, desktop :: String, expires :: Number, public_id :: String}

invitation :: Json -> Invitation
invitation value = {v:D.s "v" value,relay:D.s "relay" value,room:D.s "room" value,token:D.s "token" value,secret:D.s "secret" value,desktop:D.s "desktop" value,expires:D.n "expires" value,public_id:D.s "public_id" value}

data CorporaState = Active | Paused | Archived | OtherCorporaState String
derive instance eqCorporaState :: Eq CorporaState

corporaState :: String -> CorporaState
corporaState = case _ of
  "active" -> Active
  "paused" -> Paused
  "archived" -> Archived
  value -> OtherCorporaState value

corporaStateName :: CorporaState -> String
corporaStateName = case _ of
  Active -> "active"
  Paused -> "paused"
  Archived -> "archived"
  OtherCorporaState value -> value

instance encodeCorporaState :: EncodeJson CorporaState where
  encodeJson = D.text <<< corporaStateName

data Location = Local | Remote | Unspecified
derive instance eqLocation :: Eq Location

locationName :: Location -> String
locationName = case _ of
  Local -> "Local"
  Remote -> "Remote"
  Unspecified -> ""

instance encodeLocation :: EncodeJson Location where
  encodeJson = D.text <<< locationName

type Corpora = {key :: String, id :: String, name :: String, device :: String, scope :: String, settings_template :: String, chief_id :: String, status :: CorporaState, location :: Location, revision :: Number, archived :: Boolean, stopping :: Boolean, paired :: Boolean}

corpora :: Json -> Corpora
corpora value = {key:D.s "key" value,id:D.s "id" value,name:D.s "name" value,device:D.s "device" value,scope:D.s "scope" value,settings_template:D.s "settings_template" value,chief_id:D.s "chief_id" value,status:corporaState $ D.s "status" value,location:case D.s "location" value of
  "Local" -> Local
  "Remote" -> Remote
  _ -> Unspecified
  ,revision:D.n "revision" value,archived:D.b "archived" value,stopping:D.b "stopping" value,paired:D.b "paired" value}

updateCorpora :: Corpora -> Corpora -> Corpora
updateCorpora old fresh = fresh {key=old.key,paired=old.paired,location=if fresh.location == Unspecified then old.location else fresh.location}

type Preferences =
  {selected :: String, activity :: N.Activity, scope :: N.Scope, groupView :: Maybe String
  ,drafts :: O.Object String, panes :: Array N.Pane, workspaces :: O.Object Workspace
  ,workspaceRevision :: Number, scrollPosition :: Maybe Browser.ScrollPosition}

preferences :: Boolean -> Json -> Preferences
preferences group value =
  {selected:D.s "selected" value,activity:N.readActivity group value
  ,scope:if group then N.Groups else case D.s "scope" value of
    "groups" -> N.Groups
    "sapis" -> N.Sapis
    _ -> N.All
  ,groupView:if group then Just (D.s "selected" value) else if D.s "groupView" value == "" then Nothing else Just (D.s "groupView" value)
  ,drafts:D.objectStrings $ D.get "drafts" value
  ,panes:if not (D.valid $ D.get "panes" value) then [N.ChatPane,N.Sidebar,N.Workspace] else A.filter (\pane -> D.b (N.paneId pane) $ D.get "panes" value) [N.ChatPane,N.Sidebar,N.Workspace]
  ,workspaces:workspaces $ D.get "workspaces" value,workspaceRevision:D.n "workspace_revision" value
  ,scrollPosition:if D.valid $ D.get "chatPosition" value then Just $ Browser.scrollPosition $ D.get "chatPosition" value else Nothing}

-- Missing optional fields are supported for cached installations. Present values
-- must have the wire type we consume; malformed responses never replace state.
type Check = Json -> Either String Unit

primitive :: String -> (Json -> Boolean) -> Check
primitive expected valid value = if valid value then Right unit else Left $ "expected " <> expected

stringValue :: Check
stringValue = primitive "text" (isJust <<< toString)
numberValue :: Check
numberValue = primitive "number" (isJust <<< toNumber)
booleanValue :: Check
booleanValue = primitive "boolean" (isJust <<< toBoolean)

list :: Check -> Check
list check value = case toArray value of
  Nothing -> Left "expected array"
  Just rows -> void $ traverse check rows

field :: String -> Check -> Check
field name check value = case check $ D.get name value of
  Left failure -> Left $ name <> ": " <> failure
  Right _ -> Right unit

optional :: String -> Check -> Check
optional name check value = if D.valid $ D.get name value then field name check value else Right unit

record :: Array Check -> Check
record checks value = if isJust $ toObject value then void $ traverse (\check -> check value) checks else Left "expected object"

mapValues :: Check -> Check
mapValues check value = case toObject value of
  Nothing -> Left "expected object"
  Just values -> void $ traverse check values

checkAttachment :: Check
checkAttachment = record [optional "id" stringValue, optional "kind" stringValue, optional "name" stringValue, optional "value" stringValue]

checkOrigin :: Check
checkOrigin = record [optional "agent" stringValue, optional "call" stringValue, optional "group" stringValue, optional "caller" stringValue, optional "routine" stringValue]

checkTurn :: Check
checkTurn = record [optional "id" stringValue, optional "agent" stringValue, optional "group" stringValue, optional "flow" stringValue, optional "status" stringValue, optional "input" stringValue, optional "output" stringValue, optional "created" stringValue, optional "output_at" stringValue, optional "output_order" numberValue, optional "error" stringValue, optional "batch_id" stringValue, optional "origin" checkOrigin, optional "attachments" (list checkAttachment)]

checkWorkload :: Check
checkWorkload = record [optional "id" stringValue, optional "taskId" stringValue, optional "taskType" stringValue, optional "callId" stringValue, optional "owner" stringValue, optional "state" stringValue, optional "output" stringValue, optional "error" stringValue, optional "origin" checkOrigin, optional "participants" (list stringValue)]

checkGroupMessage :: Check
checkGroupMessage = record [optional "id" stringValue, optional "author" stringValue, optional "text" stringValue, optional "created" stringValue, optional "turn" stringValue, optional "stale" booleanValue, optional "attachments" (list checkAttachment)]

checkGroupRequest :: Check
checkGroupRequest = record [optional "id" stringValue, optional "message" stringValue, optional "target" stringValue, optional "task" stringValue, optional "memo" booleanValue, optional "status" stringValue, optional "error" stringValue]

checkTaskRevision :: Check
checkTaskRevision = record [optional "title" stringValue, optional "body" stringValue, optional "assignee" stringValue, optional "state" stringValue, optional "deleted" booleanValue]

checkTaskResult :: Check
checkTaskResult = record [optional "author" stringValue, optional "stale" booleanValue, optional "text" stringValue, optional "error" stringValue]

checkGroupTask :: Check
checkGroupTask = record [optional "id" stringValue, optional "title" stringValue, optional "body" stringValue, optional "assignee" stringValue, optional "state" stringValue, optional "deleted" booleanValue, optional "taskType" stringValue, optional "revision" numberValue, optional "results" (list checkTaskResult), optional "history" (list checkTaskRevision)]

checkNotes :: Check
checkNotes = record [optional "revision" stringValue, optional "path" stringValue, optional "workspace" stringValue, optional "modified_at" stringValue, optional "modified_by" stringValue, optional "created_at" stringValue, optional "created_by" stringValue]

checkEntity :: Check
checkEntity = record [optional "id" stringValue, optional "name" stringValue, optional "role" stringValue, optional "color" stringValue, optional "face" stringValue, optional "retired" booleanValue, optional "archived" booleanValue, optional "created" stringValue, optional "updated" stringValue, optional "description" stringValue, optional "lead" stringValue, optional "workspace" stringValue, optional "revision" numberValue, optional "stripes" (list stringValue), optional "members" (list stringValue), optional "messages" (list checkGroupMessage), optional "requests" (list checkGroupRequest), optional "tasks" (list checkGroupTask), optional "notes" checkNotes]

checkActivity :: Check
checkActivity = record [optional "turn" stringValue, optional "stopping" booleanValue, optional "started" numberValue, optional "updated" numberValue, optional "phase" stringValue, optional "tool" stringValue, optional "last_action" stringValue]

checkOrchestration :: Check
checkOrchestration = record [optional "manager" stringValue, optional "notes" checkNotes]

checkRoutine :: Check
checkRoutine = record [optional "id" stringValue, optional "owner" stringValue, optional "title" stringValue, optional "prompt" stringValue, optional "minutes" numberValue, optional "schedule" (record [field "kind" stringValue,field "time" stringValue,field "timezone" stringValue]), optional "nextDue" stringValue, optional "runCount" numberValue, optional "paused" booleanValue]

checkEvent :: Check
checkEvent = record [optional "timestamp" stringValue, optional "actor" stringValue, optional "entity" stringValue, optional "event" stringValue, optional "agency" stringValue]

checkTask :: Check
checkTask = record [optional "id" stringValue, optional "title" stringValue, optional "body" stringValue, optional "state" stringValue, optional "owner" stringValue, optional "sender" stringValue, optional "taskType" stringValue, optional "result" stringValue, optional "error" stringValue]

decodeSnapshot :: Json -> Either String Snapshot
decodeSnapshot value = case check value of
  Left failure -> Left $ "Invalid snapshot response: " <> failure
  Right _ -> Right $ snapshot value
  where
  check = record
    [field "agents" (list checkEntity),field "turns" (list checkTurn)
    ,field "main_agent_id" stringValue,field "corpora" (record [field "id" stringValue,optional "status" stringValue])
    ,optional "groups" (list checkEntity),optional "workloads" (list checkWorkload)
    ,optional "routines" (list checkRoutine),optional "orchestration" (mapValues checkOrchestration)
    ,optional "activity" (mapValues checkActivity),optional "run_activity" (mapValues checkActivity)
    ,optional "attachment_drafts" (mapValues $ list checkAttachment)]

type MemoDocument = {content :: String, path :: String, metadata :: NoteMetadata}

type NoteMetadata = { revision :: Maybe String, path :: Maybe String, workspace :: Maybe String, modified_at :: Maybe String, modified_by :: Maybe String, created_at :: Maybe String, created_by :: Maybe String }

noteMetadata :: Json -> NoteMetadata
noteMetadata value = { revision: metadataField "revision" value, path: metadataField "path" value, workspace: metadataField "workspace" value, modified_at: metadataField "modified_at" value, modified_by: metadataField "modified_by" value, created_at: metadataField "created_at" value, created_by: metadataField "created_by" value }

metadataField :: String -> Json -> Maybe String
metadataField key value = if O.member key (fromMaybe O.empty $ toObject value) then Just $ D.s key value else Nothing

withMetadata :: Notes -> NoteMetadata -> Notes
withMetadata original patch = { revision: fromMaybe original.revision patch.revision, path: fromMaybe original.path patch.path, workspace: fromMaybe original.workspace patch.workspace, modified_at: fromMaybe original.modified_at patch.modified_at, modified_by: fromMaybe original.modified_by patch.modified_by, created_at: fromMaybe original.created_at patch.created_at, created_by: fromMaybe original.created_by patch.created_by }

data AttachmentKind = ImageAttachment | DocumentAttachment | LinkAttachment | FileAttachment | OtherAttachment String
derive instance eqAttachmentKind :: Eq AttachmentKind

attachmentKind :: String -> AttachmentKind
attachmentKind = case _ of
  "image" -> ImageAttachment
  "document" -> DocumentAttachment
  "link" -> LinkAttachment
  "filepath" -> FileAttachment
  value -> OtherAttachment value

attachmentKindName :: AttachmentKind -> String
attachmentKindName = case _ of
  ImageAttachment -> "image"
  DocumentAttachment -> "document"
  LinkAttachment -> "link"
  FileAttachment -> "filepath"
  OtherAttachment value -> value

instance encodeAttachmentKind :: EncodeJson AttachmentKind where
  encodeJson = D.text <<< attachmentKindName

data LifecycleIntent = PauseCorpora | ActivateCorpora
derive instance eqLifecycleIntent :: Eq LifecycleIntent

lifecycleName :: LifecycleIntent -> String
lifecycleName = case _ of
  PauseCorpora -> "pause"
  ActivateCorpora -> "activate"
