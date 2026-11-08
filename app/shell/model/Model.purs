module Sapiens.Model where

import Sapiens.Form as F
import Prelude
import Sapiens.Environment (Environment)
import Data.Argonaut.Core (Json, toString)
import Data.Array as A
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set as Set
import Data.String as S
import Data.String.CodePoints as CP
import Data.Tuple (Tuple(..))
import Foreign.Object as O
import Libs4.Browser as B
import Sapiens.Data as D
import Sapiens.Browser as Browser
import Sapiens.Domain as X
import Sapiens.Navigation as N
import Sapiens.RoutineData as RD
import Sapiens.RoutineRequest as RR

data RemoteData error a = NotAsked | Loading Request (Maybe a) | Ready Request a | Failed Request error

data PreviewError = PreviewUnavailable String | InvalidPreview

previewError :: PreviewError -> String
previewError = case _ of
  PreviewUnavailable message -> message
  InvalidPreview -> "Invalid file preview response"

decodePreview :: Json -> Either PreviewError String
decodePreview value = case toString (D.get "content" value) of
  Just content -> Right content
  Nothing -> Left InvalidPreview

type Request = { epoch :: Int, sequence :: Int, key :: String, revision :: String, path :: String }

type DocumentRequest = {request :: Request, kind :: X.DocumentKind, refresh :: Boolean}
type RoutineScroll = {anchor :: String, offset :: Number, top :: Number, width :: Number}

data DialogKind = AppMenu | WorkspaceSettings | Computer | AgentSettings | GroupSettings | Attach | Reference X.AttachmentKind | DecisionPrompts | GroupTask | CreateCorpora | EditCorpora | ConnectRemote | HostPairing | RelayList | RelayAdd | RelaySwitch | RelayTerms | Installation | Notice | RoutineControl | RoutineRequest

derive instance eqDialogKind :: Eq DialogKind

corporaDialog :: DialogKind -> Boolean
corporaDialog kind = A.elem kind [CreateCorpora,EditCorpora,ConnectRemote,HostPairing,RelayList,RelayAdd,RelaySwitch,RelayTerms,Notice]

data DialogStatus = Editing | Submitting String | Rejected String
derive instance eqDialogStatus :: Eq DialogStatus

dialogBusy :: Dialog -> Boolean
dialogBusy d = case d.status of
  Submitting _ -> true
  _ -> false

dialogError :: Dialog -> String
dialogError d = case d.status of
  Editing -> ""
  Submitting message -> message
  Rejected message -> message

setDialogBusy :: Boolean -> Dialog -> Dialog
setDialogBusy running d = d {status=if running then Submitting (dialogError d) else if dialogError d == "" then Editing else Rejected (dialogError d)}

setDialogError :: String -> Dialog -> Dialog
setDialogError message d = d {status=if dialogBusy d then Submitting message else if message == "" then Editing else Rejected message}

type Dialog = { kind :: DialogKind, title :: String, record :: F.Context, fields :: F.Fields, content :: F.Content, status :: DialogStatus, identity :: Int }

data ConnectionState = Offline | Online | ChangingLifecycle Boolean
derive instance eqConnectionState :: Eq ConnectionState

online :: Model -> Boolean
online m = case m.connection of
  Offline -> false
  Online -> true
  ChangingLifecycle reachable -> reachable

busy :: Model -> Boolean
busy m = case m.connection of
  ChangingLifecycle _ -> true
  _ -> false

connectionResult :: Boolean -> ConnectionState -> ConnectionState
connectionResult reachable = case _ of
  ChangingLifecycle _ -> ChangingLifecycle reachable
  _ -> if reachable then Online else Offline

type Model = Record (InterfaceState (ContentState SessionState))

type InterfaceState r =
  ( selected :: String, activity :: N.Activity, scope :: N.Scope, groupView :: Maybe String
  , panes :: Array N.Pane, search :: String, searchOpen :: Boolean, menu :: Maybe String
  , dialog :: Maybe Dialog, dialogStack :: Array Dialog, theme :: String
  , toast :: String, feedback :: String, address :: String, addressOpen :: Boolean
  | r )

type ContentState r =
  ( snapshot :: X.Snapshot, agents :: Array X.Entity, messages :: O.Object (Array X.Message)
  , mention :: Maybe { start :: Int, end :: Int, choices :: Array X.Entity, index :: Int }
  , drafts :: O.Object String, attachments :: O.Object (Array X.Attachment), sending :: Set.Set String
  , uploading :: Int, clarification :: Maybe X.Workload
  , workspaces :: O.Object X.Workspace, workspaceRevision :: Number, documents :: O.Object (RemoteData String X.Document)
  , expanded :: Set.Set String, imageUrls :: O.Object String, preview :: RemoteData PreviewError String
  , scrollPosition :: Maybe Browser.ScrollPosition, restoringScroll :: Boolean
  , routineFilters :: O.Object N.RoutineFilter, routineHistories :: O.Object (Array N.HistoryPosition)
  , routineRequestDrafts :: O.Object Dialog
  , routineChatReturn :: O.Object N.Activity
  , routineReceipts :: O.Object RR.SendReceipt
  , routineControls :: O.Object (RemoteData String RD.ControlReceipt)
  , routineResultSelections :: O.Object RD.ExactResultRef
  , routineScroll :: O.Object RoutineScroll, restoringRoutine :: Maybe String
  , documentFetched :: O.Object Number
  | r )

type SessionState =
  ( polling :: Boolean, pollGeneration :: Int, saving :: Boolean, lastPreview :: Number
  , nativeReporting :: Boolean, nativeReports :: Array Browser.NativeReport
  , epoch :: Int, sequence :: Int, now :: Number, env :: Environment
  , records :: Array X.Corpora, active :: String, connection :: ConnectionState
  , localAvailable :: Boolean, showArchived :: Boolean, installRequired :: Boolean
  , preferencesSaved :: String
  )

initial :: Environment -> Model
initial env =
  { snapshot:X.emptySnapshot, agents:[], messages:O.empty, selected:"", activity:N.Chat N.Conversation, scope:N.All, groupView:Nothing
  , mention:Nothing, drafts:O.empty, attachments:O.empty, sending:Set.empty, uploading:0, clarification:Nothing
  , workspaces:O.empty, workspaceRevision:0.0, documents:O.empty, expanded:Set.empty
  , polling:false,pollGeneration:0,saving:false,lastPreview:0.0,nativeReporting:false,nativeReports:[]
  , epoch:0, sequence:0, now:0.0, env, panes:[N.ChatPane,N.Sidebar,N.Workspace], search:"", searchOpen:false, menu:Nothing, dialog:Nothing
  , records:[], active:"", connection:Offline, localAvailable:false, showArchived:false
  , toast:"", feedback:"", theme:if env.dark then "dark" else "light", installRequired:false
  , dialogStack:[], imageUrls:O.empty, preview:NotAsked, address:"", addressOpen:false, scrollPosition:Nothing, restoringScroll:false, preferencesSaved:""
  , routineRequestDrafts:O.empty,routineChatReturn:O.empty,routineReceipts:O.empty,routineControls:O.empty, routineFilters:O.empty, routineHistories:O.empty, routineResultSelections:O.empty, routineScroll:O.empty, restoringRoutine:Nothing, documentFetched:O.empty
  }

agent :: Model -> String -> X.Entity
agent model id = fromMaybe (fromMaybe X.emptyEntity $ A.head model.agents) $ X.findEntity id model.agents

selected :: Model -> X.Entity
selected model = agent model model.selected

isGroup :: X.Entity -> Boolean
isGroup a = a.kind == X.Group

ownerPath :: Model -> String
ownerPath m = "/api/" <> (if isGroup (selected m) then "groups/" else "agents/") <> B.encodeURI m.selected

activeRecord :: Model -> X.Corpora
activeRecord m = fromMaybe (X.corpora D.empty) $ A.find ((_ == m.active) <<< _.key) m.records

remote :: Model -> Boolean
remote m = O.member "remote" m.env.body

draft :: Model -> String
draft m = fromMaybe "" $ O.lookup m.selected m.drafts

attachments :: Model -> Array X.Attachment
attachments m = fromMaybe [] $ O.lookup m.selected m.attachments

workspace :: Model -> X.Workspace
workspace m = fromMaybe (X.workspace D.empty) $ O.lookup m.selected m.workspaces

tabs :: Model -> Array X.Tab
tabs m = A.filter (\t -> t.url /= "about:blank" || t.path /= "" || not (D.matches "^(blank-|id-)" t.id)) (workspace m).tabs

activeTab :: Model -> X.Tab
activeTab m = let rows = tabs m in fromMaybe (fromMaybe (X.tab D.empty) $ A.head rows) $ A.find ((_ == (workspace m).activeTab) <<< _.id) rows

hasTab :: Model -> Boolean
hasTab m = (activeTab m).id /= ""

visiblePanes :: Model -> Array N.Pane
visiblePanes m = N.visiblePanes (N.limit m.env) (hasTab m) m.panes

withWorkspaces :: X.Preferences -> Model -> Model
withWorkspaces prefs m = if prefs.workspaceRevision > m.workspaceRevision then m {workspaces=prefs.workspaces,workspaceRevision=prefs.workspaceRevision} else m

blocksChat :: X.Turn -> Boolean
blocksChat = X.blocksChat

past :: X.WorkState -> Boolean
past state = A.elem state [X.WorkCompleted,X.WorkUnresolved,X.WorkFailed,X.WorkInterrupted,X.WorkCancelled]

result :: X.Turn -> {text :: String, warning :: String}
result turn =
  let parts = S.split (S.Pattern "\n\n") turn.output
      warning = turn.status == X.Warning
      wrapped = A.head parts == Just "Warning — partial result." && A.length parts >= 3 && D.matches "^The (tool|time) limit was reached after " (fromMaybe "" $ A.last parts)
  in {text:if warning && wrapped then S.joinWith "\n\n" $ A.dropEnd 1 $ A.drop 1 parts else turn.output
     ,warning:if warning then if wrapped then fromMaybe "" (A.last parts) else D.fallback "Partial result; review before relying on it." turn.error else ""}

project :: Json -> Model -> Model
project raw m = projectSnapshot (X.snapshot raw) m

projectSnapshot :: X.Snapshot -> Model -> Model
projectSnapshot snapshot m = normalizeRoutineSelection $ withWorkspaces snapshot.preferences m { snapshot=snapshot, agents=agents, messages=messages, selected=id, groupView=groupView, scope=scope }
  where
  personal = map (\a -> a {busy=A.any (\t -> t.agent == a.id && t.status == X.Running) turns
    , lastActivity=B.parseTime $ D.fallback a.created $ fromMaybe "" $ _.created <$> A.last (A.filter (\t -> t.agent == a.id) turns)
    , preview=D.cut 150 $ D.replace "\\s+" " " $ D.fallback "Ready for your message." $ fromMaybe "" $ _.text <$> (O.lookup a.id personalMessages >>= A.last)}) snapshot.agents
  turns = snapshot.turns
  append owner rows = O.alter (Just <<< (_ <> rows) <<< fromMaybe []) owner
  personalMessages = foldl (\all t -> if t.group /= "" || not (A.elem t.flow ["chat","computer"]) then all else
    let input = {user:true,author:t.origin.caller,text:t.input,created:t.created,attachments:t.attachments,request:Just t,chatKey:"user:" <> t.id,delegated:false,warning:"",turnId:"",stale:false,outputOrder:0.0}
        reply = result t
        output = {user:false,author:"",text:reply.text,warning:reply.warning,created:D.fallback t.created t.output_at,outputOrder:t.output_order,turnId:t.id,chatKey:"assistant:" <> t.id,attachments:[],request:Nothing,delegated:false,stale:false}
    in append t.agent ([input] <> if A.elem t.status [X.Done,X.Warning] && t.hasOutput && not t.historyOnly then [output] else []) all) O.empty turns
  groups = map (\g -> g {busy=A.any ((X.Running == _) <<< _.status) g.requests,lastActivity=B.parseTime g.updated,preview=D.fallback (D.fallback "Shared workspace" g.description) $ fromMaybe "" $ _.text <$> A.last g.messages}) snapshot.groups
  agents = personal <> groups
  groupMessages = foldl (\all g -> O.insert g.id (map (\msg -> {user:msg.author == "admin",author:if msg.author == "admin" then "" else msg.author,chatKey:msg.id,text:msg.text,created:msg.created,attachments:msg.attachments,stale:msg.stale,request:Nothing,delegated:false,warning:"",turnId:"",outputOrder:0.0}) g.messages) all) personalMessages groups
  delegated = foldl (\all work ->
    let turn = A.find ((work.callId == _) <<< _.id) turns
        quiet = work.historyOnly && fromMaybe (work.error == "") ((\t -> A.elem t.status [X.Queued,X.Running,X.OutputPending,X.Done,X.Warning]) <$> turn)
        problem = D.fallback work.error $ fromMaybe "" $ _.error <$> turn
        content = if work.historyOnly then "" else work.output
        recipientName = D.fallback "a specialist" $ fromMaybe "" $ _.name <$> X.findEntity work.owner agents
    in if work.callId == work.origin.call || quiet then all else
    let message = {user:false,author:work.owner,text:D.fallback (D.fallback ("Task assigned to " <> recipientName <> " · " <> X.workName work.state) problem) content
          ,created:fromMaybe "" $ _.created <$> turn,delegated:true,request:turn >>= \t -> if A.elem t.status [X.Done,X.Warning] then Nothing else Just t,chatKey:"delegation:" <> work.id,attachments:[],warning:"",turnId:"",stale:false,outputOrder:0.0}
    in append work.origin.agent [message] all) groupMessages snapshot.workloads
  messages = map (A.sortBy (\a b -> compare (B.parseTime a.created) (B.parseTime b.created) <> compare a.outputOrder b.outputOrder)) delegated
  target = X.findEntity m.selected agents
  id = case target of
    Nothing -> snapshot.main_agent_id
    Just a -> if a.retired && not (fromMaybe false $ _.retired <$> X.findEntity m.selected m.agents) then snapshot.main_agent_id else m.selected
  groupView = m.groupView >>= \g -> if A.any (\a -> a.id == g && not a.archived) agents then Just g else Nothing
  scope = if m.scope == N.Groups && not (A.any (\a -> isGroup a && not a.archived) agents) then N.All else m.scope

reset :: Json -> Model -> Model
reset raw old = case X.decodeSnapshot raw of
  Left _ -> old
  Right snapshot -> resetSnapshot snapshot old

resetSnapshot :: X.Snapshot -> Model -> Model
resetSnapshot snapshot old = projectSnapshot snapshot old
  {selected=p.selected,activity=p.activity,scope=p.scope,groupView=p.groupView,drafts=p.drafts
  ,attachments=snapshot.attachment_drafts,sending=Set.empty,uploading=0,clarification=Nothing,documents=O.empty,expanded=Set.empty,epoch=old.epoch+1
  ,panes=p.panes,workspaces=p.workspaces,workspaceRevision=p.workspaceRevision,search="",searchOpen=false
  ,preview=NotAsked,address="",scrollPosition=p.scrollPosition,restoringScroll=p.scrollPosition /= Nothing
  ,routineRequestDrafts=O.empty,routineChatReturn=O.empty,routineReceipts=O.empty,routineControls=O.empty,routineFilters=O.empty,routineHistories=O.empty,routineResultSelections=O.empty,routineScroll=O.empty,restoringRoutine=Nothing,documentFetched=O.empty}
  where
  p = snapshot.preferences

select :: String -> Model -> Model
select id m = if target.id == "" then m else m
  { selected=id, activity=N.Chat N.Conversation, search="", groupView=if group then Just id else Nothing
  , scope=if group then N.Groups else if m.groupView /= Nothing || m.scope == N.Groups then N.All else m.scope
  , menu=Nothing, dialog=Nothing, addressOpen=false, address="", panes=N.togglePane false (N.limit m.env) (hasTab m) N.ChatPane m.panes
  , scrollPosition=Nothing, restoringScroll=false, preview=NotAsked, restoringRoutine=Nothing
  }
  where
  target = fromMaybe X.emptyEntity $ X.findEntity id m.agents
  group = isGroup target

roster :: Model -> Array X.Entity
roster m = A.sortBy (\a b -> compare (pinned b) (pinned a) <> compare b.lastActivity a.lastActivity) $ A.filter allowed m.agents
  where
  group = maybeGroup m
  pinned a = a.id == (if group.id /= "" then group.lead else m.snapshot.main_agent_id)
  allowed a = not (a.retired || a.archived) && (if group.id /= "" then A.elem a.id group.members else m.scope == N.All || (m.scope == N.Groups) == isGroup a) && S.contains (S.Pattern $ S.toLower m.search) (S.toLower $ a.name <> " " <> a.role)

maybeGroup :: Model -> X.Entity
maybeGroup m = fromMaybe X.emptyEntity $ m.groupView >>= flip X.findEntity m.agents

notes :: Model -> X.Notes
notes m = if isGroup (selected m) then (selected m).notes else fromMaybe (X.notes D.empty) $ _.notes <$> O.lookup m.selected m.snapshot.orchestration

preferences :: Model -> Json
preferences m = D.merge (N.encodeActivity m.activity) $ D.json
  { selected: m.selected, scope: N.scopeId m.scope, groupView: m.groupView
  , panes: O.fromFoldable $ map (\p -> Tuple (N.paneId p) (A.elem p m.panes)) [N.Sidebar,N.ChatPane,N.Workspace]
  , drafts: m.drafts, attachment_drafts: map (map _.id) m.attachments
  , workspaces: m.workspaces, chatPosition: m.scrollPosition
  }

selectedOwner :: Model -> N.OwnerRef
selectedOwner m = if isGroup (selected m) then N.GroupOwner m.selected else N.PersonalOwner m.selected

normalizeRoutineSelection :: Model -> Model
normalizeRoutineSelection m = case m.activity of
  N.Work (N.RoutineView route) _ | (N.routeRoutine route).owner /= selectedOwner m ->
    m {activity=N.Chat N.Conversation,restoringRoutine=Nothing}
  _ -> m

routineFilter :: Model -> N.RoutineFilter
routineFilter m = fromMaybe fallback $ O.lookup m.selected m.routineFilters
  where fallback = if N.period m.activity == N.Paused then N.PausedRoutines else N.AllRoutines

routineKey :: N.RoutineRef -> String
routineKey ref = "routine:" <> ownerKey ref.owner <> ":" <> N.routineId ref.id
  where
  ownerKey = case _ of
    N.PersonalOwner id -> "sapi:" <> id
    N.GroupOwner id -> "group:" <> id

overviewKey :: N.RoutineRef -> String
overviewKey ref = routineKey ref <> ":overview"

historyKey :: N.RoutineRef -> Maybe N.RunId -> String
historyKey ref before = routineKey ref <> ":history:" <> fromMaybe "first" (N.runId <$> before)

runKey :: N.RunRef -> String
runKey ref = routineKey ref.routine <> ":run:" <> N.runId ref.id

routinePath :: N.RoutineRef -> String
routinePath ref = "/api/" <> (case ref.owner of
  N.PersonalOwner _ -> "agents/"
  N.GroupOwner _ -> "groups/") <> B.encodeURI (N.ownerId ref.owner) <> "/routines/" <> B.encodeURI (N.routineId ref.id) <> "/runs"

historyPositions :: Model -> N.RoutineRef -> Array N.HistoryPosition
historyPositions m ref = fromMaybe [N.firstHistoryPosition] $ O.lookup (routineKey ref) m.routineHistories

historyPage :: Model -> N.RoutineRef -> Maybe N.RunId -> Maybe RD.RoutineHistoryPage
historyPage m ref before = historyDocument m (historyKey ref before)

historyDocument :: Model -> String -> Maybe RD.RoutineHistoryPage
historyDocument m key = case O.lookup key m.documents >>= valueOf of
  Just (X.RoutineHistoryDocument page) -> Just page
  _ -> Nothing

routineTitle :: Model -> N.RoutineRef -> String
routineTitle m ref = fromMaybe "Routine" $ routineDisplayTitle <$> A.find (\r -> r.id == N.routineId ref.id && r.owner == N.ownerId ref.owner) m.snapshot.routines

-- The stored title is a prompt excerpt, not an authored name. Prefer the saved
-- objective; keep full paths in Assignment rather than in a navigation label.
routineDisplayTitle :: X.Routine -> String
routineDisplayTitle routine = if CP.length title > 80 then S.trim (CP.take 79 title) <> "…" else title
  where
  title = D.fallback "Routine" $ S.joinWith " " $ map compactPath $ S.split (S.Pattern " ") source
  source = D.fallback routine.title $ fromMaybe "" routine.objective
  compactPath word
    | S.startsWith (S.Pattern "/") word || S.startsWith (S.Pattern "\\") word || S.contains (S.Pattern ":\\") word =
        fromMaybe word $ A.last $ A.filter (_ /= "") $ S.split (S.Pattern "/") $ S.replaceAll (S.Pattern "\\") (S.Replacement "/") word
    | otherwise = word

routineViewKey :: Model -> Maybe String
routineViewKey m = case m.activity of
  N.Work N.Scheduled _ -> Just $ "routine-list:" <> m.selected
  N.Work (N.RoutineView route) _ -> Just $ case route of
    N.Overview ref -> overviewKey ref
    N.History ref position -> historyKey ref position.before <> (if position.half == N.FirstTen then ":0" else ":10")
    N.RunDetails ref _ -> runKey ref
  _ -> Nothing

currentDocuments :: Model -> Array DocumentRequest
currentDocuments m = case m.activity of
  N.Work N.Memo _ | info.revision /= "missing" -> [make X.MemoKind "memo" info.revision "/notes"]
  N.Work view _ | A.elem view [N.Tasks,N.Automated] && not (isGroup a) ->
    [make X.TasksKind "tasks" (D.encode $ D.json $ map _.revision $ A.filter (\w -> A.elem m.selected w.participants) m.snapshot.workloads) "/tasks?format=json"]
  N.Work (N.RoutineView route) _
    | (N.routeRoutine route).owner == selectedOwner m
      && A.elem N.ChatPane (visiblePanes m)
      && A.any (\r -> r.owner == m.selected && r.id == N.routineId (N.routeRoutine route).id) m.snapshot.routines -> routineDocuments route
  _ -> []
  where
  a = selected m
  info = notes m
  request kind key revision path refresh = {kind,refresh,request:{key,revision,path,epoch:m.epoch,sequence:m.sequence+1}}
  make kind prefix revision suffix = request kind (prefix <> ":" <> m.selected) revision (ownerPath m <> suffix) false
  exact ref revision refresh = request (X.RoutineRunKind ref) (runKey ref) revision (routinePath ref.routine <> "/" <> B.encodeURI (N.runId ref.id)) refresh
  routineDocuments = case _ of
    N.Overview ref ->
      [request (X.RoutineHistoryKind ref Nothing) (overviewKey ref) "overview" (routinePath ref) true] <>
      (case historyDocument m (overviewKey ref) of
        Just page | page.routine.deletedAt == Nothing -> case page.latest of
          Just latest -> [exact {routine:ref,id:latest.id} latest.revision false]
          Nothing -> []
        _ -> [])
    N.History ref position ->
      [request (X.RoutineHistoryKind ref position.before) (historyKey ref position.before) "history"
        (routinePath ref <> fromMaybe "" ((\id -> "?before=" <> B.encodeURI (N.runId id)) <$> position.before)) false]
    N.RunDetails ref _ -> [exact ref "run" true]

nativeWorkspace :: Model -> Boolean
nativeWorkspace m = m.env.native && (activeRecord m).location /= X.Remote

currentPreview :: Model -> Maybe Request
currentPreview m =
  let tab = activeTab m
  in if nativeWorkspace m || tab.path == "" || not (A.elem N.Workspace m.panes) then Nothing
     else Just {epoch:m.epoch,sequence:m.sequence+1,key:tab.id,revision:D.encode $ D.json tab.command,path:ownerPath m <> "/browser/" <> B.encodeURI (tab.id) <> "/content"}

requestOf :: forall error a. RemoteData error a -> Maybe Request
requestOf = case _ of
  NotAsked -> Nothing
  Loading r _ -> Just r
  Ready r _ -> Just r
  Failed r _ -> Just r

valueOf :: forall error a. RemoteData error a -> Maybe a
valueOf = case _ of
  Loading _ value -> value
  Ready _ value -> Just value
  _ -> Nothing

decodeDocument :: DocumentRequest -> Json -> Either String X.Document
decodeDocument r = X.decodeDocument r.kind

routineControlBusy :: Model -> N.RoutineRef -> Boolean
routineControlBusy m ref = case O.lookup (routineKey ref) m.routineControls of
  Just (Loading _ _) -> true
  _ -> false
