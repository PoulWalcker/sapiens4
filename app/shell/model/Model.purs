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
import Data.Tuple (Tuple(..))
import Foreign.Object as O
import Halogen (SubscriptionId)
import Libs4.Browser (Voice)
import Libs4.Browser as B
import Sapiens.Data as D
import Sapiens.Browser as Browser
import Sapiens.Domain as X
import Sapiens.Navigation as N

data RemoteData error a = NotAsked | Loading Request (Maybe a) | Ready Request a | Failed Request error

data PreviewError = PreviewUnavailable String | InvalidPreview

previewError :: PreviewError -> String
previewError = case _ of
  PreviewUnavailable message -> message
  InvalidPreview -> "Invalid file preview response"

data Preview = TextPreview String | MemoPreview (Array Json)

decodePreview :: Boolean -> Json -> Either PreviewError Preview
decodePreview memo value = case toString (D.get "content" value) of
  Just content -> Right $ if memo then MemoPreview (D.arr $ B.parseMarkup content) else TextPreview content
  Nothing -> Left InvalidPreview

type Request = { epoch :: Int, sequence :: Int, key :: String, revision :: String, path :: String }

data DialogKind = AppMenu | WorkspaceSettings | Models | AIProviders | Computer | AgentSettings | GroupSettings | Attach | Reference X.AttachmentKind | DecisionPrompts | GroupTask | CreateCorpora | EditCorpora | ConnectRemote | HostPairing | RelayList | RelayAdd | RelaySwitch | RelayTerms | Installation | Notice

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

-- | Where a recording is in its life. The recorder exists only while recording, and how it
-- | ends is decided once, when Stop or Send is pressed.
data RecordingPhase = Starting | Recording Voice | Stopping StopAction

-- | What happens when recording stops: keep the voice messages for review, or send them.
data StopAction = Review | SendAfter
derive instance eqStopAction :: Eq StopAction

isStopping :: RecordingPhase -> Boolean
isStopping = case _ of
  Stopping _ -> true
  _ -> false

phaseRecorder :: RecordingPhase -> Maybe Voice
phaseRecorder = case _ of
  Recording recorder -> Just recorder
  _ -> Nothing

-- | One voice recording in progress. Pieces wait in 'queue' and are uploaded one at a time,
-- | in order, to 'path' (captured when recording started, so switching chats cannot redirect them).
-- | Uploading runs alongside recording, so its state stays separate from 'phase'.
type VoiceSession =
  { owner :: String, path :: String, epoch :: Int, phase :: RecordingPhase, subscription :: SubscriptionId
  , queue :: Array X.VoicePiece, sending :: Boolean, attempts :: Int, pieces :: Int
  , recording :: String, startedAt :: Number, elapsed :: Number }

-- | One chat's voice messages being sent one by one, in order, each as soon as its own
-- | transcript is ready. Kept per chat ('outboxes'), so one chat never blocks another.
-- | 'epoch' ties it to the corpora it was started in: after a switch it is dropped, never sent elsewhere.
type Outbox = { epoch :: Int, path :: String, text :: String, others :: Array String, voices :: Array String, sending :: Boolean }

-- | The selected chat is recording or sending voice messages, so it must not send anything else yet.
voiceBusy :: Model -> Boolean
voiceBusy m = O.member m.selected m.outboxes || case m.voice of
  Just v -> v.owner == m.selected
  Nothing -> false

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
  , uploading :: Int
  , workspaces :: O.Object X.Workspace, workspaceRevision :: Number, documents :: O.Object (RemoteData String X.Document)
  , expanded :: Set.Set String, preview :: RemoteData PreviewError Preview
  , scrollPosition :: Maybe Browser.ScrollPosition, restoringScroll :: Boolean
  | r )

type SessionState =
  ( snapshotContent :: Maybe X.SnapshotContent, polling :: Boolean, pollGeneration :: Int, fallbackDelay :: Number, saving :: Boolean, lastPreview :: Number
  , nativeReporting :: Boolean, nativeReports :: Array Browser.NativeReport
  , epoch :: Int, sequence :: Int, now :: Number, env :: Environment
  , records :: Array X.Corpora, active :: String, connection :: ConnectionState
  , localAvailable :: Boolean, showArchived :: Boolean, installRequired :: Boolean
  , preferencesSaved :: String, voice :: Maybe VoiceSession, outboxes :: O.Object Outbox
  )

fastPoll :: Number
fastPoll = 750.0

initial :: Environment -> Model
initial env =
  { snapshot:X.emptySnapshot, agents:[], messages:O.empty, selected:"", activity:N.Chat N.Conversation, scope:N.All, groupView:Nothing
  , mention:Nothing, drafts:O.empty, attachments:O.empty, sending:Set.empty, uploading:0, voice:Nothing, outboxes:O.empty
  , workspaces:O.empty, workspaceRevision:0.0, documents:O.empty, expanded:Set.empty
  , snapshotContent:Nothing, polling:false,pollGeneration:0,fallbackDelay:fastPoll,saving:false,lastPreview:0.0,nativeReporting:false,nativeReports:[]
  , epoch:0, sequence:0, now:0.0, env, panes:[N.ChatPane,N.Sidebar,N.Workspace], search:"", searchOpen:false, menu:Nothing, dialog:Nothing
  , records:[], active:"", connection:Offline, localAvailable:false, showArchived:false
  , toast:"", feedback:"", theme:if env.dark then "dark" else "light", installRequired:false
  , dialogStack:[], preview:NotAsked, address:"", addressOpen:false, scrollPosition:Nothing, restoringScroll:false, preferencesSaved:""
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

-- A message carries all ordinary attachments and at most one voice clip.
ordinaryAttachmentCount :: Model -> Int
ordinaryAttachmentCount = A.length <<< A.filter (not <<< X.isVoice) <<< attachments

firstMessageFull :: Model -> Boolean
firstMessageFull m = ordinaryAttachmentCount m + (if A.any X.isVoice (attachments m) then 1 else 0) >= 8

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
projectSnapshot snapshot m = withWorkspaces snapshot.preferences m { snapshot=snapshot, agents=agents, messages=messages, selected=id, groupView=groupView, scope=scope, attachments=refreshed }
  where
  -- Voice chips waiting in the composer pick up their transcript as the host finishes it. A chip the
  -- host had before but no longer has was sent or removed on another device and leaves; a chip the host
  -- has not seen yet was just added here and stays; a new chip on the host was added elsewhere and appears.
  refreshed = O.mapWithKey syncDrafts (O.union m.attachments (map (const []) snapshot.attachment_drafts))
  syncDrafts owner items =
    let latest = fromMaybe [] (O.lookup owner snapshot.attachment_drafts)
        before = map _.id (fromMaybe [] (O.lookup owner m.snapshot.attachment_drafts))
        kept = A.mapMaybe (\item -> case A.find (\row -> row.id == item.id) latest of
          Just row -> Just row
          Nothing -> if A.elem item.id before then Nothing else Just item) items
        added = A.filter (\row -> not (A.any (\item -> item.id == row.id) items) && not (A.elem row.id before)) latest
    in kept <> added
  personal = map (\a -> a {busy=A.any (\t -> t.agent == a.id && t.status == X.Running) turns
    , lastActivity=B.parseTime $ D.fallback a.created $ fromMaybe "" $ _.created <$> A.last (A.filter (\t -> t.agent == a.id) turns)
    , preview=D.cut 150 $ D.replace "\\s+" " " $ D.fallback "Ready for your message." $ fromMaybe "" $ _.text <$> (O.lookup a.id personalMessages >>= A.last)}) snapshot.agents
  turns = snapshot.turns
  append owner rows = O.alter (Just <<< (_ <> rows) <<< fromMaybe []) owner
  personalMessages = foldl (\all t -> if t.group /= "" || not (A.elem t.flow ["chat","computer"]) then all else
    let input = {user:true,author:t.origin.caller,text:t.input,created:t.created,attachments:t.attachments,request:Just t,chatKey:"user:" <> t.id,delegated:false,warning:"",turnId:"",stale:false,outputOrder:0.0}
        reply = result t
        output = {user:false,author:"",text:reply.text,warning:reply.warning,created:D.fallback t.created t.output_at,outputOrder:t.output_order,turnId:t.id,chatKey:"assistant:" <> t.id,attachments:[],request:Nothing,delegated:false,stale:false}
    in append t.agent ([input] <> if A.elem t.status [X.Done,X.Warning] && t.hasOutput then [output] else []) all) O.empty turns
  groups = map (\g -> g {busy=A.any ((X.Running == _) <<< _.status) g.requests,lastActivity=B.parseTime g.updated,preview=D.fallback (D.fallback "Shared workspace" g.description) $ fromMaybe "" $ _.text <$> A.last g.messages}) snapshot.groups
  agents = personal <> groups
  groupMessages = foldl (\all g -> O.insert g.id (map (\msg -> {user:msg.author == "admin",author:if msg.author == "admin" then "" else msg.author,chatKey:msg.id,text:msg.text,created:msg.created,attachments:msg.attachments,stale:msg.stale,request:Nothing,delegated:false,warning:"",turnId:"",outputOrder:0.0}) g.messages) all) personalMessages groups
  delegated = foldl (\all work -> if work.callId == work.origin.call then all else
    let turn = A.find ((work.callId == _) <<< _.id) turns
        message = {user:false,author:work.owner,text:D.fallback (D.fallback ("Delegated to " <> D.fallback work.owner (fromMaybe "" $ _.name <$> X.findEntity work.owner agents) <> " · " <> X.workName work.state) work.error) work.output
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
  ,snapshotContent=Nothing,attachments=snapshot.attachment_drafts,sending=Set.empty,uploading=0,outboxes=O.empty,documents=O.empty,expanded=Set.empty,epoch=old.epoch+1
  ,panes=p.panes,workspaces=p.workspaces,workspaceRevision=p.workspaceRevision,search="",searchOpen=false
  ,preview=NotAsked,address="",scrollPosition=p.scrollPosition,restoringScroll=p.scrollPosition /= Nothing}
  where
  p = snapshot.preferences

select :: String -> Model -> Model
select id m = if target.id == "" then m else m
  { selected=id, activity=N.Chat N.Conversation, search="", groupView=if group then Just id else Nothing
  , scope=if group then N.Groups else if m.groupView /= Nothing || m.scope == N.Groups then N.All else m.scope
  , menu=Nothing, dialog=Nothing, addressOpen=false, address="", panes=N.togglePane false (N.limit m.env) (hasTab m) N.ChatPane m.panes
  , scrollPosition=Nothing, restoringScroll=false, preview=NotAsked
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

notes :: Model -> String -> X.Notes
notes m owner = if isGroup entity then entity.notes else fromMaybe (X.notes D.empty) $ _.notes <$> O.lookup owner m.snapshot.orchestration
  where entity = agent m owner

preferences :: Model -> Json
preferences m = D.merge (N.encodeActivity m.activity) $ D.json
  { selected: m.selected, scope: N.scopeId m.scope, groupView: m.groupView
  , panes: O.fromFoldable $ map (\p -> Tuple (N.paneId p) (A.elem p m.panes)) [N.Sidebar,N.ChatPane,N.Workspace]
  , drafts: m.drafts, attachment_drafts: map (map _.id) m.attachments
  , workspaces: m.workspaces, chatPosition: m.scrollPosition
  }

currentDocument :: Model -> Maybe Request
currentDocument m = case m.activity of
  N.Work view _ | A.elem view [N.Tasks,N.Automated] && not (isGroup a) -> Just $ make "tasks" (D.encode $ D.json $ map _.revision $ A.filter (\w -> A.elem m.selected w.participants) m.snapshot.workloads) "/tasks?format=json"
  _ -> Nothing
  where
  a = selected m
  make kind revision suffix = {key:kind <> ":" <> m.selected,revision,path:ownerPath m <> suffix,epoch:m.epoch,sequence:m.sequence+1}

nativeWorkspace :: Model -> Boolean
nativeWorkspace m = m.env.native && (activeRecord m).location /= X.Remote

isMemoTab :: X.Notes -> X.Tab -> Boolean
isMemoTab memo tab = tab.path /= "" && tab.path == memo.path

currentPreview :: Model -> Maybe Request
currentPreview m =
  let tab = activeTab m
      memo = notes m m.selected
      isMemo = isMemoTab memo tab
  in if (nativeWorkspace m && not isMemo) || tab.path == "" || not (A.elem N.Workspace m.panes) then Nothing
     else Just {epoch:m.epoch,sequence:m.sequence+1,key:tab.id,revision:D.encode (D.json tab.command) <> (if isMemo then memo.revision else ""),path:ownerPath m <> "/browser/" <> B.encodeURI (tab.id) <> "/content"}

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

-- | Chips this device saved but the host refused (already sent from another device) leave the composer.
dropRejected :: Json -> Json -> O.Object (Array X.Attachment) -> O.Object (Array X.Attachment)
dropRejected sent kept = O.mapWithKey (\owner items -> A.filter (\item -> not (A.elem item.id (ids owner sent)) || A.elem item.id (ids owner kept)) items)
  where
  ids owner value = map D.str (D.arr (D.get owner value))
