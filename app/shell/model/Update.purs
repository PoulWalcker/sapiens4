module Sapiens.Update where

import Prelude
import Libs4.Browser as B
import Data.Either (Either(..), either)
import Data.Array as A
import Data.Foldable (foldl)
import Data.Argonaut.Core (Json)
import Data.Set as Set
import Data.String as S
import Data.String.CodeUnits as CU
import Sapiens.Browser as Browser
import Sapiens.Action as Shell
import Sapiens.Form as F
import Foreign.Object as O
import Data.Maybe (Maybe(..), fromMaybe)
import Sapiens.Data as D
import Sapiens.Domain as X
import Sapiens.Model as M
import Sapiens.Navigation as N
import Sapiens.RoutineData as RD
import Sapiens.RoutineRequest as RR

-- Pure application decisions. Commands are interpreted by Sapiens.Effects.
data Action
  = RegistryReceived Int String {records :: Array X.Corpora, localAvailable :: Boolean}
  | Entered X.Corpora (Maybe X.Snapshot)
  | Lifecycle X.LifecycleIntent
  | LifecycleReceived Int (Either String X.Corpora)
  | Global Browser.GlobalEvent
  | QueueNative Browser.NativeReport
  | DrainNative
  | NativeCompleted Int Browser.NativeReport (Either String Json)
  | Poll
  | SnapshotReceived Int String Int (Either String {snapshot :: X.Snapshot, now :: Number})
  | Persist
  | Persisted Int String (Either String Json)
  | MentionAt Int Int
  | ChooseMention Int
  | MoveMention Int
  | DismissMention
  | SetClarification X.Workload
  | PrepareDraft String String
  | SetMenu String
  | CloseMenu
  | SetTheme String
  | Address String
  | EditAddress
  | BrowserReceived Int String String (Either String Json)
  | RequestDocument
  | ReceiveDocument M.Request (Either String X.Document)
  | RetryDocument String
  | Draft String
  | Send
  | MessageReceived Submission (Either String RR.SendReceipt)
  | RemoveAttachment String
  | ToggleExpanded String
  | Select String
  | SetScope N.Scope
  | Navigate N.Activity
  | SetPeriod N.Period
  | OpenRoutine N.RoutineRoute
  | OpenRoutineHistory N.RoutineRef
  | RoutineBack
  | RoutineHistoryPage Boolean
  | SubmitRoutineRequest
  | BackFromRoutineChat
  | ControlRoutine RD.RoutineControl
  | RoutineControlReceived M.Request RD.RoutineControl (Either String RD.ControlReceipt)
  | RefreshRoutine
  | SelectRoutineResult N.RunRef (Maybe RD.ExactResultRef)
  | SetRoutineFilter N.RoutineFilter
  | Search String
  | ToggleSearch
  | TogglePane N.Pane
  | RequestPreview
  | ReceivePreview M.Request (Either M.PreviewError String)

data Command
  = SaveRegistry (Array X.Corpora)
  | SaveActive String
  | ConnectRecord X.Corpora
  | ChangeLifecycle X.LifecycleIntent
  | ReplaceHistory String
  | Dispatch Shell.Action
  | ReadVisibility
  | Suspend
  | Resume
  | SendNative Browser.NativeReport
  | FetchSnapshot Int
  | SchedulePoll Int String Int Number
  | SaveLocal
  | StorePreferences String Json
  | RetryPreferences
  | RestoreScroll
  | Focus String
  | SelectRange String Int Int
  | Sync
  | PersistTheme String
  | SubmitRoutineControl M.Request RD.RoutineControl
  | FetchDocument M.DocumentRequest
  | RestoreRoutineScroll
  | FocusRoutine
  | LoadMemoImages M.Request X.MemoDocument
  | SendMessage Submission
  | Notify String
  | PollNow
  | ResizeComposer
  | UpdateMention
  | SaveView
  | SavePreferences
  | LoadDocument
  | ScrollToStart
  | ScrollToBottom
  | FocusSearch
  | OpenWorkspace
  | LoadPreview
  | FetchPreview M.Request

type Submission =
  { epoch :: Int, owner :: String, text :: String, attachments :: Array String
  , workload :: Maybe String, path :: String, destination :: N.OwnerRef
  , routine :: Maybe {identity :: Int, origin :: N.Activity, intent :: RR.Intent} }

submission :: M.Model -> Maybe Submission
submission m =
  let text = S.trim $ M.draft m
      items = M.attachments m
      owner = M.selected m
  in if (text == "" && A.null items) || Set.member m.selected m.sending || m.uploading > 0 || not (M.online m) || (M.busy m) || owner.retired || owner.archived || A.elem (m.snapshot.corpora.status) [X.Paused,X.Archived] then Nothing
     else Just {epoch:m.epoch,owner:m.selected,text,attachments:map (_.id) items,destination:M.selectedOwner m,routine:Nothing,path:M.ownerPath m <> "/messages",workload:case m.clarification of
       Just work | work.owner == m.selected -> Just $ work.id
       _ -> Nothing}

data Transition = Unchanged | Changed M.Model (Array Command) | Commands (Array Command)

update :: Action -> M.Model -> Transition
update action m = case action of
  RegistryReceived epoch saved registry
    | epoch /= m.epoch -> Unchanged
    | otherwise ->
      let previous = A.find ((_ == m.active) <<< _.key) registry.records
          preferred = case A.find ((_ == saved) <<< _.key) registry.records of
            Just record -> Just record
            Nothing -> A.head registry.records
          selected = case previous of
            Just record -> Just record
            Nothing -> preferred
          updated = m {records=registry.records,localAvailable=registry.localAvailable}
          next = case selected of
            Nothing -> updated {active="",connection=M.Offline,feedback="",snapshot=X.emptySnapshot,agents=[]}
            Just record -> updated {polling=if record.key == m.active then true else m.polling}
          localPair = not (M.remote m) && S.contains (S.Pattern "pairing=local") m.env.search
          local = A.find ((_ == X.Local) <<< _.location) registry.records
          connect = case selected of
            Nothing -> []
            Just record -> [if record.key == m.active then ConnectRecord record else Dispatch $ Shell.EnterCorpora record.key]
          pairing = if localPair then [ReplaceHistory m.env.path] <> (case local of
            Nothing -> []
            Just record -> [Dispatch $ Shell.OpenDialog M.HostPairing $ F.CorporaRecord record false]) else []
      in Changed (if localPair then next {env=m.env {search=""}} else next) ([SaveRegistry registry.records] <> connect <> pairing <> [Sync])
  Entered record cached ->
    let current = m {active=record.key,connection=M.Offline,polling=true,saving=false,nativeReporting=false,nativeReports=[],epoch=m.epoch+1,dialog=Nothing,scrollPosition=Nothing
          ,routineRequestDrafts=O.empty,routineChatReturn=O.empty,routineReceipts=O.empty,routineControls=O.empty,routineFilters=O.empty,routineHistories=O.empty,routineResultSelections=O.empty,routineScroll=O.empty,restoringRoutine=Nothing,documentFetched=O.empty}
        updated = case cached of
          Just snapshot -> M.resetSnapshot snapshot current
          Nothing -> current {snapshot=X.emptySnapshot,agents=[],documents=O.empty}
    in Changed updated [SaveActive record.key,ConnectRecord record,RestoreScroll,Sync]
  Lifecycle command
    | M.busy m || not (M.online m) -> Unchanged
    | otherwise -> Changed (m {connection=M.ChangingLifecycle true}) [ChangeLifecycle command,Sync]
  LifecycleReceived epoch result
    | epoch /= m.epoch -> Unchanged
    | otherwise -> case result of
      Left failure -> Changed (m {connection=M.Offline,menu=Nothing}) [Dispatch $ Shell.OpenDialog M.Notice $ F.NoticeRecord failure,PollNow,Sync]
      Right info -> Changed (m {connection=M.Online,menu=Nothing,records=map (\r -> if r.key == m.active then X.updateCorpora r info else r) m.records,snapshot=m.snapshot {corpora=info}}) [PollNow,Sync]
  Global event -> case event of
    Browser.ReportError message -> Changed (m {feedback=message}) []
    Browser.Resize -> Commands [Dispatch Shell.Resize]
    Browser.Online -> Commands [Dispatch Shell.LoadRegistry]
    Browser.Resume -> Commands [Resume]
    Browser.Visibility -> Commands [ReadVisibility]
    Browser.Suspend -> Commands [Suspend]
    Browser.Back -> Commands [Dispatch Shell.CloseDialog]
    Browser.Storage key -> Commands $ (if key == "sapi-color-theme" then [Dispatch Shell.RefreshTheme] else []) <> (if key == "s4.relay-reset" then [Dispatch $ Shell.DialogCommand Shell.ResetRelay] else [])
    Browser.KeyDown key
      | key.key == "Escape" -> Commands $
          if m.dialog /= Nothing then [Dispatch Shell.CloseDialog] else if m.menu /= Nothing then [Dispatch Shell.CloseMenu] else if m.searchOpen then [Dispatch Shell.ToggleSearch,Focus "#search-toggle"] else []
      | key.key == "/" && not (Browser.editing key) && m.dialog == Nothing ->
          Changed (m {panes=N.togglePane false (N.limit m.env) (M.hasTab m) N.Sidebar m.panes}) ((if m.searchOpen then [] else [Dispatch Shell.ToggleSearch]) <> [Sync])
      | S.toLower key.key == "l" && (key.ctrl || key.meta) && m.dialog == Nothing && A.elem N.Workspace m.panes -> Commands [Dispatch Shell.EditAddress]
      | otherwise -> Unchanged
    Browser.Click ancestors ->
      let regions = case m.menu of
            Just "corpora" -> ["corpora-menu","corpora-picker"]
            Just "status" -> ["connection-status","corpora-status-menu"]
            Just key | A.elem key ["browser-options","browser-tabs"] -> ["browser-menu","workspace-menu","browser-overflow"]
            _ -> ["chat-panel"]
          outside = m.menu /= Nothing && not (A.any (flip A.elem ancestors) regions)
      in Changed (m {mention=if A.any (flip A.elem ancestors) ["message-input","mention-suggestions"] then m.mention else Nothing,menu=if outside then Nothing else m.menu}) (if outside then [Sync] else [])
    Browser.Ignore -> Unchanged
  QueueNative report
    | not (M.nativeWorkspace m) || M.busy m || not (A.any ((_ == report.owner) <<< _.id) m.agents) -> Unchanged
    | otherwise -> Changed (m {nativeReports=A.snoc m.nativeReports report}) [Dispatch Shell.DrainNative]
  DrainNative -> case A.uncons m.nativeReports of
    Just {head,tail} | not m.nativeReporting -> Changed (m {nativeReporting=true,nativeReports=tail}) [SendNative head]
    _ -> Unchanged
  NativeCompleted epoch report result
    | epoch /= m.epoch -> Unchanged
    | otherwise -> finishNative (m {nativeReporting=false}) $ update (BrowserReceived epoch report.owner report.op result) (m {nativeReporting=false})
  Poll
    | m.polling || (M.busy m) || m.active == "" || m.installRequired || m.env.hidden -> Unchanged
    | otherwise -> Changed (m {polling=true,pollGeneration=m.pollGeneration+1}) [FetchSnapshot $ m.pollGeneration+1]
  SnapshotReceived epoch active generation result
    | epoch /= m.epoch || active /= m.active || generation /= m.pollGeneration -> Unchanged
    | otherwise -> case result of
        Left failure ->
          let updated = m {polling=false,connection=M.connectionResult false m.connection,feedback=if D.matches "revoked|another window|^Invalid snapshot response:" failure then failure else m.feedback}
          in Changed updated (schedulePoll updated)
        Right {snapshot,now} ->
          let same = m.snapshot.corpora.id == snapshot.corpora.id
              projected = if same then M.projectSnapshot snapshot m else M.resetSnapshot snapshot m
              tab = M.activeTab projected
              previewDue = now - m.lastPreview >= 2000.0
              updated = projected {polling=false,now=now,connection=M.connectionResult true m.connection,feedback="",records=map (\r -> if r.key == active then X.updateCorpora r snapshot.corpora else r) m.records
                ,address=if m.addressOpen then m.address else D.fallback (if tab.url == "about:blank" then "" else tab.url) $ tab.path
                ,lastPreview=if previewDue then now else m.lastPreview}
          in Changed updated ([RestoreScroll,LoadDocument] <> (if previewDue then [LoadPreview] else []) <> [Sync] <> schedulePoll updated)
  Persist ->
    let value = D.encode $ M.preferences m
    in if (M.online m) && not (M.busy m) && not m.saving && value /= m.preferencesSaved then
      Changed (m {saving=true}) [SaveLocal,StorePreferences value $ D.json {attachment_drafts: map (map _.id) m.attachments}]
      else Commands [SaveLocal]
  Persisted epoch value result
    | epoch /= m.epoch -> Unchanged
    | otherwise ->
      let updated = case result of
            Left _ -> m {saving=false}
            Right response -> (M.withWorkspaces (X.preferences false $ D.get "preferences" response) m) {saving=false,preferencesSaved=value}
          commands = case result of
            Left failure -> [Notify $ "Settings are not saved yet: " <> failure]
            Right _ -> []
      in Changed updated (commands <> if D.encode (M.preferences updated) /= updated.preferencesSaved then [RetryPreferences] else [])
  MentionAt start end ->
    let before = CU.take start $ M.draft m
        menu = case D.match "(?:^|\\s)@([A-Za-z0-9_.:#+|()&$^\\-]*)$" before of
          Just hit | start == end ->
            let query = fromMaybe "" $ A.index hit.groups 1
                choices = A.take 12 $ A.filter (\a -> not (M.isGroup a || a.retired) && (not (M.isGroup $ M.selected m) || A.elem a.id (M.selected m).members) && S.take (S.length query) (S.toLower a.name) == S.toLower query) m.agents
            in Just {start:start-CU.length query-1,end,choices,index:0}
          _ -> Nothing
    in Changed (m {mention=menu}) []
  ChooseMention index -> case m.mention of
    Just menu -> case A.index menu.choices index of
      Just entity ->
        let value = CU.take menu.start (M.draft m) <> "@" <> entity.name <> " " <> CU.drop menu.end (M.draft m)
            cursor = menu.start + CU.length entity.name + 2
        in Changed (m {mention=Nothing,drafts=O.insert m.selected value m.drafts}) [Focus "#message-input",SelectRange "#message-input" cursor cursor,SavePreferences]
      Nothing -> Unchanged
    Nothing -> Unchanged
  MoveMention delta -> case m.mention of
    Just menu | not (A.null menu.choices) -> Changed (m {mention=Just $ menu {index=mod (menu.index+delta+A.length menu.choices) (A.length menu.choices)}}) []
    _ -> Unchanged
  DismissMention -> Changed (m {mention=Nothing}) []
  SetClarification work -> Changed (m {clarification=Just work}) [Focus "#message-input"]
  PrepareDraft preset placeholder ->
    let start = CU.length (M.draft m) + if M.draft m == "" then 0 else 2
        value = M.draft m <> (if M.draft m == "" then "" else "\n\n") <> preset
        cursor = if placeholder == "" then CU.length value else start + fromMaybe (CU.length preset) (CU.indexOf (S.Pattern placeholder) preset)
    in Changed (m {activity=N.Chat N.Conversation,menu=Nothing,drafts=O.insert m.selected value m.drafts,mention=Nothing})
      [ScrollToStart,LoadDocument,SaveView,ResizeComposer,SavePreferences,Focus "#message-input",SelectRange "#message-input" cursor (cursor+CU.length placeholder)]
  SetMenu key -> Changed (m {menu=if m.menu == Just key then Nothing else Just key,mention=Nothing}) [Sync]
  CloseMenu -> Changed (m {menu=Nothing}) [Sync]
  SetTheme theme -> Changed (m {theme=theme}) [PersistTheme theme,Sync]
  Address text -> Changed (m {address=text,addressOpen=true}) []
  EditAddress -> Changed (m {addressOpen=true,menu=Nothing}) [Sync,Focus "#browser-address",SelectRange "#browser-address" 0 (CU.length m.address)]
  BrowserReceived epoch owner op result
    | epoch /= m.epoch -> Unchanged
    | otherwise -> case result of
        Left failure -> Commands [Notify failure]
        Right snapshot ->
          let current = M.withWorkspaces (X.preferences false $ D.get "preferences" snapshot) m
              tab = M.activeTab current
              opened = op == "open"
              navigated = A.elem op ["open","focus","close","back","forward","reload"]
              updated = if owner /= m.selected then current else current
                {menu=if navigated then Nothing else current.menu
                ,addressOpen=if navigated then opened && tab.url == "about:blank" else current.addressOpen
                ,address=if not navigated && current.addressOpen then current.address else D.fallback (if tab.url == "about:blank" then "" else tab.url) $ tab.path
                ,panes=if op == "close" && not (tab.id /= "") then A.filter (_ /= N.Workspace) current.panes else if navigated then N.togglePane false (N.limit current.env) (tab.id /= "") N.Workspace current.panes else current.panes
                ,preview=if opened || op == "focus" then M.NotAsked else current.preview}
          in Changed updated ((if owner == m.selected && opened && tab.url == "about:blank" then [Focus "#browser-address"] else []) <> [Sync,LoadPreview,SavePreferences])
  RequestDocument ->
    let plan = foldl requestDocument {model:m,commands:[]} (M.currentDocuments m)
    in if A.null plan.commands then Unchanged else Changed plan.model plan.commands
  ReceiveDocument request result
    | request.epoch /= m.epoch -> Unchanged
    | otherwise -> case O.lookup request.key m.documents >>= M.requestOf of
        Just pending | pending == request -> Changed
          (m {documents=O.insert request.key (either (M.Failed request) (M.Ready request) result) m.documents
             ,documentFetched=O.insert request.key m.now m.documentFetched})
          (case result of
            Right (X.MemoDocument document) -> [LoadMemoImages request document]
            Right (X.RoutineHistoryDocument _) -> [LoadDocument,RestoreRoutineScroll]
            Right (X.RoutineRunDocument _) -> [LoadDocument,RestoreRoutineScroll]
            _ -> [RestoreRoutineScroll])
        _ -> Unchanged
  RetryDocument key -> Changed (m {documents=O.delete key m.documents,restoringRoutine=M.routineViewKey m}) [LoadDocument]
  Draft value -> Changed (m {drafts=O.insert m.selected value m.drafts}) [ResizeComposer,UpdateMention,SavePreferences]
  BackFromRoutineChat -> case O.lookup m.selected m.routineChatReturn of
    Nothing -> Unchanged
    Just activity ->
      let updated = m {activity=activity}
      in Changed (updated {restoringRoutine=M.routineViewKey updated}) [LoadDocument,SaveView,RestoreRoutineScroll]
  SubmitRoutineRequest -> case m.dialog of
    Just d | not (M.dialogBusy d) && M.online m && not (M.busy m) -> case d.record of
      F.RoutineRequestRecord target
        | not (Set.member (N.ownerId $ RR.owner target.intent) m.sending) ->
          case RR.message m.snapshot.routine_request_templates target.intent d.fields.message (d.fields.accepted == "true") of
            Left failure -> Changed (m {dialog=Just $ d {status=M.Rejected failure}}) []
            Right text ->
              let destination = RR.owner target.intent
                  owner = N.ownerId destination
                  path = "/api/" <> (case destination of
                    N.PersonalOwner _ -> "agents/"
                    N.GroupOwner _ -> "groups/") <> B.encodeURI owner <> "/messages"
                  request = {epoch:m.epoch,owner,text,attachments:[],workload:Nothing,path,destination,routine:Just {identity:d.identity,origin:target.origin,intent:target.intent}}
              in Changed (m {sending=Set.insert owner m.sending,routineRequestDrafts=O.insert owner d m.routineRequestDrafts,dialog=Just $ d {status=M.Submitting "Sending request…"}}) [SendMessage request]
      _ -> Unchanged
    _ -> Unchanged
  Send -> case submission m of
    Nothing -> Unchanged
    Just request -> Changed (m {sending=Set.insert m.selected m.sending,mention=Nothing}) [SendMessage request]
  MessageReceived request result
    | request.epoch /= m.epoch -> Unchanged
    | otherwise ->
        let current = m {sending=Set.delete request.owner m.sending}
        in case request.routine of
          Just target -> finishRoutineRequest request target result current
          Nothing -> case result of
           Left failure -> Changed current [Notify failure]
           Right _ -> Changed
             (current {drafts=if S.trim (fromMaybe "" $ O.lookup request.owner m.drafts) == request.text then O.insert request.owner "" m.drafts else m.drafts
               , attachments=O.insert request.owner (A.filter (\item -> not $ A.elem (item.id) request.attachments) $ fromMaybe [] $ O.lookup request.owner m.attachments) m.attachments
               , clarification=Nothing})
             [SavePreferences,ScrollToBottom,PollNow]
  RemoveAttachment id -> Changed (m {attachments=O.alter (Just <<< A.filter ((_ /= id) <<< _.id) <<< fromMaybe []) m.selected m.attachments}) [SavePreferences]
  ToggleExpanded key -> Changed (m {expanded=(if Set.member key m.expanded then Set.delete else Set.insert) key m.expanded}) []
  Select id ->
    let kept = case m.dialog of
          Just d | d.kind == M.RoutineRequest -> m {routineRequestDrafts=O.insert m.selected d m.routineRequestDrafts}
          _ -> m
    in Changed (M.select id kept) [ScrollToBottom,LoadPreview,SaveView]
  SetScope scope -> Changed
    (m {scope=scope,groupView=Nothing,panes=if N.limit m.env < 3 then N.togglePane false (N.limit m.env) (M.hasTab m) N.Sidebar m.panes else m.panes})
    [SaveView]
  Navigate activity -> Changed (m {activity=activity,menu=Nothing,restoringRoutine=Nothing}) [ScrollToStart,LoadDocument,SaveView]
  OpenRoutine route
    | (N.routeRoutine route).owner /= M.selectedOwner m -> Unchanged
    | otherwise ->
        let documents = case route of
              N.RunDetails ref _ -> O.delete (M.runKey ref) m.documents
              _ -> m.documents
            updated = m {activity=N.Work (N.RoutineView route) N.Planned,menu=Nothing,restoringRoutine=Nothing,documents=documents
              ,routineFilters=O.insert m.selected (M.routineFilter m) m.routineFilters}
        in Changed updated [ScrollToStart,LoadDocument,SaveView,FocusRoutine]
  OpenRoutineHistory ref
    | ref.owner /= M.selectedOwner m -> Unchanged
    | otherwise -> update (OpenRoutine $ N.History ref N.firstHistoryPosition) m
  RoutineBack -> case m.activity of
    N.Work (N.RoutineView route) _ ->
      let target = case route of
            N.Overview _ -> N.Work N.Scheduled N.Planned
            N.History ref _ -> N.Work (N.RoutineView $ N.Overview ref) N.Planned
            N.RunDetails ref N.BackToOverview -> N.Work (N.RoutineView $ N.Overview ref.routine) N.Planned
            N.RunDetails ref (N.BackToHistory position) -> N.Work (N.RoutineView $ N.History ref.routine position) N.Planned
          updated = m {activity=target,menu=Nothing}
      in Changed (updated {restoringRoutine=M.routineViewKey updated}) [LoadDocument,SaveView,RestoreRoutineScroll]
    _ -> Unchanged
  RoutineHistoryPage older -> historyPageTransition older m
  ControlRoutine control
    | M.routineControlBusy m (RD.controlRoutine control) || not (M.online m) -> Unchanged
    | otherwise ->
      let ref = RD.controlRoutine control
          path = "/api/routines/" <> N.routineId ref.id <> (case control of
            RD.StopRun run -> "/runs/" <> N.runId run.id <> "/stop"
            _ -> "/" <> RD.controlAction control)
          request = {epoch:m.epoch,sequence:m.sequence+1,key:M.routineKey ref,revision:RD.controlAction control,path}
          submitting d = case d.record of
            F.RoutineControlRecord target | target.control == control -> d {status=M.Submitting ""}
            _ -> d
      in Changed (m {sequence=request.sequence,menu=Nothing,routineControls=O.insert request.key (M.Loading request Nothing) m.routineControls,dialog=map submitting m.dialog}) [SubmitRoutineControl request control]
  RoutineControlReceived request control result
    | request.epoch /= m.epoch -> Unchanged
    | otherwise -> case O.lookup request.key m.routineControls of
        Just (M.Loading pending _) | pending == request -> finishRoutineControl request control result m
        _ -> Unchanged
  RefreshRoutine -> refreshRoutine m
  SelectRoutineResult ref selected -> case m.activity of
    N.Work (N.RoutineView (N.RunDetails current _)) _ | current == ref ->
      Changed (m {routineResultSelections=case selected of
        Nothing -> O.delete (M.runKey ref) m.routineResultSelections
        Just result -> O.insert (M.runKey ref) result m.routineResultSelections}) []
    _ -> Unchanged
  SetRoutineFilter filter -> Changed
    (m {routineFilters=O.insert m.selected filter m.routineFilters,restoringRoutine=Nothing,menu=Nothing}) [ScrollToStart,SaveView]
  SetPeriod period -> Changed
    (m {menu=Nothing,activity=case m.activity of
      N.Work view _ -> N.Work view period
      other -> other})
    [SavePreferences]
  Search search -> Changed (m {search=search}) []
  ToggleSearch -> Changed
    (m {searchOpen=not m.searchOpen,search=if m.searchOpen then "" else m.search})
    (if m.searchOpen then [] else [FocusSearch])
  TogglePane pane
    | pane == N.Workspace && not (M.hasTab m) -> Commands [OpenWorkspace]
    | otherwise -> Changed (m {panes=N.togglePane true (N.limit m.env) (M.hasTab m) pane m.panes}) [SaveView]
  RequestPreview -> case m.preview of
    M.Loading _ _ -> Unchanged
    _ -> case M.currentPreview m of
      Nothing -> Unchanged
      Just request ->
        let previous = if (_.path <$> M.requestOf m.preview) == Just request.path then M.valueOf m.preview else Nothing
        in Changed (m {sequence=request.sequence,preview=M.Loading request previous}) [FetchPreview request]
  ReceivePreview requested result -> case m.preview of
    M.Loading pending _
      | requested.epoch == m.epoch && requested == pending ->
          Changed (m {preview=either (M.Failed requested) (M.Ready requested) result}) []
    _ -> Unchanged

-- Routine history has a fixed membership until explicit refresh. Live documents
-- use the existing poll, with one request in flight and no automatic failure retry.
requestDocument :: {model :: M.Model, commands :: Array Command} -> M.DocumentRequest -> {model :: M.Model, commands :: Array Command}
requestDocument plan document =
  let m = plan.model
      expected = document.request
      old = fromMaybe M.NotAsked $ O.lookup expected.key m.documents
      routine = case document.kind of
        X.RoutineHistoryKind _ _ -> true
        X.RoutineRunKind _ -> true
        _ -> false
      changed = (_.revision <$> M.requestOf old) /= Just expected.revision
      due = document.refresh && not m.env.hidden && m.now - fromMaybe 0.0 (O.lookup expected.key m.documentFetched) >= 5000.0
      fetch = case old of
        M.NotAsked -> true
        M.Loading _ _ -> not routine && changed
        M.Failed _ _ -> not routine && changed
        M.Ready _ _ -> changed || due
      request = expected {sequence=m.sequence+1}
  in if not fetch then plan else
    {model:m {sequence=request.sequence,documents=O.insert request.key (M.Loading request $ M.valueOf old) m.documents}
    ,commands:A.snoc plan.commands $ FetchDocument $ document {request=request}}

historyPageTransition :: Boolean -> M.Model -> Transition
historyPageTransition older m = case m.activity of
  N.Work (N.RoutineView (N.History ref position)) _ ->
    let positions = M.historyPositions m ref
        index = fromMaybe 0 $ A.elemIndex position positions
        next = if not older then A.index positions (index-1) else do
          page <- M.historyPage m ref position.before
          if position.half == N.FirstTen && A.length page.runs > 10
            then Just $ position {half=N.SecondTen}
            else (\before -> {before:Just before,half:N.FirstTen}) <$> page.next
    in case next of
      Nothing -> Unchanged
      Just target ->
        let saved = if A.elem target positions then positions else A.snoc positions target
            updated = m {routineHistories=O.insert (M.routineKey ref) saved m.routineHistories}
        in update (OpenRoutine $ N.History ref target) updated
  _ -> Unchanged

refreshRoutine :: M.Model -> Transition
refreshRoutine m = case m.activity of
  N.Work (N.RoutineView route) _ ->
    let ref = N.routeRoutine route
        prefix = M.routineKey ref <> ":history:"
        historyKeys = A.filter (S.contains (S.Pattern prefix)) $ O.keys m.documents
        keys = case route of
          N.History _ _ -> historyKeys
          N.Overview _ -> [M.overviewKey ref] <> (case M.historyDocument m (M.overviewKey ref) >>= _.latest of
            Just latest -> [M.runKey {routine:ref,id:latest.id}]
            Nothing -> [])
          N.RunDetails run _ -> [M.runKey run]
        fresh = case route of
          N.History _ _ -> N.History ref N.firstHistoryPosition
          _ -> route
        updated = m {documents=foldl (flip O.delete) m.documents keys
          ,documentFetched=foldl (flip O.delete) m.documentFetched keys
          ,routineHistories=case route of
            N.History _ _ -> O.delete (M.routineKey ref) m.routineHistories
            _ -> m.routineHistories
          ,routineScroll=case route of
            N.History _ _ -> foldl (flip O.delete) m.routineScroll $ A.filter (S.contains (S.Pattern prefix)) $ O.keys m.routineScroll
            _ -> m.routineScroll
          ,activity=N.Work (N.RoutineView fresh) N.Planned,restoringRoutine=Nothing}
    in Changed updated ([LoadDocument] <> case route of
      N.History _ _ -> [ScrollToStart,FocusRoutine]
      _ -> [])
  _ -> Commands [PollNow]

type KeyPlan = {prevent :: Boolean, stop :: Boolean, actions :: Array Action}

messageKey :: Browser.Key -> M.Model -> KeyPlan
messageKey event m
  | event.composing = {prevent:false,stop:false,actions:[]}
  | otherwise = case m.mention of
      Just menu | A.elem event.key ["ArrowDown","ArrowUp","Enter","Tab","Escape"] ->
        {prevent:true,stop:true,actions:if event.key == "Escape" then [DismissMention] else if A.null menu.choices then [] else
          [if event.key == "Enter" || event.key == "Tab" then ChooseMention menu.index else MoveMention (if event.key == "ArrowDown" then 1 else -1)]}
      _ -> let send = event.key == "Enter" && not event.shift in {prevent:send,stop:false,actions:if send then [Send] else []}

schedulePoll :: M.Model -> Array Command
schedulePoll m = if m.env.hidden then [] else [SchedulePoll m.epoch m.active m.pollGeneration (if M.remote m || not (M.online m) then 2000.0 else 750.0)]

finishNative :: M.Model -> Transition -> Transition
finishNative base = case _ of
  Unchanged -> Changed base [Dispatch Shell.DrainNative]
  Changed model pending -> Changed model $ A.snoc pending $ Dispatch Shell.DrainNative
  Commands pending -> Changed base $ A.snoc pending $ Dispatch Shell.DrainNative

shortcut :: Browser.Key -> M.Model -> Boolean
shortcut key m = m.dialog == Nothing && (key.key == "/" && not (Browser.editing key) || S.toLower key.key == "l" && (key.ctrl || key.meta) && A.elem N.Workspace m.panes)

finishRoutineControl :: M.Request -> RD.RoutineControl -> Either String RD.ControlReceipt -> M.Model -> Transition
finishRoutineControl request control result m = case result of
  Left failure ->
    let message = "The change was not confirmed. " <> failure <> " Refresh to check its current state before trying again."
        rejected d = case d.record of
          F.RoutineControlRecord target | target.control == control -> d {status=M.Rejected message}
          _ -> d
    in Changed (m {routineControls=O.insert request.key (M.Failed request message) m.routineControls,dialog=map rejected m.dialog}) []
  Right receipt ->
    let ref = RD.controlRoutine control
        belongs r = r.owner == N.ownerId ref.owner && r.id == N.routineId ref.id
        routines = case receipt of
          RD.RoutineDeleted -> A.filter (not <<< belongs) m.snapshot.routines
          RD.ScheduleChanged saved -> map (\r -> if belongs r then r {paused=saved.paused,nextDue=saved.nextDue} else r) m.snapshot.routines
          _ -> m.snapshot.routines
        dialog = m.dialog >>= \d -> case d.record of
          F.RoutineControlRecord target | target.control == control -> Nothing
          _ -> Just d
        activity = case receipt, m.activity of
          RD.RoutineDeleted, N.Work (N.RoutineView route) _ | N.routeRoutine route == ref -> N.Work N.Scheduled N.Planned
          _, current -> current
        keep key = not $ S.startsWith (S.Pattern $ M.routineKey ref <> ":") key
        caption = case receipt of
          RD.ScheduleChanged saved -> if saved.paused then "Schedule paused. Current work continues." else "Schedule resumed. Next run rescheduled."
          RD.RunStopReceived RD.AlreadyFinished -> "This run has already finished. Its saved result is unchanged."
          RD.RunStopReceived RD.Stopping -> "Stopping this run…"
          RD.RunStopReceived RD.Stopped -> "This run was stopped."
          RD.RoutineDeleted -> "Routine deleted. Saved history is retained."
        updated = m {snapshot=m.snapshot {routines=routines},dialog=dialog,activity=activity
          ,documents=O.filterWithKey (\key _ -> keep key) m.documents,documentFetched=O.filterWithKey (\key _ -> keep key) m.documentFetched
          ,routineControls=O.insert request.key (M.Ready request receipt) m.routineControls
          ,polling=false,pollGeneration=m.pollGeneration+1}
    in Changed updated [Notify caption,Sync,PollNow,LoadDocument,SaveView]

finishRoutineRequest :: Submission -> {identity :: Int, origin :: N.Activity, intent :: RR.Intent} -> Either String RR.SendReceipt -> M.Model -> Transition
finishRoutineRequest request target result m = case result of
  Left failure ->
    let message = "Delivery was not confirmed. Your request is kept here. Check the chat before sending again; it may already have been accepted. " <> failure
        rejected d = if d.identity == target.identity then d {status=M.Rejected message} else d
    in Changed (m {dialog=map rejected m.dialog,routineRequestDrafts=O.alter (map rejected) request.owner m.routineRequestDrafts}) [Notify message]
  Right receipt ->
    let matching = (_.identity <$> m.dialog) == Just target.identity
        follow = matching && m.selected == request.owner && m.activity == target.origin
        updated = m {dialog=if matching then Nothing else m.dialog
          ,routineRequestDrafts=O.delete request.owner m.routineRequestDrafts
          ,routineReceipts=O.insert request.owner receipt m.routineReceipts
          ,routineChatReturn=O.insert request.owner target.origin m.routineChatReturn
          ,activity=if follow then N.Chat N.Conversation else m.activity}
    in Changed updated ([PollNow] <> if follow then [SaveView,ScrollToBottom,Focus "#message-input"] else [])
