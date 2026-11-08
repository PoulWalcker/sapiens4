module Sapiens.Update where

import Prelude
import Data.Either (Either(..), either)
import Data.Array as A
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
  | SnapshotReceived Int String (Either String {snapshot :: X.Snapshot, now :: Number})
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
  | MessageReceived Submission (Either String Unit)
  | RemoveAttachment String
  | ToggleExpanded String
  | Select String
  | SetScope N.Scope
  | Navigate N.Activity
  | SetPeriod N.Period
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
  | FetchDocument M.Request
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
  , workload :: Maybe String, path :: String }

submission :: M.Model -> Maybe Submission
submission m =
  let text = S.trim $ M.draft m
      items = M.attachments m
      owner = M.selected m
  in if M.accountBlocked m || (text == "" && A.null items) || Set.member m.selected m.sending || m.uploading > 0 || not (M.online m) || (M.busy m) || owner.retired || owner.archived || A.elem (m.snapshot.corpora.status) [X.Paused,X.Archived] then Nothing
     else Just {epoch:m.epoch,owner:m.selected,text,attachments:map (_.id) items,path:M.ownerPath m <> "/messages",workload:case m.clarification of
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
            Nothing -> [Dispatch Shell.CheckSetup]
            Just record -> [if record.key == m.active then ConnectRecord record else Dispatch $ Shell.EnterCorpora record.key]
          pairing = if localPair then [ReplaceHistory m.env.path] <> (case local of
            Nothing -> []
            Just record -> [Dispatch $ Shell.OpenDialog M.HostPairing $ F.CorporaRecord record false]) else []
      in Changed (if localPair then next {env=m.env {search=""}} else next) ([SaveRegistry registry.records] <> connect <> pairing <> [Sync])
  Entered record cached ->
    let current = m {active=record.key,connection=M.Offline,polling=true,saving=false,nativeReporting=false,nativeReports=[],epoch=m.epoch+1,dialog=Nothing,scrollPosition=Nothing}
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
  SnapshotReceived epoch active result
    | epoch /= m.epoch || active /= m.active -> Unchanged
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
          in Changed updated ([RestoreScroll,LoadDocument] <> (if previewDue then [LoadPreview] else []) <> [Sync,Dispatch Shell.CheckSetup] <> schedulePoll updated)
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
  RequestDocument -> case M.currentDocument m of
    Nothing -> Unchanged
    Just request ->
      let old = fromMaybe M.NotAsked $ O.lookup request.key m.documents
      in if (_.revision <$> M.requestOf old) == Just request.revision then Unchanged
         else Changed (m {sequence=request.sequence,documents=O.insert request.key (M.Loading request $ M.valueOf old) m.documents}) [FetchDocument request]
  ReceiveDocument request result
    | request.epoch /= m.epoch -> Unchanged
    | otherwise -> case O.lookup request.key m.documents >>= M.requestOf of
        Just pending | pending == request -> Changed
          (m {documents=O.insert request.key (either (M.Failed request) (M.Ready request) result) m.documents})
          (case result of
            Right (X.MemoDocument document) -> [LoadMemoImages request document]
            _ -> [])
        _ -> Unchanged
  RetryDocument key -> Changed (m {documents=O.delete key m.documents}) [LoadDocument]
  Draft value -> Changed (m {drafts=O.insert m.selected value m.drafts}) [ResizeComposer,UpdateMention,SavePreferences]
  Send -> case submission m of
    Nothing -> Unchanged
    Just request -> Changed (m {sending=Set.insert m.selected m.sending,mention=Nothing}) [SendMessage request]
  MessageReceived request result
    | request.epoch /= m.epoch -> Unchanged
    | otherwise ->
        let current = m {sending=Set.delete request.owner m.sending}
        in case result of
          Left failure -> Changed current [Notify failure]
          Right _ -> Changed
            (current {drafts=if S.trim (fromMaybe "" $ O.lookup request.owner m.drafts) == request.text then O.insert request.owner "" m.drafts else m.drafts
              , attachments=O.insert request.owner (A.filter (\item -> not $ A.elem (item.id) request.attachments) $ fromMaybe [] $ O.lookup request.owner m.attachments) m.attachments
              , clarification=Nothing})
            [SavePreferences,ScrollToBottom,PollNow]
  RemoveAttachment id -> Changed (m {attachments=O.alter (Just <<< A.filter ((_ /= id) <<< _.id) <<< fromMaybe []) m.selected m.attachments}) [SavePreferences]
  ToggleExpanded key -> Changed (m {expanded=(if Set.member key m.expanded then Set.delete else Set.insert) key m.expanded}) []
  Select id -> Changed (M.select id m) [ScrollToBottom,LoadPreview,SaveView]
  SetScope scope -> Changed
    (m {scope=scope,groupView=Nothing,panes=if N.limit m.env < 3 then N.togglePane false (N.limit m.env) (M.hasTab m) N.Sidebar m.panes else m.panes})
    [SaveView]
  Navigate activity -> Changed (m {activity=activity,menu=Nothing}) [ScrollToStart,LoadDocument,SaveView]
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
