module Sapiens.View where

import Sapiens.Form as F
import Prelude
import Data.Array as A
import Data.Maybe (Maybe(..))
import Data.String as S
import Data.Tuple (Tuple(..))
import Sapiens.Action as Intent
import Sapiens.Action (Action(..))
import Sapiens.Conversation as Chat
import Sapiens.Data as D
import Sapiens.Domain as X
import Sapiens.Dialog as Dialog
import Sapiens.Html as V
import Sapiens.Memo as Memo
import Sapiens.Model as M
import Sapiens.Navigation as N
import Sapiens.Text as T
import Sapiens.Work as Work
import Sapiens.Workspace as Workspace

render :: M.Model -> V.Html
render m = V.node "div" []
  [V.div "app" [header m,emptyCorpora m,installGate m,V.textNode "p" [V.id "corpora-startup-feedback",V.attr "role" "status"] (m.feedback)
    ,V.node "div" [V.cls "main-grid",V.hidden $ A.null m.agents || m.installRequired,V.attr "data-visible-panes" $ show $ A.length panes,V.style $ "grid-template-columns:" <> S.joinWith " " columns] [sidebar m,conversation m,Workspace.view m]]
  ,Dialog.view m,V.textNode "div" [V.id "toast",V.cls $ "toast " <> if m.toast /= "" then "visible" else "",V.attr "role" "status",V.attr "aria-live" "polite"] (m.toast)]
  where
  panes = M.visiblePanes m
  columns = [(if A.elem N.Sidebar panes then if A.length panes == 1 then "minmax(0,1fr)" else "clamp(205px,18vw,250px)" else "0px"),(if A.elem N.ChatPane panes then "minmax(300px,1fr)" else "0px"),(if A.elem N.Workspace panes then "minmax(350px,1.2fr)" else "0px")]

status :: M.Model -> String
status m = if (M.busy m) then "Connecting…" else if m.active == "" then "Not connected" else if not (M.online m) then "Offline" else if (M.activeRecord m).stopping then "Pausing…" else stateName $ (M.activeRecord m).status

stateName :: X.CorporaState -> String
stateName = case _ of
  X.Paused -> "Paused"
  X.Archived -> "Archived"
  _ -> "Active"

header :: M.Model -> V.Html
header m = V.node "header" [V.cls "app-header"] $
  [V.node "a" [V.cls "wordmark",V.attr "href" "#",V.label "Sapiens4 home",V.on "click" \e -> Link e Home] [V.span [V.cls "brand-name"] "Sapiens4"]
  ,V.control [V.id "app-menu",V.cls "icon-button menu-button",V.label "Menu",V.attr "title" "Menu"] "☰" (OpenDialog M.AppMenu F.NoRecord)
  ,V.node "nav" [V.cls "pane-controls",V.label "Panel visibility"] $ map paneButton [Tuple N.Sidebar "chat list",Tuple N.ChatPane "chat",Tuple N.Workspace "workspace"]
  ,V.control [V.cls "theme-toggle",V.label $ "Switch to " <> (if m.theme == "dark" then "light" else "dark") <> " theme"] (if m.theme == "dark" then "☀" else "☾") (Theme)
  ,V.node "button" [V.id "corpora-picker",V.cls "connection-button",V.attr "aria-controls" "corpora-menu",V.attr "aria-expanded" $ show $ m.menu == Just "corpora",V.click $ Menu "corpora"] [V.span [V.id "corpora-location"] (D.fallback (if M.remote m then "Remote" else "Local") $ X.locationName (M.activeRecord m).location),V.chevron false]
  ,V.node "button" [V.id "connection-status",V.cls "connection-status connection-button",V.attr "aria-controls" "corpora-status-menu",V.attr "aria-expanded" $ show $ m.menu == Just "status",V.attr "aria-live" "polite",V.attr "data-online" $ show $ (M.online m) && not (M.busy m) && (M.activeRecord m).status == X.Active,V.disabled $ m.active == "" || not (M.online m) || (M.busy m),V.click $ Menu "status"] [V.node "span" [V.cls "connection-dot",V.attr "aria-hidden" "true"] [],V.span [V.id "connection-label"] (status m),V.chevron false]]
  <> [corporaMenu m,statusMenu m]
  where
  panes = M.visiblePanes m
  paneButton (Tuple pane caption) =
    let visible = A.elem pane panes
        label = (if N.limit m.env == 1 || not visible then "Show " else "Hide ") <> caption
    in V.control [V.cls "pane-toggle",V.label label,V.attr "title" label,V.attr "aria-pressed" $ show visible,V.attr "aria-expanded" $ show visible] (case pane of
      N.Sidebar -> "☷"
      N.ChatPane -> "▤"
      N.Workspace -> "▣") (TogglePane pane)

corporaMenu :: M.Model -> V.Html
corporaMenu m = V.node "section" [V.id "corpora-menu",V.cls "corpora-popover",V.label "Corpora",V.hidden $ m.menu /= Just "corpora",V.attr "data-has-local" $ show local]
  [V.div "corpora-menu-heading" [V.textNode "strong" [] "Corpora",V.icon "Close corpora menu" "×" CloseMenu]
  ,V.div "corpora-columns" $ map (\s -> V.span [] (s)) (["Location","Name","Status","Device"] <> if local then ["Remote"] else [])
  ,V.node "div" [V.id "corpora-rows"] $ map row $ A.filter (\r -> m.showArchived || not (r.archived) && r.status /= X.Archived) m.records
  ,V.div "corpora-actions" [V.when m.localAvailable $ V.button "" "Remote Control Relay Servers" $ OpenDialog M.RelayList F.NoRecord,V.when m.localAvailable $ V.button "" "＋ Create new local corpora" $ OpenDialog M.CreateCorpora F.NoRecord,V.button "" "＋ Connect remote corpora" $ OpenDialog M.ConnectRemote F.NoRecord]
  ,V.node "label" [V.cls "corpora-archive-toggle"] [V.node "input" [V.attr "type" "checkbox",V.bool "checked" m.showArchived,V.check ShowArchived] [],V.text " Show archived corporas"]]
  where
  local = not (M.remote m) && A.any (( _ == X.Local) <<< _.location) m.records
  row r = V.node "div" [V.cls "corpora-record",V.attr "data-selected" $ show $ r.key == m.active]
    [V.span [V.cls "corpora-location-cell"] (X.locationName r.location)
    ,V.node "span" [V.cls "corpora-name-cell"] [V.control [V.cls "corpora-name",V.disabled $ (M.busy m) || r.status == X.Archived,V.attr "aria-pressed" $ show $ r.key == m.active] (r.name) (EnterCorpora $ r.key),V.control [V.cls "corpora-edit",V.disabled (M.busy m)] "Edit" (OpenDialog M.EditCorpora (F.CorporaRecord r false))]
    ,V.node "span" [V.cls "corpora-state"] [V.node "i" [V.cls "corpora-dot",V.attr "data-active" $ show $ r.status == X.Active && (r.key /= m.active || (M.online m))] [],V.text $ if r.key == m.active && not (M.online m) then "Offline" else stateName $ r.status]
    ,V.span [V.cls "corpora-device"] (D.fallback "Host computer" $ r.device)
    ,V.when local $ V.node "span" [V.cls "corpora-pairing-cell"] [V.when (r.location == X.Local) $ V.control [V.disabled (M.busy m)] (if r.paired then "Edit Pairing" else "New Pairing") (OpenDialog M.HostPairing (F.CorporaRecord r false))]]

statusMenu :: M.Model -> V.Html
statusMenu m = V.node "div" [V.id "corpora-status-menu",V.cls "corpora-popover status-popover",V.hidden $ m.menu /= Just "status"] $ if state == X.Archived then [V.text "Archived"] else map (\(Tuple action label) -> V.control [V.disabled $ (M.busy m) || state == (if action == X.PauseCorpora then X.Paused else X.Active)] (label <> if state == (if action == X.PauseCorpora then X.Paused else X.Active) then " ✓" else "") (Lifecycle action)) [Tuple X.PauseCorpora "Pause",Tuple X.ActivateCorpora "Activate"]
  where
  state = (M.activeRecord m).status

emptyCorpora :: M.Model -> V.Html
emptyCorpora m = V.node "section" [V.id "corpora-empty",V.cls "corpora-empty",V.hidden $ m.active /= "" || m.installRequired]
  [V.textNode "h1" [] "Your corpora",V.p "" "Connect to your Sapis on another computer.",V.button "" "Connect remote corpora" $ OpenDialog M.ConnectRemote F.NoRecord,V.when m.localAvailable $ V.button "" "Create local corpora" $ OpenDialog M.CreateCorpora F.NoRecord]

installGate :: M.Model -> V.Html
installGate m = V.node "section" [V.id "mobile-install-gate",V.label "Install Sapiens4",V.hidden $ not m.installRequired]
  [V.div "install-card" [V.span [V.cls "install-brand"] "Sapiens4",V.textNode "h1" [] "Add to Home Screen",V.p "" "Install to connect your corpora.",V.control [V.id "mobile-install-action"] "Show me how" (Install),V.link "https://sapiens4.ai/help?q=add2homescreen" "Need help? ↗"]]

sidebar :: M.Model -> V.Html
sidebar m = V.node "aside" [V.cls "sidebar",V.id "chat-list-panel",V.label "Chat list",V.hidden $ not $ A.elem N.Sidebar panes]
  [V.div "sidebar-heading" [V.textNode "h1" [V.id "corpora-identity",V.cls "corpora-identity",V.attr "title" $ m.snapshot.corpora.id] (D.fallback "Connecting…" $ m.snapshot.corpora.name),V.span [V.id "agent-count",V.label "Sapi count"] (show $ A.length $ A.filter (\a -> not $ M.isGroup a || a.retired || a.archived) m.agents),V.div "sidebar-actions" [V.control [V.id "search-toggle",V.cls "icon-button",V.label "Search Corpora",V.attr "aria-expanded" $ show m.searchOpen] "⌕" (ToggleSearch),V.icon "Create through Chief" "＋" $ DialogCommand Intent.CreateAgent]]
  ,V.node "div" [V.cls "scope-switch",V.label "Agent views"] $ map (\(Tuple scope label) -> V.control [V.cls $ if m.scope == scope then "active" <> if m.groupView /= Nothing then " group-open" else "" else "",V.attr "aria-pressed" $ show $ m.scope == scope,V.disabled $ scope == N.Groups && not (A.any (\a -> M.isGroup a && not (a.archived)) m.agents)] (label) (SetScope scope)) [Tuple N.All "All",Tuple N.Sapis "Sapis",Tuple N.Groups "Groups"]
  ,V.node "label" [V.id "corpora-search",V.cls $ "search-field " <> if m.searchOpen then "search-open" else ""] [V.span [V.attr "aria-hidden" "true"] "⌕",V.node "input" [V.id "agent-search",V.attr "type" "search",V.attr "placeholder" "Find a Sapi or Group…",V.label "Search agents",V.value m.search,V.input Search] [],V.textNode "kbd" [] "/"]
  ,V.node "div" [V.id "agent-list",V.cls "agent-list"] $ map row $ M.roster m
  ,V.node "footer" [V.cls "sidebar-bottom"] [V.div "resource-card" [V.node "button" [V.id "resource-button",V.cls "resource-open",V.label "Open shared computer",V.click $ OpenDialog M.Computer F.NoRecord] [V.span [V.cls "computer-icon",V.attr "aria-hidden" "true"] "▣",V.textNode "strong" [] "SHARED COMPUTER",V.node "span" [V.id "resource-status",V.cls $ "status-dot " <> if owner == "" then "idle" else ""] []],V.textNode "small" [V.id "resource-owner"] (if owner /= "" then "In use · " <> (M.agent m owner).name else if m.snapshot.computer.built then "Blindly4 · Available" else "Blindly4 · Build required")]]]
  where
  panes = M.visiblePanes m
  owner = m.snapshot.computer.owner
  group = M.maybeGroup m
  row a = let pinned = a.id == (if group.id /= "" then group.lead else m.snapshot.main_agent_id)
              caption = a.name <> if group.id /= "" && pinned then " · Lead" else ""
          in V.div "agent-entry" [V.node "div" [V.cls $ "agent-row " <> (if a.id == m.selected then "active " else "") <> (if pinned then "main-sapi-row " else "") <> if M.isGroup a then "group-row" else "",V.click $ if group.id /= "" && not (M.isGroup a) then DialogCommand $ Intent.MentionGroup a else Select $ a.id]
            [T.avatar a (if pinned then "main-sapi-avatar" else "") true,V.div "agent-row-copy" [V.div "agent-row-name" [V.textNode "button" [V.cls "agent-name",V.label $ "Open " <> caption,V.attr "aria-current" $ show $ a.id == m.selected,V.on "click" \e -> Link e $ Select $ a.id] (a.name),if M.isGroup a then V.span [V.cls "group-chip",V.style $ "--group-color:" <> T.color (a.color),V.label $ show (A.length $ a.members) <> " members"] (show $ A.length $ a.members) else T.groupLabels m a true],V.p "" $ (if group.id /= "" && pinned then "Lead · " else "") <> if group.id /= "" then a.role else D.fallback "Shared workspace" $ a.preview]]]

conversation :: M.Model -> V.Html
conversation m = V.node "section" [V.cls "conversation",V.id "chat-panel",V.label "Agent conversation",V.hidden $ not $ A.elem N.ChatPane panes]
  [V.node "div" [V.cls "agent-heading",V.id "agent-heading"] [T.avatar a (if m.selected == m.snapshot.main_agent_id then "large main-sapi-avatar" else "large") true,V.div "" [V.div "agent-title" [V.textNode "h2" [] (a.name),V.when (not $ M.isGroup a) $ T.groupLabels m a false],V.p "agent-role" $ (if M.isGroup a then a.description else a.role)],V.icon "Sapi settings" "···" $ OpenDialog (if M.isGroup a then M.GroupSettings else M.AgentSettings) (F.EntityRecord a)]
  ,activityNav m,viewbar m
  ,V.keyed "div" [V.cls $ "conversation-body " <> if m.activity == N.Work N.Memo N.Planned then "notes-view" else "",V.id "conversation-body",V.on "scroll" $ const Scroll,V.on "pointerdown" $ const UserScroll,V.on "touchstart" $ const UserScroll,V.on "wheel" $ const UserScroll,V.on "keydown" $ const UserScroll] $ case m.activity of
    N.Chat N.Conversation -> Chat.rows m
    N.Chat view -> [Tuple "unavailable" $ V.p "empty" $ N.chatLabel view <> " are not available yet."]
    N.Work N.Memo _ -> [Tuple ("memo:" <> m.selected) $ Memo.view m]
    N.Work N.Routines _ -> [Tuple "personal-routines" $ Work.personalRoutines m]
    N.Work N.Scheduled period -> [Tuple "routines" $ Work.routines m period]
    N.Work view period | A.elem view [N.Tasks,N.Automated] -> [Tuple "tasks" $ Work.tasks m view period]
    N.Work view _ -> [Tuple "unavailable" $ V.p "empty" $ N.workLabel view <> " is not available yet."]
    N.Updates -> [Tuple "events" $ Work.events m]
  ,Chat.composer m]
  where
  a = M.selected m
  panes = M.visiblePanes m

activityNav :: M.Model -> V.Html
activityNav m = V.node "nav" [V.cls "conversation-tabs",V.label "Agent activity"] $ map tab [Tuple "chat" "Chat",Tuple "work" "Work",Tuple "updates" "Updates"]
  where
  item activity label = V.choice (m.activity == activity) label (Navigate activity)
  branchOpen = case _ of
    N.Leaf _ -> false
    N.Branch id _ _ children -> m.menu == Just ("activity-" <> id) || A.any branchOpen children
  tree = case _ of
    N.Leaf view -> item (N.Work view N.Planned) (N.workLabel view)
    branch@(N.Branch id label view children) -> V.div "activity-submenu-row" [item (N.Work view N.Planned) label,V.controlWith [V.attr "data-activity-menu" $ "activity-" <> id,V.label $ label <> " menu",V.attr "aria-expanded" $ show $ branchOpen branch] [V.chevron true] (Menu $ "activity-" <> id),V.node "div" [V.cls "activity-menu activity-submenu",V.hidden $ not $ branchOpen branch] $ map tree children]
  tab (Tuple id label) = V.div ("activity-tab " <> if N.panel m.activity == id then "active" else "")
    [V.control [V.attr "data-panel" id,V.attr "aria-pressed" $ show $ N.panel m.activity == id] (label) (if id == "work" then DefaultWork else Navigate $ if id == "chat" then N.Chat N.Conversation else N.Updates)
    ,V.node "button" [V.cls "activity-toggle",V.label $ label <> " menu",V.attr "aria-expanded" $ show $ m.menu == Just ("activity-" <> id),V.click $ Menu $ "activity-" <> id] [V.chevron false]
    ,V.node "div" [V.cls "activity-menu",V.hidden $ if id == "work" then not (A.elem m.menu $ map (Just <<< ("activity-" <> _)) ["work","tasks","automation","routine","workflows","composer"]) else m.menu /= Just ("activity-" <> id)] $ case id of
      "chat" -> map (\view -> item (N.Chat view) $ N.chatLabel view) [N.Pins,N.Threads,N.Comments] <> [V.node "hr" [] [],V.button "" "＋ Add feedback" Feedback]
      "work" -> map tree N.workTree
      _ -> [item N.Updates "Events",V.textNode "button" [V.disabled true,V.attr "title" "Not active yet"] "Records"]]

viewbar :: M.Model -> V.Html
viewbar m = V.node "div" [V.id "activity-viewbar",V.hidden hidden] $ [V.span [V.cls "activity-view-title"] (label)] <> case m.activity of
  N.Work view period | A.elem view [N.Tasks,N.Automated,N.Scheduled] -> [V.div "work-nav-tools" [V.div "activity-view-menu" [V.controlWith [V.label $ if view == N.Scheduled then "Routine history" else if M.isGroup (M.selected m) then "Task status" else "Task history",V.attr "aria-expanded" $ show $ m.menu == Just "activity-filter"] [V.text $ N.periodLabel period,V.chevron false] (Menu "activity-filter"),V.node "div" [V.cls "activity-menu",V.hidden $ m.menu /= Just "activity-filter"] $ map (\p -> V.choice (p == period) (N.periodLabel p) (SetPeriod p)) (if view == N.Scheduled then [N.Planned,N.Paused] else [N.Planned,N.Executed] <> if M.isGroup (M.selected m) then [N.Deleted] else [])],V.when (not $ (M.selected m).archived) $ V.button "" "+ New" $ if view == N.Scheduled then PrepareDraft "Add New Scheduled Routine:\nFrequency: every XXX minutes\nExecute: PROMPT" "XXX" else PrepareDraft "Add New Task: TODO" "TODO"]]
  _ -> []
  where
  hidden = m.activity == N.Updates || m.activity == N.Work N.Memo N.Planned || m.activity == N.Chat N.Conversation
  label = case m.activity of
    N.Chat view -> N.chatLabel view
    N.Work view _ -> N.workLabel view
    N.Updates -> "Events"
