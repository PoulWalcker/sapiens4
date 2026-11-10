module Sapiens.View where

import Prelude
import Data.Array as A
import Data.Maybe (Maybe(..))
import Data.String as S
import Data.Tuple (Tuple(..))
import Sapiens.Action (Action(..))
import Sapiens.Conversation as Chat
import Sapiens.Data as D
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
  [V.div "app" [header m,accountBanner m,emptyCorpora m,installGate m,V.textNode "p" [V.id "corpora-startup-feedback",V.attr "role" "status"] (m.feedback)
    ,V.node "div" [V.cls "main-grid",V.hidden $ A.null m.agents || m.installRequired,V.attr "data-visible-panes" $ show $ A.length panes,V.style $ "grid-template-columns:" <> S.joinWith " " columns] [sidebar m,conversation m,Workspace.view m]]
  ,Dialog.view m,V.textNode "div" [V.id "toast",V.cls $ "toast " <> if m.toast /= "" then "visible" else "",V.attr "role" "status",V.attr "aria-live" "polite"] (m.toast)]
  where
  panes = M.visiblePanes m
  columns = [(if A.elem N.Sidebar panes then if A.length panes == 1 then "minmax(0,1fr)" else "clamp(205px,18vw,250px)" else "0px"),(if A.elem N.ChatPane panes then "minmax(300px,1fr)" else "0px"),(if A.elem N.Workspace panes then "minmax(350px,1.2fr)" else "0px")]

status :: M.Model -> String
status m = if m.busy then "Connecting…" else if m.active == "" then "Not connected" else if not m.online then "Offline" else if D.b "stopping" (M.activeRecord m) then "Pausing…" else stateName $ D.s "status" $ M.activeRecord m

stateName :: String -> String
stateName = case _ of
  "paused" -> "Paused"
  "archived" -> "Archived"
  _ -> "Active"

header :: M.Model -> V.Html
header m = V.node "header" [V.cls "app-header"] $
  [V.node "a" [V.cls "wordmark",V.attr "href" "#",V.label "Sapiens4 home",V.on "click" \e -> Link e Home] [V.span [V.cls "brand-name"] "Sapiens4"]
  ,V.control [V.id "app-menu",V.cls "icon-button menu-button",V.label "Menu",V.attr "title" "Menu"] "☰" (OpenDialog M.AppMenu D.empty)
  ,V.node "nav" [V.cls "pane-controls",V.label "Panel visibility"] $ map paneButton [Tuple N.Sidebar "chat list",Tuple N.ChatPane "chat",Tuple N.Workspace "workspace"]
  ,V.control [V.cls "theme-toggle",V.label $ "Switch to " <> (if m.theme == "dark" then "light" else "dark") <> " theme"] (if m.theme == "dark" then "☀" else "☾") (Theme)
  ,V.node "button" [V.id "corpora-picker",V.cls "connection-button",V.attr "aria-controls" "corpora-menu",V.attr "aria-expanded" $ show $ m.menu == Just "corpora",V.click $ Menu "corpora"] [V.span [V.id "corpora-location"] (D.fallback (if M.remote m then "Remote" else "Local") $ D.s "location" $ M.activeRecord m),V.chevron false]
  ,V.node "button" [V.id "connection-status",V.cls "connection-status connection-button",V.attr "aria-controls" "corpora-status-menu",V.attr "aria-expanded" $ show $ m.menu == Just "status",V.attr "aria-live" "polite",V.attr "data-online" $ show $ m.online && not m.busy && D.s "status" (M.activeRecord m) == "active",V.disabled $ m.active == "" || not m.online || m.busy,V.click $ Menu "status"] [V.node "span" [V.cls "connection-dot",V.attr "aria-hidden" "true"] [],V.span [V.id "connection-label"] (status m),V.chevron false]]
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
  ,V.node "div" [V.id "corpora-rows"] $ map row $ A.filter (\r -> m.showArchived || not (D.b "archived" r) && D.s "status" r /= "archived") m.records
  ,V.div "corpora-actions" [V.when m.localAvailable $ V.button "" "Remote Control Relay Servers" $ OpenDialog M.RelayList D.empty,V.when m.localAvailable $ V.button "" "＋ Create new local corpora" $ OpenDialog M.CreateCorpora D.empty,V.button "" "＋ Connect remote corpora" $ OpenDialog M.ConnectRemote D.empty]
  ,V.node "label" [V.cls "corpora-archive-toggle"] [V.node "input" [V.attr "type" "checkbox",V.bool "checked" m.showArchived,V.check ShowArchived] [],V.text " Show archived corporas"]]
  where
  local = not (M.remote m) && A.any (( _ == "Local") <<< D.s "location") m.records
  row r = V.node "div" [V.cls "corpora-record",V.attr "data-selected" $ show $ D.s "key" r == m.active]
    [V.span [V.cls "corpora-location-cell"] (D.s "location" r)
    ,V.node "span" [V.cls "corpora-name-cell"] [V.control [V.cls "corpora-name",V.disabled $ m.busy || D.s "status" r == "archived",V.attr "aria-pressed" $ show $ D.s "key" r == m.active] (D.s "name" r) (EnterCorpora $ D.s "key" r),V.control [V.cls "corpora-edit",V.disabled m.busy] "Edit" (OpenDialog M.EditCorpora r)]
    ,V.node "span" [V.cls "corpora-state"] [V.node "i" [V.cls "corpora-dot",V.attr "data-active" $ show $ D.s "status" r == "active" && (D.s "key" r /= m.active || m.online)] [],V.text $ if D.s "key" r == m.active && not m.online then "Offline" else stateName $ D.s "status" r]
    ,V.span [V.cls "corpora-device"] (D.fallback "Host computer" $ D.s "device" r)
    ,V.when local $ V.node "span" [V.cls "corpora-pairing-cell"] [V.when (D.s "location" r == "Local") $ V.control [V.disabled m.busy] (if D.b "paired" r then "Edit Pairing" else "New Pairing") (OpenDialog M.HostPairing r)]]

statusMenu :: M.Model -> V.Html
statusMenu m = V.node "div" [V.id "corpora-status-menu",V.cls "corpora-popover status-popover",V.hidden $ m.menu /= Just "status"] $ if state == "archived" then [V.text "Archived"] else map (\(Tuple action label) -> V.control [V.disabled $ m.busy || state == (if action == "pause" then "paused" else "active")] (label <> if state == (if action == "pause" then "paused" else "active") then " ✓" else "") (Lifecycle action)) [Tuple "pause" "Pause",Tuple "activate" "Activate"]
  where
  state = D.s "status" $ M.activeRecord m

emptyCorpora :: M.Model -> V.Html
emptyCorpora m = V.node "section" [V.id "corpora-empty",V.cls "corpora-empty",V.hidden $ m.active /= "" || m.installRequired]
  [V.textNode "h1" [] "Your corpora",V.p "" "Connect to your Sapis on another computer.",V.button "" "Connect remote corpora" $ OpenDialog M.ConnectRemote D.empty,V.when m.localAvailable $ V.button "" "Create local corpora" $ OpenDialog M.CreateCorpora D.empty]

installGate :: M.Model -> V.Html
installGate m = V.node "section" [V.id "mobile-install-gate",V.label "Install Sapiens4",V.hidden $ not m.installRequired]
  [V.div "install-card" [V.span [V.cls "install-brand"] "Sapiens4",V.textNode "h1" [] "Add to Home Screen",V.p "" "Install to connect your corpora.",V.control [V.id "mobile-install-action"] "Show me how" (Install),V.link "https://sapiens4.ai/help?q=add2homescreen" "Need help? ↗"]]

sidebar :: M.Model -> V.Html
sidebar m = V.node "aside" [V.cls "sidebar",V.id "chat-list-panel",V.label "Chat list",V.hidden $ not $ A.elem N.Sidebar panes]
  [V.div "sidebar-heading" [V.textNode "h1" [V.id "corpora-identity",V.cls "corpora-identity",V.attr "title" $ D.str $ D.at ["corpora","id"] m.snapshot] (D.fallback "Connecting…" $ D.str $ D.at ["corpora","name"] m.snapshot),V.span [V.id "agent-count",V.label "Sapi count"] (show $ A.length $ A.filter (\a -> not $ M.isGroup a || D.b "retired" a || D.b "archived" a) m.agents),V.div "sidebar-actions" [V.control [V.id "search-toggle",V.cls "icon-button",V.label "Search Corpora",V.attr "aria-expanded" $ show m.searchOpen] "⌕" (ToggleSearch),V.icon "Create through Chief" "＋" $ DialogCommand "create-agent" D.empty]]
  ,V.node "div" [V.cls "scope-switch",V.label "Agent views"] $ map (\(Tuple scope label) -> V.control [V.cls $ if m.scope == scope then "active" <> if m.groupView /= Nothing then " group-open" else "" else "",V.attr "aria-pressed" $ show $ m.scope == scope,V.disabled $ scope == N.Groups && not (A.any (\a -> M.isGroup a && not (D.b "archived" a)) m.agents)] (label) (SetScope scope)) [Tuple N.All "All",Tuple N.Sapis "Sapis",Tuple N.Groups "Groups"]
  ,V.node "label" [V.id "corpora-search",V.cls $ "search-field " <> if m.searchOpen then "search-open" else ""] [V.span [V.attr "aria-hidden" "true"] "⌕",V.node "input" [V.id "agent-search",V.attr "type" "search",V.attr "placeholder" "Find a Sapi or Group…",V.label "Search agents",V.value m.search,V.input Search] [],V.textNode "kbd" [] "/"]
  ,V.node "div" [V.id "agent-list",V.cls "agent-list"] $ map row $ M.roster m
  ,V.node "footer" [V.cls "sidebar-bottom"] [V.div "resource-card" [V.node "button" [V.id "resource-button",V.cls "resource-open",V.label "Open shared computer",V.click $ OpenDialog M.Computer D.empty] [V.span [V.cls "computer-icon",V.attr "aria-hidden" "true"] "▣",V.textNode "strong" [] "SHARED COMPUTER",V.node "span" [V.id "resource-status",V.cls $ "status-dot " <> if owner == "" then "idle" else ""] []],V.textNode "small" [V.id "resource-owner"] (if owner /= "" then "In use · " <> D.s "name" (M.agent m owner) else if D.flag $ D.at ["computer","built"] m.snapshot then "Blindly4 · Available" else "Blindly4 · Build required")]]]
  where
  panes = M.visiblePanes m
  owner = D.str $ D.at ["computer","owner"] m.snapshot
  group = M.maybeGroup m
  row a = let pinned = D.s "id" a == (if D.valid group then D.s "lead" group else D.s "main_agent_id" m.snapshot)
              caption = D.s "name" a <> if D.valid group && pinned then " · Lead" else ""
          in V.div "agent-entry" [V.node "div" [V.cls $ "agent-row " <> (if D.s "id" a == m.selected then "active " else "") <> (if pinned then "main-sapi-row " else "") <> if M.isGroup a then "group-row" else "",V.click $ if D.valid group && not (M.isGroup a) then DialogCommand "group-mention" a else Select $ D.s "id" a]
            [T.avatar a (if pinned then "main-sapi-avatar" else "") true,V.div "agent-row-copy" [V.div "agent-row-name" [V.textNode "button" [V.cls "agent-name",V.label $ "Open " <> caption,V.attr "aria-current" $ show $ D.s "id" a == m.selected,V.on "click" \e -> Link e $ Select $ D.s "id" a] (D.s "name" a),if M.isGroup a then V.span [V.cls "group-chip",V.style $ "--group-color:" <> T.color (D.s "color" a),V.label $ show (A.length $ D.rows "members" a) <> " members"] (show $ A.length $ D.rows "members" a) else T.groupLabels m a true],V.p "" $ (if D.valid group && pinned then "Lead · " else "") <> if D.valid group then D.s "role" a else D.fallback "Shared workspace" $ D.s "preview" a]]]

conversation :: M.Model -> V.Html
conversation m = V.node "section" [V.cls "conversation",V.id "chat-panel",V.label "Agent conversation",V.hidden $ not $ A.elem N.ChatPane panes]
  [V.node "div" [V.cls "agent-heading",V.id "agent-heading"] [T.avatar a (if m.selected == D.s "main_agent_id" m.snapshot then "large main-sapi-avatar" else "large") true,V.div "" [V.div "agent-title" [V.textNode "h2" [] (D.s "name" a),V.when (not $ M.isGroup a) $ T.groupLabels m a false],V.p "agent-role" $ D.s (if M.isGroup a then "description" else "role") a],V.icon "Sapi settings" "···" $ OpenDialog (if M.isGroup a then M.GroupSettings else M.AgentSettings) a]
  ,activityNav m,viewbar m
  ,V.keyed "div" [V.cls $ "conversation-body " <> if m.activity == N.Work N.Memo N.Planned then "notes-view" else "",V.id "conversation-body",V.on "scroll" $ const Scroll,V.on "pointerdown" $ const UserScroll,V.on "touchstart" $ const UserScroll,V.on "wheel" $ const UserScroll,V.on "keydown" $ const UserScroll] $ case m.activity of
    N.Chat N.Conversation -> Chat.rows m
    N.Chat view -> [Tuple "unavailable" $ V.p "empty" $ N.chatLabel view <> " are not available yet."]
    N.Work N.Memo _ -> [Tuple ("memo:" <> m.selected) $ Memo.view m]
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
  tree = case _ of
    N.Leaf view -> item (N.Work view N.Planned) (N.workLabel view)
    N.Branch id label view children -> V.div "activity-submenu-row" [item (N.Work view N.Planned) label,V.controlWith [V.attr "data-activity-menu" $ "activity-" <> id,V.label $ label <> " menu",V.attr "aria-expanded" $ show $ m.menu == Just ("activity-" <> id)] [V.chevron true] (Menu $ "activity-" <> id),V.node "div" [V.cls "activity-menu activity-submenu",V.hidden $ m.menu /= Just ("activity-" <> id)] $ map tree children]
  tab (Tuple id label) = V.div ("activity-tab " <> if N.panel m.activity == id then "active" else "")
    [V.control [V.attr "data-panel" id,V.attr "aria-pressed" $ show $ N.panel m.activity == id] (label) (if id == "work" then DefaultWork else Navigate $ if id == "chat" then N.Chat N.Conversation else N.Updates)
    ,V.node "button" [V.cls "activity-toggle",V.label $ label <> " menu",V.attr "aria-expanded" $ show $ m.menu == Just ("activity-" <> id),V.click $ Menu $ "activity-" <> id] [V.chevron false]
    ,V.node "div" [V.cls "activity-menu",V.hidden $ if id == "work" then not (A.elem m.menu $ map (Just <<< ("activity-" <> _)) ["work","tasks","automation","routine","workflows","composer"]) else m.menu /= Just ("activity-" <> id)] $ case id of
      "chat" -> map (\view -> item (N.Chat view) $ N.chatLabel view) [N.Pins,N.Threads,N.Comments] <> [V.node "hr" [] [],V.button "" "＋ Add feedback" Feedback]
      "work" -> map tree N.workTree
      _ -> [item N.Updates "Events",V.textNode "button" [V.disabled true,V.attr "title" "Not active yet"] "Records"]]

viewbar :: M.Model -> V.Html
viewbar m = V.node "div" [V.id "activity-viewbar",V.hidden hidden] $ [V.span [V.cls "activity-view-title"] (label)] <> case m.activity of
  N.Work view period | A.elem view [N.Tasks,N.Automated,N.Scheduled] -> [V.div "work-nav-tools" [V.div "activity-view-menu" [V.controlWith [V.label $ if view == N.Scheduled then "Routine history" else if M.isGroup (M.selected m) then "Task status" else "Task history",V.attr "aria-expanded" $ show $ m.menu == Just "activity-filter"] [V.text $ N.periodLabel period,V.chevron false] (Menu "activity-filter"),V.node "div" [V.cls "activity-menu",V.hidden $ m.menu /= Just "activity-filter"] $ map (\p -> V.choice (p == period) (N.periodLabel p) (SetPeriod p)) (if view == N.Scheduled then [N.Planned,N.Paused] else [N.Planned,N.Executed] <> if M.isGroup (M.selected m) then [N.Deleted] else [])],V.when (not $ D.b "archived" $ M.selected m) $ V.button "" "+ New" $ if view == N.Scheduled then PrepareDraft "Add New Scheduled Routine:\nFrequency: every XXX minutes\nExecute: PROMPT" "XXX" else PrepareDraft "Add New Task: TODO" "TODO"]]
  _ -> []
  where
  hidden = m.activity == N.Updates || m.activity == N.Work N.Memo N.Planned || m.activity == N.Chat N.Conversation
  label = case m.activity of
    N.Chat view -> N.chatLabel view
    N.Work view _ -> N.workLabel view
    N.Updates -> "Events"


accountBanner :: M.Model -> V.Html
accountBanner m = V.when (M.accountBlocked m && D.b "completed" m.account) $
  V.node "aside" [V.cls "account-banner",V.attr "role" "status"]
    [V.text $ if D.b "ready" m.account then "AI work is paused. Review any pending tasks before continuing with this account." else "Codex needs attention. Reconnect in AI Connections. Your chats and files are still available."
    ,V.button "button" "AI Connections" $ OpenDialog M.AIProviders D.empty]
