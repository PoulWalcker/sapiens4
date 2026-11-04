module Sapiens.Conversation where

import Sapiens.Form as F
import Prelude
import Data.Array as A
import Data.Int as I
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set as Set
import Data.String as S
import Data.Tuple (Tuple(..))
import Foreign.Object as O
import Libs4.Browser as B
import Sapiens.Action (Action(..), Success(..))
import Sapiens.Data as D
import Sapiens.Domain as X
import Sapiens.Html as V
import Sapiens.Model as M
import Sapiens.Navigation as N
import Sapiens.Text as T

statusName :: X.CallState -> String
statusName = X.callName

attention :: X.Turn -> Boolean
attention t = A.elem t.status [X.Failed,X.Interrupted,X.Conflict]

turnActions :: M.Model -> X.Turn -> V.Html
turnActions m turn = V.div "actions" $ case turn.status of
  X.Running -> [V.control [V.cls "button",V.disabled $ not (M.online m) || (activity m turn).stopping] "Stop" (command "cancel"),V.textNode "small" [] "Already completed actions are not undone."]
  X.Queued -> [V.button "button" "Cancel queued turn" $ command "cancel"]
  _ | attention turn -> [V.button "button" "Retry" $ command "retry",V.button "button" "Dismiss" $ command "cancel"]
  _ -> []
  where
  command action = Mutate ("/api/agents/" <> B.encodeURI (turn.agent) <> "/turns/" <> B.encodeURI (turn.id) <> "/" <> action) "POST" D.empty Refresh

activity :: M.Model -> X.Turn -> X.Activity
activity m turn =
  let current = O.lookup (D.fallback turn.id turn.batch_id) m.snapshot.run_activity
      fallback = fromMaybe (X.activity D.empty) $ O.lookup turn.agent m.snapshot.activity
      a = fromMaybe fallback current
  in if a.turn == turn.id then a else X.activity D.empty

progress :: M.Model -> X.Turn -> V.Html
progress m turn = V.div "" [V.textNode "strong" [] (phase <> " · " <> show elapsed <> "s"),V.p "" detail,V.when (quiet >= 60) $ V.textNode "small" [] ("No recent update · " <> show quiet <> "s since last activity")]
  where
  a = activity m turn
  started = if a.started == 0.0 then B.parseTime $ turn.created else a.started
  updated = if a.updated /= 0.0 then a.updated else if a.started /= 0.0 then a.started else m.now
  elapsed = max 0 $ I.floor $ (m.now-started)/1000.0
  quiet = max 0 $ I.floor $ (m.now-updated)/1000.0
  phase = if not (M.online m) then "Disconnected" else if a.stopping then "Stopping…" else if A.elem (turn.status) [X.OutputPending,X.Queued] then statusName $ turn.status else D.fallback "Starting model" $ a.phase
  detail = if a.tool /= "" then "Current tool: " <> a.tool else if a.last_action /= "" then "Last completed action: " <> a.last_action else ""

groupCallControls :: X.Entity -> X.GroupRequest -> V.Html
groupCallControls group request = V.div "actions" $ if not (A.elem state [X.Queued,X.Running,X.Failed,X.Interrupted]) then [] else
  (if A.elem state [X.Failed,X.Interrupted] && not (request.hasTask) && not (group.archived) then [V.button "button" "Retry" $ command "retry"] else []) <> [V.button "button" (if state == X.Running then "Stop" else if state == X.Queued then "Cancel" else "Dismiss") $ command "cancel"]
  where
  state = request.status
  command action = Mutate ("/api/groups/" <> group.id <> "/calls/" <> request.id <> "/" <> action) "POST" D.empty Refresh

message :: M.Model -> X.Message -> V.Html
message m msg = V.node "div" [V.attr "data-chat-key" $ msg.chatKey,V.cls $ "message " <> (if user then "user " else "") <> if msg.warning /= "" then "warning-message" else ""]
  [ V.div "message-meta" ([if user then V.span [] "↗" else T.avatar (if M.isGroup a then M.agent m (msg.author) else a) "mini" false,V.node "strong" [] [author],V.textNode "time" [] (T.time $ msg.created)] <> map _.badge warnings)
  , V.div "message-bubble" (map _.detail warnings <> T.paragraphs m (msg.text) <> [V.when (not $ A.null $ msg.attachments) $ V.div "message-attachments" $ map T.attachment $ msg.attachments,V.when (msg.stale) $ V.textNode "small" [V.cls "group-banner"] "Result from an earlier task revision · current work was preserved.",V.when (msg.delegated && M.blocksChat request) $ turnActions m request])
  ]
  where
  a = M.selected m
  user = msg.user
  request = fromMaybe (X.turn D.empty) msg.request
  author = if msg.author /= "" then if M.isGroup a then T.mention m (msg.author) else V.text $ (M.agent m msg.author).name else if user then V.text "Admin" else T.mention m m.selected
  warning key caption body = let open = Set.member ("warning:" <> key) m.expanded in
    { badge: V.control [V.cls "warning-badge",V.attr "aria-expanded" $ show open,V.attr "aria-controls" $ "warning-" <> key] (caption) (ToggleExpanded $ "warning:" <> key)
    , detail: V.node "div" [V.cls "warning-details",V.id $ "warning-" <> key,V.hidden $ not open] body }
  error = request.error
  reason = if D.matches "(timeouterror|time limit|exceeded [0-9]+(?:\\.[0-9]+)?s)" (S.toLower error) then "Timed out" else if D.matches "(tool-step limit|tool limit)" (S.toLower error) then "Tool limit reached" else ""
  label = if reason == "" then statusName $ request.status else reason <> if request.status == X.Cancelled then " · Dismissed" else ""
  warnings = (if attention request || request.status == X.Cancelled then [warning ("request-" <> request.id) label [V.p "" $ D.fallback (if request.status == X.Cancelled then "This request was dismissed." else "This request needs attention.") $ fromMaybe "" $ A.head $ S.split (S.Pattern "\n") error,V.p "" "No final reply was saved for this request.",turnActions m request]] else []) <> (if msg.warning /= "" then [warning (msg.turnId) "Warning" [V.text $ msg.warning]] else [])

rows :: M.Model -> Array (Tuple String V.Html)
rows m =
  [Tuple "banner" $ V.when (M.isGroup a && a.archived) $ V.p "group-banner" "Archived · history is preserved. Restore this Group from settings to continue."
  ,Tuple "divider" $ V.div "day-divider" [V.text $ (if M.isGroup a then "GROUP " else "") <> "CONVERSATION"]]
  <> map (\msg -> Tuple (msg.chatKey) $ message m msg) messages
  <> [Tuple "empty" $ V.when (A.null messages) $ V.div "empty" [V.text $ if M.isGroup a then "Message the Group to reach " <> (M.agent m a.lead).name <> ", or @mention a member." else "Start a conversation."]]
  <> (if M.isGroup a then map groupProgress (A.filter (\r -> A.elem (r.status) [X.Queued,X.Running,X.Failed,X.Interrupted]) $ a.requests) else map personalProgress (A.filter (\t -> t.agent == m.selected && M.blocksChat t && (t.batch_id == "" || t.batch_id == t.id)) $ m.snapshot.turns))
  <> map (\w -> Tuple ("clarify:" <> w.id) $ V.button "button" "Answer Chief" $ Clarify w) (A.filter (\w -> w.state == X.WorkWaitingForAdmin && A.elem m.selected [w.owner,w.origin.agent]) $ m.snapshot.workloads)
  where
  a = M.selected m
  messages = fromMaybe [] $ O.lookup m.selected m.messages
  groupProgress r = let turn = fromMaybe (X.turn D.empty) $ A.find ((r.id == _) <<< _.id) m.snapshot.turns in Tuple ("request:" <> r.id) $ V.node "section" [V.cls "live-status",V.attr "data-chat-key" $ "request:" <> r.id]
    [V.textNode "strong" [] ((M.agent m r.target).name <> " · " <> statusName (r.status)),V.when (M.blocksChat turn) $ progress m turn,V.when (r.error /= "") $ V.p "" $ r.error,groupCallControls a r]
  personalProgress t = Tuple ("progress:" <> t.id) $ V.node "section" [V.cls "live-status",V.attr "data-chat-key" $ "progress:" <> t.id] [progress m t,turnActions m t]

composer :: M.Model -> V.Html
composer m = V.node "div" [V.cls "composer-area",V.id "composer-area",V.hidden $ m.activity /= N.Chat N.Conversation || a.archived]
  [ V.node "div" [V.id "attachment-chip"] $ map (\item -> V.div "attachment-chip" [T.attachment item,V.icon ("Remove " <> item.name) "×" $ RemoveAttachment $ item.id]) $ M.attachments m
  , V.node "form" [V.cls "composer",V.id "chat-form",V.submit Send]
      [V.node "textarea" [V.attr "rows" "2",V.id "message-input",V.attr "maxlength" "16000",V.attr "placeholder" placeholder,V.label $ "Message " <> a.name,V.value $ M.draft m,V.input Draft,V.on "keydown" MessageKey,V.click MentionCursor,V.attr "aria-expanded" $ show $ m.mention /= Nothing] []
      ,V.div "composer-bottom" [V.icon "Add attachment" "＋" $ OpenDialog M.Attach F.NoRecord,V.textNode "button" [V.attr "type" "submit",V.cls "send-button",V.label "Send message",V.disabled blocked] "↑"]
      ,case m.mention of
        Nothing -> V.empty
        Just menu -> V.node "div" [V.cls "mention-suggestions",V.id "mention-suggestions",V.attr "role" "listbox",V.label "Sapis"] $ if A.null menu.choices then [V.div "empty" [V.text "No matching Sapis."]] else A.mapWithIndex (\i entity -> V.node "button" [V.attr "type" "button",V.attr "role" "option",V.id $ "mention-option-" <> show i,V.attr "aria-selected" $ show $ i == menu.index,V.click $ MentionChoice i] [V.textNode "strong" [] ("@" <> entity.name),V.textNode "small" [] "Sapi"]) menu.choices
      ]
  ]
  where
  a = M.selected m
  placeholder = case m.clarification of
    Just work | work.owner == m.selected -> "Clarify " <> work.taskId <> " for Chief…"
    _ -> if N.limit m.env < 3 then "Message…" else "Message " <> a.name <> "…"
  blocked = not (M.online m) || (M.busy m) || a.retired || a.archived || A.elem (m.snapshot.corpora.status) [X.Paused,X.Archived] || Set.member m.selected m.sending || m.uploading > 0
