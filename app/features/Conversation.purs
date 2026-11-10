module Sapiens.Conversation where

import Prelude
import Data.Argonaut.Core (Json)
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
import Sapiens.Html as V
import Sapiens.Model as M
import Sapiens.Navigation as N
import Sapiens.Text as T

statusName :: String -> String
statusName = case _ of
  "output_pending" -> "Awaiting output pulse"
  "queued" -> "Queued"
  "running" -> "Running"
  "done" -> "Completed"
  "warning" -> "Warning"
  "failed" -> "Failed"
  "interrupted" -> "Interrupted"
  "conflict" -> "Needs review"
  "cancelled" -> "Dismissed"
  value -> value

attention :: Json -> Boolean
attention t = A.elem (D.s "status" t) ["failed","interrupted","conflict"]

turnActions :: M.Model -> Json -> V.Html
turnActions m turn = V.div "actions" $ case D.s "status" turn of
  "running" -> [V.control [V.cls "button",V.disabled $ not m.online || D.b "stopping" (activity m turn)] "Stop" (command "cancel"),V.textNode "small" [] "Already completed actions are not undone."]
  "queued" -> [V.button "button" "Cancel queued turn" $ command "cancel"]
  _ | attention turn -> [V.button "button" "Retry" $ command "retry",V.button "button" "Dismiss" $ command "cancel"]
  _ -> []
  where
  command action = Mutate ("/api/agents/" <> B.encodeURI (D.s "agent" turn) <> "/turns/" <> B.encodeURI (D.s "id" turn) <> "/" <> action) "POST" D.empty Refresh

activity :: M.Model -> Json -> Json
activity m turn =
  let current = D.at ["run_activity",D.fallback (D.s "id" turn) $ D.s "batch_id" turn] m.snapshot
      fallback = D.at ["activity",D.s "agent" turn] m.snapshot
      a = if D.valid current then current else fallback
  in if D.s "turn" a == D.s "id" turn then a else D.empty

progress :: M.Model -> Json -> V.Html
progress m turn = V.div "" [V.textNode "strong" [] (phase <> " · " <> show elapsed <> "s"),V.p "" detail,V.when (quiet >= 60) $ V.textNode "small" [] ("No recent update · " <> show quiet <> "s since last activity")]
  where
  a = activity m turn
  started = if D.n "started" a == 0.0 then B.parseTime $ D.s "created" turn else D.n "started" a
  updated = if D.n "updated" a /= 0.0 then D.n "updated" a else if D.n "started" a /= 0.0 then D.n "started" a else m.now
  elapsed = max 0 $ I.floor $ (m.now-started)/1000.0
  quiet = max 0 $ I.floor $ (m.now-updated)/1000.0
  phase = if not m.online then "Disconnected" else if D.b "stopping" a then "Stopping…" else if A.elem (D.s "status" turn) ["output_pending","queued"] then statusName $ D.s "status" turn else D.fallback "Starting model" $ D.s "phase" a
  detail = if D.s "tool" a /= "" then "Current tool: " <> D.s "tool" a else if D.s "last_action" a /= "" then "Last completed action: " <> D.s "last_action" a else ""

groupCallControls :: Json -> Json -> V.Html
groupCallControls group request = V.div "actions" $ if not (A.elem state ["queued","running","failed","interrupted"]) then [] else
  (if A.elem state ["failed","interrupted"] && not (D.valid $ D.get "task" request) && not (D.b "archived" group) then [V.button "button" "Retry" $ command "retry"] else []) <> [V.button "button" (if state == "running" then "Stop" else if state == "queued" then "Cancel" else "Dismiss") $ command "cancel"]
  where
  state = D.s "status" request
  command action = Mutate ("/api/groups/" <> D.s "id" group <> "/calls/" <> D.s "id" request <> "/" <> action) "POST" D.empty Refresh

message :: M.Model -> Json -> V.Html
message m msg = V.node "div" [V.attr "data-chat-key" $ D.s "chatKey" msg,V.cls $ "message " <> (if user then "user " else "") <> if D.s "warning" msg /= "" then "warning-message" else ""]
  [ V.div "message-meta" ([if user then V.span [] "↗" else T.avatar (if M.isGroup a then M.agent m (D.s "author" msg) else a) "mini" false,V.node "strong" [] [author],V.textNode "time" [] (T.time $ D.s "created" msg)] <> map _.badge warnings)
  , V.div "message-bubble" (map _.detail warnings <> T.paragraphs m (D.s "text" msg) <> [V.when (not $ A.null $ D.rows "attachments" msg) $ V.div "message-attachments" $ map T.attachment $ D.rows "attachments" msg,V.when (D.b "stale" msg) $ V.textNode "small" [V.cls "group-banner"] "Result from an earlier task revision · current work was preserved.",V.when (D.valid (D.get "delegation" msg) && M.blocksChat request) $ turnActions m request])
  ]
  where
  a = M.selected m
  user = D.s "role" msg == "user"
  request = D.get "requestTurn" msg
  author = if D.s "author" msg /= "" then if M.isGroup a then T.mention m (D.s "author" msg) else V.text $ D.s "name" $ M.agent m $ D.s "author" msg else if user then V.text "Admin" else T.mention m m.selected
  warning key caption body = let open = Set.member ("warning:" <> key) m.expanded in
    { badge: V.control [V.cls "warning-badge",V.attr "aria-expanded" $ show open,V.attr "aria-controls" $ "warning-" <> key] (caption) (ToggleExpanded $ "warning:" <> key)
    , detail: V.node "div" [V.cls "warning-details",V.id $ "warning-" <> key,V.hidden $ not open] body }
  error = D.s "error" request
  reason = if D.matches "(timeouterror|time limit|exceeded [0-9]+(?:\\.[0-9]+)?s)" (S.toLower error) then "Timed out" else if D.matches "(tool-step limit|tool limit)" (S.toLower error) then "Tool limit reached" else ""
  label = if reason == "" then statusName $ D.s "status" request else reason <> if D.s "status" request == "cancelled" then " · Dismissed" else ""
  warnings = (if attention request || D.s "status" request == "cancelled" then [warning ("request-" <> D.s "id" request) label [V.p "" $ D.fallback (if D.s "status" request == "cancelled" then "This request was dismissed." else "This request needs attention.") $ fromMaybe "" $ A.head $ S.split (S.Pattern "\n") error,V.p "" "No final reply was saved for this request.",turnActions m request]] else []) <> (if D.s "warning" msg /= "" then [warning (D.s "turnId" msg) "Warning" [V.text $ D.s "warning" msg]] else [])

rows :: M.Model -> Array (Tuple String V.Html)
rows m =
  [Tuple "banner" $ V.when (M.isGroup a && D.b "archived" a) $ V.p "group-banner" "Archived · history is preserved. Restore this Group from settings to continue."
  ,Tuple "divider" $ V.div "day-divider" [V.text $ (if M.isGroup a then "GROUP " else "") <> "CONVERSATION"]]
  <> map (\msg -> Tuple (D.s "chatKey" msg) $ message m msg) messages
  <> [Tuple "empty" $ V.when (A.null messages) $ V.div "empty" [V.text $ if M.isGroup a then "Message the Group to reach " <> D.s "name" (M.agent m $ D.s "lead" a) <> ", or @mention a member." else "Start a conversation."]]
  <> (if M.isGroup a then map groupProgress (A.filter (\r -> A.elem (D.s "status" r) ["queued","running","failed","interrupted"]) $ D.rows "requests" a) else map personalProgress (A.filter (\t -> D.s "agent" t == m.selected && M.blocksChat t && (D.s "batch_id" t == "" || D.s "batch_id" t == D.s "id" t)) $ D.rows "turns" m.snapshot))
  <> map (\w -> Tuple ("clarify:" <> D.s "id" w) $ V.button "button" "Answer Chief" $ Clarify w) (A.filter (\w -> D.s "state" w == "WaitingForAdmin" && A.elem m.selected [D.s "owner" w,D.str $ D.at ["origin","agent"] w]) $ D.rows "workloads" m.snapshot)
  where
  a = M.selected m
  messages = fromMaybe [] $ O.lookup m.selected m.messages
  groupProgress r = let turn = D.find (D.s "id" r) $ D.rows "turns" m.snapshot in Tuple ("request:" <> D.s "id" r) $ V.node "section" [V.cls "live-status",V.attr "data-chat-key" $ "request:" <> D.s "id" r]
    [V.textNode "strong" [] (D.s "name" (M.agent m $ D.s "target" r) <> " · " <> statusName (D.s "status" r)),V.when (M.blocksChat turn) $ progress m turn,V.when (D.s "error" r /= "") $ V.p "" $ D.s "error" r,groupCallControls a r]
  personalProgress t = Tuple ("progress:" <> D.s "id" t) $ V.node "section" [V.cls "live-status",V.attr "data-chat-key" $ "progress:" <> D.s "id" t] [progress m t,turnActions m t]

composer :: M.Model -> V.Html
composer m = V.node "div" [V.cls "composer-area",V.id "composer-area",V.hidden $ m.activity /= N.Chat N.Conversation || D.b "archived" a]
  [ V.node "div" [V.id "attachment-chip"] $ map (\item -> V.div "attachment-chip" [T.attachment item,V.icon ("Remove " <> D.s "name" item) "×" $ RemoveAttachment $ D.s "id" item]) $ M.attachments m
  , V.node "form" [V.cls "composer",V.id "chat-form",V.submit Send]
      [V.node "textarea" [V.attr "rows" "2",V.id "message-input",V.attr "maxlength" "16000",V.attr "placeholder" placeholder,V.label $ "Message " <> D.s "name" a,V.value $ M.draft m,V.input Draft,V.on "keydown" MessageKey,V.click MentionCursor,V.attr "aria-expanded" $ show $ m.mention /= Nothing] []
      ,V.div "composer-bottom" [V.icon "Add attachment" "＋" $ OpenDialog M.Attach D.empty,V.textNode "button" [V.attr "type" "submit",V.cls "send-button",V.label "Send message",V.disabled blocked] "↑"]
      ,case m.mention of
        Nothing -> V.empty
        Just menu -> V.node "div" [V.cls "mention-suggestions",V.id "mention-suggestions",V.attr "role" "listbox",V.label "Sapis"] $ if A.null menu.choices then [V.div "empty" [V.text "No matching Sapis."]] else A.mapWithIndex (\i entity -> V.node "button" [V.attr "type" "button",V.attr "role" "option",V.id $ "mention-option-" <> show i,V.attr "aria-selected" $ show $ i == menu.index,V.click $ MentionChoice i] [V.textNode "strong" [] ("@" <> D.s "name" entity),V.textNode "small" [] "Sapi"]) menu.choices
      ]
  ]
  where
  a = M.selected m
  placeholder = case m.clarification of
    Just work | D.s "owner" work == m.selected -> "Clarify " <> D.s "taskId" work <> " for Chief…"
    _ -> if N.limit m.env < 3 then "Message…" else "Message " <> D.s "name" a <> "…"
  blocked = M.accountBlocked m || not m.online || m.busy || D.b "retired" a || D.b "archived" a || A.elem (D.str $ D.at ["corpora","status"] m.snapshot) ["paused","archived"] || Set.member m.selected m.sending || m.uploading > 0
