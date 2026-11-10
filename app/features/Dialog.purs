module Sapiens.Dialog where

import Prelude
import Data.Argonaut.Core (Json)
import Data.Array as A
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String as S
import Data.Tuple (Tuple(..))
import Libs4.Browser as B
import Sapiens.Action (Action(..), Success(..))
import Sapiens.Data as D
import Sapiens.Html as V
import Sapiens.Model as M
import Sapiens.Navigation as N
import Sapiens.Remote as Remote
import Sapiens.Text as T
import Sapiens.Work as W

nameHelp :: String
nameHelp = "Start with A–Z. Letters, numbers, and - _ . : # + | ( ) & $ ^ are allowed. No spaces."

make :: M.DialogKind -> Json -> M.Model -> M.Dialog
make kind record m = {providerPage:M.Providers,kind,title,record,fields,content:D.null,error:"",busy:false,identity:m.sequence+1}
  where
  name = D.s "name" record
  title = case kind of
    M.AppMenu -> D.fallback "CORPORA" $ D.str $ D.at ["corpora","name"] m.snapshot
    M.WorkspaceSettings -> "Workspace & settings"
    M.AIProviders -> "AI Connections"
    M.Computer -> "Shared computer"
    M.Onboarding -> "Set up Sapiens"
    M.AgentSettings -> name <> " settings"
    M.GroupSettings -> name <> " settings"
    M.Attach -> "Attach"
    M.Reference "link" -> "Add link"
    M.Reference _ -> "Add filepath"
    M.DecisionPrompts -> "Decision prompts"
    M.GroupTask -> if D.s "id" record == "" then "Create Group task" else "Edit Group task"
    M.CreateCorpora -> "Create local corpora"
    M.EditCorpora -> "Message to ChiefSapi"
    M.ConnectRemote -> "Connect remote corpora"
    M.HostPairing -> if D.b "paired" record then "Edit Pairing" else "New Pairing"
    M.RelayList -> "Remote Control Relay Servers"
    M.RelayAdd -> if D.s "id" record == "" then "Add your own relay" else "Edit relay"
    M.RelaySwitch -> if D.str (D.at ["change","action"] record) == "save" then "Change connection?" else "Switch server?"
    M.RelayTerms -> "Use beta relay?"
    M.Installation -> "Add to Home Screen"
    M.Notice -> "Connection notice"
  fields = case kind of
    M.AgentSettings -> D.merge record $ D.json {mode: D.at ["orchestration",D.s "id" record,"execution","mode"] m.snapshot, manager: D.at ["orchestration",D.s "id" record,"manager"] m.snapshot}
    M.GroupSettings -> record
    M.GroupTask -> D.merge (D.json {assignee: D.get "lead" $ M.selected m, state: "backlog"}) record
    M.EditCorpora -> D.json {message: replaceFields (D.fallback (D.str $ D.at ["corpora","settings_template"] m.snapshot) $ D.s "settings_template" record) ["name","device","scope"]}
    M.RelayAdd -> D.json {name: D.get "name" record, connection: if D.valid $ D.get "connection" record then D.encode $ D.get "connection" record else ""}
    M.Notice -> D.json {message: record}
    _ -> D.empty
  replaceFields template keys = case A.uncons keys of
    Nothing -> template
    Just {head,tail} -> replaceFields (S.replaceAll (S.Pattern $ "{" <> head <> "}") (S.Replacement $ D.s head record) template) tail

view :: M.Model -> V.Html
view m = case m.dialog of
  Nothing -> V.node "dialog" [V.id "modal"] []
  Just d -> V.node "dialog" [V.id $ if M.corporaDialog d.kind then "corpora-dialog" else "modal",V.cls $ if d.kind == M.AppMenu then "app-menu-sheet" else if d.kind == M.Onboarding then "onboarding" else "",V.attr "aria-labelledby" "dialog-title",V.on "cancel" \e -> Link e CloseDialog]
    [V.div (if M.corporaDialog d.kind then "corpora-menu-heading" else "modal-top")
      [V.when (d.kind == M.Onboarding) $ V.div "setup-wordmark" [V.span [V.cls "setup-mark",V.attr "aria-hidden" "true"] "✳",V.span [] "sapiens"]
      ,V.when (d.kind /= M.Onboarding) $ V.span [V.cls "tiny-label"] "SAPIENS4"
      ,V.when (d.kind == M.Onboarding) $ V.span [V.cls "setup-kicker"] "SETUP"
      ,V.when (dismissible d) $ V.icon (if M.corporaDialog d.kind then "Close" else "Close dialog") "×" CloseDialog]
    ,V.when (d.kind == M.Onboarding) $ V.node "div" [V.cls "setup-progress",V.attr "aria-hidden" "true"] [V.div ("setup-progress-fill setup-progress-" <> D.s "step" d.content) []]
    ,V.node "div" [V.id $ if M.corporaDialog d.kind then "corpora-dialog-body" else "modal-content"] [V.when (A.elem d.kind [M.AIProviders,M.Onboarding] && d.providerPage /= M.Providers && not (authenticating d)) $ V.control [V.cls "ai-provider-back"] "‹  AI Connections" $ ProviderNavigate M.Providers,V.node "h2" [V.id "dialog-title",V.attr "tabindex" "-1",V.cls (if A.elem d.kind [M.AIProviders,M.Onboarding] then "provider-title" else "")] ((if A.elem d.kind [M.AIProviders,M.Onboarding] && d.providerPage /= M.Providers then [providerIcon "codex"] else []) <> [V.text $ dialogTitle d]),body m d,V.textNode "p" [V.cls "corpora-feedback",V.attr "role" "status"] ((M.dialogError d))
      ,V.when (A.elem d.kind [M.Onboarding,M.AIProviders,M.Computer] && M.dialogError d /= "") $ V.button "button" "Check again" $ OnboardingPoll d.identity]]

authenticating :: M.Dialog -> Boolean
authenticating d = d.busy || A.elem (D.s "provider" d.content) ["signing_in","signing_out"]

dismissible :: M.Dialog -> Boolean
dismissible d
  | A.elem d.kind [M.AIProviders,M.Onboarding] && authenticating d = false
  | d.kind /= M.Onboarding = true
  | otherwise = D.b "completed" d.content

dialogTitle :: M.Dialog -> String
dialogTitle d
  | d.kind == M.Computer = if A.elem (D.s "computer_permission" d.content) ["not_requested","denied"] then "Computer access needed" else "Computer access"
  | d.kind == M.AIProviders = case d.providerPage of
      M.Providers -> "Your AI lineup"
      M.CodexAuth -> connectionTitle d.content
      _ -> "Codex"
  | d.kind == M.Onboarding = case D.s "step" d.content of
      "permissions" -> "Computer access"
      "corpora" -> "Choose your Corpora"
      "ready" -> "You're ready"
      _ -> case d.providerPage of
        M.Providers -> "Connect your AI"
        M.CodexAuth -> connectionTitle d.content
        _ -> "Codex"
  | otherwise = d.title

connectionTitle :: Json -> String
connectionTitle value = case D.s "provider" value of
  "missing" -> "Codex isn't available"
  "unavailable" -> "Couldn't check Codex"
  _ -> if A.elem (D.s "state" (D.get "login" value)) ["failed","expired"] then "Couldn't connect Codex" else "Connecting Codex"

body :: M.Model -> M.Dialog -> V.Html
body m d = case d.kind of
  M.AppMenu -> V.node "nav" [V.label "Corpora menu"]
    [V.button "app-menu-row" "Workspace & settings" $ OpenDialog M.WorkspaceSettings D.empty
    ,V.when m.localAvailable $ V.button "app-menu-row" "AI Connections" $ OpenDialog M.AIProviders D.empty
    ,V.button "app-menu-row" "Shared computer" $ OpenDialog M.Computer D.empty]
  M.WorkspaceSettings -> V.div ""
    [V.p "" "Talk to your Sapis in chat, manage their notes, and save documents in their workspaces. Sapis can delegate requests through ChiefSapi."
    ,V.p "" "Use the corpora menu to switch workspaces. Active and Paused describe execution on the host; Connecting means this device is restoring its connection."
    ,V.when m.localAvailable $ V.button "button" "AI Connections" $ OpenDialog M.AIProviders D.empty
    ,V.button "button" "Edit decision prompts" $ OpenDialog M.DecisionPrompts D.empty
    ,V.when m.localAvailable $ V.button "button" "Computer access" $ OpenDialog M.Computer D.empty]
  M.AIProviders -> aiProvidersView d d.content
  M.Computer -> V.div ""
    [V.div "settings-row"
      [V.span [] (if (D.flag $ D.at ["computer","built"] m.snapshot) then "Computer helper is installed" else "Computer helper is missing. Reinstall Sapiens to repair it.")
      ,V.span [V.cls "tag"] (if owner == "" then "Available" else "In use · " <> (D.s "name" $ M.agent m owner))]
    ,V.p "" (if windows then "Windows UI Automation works with accessible apps. Elevated apps and secure system dialogs cannot be controlled." else "Computer control is optional. Chat does not need Accessibility access.")
    ,computerAccess m d d.content]
  M.Onboarding -> onboardingView m d d.content
  M.AgentSettings -> form "Save"
    [V.node "div" [V.cls "settings-tabs",V.attr "role" "tablist",V.label "Sapi settings"] $ map (\caption -> V.textNode "button" [V.attr "type" "button",V.attr "role" "tab",V.attr "aria-selected" $ show $ caption == "Profile",V.disabled $ caption /= "Profile"] (caption)) ["Profile","Context","Usage","Limits"]
    ,input "Name" "name" [V.bool "required" true,V.attr "maxlength" "24",V.attr "aria-describedby" "name-help",V.attr "autocomplete" "off"]
    ,V.node "div" [V.cls "name-suggestions",V.id "name-help",V.label "Suggested names"] $ map (\name -> V.button "button" name $ Field "name" name) suggestions <> [V.icon "More name suggestions" "↻" $ DialogCommand "names" D.empty]
    ,input "Role" "role" [V.bool "required" true,V.attr "maxlength" "60"]
    ,if D.s "id" d.record == D.s "main_agent_id" m.snapshot then V.div "settings-row" [V.text "Main orchestrator"] else select "Manager" "manager" $ map (\a -> Tuple (D.s "id" a) (D.s "name" a <> if D.s "id" a == D.s "main_agent_id" m.snapshot then " · Main orchestrator" else "")) managers
    ,select "Work mode" "mode" [Tuple "normal" "Normal · high",Tuple "deep" "Deep work · xhigh"]
    ,V.p "form-hint" $ "Each call has a " <> show (D.n "timeout_seconds" execution / 60.0) <> "-minute limit in either mode."]
  M.GroupSettings -> form "Save Group"
    [input "Name" "name" [V.bool "required" true,V.attr "maxlength" "60"],textarea "Purpose" "description" 2 [V.attr "maxlength" "2000"]
    ,select "Lead" "lead" $ map (\a -> Tuple (D.s "id" a) $ D.s "name" a) sapis
    ,V.node "fieldset" [V.cls "group-members"] $ [V.textNode "legend" [] "Members · at least two, including the Lead"] <> map (\a -> V.node "label" [] [V.node "input" [V.attr "type" "checkbox",V.attr "name" "member",V.value $ D.s "id" a,V.bool "checked" $ D.has (D.s "id" a) (D.get "members" d.fields),V.check $ CheckField "members" $ D.s "id" a] [],T.avatar a "mini" false,V.span [] (D.s "name" a)]) sapis
    ,V.p "form-hint" "Each Sapi can belong to up to 11 active Groups."
    ,V.button "button" (if D.b "archived" d.record then "Restore Group" else "Archive Group") $ Mutate ("/api/groups/" <> D.s "id" d.record) "PUT" (D.json {revision: D.get "revision" d.record, archived: not $ D.b "archived" d.record}) (CloseAndRefresh "Group saved.")]
  M.Attach -> V.div "attachment-menu" $ map (\(Tuple key caption) -> V.button "button" caption $ AttachmentKind key) [Tuple "image" "Image",Tuple "document" "Document",Tuple "link" "Link",Tuple "filepath" "Filepath"]
  M.Reference kind -> form "Attach" [input (if kind == "link" then "Link" else "Local filepath") "value" [V.attr "type" $ if kind == "link" then "url" else "text",V.attr "placeholder" $ if kind == "link" then "https://…" else "/Users/…/file.pdf",V.bool "required" true,V.attr "maxlength" "4096"]]
  M.DecisionPrompts -> if not (D.valid d.content) then V.p "" "Loading…" else form "Save prompt" [select "Node" "node" $ map (\p -> Tuple (D.s "node" p) $ D.s "node" p) $ D.rows "prompts" d.content,V.textNode "small" [V.cls "prompt-path"] (field "path"),textarea "Prompt" "content" 16 [V.bool "required" true,V.attr "maxlength" "20000"],V.p "" "Changes apply to subsequent decisions. Saved decision records keep their original prompt."]
  M.GroupTask -> form (if D.b "deleted" d.record then "Restore task" else "Save task")
    [input "Title" "title" [V.bool "required" true,V.attr "maxlength" "500"],textarea "Work to do" "body" 5 [V.attr "maxlength" "16000"]
    ,select "Assignee" "assignee" $ map (\id -> Tuple id $ D.s "name" $ M.agent m id) $ D.strings $ D.get "members" $ M.selected m
    ,V.when (D.s "id" d.record /= "") $ select "Status" "state" $ map (\state -> Tuple state $ W.taskState state) ["backlog","in_progress","done"]
    ,V.when (D.s "id" d.record /= "" && not (D.b "deleted" d.record)) $ V.button "button" "Delete task" $ DialogCommand "delete-task" D.empty
    ,V.p "form-hint" "Saving keeps this as shared work. Use Run task to call the assignee. Deletions can be restored."]
  M.CreateCorpora -> form "Create" [input "Corpora name" "name" [V.bool "required" true,V.attr "maxlength" "200"],textarea "Scope of work" "scope" 4 [V.attr "maxlength" "16000"]]
  M.EditCorpora -> form "Send to ChiefSapi ↑" [textarea "Message to ChiefSapi" "message" 8 [V.bool "required" true]]
  M.ConnectRemote -> V.div ""
    [V.p "" "On the host, open the corpora menu and choose New Pairing in its Remote column."
    ,V.node "section" [V.id "pair-panel",V.hidden $ field "code" /= ""]
      [V.div "" [V.control [V.id "scan-qr",V.disabled $ d.busy || D.b "scanning" d.fields] "Scan QR" (ScanQR),V.button "qr-upload" "Upload QR" UploadQR]
      ,V.node "div" [V.id "scanner",V.hidden $ not $ D.b "scanning" d.fields] [V.node "video" [V.id "qr-camera",V.bool "autoplay" true,V.bool "playsInline" true,V.bool "muted" true] [],V.button "" "Cancel scan" StopScan]
      ,V.node "form" [V.id "paste-invitation",V.submit Pair] [textarea "Or paste an invitation" "invitation" 2 [V.bool "required" true,V.bool "spellcheck" false,V.attr "autocomplete" "off"],V.textNode "button" [V.attr "type" "submit",V.disabled d.busy] "Connect"]]
    ,V.node "section" [V.id "verify-panel",V.hidden $ field "code" == ""] [V.p "" "Approve this matching code on the host.",V.textNode "strong" [V.id "confirmation-code"] (field "code")]]
  M.HostPairing -> hostPairing m d
  M.RelayList -> relayList d
  M.RelayAdd -> form "Save" [V.p "" "",V.link "https://github.com/aleski-green/sapiens4/blob/codex/remote-control-mvp/documentation/specs/RemoteMVP.md" "Deploy on GitHub ↗",input "Name" "name" [V.bool "required" true,V.attr "maxlength" "200",V.attr "autocomplete" "off"],textarea "Connection JSON" "connection" 6 [V.bool "required" true,V.bool "spellcheck" false,V.attr "maxlength" "4096",V.attr "placeholder" "Paste your relay.connection.json"],V.button "qr-upload" "Import JSON" $ DialogCommand "relay-import" D.empty,V.p "relay-private" "Stored privately on this desktop.",V.button "" "Cancel" $ DialogCommand "relay-back" D.empty]
  M.RelaySwitch -> V.div "" [V.node "dl" [V.cls "pairing-details"] [V.textNode "dt" [] "From",V.textNode "dd" [] (D.s "from" d.record),V.textNode "dt" [] "To",V.textNode "dd" [] (D.s "to" d.record)],V.p "" "Switching revokes all paired devices and remote corpora connections for this local app.",V.p "" "Pair all devices and reconnect all remote corpora again.",V.div "pairing-actions" [V.button "" "Cancel" $ DialogCommand "relay-back" D.empty,V.control [V.disabled d.busy] "Switch & revoke all" (DialogCommand "relay-confirm" D.empty)]]
  M.RelayTerms -> V.div "" [V.p "" "For sensitive work, use your own securely maintained relay.",V.p "" "Provided “as is,” without warranties. Use at your own risk.",V.p "" "To the extent permitted by law, authors, contributors, and operators disclaim liability for losses or damages from this service.",V.p "" "Non-excludable rights and liabilities remain unaffected.",V.node "label" [V.cls "relay-check"] [V.node "input" [V.attr "type" "checkbox",V.bool "checked" $ field "accepted" == "true",V.check $ \b -> Field "accepted" $ show b] [],V.text "I accept these beta terms."],V.div "pairing-actions" [V.button "" "Back" $ DialogCommand "relay-back" D.empty,V.button "" "Use my relay" $ DialogCommand "relay-add" D.empty,V.control [V.disabled $ field "accepted" /= "true" || d.busy] "Use beta" (DialogCommand "relay-accept" D.empty)]]
  M.Installation -> V.div "" [V.node "ol" [] $ map (\s -> V.textNode "li" [] (s)) (if ios then ["In Safari, tap Share.","Add to Home Screen. Open as Web App on → Add.","Open Sapiens4 from Home Screen."] else ["In Chrome, tap ⋮.","Install and create shortcut. Then tap Install.","Open Sapiens4 from Home Screen."]),V.button "" "Install Sapiens4" $ DialogCommand "install-prompt" D.empty,V.link "https://sapiens4.ai/help?q=add2homescreen" "Need help? ↗"]
  M.Notice -> V.div "" [V.p "" $ field "message",V.button "" "OK" CloseDialog]
  where
  field key = D.s key d.fields
  input caption key props = V.field "input" caption key (field key) props []
  textarea caption key rows props = V.field "textarea" caption key (field key) ([V.attr "rows" $ show rows] <> props) []
  select caption key options = V.field "select" caption key (field key) [] $ map (\(Tuple value caption_) -> V.textNode "option" [V.value value,V.bool "selected" $ value == field key] caption_) options
  form caption children = V.node "form" [V.cls "form-stack",V.submit SubmitDialog] $ children <> [V.textNode "button" [V.attr "type" "submit",V.cls "button primary",V.disabled d.busy] (caption)]
  owner = D.str $ D.at ["computer","owner"] m.snapshot
  windows = D.str (D.at ["computer","platform"] m.snapshot) == "win32"
  execution = D.at ["orchestration",D.s "id" d.record,"execution"] m.snapshot
  sapis = A.filter (\a -> not (M.isGroup a || D.b "retired" a)) m.agents
  managers = A.filter (\a -> allowed [] $ D.s "id" a) sapis
  allowed seen id = id == "" || id /= D.s "id" d.record && not (A.elem id seen) && allowed (A.cons id seen) (D.str $ D.at ["orchestration",id,"manager"] m.snapshot)
  pools = [["Armin","Howl","Seneca","Socrates","Aesop","Nikola","Walt","Akira","Milo"],["Kiki","Chihiro","Sophie","Alice","Hypatia","Simone","Ada","Oprah","Arianna"],["Sage","River","Rowan","Sky","Nova","Phoenix","Ash","Robin","Aster"]]
  suggestions = map (\pool -> let available = A.filter (\name -> not $ A.any (( _ == S.toLower name) <<< S.toLower <<< D.s "name") m.agents) pool in fromMaybe "Sage" $ A.index (if A.null available then map (_ <> "-" <> show d.identity) pool else available) (mod d.identity $ max 1 $ A.length available)) pools
  ios = N.ios m.env

relayList :: M.Dialog -> V.Html
relayList d = if not (D.valid d.content) then V.p "" "Loading…" else V.div ""
  [V.node "div" [V.id "relay-list",V.cls $ "relay-list " <> if A.length rows >= 3 then "relay-scroll" else "",V.attr "role" "group",V.label "Relay servers"] $ map row rows
  ,V.node "label" [V.cls "relay-check"] [V.node "input" [V.attr "type" "checkbox",V.bool "checked" archived,V.check $ \b -> Field "archived" $ show b] [],V.text "Show archived"]
  ,V.button "relay-add" "+ Add your own relay" $ DialogCommand "relay-add" D.empty,V.textNode "small" [] "Recommended",V.p "relay-warning" "ⓘ Beta: use at your own risk. Self-host for sensitive work.",V.button "" (if D.s "key" d.record /= "" then "Continue" else "Done") $ DialogCommand "relay-done" D.empty]
  where
  archived = D.s "archived" d.fields == "true"
  rows = A.filter (\r -> archived || not (D.b "archived" r)) $ D.rows "servers" d.content
  row r = let current = D.s "id" r == D.s "selected" d.content in V.node "div" [V.cls "relay-row",V.attr "data-current" $ show current]
    [V.node "label" [] [V.node "input" [V.attr "type" "radio",V.attr "name" "relay-server",V.value $ D.s "id" r,V.bool "checked" current,V.disabled $ D.b "archived" r || not (D.b "available" r),V.check $ \_ -> DialogCommand "relay-select" r] [],V.node "span" [] [V.textNode "strong" [] (D.s "name" r),V.textNode "small" [] (D.replace "^https?://" "" $ D.s "address" r)]]
    ,V.span [V.cls "relay-badge"] (if D.b "archived" r then "Archived" else if not (D.b "available" r) then "Inactive" else if current then "Current" else "")
    ,V.div "relay-row-actions" [V.button "" "Edit" $ DialogCommand "relay-edit" r,V.button "" (if D.b "archived" r then "Restore" else "Archive") $ DialogCommand "relay-archive" r]]

hostPairing :: M.Model -> M.Dialog -> V.Html
hostPairing m d = if not (D.valid data_) then V.p "" "Loading…" else V.div ""
  [if D.valid (D.get "available" data_) && not (D.b "available" data_) then V.p "" "Remote control is unavailable. Install the remote dependencies and restart Sapiens4."
   else if D.b "paired" data_ then V.div "" [V.p "pairing-corpora" $ D.s "name" d.record,V.node "dl" [V.cls "pairing-details"] $ A.concatMap (\(Tuple label text) -> [V.textNode "dt" [] (label),V.textNode "dd" [] (text)]) [Tuple "Device" $ D.fallback "Not recorded" $ D.str $ D.at ["device_info","name"] data_,Tuple "Device info" $ D.fallback "Not recorded" $ S.joinWith " · " $ A.filter (_ /= "") $ map (\key -> D.str $ D.at ["device_info",key] data_) ["platform","client"],Tuple "Paired" $ date "paired_at",Tuple "Last connected" $ date "last_connected_at"],V.div "pairing-actions" [action "replace" "Replace remote",action "revoke" "Revoke remote"]]
   else if D.valid $ D.get "pending" data_ then V.div "" [V.p "" "Approve only if this code matches your remote device.",V.textNode "strong" [] (D.str $ D.at ["pending","confirmation_code"] data_),V.control [V.disabled $ D.str (D.at ["pending","confirmation_code"] data_) == "" || d.busy] "Approve device" (DialogCommand "host-approve" $ D.get "pending" data_)]
   else if D.b "enabled" data_ && D.n "expires" invitation > m.now/1000.0 then V.div "" [V.p "" "In the installed app, choose Connect remote corpora and scan this QR.",V.node "img" [V.id "host-pair-qr",V.attr "alt" "Pairing QR",V.attr "src" $ D.s "qr" d.fields] [],V.p "" $ "Expires " <> B.formatDate "" D.empty (D.s "expiration" d.fields),V.div "pairing-actions" [V.button "" "Show link" $ Field "showLink" "true",V.textNode "a" [V.id "download-pair-qr",V.attr "download" "sapiens4-pairing.png",V.attr "href" $ D.s "qr" d.fields] "Download QR",V.link (D.s "relay" invitation) "Open app ↗",action "revoke" "Cancel invitation"],V.node "input" [V.id "host-pair-link",V.label "Pairing link",V.bool "readOnly" true,V.hidden $ D.s "showLink" d.fields /= "true",V.value $ Remote.invitationLink invitation] []]
   else if D.b "enabled" data_ then V.div "" [V.p "" "Invitation expired.",action "revoke" "Cancel invitation"]
   else V.div "pairing-actions" [action "pair" "Show QR",V.button "" "Relay servers" $ OpenDialog M.RelayList d.record]
  ,V.when (D.s "error" data_ /= "") $ V.p "" $ D.s "error" data_]
  where
  data_ = d.content
  invitation = D.get "invitation" data_
  date key = D.fallback "Not recorded" $ D.str $ D.at ["dates",key] d.fields
  action key label = V.control [V.disabled d.busy] (label) (DialogCommand ("host-" <> key) D.empty)

setupButton :: M.Dialog -> String -> String -> V.Html
setupButton d name caption = V.control [V.cls "button",V.disabled $ M.dialogBusy d] caption $ DialogCommand "setup" (D.text name)

signInView :: Json -> V.Html
signInView value = V.div "account-sign-in" $
  [V.textNode "p" [V.attr "role" "status"] $ if signing then "Finish signing in with ChatGPT in your browser. Waiting for sign-in…" else if provider == "sign_in_required" && not (A.elem state ["failed","expired"]) then "Finish signing in with ChatGPT in your browser." else ""
  ,V.when (provider == "missing") $ V.p "" "The Codex installation is missing or couldn't start. Reinstall Sapiens if the problem persists."
  ,V.when (provider == "unavailable") $ V.p "" "Your account may still be connected. Try checking again."
  ,V.when (state == "failed" || state == "expired" || state == "browser_busy" || state == "logout_failed" || state == "cancelled") $ V.p "" $ case state of
    "expired" -> "Sign-in expired. Try again or use a one-time code."
    "browser_busy" -> "Another sign-in is using the browser callback. Finish it and try again, or use a one-time code."
    "logout_failed" -> "We couldn’t finish signing out. Recheck your connection, then try signing out again."
    "cancelled" -> "Sign-in cancelled. You can try again when you’re ready."
    _ -> "Sign-in didn't finish. Try again or use a one-time code."
  ,V.when (provider == "sign_in_required") $ V.button "button primary" (if state == "failed" || state == "expired" then "Try again" else "Connect") $ DialogCommand "setup" (D.text "login")
  ,V.when (provider == "sign_in_required") $ V.p "form-hint" "Your browser will open to complete sign-in."
  ,V.when (signing && D.s "url" login /= "") $ V.div "account-actions"
    [V.button "button" (if D.s "code" login == "" then "Open browser again" else "Open sign-in page") $ DialogCommand "setup" (D.text "open-login")
    ,V.button "button" "Copy sign-in link" $ Copy (D.s "url" login) "Sign-in link copied."]
  ,V.when (D.s "code" login /= "") $ V.div "account-device-code"
    [V.p "" "Enter this one-time code on the official OpenAI page. Only continue if you started this sign-in in Sapiens."
    ,V.textNode "strong" [] (D.s "code" login)]
  ,V.when signing $ V.control [V.cls "button",V.id "cancel-codex-login"] "Cancel" $ DialogCommand "setup" (D.text "cancel")
  ,V.when (not signing && provider /= "missing") $ V.button "button" "Check again" $ DialogCommand "setup" (D.text "refresh")
  ,V.when (provider == "sign_in_required") $ V.node "details" [V.cls "account-help"]
    [V.textNode "summary" [] "Having trouble signing in?"
    ,V.p "form-hint" "You can connect the same ChatGPT account using a one-time code. Device code login must be enabled in your ChatGPT security settings."
    ,V.button "button" "Use a one-time code" $ DialogCommand "setup" (D.text "device-login")]]
  where
  provider = D.s "provider" value
  login = D.get "login" value
  state = D.s "state" login
  signing = provider == "signing_in"

accountView :: M.Dialog -> V.Html
accountView d = V.node "section" [V.cls "ai-provider-detail-body",V.label "Codex account"] $
  let value = d.content
  in
      let provider = D.s "provider" value
          state = D.s "state" (D.get "login" value)
          confirmation = A.elem d.providerPage [M.ConfirmChange,M.ConfirmDisconnect]
      in if provider == "signing_out" then [V.textNode "p" [V.attr "role" "status"] "Signing out…"]
      else if confirmation then
        [V.textNode "h4" [] (if d.providerPage == M.ConfirmChange then "Change Codex account?" else "Disconnect Codex?")
        ,V.p "" (if d.providerPage == M.ConfirmChange then "We'll sign out of your current account first. If you cancel the new sign-in, Codex will stay disconnected." else "Your Sapis won't be able to use Codex until you reconnect. Your chats and settings will stay on this device.")
        ,V.div "account-actions" [setupButton d (if d.providerPage == M.ConfirmChange then "change-account" else "sign-out") (if d.providerPage == M.ConfirmChange then "Continue" else "Disconnect"),V.control [V.cls "button",V.disabled $ M.dialogBusy d] "Cancel" $ ProviderNavigate M.CodexDetails]]
      else if provider == "signed_in" then
        [V.textNode "p" [V.cls "account-connected",V.attr "role" "status"] "Connected"
        ,V.when (D.s "hold" value == "review") $ V.div ""
          [V.p "form-hint" "AI work is paused. Review any pending tasks before continuing with this account."
          ,V.control [V.cls "button",V.id "resume-ai-work",V.disabled $ M.dialogBusy d] "Resume AI work" $ DialogCommand "setup" (D.text "resume")]
        ,V.when (D.s "hold" value /= "" && D.s "hold" value /= "review") $ V.p "" "The last account change was interrupted. Sign out and reconnect before resuming AI work."
        ,V.div "ai-provider-actions"
          [V.control [V.cls "ai-provider-action",V.disabled $ M.dialogBusy d] "Change account  ›" $ ProviderNavigate M.ConfirmChange
          ,V.control [V.cls "ai-provider-action ai-provider-action-danger",V.disabled $ M.dialogBusy d] "Disconnect  ›" $ ProviderNavigate M.ConfirmDisconnect]]
      else [V.when (provider /= "signing_in") $ V.p "" (if provider == "unavailable" then "Couldn't check Codex" else if provider == "missing" then "Codex isn't available" else "Not connected")
           ,signInView value
           ,V.when (provider == "unavailable" || state == "logout_failed") $ V.control [V.cls "button",V.disabled $ M.dialogBusy d] "Sign out" $ ProviderNavigate M.ConfirmDisconnect]


providerIcon :: String -> V.Html
providerIcon slug = V.node "img" [V.cls "ai-provider-icon",V.attr "src" ("/ai-" <> slug <> ".svg"),V.attr "alt" "",V.attr "aria-hidden" "true"] []

providerCopy :: String -> String -> V.Html
providerCopy caption status = V.div "ai-provider-copy"
  [V.textNode "strong" [] caption,V.span [V.cls "ai-provider-status"] status]

providerRow :: String -> String -> String -> V.Html -> V.Html
providerRow slug caption status action = V.div "ai-provider-row"
  [providerIcon slug,providerCopy caption status,action]

providerLink :: String -> String -> String -> Action -> V.Html
providerLink slug caption status action = V.controlWith [V.cls "ai-provider-row ai-provider-link",V.label $ "Manage " <> caption]
  [providerIcon slug,providerCopy caption status,V.span [V.cls "ai-provider-arrow",V.attr "aria-hidden" "true"] "›"] action


comingSoonProviders :: Array V.Html
comingSoonProviders =
  [providerRow "claude" "Claude Code" "Anthropic" (V.span [V.cls "ai-provider-soon"] "Coming soon")
  ,providerRow "gemini" "Gemini CLI" "Google" (V.span [V.cls "ai-provider-soon"] "Coming soon")
  ,providerRow "kimi" "Kimi Code" "Moonshot AI" (V.span [V.cls "ai-provider-soon"] "Coming soon")]

aiProvidersView :: M.Dialog -> Json -> V.Html
aiProvidersView d value =
  let provider = D.s "provider" value
      hold = D.s "hold" value
      page = d.providerPage
      details = A.elem page [M.CodexDetails,M.ConfirmChange,M.ConfirmDisconnect]
      inAuth = page == M.CodexAuth
      needsAttention = A.elem (D.s "state" (D.get "login" value)) ["failed","expired","browser_busy","logout_failed"]
      status = case provider of
        "signed_in" -> if hold == "review" then "Connected" else if hold /= "" then "Needs attention" else "Connected"
        "signing_in" -> "Connecting…"
        "signing_out" -> "Disconnecting…"
        "missing" -> "Needs attention"
        "unavailable" -> "Needs attention"
        _ -> if needsAttention then "Needs attention" else "Not connected"
      navigate = ProviderNavigate M.CodexDetails
      codex = case provider of
        "sign_in_required" -> if needsAttention then providerLink "codex" "Codex" status navigate else providerRow "codex" "Codex" status (setupButton d "login" "Connect")
        "signed_in" -> providerLink "codex" "Codex" status navigate
        "signing_in" -> providerLink "codex" "Codex" status (ProviderNavigate M.CodexAuth)
        "signing_out" -> providerLink "codex" "Codex" status (ProviderNavigate M.CodexAuth)
        _ -> providerLink "codex" "Codex" status navigate
  in V.div "ai-providers" $ if inAuth then
    [if provider == "signing_out" then V.p "form-hint" "Signing out of the previous account…" else signInView value
    ,V.when (A.elem provider ["signing_in","signing_out"]) $ V.p "form-hint" "Stay here until sign-in finishes, or cancel sign-in to leave."]
  else if details then [accountView d]
  else
    [V.p "form-hint" "Connect the AI tools your Sapis can use."
    ,V.div "ai-provider-list" $ [codex] <> comingSoonProviders]


onboardingCorpora :: M.Model -> Array Json
onboardingCorpora m = A.filter (\r -> D.s "location" r == "Local" && D.s "status" r /= "archived") m.records

startingCorpora :: M.Model -> Json -> Maybe Json
startingCorpora m value = A.find (( _ == D.s "selected_corpora" value) <<< D.s "id") (onboardingCorpora m)

onboardingConnected :: M.Model -> Boolean
onboardingConnected m = m.online && D.str (D.at ["corpora","id"] m.snapshot) == D.s "id" (M.activeRecord m) && A.any (( _ == m.active) <<< D.s "key") (onboardingCorpora m)

onboardingView :: M.Model -> M.Dialog -> Json -> V.Html
onboardingView m d value = V.div "setup-screen" $
  [V.when (not (onboardingConnected m) && A.elem step ["permissions","corpora","ready"]) $ V.textNode "p" [V.attr "role" "status"] (D.fallback "Opening your Corpora…" m.feedback)] <> case step of
  "permissions" ->
    [V.p "setup-intro" "Computer control is optional. Chat works without it."
    ,computerAccess m d value]
  "corpora" ->
    [V.p "setup-intro" "Choose the Corpora you'd like to open first."
    ,V.div "setup-corpora-list" $ map (\r -> V.control [V.cls "setup-corpora-choice",V.disabled $ m.busy,V.attr "aria-pressed" $ show $ (D.s "key" r) == m.active] (D.s "name" r) (EnterCorpora (D.s "key" r))) (onboardingCorpora m)
    ,V.div "setup-footer" [V.control [V.cls "button primary",V.disabled $ M.dialogBusy d || m.busy || not (onboardingConnected m)] "Continue" $ DialogCommand "setup" (D.text "corpora-done")]]
  "ready" ->
    [V.p "setup-intro" "Your Sapiens workspace is ready to use."
    ,V.div "setup-summary"
      [V.div "setup-summary-row" [V.span [V.cls "setup-summary-label"] "AI connection",V.span [] (if D.s "provider" value == "signed_in" then "Codex connected" else "Not connected")]
      ,V.div "setup-summary-row" [V.span [V.cls "setup-summary-label"] "Computer access",V.span [] (case permission of
        "granted" -> "Enabled"
        "not_required" -> "Available for accessible apps"
        "missing" -> "Helper unavailable"
        _ -> "Not enabled")]
      ,V.div "setup-summary-row" [V.span [V.cls "setup-summary-label"] "Corpora",V.span [] (D.s "name" $ M.activeRecord m)]
      ,V.div "setup-summary-divider" []
      ,V.div "setup-summary-row" [V.span [V.cls "setup-summary-label"] "Remote Control",V.span [V.cls "setup-muted"] "Available in Settings"]
      ,V.div "setup-summary-row" [V.span [V.cls "setup-summary-label"] "Analytics",V.span [V.cls "setup-muted"] "Coming soon"]]
    ,V.div "setup-footer" [V.control [V.cls "button primary",V.disabled $ M.dialogBusy d || m.busy || not (onboardingConnected m)] (if M.dialogBusy d then "Opening Sapiens…" else "Open Sapiens") $ DialogCommand "setup" (D.text "finish")]]
  _ ->
    [aiProvidersView d value
    ,V.when (A.elem d.providerPage [M.Providers,M.CodexDetails] && D.b "ready" value) $ V.div "setup-footer" [V.control [V.cls "button primary",V.disabled $ M.dialogBusy d] "Continue" $ DialogCommand "setup" (D.text "provider-next")]]
  where
  permission = D.s "computer_permission" value
  step = D.s "step" value

computerAccess :: M.Model -> M.Dialog -> Json -> V.Html
computerAccess m d value = V.div "setup-screen"
  [V.p "" $ case permission of
    "missing" -> "Computer helper missing. Reinstall Sapiens to repair it."
    "granted" -> "Computer control enabled."
    "denied" -> "To interact with apps on your Mac, Sapiens needs Accessibility permission."
    "not_required" -> "Computer control works with accessible Windows apps."
    _ -> "Couldn't check Accessibility. Check again or open System Settings."
  ,V.node "div" [V.cls "actions",V.style "margin-top:24px"]
    [V.when (A.elem permission ["not_requested","denied","unavailable"]) $ setupButton d "permission" "Enable access"
    ,V.when (permission /= "missing" && permission /= "not_required") $ setupButton d "refresh" "Check again"
    ,if d.kind == M.Onboarding then
      V.control [V.cls "button primary",V.style "margin-left:auto",V.disabled $ M.dialogBusy d || A.length (onboardingCorpora m) <= 1 && not (onboardingConnected m)] (if A.elem permission ["granted","not_required"] then "Continue" else "Not now") $ DialogCommand "setup" (D.text "permissions-done")
     else V.control [V.cls "button",V.style "margin-left:auto"] (if A.elem permission ["not_requested","denied","unavailable"] then "Not now" else "Done") CloseDialog]]
  where permission = D.s "computer_permission" value
