module Sapiens.Dialog where

import Sapiens.Form as F
import Prelude
import Data.Argonaut.Core (Json)
import Data.Array as A
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String as S
import Data.Tuple (Tuple(..))
import Foreign.Object as O
import Libs4.Browser as B
import Sapiens.Action as Intent
import Sapiens.Action (Action(..), Success(..))
import Sapiens.Data as D
import Sapiens.Domain as X
import Sapiens.Html as V
import Sapiens.Model as M
import Sapiens.Navigation as N
import Sapiens.Remote as Remote
import Sapiens.Text as T
import Sapiens.Work as W

nameHelp :: String
nameHelp = "Start with A–Z. Letters, numbers, and - _ . : # + | ( ) & $ ^ are allowed. No spaces."

make :: M.DialogKind -> F.Context -> M.Model -> M.Dialog
make kind record m = {storage:M.emptyStorage,providerPage:M.Providers,kind,title,record,fields,content:F.NoContent,status:M.Editing,identity:m.sequence+1}
  where
  name = F.contextName record
  title = case kind of
    M.AppMenu -> D.fallback "CORPORA" $ m.snapshot.corpora.name
    M.WorkspaceSettings -> "Workspace & settings"
    M.AIProviders -> "AI Connections"
    M.Computer -> "Shared computer"
    M.Onboarding -> "Set up Sapiens"
    M.AgentSettings -> name <> " settings"
    M.GroupSettings -> name <> " settings"
    M.Attach -> "Attach"
    M.Reference X.LinkAttachment -> "Add link"
    M.Reference _ -> "Add filepath"
    M.DecisionPrompts -> "Decision prompts"
    M.GroupTask -> if F.contextId record == "" then "Create Group task" else "Edit Group task"
    M.CreateCorpora -> "Create local corpora"
    M.EditCorpora -> "Message to ChiefSapi"
    M.ConnectRemote -> "Connect remote corpora"
    M.HostPairing -> if (F.contextCorpora record).paired then "Edit Pairing" else "New Pairing"
    M.RelayList -> "Remote Control Relay Servers"
    M.RelayAdd -> if F.contextId record == "" then "Add your own relay" else "Edit relay"
    M.RelaySwitch -> if (F.contextChange record).saving then "Change connection?" else "Switch server?"
    M.RelayTerms -> "Use beta relay?"
    M.Installation -> "Add to Home Screen"
    M.Notice -> "Connection notice"
  fields = case kind of
    M.AgentSettings -> base {mode=(orchestration m $ F.contextId record).mode,manager=(orchestration m $ F.contextId record).manager}
    M.GroupTask -> case record of
      F.TaskRecord _ -> base
      _ -> base {assignee=(M.selected m).lead,state="backlog"}
    M.EditCorpora -> base {message=replaceFields (D.fallback m.snapshot.corpora.settings_template (F.contextCorpora record).settings_template) [Tuple "name" (F.contextCorpora record).name,Tuple "device" (F.contextCorpora record).device,Tuple "scope" (F.contextCorpora record).scope]}
    _ -> base
  base = F.fieldsFor record
  replaceFields template replacements = case A.uncons replacements of
    Nothing -> template
    Just {head:Tuple key value,tail} -> replaceFields (S.replaceAll (S.Pattern $ "{" <> key <> "}") (S.Replacement value) template) tail

view :: M.Model -> V.Html
view m = case m.dialog of
  Nothing -> V.node "dialog" [V.id "modal"] []
  Just d -> V.node "dialog" [V.id $ if M.corporaDialog d.kind then "corpora-dialog" else "modal",V.cls $ if d.kind == M.AppMenu then "app-menu-sheet" else if d.kind == M.Onboarding then "onboarding" else "",V.attr "aria-labelledby" "dialog-title",V.on "cancel" \e -> Link e CloseDialog]
    [V.div (if M.corporaDialog d.kind then "corpora-menu-heading" else "modal-top")
      [V.when (d.kind == M.Onboarding) $ V.div "setup-wordmark" [V.node "img" [V.cls "setup-mark",V.attr "src" (if m.theme == "dark" then "/favicon-dark.svg" else "/favicon.svg"),V.attr "alt" "",V.attr "aria-hidden" "true"] [],V.span [] "Sapiens4"]
      ,V.when (d.kind /= M.Onboarding) $ V.span [V.cls "tiny-label"] "SAPIENS4"
      ,V.when (d.kind == M.Onboarding) $ V.span [V.cls "setup-kicker"] "SETUP"
      ,V.when (dismissible d) $ V.icon (if M.corporaDialog d.kind then "Close" else "Close dialog") "×" CloseDialog]
    ,V.when (d.kind == M.Onboarding) $ V.node "div" [V.cls "setup-progress",V.attr "aria-hidden" "true"] [V.div ("setup-progress-fill setup-progress-" <> (if d.storage.page == M.SetupWelcome then "welcome" else if d.storage.page == M.ChooseStorage then "storage" else D.s "step" (F.setupValue d.content))) []]
    ,V.node "div" [V.id $ if M.corporaDialog d.kind then "corpora-dialog-body" else "modal-content"] [V.when (A.elem d.kind [M.AIProviders,M.Onboarding] && d.providerPage /= M.Providers && d.storage.page == M.SetupSteps && not (authenticating d)) $ V.control [V.cls "ai-provider-back"] "‹  AI Connections" $ ProviderNavigate M.Providers,V.node "h2" [V.id "dialog-title",V.attr "tabindex" "-1",V.cls (if A.elem d.kind [M.AIProviders,M.Onboarding] then "provider-title" else "")] ((if A.elem d.kind [M.AIProviders,M.Onboarding] && d.providerPage /= M.Providers && d.storage.page == M.SetupSteps then [providerIcon "codex"] else []) <> [V.text $ dialogTitle d]),body m d,V.textNode "p" [V.cls "corpora-feedback",V.attr "role" "status"] ((M.dialogError d))
      ,V.when (A.elem d.kind [M.Onboarding,M.AIProviders,M.Computer] && d.storage.page == M.SetupSteps && M.dialogError d /= "") $ V.button "button" "Check again" $ OnboardingPoll d.identity]]

authenticating :: M.Dialog -> Boolean
authenticating d = (M.dialogBusy d) || A.elem (D.s "provider" (F.setupValue d.content)) ["signing_in","signing_out"]

dismissible :: M.Dialog -> Boolean
dismissible d
  | A.elem d.kind [M.AIProviders,M.Onboarding] && authenticating d = false
  | d.kind /= M.Onboarding = true
  | otherwise = D.b "completed" (F.setupValue d.content)

dialogTitle :: M.Dialog -> String
dialogTitle d
  | d.kind == M.Computer = if A.elem (D.s "computer_permission" (F.setupValue d.content)) ["not_requested","denied"] then "Computer access needed" else "Computer access"
  | d.kind == M.AIProviders = case d.providerPage of
      M.Providers -> "Your AI lineup"
      M.CodexAuth -> connectionTitle (F.setupValue d.content)
      _ -> "Codex"
  | d.kind == M.Onboarding && d.storage.page == M.ChooseStorage = "Where should we start?"
  | d.kind == M.Onboarding && d.storage.page == M.SetupWelcome = "Meet your Sapis."
  | d.kind == M.Onboarding = case D.s "step" (F.setupValue d.content) of
      "permissions" -> "Computer access"
      "corpora" -> "Choose your Corpora"
      "ready" -> "You're ready"
      _ -> case d.providerPage of
        M.Providers -> "Connect your AI"
        M.CodexAuth -> connectionTitle (F.setupValue d.content)
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
    [V.button "app-menu-row" "Workspace & settings" $ OpenDialog M.WorkspaceSettings F.NoRecord
    ,V.when m.localAvailable $ V.button "app-menu-row" "AI Connections" $ OpenDialog M.AIProviders F.NoRecord
    ,V.button "app-menu-row" "Shared computer" $ OpenDialog M.Computer F.NoRecord]
  M.WorkspaceSettings -> V.div ""
    [V.p "" "Talk to your Sapis in chat, manage their notes, and save documents in their workspaces. Sapis can delegate requests through ChiefSapi."
    ,V.p "" "Use the corpora menu to switch workspaces. Active and Paused describe execution on the host; Connecting means this device is restoring its connection."
    ,V.when m.localAvailable $ V.button "button" "AI Connections" $ OpenDialog M.AIProviders F.NoRecord
    ,V.button "button" "Edit decision prompts" $ OpenDialog M.DecisionPrompts F.NoRecord
    ,V.when m.localAvailable $ V.button "button" "Computer access" $ OpenDialog M.Computer F.NoRecord]
  M.AIProviders -> aiProvidersView d (F.setupValue d.content)
  M.Computer | m.snapshot.computer.platform == "linux" -> linuxComputer m d
  M.Computer -> V.div ""
    [V.div "settings-row"
      [V.span [] (if m.snapshot.computer.built then "Computer helper is installed" else "Computer helper is missing. Reinstall Sapiens to repair it.")
      ,V.span [V.cls "tag"] (if owner == "" then "Available" else "In use · " <> (M.agent m owner).name)]
    ,V.p "" (if windows then "Windows UI Automation works with accessible apps. Elevated apps and secure system dialogs cannot be controlled." else "Computer control is optional. Chat does not need Accessibility access.")
    ,computerAccess d (F.setupValue d.content)]
  M.Onboarding -> onboardingView m d (F.setupValue d.content)
  M.AgentSettings -> form "Save"
    [V.node "div" [V.cls "settings-tabs",V.attr "role" "tablist",V.label "Sapi settings"] $ map (\caption -> V.textNode "button" [V.attr "type" "button",V.attr "role" "tab",V.attr "aria-selected" $ show $ caption == "Profile",V.disabled $ caption /= "Profile"] (caption)) ["Profile","Context","Usage","Limits"]
    ,input "Name" F.Name [V.bool "required" true,V.attr "maxlength" "24",V.attr "aria-describedby" "name-help",V.attr "autocomplete" "off"]
    ,V.node "div" [V.cls "name-suggestions",V.id "name-help",V.label "Suggested names"] $ map (\name -> V.button "button" name $ Field F.Name name) suggestions <> [V.icon "More name suggestions" "↻" $ DialogCommand Intent.SuggestNames]
    ,input "Role" F.Role [V.bool "required" true,V.attr "maxlength" "60"]
    ,if F.contextId d.record == m.snapshot.main_agent_id then V.div "settings-row" [V.text "Main orchestrator"] else select "Manager" F.Manager $ map (\a -> Tuple (a.id) (a.name <> if a.id == m.snapshot.main_agent_id then " · Main orchestrator" else "")) managers
    ,select "Work mode" F.Mode [Tuple "normal" "Normal · high",Tuple "deep" "Deep work · xhigh"]
    ,V.p "form-hint" $ "Each call has a " <> show (execution.timeout / 60.0) <> "-minute limit in either mode."]
  M.GroupSettings -> form "Save Group"
    [input "Name" F.Name [V.bool "required" true,V.attr "maxlength" "60"],textarea "Purpose" F.Description 2 [V.attr "maxlength" "2000"]
    ,select "Lead" F.Lead $ map (\a -> Tuple (a.id) $ a.name) sapis
    ,V.node "fieldset" [V.cls "group-members"] $ [V.textNode "legend" [] "Members · at least two, including the Lead"] <> map (\a -> V.node "label" [] [V.node "input" [V.attr "type" "checkbox",V.attr "name" "member",V.value $ a.id,V.bool "checked" $ A.elem a.id d.fields.members,V.check $ CheckField $ a.id] [],T.avatar a "mini" false,V.span [] (a.name)]) sapis
    ,V.p "form-hint" "Each Sapi can belong to up to 11 active Groups."
    ,V.button "button" (if F.contextArchived d.record then "Restore Group" else "Archive Group") $ Mutate ("/api/groups/" <> F.contextId d.record) "PUT" (D.json {revision: D.json $ F.contextRevision d.record, archived: not $ F.contextArchived d.record}) (CloseAndRefresh "Group saved.")]
  M.Attach -> V.div "attachment-menu" $ map (\(Tuple key caption) -> V.button "button" caption $ AttachmentKind key) [Tuple X.ImageAttachment "Image",Tuple X.DocumentAttachment "Document",Tuple X.LinkAttachment "Link",Tuple X.FileAttachment "Filepath"]
  M.Reference kind -> form "Attach" [input (if kind == X.LinkAttachment then "Link" else "Local filepath") F.Value [V.attr "type" $ if kind == X.LinkAttachment then "url" else "text",V.attr "placeholder" $ if kind == X.LinkAttachment then "https://…" else "/Users/…/file.pdf",V.bool "required" true,V.attr "maxlength" "4096"]]
  M.DecisionPrompts -> if d.content == F.NoContent then V.p "" "Loading…" else form "Save prompt" [select "Node" F.Node $ map (\p -> Tuple p.node p.node) $ F.prompts d.content,V.textNode "small" [V.cls "prompt-path"] (field F.Path),textarea "Prompt" F.Content 16 [V.bool "required" true,V.attr "maxlength" "20000"],V.p "" "Changes apply to subsequent decisions. Saved decision records keep their original prompt."]
  M.GroupTask -> form (if F.contextDeleted d.record then "Restore task" else "Save task")
    [input "Title" F.Title [V.bool "required" true,V.attr "maxlength" "500"],textarea "Work to do" F.Body 5 [V.attr "maxlength" "16000"]
    ,select "Assignee" F.Assignee $ map (\id -> Tuple id $ (M.agent m id).name) $ D.strings $ D.json (M.selected m).members
    ,V.when (F.contextId d.record /= "") $ select "Status" F.State $ map (\state -> Tuple state $ W.taskState $ X.groupState state) ["backlog","in_progress","done"]
    ,V.when (F.contextId d.record /= "" && not (F.contextDeleted d.record)) $ V.button "button" "Delete task" $ DialogCommand Intent.DeleteTask
    ,V.p "form-hint" "Saving keeps this as shared work. Use Run task to call the assignee. Deletions can be restored."]
  M.CreateCorpora -> form "Create" [input "Corpora name" F.Name [V.bool "required" true,V.attr "maxlength" "200"],textarea "Scope of work" F.Scope 4 [V.attr "maxlength" "16000"]]
  M.EditCorpora -> form "Send to ChiefSapi ↑" [textarea "Message to ChiefSapi" F.Message 8 [V.bool "required" true]]
  M.ConnectRemote -> V.div ""
    [V.p "" "On the host, open the corpora menu and choose New Pairing in its Remote column."
    ,V.node "section" [V.id "pair-panel",V.hidden $ field F.Code /= ""]
      [V.div "" [V.control [V.id "scan-qr",V.disabled $ (M.dialogBusy d) || d.fields.scanning] "Scan QR" (ScanQR),V.button "qr-upload" "Upload QR" UploadQR]
      ,V.node "div" [V.id "scanner",V.hidden $ not $ d.fields.scanning] [V.node "video" [V.id "qr-camera",V.bool "autoplay" true,V.bool "playsInline" true,V.bool "muted" true] [],V.button "" "Cancel scan" StopScan]
      ,V.node "form" [V.id "paste-invitation",V.submit Pair] [textarea "Or paste an invitation" F.Invitation 2 [V.bool "required" true,V.bool "spellcheck" false,V.attr "autocomplete" "off"],V.textNode "button" [V.attr "type" "submit",V.disabled (M.dialogBusy d)] "Connect"]]
    ,V.node "section" [V.id "verify-panel",V.hidden $ field F.Code == ""] [V.p "" "Approve this matching code on the host.",V.textNode "strong" [V.id "confirmation-code",V.label $ field F.Code] (field F.Code)]]
  M.HostPairing -> hostPairing m d
  M.RelayList -> relayList d
  M.RelayAdd -> form "Save" [V.p "" "",V.link "https://github.com/aleski-green/sapiens4/blob/codex/remote-control-mvp/documentation/specs/RemoteMVP.md" "Deploy on GitHub ↗",input "Name" F.Name [V.bool "required" true,V.attr "maxlength" "200",V.attr "autocomplete" "off"],textarea "Connection JSON" F.Connection 6 [V.bool "required" true,V.bool "spellcheck" false,V.attr "maxlength" "4096",V.attr "placeholder" "Paste your relay.connection.json"],V.button "qr-upload" "Import JSON" $ DialogCommand Intent.ImportRelay,V.p "relay-private" "Stored privately on this desktop.",V.button "" "Cancel" $ DialogCommand Intent.BackRelay]
  M.RelaySwitch -> V.div "" [V.node "dl" [V.cls "pairing-details"] [V.textNode "dt" [] "From",V.textNode "dd" [] ((F.contextChange d.record).from),V.textNode "dt" [] "To",V.textNode "dd" [] ((F.contextChange d.record).to)],V.p "" "Switching revokes all paired devices and remote corpora connections for this local app.",V.p "" "Pair all devices and reconnect all remote corpora again.",V.div "pairing-actions" [V.button "" "Cancel" $ DialogCommand Intent.BackRelay,V.control [V.disabled (M.dialogBusy d)] "Switch & revoke all" (DialogCommand Intent.ConfirmRelay)]]
  M.RelayTerms -> V.div "" [V.p "" "For sensitive work, use your own securely maintained relay.",V.p "" "Provided “as is,” without warranties. Use at your own risk.",V.p "" "To the extent permitted by law, authors, contributors, and operators disclaim liability for losses or damages from this service.",V.p "" "Non-excludable rights and liabilities remain unaffected.",V.node "label" [V.cls "relay-check"] [V.node "input" [V.attr "type" "checkbox",V.bool "checked" $ field F.Accepted == "true",V.check $ \b -> Field F.Accepted $ show b] [],V.text "I accept these beta terms."],V.div "pairing-actions" [V.button "" "Back" $ DialogCommand Intent.BackRelay,V.button "" "Use my relay" $ DialogCommand Intent.AddRelay,V.control [V.disabled $ field F.Accepted /= "true" || (M.dialogBusy d)] "Use beta" (DialogCommand Intent.AcceptRelay)]]
  M.Installation -> V.div "" [V.node "ol" [] $ map (\s -> V.textNode "li" [] (s)) (if ios then ["In Safari, tap Share.","Add to Home Screen. Open as Web App on → Add.","Open Sapiens4 from Home Screen."] else ["In Chrome, tap ⋮.","Install and create shortcut. Then tap Install.","Open Sapiens4 from Home Screen."]),V.button "" "Install Sapiens4" $ DialogCommand Intent.InstallPrompt,V.link "https://sapiens4.ai/help?q=add2homescreen" "Need help? ↗"]
  M.Notice -> V.div "" [V.p "" $ field F.Message,V.button "" "OK" CloseDialog]
  where
  field key = F.fieldValue key d.fields
  input caption key props = V.field "input" caption key (field key) props []
  textarea caption key rows props = V.field "textarea" caption key (field key) ([V.attr "rows" $ show rows] <> props) []
  select caption key options = V.field "select" caption key (field key) [] $ map (\(Tuple value caption_) -> V.textNode "option" [V.value value,V.bool "selected" $ value == field key] caption_) options
  form caption children = V.node "form" [V.cls "form-stack",V.submit SubmitDialog] $ children <> [V.textNode "button" [V.attr "type" "submit",V.cls "button primary",V.disabled (M.dialogBusy d)] (caption)]
  owner = m.snapshot.computer.owner
  windows = m.snapshot.computer.platform == "win32"
  execution = orchestration m $ F.contextId d.record
  sapis = A.filter (\a -> not (M.isGroup a || a.retired)) m.agents
  managers = A.filter (\a -> allowed [] $ a.id) sapis
  allowed seen id = id == "" || id /= F.contextId d.record && not (A.elem id seen) && allowed (A.cons id seen) ((orchestration m id).manager)
  pools = [["Armin","Howl","Seneca","Socrates","Aesop","Nikola","Walt","Akira","Milo"],["Kiki","Chihiro","Sophie","Alice","Hypatia","Simone","Ada","Oprah","Arianna"],["Sage","River","Rowan","Sky","Nova","Phoenix","Ash","Robin","Aster"]]
  suggestions = map (\pool -> let available = A.filter (\name -> not $ A.any (( _ == S.toLower name) <<< S.toLower <<< _.name) m.agents) pool in fromMaybe "Sage" $ A.index (if A.null available then map (_ <> "-" <> show d.identity) pool else available) (mod d.identity $ max 1 $ A.length available)) pools
  ios = N.ios m.env

linuxComputer :: M.Model -> M.Dialog -> V.Html
linuxComputer m d = V.div ""
  [V.p "" "Sapis use Blindly4 and Linux accessibility (AT-SPI) to work with applications on this desktop."
  ,V.p "" $ if owner == "" then "Computer ownership: available" else "In use · " <> (M.agent m owner).name
  ,case d.content of
    F.ComputerAccess access -> V.div ""
      [V.p "" $ if access.built then "Computer backend is installed" else "Build required: follow the Linux setup guide"
      ,V.p "" $ if access.trusted then "AT-SPI accessibility is available" else "AT-SPI accessibility is unavailable"
      ,V.p "" $ if access.foregroundAvailable then "Foreground window verification is available" else "No foreground window can currently be verified"
      ,V.when (access.error /= "") $ V.p "" $ access.error]
    _ -> V.p "" "Checking computer access…"
  ,V.p "" "Desktop control requires an X11 session. Wayland is not supported."
  ,V.p "" "Sapis share one computer. Access is checked again for each action; these observations do not grant permission."]
  where
  owner = m.snapshot.computer.owner

relayList :: M.Dialog -> V.Html
relayList d = if d.content == F.NoContent then V.p "" "Loading…" else V.div ""
  [V.node "div" [V.id "relay-list",V.cls $ "relay-list " <> if A.length rows >= 3 then "relay-scroll" else "",V.attr "role" "group",V.label "Relay servers"] $ map row rows
  ,V.node "label" [V.cls "relay-check"] [V.node "input" [V.attr "type" "checkbox",V.bool "checked" archived,V.check $ \b -> Field F.Archived $ show b] [],V.text "Show archived"]
  ,V.button "relay-add" "+ Add your own relay" $ DialogCommand Intent.AddRelay,V.textNode "small" [] "Recommended",V.p "relay-warning" "ⓘ Beta: use at your own risk. Self-host for sensitive work.",V.button "" (if (F.contextCorpora d.record).key /= "" then "Continue" else "Done") $ DialogCommand Intent.DoneRelay]
  where
  archived = d.fields.archived == "true"
  rows = A.filter (\r -> archived || not (r.archived)) $ (F.relays d.content).servers
  row r = let current = r.id == (F.relays d.content).selected in V.node "div" [V.cls "relay-row",V.attr "data-current" $ show current]
    [V.node "label" [] [V.node "input" [V.attr "type" "radio",V.attr "name" "relay-server",V.value $ r.id,V.bool "checked" current,V.disabled $ r.archived || not (r.available),V.check $ \_ -> DialogCommand $ Intent.SelectRelay r] [],V.node "span" [] [V.textNode "strong" [] (r.name),V.textNode "small" [] (D.replace "^https?://" "" $ r.address)]]
    ,V.span [V.cls "relay-badge"] (if r.archived then "Archived" else if not (r.available) then "Inactive" else if current then "Current" else "")
    ,V.div "relay-row-actions" [V.button "" "Edit" $ DialogCommand $ Intent.EditRelay r.id,V.button "" (if r.archived then "Restore" else "Archive") $ DialogCommand $ Intent.ArchiveRelay r]]

hostPairing :: M.Model -> M.Dialog -> V.Html
hostPairing m d = case d.content of
  F.Pairing status -> V.div "" [renderPair status, V.when (status.error /= "") $ V.p "" status.error]
  _ -> V.p "" "Loading…"
  where
  renderPair status = case F.pairView status of
    F.Unavailable -> V.p "" "Remote control is unavailable. Install the remote dependencies and restart Sapiens4."
    F.Paired device -> V.div "" [V.p "pairing-corpora" $ F.contextName d.record,V.node "dl" [V.cls "pairing-details"] $ A.concatMap (\(Tuple label text) -> [V.textNode "dt" [] label,V.textNode "dd" [] text]) [Tuple "Device" $ D.fallback "Not recorded" device.name,Tuple "Device info" $ D.fallback "Not recorded" $ S.joinWith " · " $ A.filter (_ /= "") [device.platform,device.client],Tuple "Paired" $ date "paired_at",Tuple "Last connected" $ date "last_connected_at"],V.div "pairing-actions" [action Intent.ReplaceHost "Replace remote",action Intent.RevokeHost "Revoke remote"]]
    F.AwaitingApproval pending -> V.div "" [V.p "" "Approve only if this code matches your remote device.",V.textNode "strong" [V.label pending.confirmation_code] pending.confirmation_code,V.control [V.disabled $ pending.confirmation_code == "" || M.dialogBusy d] "Approve device" (DialogCommand $ Intent.HostPair $ Intent.ApproveHost pending.peer)]
    F.Invited invitation
      | invitation.expires > m.now/1000.0 -> V.div "" [V.p "" "In the installed app, choose Connect remote corpora and scan this QR.",V.node "img" [V.id "host-pair-qr",V.attr "alt" "Pairing QR",V.attr "src" d.fields.qr] [],V.p "" $ "Expires " <> B.formatDate "" D.empty d.fields.expiration,V.div "pairing-actions" [V.button "" "Show link" $ Field F.ShowLink "true",V.textNode "a" [V.id "download-pair-qr",V.attr "download" "sapiens4-pairing.png",V.attr "href" d.fields.qr] "Download QR",V.link invitation.relay "Open app ↗",action Intent.RevokeHost "Cancel invitation"],V.node "input" [V.id "host-pair-link",V.label "Pairing link",V.bool "readOnly" true,V.hidden $ d.fields.showLink /= "true",V.value $ Remote.invitationLink invitation] []]
      | otherwise -> V.div "" [V.p "" "Invitation expired.",action Intent.RevokeHost "Cancel invitation"]
    F.Unpaired -> V.div "pairing-actions" [action Intent.InviteHost "Show QR",V.button "" "Relay servers" $ OpenDialog M.RelayList d.record]
  date key = D.fallback "Not recorded" $ fromMaybe "" $ O.lookup key d.fields.dates
  action key label = V.control [V.disabled $ M.dialogBusy d] label (DialogCommand $ Intent.HostPair key)

updateField :: F.Field -> String -> M.Dialog -> M.Dialog
updateField key value d =
  let fields = if key == F.Node && d.kind == M.DecisionPrompts then fromMaybe d.fields $ (\p -> F.promptFields p d.fields) <$> A.find (( _ == value) <<< _.node) (F.prompts d.content) else F.update key value d.fields
      withLead = if key == F.Lead && d.kind == M.GroupSettings then fields {members=A.nub $ A.snoc fields.members value} else fields
  in (M.setDialogError "" d) {fields=withLead}

orchestration :: M.Model -> String -> X.Orchestration
orchestration m id = fromMaybe (X.orchestration D.empty) $ O.lookup id m.snapshot.orchestration

open :: M.Dialog -> M.Model -> M.Model
open d m = m {dialog=Just d,dialogStack=[],menu=Nothing,sequence=d.identity,mention=Nothing}

close :: M.Model -> M.Model
close m = m {dialog=Nothing,dialogStack=[]}

push :: M.Dialog -> M.Model -> M.Model
push next m = m {dialog=Just $ next {content=fromMaybe F.NoContent $ _.content <$> m.dialog},dialogStack=m.dialogStack <> fromMaybe [] (map A.singleton m.dialog),sequence=next.identity}

back :: M.Model -> M.Model
back m = case A.unsnoc m.dialogStack of
  Nothing -> close m
  Just {init,last} -> let next = last {identity=m.sequence+1} in m {dialog=Just next,dialogStack=init,sequence=next.identity}

type SaveRequest = {path :: String, method :: String, body :: Json, caption :: String}

saveEntity :: M.Dialog -> Either String SaveRequest
saveEntity d = case d.kind of
  M.AgentSettings
    | not (D.matches "^[A-Z][A-Za-z0-9_.:#+|()&$^\\-]*$" d.fields.name) -> Left nameHelp
    | otherwise -> Right {path:"/api/agents/" <> B.encodeURI (F.contextId d.record),method:"PUT",body:D.json {name:d.fields.name,role:d.fields.role,manager:if d.fields.manager == "" then D.null else D.text d.fields.manager,execution:{mode:d.fields.mode}},caption:"Sapi saved."}
  M.GroupSettings -> Right {path:"/api/groups/" <> B.encodeURI (F.contextId d.record),method:"PUT",body:D.json {name:d.fields.name,description:d.fields.description,lead:d.fields.lead,members:d.fields.members,revision:d.fields.revision},caption:"Group saved."}
  _ -> Left "This dialog does not edit a Sapi or Group."

saveTask :: M.Model -> M.Dialog -> Boolean -> SaveRequest
saveTask m d deleted =
  let existing = F.contextId d.record /= ""
      payload = D.merge (D.json {title:d.fields.title,body:d.fields.body,assignee:d.fields.assignee,state:d.fields.state}) $ if existing then D.json {revision:D.json $ F.contextRevision d.record,deleted,state:if F.contextDeleted d.record then "backlog" else d.fields.state} else D.empty
  in {path:M.ownerPath m <> "/tasks" <> if existing then "/" <> B.encodeURI (F.contextId d.record) else "",method:if existing then "PUT" else "POST",body:payload,caption:if deleted then "Task deleted." else "Task saved."}

setupButton :: M.Dialog -> String -> String -> V.Html
setupButton d name caption = (if name == "back" then V.navigationControl else V.control) [V.cls "button",V.disabled $ M.dialogBusy d] caption $ DialogCommand $ Intent.Setup name

signInView :: Json -> V.Html
signInView value = V.div "account-sign-in" $
  [V.textNode "p" [V.attr "role" "status"] $ if signing then "Finish signing in with ChatGPT in your browser. Waiting for sign-in…" else if provider == "sign_in_required" && not (A.elem state ["failed","expired"]) then "Finish signing in with ChatGPT in your browser." else ""
  ,V.when (provider == "missing") $ V.div ""
    [V.p "" "We couldn't find a working Codex installation. Install Codex, then check again."
    ,V.div "account-actions" [V.link "https://openai.com/codex/" "Download Codex"]]
  ,V.when (provider == "unavailable") $ V.p "" "Your account may still be connected. Try checking again."
  ,V.when (state == "failed" || state == "expired" || state == "browser_busy" || state == "logout_failed" || state == "cancelled") $ V.p "" $ case state of
    "expired" -> "Sign-in expired. Try again or use a one-time code."
    "browser_busy" -> "Another sign-in is using the browser callback. Finish it and try again, or use a one-time code."
    "logout_failed" -> "We couldn’t finish signing out. Recheck your connection, then try signing out again."
    "cancelled" -> "Sign-in cancelled. You can try again when you’re ready."
    _ -> "Sign-in didn't finish. Try again or use a one-time code."
  ,V.when (provider == "sign_in_required") $ V.button "button primary" (if state == "failed" || state == "expired" then "Try again" else "Connect") $ DialogCommand $ Intent.Setup "login"
  ,V.when (provider == "sign_in_required") $ V.p "form-hint" "Your browser will open to complete sign-in."
  ,V.when (signing && D.s "url" login /= "") $ V.div "account-actions"
    [V.button "button" (if D.s "code" login == "" then "Open browser again" else "Open sign-in page") $ DialogCommand $ Intent.Setup "open-login"
    ,V.button "button" "Copy sign-in link" $ Copy (D.s "url" login) "Sign-in link copied."]
  ,V.when (D.s "code" login /= "") $ V.div "account-device-code"
    [V.p "" "Enter this one-time code on the official OpenAI page. Only continue if you started this sign-in in Sapiens."
    ,V.textNode "strong" [] (D.s "code" login)]
  ,V.when signing $ V.control [V.cls "button",V.id "cancel-codex-login"] "Cancel" $ DialogCommand $ Intent.Setup "cancel"
  ,V.when (not signing) $ V.button "button" "Check again" $ DialogCommand $ Intent.Setup "refresh"
  ,V.when (provider == "sign_in_required") $ V.node "details" [V.cls "account-help"]
    [V.textNode "summary" [] "Having trouble signing in?"
    ,V.p "form-hint" "You can connect the same ChatGPT account using a one-time code. Device code login must be enabled in your ChatGPT security settings."
    ,V.button "button" "Use a one-time code" $ DialogCommand $ Intent.Setup "device-login"]]
  where
  provider = D.s "provider" value
  login = D.get "login" value
  state = D.s "state" login
  signing = provider == "signing_in"

accountView :: M.Dialog -> V.Html
accountView d = V.node "section" [V.cls "ai-provider-detail-body",V.label "Codex account"] $
  let value = (F.setupValue d.content)
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
          ,V.control [V.cls "button",V.id "resume-ai-work",V.disabled $ M.dialogBusy d] "Resume AI work" $ DialogCommand $ Intent.Setup "resume"]
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


onboardingCorpora :: M.Model -> Array X.Corpora
onboardingCorpora m = A.filter (\r -> r.location == X.Local && r.status /= X.Archived) m.records

startingCorpora :: M.Model -> Json -> Maybe X.Corpora
startingCorpora m value = A.find ((_ == D.s "selected_corpora" value) <<< _.id) (onboardingCorpora m)

setupSelection :: M.Model -> Json -> Maybe X.Corpora
setupSelection m value
  | D.b "completed" value && (m.setupChecked || m.restoredCorpora) = Nothing
  | otherwise = case startingCorpora m value of
      Just record -> Just record
      Nothing -> case A.uncons (onboardingCorpora m) of
        Just {head,tail} | A.null tail -> Just head
        _ -> Nothing

onboardingConnected :: M.Model -> Boolean
onboardingConnected m = M.online m && m.snapshot.corpora.id == (M.activeRecord m).id && A.any ((_ == m.active) <<< _.key) (onboardingCorpora m)

onboardingView :: M.Model -> M.Dialog -> Json -> V.Html
onboardingView _ d _ | d.storage.page /= M.SetupSteps = storageView d
onboardingView m d value = V.div "setup-screen"
  [V.div "setup-step-content" content
  ,V.when navigation $ V.div "setup-footer"
    [V.div "setup-navigation"
      [V.div "setup-back" [if needsCorpora then setupButton d "back" "← Back" else V.when m.env.native $ V.navigationControl [V.cls "button",V.disabled $ authenticating d] "← Back" $ StorageNavigate M.ChooseStorage]
      ,V.navigationControl [V.cls "button primary",V.disabled $ M.dialogBusy d || needsCorpora && (M.busy m || not (onboardingConnected m))] caption $ DialogCommand $ Intent.Setup forward]]]
  where
  permission = D.s "computer_permission" value
  step = D.s "step" value
  needsCorpora = step /= "provider"
  navigation = needsCorpora || A.elem d.providerPage [M.Providers,M.CodexDetails] && not (authenticating d)
  forward = case step of
    "permissions" -> "permissions-done"
    "corpora" -> "corpora-done"
    "ready" -> "finish"
    _ -> if D.b "ready" value then "provider-next" else "provider-skip"
  caption = case step of
    "ready" -> if M.dialogBusy d then "Opening Sapiens4…" else "Open Sapiens4"
    "provider" | not (D.b "ready" value) -> "Skip for now"
    _ -> "Continue"
  content = case step of
    "permissions" ->
      [V.p "setup-intro" "Computer control is optional. Chat works without it."
      ,computerAccess d value]
    "corpora" ->
      [V.p "setup-intro" "Choose the Corpora you'd like to open first."
      ,V.div "setup-corpora-list" $ map (\r -> V.control [V.cls "setup-corpora-choice",V.disabled $ M.busy m,V.attr "aria-pressed" $ show $ r.key == m.active] r.name (EnterCorpora r.key)) (onboardingCorpora m)]
    "ready" ->
      [V.p "setup-intro" "Your Sapiens4 workspace is ready to use."
      ,V.div "setup-summary"
        [V.div "setup-summary-row" [V.span [V.cls "setup-summary-label"] "AI connection",V.span [] (if D.s "provider" value == "signed_in" then "Codex connected" else "Not connected")]
        ,V.div "setup-summary-row" [V.span [V.cls "setup-summary-label"] "Computer access",V.span [] (case permission of
          "granted" -> "Enabled"
          "not_required" -> "Available for supported apps"
          "missing" -> "Helper unavailable"
          _ -> "Not enabled")]
        ,V.div "setup-summary-row" [V.span [V.cls "setup-summary-label"] "Corpora",V.span [] (M.activeRecord m).name]
        ,V.div "setup-summary-divider" []
        ,V.div "setup-summary-row" [V.span [V.cls "setup-summary-label"] "Remote Control",V.span [V.cls "setup-muted"] "Available in Settings"]
        ,V.div "setup-summary-row" [V.span [V.cls "setup-summary-label"] "Analytics",V.span [V.cls "setup-muted"] "Coming soon"]]]
    _ -> [aiProvidersView d value]

storageView :: M.Dialog -> V.Html
storageView d = V.div "setup-screen"
  [V.div "setup-step-content" $ if storage.page == M.SetupWelcome then
    [V.p "setup-intro" "Your own team of AI coworkers. Think together, make things, and get work done on your computer."
    ,V.node "ul" [V.cls "setup-benefits"]
      [V.textNode "li" [] "Your team, conversations, and tasks in one place"
      ,V.textNode "li" [] "Connect your AI tools whenever you're ready"]]
    else
    [V.p "setup-intro" "Keep your current space or choose another storage location. Changes apply when you continue."
    ,V.div "setup-choice-list"
      [choice M.KeepStorage "Keep current storage" storage.current
      ,choice M.FreshStorage "Start fresh" storage.defaultDirectory
      ,V.node "div" [V.cls "setup-storage-option",V.attr "data-change" $ show $ storage.choice == M.ExistingStorage && storage.directory /= ""]
        [choice M.ExistingStorage "Use another folder" (D.fallback "Choose a folder with your existing agents and conversations." storage.directory)
        ,V.when (storage.choice == M.ExistingStorage && storage.directory /= "") $ V.control [V.cls "setup-storage-change",V.label "Choose another folder",V.disabled busy] "Change" StoragePick]]
    ,V.p "setup-note" "Existing data stays in its original location."]
  ,V.div "setup-footer" [V.div "setup-navigation"
    [V.div "setup-back" [V.when (storage.page /= M.SetupWelcome) $ V.navigationControl [V.cls "button",V.disabled busy] "← Back" $ StorageNavigate M.SetupWelcome]
    ,V.navigationControl [V.cls "button primary",V.disabled $ busy || storage.page == M.ChooseStorage && (storage.current == "" || storage.choice == M.ExistingStorage && storage.directory == "")] "Continue" (if storage.page == M.SetupWelcome then StorageNavigate M.ChooseStorage else StorageApply)]]]
  where
  storage = d.storage
  busy = M.dialogBusy d
  folder path =
    let parts = A.filter (_ /= "") $ S.split (S.Pattern "/") $ S.replaceAll (S.Pattern "\\") (S.Replacement "/") path
    in (if A.length parts > 2 then "…/" else "") <> S.joinWith "/" (A.drop (max 0 $ A.length parts - 2) parts)
  choice key title detail = V.node "button" [V.attr "type" "button",V.cls "setup-choice",V.label title,V.disabled busy,V.attr "aria-pressed" $ show $ key == storage.choice,V.click $ StorageChoose key]
    [V.div "setup-choice-copy" [V.textNode "strong" [] title,V.textNode "small" [V.attr "title" detail] (folder detail)]]

computerAccess :: M.Dialog -> Json -> V.Html
computerAccess d value = V.div "computer-access"
  [V.p "" $ case permission of
    "missing" -> "Computer helper missing. Reinstall Sapiens to repair it."
    "granted" -> "Computer control enabled."
    "denied" -> "To interact with apps on your Mac, Sapiens needs Accessibility permission."
    "not_required" -> "Computer control works with supported apps."
    _ -> "Couldn't check Accessibility. Check again or open System Settings."
  ,V.node "div" [V.cls "actions",V.style "margin-top:24px"]
    [V.when (A.elem permission ["not_requested","denied","unavailable"]) $ setupButton d "permission" "Enable access"
    ,V.when (permission /= "missing" && permission /= "not_required") $ setupButton d "refresh" "Recheck access"
    ,V.when (d.kind /= M.Onboarding) $ V.control [V.cls "button",V.style "margin-left:auto"] "Done" CloseDialog]]
  where permission = D.s "computer_permission" value
