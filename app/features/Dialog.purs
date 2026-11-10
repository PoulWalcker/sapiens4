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
make kind record m = {kind,title,record,fields,content:F.NoContent,status:M.Editing,identity:m.sequence+1}
  where
  name = F.contextName record
  title = case kind of
    M.AppMenu -> D.fallback "CORPORA" $ m.snapshot.corpora.name
    M.WorkspaceSettings -> "Workspace & settings"
    M.Computer -> "Shared computer"
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
  Just d -> V.node "dialog" [V.id $ if M.corporaDialog d.kind then "corpora-dialog" else "modal",V.cls $ if d.kind == M.AppMenu then "app-menu-sheet" else "",V.attr "aria-labelledby" "dialog-title",V.on "cancel" \e -> Link e CloseDialog]
    [V.div (if M.corporaDialog d.kind then "corpora-menu-heading" else "modal-top") [V.span [V.cls "tiny-label"] "SAPIENS4",V.icon (if M.corporaDialog d.kind then "Close" else "Close dialog") "×" CloseDialog]
    ,V.node "div" [V.id $ if M.corporaDialog d.kind then "corpora-dialog-body" else "modal-content"] [V.textNode "h2" [V.id "dialog-title"] (d.title),body m d,V.textNode "p" [V.cls "corpora-feedback",V.attr "role" "status"] ((M.dialogError d))]]

body :: M.Model -> M.Dialog -> V.Html
body m d = case d.kind of
  M.AppMenu -> V.node "nav" [V.label "Corpora menu"] [V.button "app-menu-row" "Workspace & settings" $ OpenDialog M.WorkspaceSettings F.NoRecord,V.button "app-menu-row" "Shared computer" $ OpenDialog M.Computer F.NoRecord]
  M.WorkspaceSettings -> V.div "" [V.p "" "Talk to your Sapis in chat, manage their notes, and save documents in their workspaces. Sapis can delegate requests through ChiefSapi.",V.p "" "Use the corpora menu to switch workspaces. Active and Paused describe execution on the host; Connecting means this device is restoring its connection.",V.button "button" "Edit decision prompts" $ OpenDialog M.DecisionPrompts F.NoRecord]
  M.Computer | m.snapshot.computer.platform == "linux" -> linuxComputer m d
  M.Computer -> V.div "" [V.p "" "Blindly4 is the main computer-use tool. Ask a Sapi in chat to work on your computer.",V.div "settings-row" [V.span [] (if m.snapshot.computer.built then "Blindly4 is built" else "Build required: run " <> if windows then ".\\start.ps1" else "./start.sh"),V.span [V.cls "tag"] (if owner == "" then "Available" else "In use · " <> (M.agent m owner).name)],V.p "" $ "Sapis share one computer. " <> if windows then "Windows UI Automation works with accessible apps on your desktop. Elevated apps and secure system dialogs cannot be controlled." else "macOS Accessibility access is required for desktop interaction; permission failures appear in chat."]
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
