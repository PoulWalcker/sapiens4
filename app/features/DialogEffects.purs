module Sapiens.DialogEffects where

import Sapiens.Form as F
import Prelude
import Control.Promise (toAffE)
import Data.Argonaut.Core (Json, toString)
import Data.Array as A
import Data.Either (Either(..))
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.String as S
import Data.String.CodeUnits as CU
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff, try, throwError)
import Effect.Class (liftEffect)
import Effect.Exception (error, message)
import Effect.Ref as Ref
import Foreign.Object as O
import Halogen as H
import Libs4.Browser as B
import Sapiens.Action as Intent
import Sapiens.Action (Action(..), Success(..))
import Sapiens.Connections as C
import Sapiens.Data as D
import Sapiens.Browser as Browser
import Sapiens.Domain as X
import Sapiens.Dialog as Dialog
import Sapiens.Effects as E
import Sapiens.Model as M
import Sapiens.Navigation as N
import Sapiens.Remote as R

handle :: C.Runtime -> E.Dispatch -> Action -> E.UI Unit
handle runtime dispatch action = case action of
  OpenDialog kind record -> do
    m <- H.get
    if kind == M.ConnectRemote && m.installRequired then dispatch Install else do
      when (m.dialog /= Nothing) $ void $ H.fork $ H.liftAff $ C.cancelPairing runtime
      let d = Dialog.make kind record m
      H.modify_ $ Dialog.open d
      when (kind == M.ConnectRemote && M.remote m) $ liftEffect $ B.historyPush $ D.json {dialog: true}
      E.sync
      case kind of
        M.DecisionPrompts -> receive d.identity (\value -> E.modifyDialog d.identity _ {content=F.Prompts $ map F.prompt $ D.rows "prompts" value,fields=F.decode $ D.first $ D.rows "prompts" value,status=M.Editing}) $ E.api runtime m "/api/decision-prompts" "GET" D.null
        M.RelayList -> receive d.identity (\value -> E.modifyDialog d.identity _ {content=F.relayRegistry value,status=M.Editing}) $ C.json "/api/host/relays" "GET" D.null
        M.Onboarding -> dispatch $ OnboardingPoll d.identity
        M.AIProviders -> when m.localAvailable $ dispatch $ OnboardingPoll d.identity
        M.Computer -> when m.localAvailable $ dispatch $ OnboardingPoll d.identity
        M.HostPairing -> dispatch $ HostPairPoll d.identity
        M.EditCorpora -> liftEffect $ B.focus "#corpora-dialog textarea"
        M.ConnectRemote -> liftEffect $ B.focus "#paste-invitation textarea"
        _ -> pure unit
  OnboardingPoll id -> E.inDialog id \d -> when (A.elem d.kind [M.Onboarding,M.AIProviders,M.Computer] && not (M.dialogBusy d)) do
    receive id (\value -> do
      H.modify_ _ {account=value}
      E.modifyDialog id \current -> current {content=F.Setup value,status=M.Editing,providerPage=if d.kind == M.AIProviders && current.providerPage == M.CodexAuth && D.s "provider" value == "signed_in" then M.ProviderList else current.providerPage}
      when (d.kind == M.Onboarding && D.b "completed" value && D.b "ready" value) $ dispatch CloseDialog
      when (A.elem (D.s "provider" value) ["signing_in","signing_out"]) $ E.later 1000.0 $ dispatch $ OnboardingPoll id) $ C.json "/api/host/onboarding" "GET" D.null
  CloseDialog -> do
    m <- H.get
    case m.dialog of
      Just d | not (Dialog.dismissible d) -> E.modifyDialog d.identity $ M.setDialogError "Finish setup before entering your workspace. You can close the app and return to setup later."
      _ -> do
        void $ H.fork $ H.liftAff $ C.cancelPairing runtime
        H.modify_ Dialog.close
        E.sync
  SetProviderPage page -> E.withDialog \d -> when (d.kind == M.AIProviders && not (M.dialogBusy d)) $
    E.modifyDialog d.identity _ {providerPage=page,status=M.Editing}
  Field key value -> do
    H.modify_ \m -> m {dialog=map (Dialog.updateField key value) m.dialog}
    when (key == F.ShowLink) do
      liftEffect $ B.focus "#host-pair-link"
      m <- H.get
      traverse_ (\d -> liftEffect $ B.selectRange "#host-pair-link" 0 $ CU.length $ d.fields.link) m.dialog
  CheckField value checked -> H.modify_ \m -> m {dialog=map (\d -> d {fields=F.member value checked d.fields}) m.dialog}
  SubmitDialog -> do
    m <- H.get
    traverse_ (\d -> unless (M.dialogBusy d) $ submit m d) m.dialog
  AttachmentKind kind -> do
    m <- H.get
    if A.elem kind [X.LinkAttachment,X.FileAttachment] then dispatch $ OpenDialog (M.Reference kind) F.NoRecord else do
      unless (A.length (M.attachments m) >= 8) do
        H.modify_ _ {uploading=m.uploading+1,dialog=map (M.setDialogError "") m.dialog}
        attach m do
          selected <- B.chooseFile (if kind == X.ImageAttachment then "image/png,image/jpeg,image/gif,image/webp" else ".pdf,.txt,.md,.csv,.json,.doc,.docx,.xls,.xlsx,.ppt,.pptx,.rtf,.odt") 10485760
          case selected of
            Nothing -> pure Nothing
            Just file -> Just <$> E.api runtime m (M.ownerPath m <> "/attachments") "POST" (D.json {kind, name: file.name, data: file.data})
      when (A.length (M.attachments m) >= 8) $ E.notify "Attach up to 8 items per message."
  DialogCommand command -> commandAction command
  _ -> pure unit
  where
  attach m operation = E.attempt operation \result -> E.inEpoch m.epoch \current -> do
    H.modify_ _ {uploading=max 0 $ current.uploading-1}
    traverse_ (\d -> E.modifyDialog d.identity (M.setDialogBusy false)) m.dialog
    case result of
      Left failure -> case m.dialog, current.dialog of
        Just requested, Just active | requested.identity == active.identity ->
          E.modifyDialog requested.identity $ M.setDialogError $ message failure
        _, _ -> E.notify $ message failure
      Right selected -> traverse_ (\item -> do
        H.modify_ \s -> s {attachments=O.alter (Just <<< flip A.snoc (X.attachment item) <<< fromMaybe []) m.selected s.attachments}
        dispatch CloseDialog
        E.save dispatch) selected
  receive :: forall a. Int -> (a -> E.UI Unit) -> Aff a -> E.UI Unit
  receive id done operation = E.attempt operation \result -> E.inDialog id \_ -> do
    E.modifyDialog id (M.setDialogBusy false)
    case result of
      Left failure -> E.modifyDialog id (M.setDialogError $ message failure)
      Right value -> done value
  runDialog :: forall a. M.Dialog -> (a -> E.UI Unit) -> Aff a -> E.UI Unit
  runDialog d done operation = do
    E.modifyDialog d.identity _ {status=M.Submitting ""}
    receive d.identity done operation
  saved caption _ = dispatch CloseDialog *> E.notify caption *> dispatch Poll
  submit m d = case d.kind of
    kind | kind == M.AgentSettings || kind == M.GroupSettings -> case Dialog.saveEntity d of
      Left failure -> E.modifyDialog d.identity $ M.setDialogError failure
      Right request -> runSave m d request
    M.GroupTask -> saveTask m d false
    M.Reference kind -> if A.length (M.attachments m) >= 8 then E.modifyDialog d.identity (M.setDialogError $ "Attach up to 8 items per message.") else do
      H.modify_ _ {uploading=m.uploading+1}
      E.modifyDialog d.identity (M.setDialogBusy true)
      attach m $ Just <$> E.api runtime m (M.ownerPath m <> "/attachments") "POST" (D.json {kind, value: D.json d.fields.value})
    M.DecisionPrompts -> dispatch $ Mutate ("/api/decision-prompts/" <> B.encodeURI (d.fields.node)) "PUT" (D.json {content: D.json d.fields.content}) (SavedPrompt $ d.fields.content)
    M.CreateCorpora -> do
      E.modifyDialog d.identity _ {status=M.Submitting ""}
      E.attempt (C.json "/api/host/local" "POST" (D.json {name:d.fields.name,scope:d.fields.scope})) \result -> do
        current <- H.get
        case result of
          Left failure -> E.modifyDialog d.identity _ {status=M.Rejected $ message failure}
          Right value -> do
            let record = (X.corpora value) {key=D.s "id" value}
            H.modify_ \s -> s {records=A.snoc s.records record}
            when ((_.identity <$> current.dialog) == Just d.identity) $ dispatch CloseDialog
            dispatch $ EnterCorpora $ record.key
    M.EditCorpora -> runDialog d (\_ -> dispatch CloseDialog *> dispatch LoadRegistry) do
      C.reconcile runtime (M.remote m) m.records m.active
      info <- C.request runtime (M.remote m) (F.contextCorpora d.record) "/api/corpora" "GET" D.null
      when (D.s "status" info == "archived") $ throwError $ error "Restore this archived corpora before sending. Your draft is kept here."
      current <- if D.s "status" info == "paused" then C.lifecycle runtime (M.remote m) (F.contextCorpora d.record) "activate" else pure info
      C.request runtime (M.remote m) (F.contextCorpora d.record) ("/api/agents/" <> B.encodeURI (D.s "chief_id" current) <> "/messages") "POST" $ D.json {text: D.json d.fields.message}
    M.RelayAdd -> case parseRelay $ d.fields.connection of
      Left failure -> E.modifyDialog d.identity (M.setDialogError $ failure)
      Right connection -> do
        let change = D.obj $ [Tuple "action" (D.text "save"),Tuple "name" (D.text $ S.trim $ d.fields.name),Tuple "connection" connection] <> if F.contextId d.record == "" then [] else [Tuple "id" $ D.json $ F.contextId d.record]
        if F.contextId d.record == (F.relays d.content).selected && F.contextConnection d.record /= connection then confirmRelay d change (d.fields.name) else changeRelay d change
    _ -> pure unit
  runSave m d request = runDialog d (saved request.caption) $ E.api runtime m request.path request.method request.body
  saveTask m d deleted = runDialog d (saved request.caption) $ E.api runtime m request.path request.method request.body
    where
    request = Dialog.saveTask m d deleted
  push kind record = do
    m <- H.get
    let next = Dialog.make kind record m
    H.modify_ $ Dialog.push next
    E.sync
  relayList content = do
    m <- H.get
    let previous = A.find (( _ == M.RelayList) <<< _.kind) m.dialogStack
        record = maybe F.NoRecord _.record previous
        next = Dialog.make M.RelayList record m
    H.modify_ _ {dialog=Just $ next {content=F.relayRegistry content},dialogStack=[],sequence=next.identity}
    E.sync
  changeRelay d data_ = runDialog d (\value -> when (D.b "reset" value) (resetRelay true) *> relayList value) $ C.json "/api/host/relays" "POST" $ D.put "revision" (D.json (F.relays d.content).revision) data_
  confirmRelay d change to = do
    let current = fromMaybe (F.relay D.empty) $ A.find ((_ == (F.relays d.content).selected) <<< _.id) (F.relays d.content).servers
    push M.RelaySwitch $ F.ChangeRecord {change,saving:D.s "action" change == "save",from:current.name,to}
  resetRelay broadcast = do
    m <- H.get
    liftEffect do
      clients <- Ref.read runtime.clients
      traverse_ R.close $ O.values clients
      Ref.write O.empty runtime.clients
      Ref.write O.empty runtime.credentials
      traverse_ (\r -> B.storageRemove $ "s4.snapshot." <> r.key) $ A.filter (( _ == X.Remote) <<< _.location) m.records
    let locals = map (_ {paired=false}) $ A.filter (( _ == X.Local) <<< _.location) m.records
        active = if (M.activeRecord m).location == X.Remote then fromMaybe "" $ _.key <$> A.head locals else m.active
    H.modify_ _ {records=locals,active=active,connection=M.Offline,epoch=m.epoch+1,polling=false,saving=false,nativeReporting=false,nativeReports=[]}
    void $ liftEffect $ C.write "s4.records" $ D.json locals
    void $ liftEffect $ C.write "s4.active" $ D.text active
    void $ liftEffect $ C.write "s4.lifecycle.pending" D.null
    void $ liftEffect $ C.write "s4.switch.pending" D.null
    when broadcast do
      uuid <- liftEffect B.uuid
      void $ liftEffect $ C.write "s4.relay-reset" $ D.text uuid
    dispatch LoadRegistry
  commandAction command = do
    m <- H.get
    case command of
      Intent.CreateAgent -> do
        dispatch $ SetScope N.All
        current <- H.get
        let template = m.snapshot.creation_template
        when (S.trim (M.draft current) == "") $ dispatch $ Draft template
        liftEffect $ B.focus "#message-input"
        latest <- H.get
        case CU.indexOf (S.Pattern "WORKFLOW") $ M.draft latest of
          Just index -> liftEffect $ B.selectRange "#message-input" index (index+8)
          Nothing -> pure unit
      Intent.MentionGroup entity -> case m.groupView of
        Nothing -> dispatch $ Select entity.id
        Just group -> do
          dispatch $ Select group
          current <- H.get
          let old = M.draft current
              prefix = A.find (\name -> S.take (CU.length name) old == name) $ map (\id -> "@" <> (M.agent current id).name <> " ") $ (M.selected current).members
          dispatch $ Draft $ "@" <> entity.name <> " " <> maybe old (\p -> CU.drop (CU.length p) old) prefix
          liftEffect $ B.focus "#message-input"
      Intent.ResetRelay -> resetRelay false
      Intent.InstallPrompt -> void $ H.fork $ void $ H.liftAff $ try $ toAffE B.installPrompt
      _ -> traverse_ (\d -> unless (M.dialogBusy d) $ dialogCommand m d command) m.dialog
  dialogCommand m d command = case command of
    Intent.Setup setupAction -> do
      -- A mutation owns a new dialog generation: old polling responses and
      -- timers cannot overwrite its result or unlock its busy controls.
      let requested = d {identity=m.sequence+1}
      H.modify_ _ {dialog=Just requested,sequence=requested.identity}
      runDialog requested (\value -> do
        H.modify_ _ {account=value}
        E.modifyDialog requested.identity \current -> current {content=F.Setup value,status=M.Editing,providerPage=if requested.kind /= M.AIProviders then current.providerPage
          else if A.elem setupAction ["connect","login","device-login","change-account"] then (if D.s "provider" value == "signed_in" then M.ProviderList else M.CodexAuth)
          else if A.elem setupAction ["cancel","sign-out"] then M.ProviderList
          else current.providerPage,fields=current.fields {value=""}}
        when (requested.kind == M.Onboarding && D.b "completed" value && D.b "ready" value) $ dispatch CloseDialog
        when (A.elem (D.s "provider" value) ["signing_in","signing_out"]) $ E.later 1000.0 $ dispatch $ OnboardingPoll requested.identity) $ C.json "/api/host/onboarding" "POST" $ D.json {action: setupAction}
    Intent.SuggestNames -> do
      E.modifyDialog d.identity (\s -> s {identity=s.identity+1})
      H.modify_ (\s -> s {sequence=s.sequence+1})
    Intent.DeleteTask -> saveTask m d true
    Intent.AddRelay -> push M.RelayAdd F.NoRecord
    Intent.BackRelay -> if A.null m.dialogStack then dispatch CloseDialog else H.modify_ Dialog.back *> E.sync
    Intent.SelectRelay relay -> when (relay.id /= (F.relays d.content).selected) $ confirmRelay d (D.json {action: "switch", id: relay.id}) relay.name
    Intent.ConfirmRelay -> changeRelay d $ D.put "confirm_revoke" (D.bool true) $ (F.contextChange d.record).change
    Intent.EditRelay id -> runDialog d (push M.RelayAdd <<< F.relayContext) $ C.json ("/api/host/relays/" <> B.encodeURI id) "GET" D.null
    Intent.ArchiveRelay relay -> changeRelay d $ D.json {action: if relay.archived then "restore" else "archive", id: relay.id}
    Intent.ImportRelay -> runDialog d (traverse_ (\raw -> E.modifyDialog d.identity \current -> current {fields=current.fields {connection=raw},status=M.Editing})) do
      selected <- B.chooseFile "application/json,.json" 4096
      case selected of
        Nothing -> pure Nothing
        Just file -> case toString (B.base64Decode file.data) of
          Nothing -> throwError $ error "Could not decode the connection file."
          Just raw -> case parseRelay raw of
            Left failure -> throwError $ error failure
            Right _ -> pure $ Just raw
    Intent.DoneRelay -> do
      let current = fromMaybe (F.relay D.empty) $ A.find ((_ == (F.relays d.content).selected) <<< _.id) (F.relays d.content).servers
      if (F.contextCorpora d.record).key == "" then dispatch CloseDialog else if current.beta && not ((F.relays d.content).beta_accepted) then push M.RelayTerms d.record else dispatch $ OpenDialog M.HostPairing $ F.readyContext d.record
    Intent.AcceptRelay -> do
      E.modifyDialog d.identity (M.setDialogBusy true)
      receive d.identity (\_ -> dispatch $ OpenDialog M.HostPairing $ F.readyContext d.record) $
        C.json "/api/host/relays" "POST" $ D.json {revision: D.json (F.relays d.content).revision, action: "accept-beta", accepted: true}
    Intent.HostPair intent -> do
      let revoking = intent == Intent.RevokeHost || intent == Intent.ReplaceHost
      proceed <- if revoking then liftEffect $ B.confirm $ if intent == Intent.RevokeHost then "Revoke this remote access? It will need to pair again." else "Replace the trusted remote device? The previous device will lose access." else pure true
      when proceed $ runDialog d (\result -> do
        E.modifyDialog d.identity $ M.setDialogError $ if D.valid (D.get "relay_deleted" result) && not (D.b "relay_deleted" result) then "Revoked locally. Relay could not be reached." else ""
        dispatch $ HostPairPoll d.identity) do
        previous <- if revoking then C.request runtime (M.remote m) (F.contextCorpora d.record) "/api/remote/revoke" "POST" D.empty else pure D.empty
        case intent of
          Intent.InviteHost -> invite m d
          Intent.ReplaceHost -> invite m d
          Intent.ApproveHost peer -> C.request runtime (M.remote m) (F.contextCorpora d.record) "/api/remote/approve" "POST" $ D.json {peer}
          Intent.RevokeHost -> pure previous
    _ -> pure unit

  invite m d = C.request runtime (M.remote m) (F.contextCorpora d.record) "/api/remote/pair" "POST" (D.json {saved:true})

parseRelay :: String -> Either String Json
parseRelay raw =
  let value = D.parse raw
      url = Browser.url "about:blank" $ D.s "relay" value
      valid = CU.length raw <= 4096 && D.s "format" value == "sapiens-relay-connection-v1" && url.username == "" && url.password == "" && url.search == "" && url.hash == "" && url.path == "/" && (url.protocol == "https:" || url.protocol == "http:" && A.elem (url.host) ["localhost","127.0.0.1","[::1]"])
      base = D.json {format: D.get "format" value, relay: D.json url.origin}
  in if not valid then Left "Enter valid relay connection JSON." else if D.b "beta" value && D.s "token" value == "" then Right $ D.put "beta" (D.bool true) base else if D.matches "^[\\x21-\\x7e]{32,256}$" $ D.s "token" value then Right $ D.put "token" (D.get "token" value) base else Left "Enter valid relay connection JSON."
