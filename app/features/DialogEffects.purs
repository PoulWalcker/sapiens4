module Sapiens.DialogEffects where

import Prelude
import Control.Promise (toAffE)
import Data.Argonaut.Core (Json)
import Data.Array as A
import Data.Either (Either(..))
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.String as S
import Data.String.CodeUnits as CU
import Data.Tuple (Tuple(..))
import Effect.Aff (try, throwError)
import Effect.Class (liftEffect)
import Effect.Exception (error, message)
import Effect.Ref as Ref
import Foreign.Object as O
import Halogen as H
import Libs4.Browser as B
import Sapiens.Action (Action(..), Success(..))
import Sapiens.Connections as C
import Sapiens.Data as D
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
      operation <- liftEffect B.uuid
      let routine = A.elem kind [M.RoutineRun,M.RoutineDelete,M.RoutineStop,M.RoutineReject,M.RoutineAnalyze,M.RoutineStopAnalysis,M.DoctorAccess]
          prepared = if routine then D.merge record $ D.json {operation, owner: if D.s "owner" record == "" then m.selected else D.s "owner" record} else record
          d = Dialog.make kind prepared m
      H.modify_ _ {dialog=Just d,dialogStack=[],menu=Nothing,sequence=d.identity,mention=Nothing}
      when (kind == M.ConnectRemote && M.remote m) $ liftEffect $ B.historyPush $ D.json {dialog: true}
      E.sync
      case kind of
        M.DecisionPrompts -> receive d.identity (\value -> E.modifyDialog d.identity _ {content=value,fields=D.first $ D.rows "prompts" value,error=""}) $ E.api runtime m "/api/decision-prompts" "GET" D.null
        M.RelayList -> receive d.identity (\value -> E.modifyDialog d.identity _ {content=value,error=""}) $ C.json "/api/host/relays" "GET" D.null
        M.HostPairing -> dispatch $ HostPairPoll d.identity
        M.EditCorpora -> liftEffect $ B.focus "#corpora-dialog textarea"
        M.ConnectRemote -> liftEffect $ B.focus "#paste-invitation textarea"
        _ -> pure unit
  CloseDialog -> do
    void $ H.fork $ H.liftAff $ C.cancelPairing runtime
    H.modify_ _ {dialog=Nothing,dialogStack=[]}
    E.sync
  Field key value -> do
    operation <- liftEffect B.uuid
    H.modify_ \m -> m {dialog=map (\d -> let updated = updateField key value d in if A.elem d.kind [M.RoutineRun,M.RoutineReject,M.DoctorAccess] then updated {record=D.put "operation" (D.text operation) updated.record} else updated) m.dialog}
    when (key == "showLink") do
      liftEffect $ B.focus "#host-pair-link"
      m <- H.get
      traverse_ (\d -> liftEffect $ B.selectRange "#host-pair-link" 0 $ CU.length $ D.s "link" d.fields) m.dialog
  CheckField key value checked -> H.modify_ \m -> m {dialog=map (\d -> d {fields=D.put key (D.array $ map D.text $ if checked then A.nub $ A.snoc (D.strings $ D.get key d.fields) value else A.filter (_ /= value) $ D.strings $ D.get key d.fields) d.fields}) m.dialog}
  SubmitDialog -> do
    m <- H.get
    traverse_ (\d -> unless d.busy $ submit m d) m.dialog
  AttachmentKind kind -> do
    m <- H.get
    if kind == "link" || kind == "filepath" then dispatch $ OpenDialog (M.Reference kind) (D.json {owner: m.selected}) else do
      unless (A.length (M.attachments m) >= 8) do
        H.modify_ _ {uploading=m.uploading+1}
        attach m do
          file <- toAffE $ B.chooseFile (if kind == "image" then "image/png,image/jpeg,image/gif,image/webp" else ".pdf,.txt,.md,.csv,.json,.doc,.docx,.xls,.xlsx,.ppt,.pptx,.rtf,.odt") 10485760
          if not (D.valid file) then pure D.null else E.api runtime m (M.ownerPath m <> "/attachments") "POST" (D.json {kind, name: D.get "name" file, data: D.get "data" file})
      when (A.length (M.attachments m) >= 8) $ E.notify "Attach up to 8 items per message."
  DialogCommand command value -> commandAction command value
  _ -> pure unit
  where
  attach m operation = E.attempt operation \result -> E.inEpoch m.epoch \current -> do
    H.modify_ \s -> s {uploading=max 0 $ current.uploading-1,dialog=map (_ {busy=false}) s.dialog}
    case result of
      Left failure -> E.notify $ message failure
      Right item -> when (D.valid item) do
        H.modify_ \s -> s {attachments=O.alter (Just <<< flip A.snoc item <<< fromMaybe []) m.selected s.attachments}
        dispatch CloseDialog
        E.save dispatch
  updateField key value d =
    let fields = if key == "node" && d.kind == M.DecisionPrompts then fromMaybe d.fields $ A.find (( _ == value) <<< D.s "node") $ D.rows "prompts" d.content else D.put key (D.text value) d.fields
        withLead = if key == "lead" && d.kind == M.GroupSettings then D.put "members" (D.array $ map D.text $ A.nub $ A.snoc (D.strings $ D.get "members" fields) value) fields else fields
    in d {fields=withLead,error=""}
  receive id done operation = E.attempt operation \result -> E.inDialog id \_ -> do
    E.modifyDialog id _ {busy=false}
    case result of
      Left failure -> E.modifyDialog id _ {error=message failure}
      Right value -> done value
  runDialog d done operation = do
    E.modifyDialog d.identity _ {busy=true,error=""}
    receive d.identity done operation
  saved caption _ = dispatch CloseDialog *> E.notify caption *> dispatch Poll
  submit m d = case d.kind of
    M.AgentSettings -> if not (D.matches "^[A-Z][A-Za-z0-9_.:#+|()&$^\\-]*$" $ D.s "name" d.fields) then E.modifyDialog d.identity _ {error=Dialog.nameHelp} else
      runDialog d (saved "Sapi saved.") $ E.api runtime m ("/api/agents/" <> B.encodeURI (D.s "id" d.record)) "PUT" $ D.json {name: D.get "name" d.fields, role: D.get "role" d.fields, manager: if D.s "manager" d.fields == "" then D.null else D.get "manager" d.fields, execution: D.json {mode: D.get "mode" d.fields}}
    M.GroupSettings -> runDialog d (saved "Group saved.") $ E.api runtime m ("/api/groups/" <> B.encodeURI (D.s "id" d.record)) "PUT" $ D.obj $ map (\key -> Tuple key $ D.get key d.fields) ["name","description","lead","members","revision"]
    M.GroupTask -> saveTask m d false
    M.RoutineStop -> routineCommand m d "stop"
    M.RoutineAnalyze -> routineCommand m d "analyze"
    M.RoutineStopAnalysis -> routineCommand m d "stop-analysis"
    M.DoctorAccess -> routineCommand m d "doctor-access"
    M.RoutineReject -> routineCommand m d "reject"
    M.RoutineDiscard -> dispatch CloseDialog *> dispatch RoutineDiscardChanges
    M.RoutineRun -> routineCommand m d "run"
    M.RoutineDelete -> routineCommand m d "delete"
    M.Reference kind -> if A.length (M.attachments m) >= 8 then E.modifyDialog d.identity _ {error="Attach up to 8 items per message."} else do
      H.modify_ _ {uploading=m.uploading+1}
      E.modifyDialog d.identity _ {busy=true}
      attach m $ E.api runtime m (M.ownerPath m <> "/attachments") "POST" $ D.json {kind, value: D.get "value" d.fields}
    M.DecisionPrompts -> dispatch $ Mutate ("/api/decision-prompts/" <> B.encodeURI (D.s "node" d.fields)) "PUT" (D.json {content: D.get "content" d.fields}) (SavedPrompt $ D.s "content" d.fields)
    M.CreateCorpora -> do
      E.modifyDialog d.identity _ {busy=true,error=""}
      E.attempt (C.json "/api/host/local" "POST" d.fields) \result -> do
        current <- H.get
        case result of
          Left failure -> E.modifyDialog d.identity _ {busy=false,error=message failure}
          Right value -> do
            let record = D.put "key" (D.get "id" value) value
            H.modify_ \s -> s {records=A.snoc s.records record}
            when ((_.identity <$> current.dialog) == Just d.identity) $ dispatch CloseDialog
            dispatch $ EnterCorpora $ D.s "key" record
    M.EditCorpora -> runDialog d (\_ -> dispatch CloseDialog *> dispatch LoadRegistry) do
      C.reconcile runtime (M.remote m) m.records m.active
      info <- C.request runtime (M.remote m) d.record "/api/corpora" "GET" D.null
      when (D.s "status" info == "archived") $ throwError $ error "Restore this archived corpora before sending. Your draft is kept here."
      current <- if D.s "status" info == "paused" then C.lifecycle runtime (M.remote m) d.record "activate" else pure info
      C.request runtime (M.remote m) d.record ("/api/agents/" <> B.encodeURI (D.s "chief_id" current) <> "/messages") "POST" $ D.json {text: D.get "message" d.fields}
    M.RelayAdd -> case parseRelay $ D.s "connection" d.fields of
      Left failure -> E.modifyDialog d.identity _ {error=failure}
      Right connection -> do
        let change = D.obj $ [Tuple "action" (D.text "save"),Tuple "name" (D.text $ S.trim $ D.s "name" d.fields),Tuple "connection" connection] <> if D.s "id" d.record == "" then [] else [Tuple "id" $ D.get "id" d.record]
        if D.s "id" d.record == D.s "selected" d.content && D.get "connection" d.record /= connection then confirmRelay d change (D.s "name" d.fields) else changeRelay d change
    _ -> pure unit
  saveTask m d deleted =
    let existing = D.s "id" d.record /= ""
        data_ = D.merge d.fields $ if existing then D.json {revision: D.get "revision" d.record, deleted, state: if D.b "deleted" d.record then D.text "backlog" else D.get "state" d.fields} else D.empty
    in runDialog d (saved $ if deleted then "Task deleted." else "Task saved.") $ E.api runtime m (M.ownerPath m <> "/tasks" <> if existing then "/" <> B.encodeURI (D.s "id" d.record) else "") (if existing then "PUT" else "POST") data_
  push kind record = do
    m <- H.get
    let next = Dialog.make kind record m
        content = fromMaybe D.null $ _.content <$> m.dialog
    H.modify_ _ {dialog=Just $ next {content=content},dialogStack=m.dialogStack <> fromMaybe [] (map A.singleton m.dialog),sequence=next.identity}
    E.sync
  relayList content = do
    m <- H.get
    let previous = A.find (( _ == M.RelayList) <<< _.kind) m.dialogStack
        record = maybe D.empty _.record previous
        next = Dialog.make M.RelayList record m
    H.modify_ _ {dialog=Just $ next {content=content},dialogStack=[],sequence=next.identity}
    E.sync
  changeRelay d data_ = runDialog d (\value -> when (D.b "reset" value) (resetRelay true) *> relayList value) $ C.json "/api/host/relays" "POST" $ D.put "revision" (D.get "revision" d.content) data_
  confirmRelay d change to = do
    let current = D.find (D.s "selected" d.content) $ D.rows "servers" d.content
    push M.RelaySwitch $ D.json {change, from: D.get "name" current, to}
  resetRelay broadcast = do
    m <- H.get
    liftEffect do
      clients <- Ref.read runtime.clients
      traverse_ R.close $ O.values clients
      Ref.write O.empty runtime.clients
      Ref.write O.empty runtime.credentials
      traverse_ (\r -> B.storageRemove $ "s4.snapshot." <> D.s "key" r) $ A.filter (( _ == "Remote") <<< D.s "location") m.records
    let locals = map (D.put "paired" $ D.bool false) $ A.filter (( _ == "Local") <<< D.s "location") m.records
        active = if D.s "location" (M.activeRecord m) == "Remote" then D.s "key" $ D.first locals else m.active
    H.modify_ _ {records=locals,active=active,online=false,epoch=m.epoch+1,polling=false,saving=false,nativeReporting=false,nativeReports=[]}
    void $ liftEffect $ C.write "s4.records" $ D.array locals
    void $ liftEffect $ C.write "s4.active" $ D.text active
    void $ liftEffect $ C.write "s4.lifecycle.pending" D.null
    void $ liftEffect $ C.write "s4.switch.pending" D.null
    when broadcast do
      uuid <- liftEffect B.uuid
      void $ liftEffect $ C.write "s4.relay-reset" $ D.text uuid
    dispatch LoadRegistry
  routineCommand m d action =
    let path = "/api/agents/" <> B.encodeURI (D.s "owner" d.record) <> "/routines" <> if action == "create" then "" else "/" <> B.encodeURI (D.s "id" d.record) <> "/" <> action
        body = D.json {operation: D.get "operation" d.record, revision: D.get "revision" d.record, title: D.get "title" d.fields, goal: D.get "goal" d.fields, criteria: A.filter (_ /= "") $ map S.trim $ S.split (S.Pattern "\n") $ D.s "criteria" d.fields, inputs: D.s "inputs" d.fields, run: D.get "run" d.record, reason: D.s "reason" d.fields, analysis:D.get "analysis" d.record, "case":D.at ["repair","id"] d.record, resources: (map (\path -> D.json {path,access:"read"}) $ A.filter (_ /= "") $ map S.trim $ S.split (S.Pattern "\n") $ D.s "readFiles" d.fields) <> (map (\path -> D.json {path,access:"write"}) $ A.filter (_ /= "") $ map S.trim $ S.split (S.Pattern "\n") $ D.s "writeFiles" d.fields)}
        done response = do
          dispatch CloseDialog
          when (m.selected == D.s "owner" d.record) $ dispatch $ RoutineNavigate $ if action == "run" then M.RoutineOccurrence (D.s "id" d.record) (D.s "run" response) else if action == "delete" then M.RoutineList true else if A.elem action ["analyze","stop-analysis","doctor-access"] then M.RoutineLatest (D.s "id" d.record) else M.RoutineOccurrence (D.s "id" d.record) (D.s "run" d.record)
          dispatch Poll
    in runDialog d done $ E.api runtime m path "POST" body
  commandAction command value = do
    m <- H.get
    case command of
      "create-agent" -> do
        dispatch $ SetScope N.All
        current <- H.get
        let template = D.s "creation_template" m.snapshot
        when (S.trim (M.draft current) == "") $ dispatch $ Draft template
        liftEffect $ B.focus "#message-input"
        latest <- H.get
        case CU.indexOf (S.Pattern "WORKFLOW") $ M.draft latest of
          Just index -> liftEffect $ B.selectRange "#message-input" index (index+8)
          Nothing -> pure unit
      "group-mention" -> case m.groupView of
        Nothing -> dispatch $ Select $ D.s "id" value
        Just group -> do
          dispatch $ Select group
          current <- H.get
          let old = M.draft current
              prefix = A.find (\name -> S.take (CU.length name) old == name) $ map (\id -> "@" <> D.s "name" (M.agent current id) <> " ") $ D.strings $ D.get "members" $ M.selected current
          dispatch $ Draft $ "@" <> D.s "name" value <> " " <> maybe old (\p -> CU.drop (CU.length p) old) prefix
          liftEffect $ B.focus "#message-input"
      "relay-reset" -> resetRelay false
      "install-prompt" -> void $ H.fork $ void $ H.liftAff $ try $ toAffE B.installPrompt
      _ -> traverse_ (\d -> unless d.busy $ dialogCommand m d command value) m.dialog
  dialogCommand m d command value = case command of
    "names" -> do
      E.modifyDialog d.identity (\s -> s {identity=s.identity+1})
      H.modify_ (\s -> s {sequence=s.sequence+1})
    "delete-task" -> saveTask m d true
    "relay-add" -> push M.RelayAdd D.empty
    "relay-back" -> case A.unsnoc m.dialogStack of
      Just {init,last} -> do
        let next = last {identity=m.sequence+1}
        H.modify_ _ {dialog=Just next,dialogStack=init,sequence=next.identity}
        E.sync
      Nothing -> dispatch CloseDialog
    "relay-select" -> when (D.s "id" value /= D.s "selected" d.content) $ confirmRelay d (D.json {action: "switch", id: D.get "id" value}) (D.s "name" value)
    "relay-confirm" -> changeRelay d $ D.put "confirm_revoke" (D.bool true) $ D.get "change" d.record
    "relay-edit" -> runDialog d (push M.RelayAdd) $ C.json ("/api/host/relays/" <> B.encodeURI (D.s "id" value)) "GET" D.null
    "relay-archive" -> changeRelay d $ D.json {action: if D.b "archived" value then "restore" else "archive", id: D.get "id" value}
    "relay-import" -> runDialog d (\imported -> E.modifyDialog d.identity \current -> current {fields=D.put "connection" (D.get "raw" imported) current.fields,error=""}) do
      file <- toAffE $ B.chooseFile "application/json,.json" 4096
      if not (D.valid file) then pure (D.json {raw: D.get "connection" d.fields}) else do
        let raw = D.str $ B.base64Decode $ D.s "data" file
        case parseRelay raw of
          Left failure -> throwError $ error failure
          Right _ -> pure $ D.json {raw}
    "relay-done" -> do
      let current = D.find (D.s "selected" d.content) $ D.rows "servers" d.content
      if D.s "key" d.record == "" then dispatch CloseDialog else if D.b "beta" current && not (D.b "beta_accepted" d.content) then push M.RelayTerms d.record else dispatch $ OpenDialog M.HostPairing $ D.put "_ready" (D.bool true) d.record
    "relay-accept" -> do
      E.modifyDialog d.identity _ {busy=true}
      receive d.identity (\_ -> dispatch $ OpenDialog M.HostPairing $ D.put "_ready" (D.bool true) d.record) $
        C.json "/api/host/relays" "POST" $ D.json {revision: D.get "revision" d.content, action: "accept-beta", accepted: true}
    _ | S.take 5 command == "host-" -> do
      let op = S.drop 5 command
      proceed <- if op == "revoke" || op == "replace" then liftEffect $ B.confirm $ if op == "revoke" then "Revoke this remote access? It will need to pair again." else "Replace the trusted remote device? The previous device will lose access." else pure true
      when proceed $ runDialog d (\result -> do
        E.modifyDialog d.identity _ {error=if D.valid (D.get "relay_deleted" result) && not (D.b "relay_deleted" result) then "Revoked locally. Relay could not be reached." else ""}
        dispatch $ HostPairPoll d.identity) do
        previous <- if op == "revoke" || op == "replace" then C.request runtime (M.remote m) d.record "/api/remote/revoke" "POST" D.empty else pure D.empty
        if op == "pair" || op == "replace" then C.request runtime (M.remote m) d.record "/api/remote/pair" "POST" (D.json {saved: true}) else if op == "approve" then C.request runtime (M.remote m) d.record "/api/remote/approve" "POST" (D.json {peer: D.get "peer" value}) else pure previous
    _ -> pure unit

parseRelay :: String -> Either String Json
parseRelay raw =
  let value = D.parse raw
      url = B.parseURL "about:blank" $ D.s "relay" value
      valid = CU.length raw <= 4096 && D.s "format" value == "sapiens-relay-connection-v1" && D.s "username" url == "" && D.s "password" url == "" && D.s "search" url == "" && D.s "hash" url == "" && D.s "path" url == "/" && (D.s "protocol" url == "https:" || D.s "protocol" url == "http:" && A.elem (D.s "host" url) ["localhost","127.0.0.1","[::1]"])
      base = D.json {format: D.get "format" value, relay: D.get "origin" url}
  in if not valid then Left "Enter valid relay connection JSON." else if D.b "beta" value && D.s "token" value == "" then Right $ D.put "beta" (D.bool true) base else if D.matches "^[\\x21-\\x7e]{32,256}$" $ D.s "token" value then Right $ D.put "token" (D.get "token" value) base else Left "Enter valid relay connection JSON."
