module Sapiens.CorporaEffects where

import Prelude
import Control.Promise (toAffE)
import Data.Array as A
import Data.Either (Either(..))
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.String as S
import Data.Tuple (Tuple(..))
import Effect.Aff (Milliseconds(..), delay, try, throwError)
import Effect.Class (liftEffect)
import Effect.Exception (error, message)
import Effect.Ref as Ref
import Halogen as H
import Foreign.Object as O
import Libs4.Browser as B
import Sapiens.Action (Action(..))
import Sapiens.Connections as C
import Sapiens.Data as D
import Sapiens.Effects as E
import Sapiens.Model as M
import Sapiens.Remote as R

handle :: C.Runtime -> E.Dispatch -> Action -> E.UI Boolean
handle runtime dispatch action = case action of
  LoadRegistry -> yes do
    initial <- H.get
    when (initial.active == "") do
      records <- D.arr <$> liftEffect (C.read "s4.records")
      active <- D.str <$> liftEffect (C.read "s4.active")
      let record = fromMaybe D.null $ A.find (( _ == active) <<< D.s "key") records
      when (D.valid record) do
        cached <- liftEffect $ C.read $ "s4.snapshot." <> active
        H.modify_ _ {records=records,active=active}
        when (D.valid cached) do
          decorated <- liftEffect $ C.decorate active cached
          H.modify_ $ M.reset decorated
          E.restoreScroll
    requested <- H.get
    unless requested.installRequired $ E.attempt (C.registry runtime (M.remote requested) $ D.s "origin" requested.env) \result -> E.inEpoch requested.epoch \m -> case result of
      Left failure -> do
        H.modify_ _ {feedback=message failure,online=false}
        E.later 3000.0 $ dispatch LoadRegistry
      Right registry -> do
        saved <- D.str <$> liftEffect (C.read "s4.active")
        let previous = fromMaybe D.null $ A.find (( _ == m.active) <<< D.s "key") registry.records
            preferred = fromMaybe (D.first registry.records) $ A.find (( _ == saved) <<< D.s "key") registry.records
            record = if D.valid previous then previous else preferred
        H.modify_ _ {records=registry.records,localAvailable=registry.localAvailable}
        void $ liftEffect $ C.write "s4.records" $ D.array registry.records
        when (D.valid record) do
          if D.s "key" record /= m.active then enter record else do
            H.modify_ _ {polling=true}
            current <- H.get
            connect current record
        unless (D.valid record) $ H.modify_ _ {active="",online=false,feedback="",snapshot=D.empty,agents=[]}
        current <- H.get
        when (not (M.remote current) && S.contains (S.Pattern "pairing=local") (D.s "search" current.env)) do
          liftEffect $ B.historyReplace $ D.s "path" current.env
          H.modify_ _ {env=D.put "search" (D.text "") current.env}
          traverse_ (dispatch <<< OpenDialog M.HostPairing) $ A.find (( _ == "Local") <<< D.s "location") registry.records
        E.sync
        unless (D.valid record) $ dispatch CheckSetup
  EnterCorpora key -> yes do
    m <- H.get
    unless (m.busy || key == m.active) $ traverse_ enter $ A.find (\r -> D.s "key" r == key && D.s "status" r /= "archived") m.records
  ShowArchived show_ -> yes $ H.modify_ _ {showArchived=show_}
  Lifecycle command -> yes do
    m <- H.get
    unless (m.busy || not m.online) do
      H.modify_ _ {busy=true}
      E.attempt (C.reconcile runtime (M.remote m) m.records m.active *> C.lifecycle runtime (M.remote m) (M.activeRecord m) command) \result -> E.inEpoch m.epoch \_ -> do
        H.modify_ _ {busy=false,menu=Nothing}
        case result of
          Left failure -> do
            H.modify_ _ {online=false}
            dispatch $ OpenDialog M.Notice $ D.text $ message failure
          Right info -> H.modify_ \s -> s {records=map (\r -> if D.s "key" r == s.active then D.merge r info else r) s.records,snapshot=D.put "corpora" info s.snapshot}
        dispatch Poll
        E.sync
      E.sync
  Pair -> yes do
    m <- H.get
    case m.dialog of
      Just d | d.kind == M.ConnectRemote && not d.busy -> case R.decode (D.s "invitation" d.fields) of
        Left failure -> E.modifyDialog d.identity _ {error=failure}
        Right invitation -> do
          dispatch StopScan
          E.modifyDialog d.identity _ {busy=true,error="Waiting for host approval…"}
          void $ H.fork do
            result <- H.liftAff $ try $ if M.remote m then do
              transport <- R.invitation invitation
              liftEffect $ Ref.write (Just transport) runtime.pendingPair
              pure $ Left transport
              else Right <$> C.json "/api/host/pair" "POST" invitation
            case result of
              Left failure -> receivePair d.identity $ Left failure
              Right (Left transport) -> do
                session <- liftEffect $ Ref.read transport.session
                code <- H.liftAff $ try $ R.confirmation session
                case code of
                  Left failure -> receivePair d.identity $ Left failure
                  Right value -> do
                    pairCode d.identity value ""
                    paired <- H.liftAff $ try $ R.authenticate transport true
                    current <- H.get
                    if (_.identity <$> current.dialog) /= Just d.identity then liftEffect $ R.close transport else case paired of
                      Left failure -> receivePair d.identity $ Left failure
                      Right _ -> do
                        record <- H.liftAff $ C.register runtime transport
                        receivePair d.identity $ Right record
              Right (Right value) -> do
                current <- H.get
                if (_.identity <$> current.dialog) /= Just d.identity then void $ H.liftAff $ try $ C.json ("/api/host/pair/" <> B.encodeURI (D.s "id" value)) "POST" D.empty else do
                  liftEffect $ Ref.write (D.s "id" value) runtime.pendingPairId
                  pairCode d.identity (D.s "code" value) (D.s "id" value)
                  pollPair d.identity value
      _ -> pure unit
  ScanQR -> yes do
    m <- H.get
    case m.dialog of
      Just d | d.kind == M.ConnectRemote -> do
        dispatch StopScan
        E.modifyDialog d.identity \s -> s {fields=D.put "scanning" (D.bool true) s.fields,error=""}
        void $ H.fork do
          result <- H.liftAff $ try $ toAffE $ B.cameraStart "#qr-camera"
          current <- H.get
          case result of
            Left failure -> dispatch StopScan *> E.modifyDialog d.identity _ {error=if D.matches "NotAllowed|denied" (message failure) then "Camera access is off. Upload a QR image." else "Camera unavailable. Upload a QR image."}
            Right camera -> if (_.identity <$> current.dialog) /= Just d.identity || not (maybe false (D.b "scanning" <<< _.fields) current.dialog) then liftEffect $ B.cameraStop camera else do
              liftEffect $ Ref.write (Just camera) runtime.camera
              dispatch $ ScanFrame d.identity
      _ -> pure unit
  ScanFrame id -> yes $ E.inDialog id \d -> when (D.b "scanning" d.fields) do
      result <- liftEffect $ B.cameraQR "#qr-camera" 1000
      if D.s "data" result == "" then E.later 200.0 (dispatch $ ScanFrame id) else receiveQR id $ Right result
  UploadQR -> yes do
    m <- H.get
    E.withDialog \d -> E.attempt (do
      unless (D.b "secure" m.env) $ throwError $ error "Open this app over HTTPS."
      file <- toAffE $ B.chooseFile "image/png,image/jpeg,image/webp" 10485760
      if not (D.valid file) then pure D.null else toAffE $ B.decodeQR ("data:" <> D.s "mime" file <> ";base64," <> D.s "data" file) 1600) $ receiveQR d.identity
  StopScan -> yes do
    liftEffect do
      Ref.read runtime.camera >>= traverse_ B.cameraStop
      Ref.write Nothing runtime.camera
    H.modify_ \m -> m {dialog=map (\d -> d {fields=D.put "scanning" (D.bool false) d.fields}) m.dialog}
  HostPairPoll id -> yes do
    requested <- H.get
    E.inDialog id \dialog -> when (dialog.kind == M.HostPairing) $
        E.attempt (C.request runtime (M.remote requested) dialog.record "/api/remote" "GET" D.null) \result -> E.inDialog id \d -> do
          m <- H.get
          case result of
            Left failure -> E.modifyDialog id _ {error=message failure}
            Right value -> do
              if not (D.valid d.content) && not (D.b "paired" value || D.b "enabled" value || D.b "_ready" d.record) then dispatch $ OpenDialog M.RelayList d.record else do
                let invitation = D.get "invitation" value
                    link = if D.n "expires" invitation > m.now/1000.0 && D.b "enabled" value then R.invitationLink invitation else ""
                    dates = D.obj $ map (\key -> Tuple key $ D.text $ if D.n key value <= 0.0 then "Not recorded" else B.formatDate "" (D.json {dateStyle: "medium", timeStyle: "long"}) (B.timestamp $ D.n key value * 1000.0)) ["paired_at","last_connected_at"]
                E.modifyDialog id \current -> current {content=value,title=if D.b "paired" value then "Edit Pairing" else "New Pairing",fields=D.put "dates" dates $ D.put "expiration" (D.text $ B.timestamp $ D.n "expires" invitation*1000.0) $ D.put "link" (D.text link) current.fields}
                H.modify_ \s -> s {records=map (\r -> if D.s "key" r == D.s "key" d.record then D.put "paired" (D.get "paired" value) r else r) s.records}
                when (link /= "" && link /= D.s "link" d.fields) $ E.attempt (toAffE $ B.qrImage link) $ receiveImage id link
          E.later 2500.0 $ dispatch $ HostPairPoll id
  Install -> yes $ dispatch $ OpenDialog M.Installation D.empty
  _ -> pure false
  where
  yes operation = operation *> pure true
  receiveImage id link result = E.inDialog id \d -> when (D.s "link" d.fields == link) $ case result of
    Left failure -> E.modifyDialog id _ {error=message failure}
    Right uri -> E.modifyDialog id \current -> current {fields=D.put "qr" (D.text uri) current.fields}
  receiveQR id result = E.inDialog id \_ -> case result of
    Left failure -> E.modifyDialog id _ {error=message failure}
    Right value | D.valid value -> if D.s "data" value == "" then E.modifyDialog id _ {error="No supported QR found in this image."} else do
      dispatch StopScan
      dispatch $ Field "invitation" $ D.s "data" value
      dispatch Pair
    _ -> pure unit
  receivePair id result = E.inDialog id \_ -> case result of
    Left failure -> do
      H.liftAff $ C.cancelPairing runtime
      E.modifyDialog id \d -> d {busy=false,error=message failure,fields=D.remove "code" d.fields}
    Right record -> do
      liftEffect $ Ref.write "" runtime.pendingPairId
      H.modify_ \s -> s {records=A.snoc (A.filter (( _ /= D.s "key" record) <<< D.s "key") s.records) record}
      current <- H.get
      void $ liftEffect $ C.write "s4.records" $ D.array current.records
      dispatch CloseDialog
      enter record
  pairCode id code pending = do
    E.modifyDialog id \d -> d {fields=D.put "code" (D.text code) $ D.put "pending" (D.text pending) d.fields}
  connect m record = E.refresh runtime dispatch m $ C.authenticate runtime (M.remote m) record *> C.request runtime (M.remote m) record "/api/state" "GET" D.null
  enter record = do
    m <- H.get
    E.captureScroll
    E.persistLocal
    cached <- liftEffect $ C.read $ "s4.snapshot." <> D.s "key" record
    snapshot <- liftEffect $ C.decorate (D.s "key" record) cached
    let current = m {active=D.s "key" record,online=false,busy=false,polling=true,saving=false,nativeReporting=false,nativeReports=[],epoch=m.epoch+1,menu=m.menu,dialog=case m.dialog of
            Just d | d.kind == M.Onboarding -> Just d
            _ -> Nothing
          ,scrollPosition=D.null}
    H.put $ if D.valid cached then M.reset snapshot current else current {snapshot=D.empty,agents=[],documents=O.empty}
    void $ liftEffect $ C.write "s4.active" $ D.get "key" record
    next <- H.get
    connect next record
    E.restoreScroll
    E.sync
  pollPair id value = E.inDialog id \_ -> do
    if D.s "state" value == "waiting" then do
      H.liftAff $ delay $ Milliseconds 1000.0
      E.inDialog id \_ -> do
        result <- H.liftAff $ try $ C.json ("/api/host/pair/" <> B.encodeURI (D.s "id" value)) "GET" D.null
        case result of
          Left failure -> receivePair id $ Left failure
          Right next -> pollPair id $ D.put "id" (D.get "id" value) next
    else if D.s "state" value == "approved" then receivePair id $ Right $ D.put "key" (D.at ["corpora","id"] value) $ D.get "corpora" value
    else receivePair id $ Left $ error $ D.fallback "Connection cancelled" $ D.s "error" value
