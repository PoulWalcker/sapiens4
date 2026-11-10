module Sapiens.CorporaEffects where

import Sapiens.Form as F
import Prelude
import Control.Promise (toAffE)
import Data.Array as A
import Data.Either (Either(..), hush)
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Tuple (Tuple(..))
import Data.Traversable (traverse)
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
import Sapiens.Domain as X
import Sapiens.Effects as E
import Sapiens.Model as M
import Sapiens.Update as U
import Sapiens.Remote as R

handle :: C.Runtime -> E.Dispatch -> Action -> E.UI Boolean
handle runtime dispatch action = case action of
  LoadRegistry -> yes do
    initial <- H.get
    when (initial.active == "") do
      records <- map X.corpora <<< D.arr <$> liftEffect (C.read "s4.records")
      active <- D.str <$> liftEffect (C.read "s4.active")
      let record = fromMaybe (X.corpora D.empty) $ A.find (( _ == active) <<< _.key) records
      when (record.id /= "") do
        cached <- liftEffect $ C.read $ "s4.snapshot." <> active
        H.modify_ _ {records=records,active=active,restoredCorpora=true}
        when (D.valid cached) do
          decorated <- liftEffect $ C.decorate active cached
          H.modify_ $ M.reset decorated
          E.restoreScroll
    requested <- H.get
    unless requested.installRequired $ E.attempt (C.registry runtime (M.remote requested) $ requested.env.origin) \result -> E.inEpoch requested.epoch \_ -> case result of
      Left failure -> do
        H.modify_ \s -> s {feedback=message failure,connection=M.connectionResult false s.connection}
        E.later 3000.0 $ dispatch LoadRegistry
      Right registry -> do
        saved <- D.str <$> liftEffect (C.read "s4.active")
        when (requested.active == "") $ H.modify_ _ {restoredCorpora=A.any ((_ == saved) <<< _.key) registry.records}
        E.runUpdate runtime dispatch $ U.RegistryReceived requested.epoch saved registry
  EnterCorpora key -> yes do
    m <- H.get
    unless ((M.busy m) || key == m.active) $ traverse_ enter $ A.find (\r -> r.key == key && r.status /= X.Archived) m.records
  ShowArchived show_ -> yes $ H.modify_ _ {showArchived=show_}
  Lifecycle command -> yes $ E.runUpdate runtime dispatch $ U.Lifecycle command
  Pair -> yes do
    m <- H.get
    case m.dialog of
      Just d | d.kind == M.ConnectRemote && not (M.dialogBusy d) -> case R.decode (d.fields.invitation) of
        Left failure -> E.modifyDialog d.identity (M.setDialogError $ failure)
        Right invitation -> do
          dispatch StopScan
          E.modifyDialog d.identity _ {status=M.Submitting "Waiting for host approval…"}
          void $ H.fork do
            result <- H.liftAff $ try $ if M.remote m then do
              transport <- R.invitation invitation
              liftEffect $ Ref.write (C.RemotePairing transport) runtime.pairing
              pure $ Left transport
              else Right <$> C.json "/api/host/pair" "POST" (D.json invitation)
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
                  liftEffect $ Ref.write (C.LocalPairing $ D.s "id" value) runtime.pairing
                  pairCode d.identity (D.s "code" value) (D.s "id" value)
                  pollPair d.identity value
      _ -> pure unit
  ScanQR -> yes do
    m <- H.get
    case m.dialog of
      Just d | d.kind == M.ConnectRemote -> do
        dispatch StopScan
        E.modifyDialog d.identity \s -> (M.setDialogError "" s) {fields=s.fields {scanning=true}}
        void $ H.fork do
          result <- H.liftAff $ try $ toAffE $ B.cameraStart "#qr-camera"
          current <- H.get
          case result of
            Left failure -> dispatch StopScan *> E.modifyDialog d.identity (M.setDialogError $ if D.matches "NotAllowed|denied" (message failure) then "Camera access is off. Upload a QR image." else "Camera unavailable. Upload a QR image.")
            Right camera -> if (_.identity <$> current.dialog) /= Just d.identity || not (maybe false (_.scanning <<< _.fields) current.dialog) then liftEffect $ B.cameraStop camera else do
              liftEffect $ Ref.write (Just camera) runtime.camera
              dispatch $ ScanFrame d.identity
      _ -> pure unit
  ScanFrame id -> yes $ E.inDialog id \d -> when (d.fields.scanning) do
      result <- liftEffect $ B.cameraQR "#qr-camera" 1000
      if D.s "data" result == "" then E.later 200.0 (dispatch $ ScanFrame id) else receiveQR id $ Right result
  UploadQR -> yes do
    m <- H.get
    E.withDialog \d -> E.attempt (do
      unless (m.env.secure) $ throwError $ error "Open this app over HTTPS."
      selected <- B.chooseFile "image/png,image/jpeg,image/webp" 10485760
      traverse (\file -> toAffE $ B.decodeQR ("data:" <> fromMaybe "application/octet-stream" file.mime <> ";base64," <> file.data) 1600) selected) \result -> case result of
        Left failure -> receiveQR d.identity $ Left failure
        Right decoded -> traverse_ (receiveQR d.identity <<< Right) decoded
  StopScan -> yes do
    liftEffect do
      Ref.read runtime.camera >>= traverse_ B.cameraStop
      Ref.write Nothing runtime.camera
    H.modify_ \m -> m {dialog=map (\d -> d {fields=d.fields {scanning=false}}) m.dialog}
  HostPairPoll id -> yes do
    requested <- H.get
    E.inDialog id \dialog -> when (dialog.kind == M.HostPairing) $
        E.attempt (C.request runtime (M.remote requested) (F.contextCorpora dialog.record) "/api/remote" "GET" D.null) \result -> E.inDialog id \d -> do
          now <- liftEffect B.now
          H.modify_ _ {now=now}
          case result of
            Left failure -> E.modifyDialog id (M.setDialogError $ message failure)
            Right value -> do
              if d.content == F.NoContent && not (D.b "paired" value || D.b "enabled" value || F.contextReady d.record) then dispatch $ OpenDialog M.RelayList d.record else do
                let status = F.pairStatus value
                    invitation = status.invitation
                    link = if invitation.expires > now/1000.0 && D.b "enabled" value then R.invitationLink invitation else ""
                    dates = O.fromFoldable $ map (\key -> Tuple key $ if D.n key value <= 0.0 then "Not recorded" else B.formatDate "" (D.json {dateStyle: "medium", timeStyle: "long"}) (B.timestamp $ D.n key value * 1000.0)) ["paired_at","last_connected_at"]
                E.modifyDialog id \current -> current {content=F.Pairing status,title=if D.b "paired" value then "Edit Pairing" else "New Pairing",fields=current.fields {dates=dates,expiration=B.timestamp $ invitation.expires*1000.0,link=link}}
                H.modify_ \s -> s {records=map (\r -> if r.key == (F.contextCorpora d.record).key then r {paired=D.b "paired" value} else r) s.records}
                when (link /= "" && link /= d.fields.link) $ E.attempt (toAffE $ B.qrImage link) $ receiveImage id link
          E.later 2500.0 $ dispatch $ HostPairPoll id
  Install -> yes $ dispatch $ OpenDialog M.Installation F.NoRecord
  _ -> pure false
  where
  yes operation = operation *> pure true
  receiveImage id link result = E.inDialog id \d -> when (d.fields.link == link) $ case result of
    Left failure -> E.modifyDialog id (M.setDialogError $ message failure)
    Right uri -> E.modifyDialog id \current -> current {fields=current.fields {qr=uri}}
  receiveQR id result = E.inDialog id \_ -> case result of
    Left failure -> E.modifyDialog id (M.setDialogError $ message failure)
    Right value | D.valid value -> if D.s "data" value == "" then E.modifyDialog id (M.setDialogError $ "No supported QR found in this image.") else do
      dispatch StopScan
      dispatch $ Field F.Invitation $ D.s "data" value
      dispatch Pair
    _ -> pure unit
  receivePair id result = E.inDialog id \_ -> case result of
    Left failure -> do
      H.liftAff $ C.cancelPairing runtime
      E.modifyDialog id \d -> d {status=M.Rejected $ message failure,fields=d.fields {code=""}}
    Right record -> do
      liftEffect $ Ref.write C.NoPairing runtime.pairing
      H.modify_ \s -> s {records=A.snoc (A.filter (( _ /= record.key) <<< _.key) s.records) record}
      current <- H.get
      void $ liftEffect $ C.write "s4.records" $ D.json current.records
      dispatch CloseDialog
      enter record
  pairCode id code pending = do
    E.modifyDialog id \d -> d {fields=d.fields {code=code,pending=pending}}
  enter record = do
    E.captureScroll
    E.persistLocal
    cached <- liftEffect $ C.read $ "s4.snapshot." <> record.key
    snapshot <- liftEffect $ C.decorate record.key cached
    E.runUpdate runtime dispatch $ U.Entered record $ hush $ X.decodeSnapshot snapshot
  pollPair id value = E.inDialog id \_ -> do
    if D.s "state" value == "waiting" then do
      H.liftAff $ delay $ Milliseconds 1000.0
      E.inDialog id \_ -> do
        result <- H.liftAff $ try $ C.json ("/api/host/pair/" <> B.encodeURI (D.s "id" value)) "GET" D.null
        case result of
          Left failure -> receivePair id $ Left failure
          Right next -> pollPair id $ D.put "id" (D.get "id" value) next
    else if D.s "state" value == "approved" then receivePair id $ Right $ (X.corpora $ D.get "corpora" value) {key=D.str $ D.at ["corpora","id"] value}
    else receivePair id $ Left $ error $ D.fallback "Connection cancelled" $ D.s "error" value
