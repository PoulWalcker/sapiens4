module Sapiens.Voice where

import Prelude
import Control.Promise (toAffE)
import Data.Array as A
import Data.Either (Either(..), either)
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.String as S
import Effect.Class (liftEffect)
import Effect.Exception (message)
import Foreign.Object as O
import Halogen as H
import Halogen.Subscription as HS
import Libs4.Browser as B
import Sapiens.Action (Action(..))
import Sapiens.Connections as C
import Sapiens.Data as D
import Sapiens.Domain as X
import Sapiens.Effects as E
import Sapiens.Model as M

-- | Seconds of audio per streamed piece. The host joins pieces and cuts one-minute voice messages.
pieceSeconds :: Int
pieceSeconds = 2

-- | A piece that keeps failing to upload is retried this many times before the recording is dropped.
maxAttempts :: Int
maxAttempts = 5

-- | Pieces allowed to wait for upload (15 × 2 s = 30 s of audio). If uploads fall further behind,
-- | the connection cannot keep up, so recording stops instead of growing the queue without limit.
maxBacklog :: Int
maxBacklog = 15

-- | Recording, ordered upload, the live timer, stopping, cancelling, sending and retrying. Returns true
-- | when it handled the action.
-- | Finished one-minute voice messages come back from the host and wait in the composer. Stop keeps them
-- | there for review; Send (in the recording bar) stops and sends them as soon as recording finishes. On Send, each voice
-- | message becomes its own chat message, in order, as soon as its own transcript is ready: the first
-- | goes at once while later ones are still being transcribed. A failed transcript is never sent: sending
-- | stops there so the person can retry it or remove it.
handle :: C.Runtime -> E.Dispatch -> Action -> E.UI Boolean
handle runtime dispatch action = case action of
  VoiceStart -> do
    m <- H.get
    case m.voice of
      Just v | v.owner /= m.selected -> E.notify "Finish or cancel the voice recording in the other chat first."
      Just _ -> pure unit
      Nothing | M.ordinaryAttachmentCount m >= 8 -> E.notify "Remove an attachment to make room for a voice message."
      Nothing -> when (m.selected /= "") do
        startedAt <- liftEffect B.now
        { emitter, listener } <- liftEffect HS.create
        -- Every piece is decoded once, here where the recorder hands it over; the rest of the app only sees typed pieces.
        subscription <- H.subscribe (either VoicePieceRejected VoicePiece <<< X.voicePiece <$> emitter)
        H.modify_ _ { voice = Just { owner: m.selected, path: M.ownerPath m <> "/voice", epoch: m.epoch, phase: M.Starting, subscription
                                   , queue: [], sending: false, attempts: 0, pieces: 0
                                   , recording: "", startedAt, elapsed: 0.0 } }
        E.later 1000.0 (dispatch (VoiceTick subscription))
        E.attempt (toAffE $ B.voiceStart "/voice-worklet.js" pieceSeconds (HS.notify listener)) case _ of
          -- Like the success branch: only the recording that asked may react. A late failure from a
          -- cancelled recording must not clear a newer one.
          Left err -> do
            H.unsubscribe subscription
            H.gets _.voice >>= case _ of
              Just w | w.subscription == subscription -> do
                H.modify_ _ { voice = Nothing }
                E.notify ("Microphone unavailable: " <> message err)
              _ -> pure unit
          Right recorder -> H.gets _.voice >>= case _ of
            Just w | w.subscription == subscription -> case w.phase of
              M.Starting -> H.modify_ _ { voice = Just w { phase = M.Recording recorder } }
              -- Stop or Send was pressed before the microphone was ready.
              _ -> stopRecorder recorder
            _ -> stopRecorder recorder
    pure true
  VoiceTick subscription -> do
    m <- H.get
    case m.voice of
      Just v | v.subscription == subscription ->
        if v.epoch /= m.epoch then abandon v else unless (M.isStopping v.phase) do
          now <- liftEffect B.now
          H.modify_ \s -> s { voice = map (_ { elapsed = now - v.startedAt }) s.voice }
          E.later 1000.0 (dispatch (VoiceTick subscription))
      _ -> pure unit
    pure true
  VoiceStop -> do
    stopWith M.Review
    pure true
  VoiceStopSend -> do
    stopWith M.SendAfter
    pure true
  VoiceCancel -> do
    H.gets _.voice >>= traverse_ \v -> do
      abandon v
      E.inEpoch v.epoch \_ -> do
        H.modify_ \m -> m { attachments = O.update (Just <<< A.filter (not <<< fromRecording v.recording)) v.owner m.attachments }
        E.save dispatch
        cancelOnHost v
    pure true
  VoiceSend -> do
    m <- H.get
    let owner = m.selected
        recordingHere = maybe false (\v -> v.owner == owner) m.voice
    when (not (O.member owner m.outboxes) && not recordingHere && M.online m) do
      let items = M.attachments m
          voices = map _.id $ A.filter X.isVoice items
          others = map _.id $ A.filter (not <<< X.isVoice) items
      H.modify_ \s -> s { outboxes = O.insert owner { epoch: m.epoch, path: M.ownerPath m <> "/messages", text: S.trim (M.draft m), others, voices, sending: false } s.outboxes }
      flush owner
    pure true
  VoiceFlush owner -> do
    flush owner
    pure true
  VoicePiece piece -> do
    H.gets _.voice >>= traverse_ \v ->
      -- A recording has one id, taken from its first piece. A piece from any other recording is stale and ignored.
      when (v.recording == "" || v.recording == piece.recording) $
        if A.length v.queue >= maxBacklog then do
          abandon v
          E.notify "Recording stopped: the connection is too slow to upload the voice in time. Finished voice messages stay in the composer."
        else do
          H.modify_ \m -> m { voice = map (\w -> w { queue = A.snoc w.queue piece, recording = piece.recording }) m.voice }
          drain
    pure true
  VoicePieceRejected reason -> do
    H.gets _.voice >>= traverse_ \v -> do
      abandon v
      cancelOnHost v
      E.notify ("Voice recording stopped: the recorder sent a damaged piece (" <> reason <> ").")
    pure true
  VoiceRetry id -> do
    m <- H.get
    let owner = m.selected
    E.attempt (E.api runtime m (M.ownerPath m <> "/attachments/" <> id <> "/transcribe") "POST" (D.json {})) \result -> E.inEpoch m.epoch \_ -> case result of
      Left err -> E.notify ("Could not retry the transcription: " <> message err)
      Right reply -> do
        let row = X.attachment reply
        H.modify_ \s -> s { attachments = O.update (Just <<< map (\item -> if item.id == row.id then row else item)) owner s.attachments }
    pure true
  _ -> pure false
  where
  stopRecorder recorder = E.attempt (toAffE $ B.voiceStop recorder) (const (pure unit))
  -- Stop once: the recorder sends its final piece, and 'finish' then reviews or sends.
  stopWith after = H.gets _.voice >>= traverse_ \v -> unless (M.isStopping v.phase) do
    H.modify_ _ { voice = Just v { phase = M.Stopping after } }
    traverse_ stopRecorder (M.phaseRecorder v.phase)
  fromRecording recording item = recording /= "" && maybe false (\note -> note.recording == recording) (X.voiceOf item)
  -- Tell the host this recording is cancelled: it deletes the audio and refuses any late piece.
  cancelOnHost v = unless (v.recording == "") $ E.inEpoch v.epoch \m -> do
    E.attempt (E.api runtime m (v.path <> "/" <> v.recording <> "/cancel") "POST" (D.json {})) \result -> E.inEpoch v.epoch \_ -> case result of
      Left err -> E.notify ("The cancelled recording could not be removed from the computer: " <> message err)
      Right _ -> dispatch Poll
  drain = H.get >>= \m -> traverse_ (step m) m.voice
  step m v
    | m.epoch /= v.epoch = abandon v
    | v.sending = pure unit
    | otherwise = traverse_ (upload m v) (A.head v.queue)
  upload m v piece = do
    H.modify_ _ { voice = Just v { sending = true } }
    E.attempt (E.api runtime m v.path "POST" piece.wire) \result -> H.get >>= \current -> case current.voice of
      Just w | w.subscription == v.subscription && w.epoch /= current.epoch -> abandon w
      -- The same recording is still running: a reply from a cancelled or replaced recording is ignored.
      Just w | w.subscription == v.subscription -> case result of
        Right reply -> do
          addCuts w (D.rows "messages" reply)
          if piece.final then finish w else do
            H.modify_ \s -> s { voice = map (\x -> x { queue = A.drop 1 x.queue, sending = false, attempts = 0, pieces = x.pieces + 1 }) s.voice }
            drain
        Left err
          | w.attempts + 1 >= maxAttempts -> do
              abandon w
              E.notify ("Voice message could not be sent: " <> message err)
          | otherwise -> do
              H.modify_ \s -> s { voice = map (\x -> x { sending = false, attempts = x.attempts + 1 }) s.voice }
              E.later 1000.0 drain
      _ -> pure unit
  -- Each finished voice message joins the composer's attachments for review (no duplicates on a resend).
  addCuts v replies = unless (A.null replies) do
    let rows = map X.attachment replies
    H.modify_ \m -> m { attachments = O.alter (\current -> let items = fromMaybe [] current in Just (items <> A.filter (\row -> not (A.any ((_ == row.id) <<< _.id) items)) rows)) v.owner m.attachments }
    E.save dispatch
  -- Send this chat's next voice message once its transcript is done; keep waiting while Whisper works.
  -- A failed transcript stops sending (the Sapi would get no words). The person's typed text and other
  -- attachments travel with the first voice message.
  flush owner = H.get >>= \m -> traverse_ (flushWith m owner) (O.lookup owner m.outboxes)
  flushWith m owner o
    -- Started in another corpora: never send it through this one. Its voice messages stay drafts there.
    | o.epoch /= m.epoch = H.modify_ \s -> s { outboxes = O.delete owner s.outboxes }
    | otherwise = unless o.sending $ case A.head o.voices of
      Nothing -> do
        H.modify_ \s -> s { outboxes = O.delete owner s.outboxes }
        dispatch Poll
      Just voiceId -> case A.find ((_ == voiceId) <<< _.id) (fromMaybe [] $ O.lookup owner m.attachments) of
        -- Removed from the composer meanwhile: skip it.
        Nothing -> do
          H.modify_ \s -> s { outboxes = O.update (\x -> Just x { voices = A.drop 1 x.voices }) owner s.outboxes }
          flush owner
        Just item -> case map _.transcription (X.voiceOf item) of
          Just (X.Transcribed _) -> sendVoice m owner o voiceId
          Just (X.TranscriptFailed _) -> do
            H.modify_ \s -> s { outboxes = O.delete owner s.outboxes }
            E.notify (item.name <> " couldn't be transcribed. Retry it or remove it, then press Send again.")
          _ -> E.later 1500.0 (dispatch (VoiceFlush owner))
  sendVoice m owner o voiceId = do
    H.modify_ \s -> s { outboxes = O.update (\x -> Just x { sending = true }) owner s.outboxes }
    E.attempt (E.api runtime m o.path "POST" (D.json { text: o.text, attachments: o.others <> [voiceId] })) \result -> do
      s <- H.get
      -- The corpora changed while this request was in flight: its reply belongs to the old one.
      when (s.epoch == o.epoch) case result of
        Left err -> do
          H.modify_ \x -> x { outboxes = O.delete owner x.outboxes }
          E.notify ("Voice message could not be sent: " <> message err <> ". Press Send to continue.")
        Right _ -> do
          H.modify_ \x -> x { outboxes = O.update (\b -> Just b { text = "", others = [], voices = A.drop 1 b.voices, sending = false }) owner x.outboxes
                            , attachments = O.update (Just <<< A.filter (\item -> item.id /= voiceId && not (A.elem item.id o.others))) owner x.attachments
                            -- Clear the draft only if it is still the text that was sent, as for normal messages:
                            -- anything typed meanwhile is kept.
                            , drafts = if o.text /= "" && S.trim (fromMaybe "" $ O.lookup owner x.drafts) == o.text then O.insert owner "" x.drafts else x.drafts }
          E.save dispatch
          dispatch Poll
          flush owner
  -- Recording is complete. After Send-while-recording, send now, but only if the person is still in the
  -- chat they recorded in; otherwise the voice messages stay in that chat's composer for review.
  finish v = do
    H.unsubscribe v.subscription
    H.modify_ _ { voice = Nothing }
    dispatch Poll
    m <- H.get
    case v.phase of
      M.Stopping M.SendAfter | m.selected == v.owner && A.any X.isVoice (M.attachments m) -> dispatch Send
      _ -> pure unit
  abandon v = do
    traverse_ stopRecorder (M.phaseRecorder v.phase)
    H.unsubscribe v.subscription
    H.modify_ _ { voice = Nothing }
