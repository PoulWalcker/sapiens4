{-# LANGUAGE OverloadedStrings #-}
-- | Voice messages. Audio streams in as short pieces (about 2 s each) and is cut
-- into voice messages of up to one minute. The decisions are pure functions;
-- 'receivePiece' is the adapter that reads and writes files.
module Voice
 ( Pcm, pcmBytes, pcmFromWav, wavFromPcm
 , Progress(..), Cut(..), PiecePlan(..), startProgress, planPiece
 , receivePiece, cancelRecording, retryTranscription, draftable
 , startTranscriber
 ) where

import Control.Concurrent (threadDelay, MVar, newMVar, modifyMVar_, withMVar, swapMVar)
import Control.Concurrent.Async (asyncWithUnmask, cancel, AsyncCancelled(..))
import Control.Exception (IOException, SomeException, SomeAsyncException, fromException, try, tryJust, throwIO, finally, mask_)
import Control.Monad
import Data.Aeson hiding (pairs)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as BL
import Data.Char (isHexDigit)
import Data.IORef (readIORef)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Network.HTTP.Client as H
import qualified Network.Socket as Socket
import Network.HTTP.Client.MultipartFormData (formDataBody, partBS, partFileSource)
import Network.HTTP.Types.Status (statusCode)
import System.Directory
import System.Environment (lookupEnv)
import System.FilePath
import System.IO (hPutStrLn, stderr)
import Types
import qualified Store as DB

sampleRate, bytesPerSecond, pieceLimit, messageLimit, minimumTail, maxParts :: Int
sampleRate = 16000
bytesPerSecond = sampleRate * 2        -- 16-bit mono
pieceLimit = 5 * bytesPerSecond        -- one streamed piece: at most 5 s
messageLimit = 60 * bytesPerSecond     -- one voice message: at most 1 minute
minimumTail = bytesPerSecond `div` 2   -- a final scrap under 0.5 s is dropped
maxParts = 60                          -- recordings stop at 60 minutes

-- | Raw 16 kHz mono 16-bit audio. Only 'pcmFromWav' can build one, so a 'Pcm'
-- is always in the format Whisper expects.
newtype Pcm = Pcm BS.ByteString

pcmBytes :: Pcm -> BS.ByteString
pcmBytes (Pcm bytes) = bytes

pcmFromWav :: BS.ByteString -> Either Text Pcm
pcmFromWav wav
 | BS.length wav < 44 || tag 0 /= "RIFF" || tag 8 /= "WAVE" || tag 12 /= "fmt " = Left "Voice audio must be a WAV file"
 | word16 20 /= 1 || word16 22 /= 1 || word32 24 /= sampleRate || word16 34 /= 16 = Left "Voice audio must be 16 kHz mono 16-bit PCM"
 | tag 36 /= "data" || word32 40 /= BS.length body = Left "Voice audio is incomplete"
 | BS.null body || odd (BS.length body) || BS.length body > pieceLimit = Left "Voice pieces must hold up to 5 seconds of audio"
 | otherwise = Right (Pcm body)
 where
 body = BS.drop 44 wav
 tag i = BS.take 4 (BS.drop i wav)
 byte i = fromIntegral (BS.index wav i) :: Int
 word16 i = byte i + 256 * byte (i + 1)
 word32 i = word16 i + 65536 * word16 (i + 2)

wavFromPcm :: BS.ByteString -> BS.ByteString
wavFromPcm pcm = BL.toStrict $ BB.toLazyByteString $ mconcat
 [ BB.byteString "RIFF", w32 (36 + size), BB.byteString "WAVEfmt ", w32 16, w16 1, w16 1
 , w32 sampleRate, w32 bytesPerSecond, w16 2, w16 16, BB.byteString "data", w32 size, BB.byteString pcm ]
 where
 size = BS.length pcm
 w32, w16 :: Int -> BB.Builder
 w32 = BB.word32LE . fromIntegral
 w16 = BB.word16LE . fromIntegral

-- | Where a recording stands: the next piece we expect, the next voice message
-- number, how many audio bytes wait for the next cut, and whether it has ended.
data Progress = Progress { nextPiece :: Int, nextPart :: Int, buffered :: Int, finished :: Bool } deriving (Eq, Show)

-- | Strict reading of saved progress: every field must be present with the right type
-- (aeson refuses a fractional or out-of-range Int). Nothing is defaulted.
instance FromJSON Progress where
 parseJSON = withObject "voice progress" $ \o ->
  Progress <$> o .: "next_piece" <*> o .: "next_part" <*> o .: "buffered" <*> o .: "finished"

startProgress :: Progress
startProgress = Progress 0 0 0 False

-- | A finished voice message: its number and how many buffered bytes it takes.
data Cut = Cut { cutPart :: Int, cutBytes :: Int } deriving (Eq, Show)

data PiecePlan = DuplicatePiece | AppendPiece Progress [Cut] deriving (Eq, Show)

-- | Decide what one incoming piece does. Pure: no files, no clock.
planPiece :: Progress -> Int -> Bool -> Int -> Either Text PiecePlan
planPiece progress index final size
 | index < nextPiece progress = Right DuplicatePiece
 | finished progress = Left "This recording is already finished"
 | index > nextPiece progress = Left "Voice piece arrived out of order; resend from the missing piece"
 | nextPart progress + length cuts > maxParts = Left "Recordings are limited to 60 minutes"
 | otherwise = Right (AppendPiece (Progress (index + 1) (nextPart progress + length cuts) remaining final) cuts)
 where
 total = buffered progress + size
 whole = total `div` messageLimit
 rest = total `mod` messageLimit
 cuts = [Cut (nextPart progress + k) messageLimit | k <- [0 .. whole - 1]]
     ++ [Cut (nextPart progress + whole) rest | final, rest >= minimumTail]
 remaining = if final then 0 else rest

-- | The adapter: validate the request, apply the plan, save any finished voice messages.
--
-- Safe to retry at every step. A piece is applied in this order:
--   1. save each finished voice message once (keyed by recording and part number)
--   2. write the leftover audio to a NEW buffer file for the next piece
--   3. save progress.json  ← the commit point
--   4. delete the old buffer file
-- A crash before step 3 leaves the previous progress and its buffer untouched, so the
-- resent piece replays exactly; step 1 then finds the parts already saved and reuses them.
receivePiece :: Service -> Text -> Value -> IO Value
receivePiece s a d = do
 let recording = at d "recording"
     final = get d "final" == Bool True
 validRecording recording
 index <- case get d "index" of
  Number n | n >= 0 && n <= 100000 && n == fromIntegral (round n :: Int) -> pure (round n)
  _ -> failure 400 "Voice piece index must be a whole number"
 require (case get d "data" of String t -> T.length t <= 300000; _ -> False) 413 "Voice pieces must be 5 seconds or shorter"
 wav <- either (const (failure 400 "Invalid voice data")) pure (B64.decode (TE.encodeUtf8 (at d "data")))
 pcm <- either (failure 400) pure (pcmFromWav wav)
 let dir = recordingDir s a recording
     progressFile = dir </> "progress.json"
 createDirectoryIfMissing True dir
 progress <- readProgress progressFile
 case planPiece progress index final (BS.length (pcmBytes pcm)) of
  Left problem -> failure 409 problem
  -- The phone resent a piece because the first reply was lost: answer with the voice
  -- messages that piece created the first time, so the phone still learns their ids.
  Right DuplicatePiece -> reply recording True . filter (createdBy index) <$> recordingParts s a recording
  Right (AppendPiece next cuts) -> do
   waiting <- readIfPresent (bufferFile dir (nextPiece progress))
   when (BS.length waiting < buffered progress) $ failure 500 "Voice recording state is damaged"
   let audio = BS.take (buffered progress) waiting <> pcmBytes pcm
   saved <- recordingParts s a recording
   (rows, leftover) <- foldM (saveCut s a recording index saved) ([], audio) cuts
   atomicBytes (bufferFile dir (nextPiece next)) (BL.fromStrict leftover)
   atomicBytes progressFile (encode (progressJSON next))
   removeIfPresent (bufferFile dir (nextPiece progress))
   when (finished next) $ removeIfPresent (bufferFile dir (nextPiece next))
   pure (reply recording False (filter draftable (reverse rows)))
 where
 reply recording duplicate rows = obj [("recording",str recording),("duplicate",Bool duplicate),("messages",arr rows)]
 createdBy i v = draftable v && get v "piece" == num i

-- | Save one finished voice message, exactly once. If this recording already has this part
-- (a retry after a crash), the saved row is reused instead of creating a duplicate.
saveCut :: Service -> Text -> Text -> Int -> [Value] -> ([Value], BS.ByteString) -> Cut -> IO ([Value], BS.ByteString)
saveCut s a recording piece saved (rows, audio) cut = do
 let (part, rest) = BS.splitAt (cutBytes cut) audio
 case filter (\v -> get v "part" == num (cutPart cut)) saved of
  existing : _ -> pure (existing : rows, rest)
  [] -> do
   let name = "voice-" <> T.pack (show (cutPart cut + 1)) <> ".wav"
   i <- newId
   let path = serviceRoot s </> "uploads" </> T.unpack a </> T.unpack i </> T.unpack name
   atomicBytes path (BL.fromStrict (wavFromPcm part))
   let row = obj [("id",str i),("agent",str a),("kind",str "voice"),("name",str name),("value",str (T.pack path))
                 ,("size",num (BS.length part + 44)),("recording",str recording),("part",num (cutPart cut)),("piece",num piece)
                 ,("duration_ms",num (BS.length part * 1000 `div` bytesPerSecond)),("transcript_status",str "pending")]
   DB.saveAttachment s row
   pure (row : rows, rest)

-- | Every saved voice message of one recording.
recordingParts :: Service -> Text -> Text -> IO [Value]
recordingParts s a recording = filter (\v -> at v "kind" == "voice" && at v "recording" == recording) <$> DB.attachments s a

-- | Still a draft: neither sent nor cancelled. Only drafts may wait in a composer, be returned
-- to a phone, or be sent; sent and cancelled voice messages must never come back.
draftable :: Value -> Bool
draftable v = get v "sent" /= Bool True && get v "cancelled" /= Bool True

validRecording :: Text -> IO ()
validRecording recording = require (T.length recording == 36 && T.all (\c -> isHexDigit c || c == '-') recording) 400 "Invalid voice recording id"

-- | Where a recording's progress and buffered audio live until it is cut.
recordingDir :: Service -> Text -> Text -> FilePath
recordingDir s a recording = serviceRoot s </> "uploads" </> T.unpack a </> "voice" </> T.unpack recording

-- | Cancel a recording (the person pressed Cancel). Every voice message of it that is still a
-- draft is marked "cancelled" (so it can never be transcribed, sent or brought back by a stale
-- device) and its audio is deleted; the recording is closed so a late piece is refused.
-- Voice messages already sent are history and stay. Safe to repeat.
cancelRecording :: Service -> Text -> Text -> IO Value
cancelRecording s a recording = do
 validRecording recording
 let dir = recordingDir s a recording
 createDirectoryIfMissing True dir
 atomicBytes (dir </> "progress.json") (encode (progressJSON startProgress { finished = True }))
 buffers <- filter (\name -> takeExtension name == ".pcm") <$> listDirectory dir
 forM_ buffers (removeIfPresent . (dir </>))
 drafts <- filter draftable <$> recordingParts s a recording
 forM_ drafts $ \row -> DB.updateAttachment s (put "cancelled" (Bool True) row)
 DB.dropDrafts s a drafts
 forM_ drafts $ \row -> do
  let audio = T.unpack (at row "value")
  removeIfPresent audio
  void (try (removeDirectory (takeDirectory audio)) :: IO (Either IOException ()))
 pure (obj [("recording",str recording),("cancelled",arr (map (`get` "id") drafts))])

-- | Try a failed transcription again: the voice message goes back to "pending" and the
-- background worker picks it up. Only a failed voice message that is still a draft qualifies.
retryTranscription :: Service -> Text -> Text -> IO Value
retryTranscription s a i = do
 row <- maybe (failure 404 "Unknown voice message") pure . findId i =<< DB.attachments s a
 require (at row "kind" == "voice" && draftable row) 404 "Unknown voice message"
 require (at row "transcript_status" == "failed") 409 "Only a failed transcription can be retried"
 let retried = remove ["transcript_error"] (put "transcript_status" (str "pending") row)
 DB.updateAttachment s retried
 pure retried

-- | Audio waiting for the next cut, one file per piece number, so a new buffer never
-- overwrites the one the saved progress still points to.
bufferFile :: FilePath -> Int -> FilePath
bufferFile dir piece = dir </> ("buffer-" <> show piece <> ".pcm")

progressJSON :: Progress -> Value
progressJSON p = obj [("next_piece",num (nextPiece p)),("next_part",num (nextPart p)),("buffered",num (buffered p)),("finished",Bool (finished p))]

-- | Read saved progress strictly. Damaged or impossible state is an explicit error (logged),
-- never silently turned into zeros, because it decides piece order and voice message numbers.
readProgress :: FilePath -> IO Progress
readProgress path = do
 exists <- doesFileExist path
 if not exists then pure startProgress else do
  decoded <- eitherDecodeStrict <$> BS.readFile path
  case decoded of
   Right p | sane p -> pure p
   Right p -> damaged ("impossible values " <> show p)
   Left problem -> damaged problem
 where
 damaged problem = do
  hPutStrLn stderr ("Voice recording state is damaged (" <> path <> "): " <> take 300 problem)
  failure 500 "Voice recording state is damaged"
 sane p = nextPiece p >= 0 && nextPart p >= 0 && nextPart p <= maxParts
       && buffered p >= 0 && buffered p < messageLimit && even (buffered p)
       && (not (finished p) || buffered p == 0)

readIfPresent :: FilePath -> IO BS.ByteString
readIfPresent path = doesFileExist path >>= \exists -> if exists then BS.readFile path else pure BS.empty

removeIfPresent :: FilePath -> IO ()
removeIfPresent path = doesFileExist path >>= \exists -> when exists (removeFile path)

-- | Background transcription. Voice messages saved as "pending" are sent, one at a time, to
-- whisper-server on this computer (the audio never leaves it); the text is saved on the
-- attachment as "done", or "failed" with the reason. If Whisper is not running yet, messages
-- stay pending and are retried. The address comes from SAPIENS_WHISPER_URL
-- (default http://127.0.0.1:8178). The loop ends when the service stops.
-- The gate owns sockets for the current request; Nothing means shutdown has begun.
-- Windows socket reads must be closed before waiting for async cancellation.
-- Disable idle pooling so completed requests cannot leave untracked live sockets.
startTranscriber :: Service -> IO (IO ())
startTranscriber s = mask_ $ do
 gate <- newMVar (Just [])
 let register socket = modifyMVar_ gate $ \active -> case active of
      Nothing -> throwIO AsyncCancelled
      Just sockets -> pure (Just (socket:sockets))
 manager <- H.newManager H.defaultManagerSettings
  { H.managerResponseTimeout = H.responseTimeoutMicro 300000000
  , H.managerIdleConnectionCount = 0
  , H.managerRawConnection = H.rawConnectionModifySocket register
  }
 worker <- asyncWithUnmask (\unmask -> unmask (transcribeLoop s gate manager))
 pure $ mask_ $ do
  sockets <- swapMVar gate Nothing
  forM_ sockets $ mapM_ $ \socket -> do
   void (try (Socket.shutdown socket Socket.ShutdownBoth) :: IO (Either IOException ()))
   Socket.close socket
  cancel worker

tryTranscription :: IO a -> IO (Either SomeException a)
tryTranscription = tryJust $ \problem -> case fromException problem :: Maybe SomeAsyncException of
 Just _ -> Nothing
 Nothing -> Just problem

transcribeLoop :: Service -> MVar (Maybe [Socket.Socket]) -> H.Manager -> IO ()
transcribeLoop s gate manager = do
 base <- maybe "http://127.0.0.1:8178" id <$> lookupEnv "SAPIENS_WHISPER_URL"
 hPutStrLn stderr ("Voice transcription: using " <> base)
 let go = do
      stopping <- readIORef (serviceStopping s)
      unless stopping $ do
       outcome <- tryTranscription (transcribeNext s gate manager base)
        `finally` modifyMVar_ gate (pure . fmap (const []))
       case outcome of
        -- Never fail silently: report the problem in the host log, then retry calmly.
        Left problem -> hPutStrLn stderr ("Voice transcription failed: " <> take 500 (show problem)) >> threadDelay 10000000
        Right worked -> threadDelay (if worked then 100000 else 2000000)
       go
 go

-- | Transcribe the next pending voice message. True when one was handled.
transcribeNext :: Service -> MVar (Maybe [Socket.Socket]) -> H.Manager -> String -> IO Bool
transcribeNext s gate manager base = do
 pending <- locked s (DB.pendingVoices s)
 case pending of
  [] -> pure False
  row : _ -> do
   result <- tryTranscription (whisper manager base (T.unpack (at row "value")))
   case result of
    Left problem | unreachable problem -> pure False
    _ -> do
     let outcome = case result of
          Right text -> obj [("transcript",str text),("transcript_status",str "done")]
          Left problem -> obj [("transcript_status",str "failed"),("transcript_error",str (T.pack (take 300 (show problem))))]
     -- Whisper can take seconds; meanwhile the voice message may have been sent (and marked
     -- "sent"). Save the transcript onto the attachment as it is now, not as it was read.
     -- A socket interrupted by shutdown is still pending, not a failed transcription.
     withMVar gate $ \active -> forM_ active $ \_ -> locked s $ do
       current <- filter (\fresh -> get fresh "id" == get row "id" && draftable fresh) <$> DB.attachments s (at row "agent")
       forM_ current $ \fresh -> DB.updateAttachment s (merge fresh outcome)
     pure True
 where
 unreachable problem = case fromException problem of
  Just (H.HttpExceptionRequest _ (H.ConnectionFailure _)) -> True
  _ -> False

-- | Product words Whisper should spell correctly, passed as its prompt (a vocabulary hint).
vocabulary :: BS.ByteString
vocabulary = "Sapiens4, Sapi, Sapis, ChiefSapi, CORPORA, corpora, Remote Control, relay, Utana, sapiens4.ai, Whisper, Haskell, PureScript."

-- | One request to whisper-server's /inference endpoint with the WAV file.
whisper :: H.Manager -> String -> FilePath -> IO Text
whisper manager base path = do
 request <- H.parseRequest (base <> "/inference")
 upload <- formDataBody [partBS "response_format" "json", partBS "temperature" "0.0", partBS "prompt" vocabulary, partFileSource "file" path] request
 response <- H.httpLbs upload manager
 let status = statusCode (H.responseStatus response)
 when (status /= 200) $ failure 502 ("Whisper answered HTTP " <> T.pack (show status))
 case decode (H.responseBody response) of
  -- Whisper returns one line per segment with leading spaces; keep plain sentences.
  Just value | String raw <- get value "text" -> pure (T.unwords (T.words raw))
  _ -> failure 502 "Whisper returned no text"
