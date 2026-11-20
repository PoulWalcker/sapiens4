module Telemetry
 ( Telemetry, RuntimeKind(..), Event(..), CorporaId(..), RequestDetails(..), AttachmentCounts(..)
 , RequestTransport(..), ClientContext, HeartbeatDetails
 , openTelemetry, closeTelemetry, enabled, setConsent, consentView
 , emit, requestFlush, heartbeatInterval, startupConsentOption, localClientContext, nativeClientContext
 ) where

import Control.Concurrent.Async
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Exception hiding (handle)
import Control.Monad
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.Foldable (toList)
import Data.Int (Int64)
import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.UUID (UUID)
import qualified Data.UUID.V4 as UUID
import Data.Version (showVersion)
import Network.HTTP.Client hiding (path)
import qualified Network.HTTP.Client as HTTP (path)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Status (statusCode)
import Paths_sapiens_domain (version)
import RuntimePaths (configDirectory)
import System.Directory
import System.Environment (lookupEnv)
import System.FilePath
import System.Info (arch)
import System.IO
import System.IO.Error (isDoesNotExistError)
import System.Timeout (timeout)
import TelemetryModel
import TelemetryProtocol
import TelemetryWire
import TelemetryDevice

queueLimit, requestTimeoutMicros :: Int
queueLimit = 100
requestTimeoutMicros = 10 * 1000000
startupConsentOption :: String
startupConsentOption = "--telemetry-consent"

data SenderStatus = Running | Disabled Text | Closed deriving Eq
data DeviceLookup = Unchecked | Checked (Maybe DeviceIdentity) deriving Eq
data Telemetry = Telemetry
 { savedSettings :: MVar Saved, settingsPath :: Maybe FilePath, senderStatus :: TVar SenderStatus
 , consent :: TVar Bool, consentEpoch :: TVar Integer, pendingEvents :: TVar (Seq.Seq EventRecord)
 , flushRequested :: TVar Bool, senderThread :: Maybe (Async ())
 , hostContext :: HostContext, nativeDevice :: TVar DeviceLookup, origin :: Maybe Request, manager :: Manager }

readSaved :: FilePath -> IO Saved
readSaved path = catchJust (guard . isDoesNotExistError)
 (BL.readFile path >>= either fail pure . decodeSaved)
 (const (pure (Saved False Nothing)))

writeSaved :: FilePath -> Saved -> IO ()
writeSaved path value = do
 createDirectoryIfMissing True (takeDirectory path)
 bracketOnError (openBinaryTempFile (takeDirectory path) ".telemetry.tmp")
  (\(temporary,handle) -> hClose handle `finally` removeFile temporary) $ \(temporary,handle) -> do
   BL.hPut handle (encodeSaved value)
   hClose handle
   renameFile temporary path

openTelemetry :: RuntimeKind -> Maybe Bool -> IO Telemetry
openTelemetry runtime choice = do
 loaded <- try $ do
  path <- (</> "telemetry.json") <$> configDirectory
  value <- readSaved path
  endpoint <- maybe "https://telemetry.sapiens4.ai" id <$> lookupEnv "SAPIENS_TELEMETRY_ORIGIN"
  pure (path,value,endpoint)
 let (file,initial,loadedStatus,endpoint) = case loaded of
      Left (e::IOException) -> (Nothing,Saved False Nothing,Disabled (T.pack (displayException e)),Nothing)
      Right (settings,value,url) -> (Just settings,value,Running,Just url)
     Saved on _ = initial
 parsed <- try $ forM endpoint $ \url -> do
  request <- parseRequest url `catch` \(e::HttpException) -> ioError (userError (displayException e))
  unless (secure request && B.null (queryString request) && HTTP.path request=="/") (fail "Telemetry requires an HTTPS origin")
  pure request
 let (request,status) = case parsed of
      Left (e::IOException) -> (Nothing,Disabled (T.pack (displayException e)))
      Right value -> (value,loadedStatus)
 saved <- newMVar initial
 active <- newTVarIO (maybe on (const False) choice)
 epoch <- newTVarIO 0
 state <- newTVarIO status
 queue <- newTVarIO Seq.empty
 flush <- newTVarIO False
 device <- newTVarIO Unchecked
 http <- newManager tlsManagerSettings
 let context = HostContext (T.pack (showVersion version)) nativeOS (T.pack arch) runtime
 let telemetry = Telemetry saved file state active epoch queue flush Nothing context device request http
 mask $ \restore -> do
  restore (forM_ choice (void . saveConsent False telemetry))
  task <- asyncWithUnmask (\unmask -> unmask (sender telemetry))
  pure telemetry {senderThread=Just task}

closeTelemetry :: Telemetry -> IO ()
closeTelemetry telemetry = mask_ $ do
 atomically $ do
  writeTVar (senderStatus telemetry) Closed
  stopReporting telemetry
 mapM_ cancel (senderThread telemetry)

stopReporting :: Telemetry -> STM ()
stopReporting telemetry = do
 writeTVar (consent telemetry) False
 modifyTVar' (consentEpoch telemetry) (+1)
 writeTVar (pendingEvents telemetry) Seq.empty
 writeTVar (flushRequested telemetry) False

permitted :: Telemetry -> Integer -> STM Bool
permitted telemetry epoch = do
 status <- readTVar (senderStatus telemetry)
 current <- readTVar (consentEpoch telemetry)
 active <- readTVar (consent telemetry)
 pure (status==Running && current==epoch && active)

enabled :: Telemetry -> IO Bool
enabled = atomically . reporting

reporting :: Telemetry -> STM Bool
reporting telemetry = (&&) <$> readTVar (consent telemetry) <*> ((==Running) <$> readTVar (senderStatus telemetry))

nativeClientContext :: Telemetry -> IO (Maybe ClientContext)
nativeClientContext telemetry = atomically $ do
 active <- reporting telemetry
 cached <- readTVar (nativeDevice telemetry)
 let identity = cachedIdentity cached
 pure (if active then Just (ClientContext Native DesktopDevice nativeOS identity) else Nothing)

localClientContext :: Telemetry -> Maybe ClientContext -> IO (Maybe ClientContext)
localClientContext telemetry value = case value of
 Just (ClientContext Native _ _ _) -> nativeClientContext telemetry
 context -> do
  active <- enabled telemetry
  pure (if active then context else Nothing)

cachedIdentity :: DeviceLookup -> Maybe DeviceIdentity
cachedIdentity Unchecked = Nothing
cachedIdentity (Checked identity) = identity

batchContext :: Telemetry -> Credentials -> IO BatchContext
batchContext telemetry credentials = do
 identity <- cachedIdentity <$> readTVarIO (nativeDevice telemetry)
 pure (BatchContext (hostContext telemetry) (installationId credentials) identity)

disable :: Telemetry -> Text -> IO ()
disable telemetry reason = atomically $ do
 modifyTVar' (senderStatus telemetry) (\status -> if status==Closed then Closed else Disabled reason)
 stopReporting telemetry

setConsent :: Telemetry -> Bool -> IO (Either Text ())
setConsent telemetry True = saveConsent True telemetry True
setConsent telemetry False = mask $ \restore -> do
 sent <- try (restore (notifyConsentOff telemetry))
 saved <- saveConsent False telemetry False
 case sent of
  Left (errorValue::SomeException) -> throwIO errorValue
  Right () -> pure saved

notifyConsentOff :: Telemetry -> IO ()
notifyConsentOff telemetry = void $ timeout 1000000 $ do
 (active,epoch) <- atomically $ (,) <$> reporting telemetry <*> readTVar (consentEpoch telemetry)
 when active $ do
  credentials <- credentialsFor telemetry epoch
  forM_ credentials $ \auth -> do
   time <- floor <$> getPOSIXTime
   row <- eventRecord time (TelemetryConsentChanged False)
   context <- batchContext telemetry auth
   sendBatch telemetry epoch auth (Batch context [row])

saveConsent :: Bool -> Telemetry -> Bool -> IO (Either Text ())
saveConsent explicit telemetry on = mask $ \restore -> do
 requested <- atomically $ do
  unless on (stopReporting telemetry)
  readTVar (consentEpoch telemetry)
 result <- try $ modifyMVarMasked (savedSettings telemetry) $ \saved@(Saved previous credentials) ->
  (do
   pending <- atomically $ do
    epoch <- readTVar (consentEpoch telemetry)
    active <- readTVar (consent telemetry)
    when (not on && active) (stopReporting telemetry)
    pure (not on || epoch == requested)
   if not pending then pure (saved,Nothing) else do
    path <- maybe (fail "Telemetry configuration unavailable until restart") pure (settingsPath telemetry)
    let updated = Saved on credentials
    when (not on || previous /= on) (writeSaved path updated)
    outcome <- atomically $ do
     status <- readTVar (senderStatus telemetry)
     epoch <- readTVar (consentEpoch telemetry)
     if on && epoch /= requested then pure Nothing else do
      writeTVar (consent telemetry) (on && status/=Closed)
      pure (Just (epoch,previous/=on && explicit))
    pure (updated,outcome))
  `onException` disable telemetry "Telemetry settings were not committed"
 case result of
  Left (e::IOException) -> pure (Left (T.pack (displayException e)))
  Right outcome -> do
   restore $ forM_ outcome $ \(epoch,changed) -> when (changed && on) $ (do
    time <- floor <$> getPOSIXTime
    row <- eventRecord time (TelemetryConsentChanged True)
    enqueue telemetry epoch row)
    `catch` \(_::IOException) -> pure ()
   pure (Right ())

consentView :: Telemetry -> IO ConsentView
consentView telemetry = atomically $ do
 on <- readTVar (consent telemetry)
 status <- readTVar (senderStatus telemetry)
 pure (ConsentView on (status==Running) (case status of Disabled reason -> reason; _ -> ""))

emit :: Telemetry -> Event -> IO ()
emit telemetry event = do
 (on,epoch) <- atomically $ (,) <$> reporting telemetry <*> readTVar (consentEpoch telemetry)
 when on $ do
  let supplied = case event of UIHeartbeat details -> heartbeatTimestamp details; _ -> Nothing
  time <- maybe (floor <$> getPOSIXTime) pure supplied
  eventRecord time event >>= enqueue telemetry epoch

enqueue :: Telemetry -> Integer -> EventRecord -> IO ()
enqueue telemetry epoch row
 | not (recordFits row) = logOversized (eventId row)
 | otherwise = atomically $ do
  active <- permitted telemetry epoch
  queue <- readTVar (pendingEvents telemetry)
  when (active && Seq.length queue<queueLimit) (writeTVar (pendingEvents telemetry) (queue Seq.|> row))

logOversized :: UUID -> IO ()
logOversized identity = hPutStrLn stderr
 ("Telemetry event "<>show identity<>" exceeds the "<>show requestByteLimit<>" byte request limit; dropped")
 `catch` \(_::IOException) -> pure ()

eventRecord :: Int64 -> Event -> IO EventRecord
eventRecord time event = EventRecord <$> UUID.nextRandom <*> pure time <*> pure event

requestFlush :: Telemetry -> IO ()
requestFlush telemetry = atomically $ do
 active <- reporting telemetry
 queue <- readTVar (pendingEvents telemetry)
 when (active && not (Seq.null queue)) (writeTVar (flushRequested telemetry) True)

nextBatch :: Telemetry -> IO (Integer,[EventRecord])
nextBatch telemetry = atomically $ do
 active <- reporting telemetry
 queue <- readTVar (pendingEvents telemetry)
 flush <- readTVar (flushRequested telemetry)
 check (active && not (Seq.null queue) && (Seq.length queue>=batchLimit || flush))
 let (taken,rest) = Seq.splitAt batchLimit queue
 epoch <- readTVar (consentEpoch telemetry)
 writeTVar (pendingEvents telemetry) rest
 writeTVar (flushRequested telemetry) (flush && not (Seq.null rest))
 pure (epoch,toList taken)

data Delivery a = Unavailable | Rejected | Received a
post :: Telemetry -> Integer -> String -> Maybe Credentials -> BL.ByteString -> (BodyReader -> IO a) -> IO (Delivery a)
post telemetry epoch route credentials body consume = case origin telemetry of
 Nothing -> pure Unavailable
 Just request -> do
  let authorized = maybe id (\(Credentials ident password) -> applyBasicAuth (TE.encodeUtf8 ident) (TE.encodeUtf8 password)) credentials
      outgoing = authorized request {HTTP.path=BC.pack route,method="POST",requestBody=RequestBodyLBS body,requestHeaders=[("Content-Type","application/json")],responseTimeout=responseTimeoutMicro requestTimeoutMicros,redirectCount=0,checkResponse = \_ _ -> pure ()}
  let send = timeout requestTimeoutMicros $ (do
       active <- atomically $ permitted telemetry epoch
       if not active then pure Unavailable else withResponse outgoing (manager telemetry) $ \response -> do
        let code = statusCode (responseStatus response)
        if code `elem` [401,403] then pure Rejected
        else if code>=200 && code<300 then Received <$> consume (responseBody response) else pure Unavailable)
       `catch` \(_::HttpException) -> pure Unavailable
      revoked = atomically (permitted telemetry epoch >>= check . not)
  either (const Unavailable) (maybe Unavailable id) <$> race revoked send

enroll :: Telemetry -> Integer -> IO (Delivery (Maybe Credentials))
enroll telemetry epoch = post telemetry epoch enrollPath Nothing "{}" $ \body ->
 decodeCredentials <$> brReadSome body (fromIntegral requestByteLimit)

sendBatch :: Telemetry -> Integer -> Credentials -> Batch -> IO ()
sendBatch telemetry epoch credentials batch = send (encodeBatches batch)
 where
 send [] = pure ()
 send (Left identity:rest) = logOversized identity >> send rest
 send (Right body:rest) = do
  result <- post telemetry epoch eventsPath (Just credentials) body $ \response ->
   void (brReadSome response (fromIntegral requestByteLimit))
  case result of
   Rejected -> disableAt telemetry epoch "Telemetry authentication rejected until restart"
   _ -> send rest

credentialsFor :: Telemetry -> Integer -> IO (Maybe Credentials)
credentialsFor telemetry epoch = do
 Saved _ current <- readMVar (savedSettings telemetry)
 case current of
  Just credentials -> pure (Just credentials)
  Nothing -> do
   enrollment <- enroll telemetry epoch
   case enrollment of
    Rejected -> disableAt telemetry epoch "Telemetry enrollment rejected until restart" >> pure Nothing
    Unavailable -> pure Nothing
    Received Nothing -> pure Nothing
    Received (Just credentials) -> do
     result <- try $ modifyMVarMasked (savedSettings telemetry) $ \saved@(Saved on existing) -> do
      active <- atomically $ permitted telemetry epoch
      if not active then pure (saved,Nothing) else do
       path <- maybe (fail "Telemetry configuration unavailable") pure (settingsPath telemetry)
       let updated = Saved on (existing `mplus` Just credentials)
       writeSaved path updated
       pure (updated,existing `mplus` Just credentials)
     case result of
      Left (e::IOException) -> disableAt telemetry epoch (T.pack (displayException e)) >> pure Nothing
      Right installed -> pure installed

sender :: Telemetry -> IO ()
sender telemetry = (forever $ do
  atomically $ reporting telemetry >>= check
  lookupEpoch <- readTVarIO (consentEpoch telemetry)
  cached <- readTVarIO (nativeDevice telemetry)
  when (cached==Unchecked) $ do
   identity <- readNativeDevice
   atomically $ permitted telemetry lookupEpoch >>= \active -> when active (writeTVar (nativeDevice telemetry) (Checked identity))
  (epoch,batch) <- nextBatch telemetry
  credentials <- credentialsFor telemetry epoch
  forM_ credentials $ \auth -> do
   context <- batchContext telemetry auth
   sendBatch telemetry epoch auth (Batch context batch))
 `catch` \(e::SomeException) -> case fromException e :: Maybe SomeAsyncException of
  Just _ -> throwIO e
  Nothing -> disable telemetry (T.pack (displayException e))

disableAt :: Telemetry -> Integer -> Text -> IO ()
disableAt telemetry epoch reason = atomically $ do
 active <- permitted telemetry epoch
 when active $ writeTVar (senderStatus telemetry) (Disabled reason) >> stopReporting telemetry
