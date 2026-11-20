module Telemetry
 ( Telemetry, RuntimeKind(..), Event(..), CorporaId(..), RequestDetails(..), AttachmentCounts(..)
 , RequestTransport(..), ClientContext, HeartbeatDetails, ConsentError(..)
 , openTelemetry, openTelemetryWith, closeTelemetry, enabled, setConsent, consentView
 , emit, heartbeatInterval, startupConsentOption, localClientContext, nativeClientContext
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
import qualified Data.UUID.V4 as UUID
import Data.Version (showVersion)
import GHC.Clock (getMonotonicTimeNSec)
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

queueLimit, batchDelayMicros, requestTimeoutMicros :: Int
queueLimit = 100
batchDelayMicros = 10 * 1000000
requestTimeoutMicros = 10 * 1000000
startupConsentOption :: String
startupConsentOption = "--telemetry-consent"

data SenderStatus = Running | Disabled Text | Closed deriving Eq
data DeviceLookup = Unchecked | Checked (Maybe DeviceIdentity) deriving Eq
data Pending = Pending { pendingAt :: Integer, pendingRecord :: EventRecord }
data Telemetry = Telemetry
 { savedSettings :: MVar Saved, settingsPath :: Maybe FilePath, senderStatus :: TVar SenderStatus
 , consent :: TVar Bool, consentEpoch :: TVar Integer, pendingEvents :: TVar (Seq.Seq Pending), senderThread :: Maybe (Async ())
 , hostContext :: HostContext, nativeDevice :: TVar DeviceLookup, origin :: Maybe Request, manager :: IO Manager, deliveryLock :: MVar () }

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
openTelemetry = openTelemetryUsing (newManager tlsManagerSettings)

openTelemetryWith :: Manager -> RuntimeKind -> Maybe Bool -> IO Telemetry
openTelemetryWith = openTelemetryUsing . pure

openTelemetryUsing :: IO Manager -> RuntimeKind -> Maybe Bool -> IO Telemetry
openTelemetryUsing create runtime choice = do
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
 device <- newTVarIO Unchecked
 connection <- newMVar Nothing
 delivery <- newMVar ()
 let http = modifyMVar connection $ \cached -> do
      value <- maybe create pure cached
      pure (Just value,value)
 let context = HostContext (T.pack (showVersion version)) nativeOS (T.pack arch) runtime
 let telemetry = Telemetry saved file state active epoch queue Nothing context device request http delivery
 mask $ \restore -> do
  restore (forM_ choice (void . saveConsent False telemetry))
  task <- asyncWithUnmask (\unmask -> unmask (sender telemetry))
  pure telemetry {senderThread=Just task}

closeTelemetry :: Telemetry -> IO ()
closeTelemetry telemetry = mask_ $ do
 atomically $ do
  writeTVar (senderStatus telemetry) Closed
  invalidate telemetry False
 mapM_ cancel (senderThread telemetry)

invalidate :: Telemetry -> Bool -> STM ()
invalidate telemetry on = do
 writeTVar (consent telemetry) on
 modifyTVar' (consentEpoch telemetry) (+1)
 writeTVar (pendingEvents telemetry) Seq.empty

permitted :: Telemetry -> Integer -> Bool -> STM Bool
permitted telemetry epoch expected = do
 status <- readTVar (senderStatus telemetry)
 current <- readTVar (consentEpoch telemetry)
 active <- readTVar (consent telemetry)
 pure (status==Running && current==epoch && active==expected)

enabled :: Telemetry -> IO Bool
enabled = atomically . reporting

reporting :: Telemetry -> STM Bool
reporting telemetry = (&&) <$> readTVar (consent telemetry) <*> ((==Running) <$> readTVar (senderStatus telemetry))

nativeClientContext :: Telemetry -> IO (Maybe ClientContext)
nativeClientContext telemetry = atomically $ do
 active <- reporting telemetry
 cached <- readTVar (nativeDevice telemetry)
 let identity = case cached of Unchecked -> Nothing; Checked value -> value
 pure (if active then Just (ClientContext Native DesktopDevice nativeOS identity) else Nothing)

localClientContext :: Telemetry -> Maybe ClientContext -> IO (Maybe ClientContext)
localClientContext telemetry value = case value of
 Just (ClientContext Native _ _ _) -> nativeClientContext telemetry
 context -> do
  active <- enabled telemetry
  pure (if active then context else Nothing)

batchContext :: Telemetry -> Credentials -> IO BatchContext
batchContext telemetry credentials = do
 context <- nativeClientContext telemetry
 pure (BatchContext (hostContext telemetry) (installationId credentials) (context >>= clientDevice))

disable :: Telemetry -> Text -> IO ()
disable telemetry reason = atomically $ do
 modifyTVar' (senderStatus telemetry) (\status -> if status==Closed then Closed else Disabled reason)
 invalidate telemetry False

setConsent :: Telemetry -> Bool -> IO (Either ConsentError ())
setConsent = saveConsent True

saveConsent :: Bool -> Telemetry -> Bool -> IO (Either ConsentError ())
saveConsent explicit telemetry on = mask $ \restore -> do
 (requested,wasReporting) <- atomically $ do
  active <- reporting telemetry
  unless on (invalidate telemetry False)
  epoch <- readTVar (consentEpoch telemetry)
  pure (epoch,active)
 result <- try $ modifyMVarMasked (savedSettings telemetry) $ \saved@(Saved previous credentials) ->
  (do
   pending <- atomically $ do
    epoch <- readTVar (consentEpoch telemetry)
    active <- readTVar (consent telemetry)
    when (not on && active) (invalidate telemetry False)
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
      pure (Just (epoch,previous/=on && explicit,credentials))
    pure (updated,outcome))
  `onException` disable telemetry "Telemetry settings were not committed"
 case result of
  Left (e::IOException) -> pure (Left (ConsentError (T.pack (displayException e))))
  Right outcome -> do
   restore $ forM_ outcome $ \(epoch,changed,credentials) -> when changed $ (do
    time <- floor <$> getPOSIXTime
    row <- eventRecord time (TelemetryConsentChanged on)
    if on then enqueue telemetry epoch row else when wasReporting $ forM_ credentials $ \auth -> do
     context <- batchContext telemetry auth
     void $ timeout 1000000 $ sendBatch telemetry epoch False auth (Batch context [row]))
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
enqueue telemetry epoch row = do
 tick <- toInteger <$> getMonotonicTimeNSec
 when (recordFits (hostContext telemetry) row) $ atomically $ do
  active <- permitted telemetry epoch True
  queue <- readTVar (pendingEvents telemetry)
  when (active && Seq.length queue<queueLimit) (writeTVar (pendingEvents telemetry) (queue Seq.|> Pending tick row))

eventRecord :: Int64 -> Event -> IO EventRecord
eventRecord time event = EventRecord <$> UUID.nextRandom <*> pure time <*> pure event

nextBatch :: Telemetry -> IO (Integer,[EventRecord])
nextBatch telemetry = do
 first <- atomically $ do
  active <- reporting telemetry
  queue <- readTVar (pendingEvents telemetry)
  check (active && not (Seq.null queue))
  pure (Seq.index queue 0)
 now <- toInteger <$> getMonotonicTimeNSec
 timer <- registerDelay (fromInteger (max 0 (toInteger batchDelayMicros - (now-pendingAt first) `div` 1000)))
 batch <- atomically $ do
  queue <- readTVar (pendingEvents telemetry)
  active <- reporting telemetry
  elapsed <- readTVar timer
  if not active || Seq.null queue || pendingAt (Seq.index queue 0)/=pendingAt first then pure Nothing
  else do
   check (Seq.length queue>=batchLimit || elapsed)
   let (taken,rest) = Seq.splitAt batchLimit queue
   epoch <- readTVar (consentEpoch telemetry)
   writeTVar (pendingEvents telemetry) rest
   pure (Just (epoch,map pendingRecord (toList taken)))
 maybe (nextBatch telemetry) pure batch

data Delivery a = Unavailable | Rejected | Received a
post :: Telemetry -> Integer -> Bool -> String -> Maybe Credentials -> BL.ByteString -> (BodyReader -> IO a) -> IO (Delivery a)
post telemetry epoch expected route credentials body consume = withMVar (deliveryLock telemetry) $ \_ -> case origin telemetry of
 Nothing -> pure Unavailable
 Just request -> do
  let authorized = maybe id (\(Credentials ident password) -> applyBasicAuth (TE.encodeUtf8 ident) (TE.encodeUtf8 password)) credentials
      outgoing = authorized request {HTTP.path=BC.pack route,method="POST",requestBody=RequestBodyLBS body,requestHeaders=[("Content-Type","application/json")],responseTimeout=responseTimeoutMicro requestTimeoutMicros,redirectCount=0,checkResponse = \_ _ -> pure ()}
  let send = timeout requestTimeoutMicros $ (do
       http <- manager telemetry
       active <- atomically $ permitted telemetry epoch expected
       if not active then pure Unavailable else withResponse outgoing http $ \response -> do
        let code = statusCode (responseStatus response)
        if code `elem` [401,403] then pure Rejected
        else if code>=200 && code<300 then Received <$> consume (responseBody response) else pure Unavailable)
       `catch` \(_::HttpException) -> pure Unavailable
      revoked = atomically (permitted telemetry epoch expected >>= check . not)
  either (const Unavailable) (maybe Unavailable id) <$> race revoked send

enroll :: Telemetry -> Integer -> IO (Delivery (Maybe Credentials))
enroll telemetry epoch = post telemetry epoch True enrollPath Nothing "{}" $ \body ->
 decodeCredentials <$> brReadSome body (fromIntegral requestByteLimit)

sendBatch :: Telemetry -> Integer -> Bool -> Credentials -> Batch -> IO (Delivery ())
sendBatch telemetry epoch expected credentials batch =
 post telemetry epoch expected eventsPath (Just credentials) (encodeBatch batch) $ \body ->
  void (brReadSome body (fromIntegral requestByteLimit))

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
      active <- atomically $ permitted telemetry epoch True
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
   atomically $ permitted telemetry lookupEpoch True >>= \active -> when active (writeTVar (nativeDevice telemetry) (Checked identity))
  (epoch,batch) <- nextBatch telemetry
  credentials <- credentialsFor telemetry epoch
  forM_ credentials $ \auth -> do
   context <- batchContext telemetry auth
   result <- sendBatch telemetry epoch True auth (Batch context batch)
   when (case result of Rejected -> True; _ -> False) (disableAt telemetry epoch "Telemetry authentication rejected until restart"))
 `catch` \(e::SomeException) -> case fromException e :: Maybe SomeAsyncException of
  Just _ -> throwIO e
  Nothing -> disable telemetry (T.pack (displayException e))

disableAt :: Telemetry -> Integer -> Text -> IO ()
disableAt telemetry epoch reason = atomically $ do
 active <- permitted telemetry epoch True
 when active $ writeTVar (senderStatus telemetry) (Disabled reason) >> invalidate telemetry False
