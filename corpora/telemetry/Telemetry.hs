module Telemetry
 ( Telemetry, RuntimeKind(..), Event(..), openTelemetry, openTelemetryWith, closeTelemetry
 , enabled, setConsent, consentView, emit, emitAt, heartbeatInterval, uiHeartbeat
 , startupConsentOption
 , RequestTransport(..), requestMetadata, ClientContext, parseClientContext, clientContextFields
 , localClientContext, nativeClientContext
 ) where

import Control.Concurrent.Async
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import qualified V00_02_03Files as TypedFiles
import Data.Aeson
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.Foldable (toList)
import Data.Int (Int64)
import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (NominalDiffTime)
import Data.Time.Clock.POSIX (getPOSIXTime)
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import Data.Version (showVersion)
import GHC.Clock (getMonotonicTimeNSec)
import Network.HTTP.Client
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
import TelemetryWire
import TelemetryDevice

data RuntimeKind = Desktop | Headless deriving Eq
data RequestTransport = Direct | Remote | UnknownTransport
data Event = AppStarted | AppUpdated | CorporaCreated | RequestAccepted | RemotePaired | PulseId | UIHeartbeat | TelemetryConsentChanged Bool
 deriving Eq
eventName :: Event -> Text
eventName event = case event of
 AppStarted -> "app_started"
 AppUpdated -> "app_update"
 CorporaCreated -> "corpora_created"
 RequestAccepted -> "request_accepted"
 RemotePaired -> "remote_paired"
 PulseId -> "pulse_id"
 UIHeartbeat -> "ui_heartbeat"
 TelemetryConsentChanged _ -> "telemetry_consent_changed"

metadataValue :: Key -> Value -> Value
metadataValue key (Object fields) = maybe Null id (KM.lookup key fields)
metadataValue _ _ = Null

category :: [Text] -> Value -> Text
category allowed (String value) | value `elem` allowed = value
category _ _ = "unknown"

data ClientContext = ClientContext Text Text Text (Maybe DeviceIdentity)
instance ToJSON ClientContext where
 toJSON = Object . clientContextFields . Just

parseClientContext :: Value -> Maybe ClientContext
parseClientContext value@(Object _) = Just (ClientContext kind family platform identity)
 where
 kind = category ["native","browser","installed_webapp"] (metadataValue "client_kind" value)
 family = category ["desktop","phone","tablet"] (metadataValue "client_device_family" value)
 platform = category clientOSCategories (metadataValue "client_os" value)
 identity = if kind=="native" then parseDeviceIdentity (metadataValue "source_device_id" value) (metadataValue "source_device_id_source" value) else Nothing
parseClientContext _ = Nothing

clientContextFields :: Maybe ClientContext -> Object
clientContextFields context = case context of
 Nothing -> clientContextFields (Just (ClientContext "unknown" "unknown" "unknown" Nothing))
 Just (ClientContext kind family platform identity) -> KM.union (deviceFields "source_" identity) (KM.fromList
  [("client_kind",String kind),("client_device_family",String family),("client_os",String platform)])

clientOSCategories :: [Text]
clientOSCategories = ["windows","macos","linux","android","ios","ipados"]

requestMetadata :: RequestTransport -> Maybe ClientContext -> Object
requestMetadata transport value = KM.insert "request_transport" (String name) (clientContextFields value)
 where name = case transport of Direct -> "direct"; Remote -> "remote"; UnknownTransport -> "unknown"

queueLimit, batchDelayMicros, requestTimeoutMicros, contextReserveBytes :: Int
queueLimit = 100
batchDelayMicros = 10 * 1000000
requestTimeoutMicros = 10 * 1000000
contextReserveBytes = 384
heartbeatInterval :: NominalDiffTime
heartbeatInterval = 60 * 60

startupConsentOption :: String
startupConsentOption = "--telemetry-consent"

batchEnvelope :: Value -> [Value] -> Value
batchEnvelope context batch = object ["context" .= context,"events" .= batch]

data Saved = Saved Bool (Maybe Credentials)
instance ToJSON Saved where
 toJSON (Saved on credentials) = case toJSON credentials of
  Object fields -> Object (KM.insert "enabled" (Bool on) fields)
  _ -> object ["enabled" .= on]
instance FromJSON Saved where
 parseJSON value = withObject "telemetry settings" (\o -> do
  on <- o .: "enabled"
  credentials <- if KM.member "installation_id" o || KM.member "password" o then Just <$> parseJSON value else pure Nothing
  pure (Saved on credentials)) value

data SenderStatus = Running | Disabled Text | Closed deriving Eq
data Pending = Pending { pendingAt :: Integer, pendingValue :: Value }
data Telemetry = Telemetry
 { savedSettings :: MVar Saved, settingsPath :: Maybe FilePath, senderStatus :: TVar SenderStatus
 , consent :: TVar Bool, consentEpoch :: TVar Integer, pendingEvents :: TVar (Seq.Seq Pending), senderThread :: Maybe (Async ())
 , hostContext :: Value, nativeDevice :: TVar (Maybe (Maybe DeviceIdentity)), origin :: Maybe Request, manager :: IO Manager, deliveryLock :: MVar () }

readSaved :: FilePath -> IO Saved
readSaved path = catchJust (guard . isDoesNotExistError)
 (TypedFiles.readValue path (Saved False Nothing))
 (const (pure (Saved False Nothing)))

writeSaved :: FilePath -> Saved -> IO ()
writeSaved = TypedFiles.writeValue

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
  unless (secure request && B.null (queryString request) && path request=="/") (fail "Telemetry requires an HTTPS origin")
  pure request
 let (request,status) = case parsed of
      Left (e::IOException) -> (Nothing,Disabled (T.pack (displayException e)))
      Right value -> (value,loadedStatus)
 saved <- newMVar initial
 active <- newTVarIO (maybe on (const False) choice)
 epoch <- newTVarIO 0
 state <- newTVarIO status
 queue <- newTVarIO Seq.empty
 device <- newTVarIO Nothing
 connection <- newMVar Nothing
 delivery <- newMVar ()
 let http = modifyMVar connection $ \cached -> do
      value <- maybe create pure cached
      pure (Just value,value)
 let context = object ["app_version" .= showVersion version,"host_os" .= nativeOS,"host_arch" .= arch,"runtime_kind" .= (if runtime==Desktop then "desktop" else "headless" :: Text)]
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
 identity <- readTVar (nativeDevice telemetry)
 pure (if active then Just (ClientContext "native" "desktop" nativeOS (identity >>= id)) else Nothing)

localClientContext :: Telemetry -> Value -> IO (Maybe ClientContext)
localClientContext telemetry value = case parseClientContext value of
 Just (ClientContext "native" _ _ _) -> nativeClientContext telemetry
 context -> do
  active <- enabled telemetry
  pure (if active then context else Nothing)

batchContext :: Telemetry -> Credentials -> IO Value
batchContext telemetry credentials = do
 context <- nativeClientContext telemetry
 let identity = case context of Just (ClientContext _ _ _ device) -> device; _ -> Nothing
 pure $ case hostContext telemetry of
  Object fields -> Object (KM.insert "installation_id" (String (installationId credentials)) (KM.union (deviceFields "host_" identity) fields))
  value -> value

disable :: Telemetry -> Text -> IO ()
disable telemetry reason = atomically $ do
 modifyTVar' (senderStatus telemetry) (\status -> if status==Closed then Closed else Disabled reason)
 invalidate telemetry False

setConsent :: Telemetry -> Bool -> IO (Either Text ())
setConsent = saveConsent True

saveConsent :: Bool -> Telemetry -> Bool -> IO (Either Text ())
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
  Left (e::IOException) -> pure (Left (T.pack (displayException e)))
  Right outcome -> do
   restore $ forM_ outcome $ \(epoch,changed,credentials) -> when changed $ (do
    time <- floor <$> getPOSIXTime
    row <- eventValue time (TelemetryConsentChanged on) Nothing KM.empty
    if on then enqueue telemetry epoch row else when wasReporting $ forM_ credentials $ \auth -> do
     context <- batchContext telemetry auth
     void $ timeout 1000000 $ post telemetry epoch False eventsPath (Just auth) (batchEnvelope context [row]))
    `catch` \(_::IOException) -> pure ()
   pure (Right ())

consentView :: Telemetry -> IO Value
consentView telemetry = atomically $ do
 on <- readTVar (consent telemetry)
 status <- readTVar (senderStatus telemetry)
 pure $ object ["enabled" .= on,"available" .= (status==Running),"heartbeat_interval_ms" .= (realToFrac heartbeatInterval * 1000 :: Double),"error" .= case status of Disabled reason -> reason; _ -> ""]

emit :: Telemetry -> Event -> Maybe Text -> Object -> IO ()
emit telemetry event corpora fields = floor <$> getPOSIXTime >>= \time -> emitAt telemetry time event corpora fields

emitAt :: Telemetry -> Int64 -> Event -> Maybe Text -> Object -> IO ()
emitAt telemetry time event corpora fields = do
 (on,epoch) <- atomically $ (,) <$> reporting telemetry <*> readTVar (consentEpoch telemetry)
 when on $ do
  eventValue time event corpora fields >>= enqueue telemetry epoch

enqueue :: Telemetry -> Integer -> Value -> IO ()
enqueue telemetry epoch row = do
 tick <- toInteger <$> getMonotonicTimeNSec
 let limit = (requestByteLimit - fromIntegral (BL.length (encode (batchEnvelope (hostContext telemetry) []))) - contextReserveBytes - batchLimit) `div` batchLimit
 when (BL.length (encode row)<=fromIntegral limit) $ atomically $ do
  active <- permitted telemetry epoch True
  queue <- readTVar (pendingEvents telemetry)
  when (active && Seq.length queue<queueLimit) (writeTVar (pendingEvents telemetry) (queue Seq.|> Pending tick row))

eventValue :: Int64 -> Event -> Maybe Text -> Object -> IO Value
eventValue time event corpora fields = do
 ident <- UUID.toText <$> UUID.nextRandom
 let common = KM.fromList [("event_id",toJSON ident),("event_type",toJSON (eventName event)),("timestamp",toJSON time)]
     details = case event of TelemetryConsentChanged on -> KM.singleton "enabled" (Bool on); _ -> KM.empty
 pure $ Object $ KM.union common $ KM.union details $ KM.union (maybe KM.empty (KM.singleton "corpora_id" . String) corpora) fields

uiHeartbeat :: Telemetry -> Maybe Text -> Maybe ClientContext -> Value -> IO ()
uiHeartbeat telemetry corpora context value = case value of
 Object fields | KM.lookup "ui_visible" fields==Just (Bool True) && BL.length (encode value)<=8192 -> do
  time <- case KM.lookup "timestamp" fields >>= \v -> case fromJSON v :: Result Int64 of Success seconds -> Just seconds; Error _ -> Nothing of
   Just time -> pure time
   Nothing -> floor <$> getPOSIXTime
  let common = clientContextFields context
      selected = case metadataValue "corpora_id" value of String identity -> UUID.toText <$> UUID.fromText identity; _ -> Nothing
      categories = KM.fromList [("ui_mode",String (category ["local","remote_control"] (metadataValue "ui_mode" value))),("target_location",String (category ["local","remote"] (metadataValue "target_location" value)))]
  emitAt telemetry time UIHeartbeat (corpora `mplus` selected) (KM.union common categories)
 _ -> pure ()

nextBatch :: Telemetry -> IO (Integer,[Value])
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
   pure (Just (epoch,map pendingValue (toList taken)))
 maybe (nextBatch telemetry) pure batch

data Delivery = Unavailable | Rejected | Received Value
post :: Telemetry -> Integer -> Bool -> String -> Maybe Credentials -> Value -> IO Delivery
post telemetry epoch expected route credentials value = withMVar (deliveryLock telemetry) $ \_ -> case origin telemetry of
 Nothing -> pure Unavailable
 Just request -> do
  let authorized = maybe id (\(Credentials ident password) -> applyBasicAuth (TE.encodeUtf8 ident) (TE.encodeUtf8 password)) credentials
      outgoing = authorized request {path=BC.pack route,method="POST",requestBody=RequestBodyLBS (encode value),requestHeaders=[("Content-Type","application/json")],responseTimeout=responseTimeoutMicro requestTimeoutMicros,redirectCount=0,checkResponse = \_ _ -> pure ()}
  let send = timeout requestTimeoutMicros $ (do
       http <- manager telemetry
       active <- atomically $ permitted telemetry epoch expected
       if not active then pure Unavailable else withResponse outgoing http $ \response -> do
        body <- brReadSome (responseBody response) (fromIntegral requestByteLimit)
        let code = statusCode (responseStatus response)
        pure $ if code `elem` [401,403] then Rejected else if code>=200 && code<300 then Received (maybe Null id (decode body)) else Unavailable)
       `catch` \(_::HttpException) -> pure Unavailable
      revoked = atomically (permitted telemetry epoch expected >>= check . not)
  either (const Unavailable) (maybe Unavailable id) <$> race revoked send

credentialsFor :: Telemetry -> Integer -> IO (Maybe Credentials)
credentialsFor telemetry epoch = do
 Saved _ current <- readMVar (savedSettings telemetry)
 case current of
  Just credentials -> pure (Just credentials)
  Nothing -> do
   enrollment <- post telemetry epoch True enrollPath Nothing (object [])
   case enrollment of
    Rejected -> disableAt telemetry epoch "Telemetry enrollment rejected until restart" >> pure Nothing
    Unavailable -> pure Nothing
    Received value -> case fromJSON value of
     Error _ -> pure Nothing
     Success credentials -> do
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
  epoch <- readTVarIO (consentEpoch telemetry)
  cached <- readTVarIO (nativeDevice telemetry)
  when (cached==Nothing) $ do
   identity <- readNativeDevice
   atomically $ permitted telemetry epoch True >>= \active -> when active (writeTVar (nativeDevice telemetry) (Just identity))
  (epoch,batch) <- nextBatch telemetry
  credentials <- credentialsFor telemetry epoch
  forM_ credentials $ \auth -> do
   context <- batchContext telemetry auth
   result <- post telemetry epoch True eventsPath (Just auth) (batchEnvelope context batch)
   when (case result of Rejected -> True; _ -> False) (disableAt telemetry epoch "Telemetry authentication rejected until restart"))
 `catch` \(e::SomeException) -> case fromException e :: Maybe SomeAsyncException of
  Just _ -> throwIO e
  Nothing -> disable telemetry (T.pack (displayException e))

disableAt :: Telemetry -> Integer -> Text -> IO ()
disableAt telemetry epoch reason = atomically $ do
 active <- permitted telemetry epoch True
 when active $ writeTVar (senderStatus telemetry) (Disabled reason) >> invalidate telemetry False
