{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TelemetrySender
  ( HostTelemetry (..)
  , HttpsEndpoint
  , SenderConfig (..)
  , mkHttpsEndpoint
  , startConfiguredTelemetry
  , startTelemetry
  ) where

import Control.Concurrent (MVar, modifyMVar, newMVar)
import Control.Concurrent.Async (async, cancel)
import Control.Concurrent.STM
import Control.Exception
  ( Handler (..), IOException, SomeAsyncException, SomeException, catch, catches
  , finally, fromException, mask, onException, throwIO, try
  )
import Control.Monad (void, when)
import Data.Aeson (Value (..), eitherDecodeStrict)
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import Data.Char (toLower)
import Data.IORef (IORef, readIORef)
import Data.Maybe (isJust)
import qualified Data.Text as T
import Data.Time (defaultTimeLocale, diffUTCTime, getCurrentTime, parseTimeM)
import Data.Version (showVersion)
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)
import Network.HTTP.Client
import Network.HTTP.Types (hAuthorization, hContentType, hRetryAfter, statusCode)
import Network.URI (URI (..), URIAuth (..), parseURI)
import qualified Paths_sapiens_domain as Package
import System.Environment (lookupEnv)
import System.Info (os)
import System.Random (randomRIO)
import System.Timeout (timeout)
import Telemetry
import qualified TelemetryHTTP
import TelemetryIdentity (preparePublicCorporaId)
import TelemetryProfile
import TelemetryWire
import Text.Read (readMaybe)

newtype HttpsEndpoint = HttpsEndpoint String

mkHttpsEndpoint :: String -> Maybe HttpsEndpoint
mkHttpsEndpoint address = do
  uri <- parseURI address
  authority <- uriAuthority uri
  let portValid = case uriPort authority of
        "" -> True
        ':':digits | length digits <= 5 -> case readMaybe digits :: Maybe Int of
          Just number -> number > 0 && number <= 65535 && all (`elem` ['0'..'9']) digits
          Nothing -> False
        _ -> False
  if uriScheme uri == "https:" && null (uriUserInfo authority)
      && not (null (uriRegName authority)) && portValid
      && uriPath uri `elem` ["", "/"] && null (uriQuery uri) && null (uriFragment uri)
    then Just (HttpsEndpoint (show uri {uriPath = ""})) else Nothing

data SenderConfig = SenderConfig
  { senderProfile :: ProfileLocation
  , senderEndpoint :: HttpsEndpoint
  , senderVersion :: AppVersion
  , senderOS :: OsFamily
  , senderRuntime :: RuntimeKind
  }

data HostTelemetry = HostTelemetry
  { hostEmit :: Emit
  , hostConsent :: Maybe ConsentEpoch -> Consent -> IO (Either ProfileError Profile)
  , hostTelemetryStop :: IO ()
  }

data Grant = Grant ConsentEpoch NoticeVersion DeviceId (Maybe BasicCredentials)
  deriving (Eq)

data LocalDecision = FollowProfile | FailedConsent (Maybe ConsentEpoch)

data Control = Control
  { observed :: Maybe Grant
  , localDecision :: LocalDecision
  , rejected :: Maybe BasicCredentials
  }

data QueuedIntent = QueuedIntent ConsentEpoch Word64 (EventRecord EventIntent)
data PendingBatch = PendingBatch ConsentEpoch NoticeVersion DeviceId BasicCredentials BS.ByteString Int

data Sender = Sender
  { config :: SenderConfig
  , manager :: IORef (Maybe Manager)
  , queue :: TBQueue QueuedIntent
  , cached :: TVar (Maybe Grant)
  , alive :: TVar Bool
  , wake :: TMVar ()
  , control :: MVar Control
  , overflowCount :: TVar Word64
  }

startConfiguredTelemetry :: RuntimeKind -> IO HostTelemetry
startConfiguredTelemetry runtime = recover $ do
  endpoint <- lookupEnv "SAPIENS_TELEMETRY_ENDPOINT"
  location <- resolveProfileLocation (if runtime == NativeDesktop then DesktopProfile else HeadlessProfile)
  let system = case os of
        "darwin" -> Just MacOS
        "mingw32" -> Just Windows
        "linux" -> Just Linux
        _ -> Nothing
  case (endpoint >>= mkHttpsEndpoint, mkAppVersion (T.pack (showVersion Package.version)), system, location) of
    (Just target, Just version, Just platform, Right profile) -> do
      settings <- TelemetryHTTP.managerSettings
      startTelemetry settings (SenderConfig profile target version platform runtime)
    _ -> pure disabled
  where
    disabled = HostTelemetry (const (pure ())) writeConsent (pure ())
    writeConsent expected choice = do
      location <- resolveProfileLocation (if runtime == NativeDesktop then DesktopProfile else HeadlessProfile)
      either (pure . Left) (\profile -> commitConsent profile expected choice) location
    recover action = action `catches`
      [ Handler (\(err :: SomeException) -> case fromException err :: Maybe SomeAsyncException of
        Just _ -> throwIO err
        Nothing -> pure disabled)
      ]

startTelemetry :: ManagerSettings -> SenderConfig -> IO HostTelemetry
startTelemetry settings configuration = mask $ \restore -> do
  (client, cleanup) <- TelemetryHTTP.newManagedClient settings
    {managerResponseTimeout = responseTimeoutMicro 2000000, managerRetryableException = const False}
  flip onException cleanup $ do
    sender <- Sender configuration client <$> newTBQueueIO 128 <*> newTVarIO Nothing
      <*> newTVarIO True <*> newEmptyTMVarIO <*> newMVar (Control Nothing FollowProfile Nothing)
      <*> newTVarIO 0
    worker <- async $ restore (workerLoop sender) `catches`
      [ Handler (\(err :: SomeException) -> do
        atomically (disable sender)
        case fromException err :: Maybe SomeAsyncException of
          Just _ -> throwIO err
          Nothing -> pure ())
      ]
    let stop = do
          atomically (writeTVar (alive sender) False >> disable sender)
          cancel worker `finally` cleanup
    pure (HostTelemetry (capture sender) (changeConsent sender) stop)

disable :: Sender -> STM ()
disable sender = do
  writeTVar (cached sender) Nothing
  void (flushTBQueue (queue sender))
  void (tryPutTMVar (wake sender) ())

grantOf :: Profile -> Maybe Grant
grantOf profile = case (profileConsent profile, profileDevice profile) of
  (Just (ConsentRecord (Granted notice) epoch), Just device) ->
    Just (Grant epoch notice device (profileEnrollment profile))
  _ -> Nothing

grantEpoch :: Maybe Grant -> Maybe ConsentEpoch
grantEpoch (Just (Grant epoch _ _ _)) = Just epoch
grantEpoch Nothing = Nothing

refresh :: Sender -> IO (Maybe Grant)
refresh sender = modifyMVar (control sender) (refreshLocked sender)

refreshLocked :: Sender -> Control -> IO (Control, Maybe Grant)
refreshLocked sender previous = do
  profile <- loadProfile (senderProfile (config sender))
  let current = either (const Nothing) grantOf profile
      decision = case localDecision previous of
        FailedConsent epoch | epoch == grantEpoch current -> FailedConsent epoch
        _ -> FollowProfile
      refused = case current of
        Just (Grant _ _ _ credentials) | credentials /= rejected previous -> Nothing
        _ -> rejected previous
      blocked = case decision of
        FailedConsent _ -> True
        FollowProfile -> False
      permitted = if blocked || isJust refused then Nothing else current
      changed = case (observed previous, current) of
        (Just (Grant oldEpoch oldNotice oldDevice oldCredentials), Just (Grant epoch notice device credentials)) ->
          oldEpoch /= epoch || oldNotice /= notice || oldDevice /= device
            || isJust oldCredentials && oldCredentials /= credentials
        _ -> True
  running <- atomically $ do
    live <- readTVar (alive sender)
    when (changed || not (isJust permitted)) (void (flushTBQueue (queue sender)))
    writeTVar (cached sender) (if live then permitted else Nothing)
    pure live
  pure (Control current decision refused, if running then permitted else Nothing)

changeConsent :: Sender -> Maybe ConsentEpoch -> Consent -> IO (Either ProfileError Profile)
changeConsent sender expected choice = do
  outcome <- modifyMVar (control sender) $ \previous -> mask $ \restore -> do
    atomically (disable sender)
    result <- try (restore (commitConsent (senderProfile (config sender)) expected choice))
    case result of
      Right success@(Right _) -> do
        (updated, _) <- refreshLocked sender previous {localDecision = FollowProfile}
        pure (updated, Right success)
      _ -> do
        let failed = previous {localDecision = FailedConsent (grantEpoch (observed previous))}
        pure (failed, result)
  either (throwIO :: SomeException -> IO (Either ProfileError Profile)) pure outcome

capture :: Sender -> Emit
capture sender intent = captureOnce `catch` \(_ :: IOException) -> pure ()
  where
    captureOnce = do
      grant <- readTVarIO (cached sender)
      case grant of
        Nothing -> pure ()
        Just (Grant epoch _ _ _) -> do
          event <- newEventRecord intent
          captured <- getMonotonicTimeNSec
          atomically $ do
            current <- readTVar (cached sender)
            full <- isFullTBQueue (queue sender)
            when (current == grant) $
              if full then modifyTVar' (overflowCount sender) (\count -> if count == maxBound then count else count + 1)
              else writeTBQueue (queue sender) (QueuedIntent epoch captured event)

waitWake :: Sender -> Int -> IO ()
waitWake sender microseconds = do
  timer <- registerDelay (max 0 microseconds)
  atomically $ takeTMVar (wake sender) `orElse` (readTVar timer >>= check)

nextIntent :: Sender -> Int -> IO (Maybe QueuedIntent)
nextIntent sender microseconds = do
  timer <- registerDelay (max 0 microseconds)
  atomically $ (Just <$> readTBQueue (queue sender))
    `orElse` (takeTMVar (wake sender) >> pure Nothing)
    `orElse` (readTVar timer >>= check >> pure Nothing)

pause :: Sender -> Grant -> Int -> IO Bool
pause sender expected microseconds = do
  start <- getMonotonicTimeNSec
  let deadline = start + fromIntegral microseconds * 1000
      loop = do
        current <- refresh sender
        now <- getMonotonicTimeNSec
        if current /= Just expected then pure False
          else if now >= deadline then pure True
          else waitWake sender (fromIntegral (min 10000000 ((deadline - now) `div` 1000))) >> loop
  loop

workerLoop :: Sender -> IO ()
workerLoop sender = do
  grant <- refresh sender >>= enroll sender 0
  case grant of
    Just authorized@(Grant _ _ _ (Just _)) -> do
      first <- nextIntent sender 10000000
      mapM_ (collect sender authorized) first
    _ -> waitWake sender 10000000
  workerLoop sender

enroll :: Sender -> Int -> Maybe Grant -> IO (Maybe Grant)
enroll sender attempt (Just expected@(Grant epoch _ _ Nothing)) = do
  current <- refresh sender
  if current /= Just expected then pure current else do
    reply <- post sender "/v1/enroll" Nothing "{\"protocol_version\":1}"
    case reply of
      Just (201, _, bytes) | Just credentials <- enrollmentResponse bytes -> do
        void (commitEnrollmentIfEmpty (senderProfile (config sender)) epoch credentials)
        refresh sender
      _ | attempt < 2, retryable reply -> do
        delay <- retryDelay attempt reply
        valid <- pause sender expected delay
        if valid then enroll sender (attempt + 1) (Just expected) else refresh sender
      _ -> pure Nothing
enroll _ _ current = pure current

enrollmentResponse :: BS.ByteString -> Maybe BasicCredentials
enrollmentResponse bytes = case eitherDecodeStrict bytes of
  Right (Object fields) | KM.size fields == 3
    , KM.lookup "protocol_version" fields == Just (Number 1)
    , Just (String identifier) <- KM.lookup "installation_id" fields
    , Just (String password) <- KM.lookup "basic_password" fields -> mkBasicCredentials identifier password
  _ -> Nothing

materialize :: Sender -> Grant -> Word64 -> QueuedIntent -> IO (Maybe (EventRecord TelemetryEvent))
materialize sender expected@(Grant epoch _ _ _) deadline (QueuedIntent captured _ record@(EventRecord _ _ intent)) = do
  current <- refresh sender
  if current /= Just expected || captured /= epoch then pure Nothing else
    let scoped reference constructor = do
          now <- getMonotonicTimeNSec
          let budget = if now >= deadline then 1000000 else min 1000000 ((deadline - now) `div` 1000)
          result <- timeout (max 1 (fromIntegral budget)) $
            preparePublicCorporaId (senderProfile (config sender)) epoch (corporaDirectory reference)
          pure $ case result of
            Just (Right public) -> Just (fmap (const (constructor public)) record)
            _ -> Nothing
    in case intent of
      IAppOpened
        | senderRuntime (config sender) == NativeDesktop -> pure (Just (fmap (const AppOpened) record))
        | otherwise -> pure Nothing
      ICorporaCreated reference -> scoped reference CorporaCreated
      IRequestAccepted reference kind -> scoped reference (\public -> RequestAccepted public kind)
      IRemotePaired reference -> scoped reference RemotePaired

collect :: Sender -> Grant -> QueuedIntent -> IO ()
collect sender expected@(Grant epoch notice device (Just credentials)) first@(QueuedIntent _ captured _) = gather [] first
  where
    deadline = captured + 10000000000
    gather records item = do
      event <- materialize sender expected deadline item
      current <- readTVarIO (cached sender)
      when (current == Just expected) $ do
        let accumulated = records <> maybe [] (:[]) event
        now <- getMonotonicTimeNSec
        if length accumulated >= 20 || now >= deadline then flush accumulated else do
          next <- nextIntent sender (fromIntegral ((deadline - now) `div` 1000))
          case next of
            Nothing -> flush accumulated
            Just queued -> gather accumulated queued
    flush records = do
      let configuration = config sender
          context = ClientContext device (senderVersion configuration) (senderOS configuration)
            (senderRuntime configuration) notice
      mapM_ (sendPending sender 0 . (\bytes -> PendingBatch epoch notice device credentials bytes (length records)))
        (encodeBatch context records)
collect _ _ _ = pure ()

sendPending :: Sender -> Int -> PendingBatch -> IO ()
sendPending sender attempt pending@(PendingBatch epoch notice device credentials bytes count) = do
  let expected = Grant epoch notice device (Just credentials)
  current <- refresh sender
  when (current == Just expected) $ do
    reply <- post sender "/v1/events" (Just credentials) bytes
    case reply of
      Just (202, _, body) | acceptedResponse count body -> pure ()
      Just (401, _, _) -> modifyMVar (control sender) $ \previous -> do
        atomically (disable sender)
        pure (previous {rejected = Just credentials}, ())
      _ | attempt < 2, retryable reply -> do
        delay <- retryDelay attempt reply
        valid <- pause sender expected delay
        when valid (sendPending sender (attempt + 1) pending)
      _ -> pure ()

acceptedResponse :: Int -> BS.ByteString -> Bool
acceptedResponse count bytes = case eitherDecodeStrict bytes of
  Right (Object fields) -> KM.size fields == 2
    && KM.lookup "status" fields == Just (String "accepted_volatile")
    && KM.lookup "accepted" fields == Just (Number (fromIntegral count))
  _ -> False

type Reply = (Int, Maybe BS.ByteString, BS.ByteString)

retryable :: Maybe Reply -> Bool
retryable Nothing = True
retryable (Just (code, _, _)) = code >= 200 && code <= 299 || code == 429 || code >= 500 && code <= 599

retryDelay :: Int -> Maybe Reply -> IO Int
retryDelay attempt reply = do
  jitter <- randomRIO (0, 250000)
  now <- getCurrentTime
  let retryAfter = case reply of
        Just (_, Just value, _) | BS.length value <= 64 ->
          case readMaybe (B8.unpack value) :: Maybe Integer of
            Just seconds -> fromInteger (max 0 (min 30 seconds)) * 1000000
            Nothing -> case parseTimeM True defaultTimeLocale "%a, %d %b %Y %H:%M:%S GMT" (B8.unpack value) of
              Just date -> floor (max 0 (min 30 (diffUTCTime date now)) * 1000000)
              Nothing -> 0
        _ -> 0
  pure (min 30000000 (max retryAfter ((if attempt == 0 then 1000000 else 3000000) + jitter)))

post :: Sender -> String -> Maybe BasicCredentials -> BS.ByteString -> IO (Maybe Reply)
post sender route credentials bytes = do
  client <- readIORef (manager sender)
  maybe (pure Nothing) requestWith client
  where
    requestWith client = transport $ timeout 5000000 $ do
      let HttpsEndpoint origin = senderEndpoint (config sender)
      initial <- parseRequest (origin <> route)
      let request = initial
            { method = "POST"
            , redirectCount = 0
            , cookieJar = Nothing
            , requestHeaders = [(hContentType, "application/json")]
                <> maybe [] (\secret -> [(hAuthorization, basicAuthorization secret)]) credentials
            , requestBody = RequestBodyBS bytes
            , responseTimeout = responseTimeoutMicro 2000000
            , checkResponse = \_ _ -> pure ()
            , decompress = const False
            }
      withResponse request client $ \response -> do
        let code = statusCode (responseStatus response)
            json = case lookup hContentType (responseHeaders response) of
              Just value -> B8.map toLower (B8.takeWhile (/= ';') value) == "application/json"
              Nothing -> False
        body <- if code `elem` [201, 202] && json then boundedBody (responseBody response) 4096 [] else pure BS.empty
        let retryAfter = lookup hRetryAfter (responseHeaders response) >>= \value ->
              if BS.length value <= 64 then Just value else Nothing
        pure (code, retryAfter, body)
    transport action = action `catches`
      [ Handler (\(_ :: HttpException) -> pure Nothing)
      , Handler (\(_ :: IOException) -> pure Nothing)
      ]
    boundedBody reader remaining chunks = do
      chunk <- brRead reader
      if BS.length chunk > remaining then pure BS.empty
        else if BS.null chunk then pure (BS.concat (reverse chunks))
        else boundedBody reader (remaining - BS.length chunk) (chunk : chunks)
