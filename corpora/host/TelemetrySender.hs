{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TelemetrySender (HostTelemetry (..), startConfiguredTelemetry) where

import Control.Concurrent (MVar, forkIO, newMVar, modifyMVar, modifyMVar_, withMVar)
import Control.Concurrent.Async (asyncWithUnmask, cancel)
import Control.Concurrent.STM
import Control.Exception (SomeException, SomeAsyncException, IOException, catch, fromException, throwIO)
import Control.Monad (void, when)
import Data.Aeson (Value)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import Data.Char (toLower)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Maybe (isJust)
import qualified Data.Text as T
import Data.Time (UTCTime, defaultTimeLocale, diffUTCTime, getCurrentTime, parseTimeM)
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)
import Network.HTTP.Client hiding (path)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types (hAuthorization, hContentType, hRetryAfter, statusCode)
import System.Info (os)
import System.Environment (lookupEnv)
import System.Random (randomRIO)
import System.Timeout (timeout)
import Telemetry
import TelemetryControl (ConsentAction, runConsentAction)
import TelemetryCredential
import TelemetryEndpoint (configuredEndpoint, configuredAppVersion, endpointOrigin)
import TelemetryIdentity (prepareCorporaId)
import TelemetryProfile
import Text.Read (readMaybe)

data HostTelemetry = HostTelemetry
  { hostEmit :: Emit
  , hostControl :: ConsentAction -> IO Value
  , hostTelemetryStop :: IO ()
  }

data Grant = Grant ConsentEpoch NoticeVersion (Maybe Token) deriving (Eq)
data SenderError = CredentialUpdateFailed | ScopedIdentityUnavailable
senderErrorText :: SenderError -> T.Text
senderErrorText CredentialUpdateFailed = "credential_update_failed"
senderErrorText ScopedIdentityUnavailable = "scoped_identity_unavailable"
data Control = Control (Maybe Grant) (Maybe Token) (Maybe SenderError)
data Intent = Intent ConsentEpoch Word64 UTCTime EventIntent

data Sender = Sender
  { scope :: ProfileScope
  , runtime :: RuntimeKind
  , queue :: TBQueue Intent
  , cached :: TVar (Maybe Grant)
  , stopped :: TVar Bool
  , locallyBlocked :: TVar Bool
  , wake :: TMVar ()
  , control :: MVar Control
  , deliveryConfigured :: TVar Bool
  }

startConfiguredTelemetry :: RuntimeKind -> IO HostTelemetry
startConfiguredTelemetry kind = do
  -- Native launchers carry their process-local failure block across bootstrap
  -- and backend recovery. Only an explicit successful mutation clears it.
  blocked <- (== Just "1") <$> lookupEnv "SAPIENS_TELEMETRY_BLOCKED"
  sender <- Sender (if kind == NativeDesktop then DesktopProfile else HeadlessProfile) kind
    <$> newTBQueueIO 128 <*> newTVarIO Nothing <*> newTVarIO False
    <*> newTVarIO blocked <*> newEmptyTMVarIO <*> newMVar (Control Nothing Nothing Nothing) <*> newTVarIO False
  worker <- asyncWithUnmask $ \unmask -> unmask (background sender) `catch` \(err :: SomeException) -> do
    atomically (writeTVar (stopped sender) True >> closeAdmission sender)
    case fromException err :: Maybe SomeAsyncException of
      Just _ -> throwIO err
      Nothing -> pure ()
  let stop = do
        first <- atomically $ do
          done <- readTVar (stopped sender)
          writeTVar (stopped sender) True
          closeAdmission sender
          pure (not done)
        when first (void (forkIO (cancel worker)))
      action = runConsentAction (readStatus sender)
        (\expected choice -> mutate sender (\location -> commitConsent location expected choice))
        (\expected -> mutate sender (\location -> resetIdentity location expected))
  pure (HostTelemetry (capture sender) action stop)

closeAdmission :: Sender -> STM ()
closeAdmission sender = do
  writeTVar (cached sender) Nothing
  void (flushTBQueue (queue sender))
  void (tryPutTMVar (wake sender) ())

profileLocation :: Sender -> IO (Either ProfileError ProfileLocation)
profileLocation = resolveProfileLocation . scope

load :: Sender -> IO (Either ProfileError Profile)
load sender = profileLocation sender >>= either (pure . Left) loadProfile

grantOf :: Profile -> Maybe Grant
grantOf profile = case profileConsent profile of
  Just (ConsentRecord (Granted notice) epoch) -> Just (Grant epoch notice (profileToken profile))
  _ -> Nothing

grantRevision :: Grant -> ConsentEpoch
grantRevision (Grant epoch _ _) = epoch

refresh :: Sender -> IO (Maybe Grant)
refresh sender = modifyMVar (control sender) (refreshLocked sender)

refreshLocked :: Sender -> Control -> IO (Control, Maybe Grant)
refreshLocked sender (Control previous rejected diagnostic) = do
  profile <- load sender
  let current = either (const Nothing) grantOf profile
      refused = case profile of
        Right value | profileToken value /= rejected -> Nothing
        _ -> rejected
      changed = case (previous, current) of
        (Just (Grant oldEpoch oldNotice oldToken), Just (Grant epoch notice newToken)) ->
          oldEpoch /= epoch || oldNotice /= notice || isJust oldToken && oldToken /= newToken
        _ -> True
      detail = if current /= previous || rejected /= refused then Nothing else diagnostic
  permitted <- atomically $ do
    blocked <- readTVar (locallyBlocked sender)
    dead <- readTVar (stopped sender)
    configured <- readTVar (deliveryConfigured sender)
    let allowed = if blocked || dead || isJust refused || not configured then Nothing else current
    when (changed || not (isJust allowed)) (void (flushTBQueue (queue sender)))
    writeTVar (cached sender) allowed
    pure allowed
  pure (Control current refused detail, permitted)

readStatus :: Sender -> IO (Either ProfileError Profile, Bool, Maybe T.Text)
readStatus sender = withMVar (control sender) $ \(Control _ rejected detail) -> do
  profile <- load sender
  blocked <- readTVarIO (locallyBlocked sender)
  dead <- readTVarIO (stopped sender)
  configured <- readTVarIO (deliveryConfigured sender)
  let refused = case profile of
        Right value -> isJust rejected && profileToken value == rejected
        Left _ -> False
      diagnostic = case profile of
        Left _ -> Nothing
        Right _ | blocked -> Just "consent_write_failed"
                | refused -> Just "invalid_credentials"
                | not configured -> Just "endpoint_unavailable"
                | otherwise -> senderErrorText <$> detail
  pure (profile, blocked || dead || refused, diagnostic)

mutate :: Sender -> (ProfileLocation -> IO (Either ProfileError Profile)) -> IO (Either ProfileError Profile)
mutate sender operation = modifyMVar (control sender) $ \previous -> do
  -- Serialize mutations with admission and refresh. A read or a stale mutation
  -- must never clear the local block left by a failed opt-out.
  wasBlocked <- atomically $ do
    previousBlock <- readTVar (locallyBlocked sender)
    writeTVar (locallyBlocked sender) True
    closeAdmission sender
    pure previousBlock
  result <- profileLocation sender >>= either (pure . Left) operation
  case result of
    Right _ -> atomically (writeTVar (locallyBlocked sender) False)
    Left ConflictingConsentRevision -> atomically (writeTVar (locallyBlocked sender) wasBlocked)
    Left ResetNotApplicable -> atomically (writeTVar (locallyBlocked sender) wasBlocked)
    Left _ -> pure ()
  (updated, _) <- refreshLocked sender previous
  pure (updated, result)

capture :: Sender -> Emit
capture sender intent = do
  occurred <- getCurrentTime
  captured <- getMonotonicTimeNSec
  atomically $ do
    grant <- readTVar (cached sender)
    case grant of
      Just (Grant epoch _ _) -> do
        full <- isFullTBQueue (queue sender)
        when (not full) (writeTBQueue (queue sender) (Intent epoch captured occurred intent))
      Nothing -> pure ()

nextIntent :: Sender -> Int -> IO (Maybe Intent)
nextIntent sender micros = do
  timer <- registerDelay (max 0 micros)
  atomically $ (Just <$> readTBQueue (queue sender))
    `orElse` (takeTMVar (wake sender) >> pure Nothing)
    `orElse` (readTVar timer >>= check >> pure Nothing)

waitWake :: Sender -> Int -> IO ()
waitWake sender micros = do
  timer <- registerDelay (max 0 micros)
  atomically $ takeTMVar (wake sender) `orElse` (readTVar timer >>= check)

pause :: Sender -> Grant -> Int -> IO Bool
pause sender expected micros = do
  start <- getMonotonicTimeNSec
  let deadline = start + fromIntegral micros * 1000
      loop = do
        current <- refresh sender
        now <- getMonotonicTimeNSec
        if current /= Just expected then pure False
          else if now >= deadline then pure True
          else waitWake sender (fromIntegral (min 10000000 ((deadline - now) `div` 1000))) >> loop
  loop

admit :: Sender -> Grant -> IO Bool
admit sender expected@(Grant epoch _ token) = withMVar (control sender) $ \_ -> do
  location <- profileLocation sender
  result <- case location of
    Left err -> pure (Left err)
    Right path -> withAttempt path epoch token $ atomically $ do
      current <- readTVar (cached sender)
      blocked <- readTVar (locallyBlocked sender)
      dead <- readTVar (stopped sender)
      pure (not blocked && not dead && current == Just expected)
  pure (result == Right (Just True))

data Transport = Transport Manager String AppVersion OsFamily

configuredTransport :: IO (Maybe Transport)
configuredTransport = do
  endpoint <- configuredEndpoint
  version <- (>>= mkAppVersion) <$> configuredAppVersion
  let platform = case os of "darwin" -> Just MacOS; "mingw32" -> Just Windows; "linux" -> Just Linux; _ -> Nothing
  case (endpoint, version, platform) of
    (Just origin, Just version, Just system) -> do
      manager <- newManager tlsManagerSettings
        { managerConnCount = 1, managerIdleConnectionCount = 1
        , managerResponseTimeout = responseTimeoutMicro 2000000
        , managerRetryableException = const False }
      pure (Just (Transport manager (endpointOrigin origin) version system))
    _ -> pure Nothing

background :: Sender -> IO ()
background sender = do
  transport <- configuredTransport
  atomically (writeTVar (deliveryConfigured sender) (isJust transport))
  let loop lastEnrollment = do
        grant <- refresh sender
        case (transport, grant) of
          (Just http, Just expected@(Grant epoch _ Nothing)) | lastEnrollment /= Just epoch -> do
            enroll sender http expected
            loop (Just epoch)
          (Just http, Just expected) -> do
            first <- nextIntent sender 10000000
            case first of
              Just item -> case expected of
                Grant _ _ Nothing -> do
                  enroll sender http expected
                  refreshed <- refresh sender
                  case refreshed of
                    Just ready@(Grant epoch _ (Just _)) | epoch == grantRevision expected -> collect sender http ready item
                    _ -> pure ()
                  loop (Just (grantRevision expected))
                Grant _ _ (Just _) -> collect sender http expected item >> loop lastEnrollment
              Nothing -> loop lastEnrollment
          _ -> waitWake sender 10000000 >> loop lastEnrollment
  loop Nothing

enroll :: Sender -> Transport -> Grant -> IO ()
enroll sender transport expected@(Grant _ _ Nothing) = do
  reply <- attempts sender expected (post transport Enrollment)
  case reply of
    Just (Reply 201 _ (Just bytes)) | Right token <- decodeCredentialResponse bytes ->
      install sender expected token
    _ -> pure ()
  atomically $ do
    current <- readTVar (cached sender)
    when (current == Just expected) (void (flushTBQueue (queue sender)))
enroll _ _ _ = pure ()

install :: Sender -> Grant -> Token -> IO ()
install sender expected@(Grant epoch _ old) token = modifyMVar (control sender) $ \previous -> do
  current <- readTVarIO (cached sender)
  result <- if current == Just expected then do
    location <- profileLocation sender
    either (pure . Left) (\path -> commitToken path epoch old token) location
    else pure (Left ConflictingConsentRevision)
  let Control observed rejected _ = previous
      failed = case result of
        Left _ -> True
        Right profile -> case profileToken profile of
          Just saved -> tokenInstallation saved /= tokenInstallation token
          Nothing -> True
      updated = Control observed rejected (if failed && isJust old then Just CredentialUpdateFailed else Nothing)
  (refreshed, _) <- refreshLocked sender updated
  pure (refreshed, ())

materialize :: Sender -> Grant -> Intent -> IO (Maybe Record)
materialize sender expected@(Grant epoch _ _) (Intent revision _ occurred intent)
  | revision /= epoch = pure Nothing
  | otherwise = do
      current <- refresh sender
      if current /= Just expected then pure Nothing else do
        let scoped reference constructor = do
              location <- profileLocation sender
              result <- either (const (pure Nothing))
                (\path -> timeout 1000000 (prepareCorporaId path epoch (corporaDirectory reference))) location
              case result >>= id of
                Nothing -> markError sender expected ScopedIdentityUnavailable >> pure Nothing
                Just identifier -> do
                  clearScopedError sender expected
                  Just <$> newRecord occurred (constructor identifier)
        case intent of
          IAppOpened -> if runtime sender == NativeDesktop then Just <$> newRecord occurred AppOpened else pure Nothing
          ICorporaCreated reference -> scoped reference CorporaCreated
          IRequestAccepted reference -> scoped reference RequestAccepted
          IRemotePaired reference -> scoped reference RemotePaired
          ISchedulerHeartbeat -> Just <$> newRecord occurred SchedulerHeartbeat

collect :: Sender -> Transport -> Grant -> Intent -> IO ()
collect sender transport@(Transport _ _ version system) expected@(Grant _ notice (Just token)) first@(Intent _ captured _ _) = gather [] first
  where
    deadline = captured + 10000000000
    gather records item = do
      event <- materialize sender expected item
      current <- readTVarIO (cached sender)
      when (current == Just expected) $ do
        let accumulated = records <> maybe [] (:[]) event
        now <- getMonotonicTimeNSec
        if length accumulated >= 10 || now >= deadline then flush accumulated else do
          next <- nextIntent sender (fromIntegral ((deadline - now) `div` 1000))
          maybe (flush accumulated) (gather accumulated) next
    flush [] = pure ()
    flush records = do
      let bytes = encodeBatch (Context version system (runtime sender) notice) records
      reply <- attempts sender expected (post transport (Submission token bytes))
      case reply of
        Just (Reply 202 _ (Just body)) | BS.null body -> pure ()
        Just (Reply 202 _ body) -> case body >>= either (const Nothing) Just . decodeCredentialResponse of
          Just replacement | tokenInstallation replacement == tokenInstallation token -> install sender expected replacement
          _ -> markError sender expected CredentialUpdateFailed
        Just (Reply 401 _ _) -> modifyMVar (control sender) $ \previous -> do
          (updated@(Control current _ _), _) <- refreshLocked sender previous
          if current == Just expected then do
            atomically (closeAdmission sender)
            pure (Control current (Just token) Nothing, ())
            else pure (updated, ())
        _ -> pure ()
collect _ _ _ _ = pure ()

markError :: Sender -> Grant -> SenderError -> IO ()
markError sender expected message = modifyMVar (control sender) $ \previous -> do
  (updated@(Control current rejected _), _) <- refreshLocked sender previous
  pure (if current == Just expected then Control current rejected (Just message) else updated, ())

clearScopedError :: Sender -> Grant -> IO ()
clearScopedError sender expected = modifyMVar_ (control sender) $ \previous ->
  pure $ case previous of
    Control current rejected (Just ScopedIdentityUnavailable) | current == Just expected ->
      Control current rejected Nothing
    _ -> previous

data Payload = Enrollment | Submission Token BS.ByteString
data Reply = Reply Int (Maybe BS.ByteString) (Maybe BS.ByteString)

attempts :: Sender -> Grant -> IO (Maybe Reply) -> IO (Maybe Reply)
attempts sender expected action = go 0
  where
    go attempt = do
      allowed <- admit sender expected
      if not allowed then pure Nothing else do
        reply <- action
        if attempt < 2 && retryable reply then do
          delay <- retryDelay attempt reply
          valid <- pause sender expected delay
          if valid then go (attempt + 1) else pure reply
          else pure reply
    retryable Nothing = True
    retryable (Just (Reply code _ _)) = code == 429 || code >= 500 && code <= 599

retryDelay :: Int -> Maybe Reply -> IO Int
retryDelay attempt reply = do
  jitter <- randomRIO (0, 250000)
  now <- getCurrentTime
  let retryAfter = case reply of
        Just (Reply _ (Just value) _) | BS.length value <= 64 ->
          case readMaybe (B8.unpack value) :: Maybe Integer of
            Just seconds -> fromInteger (max 0 (min 30 seconds)) * 1000000
            Nothing -> case parseTimeM True defaultTimeLocale "%a, %d %b %Y %H:%M:%S GMT" (B8.unpack value) of
              Just date -> floor (max 0 (min 30 (diffUTCTime date now)) * 1000000)
              Nothing -> 0
        _ -> 0
  pure (min 30000000 (max retryAfter ((if attempt == 0 then 1000000 else 3000000) + jitter)))

post :: Transport -> Payload -> IO (Maybe Reply)
post (Transport manager origin _ _) payload = do
  acknowledged <- newIORef False
  let fallback = do
        accepted <- readIORef acknowledged
        pure (if accepted then Just (Reply 202 Nothing Nothing) else Nothing)
      run = do
        result <- timeout 5000000 $ do
          initial <- parseRequest (origin <> case payload of Enrollment -> "/credentials"; Submission _ _ -> "/events")
          let request = initial
                { method = "POST", redirectCount = 0, cookieJar = Nothing
                , requestHeaders = [(hContentType, "application/json")]
                    <> case payload of Enrollment -> []; Submission token _ -> [(hAuthorization, "Bearer " <> tokenBytes token)]
                , requestBody = RequestBodyBS (case payload of Enrollment -> BS.empty; Submission _ bytes -> bytes), responseTimeout = responseTimeoutMicro 2000000
                , checkResponse = \_ _ -> pure (), decompress = const False }
          withResponse request manager $ \response -> do
            let code = statusCode (responseStatus response)
                json = case lookup hContentType (responseHeaders response) of
                  Just value -> B8.map toLower (B8.takeWhile (/= ';') value) == "application/json"
                  Nothing -> False
                retryAfter = lookup hRetryAfter (responseHeaders response) >>= \value ->
                  if BS.length value <= 64 then Just value else Nothing
            when (code == 202) (writeIORef acknowledged True)
            body <- if code `elem` [201, 202] then boundedBody (responseBody response) 128 [] else pure Nothing
            pure (Reply code retryAfter (body >>= \value -> if BS.null value || json then Just value else Nothing))
        maybe fallback (pure . Just) result
  (run `catch` (\(_ :: HttpException) -> fallback)) `catch` (\(_ :: IOException) -> fallback)
  where
    boundedBody reader remaining chunks = do
      chunk <- brRead reader
      if BS.length chunk > remaining then pure Nothing
        else if BS.null chunk then pure (Just (BS.concat (reverse chunks)))
        else boundedBody reader (remaining - BS.length chunk) (chunk : chunks)
