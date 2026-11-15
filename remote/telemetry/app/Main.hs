{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Auth
import Control.Concurrent.STM
import Control.Exception (SomeAsyncException, SomeException, mask, finally, catch, fromException, throwIO)
import Data.Aeson (Value, encode, object, (.=))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import Data.Char (toLower)
import Data.Time (getCurrentTime)
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp
  ( defaultSettings, runSettings, setHost, setPort, setTimeout
  , setMaxTotalHeaderLength, setMaximumBodyFlush, setOnException, setOnExceptionResponse
  )
import Parquet (Admission (..), Collector, admitBatch, collectorReady, withCollector)
import Protocol (WireError (..), decodeRows)
import System.Environment (getArgs, lookupEnv)
import System.Exit (die)
import System.Timeout (timeout)
import TelemetryCredential (credentialResponse)
import Text.Read (readMaybe)

data Service = Service Auth Collector (TVar (Word64, Int)) (TVar Int) Int

application :: Service -> Application
application service request respond = do
  response <- catch (dispatch service request) $ \(err :: SomeException) ->
    case fromException err :: Maybe SomeAsyncException of
      Just _ -> throwIO err
      Nothing -> pure (failure status500 "internal_error")
  respond response

dispatch :: Service -> Request -> IO Response
dispatch service@(Service auth collector _ _ _) request =
  case (requestMethod request, pathInfo request) of
    ("POST", ["credentials"]) -> bounded service (enroll auth request)
    ("POST", ["events"]) -> bounded service (ingest service request)
    ("GET", ["healthz"]) -> do
      ready <- atomically (collectorReady collector)
      pure $ if ready then jsonResponse status200 (object ["status" .= ("ok" :: String)])
        else failure status503 "ingest_unavailable"
    (_, ["credentials"]) -> pure (methodNotAllowed "POST")
    (_, ["events"]) -> pure (methodNotAllowed "POST")
    (_, ["healthz"]) -> pure (methodNotAllowed "GET")
    _ -> pure (failure status404 "not_found")

-- Preserve the existing application ceiling. Public route/IP budgets and
-- connection policy are separately required at ingress.
bounded :: Service -> IO Response -> IO Response
bounded (Service _ _ budget active maximumActive) action = do
  second <- (`div` 1000000000) <$> getMonotonicTimeNSec
  mask $ \restore -> do
    allowed <- atomically $ do
      (previous, used) <- readTVar budget
      running <- readTVar active
      let count = if previous == second then used else 0
      if count >= 100 || running >= maximumActive then pure False else do
        writeTVar budget (second, count + 1)
        writeTVar active (running + 1)
        pure True
    if allowed then restore action `finally` atomically (modifyTVar' active (subtract 1))
      else pure $ mapResponseHeaders ((hRetryAfter, "1") :) (failure status429 "rate_limited")

enroll :: Auth -> Request -> IO Response
enroll auth request
  | any ((== hAuthorization) . fst) (requestHeaders request) = pure (failure status400 "bad_request")
  | otherwise = case checkTransport False 0 request of
      Just response -> pure response
      Nothing -> do
        body <- readBody 0 request
        case body of
          Left err -> pure (wireFailure err)
          Right _ -> do
            issued <- issueCredential auth
            pure $ either (const (failure status500 "internal_error"))
              (jsonResponse status201 . credentialResponse) issued

ingest :: Service -> Request -> IO Response
ingest (Service auth collector _ _ _) request =
  case verifyAuthorization auth [value | (name, value) <- requestHeaders request, name == hAuthorization] of
    Nothing -> pure $ mapResponseHeaders ((hWWWAuthenticate, "Bearer realm=\"d4\"") :)
      (failure status401 "invalid_credentials")
    Just principal -> case checkTransport True (64 * 1024) request of
      Just response -> pure response
      Nothing -> do
        body <- readBody (64 * 1024) request
        received <- getCurrentTime
        case body >>= decodeRows principal received of
          Left err -> pure (wireFailure err)
          Right rows -> do
            result <- atomically (admitBatch collector rows)
            pure $ case result of
              RejectedCapacity -> failure status503 "ingest_unavailable"
              AcceptedVolatile -> case replacementCredential auth principal of
                Nothing -> emptyResponse status202
                Just token -> jsonResponse status202 (credentialResponse token)

checkTransport :: Bool -> Word64 -> Request -> Maybe Response
checkTransport requireJSON limit request
  | not (null encodings) = Just (failure status400 "bad_request")
  | KnownLength size <- requestBodyLength request, size > limit = Just (failure status413 "payload_too_large")
  | otherwise = case contentTypes of
      [] | not requireJSON -> Nothing
      [value] | mediaType value == "application/json" -> Nothing
      _ -> Just (failure status400 "bad_request")
  where
    encodings = [value | (name, value) <- requestHeaders request, name == hContentEncoding]
    contentTypes = [value | (name, value) <- requestHeaders request, name == hContentType]
    mediaType = B8.map toLower . B8.dropWhileEnd (== ' ')
      . fst . B8.break (== ';') . B8.dropWhile (== ' ')

readBody :: Int -> Request -> IO (Either WireError BS.ByteString)
readBody limit request = do
  result <- timeout 5000000 (collect limit [])
  pure (maybe (Left BadJSON) id result)
  where
    collect remaining chunks = do
      chunk <- getRequestBodyChunk request
      if BS.length chunk > remaining then pure (Left PayloadTooLarge)
        else if BS.null chunk then pure (Right (BS.concat (reverse chunks)))
        else collect (remaining - BS.length chunk) (chunk : chunks)

wireFailure :: WireError -> Response
wireFailure PayloadTooLarge = failure status413 "payload_too_large"
wireFailure BadJSON = failure status400 "bad_request"
wireFailure UnknownField = failure status400 "bad_request"
wireFailure InvalidEvent = failure status422 "invalid_event"
methodNotAllowed :: BS.ByteString -> Response
methodNotAllowed allowed = mapResponseHeaders ((hAllow, allowed) :) (failure status405 "method_not_allowed")
failure :: Status -> String -> Response
failure status code = jsonResponse status (object ["error" .= object ["code" .= code]])
jsonResponse :: Status -> Value -> Response
jsonResponse status value = responseLBS status
  [(hContentType, "application/json"), (hCacheControl, "no-store")] (encode value)
emptyResponse :: Status -> Response
emptyResponse status = responseLBS status [(hCacheControl, "no-store")] ""

requiredPositive :: String -> IO Int
requiredPositive name = do
  supplied <- lookupEnv name
  case supplied >>= readMaybe of
    Just value | value > 0 -> pure value
    _ -> die ("Set a positive integer " <> name)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [config] -> start config "telemetry-v2-data"
    [config, root] -> start config root
    _ -> die "Usage: telemetry AUTH_CONFIG [NEW_DATASET_ROOT]"
  where
    start config root = do
      auth <- loadAuth config >>= either (const (die "Invalid or insecure telemetry auth configuration")) pure
      maximumActive <- requiredPositive "TELEMETRY_MAX_INFLIGHT"
      headerBytes <- requiredPositive "TELEMETRY_MAX_HEADER_BYTES"
      catch (withCollector root $ \collector -> do
        service <- Service auth collector <$> newTVarIO (0, 0) <*> newTVarIO 0 <*> pure maximumActive
        runSettings
          ( setHost "127.0.0.1" $ setPort 8081 $ setTimeout 10
          $ setMaxTotalHeaderLength headerBytes $ setMaximumBodyFlush (Just 0)
          $ setOnException (\_ _ -> pure ())
          $ setOnExceptionResponse (\_ -> failure status500 "internal_error") defaultSettings)
          (application service)) $ \(err :: SomeException) ->
            case fromException err :: Maybe SomeAsyncException of
              Just _ -> throwIO err
              Nothing -> die "Cannot start telemetry collector"
