{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Auth
  ( AuthError (..), AuthPrincipal, ServerKey, ServerKeyError (..)
  , enrollmentJSON, issueCredentials, loadServerKey, verifyBasic
  )
import Control.Concurrent.STM
import Control.Exception (SomeAsyncException, SomeException, catch, fromException, throwIO)
import Data.Aeson (Value, encode, object, (.=))
import Data.Aeson.Decoding.ByteString (bsToTokens)
import Data.Aeson.Decoding.Tokens
  ( Number (NumInteger), TkRecord (TkPair, TkRecordEnd)
  , Tokens (TkNumber, TkRecordOpen)
  )
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import Data.Char (toLower)
import Data.Time (getCurrentTime)
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp
  ( defaultSettings, runSettings, setHost, setOnException
  , setOnExceptionResponse, setPort, setTimeout
  )
import Parquet (Collector, admitBatch, collectorReady, withCollector)
import Protocol (Admission (..), ValidatedIngest (..), WireError (..), decodeWire)
import System.Environment (getArgs)
import System.Exit (die)
import System.Timeout (timeout)

data Service = Service
  { signingKey :: ServerKey
  , publisher :: Collector
  , requestBudget :: TVar (Word64, Int)
  }

application :: Service -> Application
application service request respond = do
  response <- catch (dispatch service request) $ \(err :: SomeException) ->
    case fromException err :: Maybe SomeAsyncException of
      Just _ -> throwIO err
      Nothing -> pure (failure status500 "internal_error")
  respond response

dispatch :: Service -> Request -> IO Response
dispatch service request =
  case (requestMethod request, pathInfo request) of
    ("POST", ["v1", "enroll"]) -> rateLimited service (enroll (signingKey service) request)
    ("POST", ["v1", "events"]) -> rateLimited service (ingest service request)
    ("GET", ["healthz"]) -> do
      ready <- atomically (collectorReady (publisher service))
      pure $ if ready
        then jsonResponse status200 (object ["status" .= ("ok" :: String)])
        else failure status503 "ingest_unavailable"
    (_, ["v1", "enroll"]) -> pure (methodNotAllowed "POST")
    (_, ["v1", "events"]) -> pure (methodNotAllowed "POST")
    (_, ["healthz"]) -> pure (methodNotAllowed "GET")
    _ -> pure (failure status400 "bad_request")

rateLimited :: Service -> IO Response -> IO Response
rateLimited service action = do
  second <- fmap (flip div 1000000000) getMonotonicTimeNSec
  allowed <- atomically $ do
    (previous, used) <- readTVar (requestBudget service)
    let count = if previous == second then used else 0
    if count >= 100
      then pure False
      else writeTVar (requestBudget service) (second, count + 1) >> pure True
  if allowed then action else pure $
    mapResponseHeaders ((hRetryAfter, "1") :) (failure status429 "rate_limited")

authenticateRequest :: ServerKey -> Request -> Either AuthError AuthPrincipal
authenticateRequest key request =
  case [value | (name, value) <- requestHeaders request, name == hAuthorization] of
    [] -> Left MissingBasic
    [value] -> verifyBasic key value
    _ -> Left InvalidBasic

enroll :: ServerKey -> Request -> IO Response
enroll key request =
  case checkTransport False 1024 request of
    Just response -> pure response
    Nothing -> do
      result <- readBody 1024 request
      case result of
        Left err -> pure (wireFailure err)
        Right bytes
          | validEnrollment bytes -> do
              credentials <- issueCredentials key
              pure (jsonResponse status201 (enrollmentJSON credentials))
          | otherwise -> pure (failure status400 "bad_request")

ingest :: Service -> Request -> IO Response
ingest service request =
  case checkTransport True (64 * 1024) request of
    Just response -> pure response
    Nothing ->
      case authenticateRequest (signingKey service) request of
        Left _ -> pure $
          mapResponseHeaders ((hWWWAuthenticate, "Basic realm=\"telemetry\"") :)
            (failure status401 "invalid_credentials")
        Right principal -> do
          body <- readBody (64 * 1024) request
          case body >>= decodeWire of
            Left err -> pure (wireFailure err)
            Right batch -> do
              received <- getCurrentTime
              result <- atomically $
                admitBatch (publisher service) (ValidatedIngest principal received batch)
              pure $ case result of
                RejectedCapacity -> failure status503 "ingest_unavailable"
                AcceptedVolatile count ->
                  jsonResponse status202 $
                    object ["status" .= ("accepted_volatile" :: String), "accepted" .= count]

checkTransport :: Bool -> Word64 -> Request -> Maybe Response
checkTransport requireJSON limit request
  | not (null encodings) = Just (failure status400 "bad_request")
  | KnownLength size <- requestBodyLength request, size > limit =
      Just (failure status413 "payload_too_large")
  | otherwise =
      case contentTypes of
        [] | not requireJSON -> Nothing
        [value] | mediaType value == "application/json" -> Nothing
        _ -> Just (failure status400 "bad_request")
  where
    encodings = [value | (name, value) <- requestHeaders request, name == hContentEncoding]
    contentTypes = [value | (name, value) <- requestHeaders request, name == hContentType]
    mediaType = B8.map toLower . B8.dropWhileEnd (== ' ')
      . fst . B8.break (== ';') . B8.dropWhile (== ' ')

-- Bound chunked input as it arrives, irrespective of Content-Length.
readBody :: Int -> Request -> IO (Either WireError BS.ByteString)
readBody limit request = do
  result <- timeout (5 * 1000000) (collect limit [])
  pure $ maybe (Left BadJSON) id result
  where
    collect remaining chunks = do
      chunk <- getRequestBodyChunk request
      if BS.length chunk > remaining
        then pure (Left PayloadTooLarge)
        else if BS.null chunk
          then pure (Right (BS.concat (reverse chunks)))
          else collect (remaining - BS.length chunk) (chunk : chunks)

wireFailure :: WireError -> Response
wireFailure PayloadTooLarge = failure status413 "payload_too_large"
wireFailure BadJSON = failure status400 "bad_request"
wireFailure UnknownField = failure status400 "bad_request"
wireFailure _ = failure status422 "invalid_event"

methodNotAllowed :: BS.ByteString -> Response
methodNotAllowed allowed =
  mapResponseHeaders ((hAllow, allowed) :) (failure status405 "method_not_allowed")

failure :: Status -> String -> Response
failure status code =
  jsonResponse status (object ["error" .= object ["code" .= code]])

jsonResponse :: Status -> Value -> Response
jsonResponse status value =
  responseLBS status [(hContentType, "application/json"), (hCacheControl, "no-store")]
    (encode value)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [keyPath] -> start keyPath "telemetry-data"
    [keyPath, root] -> start keyPath root
    _ -> die "Usage: telemetry KEY_FILE [DATASET_ROOT]"
  where
    start keyPath root = do
      result <- loadServerKey keyPath
      case result of
        Left KeyReadFailed -> die "Cannot read telemetry server key"
        Left InvalidKeyLength -> die "Telemetry server key must contain exactly 32 bytes"
        Left InsecureKeyFile -> die "Telemetry server key file is insecure"
        Right key -> catch
          (withCollector root $ \collector -> do
            budget <- newTVarIO (0, 0)
            runSettings
              ( setHost "127.0.0.1" $ setPort 8081 $ setTimeout 10
                $ setOnException (\_ _ -> pure ())
                $ setOnExceptionResponse (\_ -> failure status500 "internal_error")
                $ defaultSettings
              )
              (application (Service key collector budget)))
          (\(err :: SomeException) ->
            case fromException err :: Maybe SomeAsyncException of
              Just _ -> throwIO err
              Nothing -> die "Cannot start telemetry collector")

validEnrollment :: BS.ByteString -> Bool
validEnrollment body =
  case bsToTokens body of
    TkRecordOpen (TkPair "protocol_version" (TkNumber (NumInteger 1) (TkRecordEnd rest))) ->
      BS.all (\byte -> elem byte [9, 10, 13, 32]) rest
    _ -> False
