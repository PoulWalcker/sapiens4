{-# LANGUAGE OverloadedStrings #-}

module Main where

import Auth (ServerKey, ServerKeyError (..), enrollmentJSON, issueCredentials, loadServerKey)
import Data.Aeson (Value, encode, object, (.=))
import Data.Aeson.Decoding.ByteString (bsToTokens)
import Data.Aeson.Decoding.Tokens
  ( Number (NumInteger)
  , TkRecord (TkPair, TkRecordEnd)
  , Tokens (TkNumber, TkRecordOpen)
  )
import qualified Data.ByteString as BS
import Network.HTTP.Types
  ( Status
  , hAllow
  , hCacheControl
  , hContentType
  , status201
  , status400
  , status405
  , status413
  , status500
  )
import Network.Wai
  ( Application
  , Request
  , Response
  , getRequestBodyChunk
  , mapResponseHeaders
  , pathInfo
  , requestMethod
  , responseLBS
  )
import Network.Wai.Handler.Warp
  ( defaultSettings
  , runSettings
  , setHost
  , setOnExceptionResponse
  , setPort
  , setTimeout
  )
import System.Environment (getArgs)
import System.Exit (die)

application :: ServerKey -> Application
application key request respond = do
  response <-
    case (requestMethod request, pathInfo request) of
      ("POST", ["v1", "enroll"]) ->
        enroll key request
      (_, ["v1", "enroll"]) ->
        pure $
          mapResponseHeaders
            (\headers -> (hAllow, "POST") : headers)
            (failure status405 "method_not_allowed")
      _ ->
        pure (failure status400 "bad_request")
  respond response

enroll :: ServerKey -> Request -> IO Response
enroll key request = do
  body <- readEnrollmentBody request
  case body of
    Nothing ->
      pure (failure status413 "payload_too_large")
    Just bytes
      | validEnrollment bytes -> do
        credentials <- issueCredentials key
        pure (jsonResponse status201 (enrollmentJSON credentials))
      | otherwise ->
        pure (failure status400 "bad_request")

failure :: Status -> String -> Response
failure status code =
  jsonResponse status (object ["error" .= object ["code" .= code]])

jsonResponse :: Status -> Value -> Response
jsonResponse status value =
  responseLBS
    status
    [ (hContentType, "application/json")
    , (hCacheControl, "no-store")
    ]
    (encode value)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [keyPath] -> do
      result <- loadServerKey keyPath
      case result of
        Left KeyReadFailed ->
          die "Cannot read telemetry server key"
        Left InvalidKeyLength ->
          die "Telemetry server key must contain exactly 32 bytes"
        Left InsecureKeyFile ->
          die "Telemetry server key file is insecure"
        Right key ->
          runSettings
            ( setHost "127.0.0.1"
                $ setPort 8081
                $ setTimeout 5
                $ setOnExceptionResponse (\_ -> failure status500 "internal_error")
                $ defaultSettings
            )
            (application key)
    _ ->
      die "Usage: telemetry KEY_FILE"

readEnrollmentBody :: Request -> IO (Maybe BS.ByteString)
readEnrollmentBody request = collect 1024 []
  where
    collect remaining chunks = do
      chunk <- getRequestBodyChunk request
      let size = BS.length chunk
      if size > remaining
        then pure Nothing
        else
          if BS.null chunk
            then pure (Just (BS.concat (reverse chunks)))
            else collect (remaining - size) (chunk : chunks)

validEnrollment :: BS.ByteString -> Bool
validEnrollment body =
  case bsToTokens body of
    TkRecordOpen (TkPair "protocol_version" (TkNumber (NumInteger 1) (TkRecordEnd rest))) ->
      BS.all (\byte -> byte `elem` [9, 10, 13, 32]) rest
    _ ->
      False
