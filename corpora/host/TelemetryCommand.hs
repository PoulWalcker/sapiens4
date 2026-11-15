{-# LANGUAGE OverloadedStrings #-}

module TelemetryCommand (run, desktop) where

import Data.Aeson (Value (..), decodeStrict, encode, object, (.=))
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Text (Text)
import qualified Data.Text as T
import System.Exit (exitFailure)
import Telemetry (EventIntent (IAppOpened))
import TelemetryControl
import TelemetryProfile
import TelemetrySender (HostTelemetry (..))
import Text.Read (readMaybe)

run :: [String] -> IO ()
run arguments = do
  let (scope, args) = case arguments of
        "--desktop":rest -> (DesktopProfile, rest)
        _ -> (HeadlessProfile, arguments)
  case args of
    ["--help"] -> putStrLn usage
    ["status"] -> withLocation scope $ \location ->
      loadProfile location >>= result
    ["grant", versionText, expectedText]
      | Just version <- readMaybe versionText >>= mkNoticeVersion
      , Just expected <- expectedEpoch expectedText ->
          withLocation scope $ \location ->
            commitConsent location expected (Granted version) >>= result
    ["deny", expectedText]
      | Just expected <- expectedEpoch expectedText ->
          withLocation scope $ \location -> commitConsent location expected Denied >>= result
    ["reset-identity", expectedText] | Just expected <- parseConsentEpoch (T.pack expectedText) ->
      withLocation scope $ \location -> resetIdentity location expected >>= result
    _ -> do
      output (object ["error" .= ("invalid_arguments" :: Text), "usage" .= usage])
      exitFailure

usage :: String
usage = "sapiens4 telemetry [--desktop] status | grant NOTICE_VERSION EXPECTED_EPOCH | deny EXPECTED_EPOCH | reset-identity EXPECTED_EPOCH; use none when no valid epoch exists"

expectedEpoch :: String -> Maybe (Maybe ConsentEpoch)
expectedEpoch "none" = Just Nothing
expectedEpoch input = Just <$> parseConsentEpoch (T.pack input)

withLocation :: ProfileScope -> (ProfileLocation -> IO ()) -> IO ()
withLocation scope action = resolveProfileLocation scope >>= either reportError action

result :: Either ProfileError Profile -> IO ()
result = either reportError (output . consentStatusJSON)

reportError :: ProfileError -> IO ()
reportError err = do
  output (object ["error" .= profileErrorCode err])
  exitFailure

output :: Value -> IO ()
output = BL.putStrLn . encode

desktop :: HostTelemetry -> IO ()
desktop telemetry = loop False
  where
    loop opened = do
      line <- BS.dropWhileEnd (== '\r') <$> BS.getLine
      case BS.stripPrefix "telemetry " line of
        Just "opened" -> do
          if opened then pure () else hostEmit telemetry IAppOpened
          loop True
        Just bytes | BS.length bytes <= 2048
          , Just (Object fields) <- decodeStrict bytes
          , KM.size fields == 2
          , Just (Number identifier) <- KM.lookup "request" fields
          , identifier > 0
          , Just command <- KM.lookup "command" fields
          , Just action <- parseConsentAction command -> do
              reply <- hostControl telemetry action
              BL.putStrLn ("Sapiens4 telemetry: " <> encode (object ["request" .= identifier, "result" .= reply]))
              loop opened
        _ -> pure ()
