{-# LANGUAGE OverloadedStrings #-}

module TelemetryCommand (run) where

import Data.Aeson (Value, encode, object, (.=))
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Text (Text)
import qualified Data.Text as T
import System.Exit (exitFailure)
import TelemetryProfile
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
    _ -> do
      output (object ["error" .= ("invalid_arguments" :: Text), "usage" .= usage])
      exitFailure

usage :: String
usage = "sapiens4 telemetry [--desktop] status | grant NOTICE_VERSION EXPECTED_EPOCH | deny EXPECTED_EPOCH; use none when no valid epoch exists"

expectedEpoch :: String -> Maybe (Maybe ConsentEpoch)
expectedEpoch "none" = Just Nothing
expectedEpoch input = Just <$> parseConsentEpoch (T.pack input)

withLocation :: ProfileScope -> (ProfileLocation -> IO ()) -> IO ()
withLocation scope action = resolveProfileLocation scope >>= either reportError action

result :: Either ProfileError Profile -> IO ()
result = either reportError (output . consentStatusJSON)

reportError :: ProfileError -> IO ()
reportError err = do
  let code :: Text
      code = case err of
        InvalidProfile -> "invalid_profile"
        InsecureProfile -> "insecure_profile"
        UnsupportedProfileVersion -> "unsupported_profile_version"
        ProfileIOError -> "profile_io_error"
        ConflictingConsentRevision -> "conflicting_consent_revision"
  output (object ["error" .= code])
  exitFailure

output :: Value -> IO ()
output = BL.putStrLn . encode
