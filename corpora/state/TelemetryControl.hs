{-# LANGUAGE OverloadedStrings #-}

module TelemetryControl
  ( ConsentAction (..)
  , consentActionJSON
  , isConsentMessage
  , localConsent
  , parseConsentAction
  , profileErrorCode
  , runConsentAction
  ) where

import Control.Monad (guard)
import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.KeyMap as KM
import Data.Text (Text)
import TelemetryProfile

data ConsentAction = ReadConsent | WriteConsent (Maybe ConsentEpoch) Consent | ResetIdentity ConsentEpoch

isConsentMessage :: Value -> Bool
isConsentMessage (Object fields) = KM.member "telemetry" fields
isConsentMessage _ = False

parseConsentAction :: Value -> Maybe ConsentAction
parseConsentAction (Object outer) = do
  guard (KM.size outer == 1)
  Object fields <- KM.lookup "telemetry" outer
  case KM.lookup "action" fields of
    Just (String "read") | KM.size fields == 1 -> Just ReadConsent
    Just (String "write") | KM.size fields == 4 -> do
      Bool choice <- KM.lookup "choice" fields
      Number notice <- KM.lookup "notice_version" fields
      guard (notice == 1)
      approved <- mkNoticeVersion 1
      expected <- case KM.lookup "expected_epoch" fields of
        Just Null -> Just Nothing
        Just (String text) -> Just <$> parseConsentEpoch text
        _ -> Nothing
      pure (WriteConsent expected (if choice then Granted approved else Denied))
    Just (String "reset_identity") | KM.size fields == 2 -> do
      String expected <- KM.lookup "expected_epoch" fields
      ResetIdentity <$> parseConsentEpoch expected
    _ -> Nothing
parseConsentAction _ = Nothing

consentActionJSON :: ConsentAction -> Value
consentActionJSON action = object
  [ "telemetry" .= case action of
      ReadConsent -> object ["action" .= ("read" :: Text)]
      ResetIdentity expected -> object
        ["action" .= ("reset_identity" :: Text), "expected_epoch" .= consentEpochText expected]
      WriteConsent expected choice -> object
        [ "action" .= ("write" :: Text)
        , "choice" .= case choice of
            Granted _ -> True
            Denied -> False
        , "notice_version" .= (1 :: Int)
        , "expected_epoch" .= (consentEpochText <$> expected)
        ]
  ]

profileErrorCode :: ProfileError -> Text
profileErrorCode err = case err of
  InvalidProfile -> "invalid_profile"
  InsecureProfile -> "insecure_profile"
  UnsupportedProfileVersion -> "unsupported_profile_version"
  ProfileIOError -> "profile_io_error"
  ConflictingConsentRevision -> "conflicting_consent_revision"
  ResetNotApplicable -> "reset_not_applicable"

runConsentAction :: IO (Either ProfileError Profile, Bool, Maybe Text)
                 -> (Maybe ConsentEpoch -> Consent -> IO (Either ProfileError Profile))
                 -> (ConsentEpoch -> IO (Either ProfileError Profile))
                 -> ConsentAction -> IO Value
runConsentAction readStatus write reset action = case action of
  ReadConsent -> do
    (current, disabled, diagnostic) <- readStatus
    pure (reply current disabled diagnostic)
  WriteConsent expected choice -> do
    write expected choice >>= mutationReply
  ResetIdentity expected -> reset expected >>= mutationReply
  where
    mutationReply result = case result of
      Right profile -> pure (reply (Right profile) False Nothing)
      Left err -> do
        (current, _, _) <- readStatus
        pure (reply current True (Just (profileErrorCode err)))
    reply current disabled failure = object
      [ "consent" .= case current of
          Right profile -> case consentStatusJSON profile of
            Object fields -> KM.lookup "consent" fields
            _ -> Nothing
          Left _ -> Nothing
      , "locally_disabled" .= disabled
      , "error" .= case failure of
          Just err -> Just err
          Nothing -> either (Just . profileErrorCode) (const Nothing) current
      ]

localConsent :: ConsentAction -> IO Value
localConsent action = do
  location <- resolveProfileLocation DesktopProfile
  case location of
    Left err -> runConsentAction (pure (Left err, True, Nothing)) (\_ _ -> pure (Left err)) (\_ -> pure (Left err)) action
    Right profile -> runConsentAction ((\value -> (value, False, Nothing)) <$> loadProfile profile) (commitConsent profile) (resetIdentity profile) action
