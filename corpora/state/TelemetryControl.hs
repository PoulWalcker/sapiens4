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
import Data.IORef (IORef, atomicModifyIORef', readIORef, writeIORef)
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
      Right _ -> do
        (current, disabled, diagnostic) <- readStatus
        pure (reply current disabled diagnostic)
      Left err -> do
        (current, disabled, _) <- readStatus
        pure (reply current disabled (Just (profileErrorCode err)))
    reply current disabled failure = object
      [ "consent" .= case current of
          Right profile -> consentJSON <$> profileConsent profile
          Left _ -> Nothing
      , "locally_disabled" .= disabled
      , "error" .= case failure of
          Just err -> Just err
          Nothing -> either (Just . profileErrorCode) (const Nothing) current
      ]

-- Bootstrap has no sender yet. Keep failures latched in the native process so
-- continuing setup cannot revive an older grant when the backend starts.
localConsent :: IORef Bool -> ConsentAction -> IO Value
localConsent blocked action = do
  previous <- atomicModifyIORef' blocked $ \current ->
    (case action of ReadConsent -> current; _ -> True, current)
  location <- resolveProfileLocation DesktopProfile
  let status readProfile = do
        profile <- readProfile
        disabled <- readIORef blocked
        pure (profile, disabled, if disabled then Just "consent_write_failed" else Nothing)
      mutation operation = do
        result <- operation
        case result of
          Right _ -> writeIORef blocked False
          Left ConflictingConsentRevision -> writeIORef blocked previous
          Left ResetNotApplicable -> writeIORef blocked previous
          Left _ -> pure ()
        pure result
  case location of
    Left err -> runConsentAction (status (pure (Left err)))
      (\_ _ -> mutation (pure (Left err))) (\_ -> mutation (pure (Left err))) action
    Right profile -> runConsentAction (status (loadProfile profile))
      (\expected choice -> mutation (commitConsent profile expected choice))
      (\expected -> mutation (resetIdentity profile expected)) action
