{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TelemetryProfile
  ( Consent (..), ConsentEpoch, ConsentRecord (..), NoticeVersion
  , Profile, ProfileError (..), ProfileLocation, ProfileScope (..)
  , resolveProfileLocation, loadProfile, commitConsent, resetIdentity
  , commitToken, withAttempt, withGrantedConsent
  , profileConsent, profileToken, consentEpochText, parseConsentEpoch
  , mkNoticeVersion, noticeVersionNumber, consentStatusJSON
  ) where

import Control.Exception (Exception, Handler (..), IOException, catches, throwIO)
import Control.Monad (unless, when)
import Data.Aeson (Value (..), encode, object, withObject, (.:), (.=))
import Data.Aeson.Types (Parser, parseEither)
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import RuntimePaths (configDirectory, defaultConfigDirectory, externalStoragePath)
import System.Directory (canonicalizePath, createDirectoryIfMissing, getHomeDirectory, makeAbsolute)
import System.Environment (lookupEnv)
import System.FilePath ((</>), equalFilePath, takeDirectory)
import TelemetryCredential
import TelemetryJSON (decodeJSON, exactFields)
import TelemetryStorage

newtype NoticeVersion = NoticeVersion Int deriving (Eq, Show)
data Consent = Denied | Granted NoticeVersion deriving (Eq, Show)
newtype ConsentEpoch = ConsentEpoch UUID deriving (Eq, Show)
data ConsentRecord = ConsentRecord
  { consentChoice :: Consent, consentEpoch :: ConsentEpoch } deriving (Eq, Show)
data Profile = Profile
  { profileConsent :: Maybe ConsentRecord, profileToken :: Maybe Token }
data ProfileError = InvalidProfile | InsecureProfile | UnsupportedProfileVersion
  | ProfileIOError | ConflictingConsentRevision | ResetNotApplicable
  deriving (Eq, Show)
instance Exception ProfileError

data ProfileScope = DesktopProfile | HeadlessProfile deriving (Eq, Show)
data ProfileLocation = ProfileLocation ProfileScope FilePath

consentEpochText :: ConsentEpoch -> Text
consentEpochText (ConsentEpoch uuid) = UUID.toText uuid
parseConsentEpoch :: Text -> Maybe ConsentEpoch
parseConsentEpoch input = ConsentEpoch <$> parseUUIDv4 input
mkNoticeVersion :: Int -> Maybe NoticeVersion
mkNoticeVersion 1 = Just (NoticeVersion 1)
mkNoticeVersion _ = Nothing
noticeVersionNumber :: NoticeVersion -> Int
noticeVersionNumber (NoticeVersion number) = number

parseConsent :: Value -> Parser ConsentRecord
parseConsent = withObject "consent" $ \fields -> do
  state <- fields .: "state" :: Parser Text
  epochText <- fields .: "consent_epoch"
  epoch <- maybe (fail "Invalid revision") pure (parseConsentEpoch epochText)
  choice <- case state of
    "denied" -> exactFields ["state", "consent_epoch"] fields >> pure Denied
    "granted" -> do
      exactFields ["state", "consent_epoch", "notice_version"] fields
      number <- fields .: "notice_version"
      maybe (fail "Invalid notice") (pure . Granted) (mkNoticeVersion number)
    _ -> fail "Invalid choice"
  pure (ConsentRecord choice epoch)

parseProfile :: Value -> Parser Profile
parseProfile = withObject "profile" $ \fields -> do
  exactFields ["format_version", "consent", "token"] fields
  version <- fields .: "format_version" :: Parser Int
  unless (version == 1) (fail "Unsupported profile")
  consentValue <- fields .: "consent"
  record <- case consentValue of Null -> pure Nothing; value -> Just <$> parseConsent value
  tokenText <- fields .: "token"
  token <- traverse (\value -> maybe (fail "Invalid token") pure (parseToken (TE.encodeUtf8 value))) tokenText
  pure (Profile record token)

consentJSON :: ConsentRecord -> Value
consentJSON (ConsentRecord choice epoch) = object $
  ["consent_epoch" .= consentEpochText epoch] <> case choice of
    Denied -> ["state" .= ("denied" :: Text)]
    Granted notice -> ["state" .= ("granted" :: Text), "notice_version" .= noticeVersionNumber notice]

consentStatusJSON :: Profile -> Value
consentStatusJSON profile = object ["consent" .= (consentJSON <$> profileConsent profile)]

profileJSON :: Profile -> Value
profileJSON profile = object
  [ "format_version" .= (1 :: Int), "consent" .= (consentJSON <$> profileConsent profile)
  , "token" .= (TE.decodeUtf8 . tokenBytes <$> profileToken profile) ]

profileAction :: IO a -> IO (Either ProfileError a)
profileAction action = (Right <$> action) `catches`
  [ Handler (\(err :: ProfileError) -> pure (Left err))
  , Handler (\(err :: PrivateFileError) -> pure (Left (case err of
      InsecurePrivateFile -> InsecureProfile
      PrivateFileTooLarge -> InvalidProfile)))
  , Handler (\(_ :: IOException) -> pure (Left ProfileIOError))
  ]

resolveProfileLocation :: ProfileScope -> IO (Either ProfileError ProfileLocation)
resolveProfileLocation scope = profileAction $ ProfileLocation scope <$> resolveRoot scope

resolveRoot :: ProfileScope -> IO FilePath
resolveRoot scope = do
  when (scope == HeadlessProfile) $ do
    supplied <- lookupEnv "SAPIENS_CONFIG_DIR"
    when (maybe True null supplied) (throwIO InsecureProfile)
  root <- configDirectory
  when (scope == HeadlessProfile) $ do
    home <- getHomeDirectory >>= canonicalizePath
    shared <- defaultConfigDirectory
    when (any (equalFilePath root) (ancestors shared) || equalFilePath root home)
      (throwIO InsecureProfile)
  inCorpora <- or <$> mapM (privatePathExists . (</> "corpora-identity.json")) (ancestors root)
  when inCorpora (throwIO InsecureProfile)
  dataRoot <- lookupEnv "SAPIENS_DATA_DIR"
  case dataRoot of
    Just path | not (null path) -> do
      directory <- makeAbsolute path >>= canonicalizePath
      when (any (equalFilePath directory) (ancestors root)) (throwIO InsecureProfile)
    _ -> pure ()
  pure root

ancestors :: FilePath -> [FilePath]
ancestors path = path : if parent == path then [] else ancestors parent
  where parent = takeDirectory path

checkLocation :: ProfileLocation -> IO ()
checkLocation (ProfileLocation scope root) = do
  current <- resolveRoot scope
  unless (equalFilePath current root) (throwIO InsecureProfile)
  resolved <- externalStoragePath root
  unless (equalFilePath resolved root) (throwIO InsecureProfile)

emptyProfile :: Profile
emptyProfile = Profile Nothing Nothing

loadProfile :: ProfileLocation -> IO (Either ProfileError Profile)
loadProfile location@(ProfileLocation _ root) = profileAction $ do
  checkLocation location
  exists <- privatePathExists root
  if exists then checkPrivate root True >> readProfile root else pure emptyProfile

readProfile :: FilePath -> IO Profile
readProfile root = do
  let path = root </> "telemetry.json"
  exists <- privatePathExists path
  if not exists then pure emptyProfile else do
    bytes <- readPrivateFile path
    either (const (throwIO InvalidProfile)) pure (decodeJSON (64 * 1024) bytes >>= parseEither parseProfile)

withProfileLock :: ProfileLocation -> IO a -> IO a
withProfileLock location@(ProfileLocation _ root) action = do
  checkLocation location
  exists <- privatePathExists root
  unless exists $ createDirectoryIfMissing True (takeDirectory root) >> createPrivateDirectory root
  checkPrivate root True
  withPrivateLock (root </> "telemetry.lock") $ do
    checkLocation location
    checkPrivate root True
    action

-- The action is the short STM admission only, never HTTP. The profile lock
-- serializes the final snapshot with every cooperating local process.
withAttempt :: ProfileLocation -> ConsentEpoch -> Maybe Token -> IO a
            -> IO (Either ProfileError (Maybe a))
withAttempt location@(ProfileLocation _ root) expected token action = profileAction $
  withProfileLock location $ do
    current <- readProfile root
    case profileConsent current of
      Just (ConsentRecord (Granted _) epoch)
        | epoch == expected && profileToken current == token -> Just <$> action
      _ -> pure Nothing

withGrantedConsent :: ProfileLocation -> ConsentEpoch -> IO a
                   -> IO (Either ProfileError (Maybe a))
withGrantedConsent location@(ProfileLocation _ root) expected action = profileAction $
  withProfileLock location $ do
    current <- readProfile root
    case profileConsent current of
      Just (ConsentRecord (Granted _) epoch) | epoch == expected -> Just <$> action
      _ -> pure Nothing

commitConsent :: ProfileLocation -> Maybe ConsentEpoch -> Consent -> IO (Either ProfileError Profile)
commitConsent location@(ProfileLocation _ root) expected choice = profileAction $
  withProfileLock location $ do
    current <- readProfile root
    unless (expected == (consentEpoch <$> profileConsent current)) (throwIO ConflictingConsentRevision)
    case profileConsent current of
      Nothing | choice == Denied -> pure current
      Just record | consentChoice record == choice -> pure current
      _ -> do
        epoch <- ConsentEpoch <$> UUID.nextRandom
        let updated = current { profileConsent = Just (ConsentRecord choice epoch) }
        publishProfile location updated
        pure updated

resetIdentity :: ProfileLocation -> ConsentEpoch -> IO (Either ProfileError Profile)
resetIdentity location@(ProfileLocation _ root) expected = profileAction $
  withProfileLock location $ do
    current <- readProfile root
    case profileConsent current of
      Nothing -> throwIO ResetNotApplicable
      Just record -> do
        unless (consentEpoch record == expected) (throwIO ConflictingConsentRevision)
        epoch <- ConsentEpoch <$> UUID.nextRandom
        let updated = Profile (Just record { consentEpoch = epoch }) Nothing
        publishProfile location updated
        pure updated

-- First enrollment wins. Renewal additionally preserves installation identity.
-- CAS failure returns the current winner; no unpersisted token is usable.
commitToken :: ProfileLocation -> ConsentEpoch -> Maybe Token -> Token
            -> IO (Either ProfileError Profile)
commitToken location@(ProfileLocation _ root) expected old replacement = profileAction $
  withProfileLock location $ do
    current <- readProfile root
    case profileConsent current of
      Just (ConsentRecord (Granted _) epoch)
        | epoch == expected && profileToken current == old
        , maybe True ((== tokenInstallation replacement) . tokenInstallation) old -> do
            let updated = current { profileToken = Just replacement }
            publishProfile location updated
            pure updated
      _ -> pure current

publishProfile :: ProfileLocation -> Profile -> IO ()
publishProfile location@(ProfileLocation _ root) profile =
  atomicWritePrivate (root </> "telemetry.json") (encode (profileJSON profile)) (checkLocation location)
