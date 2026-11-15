{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TelemetryProfile
  ( BasicCredentials
  , Consent (..)
  , ConsentEpoch
  , ConsentRecord (..)
  , DeviceId
  , InstallationId
  , NoticeVersion
  , Profile
  , ProfileError (..)
  , ProfileLocation
  , ProfileScope (..)
  , basicAuthorization
  , commitConsent
  , commitEnrollmentIfEmpty
  , consentEpochText
  , consentStatusJSON
  , deviceIdText
  , loadProfile
  , mkBasicCredentials
  , mkNoticeVersion
  , noticeVersionNumber
  , parseConsentEpoch
  , profileConsent
  , profileDevice
  , profileEnrollment
  , resolveProfileLocation
  , withGrantedConsent
  ) where

import Control.Exception (Exception, Handler (..), IOException, catches, throwIO)
import Control.Monad (guard, unless, when)
import Data.Aeson
  ( Object
  , Value (Object)
  , eitherDecodeStrict
  , encode
  , object
  , withObject
  , (.:)
  , (.:?)
  , (.=)
  )
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Types (Parser, parseMaybe)
import Data.Bits ((.&.))
import Data.ByteArray (ScrubbedBytes)
import qualified Data.ByteArray as BA
import Data.ByteArray.Encoding
  ( Base (Base64URLUnpadded)
  , convertFromBase
  , convertToBase
  )
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as Base64
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUIDv4
import RuntimePaths (configDirectory, defaultConfigDirectory, externalStoragePath)
import System.Directory (canonicalizePath, createDirectoryIfMissing, getHomeDirectory, makeAbsolute)
import System.Environment (lookupEnv)
import System.FilePath ((</>), equalFilePath, takeDirectory)
import TelemetryStorage
  ( PrivateFileError (..), atomicWritePrivate, checkPrivate, createPrivateDirectory
  , privatePathExists, readPrivateFile, withPrivateLock
  )

newtype NoticeVersion = NoticeVersion Int
  deriving (Eq, Show)

data Consent
  = Denied
  | Granted NoticeVersion
  deriving (Eq, Show)

newtype ConsentEpoch = ConsentEpoch UUID
  deriving (Eq, Show)

consentEpochText :: ConsentEpoch -> Text
consentEpochText (ConsentEpoch epoch) = UUID.toText epoch

data ConsentRecord = ConsentRecord
  { consentChoice :: Consent
  , consentEpoch :: ConsentEpoch
  }
  deriving (Eq, Show)

newtype DeviceId = DeviceId UUID
  deriving (Eq, Show)

newtype InstallationId = InstallationId UUID
  deriving (Eq, Show)

newtype BasicPassword = BasicPassword ScrubbedBytes

data BasicCredentials = BasicCredentials InstallationId BasicPassword

instance Eq BasicCredentials where
  BasicCredentials left (BasicPassword a) == BasicCredentials right (BasicPassword b) =
    left == right && BA.constEq a b

basicAuthorization :: BasicCredentials -> BS.ByteString
basicAuthorization (BasicCredentials (InstallationId identifier) (BasicPassword password)) =
  "Basic " <> Base64.encode (TE.encodeUtf8 (UUID.toText identifier) <> ":" <> BA.convert password)

deviceIdText :: DeviceId -> Text
deviceIdText (DeviceId identifier) = UUID.toText identifier

noticeVersionNumber :: NoticeVersion -> Int
noticeVersionNumber (NoticeVersion version) = version

data Profile = Profile
  { profileConsent :: Maybe ConsentRecord
  , profileDevice :: Maybe DeviceId
  , profileEnrollment :: Maybe BasicCredentials
  }

data ProfileError
  = InvalidProfile
  | InsecureProfile
  | UnsupportedProfileVersion
  | ProfileIOError
  | ConflictingConsentRevision
  deriving (Eq, Show)

instance Exception ProfileError

data ProfileScope = DesktopProfile | HeadlessProfile
  deriving (Eq, Show)

data ProfileLocation = ProfileLocation ProfileScope FilePath

newDeviceId :: IO DeviceId
newDeviceId = DeviceId <$> UUIDv4.nextRandom

mkNoticeVersion :: Int -> Maybe NoticeVersion
mkNoticeVersion 1 = Just (NoticeVersion 1)
mkNoticeVersion _ = Nothing

newConsentEpoch :: IO ConsentEpoch
newConsentEpoch = ConsentEpoch <$> UUIDv4.nextRandom

parseConsentEpoch :: Text -> Maybe ConsentEpoch
parseConsentEpoch input = ConsentEpoch <$> parseUUIDv4 input

mkBasicCredentials :: Text -> Text -> Maybe BasicCredentials
mkBasicCredentials identifier password =
  BasicCredentials <$> (InstallationId <$> parseUUIDv4 identifier)
    <*> parseBasicPassword password

parseUUIDv4 :: Text -> Maybe UUID
parseUUIDv4 input = do
  identifier <- UUID.fromText input
  let (_, version, variant, _) = UUID.toWords identifier
  guard
    ( UUID.toText identifier == input
        && version .&. 0xf000 == 0x4000
        && variant .&. 0xc0000000 == 0x80000000
    )
  pure identifier

parseConsentRecord :: Value -> Parser ConsentRecord
parseConsentRecord = withObject "ConsentRecord" $ \fields -> do
  epochText <- fields .: "consent_epoch"
  epoch <- maybe
    (fail "Invalid consent_epoch")
    (pure . ConsentEpoch)
    (parseUUIDv4 epochText)

  state <- fields .: "state" :: Parser Text
  choice <- case state of
    "denied" -> pure Denied
    "granted" -> do
      version <- fields .: "notice_version"
      maybe
        (fail "Unsupported notice_version")
        (pure . Granted)
        (mkNoticeVersion version)
    _ -> fail "Invalid consent state"
  pure (ConsentRecord choice epoch)

parseBasicPassword :: Text -> Maybe BasicPassword
parseBasicPassword input = do
  let bytes = TE.encodeUtf8 input
  encoded <- BS.stripPrefix "v1." bytes
  guard (BS.length encoded == 43)
  decoded <- either (const Nothing) Just
    (convertFromBase Base64URLUnpadded encoded :: Either String BS.ByteString)
  guard
    ( BS.length decoded == 32
        && convertToBase Base64URLUnpadded decoded == encoded
    )
  pure (BasicPassword (BA.convert bytes))

parseEnrollment :: Value -> Parser BasicCredentials
parseEnrollment = withObject "Enrollment" $ \fields -> do
  identifierText <- fields .: "installation_id"
  passwordText <- fields .: "basic_password"
  maybe
    (fail "Invalid enrollment")
    pure
    (mkBasicCredentials identifierText passwordText)

parseProfileFields :: Object -> Parser Profile
parseProfileFields fields = do
  let consent =
        KM.lookup "consent" fields >>= parseMaybe parseConsentRecord

  deviceText <- fields .:? "device_id"
  device <- traverse
    (\input -> maybe
      (fail "Invalid device_id")
      (pure . DeviceId)
      (parseUUIDv4 input))
    deviceText

  enrollmentValue <- fields .:? "enrollment"
  enrollment <- traverse parseEnrollment enrollmentValue

  pure (Profile consent device enrollment)

parseProfile :: Value -> Either ProfileError Profile
parseProfile (Object fields) =
  case parseMaybe (.: "format_version") fields :: Maybe Integer of
    Nothing -> Left InvalidProfile
    Just 1 -> maybe
      (Left InvalidProfile)
      Right
      (parseMaybe parseProfileFields fields)
    Just _ -> Left UnsupportedProfileVersion
parseProfile _ = Left InvalidProfile

decodeProfile :: BS.ByteString -> Either ProfileError Profile
decodeProfile bytes =
  case eitherDecodeStrict bytes of
    Left _ -> Left InvalidProfile
    Right value -> parseProfile value

consentRecordJSON :: ConsentRecord -> Value
consentRecordJSON (ConsentRecord choice (ConsentEpoch epoch)) =
  object $
    ["consent_epoch" .= UUID.toText epoch]
      <> case choice of
        Denied ->
          ["state" .= ("denied" :: Text)]
        Granted (NoticeVersion version) ->
          [ "state" .= ("granted" :: Text)
          , "notice_version" .= version
          ]

enrollmentJSON :: BasicCredentials -> Value
enrollmentJSON (BasicCredentials (InstallationId identifier) (BasicPassword password)) =
  object
    [ "installation_id" .= UUID.toText identifier
    , "basic_password" .= TE.decodeUtf8 (BA.convert password)
    ]

profileJSON :: Profile -> Value
profileJSON profile = object
  [ "format_version" .= (1 :: Int)
  , "consent" .= (consentRecordJSON <$> profileConsent profile)
  , "device_id" .= ((\(DeviceId identifier) -> UUID.toText identifier)
      <$> profileDevice profile)
  , "enrollment" .= (enrollmentJSON <$> profileEnrollment profile)
  ]

-- The local read surface intentionally exposes neither IDs nor credentials.
consentStatusJSON :: Profile -> Value
consentStatusJSON profile = object
  ["consent" .= (consentRecordJSON <$> profileConsent profile)]

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
emptyProfile = Profile Nothing Nothing Nothing

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
    either throwIO pure (decodeProfile bytes)

withProfileLock :: ProfileLocation -> IO a -> IO a
withProfileLock location@(ProfileLocation _ root) action = do
  checkLocation location
  exists <- privatePathExists root
  unless exists $ do
    createDirectoryIfMissing True (takeDirectory root)
    createPrivateDirectory root
  checkPrivate root True
  let lockPath = root </> "telemetry.lock"
  withPrivateLock lockPath $ do
    checkLocation location
    checkPrivate root True
    action

withGrantedConsent :: ProfileLocation -> ConsentEpoch -> IO a
                   -> IO (Either ProfileError (Maybe a))
withGrantedConsent location@(ProfileLocation _ root) expected action = profileAction $
  withProfileLock location $ do
    current <- readProfile root
    case profileConsent current of
      Just (ConsentRecord (Granted _) epoch) | epoch == expected -> Just <$> action
      _ -> pure Nothing

commitConsent :: ProfileLocation -> Maybe ConsentEpoch -> Consent
              -> IO (Either ProfileError Profile)
commitConsent location@(ProfileLocation _ root) expected choice = profileAction $
  withProfileLock location $ do
    current <- readProfile root
    unless (expected == (consentEpoch <$> profileConsent current))
      (throwIO ConflictingConsentRevision)
    case (profileConsent current, choice) of
      (Nothing, Denied) -> pure current
      (Just record, _) | consentChoice record == choice -> pure current
      _ -> do
        epoch <- newConsentEpoch
        device <- case (choice, profileDevice current) of
          (Granted _, Nothing) -> Just <$> newDeviceId
          _ -> pure (profileDevice current)
        let updated = current
              { profileConsent = Just (ConsentRecord choice epoch)
              , profileDevice = device
              }
        publishProfile location updated
        pure updated

commitEnrollmentIfEmpty :: ProfileLocation -> ConsentEpoch -> BasicCredentials
                        -> IO (Either ProfileError (Maybe BasicCredentials))
commitEnrollmentIfEmpty location@(ProfileLocation _ root) expected credentials = profileAction $
  withProfileLock location $ do
    current <- readProfile root
    case profileConsent current of
      Just (ConsentRecord (Granted _) epoch) | epoch == expected ->
        case profileEnrollment current of
          Just winner -> pure (Just winner)
          Nothing -> do
            publishProfile location current { profileEnrollment = Just credentials }
            pure (Just credentials)
      _ -> pure Nothing

publishProfile :: ProfileLocation -> Profile -> IO ()
publishProfile location@(ProfileLocation _ root) profile =
  atomicWritePrivate (root </> "telemetry.json") (encode (profileJSON profile))
    (checkLocation location)
