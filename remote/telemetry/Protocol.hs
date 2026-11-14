{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Protocol
  ( Admission (..)
  , AppVersion
  , ApplicationId (..)
  , ClientContext (..)
  , DeviceId
  , EventId
  , EventRecord (..)
  , EventType (..)
  , NoticeVersion
  , OsFamily (..)
  , ParquetRow (..)
  , PublicCorporaId
  , RequestKind (..)
  , RuntimeKind (..)
  , TelemetryEvent (..)
  , ValidBatch
  , ValidatedIngest (..)
  , WireBatch (..)
  , WireError (..)
  , appVersionText
  , decodeWire
  , deviceIdText
  , eventIdText
  , noticeVersionNumber
  , publicCorporaIdText
  , rowsForBatch
  ) where

import Auth (AuthPrincipal, InstallationId, isUUIDv4, principalInstallationId)
import Control.Monad (unless, when)
import Data.Aeson (FromJSON (parseJSON), Object, Value (..), eitherDecodeStrict, withObject, withText, (.:))
import Data.Aeson.Decoding.ByteString (bsToTokens)
import Data.Aeson.Decoding.Tokens (TkArray (..), TkRecord (..), Tokens (..))
import Data.Aeson.Key (Key)
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Types (Parser, parseEither)
import Data.ByteArray.Encoding (Base (Base64URLUnpadded), convertFromBase, convertToBase)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import Data.Char (isAlphaNum, isAscii)
import Data.Int (Int32)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (UTCTime, defaultTimeLocale, formatTime, parseTimeM)
import Data.UUID (UUID)
import qualified Data.UUID as UUID

newtype AppVersion = AppVersion T.Text
  deriving (Eq, Show)

data ClientContext = ClientContext
  { logicalDevice :: DeviceId
  , version :: AppVersion
  , osFamily :: OsFamily
  , runtimeKind :: RuntimeKind
  , noticeVersion :: NoticeVersion
  }
  deriving (Eq, Show)

instance FromJSON AppVersion where
  parseJSON = withText "app_version" $ \value ->
    if not (T.null value)
        && T.length value <= 40
        && T.all validCharacter value
      then pure (AppVersion value)
      else fail "Invalid app_version"
    where
      validCharacter character =
        isAscii character
          && (isAlphaNum character
                || character `elem` ("._+-" :: String))

newtype DeviceId = DeviceId UUID
  deriving (Eq, Show)

instance FromJSON DeviceId where
  parseJSON value = DeviceId <$> parseUUIDv4 value

newtype EventId = EventId UUID
  deriving (Eq, Show)

instance FromJSON EventId where
  parseJSON value = EventId <$> parseUUIDv4 value

newtype NoticeVersion = NoticeVersion Int
  deriving (Eq, Show)

instance FromJSON NoticeVersion where
  parseJSON value = do
    number <- parseJSON value :: Parser Int
    if number == 1
      then pure (NoticeVersion number)
      else fail "Unsupported notice_version"

data OsFamily = MacOS | Windows | Linux
  deriving (Eq, Show)

instance FromJSON OsFamily where
  parseJSON = withText "os" $ \value ->
    case value of
      "macos" -> pure MacOS
      "windows" -> pure Windows
      "linux" -> pure Linux
      _ -> fail "Invalid os"

newtype PublicCorporaId = PublicCorporaId BS.ByteString
  deriving (Eq, Show)

instance FromJSON PublicCorporaId where
  parseJSON = withText "corpora_id" $ \value -> do
    let encoded = TE.encodeUtf8 value
    if BS.length encoded /= 43
      then fail "Invalid corpora_id"
      else
        case convertFromBase Base64URLUnpadded encoded of
          Left _ -> fail "Invalid corpora_id"
          Right decoded
            | BS.length decoded == 32
                && convertToBase Base64URLUnpadded decoded == encoded ->
                pure (PublicCorporaId decoded)
            | otherwise -> fail "Invalid corpora_id"

data RequestKind
  = AgentMessage
  | GroupMessage
  | GroupTaskRun
  deriving (Eq, Show)

instance FromJSON RequestKind where
  parseJSON = withText "request_kind" $ \value ->
    case value of
      "agent_message" -> pure AgentMessage
      "group_message" -> pure GroupMessage
      "group_task_run" -> pure GroupTaskRun
      _ -> fail "Invalid request_kind"

data RuntimeKind = NativeDesktop | Headless
  deriving (Eq, Show)

instance FromJSON RuntimeKind where
  parseJSON = withText "runtime_kind" $ \value ->
    case value of
      "native_desktop" -> pure NativeDesktop
      "headless" -> pure Headless
      _ -> fail "Invalid runtime_kind"

data ApplicationId = Sapiens4
  deriving (Eq, Show)

data EventType
  = EAppOpened
  | ECorporaCreated
  | ERequestAccepted
  | ERemotePaired
  deriving (Eq, Show)

data TelemetryEvent
  = AppOpened
  | CorporaCreated PublicCorporaId
  | RequestAccepted PublicCorporaId RequestKind
  | RemotePaired PublicCorporaId
  deriving (Eq, Show)

data EventRecord a = EventRecord
  { eventId    :: EventId
  , occurredAt :: UTCTime
  , eventBody  :: a
  }
  deriving (Eq, Show)

data ParquetRow = ParquetRow
  { rowSchemaVersion :: Int32
  , rowEventId :: EventId
  , rowEventType :: EventType
  , rowOccurredAt :: UTCTime
  , rowReceivedAt :: UTCTime
  , rowInstallation :: InstallationId
  , rowDevice :: DeviceId
  , rowCorpora :: Maybe PublicCorporaId
  , rowRequestKind :: Maybe RequestKind
  , rowAppId :: ApplicationId
  , rowAppVersion :: AppVersion
  , rowOS :: OsFamily
  , rowRuntime :: RuntimeKind
  , rowNotice :: NoticeVersion
  }

newtype ValidBatch =
  ValidBatch (NonEmpty (EventRecord TelemetryEvent))
  deriving (Eq, Show)

data WireBatch = WireBatch ClientContext ValidBatch
  deriving (Eq, Show)

data ValidatedIngest = ValidatedIngest
  { sourceInstallation :: AuthPrincipal
  , serverReceivedAt :: UTCTime
  , incomingBatch :: WireBatch
  }

data Admission
  = AcceptedVolatile Int
  | RejectedCapacity
  deriving (Eq, Show)

data WireError
  = BadJSON
  | UnknownField
  | InvalidEvent
  | PayloadTooLarge
  deriving (Eq, Show)

-- Requires a successfully validated v1 batch.
checkEventSizes :: BS.ByteString -> Either WireError ()
checkEventSizes bytes =
  go (BS.drop 1 (B8.dropWhile (/= '{') bytes))
  where
    go remaining =
      case B8.elemIndex '{' remaining of
        Nothing -> Right ()
        Just start ->
          let eventBytes = BS.drop start remaining
          in case B8.elemIndex '}' eventBytes of
            Nothing -> Left BadJSON
            Just end
              | end + 1 > 1024 -> Left PayloadTooLarge
              | otherwise ->
                  go (BS.drop (end + 1) eventBytes)

checkJSONTokens :: Tokens k e -> Either WireError k
checkJSONTokens = checkDepth 0
  where
    -- Every legal v1 payload is shallow. Bound malformed nesting before
    -- materializing Value or recursing through attacker-controlled input.
    checkDepth :: Int -> Tokens k e -> Either WireError k
    checkDepth depth tokens
      | depth > 32 = Left BadJSON
      | otherwise =
          case tokens of
            TkLit _ next -> Right next
            TkText _ next -> Right next
            TkNumber _ next -> Right next
            TkArrayOpen items -> checkArray (depth + 1) items
            TkRecordOpen fields -> checkRecord (depth + 1) KM.empty fields
            TkErr _ -> Left BadJSON
    checkArray :: Int -> TkArray k e -> Either WireError k
    checkArray depth (TkItem item) =
      checkDepth depth item >>= checkArray depth
    checkArray _ (TkArrayEnd next) = Right next
    checkArray _ (TkArrayErr _) = Left BadJSON
    checkRecord :: Int -> KM.KeyMap () -> TkRecord k e -> Either WireError k
    checkRecord depth seen (TkPair key value)
      | KM.member key seen = Left BadJSON
      | otherwise =
          checkDepth depth value
            >>= checkRecord depth (KM.insert key () seen)
    checkRecord _ _ (TkRecordEnd next) = Right next
    checkRecord _ _ (TkRecordErr _) = Left BadJSON

decodeWire :: BS.ByteString -> Either WireError WireBatch
decodeWire bytes
  | BS.length bytes > 64 * 1024 = Left PayloadTooLarge
  | otherwise = do
      _ <- checkJSONTokens (bsToTokens bytes)
      value <-
        case eitherDecodeStrict bytes of
          Left _ -> Left BadJSON
          Right decoded -> Right decoded
      checkFieldNames value
      case parseEither parseWireBatch value of
        Left _ -> Left InvalidEvent
        Right batch -> do
          checkEventSizes bytes
          Right batch

-- Unknown names are transport/schema-shape errors (400). Declared fields in
-- an invalid event scope are semantic errors, handled by parseEventBody (422).
checkFieldNames :: Value -> Either WireError ()
checkFieldNames (Object fields) = do
  checkNames
    ["schema_version", "device_id", "app_version", "os",
     "runtime_kind", "notice_version", "events"]
    fields
  case KM.lookup "events" fields of
    Just (Array values) -> mapM_ checkEvent values
    _ -> Right ()
  where
    checkEvent (Object event) =
      checkNames ["event_id", "occurred_at", "event_type", "corpora_id", "request_kind"] event
    checkEvent _ = Right ()
    checkNames allowed objectFields =
      unless (all (flip elem allowed) (KM.keys objectFields)) (Left UnknownField)
checkFieldNames _ = Right ()

appVersionText :: AppVersion -> T.Text
appVersionText (AppVersion value) = value

deviceIdText :: DeviceId -> T.Text
deviceIdText (DeviceId value) = UUID.toText value

eventIdText :: EventId -> T.Text
eventIdText (EventId value) = UUID.toText value

noticeVersionNumber :: NoticeVersion -> Int32
noticeVersionNumber (NoticeVersion value) = fromIntegral value

publicCorporaIdText :: PublicCorporaId -> T.Text
publicCorporaIdText (PublicCorporaId value) =
  TE.decodeUtf8 (convertToBase Base64URLUnpadded value)

eventColumns
  :: TelemetryEvent
  -> (EventType, Maybe PublicCorporaId, Maybe RequestKind)
eventColumns AppOpened =
  (EAppOpened, Nothing, Nothing)
eventColumns (CorporaCreated corpora) =
  (ECorporaCreated, Just corpora, Nothing)
eventColumns (RequestAccepted corpora kind) =
  (ERequestAccepted, Just corpora, Just kind)
eventColumns (RemotePaired corpora) =
  (ERemotePaired, Just corpora, Nothing)

parseClientContext :: Object -> Parser ClientContext
parseClientContext fields =
  ClientContext
    <$> fields .: "device_id"
    <*> fields .: "app_version"
    <*> fields .: "os"
    <*> fields .: "runtime_kind"
    <*> fields .: "notice_version"

parseEventBody :: Object -> Parser TelemetryEvent
parseEventBody fields = do
  eventType <- fields .: "event_type" :: Parser T.Text
  let common = ["event_id", "occurred_at", "event_type"]
  case eventType of
    "app_opened" -> do
      rejectUnknownFields common fields
      pure AppOpened
    "corpora_created" -> do
      rejectUnknownFields (common ++ ["corpora_id"]) fields
      CorporaCreated <$> fields .: "corpora_id"
    "request_accepted" -> do
      rejectUnknownFields
        (common ++ ["corpora_id", "request_kind"]) fields
      RequestAccepted
        <$> fields .: "corpora_id"
        <*> fields .: "request_kind"
    "remote_paired" -> do
      rejectUnknownFields (common ++ ["corpora_id"]) fields
      RemotePaired <$> fields .: "corpora_id"
    _ -> fail "Invalid event_type"

parseEventRecord :: Value -> Parser (EventRecord TelemetryEvent)
parseEventRecord = withObject "event" $ \fields -> do
  identifier <- fields .: "event_id"
  timestampValue <- fields .: "occurred_at"
  timestamp <- parseOccurredAt timestampValue
  body <- parseEventBody fields
  pure (EventRecord identifier timestamp body)

parseEvents :: Value -> Parser ValidBatch
parseEvents value = do
  values <- parseJSON value :: Parser (NonEmpty Value)
  when (NE.length values > 20) $ fail "Invalid events"
  ValidBatch <$> traverse parseEventRecord values

parseOccurredAt :: Value -> Parser UTCTime
parseOccurredAt = withText "occurred_at" $ \value -> do
  let input = T.unpack value
  if T.length value /= 24
    then fail "Invalid occurred_at"
    else
      case parseTimeM False defaultTimeLocale
        "%Y-%m-%dT%H:%M:%S%QZ" input of
        Just timestamp
          | formatTime defaultTimeLocale
              "%Y-%m-%dT%H:%M:%S%3QZ" timestamp == input ->
              pure timestamp
        _ -> fail "Invalid occurred_at"

parseUUIDv4 :: Value -> Parser UUID
parseUUIDv4 = withText "UUIDv4" $ \value ->
  case UUID.fromText value of
    Just identifier
      | UUID.toText identifier == value && isUUIDv4 identifier ->
          pure identifier
    _ -> fail "Invalid UUIDv4"

parseWireBatch :: Value -> Parser WireBatch
parseWireBatch = withObject "batch" $ \fields -> do
  schemaVersion <- fields .: "schema_version" :: Parser Int
  unless (schemaVersion == 1) $
    fail "Unsupported schema_version"
  context <- parseClientContext fields
  eventsValue <- fields .: "events"
  batch@(ValidBatch records) <- parseEvents eventsValue
  when
    (runtimeKind context == Headless
      && any ((== AppOpened) . eventBody) records) $
    fail "Invalid event runtime"
  pure (WireBatch context batch)

rowsForBatch :: ValidatedIngest -> NonEmpty ParquetRow
rowsForBatch ingest =
  fmap toRow records
  where
    WireBatch context (ValidBatch records) = incomingBatch ingest

    toRow record =
      let (eventType, corpora, request) = eventColumns (eventBody record)
      in ParquetRow
        { rowSchemaVersion = 1
        , rowEventId = eventId record
        , rowEventType = eventType
        , rowOccurredAt = occurredAt record
        , rowReceivedAt = serverReceivedAt ingest
        , rowInstallation =
            principalInstallationId (sourceInstallation ingest)
        , rowDevice = logicalDevice context
        , rowCorpora = corpora
        , rowRequestKind = request
        , rowAppId = Sapiens4
        , rowAppVersion = version context
        , rowOS = osFamily context
        , rowRuntime = runtimeKind context
        , rowNotice = noticeVersion context
        }

rejectUnknownFields :: [Key] -> Object -> Parser ()
rejectUnknownFields allowed fields
  | all (`elem` allowed) (KM.keys fields) = pure ()
  | otherwise = fail "Unknown field"
