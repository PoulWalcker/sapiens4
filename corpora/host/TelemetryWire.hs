{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}

module TelemetryWire
  ( AppVersion
  , ClientContext (..)
  , EventRecord (..)
  , OsFamily (..)
  , RuntimeKind (..)
  , TelemetryEvent (..)
  , encodeBatch
  , mkAppVersion
  , newEventRecord
  ) where

import Data.Aeson (Value, encode, object, (.=))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Char (isAlphaNum, isAscii)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime, defaultTimeLocale, formatTime, getCurrentTime)
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUIDv4
import Telemetry (RequestKind (..))
import TelemetryIdentity (PublicCorporaId, publicCorporaIdText)
import TelemetryProfile (DeviceId, NoticeVersion, deviceIdText, noticeVersionNumber)

newtype AppVersion = AppVersion Text

mkAppVersion :: Text -> Maybe AppVersion
mkAppVersion text
  | not (T.null text) && T.length text <= 40 && T.all valid text = Just (AppVersion text)
  | otherwise = Nothing
  where valid c = isAscii c && (isAlphaNum c || c `elem` ("._+-" :: String))

data OsFamily = MacOS | Windows | Linux
data RuntimeKind = NativeDesktop | Headless deriving (Eq)

data ClientContext = ClientContext DeviceId AppVersion OsFamily RuntimeKind NoticeVersion

newtype EventId = EventId UUID

data EventRecord a = EventRecord EventId UTCTime a
  deriving (Functor)

newEventRecord :: a -> IO (EventRecord a)
newEventRecord body = EventRecord <$> (EventId <$> UUIDv4.nextRandom) <*> getCurrentTime <*> pure body

data TelemetryEvent
  = AppOpened
  | CorporaCreated PublicCorporaId
  | RequestAccepted PublicCorporaId RequestKind
  | RemotePaired PublicCorporaId

eventJSON :: EventRecord TelemetryEvent -> Value
eventJSON (EventRecord (EventId identifier) occurred body) = object $
  [ "event_id" .= UUID.toText identifier
  , "occurred_at" .= formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S%3QZ" occurred
  ] <> case body of
    AppOpened -> ["event_type" .= ("app_opened" :: Text)]
    CorporaCreated corpora -> scoped "corpora_created" corpora
    RemotePaired corpora -> scoped "remote_paired" corpora
    RequestAccepted corpora kind -> scoped "request_accepted" corpora
      <> ["request_kind" .= case kind of
            AgentMessage -> ("agent_message" :: Text)
            GroupMessage -> "group_message"
            GroupTaskRun -> "group_task_run"]
  where scoped name corpora = ["event_type" .= (name :: Text), "corpora_id" .= publicCorporaIdText corpora]

encodeBatch :: ClientContext -> [EventRecord TelemetryEvent] -> Maybe BS.ByteString
encodeBatch (ClientContext device (AppVersion version) system runtime notice) events
  | null events || length events > 20 = Nothing
  | runtime == Headless && any appOpened events = Nothing
  | any ((> 1024) . BL.length . encode) values = Nothing
  | BS.length bytes > 64 * 1024 = Nothing
  | otherwise = Just bytes
  where
    appOpened (EventRecord _ _ AppOpened) = True
    appOpened _ = False
    values = map eventJSON events
    bytes = BL.toStrict $ encode $ object
      [ "schema_version" .= (1 :: Int)
      , "device_id" .= deviceIdText device
      , "app_version" .= version
      , "os" .= case system of MacOS -> ("macos" :: Text); Windows -> "windows"; Linux -> "linux"
      , "runtime_kind" .= if runtime == NativeDesktop then ("native_desktop" :: Text) else "headless"
      , "notice_version" .= noticeVersionNumber notice
      , "events" .= values
      ]
