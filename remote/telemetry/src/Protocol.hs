{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Protocol
  ( Row, Event (..), WireError (..), decodeRows
  , rowEventId, rowEvent, rowOccurredAt, rowReceivedAt, rowInstallation
  , rowAppVersion, rowOS, rowRuntime, rowNotice, eventType, eventCorpora
  ) where

import Auth (Principal, principalInstallation)
import Control.Monad (when)
import Data.Aeson (Value, withObject, withText, (.:))
import Data.Aeson.Types (Parser, parseEither)
import qualified Data.ByteString as BS
import Data.List.NonEmpty (NonEmpty)
import qualified Data.Text as T
import Data.Time (UTCTime, defaultTimeLocale, formatTime, parseTimeM)
import Telemetry
import TelemetryCredential (InstallationId)
import TelemetryJSON (decodeJSON, exactFields)

data Row = Row
  { rowEventId :: EventId
  , rowEvent :: Event
  , rowOccurredAt :: UTCTime
  , rowReceivedAt :: UTCTime
  , rowInstallation :: InstallationId
  , rowAppVersion :: AppVersion
  , rowOS :: OsFamily
  , rowRuntime :: RuntimeKind
  , rowNotice :: NoticeVersion
  }

data WireError = BadJSON | InvalidEvent | PayloadTooLarge
  deriving (Eq, Show)

decodeRows :: Principal -> UTCTime -> BS.ByteString -> Either WireError (NonEmpty Row)
decodeRows principal received bytes
  | BS.length bytes > 64 * 1024 = Left PayloadTooLarge
  | otherwise = do
      value <- either (const (Left BadJSON)) Right (decodeJSON (64 * 1024) bytes)
      case parseEither parseBatch value of
        Left _ -> Left InvalidEvent
        Right rows -> Right rows
  where
    parseBatch = withObject "batch" $ \fields -> do
      exactFields ["app_version", "os", "runtime_kind", "notice_version", "events"] fields
      version <- fields .: "app_version" >>= require mkAppVersion
      os <- fields .: "os" >>= enum [("macos", MacOS), ("windows", Windows), ("linux", Linux)]
      runtime <- fields .: "runtime_kind" >>= enum [("native_desktop", NativeDesktop), ("headless", Headless)]
      notice <- fields .: "notice_version" >>= require mkNoticeVersion
      events <- fields .: "events" :: Parser (NonEmpty Value)
      when (length events > 10) (fail "Invalid batch size")
      traverse (parseEvent version os runtime notice) events
    parseEvent version os runtime notice = withObject "event" $ \fields -> do
      identifier <- fields .: "event_id" >>= require parseEventId
      occurred <- fields .: "occurred_at" >>= timestamp
      kind <- fields .: "event_type" :: Parser T.Text
      let common = ["event_id", "occurred_at", "event_type"]
          scoped constructor = do
            exactFields (common <> ["corpora_id"]) fields
            constructor <$> (fields .: "corpora_id" >>= require parseCorporaId)
      event <- case kind of
        "app_opened" -> do
          exactFields common fields
          when (runtime == Headless) (fail "Invalid headless event")
          pure AppOpened
        "scheduler_heartbeat" -> exactFields common fields >> pure SchedulerHeartbeat
        "corpora_created" -> scoped CorporaCreated
        "request_accepted" -> scoped RequestAccepted
        "remote_paired" -> scoped RemotePaired
        _ -> fail "Invalid event"
      pure (Row identifier event occurred received (principalInstallation principal) version os runtime notice)
    require parse value = maybe (fail "Invalid value") pure (parse value)
    enum choices (value :: T.Text) = require (`lookup` choices) value
    timestamp = withText "timestamp" $ \value ->
      case parseTimeM False defaultTimeLocale "%Y-%m-%dT%H:%M:%S%QZ" (T.unpack value) of
        Just time | T.length value == 24
          && T.pack (formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S%3QZ" time) == value -> pure time
        _ -> fail "Invalid timestamp"
