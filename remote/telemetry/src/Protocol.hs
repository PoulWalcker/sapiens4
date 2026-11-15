{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Protocol
  ( Row, Event (..), WireError (..), decodeRows
  , rowEventId, rowEvent, rowOccurredAt, rowReceivedAt, rowInstallation
  , rowAppVersion, rowOS, rowRuntime, rowNotice, eventType, eventCorpora
  ) where

import Auth (Principal, principalInstallation)
import Control.Monad (unless, when)
import Data.Aeson (Value, withObject, withText, (.:))
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Types (Parser, parseEither)
import qualified Data.ByteString as BS
import Data.Char (isAlphaNum, isAscii)
import Data.Int (Int32)
import Data.List (isInfixOf)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.Text as T
import Data.Time (UTCTime, defaultTimeLocale, formatTime, parseTimeM)
import qualified Data.UUID as UUID
import TelemetryCredential (parseUUIDv4)
import TelemetryJSON (decodeJSON, exactFields)

data Event = AppOpened | CorporaCreated T.Text | RequestAccepted T.Text
  | RemotePaired T.Text | SchedulerHeartbeat

data Row = Row
  { rowEventId :: T.Text
  , rowEvent :: Event
  , rowOccurredAt :: UTCTime
  , rowReceivedAt :: UTCTime
  , rowInstallation :: T.Text
  , rowAppVersion :: T.Text
  , rowOS :: T.Text
  , rowRuntime :: T.Text
  , rowNotice :: Int32
  }

data WireError = BadJSON | UnknownField | InvalidEvent | PayloadTooLarge
  deriving (Eq, Show)

eventType :: Event -> T.Text
eventType AppOpened = "app_opened"
eventType (CorporaCreated _) = "corpora_created"
eventType (RequestAccepted _) = "request_accepted"
eventType (RemotePaired _) = "remote_paired"
eventType SchedulerHeartbeat = "scheduler_heartbeat"

eventCorpora :: Event -> Maybe T.Text
eventCorpora (CorporaCreated identifier) = Just identifier
eventCorpora (RequestAccepted identifier) = Just identifier
eventCorpora (RemotePaired identifier) = Just identifier
eventCorpora _ = Nothing

decodeRows :: Principal -> UTCTime -> BS.ByteString -> Either WireError (NonEmpty Row)
decodeRows principal received bytes
  | BS.length bytes > 64 * 1024 = Left PayloadTooLarge
  | otherwise = do
      value <- either (const (Left BadJSON)) Right (decodeJSON (64 * 1024) bytes)
      case parseEither parseBatch value of
        Left message -> Left (if "unknown_field" `isInfixOf` message then UnknownField else InvalidEvent)
        Right rows -> Right rows
  where
    parseBatch = withObject "batch" $ \fields -> do
      allowed ["app_version", "os", "runtime_kind", "notice_version", "events"] fields
      version <- fields .: "app_version"
      unless (not (T.null version) && T.length version <= 40
        && T.all (\c -> isAscii c && (isAlphaNum c || c `elem` ("._+-" :: String))) version)
        (fail "Invalid version")
      os <- fields .: "os"
      unless (os `elem` ["macos", "windows", "linux"]) (fail "Invalid OS")
      runtime <- fields .: "runtime_kind"
      unless (runtime `elem` ["native_desktop", "headless"]) (fail "Invalid runtime")
      notice <- fields .: "notice_version"
      unless (notice == 1) (fail "Invalid notice")
      events <- fields .: "events" :: Parser (NonEmpty Value)
      when (length events > 10) (fail "Invalid batch size")
      traverse (parseEvent version os runtime notice) events
    parseEvent version os runtime notice = withObject "event" $ \fields -> do
      allowed ["event_id", "occurred_at", "event_type", "corpora_id"] fields
      identifier <- fields .: "event_id" >>= uuid
      occurred <- fields .: "occurred_at" >>= timestamp
      kind <- fields .: "event_type" :: Parser T.Text
      let common = ["event_id", "occurred_at", "event_type"]
          scoped constructor = do
            exactFields (common <> ["corpora_id"]) fields
            constructor <$> (fields .: "corpora_id" >>= uuid)
      event <- case kind of
        "app_opened" -> do
          exactFields common fields
          when (runtime == "headless") (fail "Invalid headless event")
          pure AppOpened
        "scheduler_heartbeat" -> exactFields common fields >> pure SchedulerHeartbeat
        "corpora_created" -> scoped CorporaCreated
        "request_accepted" -> scoped RequestAccepted
        "remote_paired" -> scoped RemotePaired
        _ -> fail "Invalid event"
      pure (Row identifier event occurred received (principalInstallation principal) version os runtime notice)
    uuid value = maybe (fail "Invalid UUID") (pure . UUID.toText) (parseUUIDv4 value)
    timestamp = withText "timestamp" $ \value ->
      case parseTimeM False defaultTimeLocale "%Y-%m-%dT%H:%M:%S%QZ" (T.unpack value) of
        Just time | T.length value == 24
          && T.pack (formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S%3QZ" time) == value -> pure time
        _ -> fail "Invalid timestamp"
    allowed names fields = unless (all (`elem` names) (KM.keys fields)) (fail "unknown_field")
