{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Telemetry
  ( CorporaRef, corporaRef, corporaDirectory, Emit, EventIntent (..)
  , Event (..), Record (..), EventId, parseEventId, eventIdText
  , CorporaId, parseCorporaId, newCorporaId, corporaIdText
  , AppVersion, mkAppVersion, appVersionText
  , OsFamily (..), osText, RuntimeKind (..), runtimeText
  , NoticeVersion, mkNoticeVersion, noticeVersionNumber
  , Context (..), newRecord, encodeBatch, eventType, eventCorpora
  ) where

import Control.Monad (guard)
import Data.Aeson (encode, object, (.=))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Char (isAlphaNum, isAscii)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (UTCTime, defaultTimeLocale, formatTime)
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import System.FilePath (isAbsolute)
import TelemetryCredential (parseUUIDv4)

newtype CorporaRef = CorporaRef FilePath
corporaRef :: FilePath -> Maybe CorporaRef
corporaRef path
  | isAbsolute path && BS.length (TE.encodeUtf8 (T.pack path)) <= 4096 = Just (CorporaRef path)
  | otherwise = Nothing
corporaDirectory :: CorporaRef -> FilePath
corporaDirectory (CorporaRef path) = path

newtype EventId = EventId UUID deriving (Eq, Show)
newtype CorporaId = CorporaId UUID deriving (Eq, Show)
parseEventId :: T.Text -> Maybe EventId
parseEventId = fmap EventId . parseUUIDv4
parseCorporaId :: T.Text -> Maybe CorporaId
parseCorporaId = fmap CorporaId . parseUUIDv4
newCorporaId :: IO CorporaId
newCorporaId = CorporaId <$> UUID.nextRandom
eventIdText :: EventId -> T.Text
eventIdText (EventId uuid) = UUID.toText uuid
corporaIdText :: CorporaId -> T.Text
corporaIdText (CorporaId uuid) = UUID.toText uuid

newtype AppVersion = AppVersion T.Text deriving (Eq, Show)
mkAppVersion :: T.Text -> Maybe AppVersion
mkAppVersion value = do
  guard (not (T.null value) && T.length value <= 40
    && T.all (\c -> isAscii c && (isAlphaNum c || c `elem` ("._+-" :: String))) value)
  pure (AppVersion value)
appVersionText :: AppVersion -> T.Text
appVersionText (AppVersion value) = value

data OsFamily = MacOS | Windows | Linux deriving (Eq, Show)
osText :: OsFamily -> T.Text
osText MacOS = "macos"
osText Windows = "windows"
osText Linux = "linux"
data RuntimeKind = NativeDesktop | Headless deriving (Eq, Show)
runtimeText :: RuntimeKind -> T.Text
runtimeText NativeDesktop = "native_desktop"
runtimeText Headless = "headless"
newtype NoticeVersion = NoticeVersion Int deriving (Eq, Show)
mkNoticeVersion :: Int -> Maybe NoticeVersion
mkNoticeVersion 1 = Just (NoticeVersion 1)
mkNoticeVersion _ = Nothing
noticeVersionNumber :: NoticeVersion -> Int
noticeVersionNumber (NoticeVersion number) = number

data Context = Context AppVersion OsFamily RuntimeKind NoticeVersion

data EventIntent = IAppOpened | ICorporaCreated CorporaRef
  | IRequestAccepted CorporaRef | IRemotePaired CorporaRef | ISchedulerHeartbeat
type Emit = EventIntent -> IO ()
data Event = AppOpened | CorporaCreated CorporaId | RequestAccepted CorporaId
  | RemotePaired CorporaId | SchedulerHeartbeat
  deriving (Eq, Show)
data Record = Record EventId UTCTime Event

newRecord :: UTCTime -> Event -> IO Record
newRecord occurred event = Record . EventId <$> UUID.nextRandom <*> pure occurred <*> pure event

eventType :: Event -> T.Text
eventType AppOpened = "app_opened"
eventType (CorporaCreated _) = "corpora_created"
eventType (RequestAccepted _) = "request_accepted"
eventType (RemotePaired _) = "remote_paired"
eventType SchedulerHeartbeat = "scheduler_heartbeat"
eventCorpora :: Event -> Maybe CorporaId
eventCorpora (CorporaCreated identifier) = Just identifier
eventCorpora (RequestAccepted identifier) = Just identifier
eventCorpora (RemotePaired identifier) = Just identifier
eventCorpora _ = Nothing

encodeBatch :: Context -> [Record] -> BS.ByteString
encodeBatch (Context version os runtime notice) records = BL.toStrict $ encode $ object
  [ "app_version" .= appVersionText version, "os" .= osText os, "runtime_kind" .= runtimeText runtime
  , "notice_version" .= noticeVersionNumber notice, "events" .= map recordJSON records
  ]
  where
    recordJSON (Record identifier occurred event) = object $
      [ "event_id" .= eventIdText identifier
      , "occurred_at" .= formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S%3QZ" occurred
      , "event_type" .= eventType event
      ] <> maybe [] (\corpora -> ["corpora_id" .= corporaIdText corpora]) (eventCorpora event)
