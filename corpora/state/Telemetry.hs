{-# LANGUAGE OverloadedStrings #-}

module Telemetry
  ( CorporaRef, corporaRef, corporaDirectory, Emit, EventIntent (..)
  , Event (..), Record (..), newRecord, encodeBatch
  ) where

import Data.Aeson ( encode, object, (.=))
import Data.Aeson.Types (Pair)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (UTCTime, defaultTimeLocale, formatTime)
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import System.FilePath (isAbsolute)

newtype CorporaRef = CorporaRef FilePath
corporaRef :: FilePath -> Maybe CorporaRef
corporaRef path
  | isAbsolute path && BS.length (TE.encodeUtf8 (T.pack path)) <= 4096 = Just (CorporaRef path)
  | otherwise = Nothing
corporaDirectory :: CorporaRef -> FilePath
corporaDirectory (CorporaRef path) = path

data EventIntent = IAppOpened | ICorporaCreated CorporaRef
  | IRequestAccepted CorporaRef | IRemotePaired CorporaRef | ISchedulerHeartbeat
type Emit = EventIntent -> IO ()
data Event = AppOpened | CorporaCreated T.Text | RequestAccepted T.Text
  | RemotePaired T.Text | SchedulerHeartbeat
data Record = Record T.Text UTCTime Event

newRecord :: UTCTime -> Event -> IO Record
newRecord occurred event = Record <$> (UUID.toText <$> UUID.nextRandom) <*> pure occurred <*> pure event

encodeBatch :: T.Text -> T.Text -> T.Text -> Int -> [Record] -> BS.ByteString
encodeBatch version os runtime notice records = BL.toStrict $ encode $ object
  [ "app_version" .= version, "os" .= os, "runtime_kind" .= runtime
  , "notice_version" .= notice, "events" .= map recordJSON records
  ]
  where
    recordJSON (Record identifier occurred event) = object $
      [ "event_id" .= identifier
      , "occurred_at" .= formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S%3QZ" occurred
      ] <> case event of
        AppOpened -> ["event_type" .= ("app_opened" :: T.Text)]
        SchedulerHeartbeat -> ["event_type" .= ("scheduler_heartbeat" :: T.Text)]
        CorporaCreated corpora -> scoped "corpora_created" corpora
        RequestAccepted corpora -> scoped "request_accepted" corpora
        RemotePaired corpora -> scoped "remote_paired" corpora
    scoped :: T.Text -> T.Text -> [Pair]
    scoped kind corpora = ["event_type" .= kind, "corpora_id" .= corpora]
