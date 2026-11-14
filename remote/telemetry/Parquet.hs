{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Parquet
  ( Collector
  , admitBatch
  , collectorReady
  , withCollector
  ) where

import Auth (installationIdText)
import Control.Concurrent.Async (withAsync)
import Control.Concurrent.STM
import Control.Exception
  ( Exception, IOException, SomeAsyncException, SomeException, bracket
  , bracketOnError, catch, evaluate, finally, fromException, throwIO
  )
import Control.Monad (unless, when)
import qualified Data.ByteString as BS
import Data.Foldable (traverse_)
import Data.Int (Int64)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (UTCTime, defaultTimeLocale, formatTime, getCurrentTime, utctDay)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import qualified Data.Vector as V
import qualified Data.Vector.Primitive as VP
import qualified Parquet.HighLevel as P
import qualified Parquet.Types as P
import Protocol
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesPathExist, removeFile, renameFile)
import System.FilePath ((</>))
import System.IO (Handle, SeekMode (AbsoluteSeek), hClose, hPutStrLn, openBinaryTempFile, stderr)
import System.Posix.Files (fileOwner, getFdStatus, isRegularFile, setFileMode)
import System.Posix.IO
  ( LockRequest (WriteLock), OpenMode (ReadWrite), closeFd, defaultFileFlags
  , OpenFileFlags (cloexec, creat, nofollow), openFd, setLock
  )
import System.Posix.User (getEffectiveUserID)

maxRows, rowInputEstimate, maxInputBytes, maxOutputBytes, maxBufferAgeMicros :: Int
maxRows = 4096
rowInputEstimate = 2048
maxInputBytes = 8 * 1024 * 1024
maxOutputBytes = 16 * 1024 * 1024
maxBufferAgeMicros = 60 * 1000000

data Collector = Collector
  { intakeQueue :: !(TBQueue ParquetRow)
  , publisherHealthy :: !(TVar Bool)
  }

data WriterError = WriterEncodeFailed | WriterBoundsExceeded | WriterNotWritable
  deriving (Show)

instance Exception WriterError

withCollector :: FilePath -> (Collector -> IO a) -> IO a
withCollector root action = do
  createPrivateDirectory root
  let flags = defaultFileFlags
        { creat = Just 0o600, nofollow = True, cloexec = True }
  bracket (openFd (root </> ".writer.lock") ReadWrite flags) closeFd $ \fd -> do
    status <- getFdStatus fd
    owner <- getEffectiveUserID
    unless (isRegularFile status && fileOwner status == owner) $
      throwIO WriterNotWritable
    setLock fd (WriteLock, AbsoluteSeek, 0, 0)
    bracketOnError (openBinaryTempFile root ".probe.tmp") cleanupTemp $ \(path, handle) -> do
      hClose handle
      removeFile path
    collector <- Collector <$> newTBQueueIO (fromIntegral maxRows) <*> newTVarIO True
    finally
      (withAsync (runWriter root collector) $ \_ -> action collector)
      (atomically (writeTVar (publisherHealthy collector) False))

collectorReady :: Collector -> STM Bool
collectorReady collector = do
  healthy <- readTVar (publisherHealthy collector)
  full <- isFullTBQueue (intakeQueue collector)
  pure (healthy && not full)

admitBatch :: Collector -> ValidatedIngest -> STM Admission
admitBatch collector ingest = do
  healthy <- readTVar (publisherHealthy collector)
  if not healthy
    then pure RejectedCapacity
    else orElse enqueue (pure RejectedCapacity)
  where
    rows = rowsForBatch ingest
    enqueue = do
      traverse_ (\row -> seq row (writeTBQueue (intakeQueue collector) row)) rows
      pure (AcceptedVolatile (NE.length rows))

runWriter :: FilePath -> Collector -> IO ()
runWriter root collector =
  runWriterWith (publishRows root) collector maxBufferAgeMicros

runWriterWith :: (NonEmpty ParquetRow -> IO ()) -> Collector -> Int -> IO ()
runWriterWith publish collector maxAge =
  catch
    (finally
      (writerLoopWith publish (intakeQueue collector) maxAge)
      (atomically (writeTVar (publisherHealthy collector) False))) $ \(err :: SomeException) ->
    case fromException err :: Maybe SomeAsyncException of
      Just _ -> throwIO err
      Nothing -> hPutStrLn stderr "Telemetry publisher unavailable"

writerLoopWith :: (NonEmpty ParquetRow -> IO ()) -> TBQueue ParquetRow -> Int -> IO ()
writerLoopWith publish queue maxAge = loop
  where
    loop = atomically (readTBQueue queue) >>= start
    start row = do
      deadline <- registerDelay maxAge
      collect (row :| []) 1 deadline
    seal rows = publish (NE.reverse rows)
    collect rows !count deadline
      | count >= maxRows || count * rowInputEstimate >= maxInputBytes =
          seal rows >> loop
      | otherwise = do
          next <- atomically $
            orElse
              (readTVar deadline >>= check >> pure Nothing)
              (Just <$> readTBQueue queue)
          case next of
            Nothing -> seal rows >> loop
            Just row
              | utctDay (rowReceivedAt row) /= utctDay (rowReceivedAt (NE.head rows)) ->
                  seal rows >> start row
              | otherwise ->
                  collect (row NE.<| rows) (count + 1) deadline

publishRows :: FilePath -> NonEmpty ParquetRow -> IO ()
publishRows = publishRowsWith hClose renameFile

publishRowsWith
  :: (Handle -> IO ())
  -> (FilePath -> FilePath -> IO ())
  -> FilePath
  -> NonEmpty ParquetRow
  -> IO ()
publishRowsWith closePart movePart root rows = do
  let count = NE.length rows
  when (count > maxRows || count * rowInputEstimate > maxInputBytes) $
    throwIO WriterBoundsExceeded
  result <- P.encodeParquetMixedIO options schema [columns rows]
  bytes <- either (const (throwIO WriterEncodeFailed)) pure result
  size <- evaluate (BS.length bytes)
  when (size > maxOutputBytes) $ throwIO WriterBoundsExceeded
  let day = utctDay (rowReceivedAt (NE.head rows))
  unless (all ((== day) . utctDay . rowReceivedAt) rows) $
    throwIO WriterBoundsExceeded
  let directory = root </> "schema=v1" </> "events"
        </> ("received_date=" ++ show day)
  createPrivateDirectory directory
  identifier <- UUID.nextRandom
  now <- getCurrentTime
  let final = directory </>
        ("part-" ++ formatTime defaultTimeLocale "%Y%m%dT%H%M%S%3QZ" now
          ++ "-" ++ UUID.toString identifier ++ ".parquet")
  exists <- doesPathExist final
  when exists $ throwIO WriterNotWritable
  bracketOnError
    (openBinaryTempFile directory (".part-" ++ UUID.toString identifier ++ ".tmp"))
    cleanupTemp $ \(temporary, handle) -> do
      setFileMode temporary 0o600
      BS.hPut handle bytes
      closePart handle
      movePart temporary final
  where
    options = P.defaultWriteOptions { P.writeCompression = P.Uncompressed }

createPrivateDirectory :: FilePath -> IO ()
createPrivateDirectory path = do
  exists <- doesDirectoryExist path
  unless exists $ do
    createDirectoryIfMissing True path
    setFileMode path 0o700

cleanupTemp :: (FilePath, Handle) -> IO ()
cleanupTemp (path, handle) = do
  catch (hClose handle) (\(_ :: IOException) -> pure ())
  catch (removeFile path) (\(_ :: IOException) -> pure ())

schema :: V.Vector P.SchemaElement
schema = V.fromList
  [ P.SchemaElement "schema" Nothing Nothing (Just 14) Nothing Nothing Nothing Nothing
  , leaf "schema_version" P.Required P.PTInt32 Nothing Nothing
  , text "event_id" P.Required
  , text "event_type" P.Required
  , timestamp "received_at"
  , timestamp "occurred_at"
  , text "installation_id" P.Required
  , text "device_id" P.Required
  , text "corpora_id" P.Optional
  , text "request_kind" P.Optional
  , text "app_id" P.Required
  , text "app_version" P.Required
  , text "os" P.Required
  , text "runtime_kind" P.Required
  , leaf "notice_version" P.Required P.PTInt32 Nothing Nothing
  ]
  where
    leaf name repetition physical converted logical =
      P.SchemaElement name (Just repetition) (Just physical) Nothing
        converted logical Nothing Nothing
    text name repetition =
      leaf name repetition P.PTByteArray (Just P.CTUtf8) (Just P.LTString)
    timestamp name =
      leaf name P.Required P.PTInt64 (Just P.CTTimestampMillis)
        (Just (P.LTTimestamp True P.LtMillis))

columns :: NonEmpty ParquetRow -> V.Vector P.ParquetColumn
columns rows = V.fromList
  [ int32 rowSchemaVersion
  , text (eventIdText . rowEventId)
  , text (eventTypeText . rowEventType)
  , int64 (timestampMillis . rowReceivedAt)
  , int64 (timestampMillis . rowOccurredAt)
  , text (T.pack . installationIdText . rowInstallation)
  , text (deviceIdText . rowDevice)
  , optional (fmap publicCorporaIdText . rowCorpora)
  , optional (fmap requestKindText . rowRequestKind)
  , text (applicationIdText . rowAppId)
  , text (appVersionText . rowAppVersion)
  , text (osText . rowOS)
  , text (runtimeText . rowRuntime)
  , int32 (noticeVersionNumber . rowNotice)
  ]
  where
    values = NE.toList rows
    int32 get = P.PCRequired (P.ColInt32 (VP.fromList (map get values)))
    int64 get = P.PCRequired (P.ColInt64 (VP.fromList (map get values)))
    text get = P.PCRequired (P.ColByteArray (V.fromList (map (TE.encodeUtf8 . get) values)))
    optional get =
      P.PCOptional (P.OptByteArray (V.fromList (map (fmap TE.encodeUtf8 . get) values)))

timestampMillis :: UTCTime -> Int64
timestampMillis = floor . (* 1000) . utcTimeToPOSIXSeconds

applicationIdText :: ApplicationId -> T.Text
applicationIdText Sapiens4 = "sapiens4"

eventTypeText :: EventType -> T.Text
eventTypeText EAppOpened = "app_opened"
eventTypeText ECorporaCreated = "corpora_created"
eventTypeText ERequestAccepted = "request_accepted"
eventTypeText ERemotePaired = "remote_paired"

requestKindText :: RequestKind -> T.Text
requestKindText AgentMessage = "agent_message"
requestKindText GroupMessage = "group_message"
requestKindText GroupTaskRun = "group_task_run"

osText :: OsFamily -> T.Text
osText MacOS = "macos"
osText Windows = "windows"
osText Linux = "linux"

runtimeText :: RuntimeKind -> T.Text
runtimeText NativeDesktop = "native_desktop"
runtimeText Headless = "headless"
