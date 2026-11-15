{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Parquet
  ( Admission (..), Collector
  , admitBatch
  , collectorReady
  , withCollector
  ) where

import Control.Concurrent.Async (withAsync)
import Control.Concurrent.STM
import Control.Exception
  ( Exception, IOException, SomeAsyncException, SomeException, bracket
  , bracketOnError, catch, evaluate, finally, fromException, throwIO
  )
import Control.Monad (unless, when)
import Data.Bits ((.&.))
import qualified Data.ByteString as BS
import Data.Foldable (traverse_)
import Data.Int (Int64)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
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
import System.Posix.Files (fileMode, fileOwner, getFdStatus, getSymbolicLinkStatus, isDirectory, isRegularFile, linkCount, setFileMode)
import System.Posix.IO
  ( LockRequest (WriteLock), OpenMode (ReadWrite), closeFd, defaultFileFlags
  , OpenFileFlags (cloexec, creat, nofollow), openFd, setLock
  )
import System.Posix.User (getEffectiveUserID)
import Telemetry
import TelemetryCredential (installationIdText)

maxRows, rowInputEstimate, maxInputBytes, maxOutputBytes, maxBufferAgeMicros :: Int
maxRows = 4096
rowInputEstimate = 2048
maxInputBytes = 8 * 1024 * 1024
maxOutputBytes = 16 * 1024 * 1024
maxBufferAgeMicros = 60 * 1000000

data Admission = AcceptedVolatile | RejectedCapacity

data Collector = Collector
  { intakeQueue :: !(TBQueue Row)
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
    unless (isRegularFile status && fileOwner status == owner && linkCount status == 1 && fileMode status .&. 0o077 == 0) $
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

admitBatch :: Collector -> NonEmpty Row -> STM Admission
admitBatch collector rows = do
  healthy <- readTVar (publisherHealthy collector)
  if not healthy
    then pure RejectedCapacity
    else orElse enqueue (pure RejectedCapacity)
  where
    enqueue = do
      traverse_ (\row -> seq row (writeTBQueue (intakeQueue collector) row)) rows
      pure AcceptedVolatile

runWriter :: FilePath -> Collector -> IO ()
runWriter root collector =
  catch
    (finally (writerLoop root (intakeQueue collector))
      (atomically (writeTVar (publisherHealthy collector) False))) $ \(err :: SomeException) ->
    case fromException err :: Maybe SomeAsyncException of
      Just _ -> throwIO err
      Nothing -> hPutStrLn stderr "Telemetry publisher unavailable"

writerLoop :: FilePath -> TBQueue Row -> IO ()
writerLoop root queue = loop
  where
    loop = atomically (readTBQueue queue) >>= start
    start row = do
      deadline <- registerDelay maxBufferAgeMicros
      collect (row :| []) 1 deadline
    seal rows = publishRows root (NE.reverse rows)
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

publishRows :: FilePath -> NonEmpty Row -> IO ()
publishRows root rows = do
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
  let directory = root </> "schema=d4-v2" </> "events"
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
      hClose handle
      renameFile temporary final
  where
    options = P.defaultWriteOptions { P.writeCompression = P.Uncompressed }

createPrivateDirectory :: FilePath -> IO ()
createPrivateDirectory path = do
  exists <- doesDirectoryExist path
  unless exists $ do
    createDirectoryIfMissing True path
    setFileMode path 0o700
  status <- getSymbolicLinkStatus path
  owner <- getEffectiveUserID
  unless (isDirectory status && fileOwner status == owner && fileMode status .&. 0o077 == 0)
    (throwIO WriterNotWritable)

cleanupTemp :: (FilePath, Handle) -> IO ()
cleanupTemp (path, handle) = do
  catch (hClose handle) (\(_ :: IOException) -> pure ())
  catch (removeFile path) (\(_ :: IOException) -> pure ())

schema :: V.Vector P.SchemaElement
schema = V.fromList
  [ P.SchemaElement "schema" Nothing Nothing (Just 10) Nothing Nothing Nothing Nothing
  , text "event_id" P.Required
  , text "event_type" P.Required
  , timestamp "received_at"
  , timestamp "occurred_at"
  , text "installation_id" P.Required
  , text "corpora_id" P.Optional
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

columns :: NonEmpty Row -> V.Vector P.ParquetColumn
columns rows = V.fromList
  [ text (eventIdText . rowEventId)
  , text (eventType . rowEvent)
  , int64 (timestampMillis . rowReceivedAt)
  , int64 (timestampMillis . rowOccurredAt)
  , text (installationIdText . rowInstallation)
  , optional (fmap corporaIdText . eventCorpora . rowEvent)
  , text (appVersionText . rowAppVersion)
  , text (osText . rowOS)
  , text (runtimeText . rowRuntime)
  , int32 (fromIntegral . noticeVersionNumber . rowNotice)
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
