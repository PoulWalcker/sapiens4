module Collector where

import Control.Concurrent.MVar
import Control.Exception
import Control.Monad (unless)
import Data.Aeson
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Aeson.Key as K
import Data.Aeson.Types (parseEither)
import Data.Int (Int64)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Data.Time (NominalDiffTime, defaultTimeLocale, formatTime)
import Data.Time.Clock.POSIX (POSIXTime, getPOSIXTime, posixSecondsToUTCTime)
import qualified Data.Vector as V
import Network.HTTP.Types
import Network.Wai
import qualified Parquet.HighLevel as P
import qualified Parquet.Types as P
import qualified Parquet.Write as P
import System.Directory
import System.FilePath
import System.IO
import System.IO.Error (isAlreadyExistsError)
import System.Posix.Files (createLink)
import TelemetryProtocol

data Config = Config
 { bindAddress :: String, listenPort :: Int, outputDirectory :: FilePath
 , flushRows :: Int, flushBytes :: Int, flushColumns :: Int, flushAgeSeconds :: NominalDiffTime
 , blockedInstallations :: S.Set Text, blockedVersions :: S.Set Text }
 deriving Show

defaultFlushRows, defaultFlushBytes, defaultFlushColumns :: Int
defaultFlushRows = 1000
defaultFlushBytes = 4 * 1024 * 1024
defaultFlushColumns = 1024
defaultFlushAge :: NominalDiffTime
defaultFlushAge = 60
instance FromJSON Config where
 parseJSON = withObject "collector config" $ \o -> do
  config <- Config <$> o .:? "bind" .!= "127.0.0.1" <*> o .:? "port" .!= 4181
   <*> o .: "output_directory"
   <*> o .:? "flush_rows" .!= defaultFlushRows <*> o .:? "flush_bytes" .!= defaultFlushBytes
   <*> o .:? "flush_columns" .!= defaultFlushColumns
   <*> (realToFrac <$> (o .:? "flush_age_seconds" .!= (realToFrac defaultFlushAge :: Double)))
   <*> (S.fromList <$> o .:? "blocked_installations" .!= []) <*> (S.fromList <$> o .:? "blocked_versions" .!= [])
  unless (listenPort config>0 && listenPort config<=65535 && flushRows config>=batchLimit && flushBytes config>=requestByteLimit && flushColumns config>=objectFieldLimit && flushAgeSeconds config>0) (fail "Invalid collector limits")
  pure config

type Row = Object
data Buffer = Buffer { bufferedRows :: [Row], bufferedCount :: Int, bufferedBytes :: Int, bufferedColumns :: S.Set K.Key, firstRowAt :: Maybe POSIXTime }
emptyBuffer :: Buffer
emptyBuffer = Buffer [] 0 0 S.empty Nothing
data Collector = Collector { collectorConfig :: Config, collectorBuffer :: MVar Buffer }

newCollector :: Config -> IO Collector
newCollector config = do
 createDirectoryIfMissing True (outputDirectory config </> "logs")
 Collector config <$> newMVar emptyBuffer

normalizeRows :: POSIXTime -> Text -> Object -> [Object] -> [Row]
normalizeRows received ident context = map (\row -> KM.union reserved (KM.union commonContext (KM.union row context)))
 where
 reserved = KM.fromList [("installation_id",String ident),("received_at",toJSON (floor received :: Int64))]
 commonContext = KM.filterWithKey (\key _ -> key `elem` ["app_version","host_os","host_arch","runtime_kind"]) context

cell :: Value -> Maybe B.ByteString
cell Null = Nothing
cell (String value) = Just (TE.encodeUtf8 value)
cell value = Just (BL.toStrict (encode value))

timeCell :: Value -> Maybe Int64
timeCell value = case fromJSON value of
 Success seconds -> Just seconds
 Error _ -> Nothing

encodeRows :: [Row] -> B.ByteString
encodeRows rows = P.encodeParquetMixed P.defaultWriteOptions {P.writeCompression=P.Uncompressed} schema [columns]
 where
 names = S.toAscList (S.unions (map (S.fromList . KM.keys) rows))
 root = P.SchemaElement "schema" Nothing Nothing (Just (fromIntegral (length names))) Nothing Nothing Nothing Nothing
 isTime name = name `elem` ["timestamp","received_at"]
 leaf name = if isTime name
  then P.SchemaElement (K.toText name) (Just P.Optional) (Just P.PTInt64) Nothing Nothing Nothing Nothing Nothing
  else P.SchemaElement (K.toText name) (Just P.Optional) (Just P.PTByteArray) Nothing (Just P.CTUtf8) (Just P.LTString) Nothing Nothing
 schema = V.fromList (root : map leaf names)
 column name = if isTime name
  then P.PCOptional (P.OptInt64 (V.fromList [KM.lookup name row >>= timeCell | row<-rows]))
  else P.PCOptional (P.OptByteArray (V.fromList [KM.lookup name row >>= cell | row<-rows]))
 columns = V.fromList (map column names)

writeRows :: Config -> [Row] -> IO ()
writeRows config rows = do
 name <- formatTime defaultTimeLocale "telemetry-%Y-%m-%d-%H-%s" . posixSecondsToUTCTime <$> getPOSIXTime
 let folder = outputDirectory config </> "logs"
 bracketOnError (openBinaryTempFile folder ".telemetry.tmp") (\(path,handle) -> hClose handle `finally` removeFile path) $ \(path,handle) -> do
  B.hPut handle (encodeRows rows)
  hClose handle
  let publish :: Int -> IO ()
      publish index = do
       let suffix = if index==0 then "" else "-"<>show index
       createLink path (folder </> name<>suffix <.> "parquet") `catch` \(errorValue::IOException) ->
        if isAlreadyExistsError errorValue then publish (index+1) else throwIO errorValue
  publish 0
  removeFile path

due :: Config -> POSIXTime -> Buffer -> Bool
due config now buffer = bufferedCount buffer>=flushRows config || bufferedBytes buffer>=flushBytes config || maybe False (\started -> now-started>=flushAgeSeconds config) (firstRowAt buffer)

flushBuffer :: Config -> Buffer -> IO Bool
flushBuffer config buffer = if bufferedCount buffer==0 then pure True else do
 result <- try (writeRows config (reverse (bufferedRows buffer)))
 case result of
  Left (errorValue::IOException) -> hPutStrLn stderr ("Telemetry Parquet write failed: "<>displayException errorValue) >> pure False
  Right () -> pure True

flushCollector :: Collector -> (Buffer -> Bool) -> IO ()
flushCollector collector shouldFlush = modifyMVar_ (collectorBuffer collector) $ \buffer ->
 if not (shouldFlush buffer) then pure buffer else do
  saved <- flushBuffer (collectorConfig collector) buffer
  pure (if saved then emptyBuffer else buffer)

admit :: Collector -> POSIXTime -> [Row] -> IO Bool
admit collector now rows = modifyMVar (collectorBuffer collector) $ \prior -> do
 let config = collectorConfig collector
     bytes = sum [fromIntegral (BL.length (encode row)) | row<-rows]
     count = length rows
     names = S.unions (map (S.fromList . KM.keys) rows)
     tooLarge = count>flushRows config || bytes>flushBytes config || S.size names>flushColumns config
     overflow = bufferedCount prior+count>flushRows config || bufferedBytes prior+bytes>flushBytes config || S.size (S.union names (bufferedColumns prior))>flushColumns config
     flush = flushBuffer config
 ready <- if tooLarge then pure False else if overflow then flush prior else pure True
 if not ready then pure (prior,False) else do
  let base = if overflow then emptyBuffer else prior
      appended = Buffer (reverse rows ++ bufferedRows base) (bufferedCount base+count) (bufferedBytes base+bytes) (S.union names (bufferedColumns base)) (case firstRowAt base of Nothing -> Just now; value -> value)
  if due config now appended then do
   saved <- flush appended
   pure (if saved then emptyBuffer else appended,True)
  else pure (appended,True)

readBounded :: Request -> IO (Either () BL.ByteString)
readBounded req = loop 0 []
 where
 loop count chunks = do
  chunk <- getRequestBodyChunk req
  let next = count+B.length chunk
  if next>requestByteLimit then pure (Left ()) else if B.null chunk then pure (Right (BL.fromChunks (reverse chunks))) else loop next (chunk:chunks)

collectorApplication :: Collector -> Application
collectorApplication collector req respond = do
 let reply status value = respond (responseLBS status [(hContentType,"application/json")] (encode value))
     errorReply status reason = reply status (object ["error" .= (reason::Text)])
     config = collectorConfig collector
 if requestMethod req/="POST" then errorReply status405 "POST required"
 else if rawPathInfo req/=BC.pack eventsPath then errorReply status404 "Not found"
 else do
  body <- readBounded req
  case body of
   Left () -> errorReply status413 "Request too large"
   Right raw -> case eitherDecode raw >>= parseEither envelope of
    Left _ -> errorReply status400 "Invalid telemetry envelope"
    Right (ident,context,events)
     | S.member ident (blockedInstallations config) -> errorReply status403 "Installation blocked"
     | Just (String version) <- KM.lookup "app_version" context, S.member version (blockedVersions config) -> errorReply status403 "Version blocked"
     | otherwise -> do
      now <- getPOSIXTime
      accepted <- admit collector now (normalizeRows now ident context events)
      if accepted then reply status202 (object ["accepted" .= length events]) else errorReply status503 "Telemetry buffer full"
