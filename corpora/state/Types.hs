{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_GHC -Werror=incomplete-patterns #-}
module Types (module Types, module Domain) where

import Admission
import Domain
import qualified Telemetry

import Control.Concurrent
import Control.Concurrent.STM (TVar, atomically, modifyTVar')
import Control.Concurrent.Async
import Control.Exception
import Control.Monad
import Data.Aeson hiding (pairs)
import Data.Aeson.Types (Parser, parseEither)
import qualified Data.ByteString.Lazy as BL
import Data.IORef
import Data.List (find)
import Data.Map.Strict (Map)
import Data.Set (Set)
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time
import Data.Typeable
import Data.Word (Word64)
import qualified V00_02_03Files as TypedFiles
import qualified AppStorage as A
import qualified Clock
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import System.Directory
import System.FileLock
import System.FilePath
import System.IO
import System.Process
#ifdef mingw32_HOST_OS
import Foreign.C
import System.IO.Error (isPermissionError)
foreign import ccall unsafe "_wopen" syncOpen :: CWString -> CInt -> CInt -> IO CInt
foreign import ccall unsafe "_commit" syncCommit :: CInt -> IO CInt
foreign import ccall unsafe "_close" syncClose :: CInt -> IO CInt
#else
import qualified System.Posix.IO as Posix
import qualified System.Posix.Unistd as Posix
#endif


data APIError = APIError Int Text deriving (Show, Typeable)
instance Exception APIError

data RunFailure = RunFailure Text [Value] deriving (Show,Typeable)
instance Exception RunFailure

newtype ProviderId = ProviderId { providerName :: Text } deriving (Eq, Ord, Show)

data ModelSelection = ModelSelection { selectionProvider :: ProviderId, selectionModel :: Text, selectionEffort :: Text }
 deriving (Eq, Show)
instance FromJSON ModelSelection where
 parseJSON value = do
  onlyFields ["provider","model","effort"] value
  withObject "Model selection" (\o -> do
   provider <- o .: "provider"
   model <- o .: "model"
   effort <- o .: "effort"
   unless (not (T.null provider) && T.length provider <= 80 && not (T.null model) && T.length model <= 200 && T.length effort <= 80) $ fail "Choose a model and effort"
   pure (ModelSelection (ProviderId provider) model effort)) value
instance ToJSON ModelSelection where
 toJSON choice = obj [("provider",str (providerName (selectionProvider choice))),("model",str (selectionModel choice)),("effort",str (selectionEffort choice))]

data Run = Run { runOwner :: AgentId, runId :: RunId, runSlot :: Int, runCancel :: IORef Bool, runProcess :: MVar (Maybe ProcessHandle), runActivity :: IORef Value, runDeadline :: Word64, runModelConfig :: ModelSelection }
data Service = Service { serviceAdmission :: Admission, serviceRoot :: FilePath, repositoryRoot :: FilePath, serviceGuard :: MVar (), serviceFileLock :: FileLock, serviceStopping :: IORef Bool, serviceWorker :: MVar (Maybe (Async ())), serviceRuns :: MVar (Map RunId Run), serviceLease :: IORef (Maybe (AgentId, RunId)), serviceEndpoint :: IORef Text, servicePulse :: IORef Value, serviceActors :: MVar (Map ThreadId (Text,Maybe Text)), serviceStore :: A.Store, serviceClock :: Clock.Clock
 , serviceRevision :: TVar Integer, updateSubscribers :: TVar Int, snapshotFiles :: IORef (Set FilePath)
 , serviceVoicePieces :: IORef (Map (Text,Text) Word64)
 , serviceTelemetry :: IORef (Maybe Telemetry.Telemetry), serviceCorporaId :: Text }

emitTelemetry :: Service -> Telemetry.Event -> Object -> IO ()
emitTelemetry s event fields = do
 telemetry <- readIORef (serviceTelemetry s)
 forM_ telemetry $ \reporter -> Telemetry.emit reporter event (Just (serviceCorporaId s)) fields

notifyChange :: Service -> IO ()
notifyChange s = afterCommit s $ atomically $ modifyTVar' (serviceRevision s) (+1)

-- Pure validation retains the HTTP error contract; adapters choose when to throw.
type Validation a = Either APIError a
check :: Bool -> Int -> Text -> Validation ()
check predicate code message = if predicate then Right () else Left (APIError code message)
validated :: Validation a -> IO a
validated = either throwIO pure
validateValue :: Int -> (Value -> Parser a) -> Value -> Validation a
validateValue code parser = either (Left . APIError code . T.pack) Right . parseEither parser

decodeValue :: FromJSON a => Int -> Value -> IO a
decodeValue code = parseValue code parseJSON
parseValue :: Int -> (Value -> Parser a) -> Value -> IO a
parseValue code parser = validated . validateValue code parser
field :: FromJSON a => Int -> Value -> Text -> IO a
field code value key = decodeValue code (get value key)

turnResult :: Either TurnControlError a -> IO a
turnResult = either (failure 409 . message) pure
 where
 message TurnNotRetryable = "Only failed or interrupted batches can be retried"
 message TurnAlreadyComplete = "Completed input cannot be cancelled"

failure :: Int -> Text -> IO a
failure n = throwIO . APIError n
require :: Bool -> Int -> Text -> IO ()
require p n m = unless p (failure n m)
textField :: Value -> Text -> Int -> IO Text
textField v k n = parseValue 400 (textFieldP k n) v
now :: IO Text
now = timestamp <$> getCurrentTime
timestamp :: UTCTime -> Text
timestamp = T.pack . formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S%Q+00:00"
parseTime :: Text -> Maybe UTCTime
parseTime s = parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%S%Q%Ez" (T.unpack s)
newId :: IO Text
newId = T.filter (/= '-') . UUID.toText <$> UUID.nextRandom
fresh :: Text -> IO Text
fresh prefix = (prefix <>) . T.take 12 <$> newId
readJSON :: FromJSON a => FilePath -> a -> IO a
readJSON = TypedFiles.readValue
atomicBytes :: FilePath -> BL.ByteString -> IO ()
atomicBytes p bytes = do
 createDirectoryIfMissing True (takeDirectory p)
 bracketOnError (openBinaryTempFile (takeDirectory p) ".writing-") (\(f,h) -> do
  hClose h `catch` (\(_::IOException) -> pure ())
  removeFile f `catch` (\(_::IOException) -> pure ())) $ \(f,h) -> do
  BL.hPut h bytes
  hFlush h
#ifdef mingw32_HOST_OS
  hClose h
  withCWString f $ \name -> bracket (syncOpen name 32770 0) (void . syncClose) $ \fd -> do
   require (fd >= 0) 500 "Cannot open durable temporary file"
   code <- syncCommit fd
   require (code == 0) 500 "Cannot synchronize temporary file"
  let replace attempt = renameFile f p `catch` \(e::IOException) ->
       if isPermissionError e && attempt < (100::Int) then threadDelay 10000 >> replace (attempt+1) else throwIO e
  replace 0
#else
  fd <- Posix.handleToFd h
  Posix.fileSynchronise fd `finally` Posix.closeFd fd
  renameFile f p
  bracket (Posix.openFd (takeDirectory p) Posix.ReadOnly Posix.defaultFileFlags) Posix.closeFd Posix.fileSynchronise
#endif
writeJSON :: ToJSON a => FilePath -> a -> IO ()
writeJSON = TypedFiles.writeValue
locked :: Service -> IO a -> IO a
locked s action = withMVar (serviceGuard s) (const (A.transaction (serviceStore s) action))
agentRoot :: Service -> Text -> FilePath
agentRoot s a = serviceRoot s </> "agentpy/agents" </> T.unpack a
workspaceRoot :: Service -> Text -> FilePath
workspaceRoot s a = serviceRoot s </> "workspaces" </> T.unpack a
agentState :: Service -> Text -> IO AgentState
agentState s a = loadValue s ("states/"<>a) Null >>= decodeValue 500
saveState :: Service -> Text -> AgentState -> IO ()
saveState s a v = do
 old <- loadValue s ("states/"<>a) Null
 let next = stateJSON v
 when (remove ["revision"] old /= remove ["revision"] next) $
  saveValue s ("states/"<>a) (put "revision" (num (int (get old "revision")+1)) next)
loadValue :: FromJSON a => Service -> Text -> a -> IO a
loadValue s key def = A.getValue (serviceStore s) key >>= maybe (pure def) (decodeValue 500)
saveValue :: ToJSON a => Service -> Text -> a -> IO ()
saveValue s key = A.putValue (serviceStore s) key . toJSON
readArchive :: Service -> Text -> Text -> IO Value
readArchive s a key = loadValue s ("audit/"<>a<>"/"<>key) empty
writeArchive :: Service -> Text -> Text -> Value -> IO ()
writeArchive s a key = saveValue s ("audit/"<>a<>"/"<>key)
afterCommit :: Service -> IO () -> IO ()
afterCommit = A.afterCommit . serviceStore
modifyState :: Service -> Text -> (Value -> Value) -> IO ()
modifyState s a f = agentState s a >>= decodeValue 500 . f . stateJSON >>= saveState s a
modifyTurns :: Service -> Text -> (Turn -> Turn) -> IO ()
modifyTurns s a f = agentState s a >>= saveState s a . mapTurns f
findId :: Text -> [Value] -> Maybe Value
findId i = find ((== i) . (`at` "id"))
readText :: FilePath -> IO Text
readText = fmap (TE.decodeUtf8 . BL.toStrict) . BL.readFile
writeText :: FilePath -> Text -> IO ()
writeText p = atomicBytes p . BL.fromStrict . TE.encodeUtf8
prompt :: Service -> Text -> IO Text
prompt s k = readText (repositoryRoot s </> "corpora/prompts" </> T.unpack k <.> "md")
render :: Text -> [(Text,Text)] -> Text
render = foldl (\t (k,v) -> T.replace ("{" <> k <> "}") v t)

withActor :: Service -> Text -> Maybe Text -> IO a -> IO a
withActor s actor agency action = do
 thread <- myThreadId
 bracket (modifyMVar (serviceActors s) (\actors -> pure (M.insert thread (actor,agency) actors,M.lookup thread actors)))
  (\previous -> modifyMVar_ (serviceActors s) (pure . maybe (M.delete thread) (M.insert thread) previous)) (const action)
currentActor :: Service -> IO (Text,Maybe Text)
currentActor s = do
 thread <- myThreadId
 fromMaybe ("admin",Nothing) . M.lookup thread <$> readMVar (serviceActors s)

admitted :: Service -> Bool -> IO a -> IO a
admitted s write action = withAdmission (serviceAdmission s) write action >>= maybe (failure 503 "Application update in progress; work admission is closed") pure
