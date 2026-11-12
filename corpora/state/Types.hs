{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_GHC -Werror=incomplete-patterns #-}
module Types (module Types, module Domain) where

import Domain

import Control.Concurrent
import Control.Concurrent.STM (TVar, atomically, modifyTVar')
import Control.Concurrent.Async
import Control.Exception
import Control.Monad
import Data.Aeson hiding (pairs)
import Data.Aeson.Types (Parser, parseEither)
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as BS
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
import Database.SQLite.Simple (Connection)
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
data Service = Service { serviceRoot :: FilePath, repositoryRoot :: FilePath, serviceGuard :: MVar (), serviceFileLock :: FileLock, serviceStopping :: IORef Bool, serviceWorker :: MVar (Maybe (Async ())), serviceRuns :: MVar (Map RunId Run), serviceLease :: IORef (Maybe (AgentId, RunId)), serviceEndpoint :: IORef Text, servicePulse :: IORef Value, serviceActors :: MVar (Map ThreadId (Text,Maybe Text)), serviceDatabase :: MVar (Maybe Connection), serviceStates :: MVar (Map Text CachedAgentState)
 , serviceRevision :: TVar Integer, updateSubscribers :: TVar Int, snapshotFiles :: IORef (Set FilePath) }

notifyChange :: Service -> IO ()
notifyChange s = atomically $ modifyTVar' (serviceRevision s) (+1)

-- Committed projections retain only turns that still depend on archive output.
data ProjectionStatus = ProjectionPending | ProjectionCommitted [Turn]
data CachedAgentState = CachedAgentState
 { cachedBytes :: BS.ByteString, cachedState :: AgentState, cachedProjection :: ProjectionStatus }

migrateAgentState :: Int -> Value -> Validation AgentState
migrateAgentState version = validateValue 500 $ withObject "legacy agent state" $ \o -> do
 unless (version `elem` [1,2]) $ fail "Unsupported legacy state version"
 savedVersion <- o .: "schema_version"
 unless (savedVersion == version) $ fail "Mismatched legacy state version"
 agent <- o .: "agid" :: Parser Text
 revision <- o .: "revision" :: Parser Int
 sequenceNumber <- o .: "event_sequence" :: Parser Int
 lastOutput <- o .: "last_output" :: Parser (Maybe Text)
 history <- o .: (if version == 1 then "jobs" else "turns") >>= mapM migrateTurn
 chat <- o .: "chat" >>= mapM (message $ \row -> do
  void (row .: "role" :: Parser Text)
  void (row .: "content" :: Parser Text))
 events <- o .: "events" >>= mapM (message $ \row -> do
  void (row .: "sequence" :: Parser Int)
  void (row .: "kind" :: Parser Text))
 pure $ AgentState (catMaybes history) (KM.fromList
  [("schema_version",num 3),("agid",str agent),("revision",num revision),("event_sequence",num sequenceNumber)
  ,("last_output",toJSON lastOutput),("chat",arr chat),("events",arr events)])
 where
 message validate = withObject "legacy message" $ \row -> do
  validate row
  void (row .:? "time" :: Parser (Maybe Text))
  void (row .:? "turn" :: Parser (Maybe Text))
  job <- traverse parseJSON (KM.lookup "job" row) :: Parser (Maybe Text)
  pure $ maybe (Object row) (\ident -> Object $ KM.insert "turn" (str ident) $ KM.delete "job" row) job
 migrateTurn = withObject "legacy turn" $ \row -> do
  ident <- row .: "id" :: Parser RunId
  input <- row .: (if version==1 then "task" else "input") :: Parser Text
  flow <- row .: "flow"
  status <- row .: "status" :: Parser Text
  let extra = selectPresent ["created","error","warning"] (Object row)
      details = if status=="budget_blocked" then put "error" (str "Former budget block removed; retry to continue") extra else extra
  if version==1 && not (T.null flow) && flow `notElem` ["chat","computer"]
   then do -- v1 also stored jobs from configurable, retired flows.
    unless (status `elem` ["budget_blocked","conflict"]) $ void (parseJSON (str status) :: Parser TurnStatus)
    pure Nothing
   else do
    newStatus <- if status=="budget_blocked" then pure Interrupted else parseJSON (str status)
    Just <$> parseJSON (merge details (obj [("id",toJSON ident),("input",str input),("flow",str flow),("status",toJSON newStatus)]))

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
readJSON p def = do
 exists <- doesFileExist p
 if exists then either (throwIO . userError) pure . eitherDecode =<< BL.readFile p else pure def
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
writeJSON p = atomicBytes p . encode
locked :: Service -> IO a -> IO a
locked s = withMVar (serviceGuard s) . const
agentRoot :: Service -> Text -> FilePath
agentRoot s a = serviceRoot s </> "agentpy/agents" </> T.unpack a
workspaceRoot :: Service -> Text -> FilePath
workspaceRoot s a = serviceRoot s </> "workspaces" </> T.unpack a
agentState :: Service -> Text -> IO AgentState
agentState s a = cachedState <$> agentStateInput s a

agentStateInput :: Service -> Text -> IO CachedAgentState
agentStateInput s a = do
 let path = agentRoot s a </> "state.json"
 bytes <- BS.readFile path
 modifyMVar (serviceStates s) $ \cache -> case M.lookup a cache of
  Just entry | cachedBytes entry == bytes -> pure (cache,entry)
  _ -> do
   state <- either (throwIO . userError) pure (eitherDecodeStrict' bytes)
   let remaining = M.delete a cache
       -- Large states still validate/project normally, without retaining their contents.
       bounded entries
        | M.size entries < 32 && sum (map (BS.length . cachedBytes) (M.elems entries)) + BS.length bytes <= 16*1024*1024 = entries
        | otherwise = bounded (M.deleteMin entries)
       entry = CachedAgentState bytes state ProjectionPending
       updated = if BS.length bytes > 4*1024*1024 then remaining else M.insert a entry (bounded remaining)
   pure (updated,entry)
saveState :: Service -> Text -> AgentState -> IO ()
saveState s a v = writeJSON (agentRoot s a </> "state.json") (put "revision" (num (int (get (stateJSON v) "revision") + 1)) (stateJSON v))
modifyState :: Service -> Text -> (Value -> Value) -> IO ()
modifyState s a f = agentState s a >>= decodeValue 500 . f . stateJSON >>= saveState s a
modifyTurns :: Service -> Text -> (Turn -> Turn) -> IO ()
modifyTurns s a f = agentState s a >>= saveState s a . mapTurns f
archivePath :: Service -> Text -> Text -> FilePath
archivePath s a k = serviceRoot s </> "agentpy/corpora/archive" </> T.unpack a </> T.unpack k <.> "json"
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
