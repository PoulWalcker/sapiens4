{-# LANGUAGE CPP #-}
module Registry where

import Types
import qualified Service as Service
import Bridge
import Client
import Crypto (confirmationCode)
import Setup
import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.Aeson hiding (pairs)
import qualified Data.ByteString.Lazy as BL
import Data.IORef
import Data.List (find, nub)
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import System.Directory
import System.FilePath
import System.Info
#ifndef mingw32_HOST_OS
import System.Posix.Files (setFileMode)
#endif

data Registry = Registry
 { registryService :: Service, registryBridge :: Bridge
 , registryOpenChild :: FilePath -> IO (Service, Bridge, IO ())
 , registryGuard :: MVar (), registrySaved :: IORef Value
 , registryLocals :: IORef (M.Map Text (Service, Bridge, IO ()))
 , registryClients :: IORef (M.Map Text Client)
 , registryPending :: IORef (M.Map Text Pairing)
 , registryPairClients :: IORef (M.Map Text Client)
 , registryClosed :: IORef Bool
 }

registryPath :: Registry -> FilePath
registryPath registry = serviceRoot (registryService registry) </> "host-corpora.json"
localAvailable :: Bool
localAvailable = os `elem` ["darwin", "mingw32"]
registryLock :: Registry -> IO a -> IO a
registryLock registry = withMVar (registryGuard registry) . const
saveRegistry :: Registry -> IO ()
saveRegistry registry = readIORef (registrySaved registry) >>= writeJSON (registryPath registry)

newRegistry :: Service -> Bridge -> (FilePath -> IO (Service, Bridge, IO ())) -> IO Registry
newRegistry service bridge openChild = do
 saved <- readJSON (serviceRoot service </> "host-corpora.json") (obj [("locals",arr []),("remotes",empty)])
 identity <- Service.corporaIdentity service
 registry <- Registry service bridge openChild <$> newMVar () <*> newIORef saved
   <*> newIORef (M.singleton (at identity "id") (service,bridge,pure ())) <*> newIORef M.empty
   <*> newIORef M.empty <*> newIORef M.empty <*> newIORef False
 unless (null $ items saved "relay_cleanup") $ void $ forkIO $ registryLock registry $ cleanupRelays registry Nothing Nothing
 pure registry

local :: Registry -> Text -> IO (Service, Bridge, IO ())
local registry identity = do
 opened <- readIORef (registryLocals registry)
 case M.lookup identity opened of
  Just value -> pure value
  Nothing -> do
   saved <- readIORef (registrySaved registry)
   require (str identity `elem` items saved "locals") 404 "Unknown corpora"
   let folder = serviceRoot (registryService registry) </> "corporas" </> T.unpack identity
   exists <- doesFileExist (folder </> "corpora-identity.json")
   require exists 409 "Corpora files are unavailable. Restore them before opening."
   child@(_,bridge,_) <- registryOpenChild registry folder
   ownRelay <- savedRelay bridge
   mainRelay <- savedRelay (registryBridge registry)
   when (ownRelay == empty && mainRelay /= empty) $ writeJSON (relayPath bridge) mainRelay
   modifyIORef' (registryLocals registry) (M.insert identity child)
   pure child

registryRecords :: Registry -> IO Value
registryRecords registry = registryLock registry $ do
 saved <- readIORef (registrySaved registry)
 mainIdentity <- Service.corporaIdentity (registryService registry)
 localRecords <- if not localAvailable then pure [] else forM (get mainIdentity "id" : items saved "locals") $ \identity -> do
  (service, bridge, _) <- local registry (text identity)
  metadata <- Service.corporaIdentity service
  status <- bridgeStatus bridge
  pure $ merge metadata $ obj [("location",str "Local"),("paired",get status "paired")]
 let remoteRecords = [put "location" (str "Remote") (get config "corpora") | (_,config) <- pairs (get saved "remotes")]
 pure $ obj [("local_available",Bool localAvailable),("corpora",arr (localRecords ++ remoteRecords))]

data RouteTarget = LocalTarget Service Bridge | RemoteTarget Client

registryRoute :: Registry -> Text -> Text -> Text -> Maybe Value -> IO (Int, BL.ByteString, Text)
registryRoute registry identity method path input = do
 target <- registryLock registry $ do
  saved <- readIORef (registrySaved registry)
  if has (get saved "remotes") identity then do
   cache <- readIORef (registryClients registry)
   client <- case M.lookup identity cache of
    Just value -> pure value
    Nothing -> do
     value <- newClient (get (get saved "remotes") identity)
     modifyIORef' (registryClients registry) (M.insert identity value)
     pure value
   pure (RemoteTarget client)
  else do
   require localAvailable 404 "Local corpora are unavailable on this device"
   (service,bridge,_) <- local registry identity
   pure (LocalTarget service bridge)
 require ("/api/" `T.isPrefixOf` path && not ("/api/host" `T.isPrefixOf` path) && not (T.any (=='%') $ T.takeWhile (/='?') path) && not (".." `T.isInfixOf` path) && not (T.any (=='\\') path)) 400 "Invalid corpora API path"
 if "/api/remote" `T.isPrefixOf` path then case target of
  RemoteTarget _ -> failure 403 "Pairing approval is available on the host only"
  LocalTarget _ bridge -> do
   value <- if path == "/api/remote" && method == "GET" then bridgeStatus bridge
    else if method == "POST" then remoteAction registry bridge path (fromMaybe empty input)
    else failure 404 "Not found"
   pure (200,encode value,"application/json")
 else case target of
  LocalTarget service _ -> Service.dispatch service method path input
  RemoteTarget client -> do
   result@(status,body,_) <- clientRequest client method path input
   registryLock registry $ do
    saved <- readIORef (registrySaved registry)
    clients <- readIORef (registryClients registry)
    require (has (get saved "remotes") identity && maybe False ((== clientConfig client) . clientConfig) (M.lookup identity clients)) 410 "Remote connection was revoked"
    let payload = fromMaybe Null (decode body)
        metadata = if path == "/api/state" then get payload "corpora" else if path `elem` ["/api/corpora","/api/corpora/lifecycle"] then payload else Null
    when (status == 200 && at metadata "id" == identity && get (get (get saved "remotes") identity) "corpora" /= metadata) $ do
     let updated = put "corpora" metadata (get (get saved "remotes") identity)
     writeIORef (registrySaved registry) $ put "remotes" (put identity updated $ get saved "remotes") saved
     saveRegistry registry
   pure result

remoteAction :: Registry -> Bridge -> Text -> Value -> IO Value
remoteAction registry bridge path input = registryLock registry $ do
 saved <- readIORef (registrySaved registry)
 require (not $ boolean $ get (get saved "relay_servers") "reset_pending") 409 "Relay switch interrupted. Restart the app to finish."
 case path of
  "/api/remote/pair" -> if has saved "relay_servers" then do
    connection <- selectedConnection registry
    require (boolean (get input "saved") && not (boolean $ get input "local")) 409 "Choose the relay in Remote Control Relay Servers."
    identity <- Service.corporaIdentity (bridgeService bridge)
    cleanupRelays registry (Just $ at connection "relay") (Just $ at identity "id")
    createPairing bridge connection
   else createPairing bridge input
  "/api/remote/approve" -> approvePairing bridge input
  "/api/remote/revoke" -> revokeBridge bridge True
  _ -> failure 404 "Not found"

registryCreate :: Registry -> Value -> IO Value
registryCreate registry input = do
 require localAvailable 403 "Local corpora are unavailable on this device"
 let name = at input "name"; scope = fallback (get input "scope") (str "")
 require (null (filter (`notElem` ["name","scope"]) (keys input)) && not (T.null $ T.strip name) && T.length name <= 200 && case scope of String value -> T.length value <= 16000; _ -> False) 400 "Provide a corpora name and scope of work"
 identity <- UUID.toText <$> UUID.nextRandom
 let folder = serviceRoot (registryService registry) </> "corporas" </> T.unpack identity
 createDirectoryIfMissing True folder
#ifndef mingw32_HOST_OS
 setFileMode folder 0o700
#endif
 writeJSON (folder </> "corpora-identity.json") $ obj [("version",num 1),("corpora_id",str identity)]
 mainIdentity <- Service.corporaIdentity (registryService registry)
 writeJSON (folder </> "corpora-settings.json") $ obj [("name",str $ T.strip name),("scope",str $ T.strip $ text scope),("device",get mainIdentity "device"),("status",str "paused"),("revision",num 1),("operations",empty)]
 registryLock registry $ do
  modifyIORef' (registrySaved registry) $ \saved -> put "locals" (arr $ items saved "locals" ++ [str identity]) saved
  saveRegistry registry
  (service,_,_) <- local registry identity
  metadata <- Service.corporaIdentity service
  pure $ put "location" (str "Local") metadata

data Pairing = Pairing Text Text PairingProgress
data PairingProgress = AwaitingApproval | PairApproved Value | PairFailed Text PairingProgress | PairCancelled PairingProgress
data PairingEvent = ApprovePair Value | FailPair Text | CancelPair
instance ToJSON Pairing where
 toJSON (Pairing identity code progress) = merge (obj [("id",str identity),("code",str code)]) (fields progress)
  where
  fields AwaitingApproval = obj [("state",str "waiting")]
  fields (PairApproved corpora) = obj [("state",str "approved"),("corpora",put "location" (str "Remote") corpora)]
  fields (PairFailed reason prior) = merge (fields prior) (obj [("state",str "failed"),("error",str reason)])
  fields (PairCancelled prior) = put "state" (str "cancelled") (fields prior)
pairWaiting :: Pairing -> Bool
pairWaiting (Pairing _ _ AwaitingApproval) = True
pairWaiting _ = False
pairCancelled :: Pairing -> Bool
pairCancelled (Pairing _ _ (PairCancelled _)) = True
pairCancelled _ = False
advancePairing :: PairingEvent -> Pairing -> Pairing
advancePairing event pairing@(Pairing identity code progress) = Pairing identity code $ case event of
 CancelPair | pairCancelled pairing -> progress
 CancelPair -> PairCancelled progress
 FailPair _ | pairCancelled pairing -> progress
 FailPair reason -> PairFailed reason progress
 ApprovePair _ | pairCancelled pairing -> progress
 ApprovePair corpora -> PairApproved corpora
canApprovePairing :: Bool -> Maybe Pairing -> Bool
canApprovePairing closed pending = not closed && maybe False (not . pairCancelled) pending

beginPair :: Registry -> Value -> IO Value
beginPair registry input = do
 client <- invitation input
 identity <- newId
 config <- readMVar (clientConfig client)
 code <- confirmationCode (get input "desktop") (get config "public") (at input "room")
 let item = Pairing identity code AwaitingApproval
 registryLock registry $ do
  saved <- readIORef (registrySaved registry)
  require (not $ boolean $ get (get saved "relay_servers") "reset_pending") 409 "Relay switch interrupted. Restart the app to finish."
  pending <- readIORef (registryPending registry)
  require (all (not . pairWaiting) pending) 409 "A connection is already waiting for approval"
  writeIORef (registryPending registry) (M.singleton identity item)
  modifyIORef' (registryPairClients registry) (M.insert identity client)
 let run = do
      corpora <- pairClient client input
      registryLock registry $ do
       closed <- readIORef (registryClosed registry)
       pending <- readIORef (registryPending registry)
       when (canApprovePairing closed (M.lookup identity pending)) $ do
        saved <- readIORef (registrySaved registry)
        opened <- readIORef (registryLocals registry)
        let corporaId = at corpora "id"
        require (M.notMember corporaId opened && str corporaId `notElem` items saved "locals") 409 "This corpora is already available locally"
        configValue <- readMVar (clientConfig client)
        writeIORef (registrySaved registry) $ put "remotes" (put corporaId configValue $ get saved "remotes") saved
        saveRegistry registry
        modifyIORef' (registryPending registry) (M.adjust (advancePairing (ApprovePair corpora)) identity)
     failed (errorValue :: SomeException) = registryLock registry $
      modifyIORef' (registryPending registry) $ M.adjust (advancePairing (FailPair (exceptionMessage errorValue))) identity
     finish = closeClient client >> registryLock registry (modifyIORef' (registryPairClients registry) (M.delete identity))
 void $ forkIO $ (run `catch` failed) `finally` finish
 pure (toJSON item)

exceptionMessage :: SomeException -> Text
exceptionMessage errorValue = case fromException errorValue of
 Just (APIError _ message) -> message
 Nothing -> T.pack $ displayException errorValue

pairing :: Registry -> Text -> Bool -> IO Value
pairing registry identity cancelPairing = registryLock registry $ do
 pending <- readIORef (registryPending registry)
 item <- maybe (failure 404 "Unknown pairing") pure (M.lookup identity pending)
 if not cancelPairing then pure (toJSON item) else do
  let updated = advancePairing CancelPair item
  modifyIORef' (registryPending registry) (M.insert identity updated)
  clients <- readIORef (registryPairClients registry)
  mapM_ revokeClient (M.lookup identity clients)
  pure (toJSON updated)

closeRegistry :: Registry -> IO ()
closeRegistry registry = do
 writeIORef (registryClosed registry) True
 clients <- readIORef (registryClients registry)
 pending <- readIORef (registryPairClients registry)
 mapM_ revokeClient (M.elems clients ++ M.elems pending)
 opened <- readIORef (registryLocals registry)
 mapM_ (\(_,_,close) -> close) opened

resetSavedConnections :: FilePath -> [Value] -> Value -> IO ()
resetSavedConnections root identities connection = forM_ (root : map ((root </> "corporas") </>) (map (T.unpack . text) identities)) $ \folder -> do
 let device = folder </> "remote-device.json"
 exists <- doesFileExist device
 when exists $ removeFile device
 present <- doesDirectoryExist folder
 when present $ writeJSON (folder </> "remote-relay.json") connection

recoverRelays :: FilePath -> IO ()
recoverRelays root = do
 let path = root </> "host-corpora.json"
 saved <- readJSON path empty
 let state = get saved "relay_servers"
 when (boolean $ get state "reset_pending") $ do
  selected <- maybe (failure 409 "Selected relay is unavailable") pure $ findId (at state "selected") (items state "servers")
  resetSavedConnections root (items saved "locals") (get selected "connection")
  writeJSON path $ put "remotes" empty $ put "relay_servers" (remove ["reset_pending"] state) saved

relayState :: Registry -> IO Value
relayState registry = do
 saved <- readIORef (registrySaved registry)
 if has saved "relay_servers" then pure (get saved "relay_servers") else do
  let builtin identity name relay available = obj [("id",str identity),("name",str name),("address",str $ relay <> "/beta"),("available",Bool available),("archived",Bool False),("builtin",Bool True),("connection",obj [("format",str connectionFormat),("relay",str relay),("beta",Bool True)])]
      initial = [builtin "sapiens-beta" "Sapiens4 beta" "https://remote-relay.sapiens4.ai" True, builtin "utana-beta" "Utana beta" "https://remote-relay.utana.group" False]
      root = serviceRoot (registryService registry)
      folders = root : [root </> "corporas" </> T.unpack (text identity) | identity <- items saved "locals"]
  (rows,selected) <- foldM (\(rows,selected) folder -> do
    old <- try $ readConnection (folder </> "remote-relay.json")
    case old of
     Left (_::SomeException) -> pure (rows,selected)
     Right connection -> do
      row <- case find ((== connection) . (`get` "connection")) rows of
       Just existing -> pure existing
       Nothing -> do
        identity <- newId
        pure $ obj [("id",str identity),("name",str $ if folder == root then "My relay" else "Imported relay"),("connection",connection),("available",Bool True),("archived",Bool False),("builtin",Bool False)]
      pure (if row `elem` rows then rows else rows ++ [row], if folder == root then get row "id" else selected)) (initial,str "sapiens-beta") folders
  let state = obj [("servers",arr rows),("selected",selected),("revision",num 1),("beta_accepted",Bool False)]
  writeIORef (registrySaved registry) (put "relay_servers" state saved)
  saveRegistry registry
  pure state

findRelay :: Value -> Text -> IO Value
findRelay state identity = maybe (failure 404 "Unknown relay server") pure $ findId identity (items state "servers")

relayView :: Registry -> Maybe Text -> IO Value
relayView registry identity = registryLock registry $ relayState registry >>= \state -> maybe (pure $ publicRelays state) (findRelay state) identity

publicRelays :: Value -> Value
publicRelays state = obj [("selected",get state "selected"),("revision",get state "revision"),("beta_accepted",get state "beta_accepted"),("servers",arr $ map public $ items state "servers")]
 where public row = merge (remove ["connection"] row) $ obj [("address",fallback (get row "address") $ get (get row "connection") "relay"),("beta",Bool $ boolean $ get (get row "connection") "beta")]

selectedConnection :: Registry -> IO Value
selectedConnection registry = do
 state <- relayState registry
 row <- findRelay state (at state "selected")
 require (not (boolean $ get row "archived") && boolean (get row "available")) 409 "Select an available relay."
 require (not (boolean $ get (get row "connection") "beta") || boolean (get state "beta_accepted")) 409 "Accept the beta terms before pairing."
 pure $ get row "connection"

data RelayChange = RelayChange Value Bool
planRelayChange :: Text -> Value -> Value -> Validation RelayChange
planRelayChange key state input = do
 check (get input "revision" == get state "revision") 409 "Relay settings changed. Reopen the selector."
 let action = at input "action"; identity = at input "id"
 row <- if T.null identity then pure Null else maybe (Left (APIError 404 "Unknown relay server")) Right (findId identity (items state "servers"))
 (changed,reset) <- case action of
  "save" -> do
   let name = at input "name"
   check (not (T.null $ T.strip name) && T.length name <= 200) 400 "Enter a name (up to 200 characters)."
   connection <- either (\(APIError _ message) -> Left (APIError 409 message)) Right (connectionValue (get input "connection"))
   let base = if row /= Null then row else obj [("id",str key),("archived",Bool False),("available",Bool True),("builtin",Bool False)]
       different = row /= Null && connection /= get row "connection"
       updated = merge (if different then remove ["address"] base else base) $ obj [("name",str $ T.strip name),("connection",connection)]
       rows = if row == Null then items state "servers" ++ [updated] else map (\old -> if get old "id" == get row "id" then updated else old) (items state "servers")
       selected = identity == at state "selected"
   pure (put "servers" (arr rows) $ if different && selected then put "beta_accepted" (Bool False) state else state, different && selected)
  "switch" -> do
   check (row /= Null && not (boolean $ get row "archived") && boolean (get row "available")) 409 "This relay is unavailable."
   let reset = identity /= at state "selected"
   pure (put "selected" (str identity) $ if reset then put "beta_accepted" (Bool False) state else state,reset)
  value | value `elem` ["archive","restore"] -> do
   check (row /= Null) 404 "Unknown relay server"
   check (action /= "archive" || identity /= at state "selected") 409 "Switch servers before archiving this relay."
   pure (put "servers" (arr $ map (\old -> if at old "id" == identity then put "archived" (Bool $ action == "archive") old else old) $ items state "servers") state,False)
  "accept-beta" -> do
   check (get input "accepted" == Bool True) 400 "Accept the beta terms to continue."
   pure (put "beta_accepted" (Bool True) state,False)
  _ -> Left (APIError 400 "Unknown relay action")
 check (not reset || get input "confirm_revoke" == Bool True) 409 "Confirm revoking all paired devices and remote corpora connections."
 let next = put "revision" (num $ int (get state "revision") + 1) changed
 pure (RelayChange next reset)

relayChange :: Registry -> Value -> IO Value
relayChange registry input = registryLock registry $ do
 state <- relayState registry
 key <- newId
 RelayChange next reset <- validated (planRelayChange key state input)
 modifyIORef' (registrySaved registry) $ put "relay_servers" next
 when reset $ do
  queueRelayCleanup registry
  modifyIORef' (registrySaved registry) $ put "remotes" empty . put "relay_servers" (put "reset_pending" (Bool True) next)
  saveRegistry registry
  selected <- findRelay next (at next "selected")
  resetConnections registry (get selected "connection")
  modifyIORef' (registrySaved registry) $ put "relay_servers" next
 saveRegistry registry
 pure $ put "reset" (Bool reset) $ publicRelays next

queueRelayCleanup :: Registry -> IO ()
queueRelayCleanup registry = do
 saved <- readIORef (registrySaved registry)
 identity <- Service.corporaIdentity (registryService registry)
 let root = serviceRoot (registryService registry)
     folders = (root,at identity "id") : [(root </> "corporas" </> T.unpack (text key),text key) | key <- items saved "locals"]
 entries <- forM folders $ \(folder,key) -> (do
  config <- readJSON (folder </> "remote-device.json") Null
  if all (has config) ["relay","room","token"] then pure $ Just $ obj [("relay",get config "relay"),("room",get config "room"),("token",get config "token"),("corpora_id",fallback (get config "corpora_id") $ str key)] else pure Nothing) `catch` \(_::SomeException) -> pure Nothing
 writeIORef (registrySaved registry) $ put "relay_cleanup" (arr $ nub $ items saved "relay_cleanup" ++ catMaybes entries) saved

cleanupRelays :: Registry -> Maybe Text -> Maybe Text -> IO ()
cleanupRelays registry relay identity = do
 saved <- readIORef (registrySaved registry)
 forM_ (items saved "relay_cleanup") $ \item -> when (maybe True (== at item "relay") relay && maybe True (== at item "corpora_id") identity) $ do
  closed <- readIORef (registryClosed registry)
  unless closed $ do
   deleted <- deleteRoom item
   when deleted $ do
    modifyIORef' (registrySaved registry) $ \current -> put "relay_cleanup" (arr $ filter (/=item) $ items current "relay_cleanup") current
    saveRegistry registry

resetConnections :: Registry -> Value -> IO ()
resetConnections registry connection = do
 modifyIORef' (registryPending registry) $ M.map (advancePairing CancelPair)
 clients <- atomicModifyIORef' (registryClients registry) (\current -> (M.empty,current))
 pending <- readIORef (registryPairClients registry)
 mapM_ revokeClient (M.elems clients ++ M.elems pending)
 opened <- readIORef (registryLocals registry)
 mapM_ (\(_,bridge,_) -> void $ revokeBridge bridge False) opened
 saved <- readIORef (registrySaved registry)
 resetSavedConnections (serviceRoot $ registryService registry) (items saved "locals") connection
 void $ forkIO $ registryLock registry $ cleanupRelays registry Nothing Nothing
