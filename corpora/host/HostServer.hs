module HostServer where

import qualified Clock
import Types
import Admission
import qualified Telemetry
import qualified TelemetryWire
import System.Environment (getArgs)
import DataRoot
import UpdateState (assertDataAvailable)
import System.Environment (lookupEnv)
import qualified Service as Service
import Assets
import HTTP
import Bridge
import BrowserCrypto
import Registry
import qualified Onboarding as O
import Voice (startTranscriber)
import Control.Concurrent
import Control.Concurrent.STM hiding (check)
import qualified Control.Concurrent.STM as STM
import Control.Concurrent.Async
import Control.Exception
import Control.Monad
import qualified Data.ByteString as B
import Data.IORef
import qualified Data.Map.Strict as M
import Data.Aeson (encode,toJSON,Value(..))
import System.Directory (getModificationTime)
import Network.Wai.Handler.WebSockets (websocketsOr, isWebSocketsReq)
import qualified Network.WebSockets as WS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import System.Directory (doesFileExist)
import System.FilePath ((</>))
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp
import qualified Network.Socket as Socket


data Host = Host { hostPort :: Int, hostService :: Service, hostStop :: IO (), hostActivate :: IO (), hostBusy :: IO Bool, hostQuiesce :: IO Bool, hostResume :: IO (), hostPrepareReady :: IO () }
listenLoopback :: Int -> IO (Socket.Socket,Int)
listenLoopback port = Socket.withSocketsDo $ do
 sock <- Socket.socket Socket.AF_INET Socket.Stream Socket.defaultProtocol
 flip onException (Socket.close sock) $ do
  Socket.setSocketOption sock Socket.ReuseAddr 1
  Socket.bind sock (Socket.SockAddrInet (fromIntegral port) (Socket.tupleToHostAddress (127,0,0,1)))
  Socket.listen sock 128
  Socket.SockAddrInet assigned _ <- Socket.getSocketName sock
  pure (sock,fromIntegral assigned)
startHost :: Maybe Bool -> FilePath -> Int -> Bool -> IO Host
startHost consent root port preparing = mask $ \restore -> do
 mode <- lookupEnv "SAPIENS_STORAGE_MODE"
 exists <- doesFileExist (root </> "corpora-identity.json")
 let choice = if mode==Just "existing" || mode==Nothing && exists then AttachExisting else StartNew
 gate <- newAdmission preparing
 reporter <- newIORef Nothing
 args <- getArgs
 desktopPid <- lookupEnv "SAPIENS_DESKTOP_PID"
 let context = if "--desktop" `elem` args || desktopPid/=Nothing then Telemetry.Desktop else Telemetry.Headless
     startReporter = do
      current <- readIORef reporter
      case current of
       Just _ -> pure ()
       Nothing -> Telemetry.openTelemetry context consent >>= writeIORef reporter . Just
     closeReporter = readIORef reporter >>= mapM_ Telemetry.closeTelemetry
 clock <- Clock.start
 browser <- newBrowserCrypto
 unless preparing startReporter
 (assigned,host) <- restore (openLocal clock gate reporter browser (not preparing) choice root port False) `onException` (closeReporter `finally` Clock.stop clock)
 let service = registryService (localRegistry host)
     activate = do
      assertDataAvailable (serviceRoot service)
      startReporter
      openedHosts host >>= mapM_ startLocal
      openAdmission gate
     prepare = do
      closeAdmission gate
      openedHosts host >>= mapM_ (Service.prepareUpdatedService . registryService . localRegistry)
     busy h = do
      let r = localRegistry h
      work <- Service.busyService (registryService r)
      pairClients <- readIORef (registryPairClients r)
      pure (work || not (M.null pairClients))
     busyHosts = openedHosts host >>= fmap or . mapM busy
     quiesce = do
      closeAdmission gate
      occupied <- busyHosts
      when occupied (openAdmission gate)
      pure (not occupied)
 pure (Host assigned service (stopLocal host `finally` (closeReporter `finally` Clock.stop clock)) activate busyHosts (quiesce `onException` openAdmission gate) (openAdmission gate) prepare)

openLocal :: Clock.Clock -> Admission -> IORef (Maybe Telemetry.Telemetry) -> BrowserCrypto -> Bool -> StorageChoice -> FilePath -> Int -> Bool -> IO (Int,LocalHost)
openLocal clock gate reporterRef browser startWorkers choice root requested managed = do
 mode <- lookupEnv "SAPIENS_STORAGE_MODE"
 let report phase = when (not managed && mode `elem` [Just "new",Just "existing"]) (putStrLn phase)
 report openingStorage
 bracketOnError (Service.openService clock gate reporterRef choice root) Service.closeService $ \service -> do
  opened@(_,host) <- start service
  report openedStorage `onException` stopLocal host
  pure opened
 where
 start service = do
  (socket,port) <- listenLoopback requested
  flip onException (Socket.close socket) $ do
   Service.attachEndpoint service ("http://127.0.0.1:"<>T.pack (show port))
   bridge <- newBridge browser service
   activated <- newMVar False
   let child selection path = do
        running <- readMVar activated
        snd <$> openLocal clock gate reporterRef browser running selection path 0 True
   registry <- newRegistry service bridge child
   onboarding <- if managed then pure Nothing else Just <$> O.openOnboarding (repositoryRoot service)
   shutdown <- newEmptyMVar
   ready <- newEmptyMVar
   let settings = setServerName "Sapiens4" $ setPort port $ setHost "127.0.0.1" $ setGracefulCloseTimeout1 1000 $ setGracefulShutdownTimeout (Just 5) $ setInstallShutdownHandler (putMVar shutdown) $ setBeforeMainLoop (putMVar ready ()) defaultSettings
   worker <- async $ withAsync (watchSnapshotFiles service) $ \watcher -> link watcher >> runSettingsSocket settings socket (application service bridge (if managed then Nothing else Just registry) onboarding port)
   void (race (wait worker) (takeMVar ready))
   heartbeat <- newMVar Nothing
   transcriber <- newMVar (pure ())
   let activate = modifyMVar_ activated $ \done -> if done then pure True else do
        startRegistry registry
        Service.startService service
        unless managed $ do
         reporter <- readIORef (serviceTelemetry service)
         forM_ reporter $ \telemetry -> Telemetry.emit telemetry Telemetry.AppStarted
         when (choice==StartNew) (emitTelemetry service Telemetry.CorporaCreated)
         let ticksPerMinute = 120
             heartbeatTicks = ticksPerMinute * round (Telemetry.heartbeatInterval / 60)
         task <- Clock.subscribe clock $ \tick -> when (tick `mod` ticksPerMinute==0) $
          forM_ reporter $ \telemetry -> do
           when (tick `mod` heartbeatTicks==0) (Telemetry.emit telemetry Telemetry.PulseId)
           Telemetry.requestFlush telemetry
         modifyMVar_ heartbeat (const (pure (Just task)))
        startBridge bridge
        startTranscriber service >>= \stopVoice -> modifyMVar_ transcriber (const (pure stopVoice))
        pure True
   stopped <- newMVar False
   let stop = modifyMVar_ stopped $ \done -> if done then pure True else do
        mapM_ O.closeOnboarding onboarding
        closeRegistry registry
        closeBridge bridge
        readMVar heartbeat >>= mapM_ cancel
        readMVar shutdown >>= id
        readMVar transcriber >>= id
        -- Children can retain Windows socket handles until their work is stopped.
        Service.closeService service
        void (waitCatch worker)
        Socket.close socket
        pure True
   (if startWorkers then Service.prepareService service >> activate else prepareRegistry registry) `onException` stop
   pure (port,LocalHost registry activate stop)
localRequest :: Int -> Bool -> RequestHeaders -> Validation ()
localRequest port write headers = do
 let host = lookup hHost headers
     originHeader = lookup hOrigin headers
     allowed = map (TE.encodeUtf8 . (<> ":"<>T.pack (show port))) ["127.0.0.1","localhost"]
 check (maybe False (`elem` allowed) host) 403 "Loopback host required"
 check (originHeader==Nothing || originHeader==(("http://" <>) <$> host)) 403 "Same-origin request required"
 when write (check (lookup "X-Sapiens-Local" headers==Just "1" && fmap (B.takeWhile (/=59)) (lookup hContentType headers)==Just "application/json") 403 "Local JSON request required")

application :: Service -> Bridge -> Maybe Registry -> Maybe O.Onboarding -> Int -> Application
application service bridge registry onboarding port req respond
 | ["api","host","crypto"] <- pathInfo req = do
    result <- try $ do
     validated (localRequest port False (requestHeaders req))
     require (requestMethod req == "GET" && B.null (rawQueryString req)) 403 "Local crypto endpoint required"
     when (isWebSocketsReq req) $
      require (lookup hOrigin (requestHeaders req) /= Nothing) 403 "Same-origin crypto window required"
    case result of
     Left (APIError code message) -> respond (jsonResponse code (obj [("error",str message)]))
     Right () -> websocketsOr WS.defaultConnectionOptions
      {WS.connectionMessageDataSizeLimit=WS.SizeLimit 24000000,WS.connectionFramePayloadSizeLimit=WS.SizeLimit 24000000}
      (cryptoSocket (bridgeCrypto bridge)) (\_ send -> do
       connected <- browserConnected (bridgeCrypto bridge)
       send (jsonResponse 200 (obj [("browser_crypto",Bool True),("connected",Bool connected)]))) req respond
 | ["api","host","corpora",owner,"api","updates"] <- pathInfo req = do
    let headers = requestHeaders req
    result <- try $ admitted service False $ do
     validated (localRequest port False headers)
     require (lookup hOrigin headers /= Nothing && B.null (rawQueryString req)) 403 "Same-origin updates required"
     r <- maybe (failure 404 "Host registry unavailable") pure registry
     require localAvailable 404 "Local corpora are unavailable"
     registryService <$> registryLock r (local r owner)
    case result of
     Left (APIError code message) -> respond (jsonResponse code (obj [("error",str message)]))
     Right s -> websocketsOr WS.defaultConnectionOptions {WS.connectionMessageDataSizeLimit=WS.SizeLimit 1024}
      (updatesSocket s) (\_ send -> send (jsonResponse 400 (obj [("error",str "WebSocket upgrade required")]))) req respond
 | otherwise = httpApplication service bridge registry onboarding port req respond

-- One metadata sampler per service, asleep when no window is subscribed.
watchSnapshotFiles :: Service -> IO ()
watchSnapshotFiles s = do
 previous <- newIORef M.empty
 let sample _ = do
      count <- readTVarIO (updateSubscribers s)
      when (count>0) $ do
       files <- readIORef (snapshotFiles s)
       current <- M.traverseWithKey (\path _ -> (Just <$> getModificationTime path) `catch` (\(_::IOException) -> pure Nothing)) (M.fromSet (const ()) files)
       old <- atomicModifyIORef' previous (\old -> (current,old))
       when (old/=current) (notifyChange s)
 bracket (Clock.subscribe (serviceClock s) sample) cancel wait

updatesSocket :: Service -> WS.ServerApp
updatesSocket s pending = do
 conn <- WS.acceptRequest pending
 let count f = atomically (modifyTVar' (updateSubscribers s) f)
     send previous = do
      atomically $ readTVar (serviceRevision s) >>= STM.check . (/= previous)
      revision <- readTVarIO (serviceRevision s)
      WS.sendTextData conn (encode revision)
      send revision
 bracket_ (count (+1)) (count (subtract 1)) $ WS.withPingThread conn 25 (pure ()) $
  race_ (send (-1)) (void (WS.receiveDataMessage conn))

httpApplication :: Service -> Bridge -> Maybe Registry -> Maybe O.Onboarding -> Int -> Application
httpApplication service bridge registry onboarding port req respond = respond =<< respondError (admitted service (requestMethod req /= "GET") $ do
 let method = TE.decodeUtf8 (requestMethod req)
     path = TE.decodeUtf8 (rawPathInfo req)
     target = requestTarget req
     write = method `elem` ["POST","PUT"]
 validated (localRequest port write (requestHeaders req))
 body <- if write then Just <$> bodyObject (if "/attachments" `T.isSuffixOf` path then 15000000 else 2000000) req else pure Nothing
 let data_ = maybe empty id body
 if "/api/host" `T.isPrefixOf` path && maybe False (const True) registry then do
  r <- maybe (failure 404 "Host registry unavailable") pure registry
  stopping <- readIORef (serviceStopping service)
  when write (require (not stopping) 503 "Host is shutting down")
  case (method,T.splitOn "/" path) of
   ("GET",["","api","host","computer"]) -> maybe (failure 404 "Local setup unavailable") (fmap (jsonResponse 200) . O.computerView) onboarding
   ("GET",["","api","host","telemetry"]) -> do
    reporter <- readIORef (serviceTelemetry service)
    jsonResponse 200 <$> maybe (pure (obj [("enabled",Bool False),("available",Bool False)])) (fmap toJSON . Telemetry.consentView) reporter
   ("PUT",["","api","host","telemetry"]) -> do
    reporter <- readIORef (serviceTelemetry service) >>= maybe (failure 503 "Telemetry configuration unavailable") pure
    on <- case get data_ "enabled" of Bool value -> pure value; _ -> failure 400 "Expected enabled boolean"
    saved <- Telemetry.setConsent reporter on
    either (failure 500) pure saved
    jsonResponse 200 . toJSON <$> Telemetry.consentView reporter
   ("POST",["","api","host","telemetry","heartbeat"]) -> do
    reporter <- readIORef (serviceTelemetry service)
    forM_ reporter $ \telemetry -> do
     context <- Telemetry.localClientContext telemetry (TelemetryWire.parseClientContext data_)
     mapM_ (Telemetry.emit telemetry . Telemetry.UIHeartbeat) (TelemetryWire.parseHeartbeat Nothing context data_)
    pure (jsonResponse 202 empty)
   ("POST",["","api","host","computer"]) -> maybe (failure 404 "Local setup unavailable") (fmap (jsonResponse 200) . flip O.computerAction data_) onboarding
   ("GET",["","api","host","onboarding"]) -> maybe (failure 404 "Local setup unavailable") (fmap (jsonResponse 200) . O.onboardingView) onboarding
   ("GET",["","api","host"]) -> jsonResponse 200 <$> registryRecords r
   ("GET",["","api","host","relays"]) -> jsonResponse 200 <$> relayView r Nothing
   ("GET",["","api","host","relays",ident]) -> jsonResponse 200 <$> relayView r (Just ident)
   ("POST",["","api","host","relays"]) -> jsonResponse 200 <$> relayChange r data_
   ("POST",["","api","host","local"]) -> jsonResponse 201 <$> registryCreate r data_
   ("POST",["","api","host","pair"]) -> jsonResponse 202 <$> beginPair r data_
   (m,["","api","host","pair",ident]) | m `elem` ["GET","POST"] -> jsonResponse 200 <$> pairing r ident write
   (_,"":"api":"host":"corpora":ident:rest) -> do
    (code,raw,mime) <- registryRoute r ident method ("/"<>T.intercalate "/" rest<>TE.decodeUtf8 (rawQueryString req)) body
    pure (response code raw mime)
   _ -> failure 404 "Not found"
 else if method=="GET" && path=="/api/remote" then jsonResponse 200 <$> bridgeStatus bridge
 else if method=="GET" && path `elem` ["/","/remote","/remote/"] then pure (responseLBS status302 [(hLocation,if "/remote" `T.isPrefixOf` path then "/workspace/?pairing=local" else "/workspace/")] "")
 else do
  found <- if method=="GET" then asset (repositoryRoot service) path else pure Nothing
  case found of
   Just (mime,raw) -> pure (response 200 raw mime)
   Nothing -> if method=="POST" && "/api/remote/" `T.isPrefixOf` path then do
    value <- case registry of
     Just r -> remoteAction r bridge path data_
     Nothing -> case path of
      "/api/remote/pair" -> createPairing bridge data_
      "/api/remote/approve" -> approvePairing bridge data_
      "/api/remote/revoke" -> revokeBridge bridge True
      _ -> failure 404 "Not found"
    pure (jsonResponse (if "/pair" `T.isSuffixOf` path then 201 else 200) value)
   else do
    (code,raw,mime) <- Service.dispatch Telemetry.Direct service method target body
    pure (response code raw mime))
