module HostServer where

import Types
import DataRoot
import System.Environment (lookupEnv)
import qualified Service as Service
import Telemetry (Emit, EventIntent (ICorporaCreated), corporaRef)
import qualified TelemetrySender as Telemetry
import TelemetryWire (RuntimeKind)
import Assets
import HTTP
import Bridge
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
import Data.Aeson (encode)
import System.Directory (getModificationTime)
import Network.Wai.Handler.WebSockets (websocketsOr)
import qualified Network.WebSockets as WS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import System.Directory (doesFileExist)
import System.FilePath ((</>))
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp
import qualified Network.Socket as Socket


data Host = Host { hostPort :: Int, hostService :: Service, hostBridge :: Bridge, hostStop :: IO () }
listenLoopback :: Int -> IO (Socket.Socket,Int)
listenLoopback port = Socket.withSocketsDo $ do
 sock <- Socket.socket Socket.AF_INET Socket.Stream Socket.defaultProtocol
 flip onException (Socket.close sock) $ do
  Socket.setSocketOption sock Socket.ReuseAddr 1
  Socket.bind sock (Socket.SockAddrInet (fromIntegral port) (Socket.tupleToHostAddress (127,0,0,1)))
  Socket.listen sock 128
  Socket.SockAddrInet assigned _ <- Socket.getSocketName sock
  pure (sock,fromIntegral assigned)
startHost :: RuntimeKind -> FilePath -> Int -> Bool -> IO Host
startHost runtime root requested managed = mask $ \restore -> do
 mode <- lookupEnv "SAPIENS_STORAGE_MODE"
 exists <- doesFileExist (root </> "corpora-identity.json")
 telemetry <- Telemetry.startConfiguredTelemetry runtime
 host <- restore (startHostWith (Telemetry.hostEmit telemetry)
   (if managed || mode==Just "existing" || mode==Nothing && exists then AttachExisting else StartNew) root requested managed)
   `onException` Telemetry.hostTelemetryStop telemetry
 pure host {hostStop = Telemetry.hostTelemetryStop telemetry `finally` hostStop host}

startHostWith :: Emit -> StorageChoice -> FilePath -> Int -> Bool -> IO Host
startHostWith emit choice root requested managed = do
 mode <- lookupEnv "SAPIENS_STORAGE_MODE"
 let report phase = when (not managed && mode `elem` [Just "new",Just "existing"]) (putStrLn phase)
 report openingStorage
 bracketOnError (Service.openService emit choice root) Service.closeService $ \service -> do
  report openedStorage >> start service
 where
 start service = do
  unless managed (recoverRelays root)
  (socket,port) <- listenLoopback requested
  flip onException (Socket.close socket) $ do
   Service.attachEndpoint service ("http://127.0.0.1:"<>T.pack (show port))
   bracketOnError (newBridge service) closeBridge $ \bridge -> do
    let child selection path = do
         host <- startHostWith emit selection path 0 True
         pure (hostService host,hostBridge host,hostStop host)
    bracketOnError (if managed then pure Nothing else Just <$> newRegistry service bridge child)
      (mapM_ closeRegistry) $ \registry ->
     bracketOnError (if managed then pure Nothing else Just <$> O.openOnboarding (repositoryRoot service))
       (mapM_ O.closeOnboarding) $ \onboarding -> do
      shutdown <- newEmptyMVar
      ready <- newEmptyMVar
      let settings = setServerName "Sapiens4" $ setPort port $ setHost "127.0.0.1" $ setGracefulCloseTimeout1 1000 $ setGracefulShutdownTimeout (Just 5) $ setInstallShutdownHandler (putMVar shutdown) $ setBeforeMainLoop (putMVar ready ()) defaultSettings
      bracketOnError (async $ withAsync (watchSnapshotFiles service) $ \watcher -> link watcher >> runSettingsSocket settings socket (application service bridge registry onboarding port)) cancel $ \worker -> do
       void (race (wait worker) (takeMVar ready))
       Service.startService service
       bracketOnError (startTranscriber service) id $ \stopTranscriber -> do
        stopped <- newMVar False
        let stop = do
             first <- modifyMVar stopped (\done -> pure (True, not done))
             when first $
              mapM_ O.closeOnboarding onboarding `finally`
               (mapM_ closeRegistry registry `finally`
                (closeBridge bridge `finally`
                 ((readMVar shutdown >>= id) `finally`
                  -- Stop transcription before closing the database it uses.
                  (stopTranscriber `finally`
                   (Service.closeService service `finally`
                    (void (waitCatch worker) `finally` Socket.close socket))))))
        when (choice == StartNew && not managed) $
         forM_ (corporaRef (serviceRoot service)) (emit . ICorporaCreated)
        pure (Host port service bridge stop)
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
 | ["api","host","corpora",owner,"api","updates"] <- pathInfo req = do
    let headers = requestHeaders req
    result <- try $ do
     validated (localRequest port False headers)
     require (lookup hOrigin headers /= Nothing && B.null (rawQueryString req)) 403 "Same-origin updates required"
     r <- maybe (failure 404 "Host registry unavailable") pure registry
     require localAvailable 404 "Local corpora are unavailable"
     (s,_,_) <- registryLock r (local r owner)
     pure s
    case result of
     Left (APIError code message) -> respond (jsonResponse code (obj [("error",str message)]))
     Right s -> websocketsOr WS.defaultConnectionOptions {WS.connectionMessageDataSizeLimit=WS.SizeLimit 1024}
      (updatesSocket s) (\_ send -> send (jsonResponse 400 (obj [("error",str "WebSocket upgrade required")]))) req respond
 | otherwise = httpApplication service bridge registry onboarding port req respond

-- One metadata sampler per service, asleep when no window is subscribed.
watchSnapshotFiles :: Service -> IO ()
watchSnapshotFiles s = loop M.empty
 where
 loop previous = do
  atomically $ readTVar (updateSubscribers s) >>= STM.check . (>0)
  files <- readIORef (snapshotFiles s)
  current <- M.traverseWithKey (\path _ -> (Just <$> getModificationTime path) `catch` (\(_::IOException) -> pure Nothing)) (M.fromSet (const ()) files)
  when (previous /= current) (notifyChange s)
  threadDelay 1000000
  loop current

updatesSocket :: Service -> WS.ServerApp
updatesSocket s pending = do
 conn <- WS.acceptRequest pending
 let count f = atomically (modifyTVar' (updateSubscribers s) f)
     send previous = do
      atomically $ readTVar (serviceRevision s) >>= STM.check . (/= previous)
      -- Provider events can arrive in bursts. Send one revision for the batch.
      when (previous >= 0) $ threadDelay 250000
      revision <- readTVarIO (serviceRevision s)
      WS.sendTextData conn (encode revision)
      send revision
 bracket_ (count (+1)) (count (subtract 1)) $ WS.withPingThread conn 25 (pure ()) $
  race_ (send (-1)) (void (WS.receiveDataMessage conn))

httpApplication :: Service -> Bridge -> Maybe Registry -> Maybe O.Onboarding -> Int -> Application
httpApplication service bridge registry onboarding port req respond = respond =<< respondError (do
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
    (code,raw,mime) <- Service.dispatch service method target body
    pure (response code raw mime))
