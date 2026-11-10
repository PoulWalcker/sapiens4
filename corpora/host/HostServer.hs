module HostServer where

import Types
import DataRoot
import System.Environment (lookupEnv)
import qualified Service as Service
import Assets
import HTTP
import Bridge
import Registry
import qualified Onboarding as O
import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception
import Control.Monad
import qualified Data.ByteString as B
import Data.IORef
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
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
startHost :: FilePath -> Int -> Bool -> IO Host
startHost root requested managed = do
 mode <- lookupEnv "SAPIENS_STORAGE_MODE"
 startHostWith (if mode==Just "existing" || managed then AttachExisting else StartNew) root requested managed

startHostWith :: StorageChoice -> FilePath -> Int -> Bool -> IO Host
startHostWith choice root requested managed = do
 service <- Service.openService choice root
 flip onException (Service.closeService service) $ do
  unless managed (recoverRelays root)
  (socket,port) <- listenLoopback requested
  flip onException (Socket.close socket) $ do
   Service.attachEndpoint service ("http://127.0.0.1:"<>T.pack (show port))
   bridge <- newBridge service
   let child selection path = do
        host <- startHostWith selection path 0 True
        pure (hostService host,hostBridge host,hostStop host)
   registry <- if managed then pure Nothing else Just <$> newRegistry service bridge child
   onboarding <- if managed then pure Nothing else Just <$> O.openOnboarding (repositoryRoot service)
   shutdown <- newEmptyMVar
   ready <- newEmptyMVar
   let settings = setServerName "Sapiens4" $ setPort port $ setHost "127.0.0.1" $ setGracefulCloseTimeout1 1000 $ setGracefulShutdownTimeout (Just 5) $ setInstallShutdownHandler (putMVar shutdown) $ setBeforeMainLoop (putMVar ready ()) defaultSettings
   worker <- async (runSettingsSocket settings socket (application service bridge registry onboarding port))
   void (race (wait worker) (takeMVar ready))
   Service.startService service
   stopped <- newMVar False
   let stop = modifyMVar_ stopped $ \done -> if done then pure True else do
        mapM_ O.closeOnboarding onboarding
        mapM_ closeRegistry registry
        closeBridge bridge
        readMVar shutdown >>= id
        -- Children can retain Windows socket handles until their work is stopped.
        Service.closeService service
        void (waitCatch worker)
        Socket.close socket
        pure True
   pure (Host port service bridge stop)
application :: Service -> Bridge -> Maybe Registry -> Maybe O.Onboarding -> Int -> Application
application service bridge registry onboarding port req respond = respond =<< respondError (do
 let method = TE.decodeUtf8 (requestMethod req)
     path = TE.decodeUtf8 (rawPathInfo req)
     target = requestTarget req
     write = method `elem` ["POST","PUT"]
     host = lookup hHost (requestHeaders req)
     originHeader = lookup hOrigin (requestHeaders req)
     allowed = map (TE.encodeUtf8 . (<> ":"<>T.pack (show port))) ["127.0.0.1","localhost"]
 require (maybe False (`elem` allowed) host) 403 "Loopback host required"
 require (originHeader==Nothing || originHeader==(("http://" <>) <$> host)) 403 "Same-origin request required"
 when write (require (lookup "X-Sapiens-Local" (requestHeaders req)==Just "1" && fmap (B.takeWhile (/=59)) (lookup hContentType (requestHeaders req))==Just "application/json") 403 "Local JSON request required")
 body <- if write then Just <$> bodyObject (if "/attachments" `T.isSuffixOf` path then 15000000 else 2000000) req else pure Nothing
 let data_ = maybe empty id body
 if "/api/host" `T.isPrefixOf` path && maybe False (const True) registry then do
  r <- maybe (failure 404 "Host registry unavailable") pure registry
  stopping <- readIORef (serviceStopping service)
  when write (require (not stopping) 503 "Host is shutting down")
  case (method,T.splitOn "/" path) of
   ("GET",["","api","host","onboarding"]) -> maybe (failure 404 "Local setup unavailable") (fmap (jsonResponse 200) . O.onboardingView) onboarding
   ("POST",["","api","host","onboarding"]) -> do
    setup <- maybe (failure 404 "Local setup unavailable") pure onboarding
    when (at data_ "action"=="finish") $ do
     saved <- O.setupState setup
     when (at saved "step"=="ready") (resumeRegistry r)
    value <- O.onboardingAction setup data_
    pure (jsonResponse 200 value)
   ("GET",["","api","host"]) -> do
    forM_ onboarding $ \setup -> do
     saved <- O.setupState setup
     when (boolean (get saved "completed")) (resumeRegistry r)
    jsonResponse 200 <$> registryRecords r
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
