module Server where

import Types
import Assets
import HTTP
import Crypto
import Mailboxes
import Transport (readFrame)
import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception
import Control.Monad
import Data.Aeson hiding (pairs)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Char
import Data.List (sortOn)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Text (Text)
import Database.SQLite.Simple
import Network.HTTP.Types
import Network.URI hiding (query)
import Network.Wai
import Network.Wai.Handler.WebSockets
import qualified Network.WebSockets as WS
import System.Timeout


data Relay = Relay { relayDB :: FilePath, relayOrigin :: Text, relayAdmin :: Text, relayBeta :: Bool, relayRepo :: FilePath, relayPeers :: MVar (M.Map (Text,Text) (Text,WS.Connection)), relayCount :: MVar Int }
newRelay :: FilePath -> FilePath -> Text -> Text -> Bool -> IO Relay
newRelay repo path admin url beta = do
 require (T.length admin >= 32 && T.all isAscii admin) 400 "SAPIENS_RELAY_ADMIN_TOKEN must contain at least 32 ASCII characters"
 approved <- origin (str url)
 openMailboxes path
 Relay path approved admin beta repo <$> newMVar M.empty <*> newMVar 0
relayApplication :: Relay -> Application
relayApplication relay req respond = do
 let target = TE.decodeUtf8 (rawPathInfo req)
     requestOrigin = lookup hOrigin (requestHeaders req)
     expectedHost = case parseURI (T.unpack (relayOrigin relay)) >>= uriAuthority of Just a -> T.pack (uriRegName a<>uriPort a); _ -> ""
     check = do
      require (lookup hHost (requestHeaders req) == Just (TE.encodeUtf8 expectedHost)) 403 "Unexpected host"
      require (requestOrigin == Nothing || requestOrigin == Just (TE.encodeUtf8 (relayOrigin relay))) 403 "Unexpected origin"
 if target == "/api/socket" && requestMethod req == "GET" then do
  checked <- try (check >> require (B.null (rawQueryString req) && requestOrigin == Just (TE.encodeUtf8 (relayOrigin relay))) 403 "WebSocket requires the relay origin and no query credentials")
  case checked of
   Left (APIError n message) -> respond (jsonResponse n (obj [("error",str message)]))
   Right () -> websocketsOr WS.defaultConnectionOptions {WS.connectionMessageDataSizeLimit=WS.SizeLimit (fromIntegral (maxFrame+4096))} (socketApp relay) (\_ r -> r (jsonResponse 400 (obj [("error",str "WebSocket upgrade required")]))) req respond
 else respond =<< respondError (do
  if target == "/healthz" && requestMethod req == "GET" then pure (jsonResponse 200 (obj [("status",str "ok")])) else do
   check
   if "/api/corpora/" `T.isPrefixOf` target && requestMethod req == "GET" then relayResponse relay 200 <$> lookupAddress (relayDB relay) (T.drop 13 target)
   else do
    found <- if requestMethod req == "GET" then relayAsset relay req else pure Nothing
    case found of
     Just result -> pure result
     Nothing -> do
      let method = TE.decodeUtf8 (requestMethod req)
      body <- if method == "POST" then do
       require (fmap (B.takeWhile (/=59)) (lookup hContentType (requestHeaders req)) == Just "application/json") 413 "Invalid body size or content type"
       bodyObject (if target == "/beta/api/rooms" then 4096 else maxFrame) req
       else pure empty
      if target == "/beta/api/rooms" && method == "POST" then do
       require (relayBeta relay) 404 "Beta access is unavailable. Use your own relay."
       relayResponse relay 200 <$> transact (relayDB relay) "POST" ["api","rooms"] "" body (relayAdmin relay) True
      else do
       let auth = maybe "" TE.decodeUtf8 (lookup hAuthorization (requestHeaders req))
           bearer = T.drop 7 auth
       require ("Bearer " `T.isPrefixOf` auth && not (T.null bearer) && T.length bearer <= 256 && T.all (\c -> ord c >=33 && ord c<=126) bearer) 401 "Bearer token required"
       relayResponse relay 200 <$> transact (relayDB relay) method (filter (/="") (T.splitOn "/" target)) bearer body (relayAdmin relay) False)
relayHeaders :: Relay -> Text -> ResponseHeaders
relayHeaders relay mime = headers mime ++ [("Content-Security-Policy",TE.encodeUtf8 ("default-src 'none'; script-src 'self'; style-src 'self' 'unsafe-inline'; connect-src 'self' " <> wsOrigin <> "; img-src 'self' blob: data:; media-src 'self' blob:; manifest-src 'self'; worker-src 'self'; base-uri 'none'; form-action 'none'; frame-ancestors 'none'"))]
 where wsOrigin = (if "https:" `T.isPrefixOf` relayOrigin relay then "wss" else "ws") <> T.dropWhile (/=':') (relayOrigin relay)
relayResponse :: Relay -> Int -> Value -> Response
relayResponse relay code value = responseLBS (mkStatus code "") (relayHeaders relay "application/json") (encode value)
relayAsset :: Relay -> Request -> IO (Maybe Response)
relayAsset relay req = do
 let path = TE.decodeUtf8 (rawPathInfo req)
     platform = T.dropAround (=='/') path
     platforms = ["ios","android","mac","windows","linux"]
     entry = platform `elem` platforms || path == "/beta"
 found <- asset (relayRepo relay) (if entry then "/" else path)
 case found of
  Nothing -> pure Nothing
  Just (mime,content) -> do
   updated <- if mime == "text/html" then do
     let html = T.replace "<body class=" "<body data-remote class=" (TE.decodeUtf8 (BL.toStrict content))
     pure (BL.fromStrict (TE.encodeUtf8 (if platform `elem` platforms then T.replace "href=\"/manifest.webmanifest\"" ("href=\"/manifest.webmanifest?platform="<>platform<>"\"") html else html)))
    else if path == "/sw.js" then do
     assets <- mapM (asset (relayRepo relay)) (["/","/workspace/app.js"] ++ [p | (p,_,_) <- sortOn (\(p,_,_)->p) publicAssets])
     let version = T.take 16 (hash (BL.toStrict (BL.concat [raw | Just (_,raw) <- assets])))
     pure (BL.fromStrict (TE.encodeUtf8 (T.replace "sapiens-shell-v1" ("sapiens-shell-"<>version) (TE.decodeUtf8 (BL.toStrict content)))))
    else if path == "/manifest.webmanifest" then do
     let selected = lookup "platform" (queryString req) >>= id
     pure (case fmap TE.decodeUtf8 selected of Just p | p `elem` platforms -> encode (put "start_url" (str ("/"<>p)) (maybe Null id (decode content))); _ -> content)
    else pure content
   pure (Just (responseLBS status200 (relayHeaders relay mime) updated))
deliveryBatch :: S.Set Int -> Value -> S.Set Int -> (S.Set Int,Maybe Value)
deliveryBatch live result inflight
 | S.null kept && not (null rows) = (S.fromList (map (int . (`get` "id")) rows),Just result)
 | otherwise = (kept,Nothing)
 where
 kept=S.intersection inflight live
 rows=items result "messages"

socketApp :: Relay -> WS.ServerApp
socketApp relay pending = do
 count <- modifyMVar (relayCount relay) (\n -> pure (n+1,n))
 if count >= 128 then modifyMVar_ (relayCount relay) (pure . subtract 1) >> WS.rejectRequest pending "Connection limit reached" else
  bracket (WS.acceptRequest pending) (\connection -> do
   WS.sendClose connection (""::Text) `catch` \(_::SomeException) -> pure ()
   modifyMVar_ (relayCount relay) (pure . subtract 1)) $ \conn ->
   (do
    auth <- timeout 10000000 (readFrame conn) >>= maybe (failure 401 "Invalid mailbox authentication") pure
    let room = at auth "room"; bearer = at auth "token"; key = (room,hashText bearer)
        path = ["api","rooms",room,"messages"]
    require (at auth "type" == "auth" && T.length room >=32 && T.length room <=64 && T.all (\c->isAscii c && (isAlphaNum c || c `elem` ("_-"::String))) room && T.length bearer>=32 && T.length bearer<=256 && T.all (\c->ord c>=33 && ord c<=126) bearer) 401 "Invalid mailbox authentication"
    void (transact (relayDB relay) "GET" path bearer empty (relayAdmin relay) False)
    identity <- newId
    old <- modifyMVar (relayPeers relay) (\peers -> pure (M.insert key (identity,conn) peers,M.lookup key peers))
    forM_ old (\(_,previous) -> void (async (WS.sendCloseCode previous 4002 ("Connected in another tab"::Text))))
    WS.sendTextData conn (encode (obj [("type",str "ready")]))
    inflight <- newMVar S.empty
    sendGuard <- newMVar ()
    let send = withMVar sendGuard . const . WS.sendTextData conn . encode
        current = withMVar (relayPeers relay) (pure . maybe False ((==identity).fst) . M.lookup key)
        deliver = forever $ do
         active <- current
         unless active (failure 401 "Connected in another tab")
         result <- transact (relayDB relay) "GET" path bearer empty (relayAdmin relay) False
         live <- withDB (relayDB relay) (\db -> query db "SELECT id FROM messages WHERE room=?" (Only room)) :: IO [Only Int]
         batch <- modifyMVar inflight (pure . deliveryBatch (S.fromList [i | Only i<-live]) result)
         forM_ batch (send . put "type" (str "messages"))
         threadDelay 100000
        receive = forever $ do
         value <- readFrame conn
         active <- current
         unless active (failure 401 "Connected in another tab")
         let ident = at value "id"
         require (not (T.null ident) && T.length ident<=64) 400 "Invalid request ID"
         result <- try $ case at value "type" of
          "send" -> do
           require (BL.length (encode (get value "frame")) <= fromIntegral maxFrame) 413 "Frame too large"
           transact (relayDB relay) "POST" path bearer (get value "frame") (relayAdmin relay) False
          "ack" -> do
           require (int (get value "message")>0) 400 "Unsupported socket operation"
           reply <- transact (relayDB relay) "DELETE" (path++[T.pack (show (int (get value "message")))]) bearer empty (relayAdmin relay) False
           modifyMVar_ inflight (pure . S.delete (int (get value "message")))
           pure reply
          _ -> failure 400 "Unsupported socket operation"
         send (obj ([("type",str "result"),("id",str ident)] ++ case result of Right v -> [("result",v)]; Left (APIError _ msg) -> [("error",str msg)]))
    -- Send the close frame before cancelling a Windows thread blocked in receive.
    let delivery = deliver `catch` \(e::APIError) -> do
          WS.sendCloseCode conn 4001 ("Invalid or expired mailbox request"::Text)
          throwIO e
    (WS.withPingThread conn 25 (pure ()) (race_ delivery receive)) `finally`
      modifyMVar_ (relayPeers relay) (\peers -> pure (case M.lookup key peers of Just (i,_) | i==identity -> M.delete key peers; _ -> peers)))
   `catch` \(_::SomeException) -> WS.sendCloseCode conn 4001 ("Invalid or expired mailbox request"::Text) `catch` \(_::SomeException) -> pure ()
