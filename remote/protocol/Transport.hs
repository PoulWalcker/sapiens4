module Transport where

import Types
import Crypto
import Control.Concurrent.Async
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Data.Aeson hiding (pairs)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.IORef
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Text (Text)
import qualified Network.HTTP.Client as H
import Network.HTTP.Client.TLS
import Network.HTTP.Types.Status
import qualified Network.WebSockets as WS
import qualified Wuss
import System.Timeout

request :: Text -> Text -> Text -> Text -> Maybe Value -> IO Value
request relay path bearer method body = do
 manager <- H.newManager tlsManagerSettings
 req <- H.parseRequest (T.unpack (relay <> path))
 let configured = req {H.method=TE.encodeUtf8 method,H.redirectCount=0,H.responseTimeout=H.responseTimeoutMicro 5000000,
                       H.requestHeaders=[("Authorization",TE.encodeUtf8 ("Bearer "<>bearer)),("Content-Type","application/json")],
                       H.requestBody=maybe (H.RequestBodyBS B.empty) (H.RequestBodyLBS . encode) body,H.checkResponse = \_ _ -> pure ()}
 H.withResponse configured manager $ \response -> do
  let status = statusCode (H.responseStatus response)
  require (status >= 200 && status < 300) status ("Relay request failed (HTTP " <> T.pack (show status) <> ")")
  let collect size chunks = do
       chunk <- H.brRead (H.responseBody response)
       require (size+B.length chunk <= 24000000) 400 "Relay response too large"
       if B.null chunk then pure (B.concat (reverse chunks)) else collect (size+B.length chunk) (chunk:chunks)
  raw <- collect 0 []
  case eitherDecodeStrict raw of Right v@(Object _) -> pure v; _ -> failure 400 "Invalid relay response"

data SocketClient = SocketClient { socketConnection :: WS.Connection, socketReader :: Async (), socketMessages :: TBQueue Value, socketResults :: TQueue Value, socketEnded :: TMVar SomeException, socketSerial :: IORef Int, socketGuard :: MVar () }

openSocket :: Value -> IO SocketClient
openSocket config = do
 relay <- parseRelayOrigin (get config "relay")
 let secure = relaySecure relay
     port = relayPort relay
     host = relayHost relay
     headers = [("Origin",TE.encodeUtf8 (relayURL relay))]
     options = WS.defaultConnectionOptions {WS.connectionMessageDataSizeLimit=WS.SizeLimit (fromIntegral (maxFrame+8192))}
 ready <- newEmptyMVar
 messages <- newTBQueueIO 40
 results <- newTQueueIO
 ended <- newEmptyTMVarIO
 serial <- newIORef 0
 guard <- newMVar ()
 let application conn = do
      WS.sendTextData conn (encode (obj [("type",str "auth"),("room",get config "room"),("token",get config "token")]))
      response <- readFrame conn
      require (at response "type" == "ready") 400 "Mailbox authentication failed"
      putMVar ready (Right conn)
      forever $ do
       frame <- readFrame conn
       if at frame "type" == "messages" then do
        let xs = items frame "messages"
        require (length xs <= 20) 400 "Invalid relay delivery"
        atomically $ do
         count <- lengthTBQueue messages
         when (fromIntegral (length xs) + count > 40) (throwSTM (APIError 400 "Relay delivery overflow"))
         mapM_ (writeTBQueue messages) xs
       else atomically (writeTQueue results frame)
     connect = if secure then Wuss.runSecureClientWith host (fromIntegral port) "/api/socket" options headers application
                         else WS.runClientWith host port "/api/socket" options headers application
 worker <- async (connect `catch` \(e::SomeException) -> void (tryPutMVar ready (Left e)) >> atomically (void (tryPutTMVar ended e)))
 result <- timeout 6000000 (takeMVar ready)
 case result of
  Just (Right conn) -> pure (SocketClient conn worker messages results ended serial guard)
  Just (Left e) -> cancel worker >> throwIO e
  Nothing -> cancel worker >> failure 503 "Mailbox connection timed out"
readFrame :: WS.Connection -> IO Value
readFrame conn = do
 raw <- WS.receiveData conn :: IO BL.ByteString
 require (BL.length raw <= fromIntegral (maxFrame+8192)) 400 "Invalid relay socket message"
 case eitherDecode raw of Right v@(Object _) -> pure v; _ -> failure 400 "Invalid relay socket response"
closeSocket :: SocketClient -> IO ()
closeSocket client = do
 void (timeout 1000000 (WS.sendClose (socketConnection client) (""::Text))) `catch` \(_::SomeException) -> pure ()
 cancel (socketReader client)
data SocketCommand = SendFrame Value | AckMessage Value
socketCall :: SocketClient -> SocketCommand -> IO Value
socketCall client command = withMVar (socketGuard client) $ \_ -> do
 n <- atomicModifyIORef' (socketSerial client) (\i -> (i+1,i+1))
 let ident = T.pack (show n)
 WS.sendTextData (socketConnection client) (encode (socketCommand ident command))
 result <- timeout 10000000 (atomically ((readTQueue (socketResults client)) `orElse` (readTMVar (socketEnded client) >>= throwSTM)))
 case result of
  Nothing -> failure 503 "Relay did not confirm delivery"
  Just value -> validated (socketResult ident value)

socketCommand :: Text -> SocketCommand -> Value
socketCommand identity command = merge (obj [("type",str kind),("id",str identity)]) body
 where
 (kind,body)=case command of
  SendFrame frame -> ("send",obj [("frame",frame)])
  AckMessage message -> ("ack",obj [("message",message)])

socketResult :: Text -> Value -> Validation Value
socketResult identity value =
 Types.check (at value "type"=="result" && at value "id"==identity) 400 "Unexpected relay response" *>
 Types.check (T.null (at value "error")) 400 (at value "error") *> Right (get value "result")

socketReceive :: SocketClient -> IO (Maybe Value)
socketReceive client = timeout 500000 (atomically (readTBQueue (socketMessages client) `orElse` (readTMVar (socketEnded client) >>= throwSTM)))
