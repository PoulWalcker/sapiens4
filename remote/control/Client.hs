module Client where

import Types
import Crypto
import Transport
import Control.Concurrent.MVar
import Control.Concurrent.Async (AsyncCancelled)
import Control.Exception
import Control.Monad
import Data.Aeson hiding (pairs)
import qualified Data.ByteString.Lazy as BL
import Data.Char
import Data.IORef
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.UUID as UUID
import System.Info
import System.Environment


data ClientTransport = Disconnected | Connected Text SocketClient
data Client = Client { clientConfig :: MVar Value, clientSocket :: MVar ClientTransport, clientGuard :: MVar (), clientRevoked :: IORef Bool }
newClient :: Value -> IO Client
newClient config = Client <$> newMVar config <*> newMVar Disconnected <*> newMVar () <*> newIORef False
closeClient :: Client -> IO ()
closeClient c = modifyMVar_ (clientSocket c) $ \transport -> do
 case transport of Disconnected -> pure (); Connected _ socket -> closeSocket socket
 pure Disconnected
revokeClient :: Client -> IO ()
revokeClient c = writeIORef (clientRevoked c) True >> closeClient c
exchange :: Client -> Value -> Double -> IO Value
exchange c payload duration = withMVar (clientGuard c) $ \_ -> do
 revoked <- readIORef (clientRevoked c)
 require (not revoked) 410 "Remote connection was revoked"
 config <- readMVar (clientConfig c)
 currentSocket <- readMVar (clientSocket c)
 connection <- case currentSocket of
  Disconnected -> newId
  Connected identity _ -> pure identity
 ident <- newId
 time <- seconds
 hostname <- maybe "" T.pack <$> lookupEnv "HOSTNAME"
 let body = merge (obj [("id",str ident),("expires",toJSON (time+duration)),("corpora_id",get (get config "corpora") "id"),("connection_id",str connection),
             ("device_info",obj [("name",str hostname),("platform",str (T.pack os)),("client",str "Sapiens4 desktop")])]) payload
 frame <- pack (get config "private") (get config "desktop") (at config "room") PhoneToDesktop body
 let transport action = action `catch` \(APIError _ message) -> ioError (userError $ T.unpack message)
     unreachable (errorValue::SomeException) = case fromException errorValue :: Maybe SomeAsyncException of
      Just _ -> throwIO errorValue
      Nothing -> case fromException errorValue :: Maybe AsyncCancelled of
       Just _ -> throwIO errorValue
       Nothing -> closeClient c >> failure 503 "This device is unreachable. Try again later."
 (do
  socket <- transport $ modifyMVar (clientSocket c) $ \existing -> case existing of
   Connected _ socket -> pure (existing,socket)
   Disconnected -> do
    socket <- openSocket config
    pure (Connected connection socket,socket)
  nowRevoked <- readIORef (clientRevoked c)
  when nowRevoked (closeClient c >> failure 410 "Remote connection was revoked")
  void $ transport $ socketCall socket (SendFrame frame)
  let wait = do
       nowTime <- seconds
       unless (nowTime < time+duration) $ ioError (userError "Host did not respond")
       incoming <- transport (socketReceive socket)
       case incoming of
        Nothing -> wait
        Just item -> do
         decoded <- try (unpack (get config "private") (get config "desktop") (at config "room") DesktopToPhone (get item "frame"))
         void $ transport $ socketCall socket (AckMessage (get item "id"))
         case responseDecision ident decoded of
          IgnoreResponse -> wait
          AcceptResponse result -> pure result
          RejectResponse reason -> failure 409 reason
  wait) `catches` [Handler (\(e::APIError) -> throwIO e),Handler unreachable]
data ResponseDecision = IgnoreResponse | AcceptResponse Value | RejectResponse Text
responseDecision :: Text -> Either APIError Value -> ResponseDecision
responseDecision identity decoded = case decoded of
 Right response | at response "id"==identity && at response "kind"=="response" ->
  let result = get response "result"; reason = at result "error"
  in if T.null reason then AcceptResponse result else RejectResponse reason
 _ -> IgnoreResponse

clientRequest :: Client -> Text -> Text -> Maybe Value -> IO (Int,BL.ByteString,Text)
clientRequest c method path body = do
 result <- exchange c (obj [("kind",str "request"),("op",str "api"),("method",str method),("path",str path),("data",maybe Null id body)]) 12
 raw <- if boolean (get result "binary") then BL.fromStrict <$> unb64 (get result "body") Nothing else pure (encode (get result "body"))
 pure (int (get result "status"),raw,at result "mime")
validateInvitation :: Double -> Value -> Validation Text
validateInvitation time value = do
 check (at value "v"==protocol) 400 "Invalid pairing invitation"
 relay <- relayURL <$> decodeRelayOrigin (get value "relay")
 forM_ ["room","token","secret"] $ \field -> do
  let input = at value field
  check (T.length input>=32 && T.length input<=64 && T.all (\c -> isAscii c && (isAlphaNum c || c `elem` ("_-"::String))) input) 400 "Invalid pairing invitation"
 let expiry = case get value "expires" of Number n -> realToFrac n; _ -> 0
 check (time<expiry && expiry<=time+fromIntegral pairingTTL+60) 400 "Invitation expired. Show a new QR on the host."
 void (decodeBase64 (get value "desktop") (Just 32))
 pure relay

invitation :: Value -> IO Client
invitation value = do
 time <- seconds
 relay <- validated (validateInvitation time value)
 (private,public) <- keypair
 newClient (obj [("relay",str relay),("room",get value "room"),("token",get value "token"),("desktop",get value "desktop"),("private",str private),("public",str public)])
pairClient :: Client -> Value -> IO Value
pairClient c value = do
 time <- seconds
 let expiry = case get value "expires" of Number n -> realToFrac n; _ -> 0
 result <- exchange c (obj [("kind",str "pair"),("secret",get value "secret")]) (min 180 (expiry-time))
 let corpora = get result "corpora"; identity = at corpora "id"
 require (boolean (get result "paired") && maybe False ((==identity) . UUID.toText) (UUID.fromText identity)) 409 "Corpora identity could not be verified"
 modifyMVar_ (clientConfig c) (pure . put "corpora" corpora)
 pure corpora
