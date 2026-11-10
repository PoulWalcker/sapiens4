module Bridge where

import Types
import qualified Service as Service
import Crypto
import Receipt (legacyJSON)
import Setup
import Transport
import Mailboxes (withDB)
import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception
import Control.Monad
import Data.Aeson hiding (pairs)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Char
import Data.IORef
import Data.List (sortOn)
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Text (Text)
import Database.SQLite.Simple
import System.Directory
import System.FilePath


data Bridge = Bridge { bridgeService :: Service, bridgeConfig :: MVar Value, bridgeError :: IORef Text, bridgeContact :: IORef Value, bridgeConnections :: IORef (S.Set Text), bridgeThread :: MVar (Maybe (Async ())) }
configPath :: Bridge -> FilePath
configPath b = serviceRoot (bridgeService b) </> "remote-device.json"
relayPath :: Bridge -> FilePath
relayPath b = serviceRoot (bridgeService b) </> "remote-relay.json"
expiry :: Value -> Double
expiry (Number n) = realToFrac n
expiry _ = 0
newBridge :: Service -> IO Bridge
newBridge service = do
 config <- newMVar Null
 b <- Bridge service config <$> newIORef "" <*> newIORef Null <*> newIORef S.empty <*> newMVar Nothing
 loaded <- try $ do
  exists <- doesFileExist (configPath b)
  saved <- readJSON (configPath b) Null
  if not exists then pure Null else do
   identity <- Service.corporaIdentity service
   require (get saved "corpora_id" == Null || get saved "corpora_id" == get identity "id") 400 "Pairing belongs to another CORPORA"
   void (origin (get saved "relay"))
   mapM_ (\k -> void (unb64 (get saved k) (Just 32))) ["private","public"]
   when (get saved "peer" /= Null) (void (unb64 (get saved "peer") (Just 32)))
   require (not (T.null (at saved "room")) && not (T.null (at saved "token")) && expiry (get saved "expires") > 0) 400 "Invalid saved pairing"
   let checked = put "corpora_id" (get identity "id") saved
   saveConfig b checked
   pure checked
 case loaded of
  Right v -> modifyMVar_ config (const (pure v)) >> writeIORef (bridgeConnections b) (S.singleton (at v "last_connection_id"))
  Left (errorValue::SomeException) -> rethrowAsync errorValue >> writeIORef (bridgeError b) "Saved remote pairing is invalid. Create a new pairing; local CORPORA is unaffected."
 worker <- async (runBridge b)
 modifyMVar_ (bridgeThread b) (const (pure (Just worker)))
 pure b
saveConfig :: Bridge -> Value -> IO ()
saveConfig b Null = removeFile (configPath b) `catch` \(e::IOException) -> do
 exists <- doesFileExist (configPath b)
 when exists (throwIO e)
saveConfig b v = writeJSON (configPath b) v
savedRelay :: Bridge -> IO Value
savedRelay b = readConnection (relayPath b) `catch` \(errorValue::SomeException) -> rethrowAsync errorValue >> pure empty
bridgeStatus :: Bridge -> IO Value
bridgeStatus b = withMVar (bridgeConfig b) $ \config -> do
 identity <- Service.corporaIdentity (bridgeService b)
 address <- publicId (at identity "id")
 time <- seconds
 local <- localConnection (repositoryRoot (bridgeService b))
 relay <- savedRelay b
 errorText <- readIORef (bridgeError b)
 contact <- readIORef (bridgeContact b)
 finger <- if get config "peer" /= Null then str <$> fingerprint (get config "peer") else pure Null
 let pending = get config "pending"; live = expiry (get config "expires") > time
 pendingInfo <- if pending /= Null && live then do
  fp <- fingerprint (get pending "peer")
  code <- confirmationCode (get config "public") (get pending "peer") (at config "room")
  pure (obj [("fingerprint",str fp),("peer",get pending "peer"),("confirmation_code",str code)])
  else pure Null
 pure (obj [("available",Bool True),("enabled",Bool (config/=Null)),("corpora",identity),("public_id",str address),
  ("invitation",if get config "peer"==Null && live then get config "invitation" else Null),
  ("local_relay",Bool (local/=empty)),("configured_relay",get relay "relay"),("paired",Bool (get config "peer"/=Null)),
  ("relay",get config "relay"),("paired_at",get config "paired_at"),("last_connected_at",get config "last_connected_at"),
  ("device_info",fallback (get config "device_info") empty),("fingerprint",finger),("pending",pendingInfo),
  ("expires",get config "expires"),("error",str errorText),("last_contact",contact)])
createPairing :: Bridge -> Value -> IO Value
createPairing b input = modifyMVar (bridgeConfig b) $ \current -> do
 require (current==Null) 409 "Revoke the existing pairing before creating another"
 selected <- if boolean (get input "local") then localConnection (repositoryRoot (bridgeService b)) else if boolean (get input "saved") then savedRelay b else pure input
 connection <- validateConnection (put "format" (str connectionFormat) selected)
 (private,public) <- keypair
 identity <- Service.corporaIdentity (bridgeService b)
 address <- publicId (at identity "id")
 let relay = at connection "relay"
 room <- request relay (if boolean (get connection "beta") then "/beta/api/rooms" else "/api/rooms") (at connection "token") "POST" (Just (obj [("public_id",str address),("corpora_id",get identity "id"),("public_key",str public)]))
 forM_ ["room","desktop","phone"] $ \k -> do
  let value = at room k
  require (T.length value>=32 && T.length value<=64 && T.all (\c -> isAscii c && (isAlphaNum c || c `elem` ("_-"::String))) value) 400 "Invalid relay mailbox"
 secret <- token
 expires <- (+fromIntegral pairingTTL) <$> seconds
 let invitation = obj [("v",str protocol),("relay",str relay),("room",get room "room"),("token",get room "phone"),("desktop",str public),("secret",str secret),("expires",toJSON expires),("public_id",str address)]
     config = obj [("relay",str relay),("room",get room "room"),("token",get room "desktop"),("corpora_id",get identity "id"),("private",str private),("public",str public),("secret",str secret),("expires",toJSON expires),("peer",Null),("pending",Null),("invitation",invitation)]
 saveConfig b config
 writeJSON (relayPath b) connection
 writeIORef (bridgeError b) ""
 pure (config,obj [("qr",invitation)])
replyFrame :: Value -> Text -> Value -> IO Value
replyFrame config ident value = do
 expires <- (+86390) <$> seconds
 pack (get config "private") (get config "peer") (at config "room") DesktopToPhone (obj [("id",str ident),("expires",toJSON expires),("kind",str "response"),("result",value)])
data PairingApproval = PairingApproval { approvedConfig :: Value, approvalRequest :: Text }
planApproval :: Double -> Value -> Value -> Validation PairingApproval
planApproval time input config = do
 let pending = get config "pending"
 check (pending/=Null && expiry (get config "expires") > time && get input "peer" == get pending "peer") 409 "Pairing expired or phone changed; generate a new QR"
 let paired = merge (remove ["secret","invitation"] config) (obj [("peer",get pending "peer"),("paired_at",toJSON time),("device_info",fallback (get pending "device_info") empty),("pending",Null)])
 pure (PairingApproval paired (at pending "id"))

pairingProof :: Double -> Bool -> Value -> Value -> Value -> Validation Value
pairingProof time proof config peer message = do
 check (at message "kind"=="pair" && expiry (get config "expires")>time && has message "secret" && proof) 400 "Invalid pairing proof"
 let pending = get config "pending"
 check (pending==Null || get pending "peer"==peer) 400 "Another phone is awaiting approval"
 pure (put "pending" (obj [("peer",peer),("id",get message "id"),("device_info",cleanDeviceInfo (get message "device_info"))]) config)

approvePairing :: Bridge -> Value -> IO Value
approvePairing b input = modifyMVar (bridgeConfig b) $ \config -> do
 time <- seconds
 approval <- validated (planApproval time input config)
 void (request (at config "relay") ("/api/rooms/"<>at config "room"<>"/trust") (at config "token") "POST" (Just empty))
 identity <- Service.corporaIdentity (bridgeService b)
 let paired = approvedConfig approval
 accept <- replyFrame paired (approvalRequest approval) (obj [("paired",Bool True),("corpora",identity)])
 let updated = put "accept" accept paired
 saveConfig b updated
 pure (updated,obj [("approved",Bool True)])
deleteRoom :: Value -> IO Bool
deleteRoom config = (request (at config "relay") ("/api/rooms/"<>at config "room") (at config "token") "DELETE" Nothing >> pure True)
 `catches` [Handler (\(APIError code _) -> pure (code `elem` [401,404])),Handler (\(errorValue::SomeException) -> rethrowAsync errorValue >> pure False)]
revokeBridge :: Bridge -> Bool -> IO Value
revokeBridge b notify = do
 previous <- modifyMVar (bridgeConfig b) $ \config -> saveConfig b Null >> pure (Null,config)
 writeIORef (bridgeError b) ""
 writeIORef (bridgeContact b) Null
 deleted <- if previous==Null then pure (Bool True) else if notify then Bool <$> deleteRoom previous else pure Null
 pure (obj [("revoked",Bool True),("relay_deleted",deleted)])
closeBridge :: Bridge -> IO ()
closeBridge b = readMVar (bridgeThread b) >>= mapM_ cancel
cleanDeviceInfo :: Value -> Value
cleanDeviceInfo value = obj [(k,str (T.take 200 (T.strip (T.filter isPrint s)))) | k <- ["name","platform","client"], String s <- [get value k]]
connectionUpdate :: Double -> S.Set Text -> Value -> Value -> (Bool,Text,Value)
connectionUpdate time connections config message = (changed,connection,final)
 where
 raw = at message "connection_id"
 connection = if T.length raw <=100 then raw else ""
 info = cleanDeviceInfo (get message "device_info")
 changed = get config "last_connected_at"==Null || (not (T.null connection) && S.notMember connection connections) || (T.null connection && at message "op"=="resume")
 updated = if changed then merge config (obj [("last_connected_at",toJSON time),("last_connection_id",if T.null connection then Null else str connection)]) else config
 final = if info/=empty then put "device_info" info updated else updated

recordConnection :: Bridge -> Value -> Value -> IO Value
recordConnection b config message = do
 connections <- readIORef (bridgeConnections b)
 time <- seconds
 let (changed,connection,final)=connectionUpdate time connections config message
 when changed (writeIORef (bridgeConnections b) (if S.size connections >128 then S.singleton connection else S.insert connection connections))
 when (final/=config) (saveConfig b final)
 pure final
data RemoteRequest = RemoteRequest Text Text (Maybe Value)
remoteRequest :: Value -> Validation RemoteRequest
remoteRequest message = do
 let method = at message "method"; target = at message "path"; path = T.takeWhile (/='?') target; parts = T.splitOn "/" (T.dropAround (=='/') path)
 check (at message "op"=="api") 409 "Remote operation not allowed"
 check (method `elem` ["GET","POST","PUT"] && T.length target<=8192) 409 "Unsupported remote API request"
 check ("/api/" `T.isPrefixOf` target && not (T.any (=='%') path) && not (T.any (`elem` ("\\#"::String)) target) && ".." `notElem` parts && take 2 parts `notElem` [["api","remote"],["api","host"]] && not (method/="GET" && length parts==4 && last parts=="browser")) 409 "Remote API route is not allowed"
 pure (RemoteRequest method target (if get message "data"==Null then Nothing else Just (get message "data")))

executeRemote :: Bridge -> Value -> IO Value
executeRemote b message = do
 RemoteRequest method target input <- validated (remoteRequest message)
 (status,body,mime) <- Service.dispatch (bridgeService b) method target input
  `catch` \(APIError status messageText) -> pure (status,encode (obj [("error",str messageText)]),"application/json")
 let isJSON = "application/json" `T.isPrefixOf` mime
 pure (obj [("status",num status),("body",if isJSON then fromMaybeValue (decode body) else str (b64 (BL.toStrict body))),("mime",str mime),("binary",Bool (not isJSON))])
 where fromMaybeValue = maybe Null id
executeReply :: Bridge -> Value -> Value -> IO Value
executeReply b config message = do
 value <- executeRemote b message `catches` [Handler (\(APIError _ err) -> pure (obj [("error",str err)])), Handler (\(errorValue::SomeException) -> rethrowAsync errorValue >> pure (obj [("error",str "Local operation failed; check desktop before sending again")]))]
 replyFrame config (at message "id") value `catch` \(_::APIError) -> replyFrame config (at message "id") (obj [("error",str "Response too large; inspect on desktop")])
processFrame :: Bridge -> Value -> Value -> IO (Value,Maybe Value)
processFrame b config frame = do
 let peer = fallback (get config "peer") (get frame "sender")
 (message,plaintext) <- unpackWithPlaintext (get config "private") peer (at config "room") PhoneToDesktop frame
 time <- seconds
 if get config "peer"==Null then do
  proof <- constantEqual (at message "secret") (at config "secret")
  updated <- validated (pairingProof time proof config peer message)
  saveConfig b updated
  pure (updated,Nothing)
 else do
  require (at message "kind"=="request" && expiry (get message "expires")<=time+310) 400 "Invalid request"
  identity <- Service.corporaIdentity (bridgeService b)
  if fallback (get message "corpora_id") (get identity "id") /= get identity "id" then (config,) . Just <$> replyFrame config (at message "id") (obj [("error",str "This device is connected to a different CORPORA. Pair again.")]) else do
   current <- recordConnection b config message
   reply <- if at message "op"=="resume" then replyFrame current (at message "id") (obj [("paired",Bool True),("corpora",identity)])
    else if at message "op"=="api" && at message "method"=="GET" then executeReply b current message
    else do
     let path = serviceRoot (bridgeService b) </> "remote-receipts.sqlite3"
         currentDigest = hash (BL.toStrict (encode (obj (sortOn fst (pairs message)))))
         -- New receipts and matching pre-v2 canonical receipts never need legacyJSON.
         -- Limit the slow Python-era fallback to 4 KiB during the one-day migration
         -- window. Oversized mismatches are rejected without parsing or re-execution.
         stored = "v2:" <> currentDigest
         legacyDigest = either (const currentDigest) hash (legacyJSON plaintext)
         matches saved = if "v2:" `T.isPrefixOf` saved then saved == stored
          else saved == currentDigest || (BS.length plaintext <= 4096 && saved == legacyDigest)
         room = at current "room"; ident = at message "id"
     receipt <- withDB path $ \db -> withTransaction db $ do
      execute_ db "CREATE TABLE IF NOT EXISTS receipts (room TEXT,id TEXT,hash TEXT,reply TEXT,created REAL,PRIMARY KEY(room,id))"
      execute db "DELETE FROM receipts WHERE created < ?" (Only (time-86400))
      rows <- query db "SELECT hash,reply FROM receipts WHERE room=? AND id=?" (room,ident) :: IO [(Text,Maybe Text)]
      when (null rows) (execute db "INSERT INTO receipts VALUES (?,?,?,NULL,?)" (room,ident,stored,time))
      pure rows
     case receipt of
      (saved,raw):_ -> do
       require (matches saved) 400 "Request ID was reused"
       maybe (replyFrame current ident (obj [("error",str "Outcome uncertain after interruption. Check desktop history before sending again.")])) (pure . parseJSONText) raw
      [] -> do
       result <- executeReply b current message
       withDB path (\db -> execute db "UPDATE receipts SET reply=? WHERE room=? AND id=?" (jsonText result,room,ident))
       pure result
   pure (current,Just reply)
runBridge :: Bridge -> IO ()
runBridge b = loop 1
 where
 loop delay = do
  config <- readMVar (bridgeConfig b)
  if config==Null then threadDelay 500000 >> loop 1 else do
   result <- try $ bracket (openSocket config) closeSocket $ \client -> do
    writeIORef (bridgeError b) ""
    seconds >>= writeIORef (bridgeContact b) . toJSON
    let receive = do
         active <- modifyMVar (bridgeConfig b) $ \current -> if at current "room"/=at config "room" then pure (current,False) else do
          updated <- if get current "accept"/=Null then do
           void (socketCall client (SendFrame (get current "accept")))
           let next = remove ["accept"] current
           saveConfig b next
           pure next
           else pure current
          pure (updated,True)
         when active $ do
          item <- socketReceive client
          forM_ item $ \incoming -> modifyMVar_ (bridgeConfig b) $ \current ->
           if at current "room"/=at config "room" then pure current else do
            result <- try (processFrame b current (get incoming "frame"))
            (updated,reply) <- case result of
             Right r -> pure r
             Left (errorValue::SomeException) -> rethrowAsync errorValue >> pure (current,Nothing)
            forM_ reply (\v -> void (socketCall client (SendFrame v)))
            void (socketCall client (AckMessage (get incoming "id")))
            seconds >>= writeIORef (bridgeContact b) . toJSON
            writeIORef (bridgeError b) ""
            pure updated
          receive
    receive
   case result of
    Right () -> loop 1
    Left (e::SomeException) -> do
     rethrowAsync e
     writeIORef (bridgeError b) "Relay disconnected. Reconnecting; local CORPORA remains available." >> threadDelay (delay*1000000) >> loop (min 30 (delay*2))

rethrowAsync :: SomeException -> IO ()
rethrowAsync errorValue = case fromException errorValue :: Maybe SomeAsyncException of
 Just _ -> throwIO errorValue
 Nothing -> case fromException errorValue :: Maybe AsyncCancelled of
  Just _ -> throwIO errorValue
  Nothing -> pure ()
