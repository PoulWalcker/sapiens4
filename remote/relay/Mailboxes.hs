module Mailboxes where

import Types
import Crypto
import qualified AppStorage as A
import Control.Monad
import Data.Aeson hiding (pairs)
import qualified Data.ByteString as B
import Data.List (sort,sortOn)
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Text.Encoding

rows :: A.Store -> Text -> IO [Value]
rows store key = map snd <$> A.recordsAt (if key=="messages" then 2 else 1) store (key<>"/")
row :: A.Store -> Text -> IO Value
row store key = maybe Null id <$> A.getValue store key
putRow :: A.Store -> Text -> Text -> Value -> IO ()
putRow store kind identity value = do
 let key = kind<>"/"<>identity
 previous <- row store key
 forM_ (expiryKey key previous) (A.deleteValue store)
 A.putValue store key value
 forM_ (expiryKey key value) (\index -> A.putValue store index (str key))
removeRow :: A.Store -> Text -> Text -> IO ()
removeRow store kind identity = do
 let key = kind<>"/"<>identity
 previous <- row store key
 forM_ (expiryKey key previous) (A.deleteValue store)
 A.deleteValue store key
expiry :: Value -> Double
expiry (Number n) = realToFrac n
expiry _ = 0
orderedNumber :: Integer -> Text
orderedNumber = T.justifyRight 20 '0' . T.pack . show
expiryKey :: Text -> Value -> Maybe Text
expiryKey key value
 | any (`T.isPrefixOf` key) ["rooms/","messages/"] && expiry (get value "expires") > 0 =
    Just ("expiry/"<>orderedNumber (ceiling (expiry (get value "expires")*1000))<>"/"<>key)
 | otherwise = Nothing
roomMessages :: A.Store -> Text -> IO [Value]
roomMessages store room = rows store ("messages/"<>room)
messageIdentity :: Text -> Value -> Text
messageIdentity room value = room<>"/"<>orderedNumber (fromIntegral (int value))
removeRoom :: A.Store -> Text -> IO ()
removeRoom store room = do
 roomMessages store room >>= mapM_ (removeRow store "messages" . messageIdentity room . (`get` "id"))
 removeRow store "rooms" room
expire :: A.Store -> Double -> IO ()
expire store time = do
 due <- A.rangeValues store "expiry/" ("expiry/"<>orderedNumber (floor (time*1000)+1)) 256
 forM_ due $ \(index,value) -> do
  let key = text value
  saved <- row store key
  if expiryKey key saved /= Just index then A.deleteValue store index
  else when (expiry (get saved "expires")<=time) $ do
   if "rooms/" `T.isPrefixOf` key then removeRoom store (T.drop 6 key)
   else removeRow store "messages" (T.drop 9 key)
   A.deleteValue store index
transact :: A.Store -> Text -> [Text] -> Text -> Value -> Text -> Bool -> IO Value
transact store method parts bearer body admin beta = A.transaction store $ do
 time <- seconds
 expire store time
 if parts==["api","rooms"] && method=="POST" then do
  authorized <- constantEqual bearer admin
  require (beta || authorized) 401 "Unauthorized"
  count <- length . filter (\v -> get v "expires"==Null || expiry (get v "expires")>time) <$> rows store "rooms"
  require (count<100) 429 "Room limit reached"
  when beta $ do
   recent <- filter ((>time-60) . expiry) . list <$> row store "beta"
   require (length recent<10) 429 "Beta is busy. Try again in a minute or use your own relay."
   A.putValue store "beta" (toJSON (toJSON time:recent))
  let address=get body "public_id"
  require (not beta || address/=Null) 400 "CORPORA identity required"
  when (address/=Null) $ do
   expected <- publicId (at body "corpora_id")
   require (address==str expected) 400 "Invalid CORPORA ID"
   void (unb64 (get body "public_key") (Just 32))
   existing <- row store ("corpora/"<>text address)
   live <- row store ("rooms/"<>at existing "room")
   require (not beta || live==Null || (get live "expires"/=Null && expiry (get live "expires")<=time)) 409 "Revoke the existing pairing before creating another"
   require (existing==Null || at existing "identity"==at body "corpora_id") 409 "CORPORA ID collision"
  room <- token
  desktop <- token
  phone <- token
  putRow store "rooms" room (obj [("id",str room),("desktop",str (hashText desktop)),("phone",str (hashText phone)),("expires",toJSON (time+604800))])
  when (address/=Null) $ putRow store "corpora" (text address) (obj [("identity",get body "corpora_id"),("room",str room),("public_key",get body "public_key"),("expires",toJSON (time+fromIntegral pairingTTL))])
  pure (obj [("room",str room),("desktop",str desktop),("phone",str phone)])
 else do
  require (length parts>=3 && take 2 parts==["api","rooms"]) 404 "Not found"
  let room=parts!!2
  saved <- row store ("rooms/"<>room)
  require (saved/=Null && (get saved "expires"==Null || expiry (get saved "expires")>time)) 401 "Unknown or expired mailbox"
  desktop <- constantEqual (hashText bearer) (at saved "desktop")
  phone <- constantEqual (hashText bearer) (at saved "phone")
  require (desktop || phone) 401 "Unknown or expired mailbox"
  let role=if desktop then "desktop" else "phone"
      recipient=if desktop then "phone" else "desktop"
      updateAddress = A.values store "corpora/" >>= mapM_ (\(key,v) -> when (at v "room"==room) (A.putValue store key (put "expires" (num 0) v)))
  messages <- filter ((>time) . expiry . (`get` "expires")) <$> roomMessages store room
  case (drop 3 parts,method) of
   (["trust"],"POST") | desktop -> do
    putRow store "rooms" room (put "expires" Null saved)
    updateAddress
    pure (obj [("trusted",Bool True)])
   ([],"DELETE") | desktop -> do
    removeRoom store room
    updateAddress
    pure (obj [("deleted",Bool True)])
   (["messages"],"GET") -> pure (obj [("messages",arr (mailboxBatch (take 20 (sortOn (int . (`get` "id")) (filter ((==role) . (`at` "recipient")) messages)))))])
   (["messages"],"POST") -> do
    require (sort (keys body)==["ciphertext","nonce","sender"]) 400 "Expected an encrypted frame"
    void (unb64 (get body "sender") (Just 32))
    void (unb64 (get body "nonce") (Just 24))
    cipher <- unb64 (get body "ciphertext") Nothing
    require (B.length cipher>=16) 400 "Invalid ciphertext"
    require (length messages<100 && sum (map (T.length . jsonText . (`get` "frame")) messages)+T.length (jsonText body)<=48000000) 429 "Mailbox full"
    identity <- (+1) . int <$> row store "sequence"
    A.putValue store "sequence" (num identity)
    putRow store "messages" (messageIdentity room (num identity)) (obj [("id",num identity),("room",str room),("recipient",str recipient),("frame",body),("expires",toJSON (time+86400))])
    pure (obj [("id",num identity)])
   (["messages",identity],"DELETE") -> do
    require (not (T.null identity) && T.length identity<=20 && T.all (\c -> c>='0' && c<='9') identity) 400 "Invalid message ID"
    let key = room<>"/"<>T.justifyRight 20 '0' identity
    message <- row store ("messages/"<>key)
    when (at message "room"==room && at message "recipient"==role) (removeRow store "messages" key)
    pure (obj [("deleted",Bool True)])
   _ -> failure 404 "Not found"
mailboxBatch :: [Value] -> [Value]
mailboxBatch = bounded 0 where
 bounded _ [] = []
 bounded size (v:rest)
  | size>0 && size+width>maxFrame = []
  | otherwise = selectPresent ["id","frame"] v : bounded (size+width) rest
  where width=T.length (jsonText (get v "frame"))
hashText :: Text -> Text
hashText = hash . Data.Text.Encoding.encodeUtf8
lookupAddress :: A.Store -> Text -> IO Value
lookupAddress store address = A.transaction store $ do
 let suffix=T.drop 6 address
 require (T.isPrefixOf "sapi4_" address && T.length suffix==9 && T.all (\c -> c>='a' && c<='z') (T.take 3 suffix<>T.drop 6 suffix) && T.all (\c -> c>='0' && c<='9') (T.take 3 (T.drop 3 suffix))) 400 "Use an ID like sapi4_abc123qwe."
 time <- seconds
 saved <- row store ("corpora/"<>address)
 room <- row store ("rooms/"<>at saved "room")
 require (room/=Null && expiry (get saved "expires")>time) 404 "No active QR. Open Connect phone on your desktop."
 pure (obj [("public_id",str address),("desktop",get saved "public_key"),("expires",get saved "expires")])
