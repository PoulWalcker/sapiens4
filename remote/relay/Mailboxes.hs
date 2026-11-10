module Mailboxes where

import Types
import Crypto
import Control.Exception
import Control.Monad
import Data.Aeson hiding (pairs)
import qualified Data.ByteString as B
import Data.Char
import Data.Int
import Data.List (sort)
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Text.Encoding
import Database.SQLite.Simple
import System.Directory
import System.FilePath

data MailboxRole = Desktop | Phone deriving (Eq, Show)
roleName :: MailboxRole -> Text
roleName role = case role of Desktop -> "desktop"; Phone -> "phone"
recipient :: MailboxRole -> MailboxRole
recipient role = case role of Desktop -> Phone; Phone -> Desktop

openMailboxes :: FilePath -> IO ()
openMailboxes path = do
 createDirectoryIfMissing True (takeDirectory path)
 withDB path $ \db -> mapM_ (execute_ db)
  ["CREATE TABLE IF NOT EXISTS rooms (id TEXT PRIMARY KEY, desktop TEXT, phone TEXT, expires REAL)",
   "CREATE TABLE IF NOT EXISTS messages (id INTEGER PRIMARY KEY AUTOINCREMENT, room TEXT, recipient TEXT, body TEXT, expires REAL)",
   "CREATE INDEX IF NOT EXISTS inbox ON messages(room,recipient,id)",
   "CREATE TABLE IF NOT EXISTS beta_requests (created REAL NOT NULL)",
   "CREATE TABLE IF NOT EXISTS corpora (address TEXT PRIMARY KEY, identity TEXT NOT NULL, room TEXT NOT NULL, public_key TEXT NOT NULL, expires REAL NOT NULL)"]
withDB :: FilePath -> (Connection -> IO a) -> IO a
withDB path f = bracket (open path) close $ \db -> execute_ db "PRAGMA busy_timeout=10000" >> f db
expire :: Connection -> Double -> IO ()
expire db time = do
 execute db "DELETE FROM messages WHERE expires <= ? OR room IN (SELECT id FROM rooms WHERE expires <= ?)" (time,time)
 execute db "DELETE FROM rooms WHERE expires <= ?" (Only time)
transact :: FilePath -> Text -> [Text] -> Text -> Value -> Text -> Bool -> IO Value
transact path method parts bearer body admin beta = withDB path $ \db -> withImmediateTransaction db $ do
 time <- seconds
 expire db time
 if parts == ["api","rooms"] && method == "POST" then do
  authorized <- constantEqual bearer admin
  require (beta || authorized) 401 "Unauthorized"
  [Only count] <- query_ db "SELECT count(*) FROM rooms" :: IO [Only Int]
  require (count < 100) 429 "Room limit reached"
  when beta $ do
   execute db "DELETE FROM beta_requests WHERE created < ?" (Only (time-60))
   [Only n] <- query_ db "SELECT count(*) FROM beta_requests" :: IO [Only Int]
   require (n < 10) 429 "Beta is busy. Try again in a minute or use your own relay."
   execute db "INSERT INTO beta_requests VALUES (?)" (Only time)
  let address = get body "public_id"
  require (not beta || address /= Null) 400 "CORPORA identity required"
  when (address /= Null) $ do
   expected <- publicId (at body "corpora_id")
   require (address == str expected) 400 "Invalid CORPORA ID"
   void (unb64 (get body "public_key") (Just 32))
   existing <- query db "SELECT identity FROM corpora WHERE address=?" (Only (text address)) :: IO [Only Text]
   live <- query db "SELECT 1 FROM corpora c JOIN rooms r ON r.id=c.room WHERE address=?" (Only (text address)) :: IO [Only Int]
   require (not beta || null live) 409 "Revoke the existing pairing before creating another"
   require (null existing || existing == [Only (at body "corpora_id")]) 409 "CORPORA ID collision"
  room <- token
  desktop <- token
  phone <- token
  execute db "INSERT INTO rooms VALUES (?,?,?,?)" (room,hashText desktop,hashText phone,time+604800)
  when (address /= Null) (execute db "INSERT OR REPLACE INTO corpora VALUES (?,?,?,?,?)" (text address,at body "corpora_id",room,at body "public_key",time+fromIntegral pairingTTL))
  pure (obj [("room",str room),("desktop",str desktop),("phone",str phone)])
 else do
  require (length parts >= 3 && take 2 parts == ["api","rooms"]) 404 "Not found"
  let room = parts !! 2
  rows <- query db "SELECT desktop,phone FROM rooms WHERE id=?" (Only room) :: IO [(Text,Text)]
  role <- case rows of
   [(d,p)] -> do
    isDesktop <- constantEqual (hashText bearer) d
    isPhone <- constantEqual (hashText bearer) p
    if isDesktop then pure Desktop else if isPhone then pure Phone else failure 401 "Unknown or expired mailbox"
   _ -> failure 401 "Unknown or expired mailbox"
  case (drop 3 parts,method,role) of
   (["trust"],"POST",Desktop) -> do
    execute db "UPDATE rooms SET expires=NULL WHERE id=?" (Only room)
    execute db "UPDATE corpora SET expires=0 WHERE room=?" (Only room)
    pure (obj [("trusted",Bool True)])
   ([],"DELETE",Desktop) -> do
    execute db "DELETE FROM messages WHERE room=?" (Only room)
    execute db "DELETE FROM rooms WHERE id=?" (Only room)
    execute db "UPDATE corpora SET expires=0 WHERE room=?" (Only room)
    pure (obj [("deleted",Bool True)])
   (["messages"],"GET",_) -> do
    messages <- query db "SELECT id,body FROM messages WHERE room=? AND recipient=? ORDER BY id LIMIT 20" (room,roleName role) :: IO [(Int64,Text)]
    pure (obj [("messages",arr (mailboxBatch messages))])
   (["messages"],"POST",_) -> do
    require (sort (keys body) == ["ciphertext","nonce","sender"]) 400 "Expected an encrypted frame"
    void (unb64 (get body "sender") (Just 32))
    void (unb64 (get body "nonce") (Just 24))
    cipher <- unb64 (get body "ciphertext") Nothing
    require (B.length cipher >= 16) 400 "Invalid ciphertext"
    let raw = jsonText body
    [(count,size)] <- query db "SELECT count(*),coalesce(sum(length(body)),0) FROM messages WHERE room=?" (Only room) :: IO [(Int,Int)]
    require (count < 100 && size+T.length raw <= 48000000) 429 "Mailbox full"
    execute db "INSERT INTO messages(room,recipient,body,expires) VALUES(?,?,?,?)" (room,roleName (recipient role),raw,time+86400)
    ident <- lastInsertRowId db
    pure (obj [("id",toJSON ident)])
   (["messages",ident],"DELETE",_) -> do
    require (not (T.null ident) && T.all isDigit ident) 400 "Invalid message ID"
    execute db "DELETE FROM messages WHERE id=? AND room=? AND recipient=?" (ident,room,roleName role)
    pure (obj [("deleted",Bool True)])
   _ -> failure 404 "Not found"
mailboxBatch :: [(Int64,Text)] -> [Value]
mailboxBatch = bounded 0
 where
 bounded _ [] = []
 bounded size ((identity,raw):rest)
  | size>0 && size+T.length raw>maxFrame = []
  | otherwise = obj [("id",toJSON identity),("frame",parseJSONText raw)] : bounded (size+T.length raw) rest

hashText :: Text -> Text
hashText = hash . Data.Text.Encoding.encodeUtf8
lookupAddress :: FilePath -> Text -> IO Value
lookupAddress path address = do
 let suffix = T.drop 6 address
 require (T.isPrefixOf "sapi4_" address && T.length suffix == 9 && T.all (\c -> c >= 'a' && c <= 'z') (T.take 3 suffix<>T.drop 6 suffix) && T.all (\c -> c >= '0' && c <= '9') (T.take 3 (T.drop 3 suffix))) 400 "Use an ID like sapi4_abc123qwe."
 time <- seconds
 rows <- withDB path $ \db -> query db "SELECT c.public_key,c.expires FROM corpora c JOIN rooms r ON r.id=c.room WHERE c.address=? AND c.expires>?" (address,time) :: IO [(Text,Double)]
 case rows of
  [(public,expiry)] -> pure (obj [("public_id",str address),("desktop",str public),("expires",toJSON expiry)])
  _ -> failure 404 "No active QR. Open Connect phone on your desktop."
