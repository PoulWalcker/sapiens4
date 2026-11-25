{-# LANGUAGE CPP #-}
module Main where
import Control.Exception (bracket)
import Control.Monad
import Data.Aeson (Value(..), encode, toJSON)
import Types
import qualified AppStorage as A
import qualified Append4.KV as K
import qualified Mailboxes as M
import Crypto (seconds)
import System.Environment
import System.Directory
import System.FilePath
#if defined(mingw32_HOST_OS) || defined(darwin_HOST_OS)
import qualified UpdateBackup as Backup
import UpdateState (UpdateRoots(..))
#endif
import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as L

-- Observations only. Lean owns the behavioral assertions.
main :: IO ()
main = do
 [path] <- getArgs
 relay <- relayChecks path
#if defined(mingw32_HOST_OS) || defined(darwin_HOST_OS)
 backup <- backupCheck (takeDirectory path)
 legacyBackup <- legacyBackupCheck (takeDirectory path)
 L.putStrLn (encode (put "legacy_backup_snapshot_valid" (Bool legacyBackup) (put "backup_snapshot_valid" (Bool backup) relay)))
#else
 L.putStrLn (encode relay)
#endif

relayChecks :: FilePath -> IO Value
relayChecks path = do
 let config = A.defaultOptions { A.historyPolicy = A.CurrentOnly }
 bracket (A.openStoreWith config path) A.closeStore $ \store -> do
  time <- seconds
  let room name expires = obj [("id",str name),("desktop",str (M.hashText "desktop-token")),("phone",str (M.hashText "phone-token")),("expires",expires)]
      message ident owner expires = obj [("id",num ident),("room",str owner),("recipient",str "phone"),("frame",obj [("ciphertext",str "payload")]),("expires",toJSON expires)]
      saveMessage ident owner expires = M.putRow store "messages" (M.messageIdentity owner (num ident)) (message ident owner expires)
  A.transaction store $ do
   M.putRow store "rooms" "expired-room" (room "expired-room" (toJSON (time-1)))
   M.putRow store "rooms" "live-room" (room "live-room" Null)
   M.putRow store "rooms" "trusted-room" (room "trusted-room" (toJSON (time-1)))
   M.putRow store "rooms" "trusted-room" (room "trusted-room" Null)
   saveMessage 1 "expired-room" (time+1000)
   saveMessage 2 "live-room" (time-1)
   saveMessage 3 "live-room" (time+1000)
   M.expire store time
  expired <- M.row store "rooms/expired-room"
  expiredMessages <- M.roomMessages store "expired-room"
  live <- M.roomMessages store "live-room"
  trusted <- M.row store "rooms/trusted-room"
  indexes <- K.values store "expiry/"
  -- Bounded cleanup must not let overdue rows through while a backlog remains.
  A.transaction store $ forM_ [10..309] $ \ident -> saveMessage ident "live-room" (time-1)
  batch <- M.transact store "GET" ["api","rooms","live-room","messages"] "phone-token" empty "admin" False
  A.transaction store (M.expire store time)
  void (M.transact store "DELETE" ["api","rooms","live-room","messages","3"] "phone-token" empty "admin" False)
  remaining <- M.roomMessages store "live-room"
  remainingIndexes <- K.values store "expiry/"
  retained <- K.history store
  pure $ obj
   [("expired_room_removed",Bool (expired==Null && null expiredMessages))
   ,("live_room_isolated",Bool (map (`get` "id") live==[num 3]))
   ,("trust_removes_expiry",Bool (trusted/=Null && all (not . T.isInfixOf "trusted-room" . fst) indexes))
   ,("backlog_never_delivered",Bool (map (`get` "id") (items batch "messages")==[num 3]))
   ,("ack_cleans_indexes",Bool (null remaining && null remainingIndexes))
   ,("no_payload_history",Bool (null retained))]

#if defined(mingw32_HOST_OS) || defined(darwin_HOST_OS)
backupCheck :: FilePath -> IO Bool
backupCheck root = do
 let live = root </> "backup-live"
     config = root </> "backup-config"
     destination = root </> "snapshot"
     database = live </> "corpora.lmdb"
     copied = destination </> "0" </> "corpora.lmdb"
 createDirectory live
 createDirectory config
 bracket (A.openStore database) A.closeStore $ \store -> A.transaction store $ do
  A.putValue store "local/registry" (obj [("locals",arr [])])
  A.putValue store "saved" (str "value")
 Backup.withRootLocks live $ void (Backup.snapshot destination (UpdateRoots live config))
 value <- A.getOfflineValue copied "saved"
 ownership <- doesFileExist (copied<.>"lock")
 native <- doesFileExist (copied++"-lock")
 pure (value==Just (str "value") && not ownership && not native)

legacyBackupCheck :: FilePath -> IO Bool
legacyBackupCheck root = do
 let live=root </> "legacy-backup-live"
     config=root </> "legacy-backup-config"
     destination=root </> "legacy-snapshot"
     ident="00000000-0000-4000-8000-000000000002"
     child=live </> "corporas" </> ident
 createDirectoryIfMissing True child
 createDirectory config
 L.writeFile (live </> "host-corpora.json") (encode (obj [("locals",arr [str (T.pack ident)])]))
 writeFile (child </> "customer-document.txt") "customer data"
 Backup.withRootLocks live $ void (Backup.snapshot destination (UpdateRoots live config))
 copied <- readFile (destination </> "0/corporas" </> ident </> "customer-document.txt")
 pure (copied=="customer data")
#endif
