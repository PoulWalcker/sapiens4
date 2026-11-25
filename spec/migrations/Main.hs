module Main where
import qualified Append4 as A
import qualified Append4.KV as K
import qualified Append4.Key as Key
import qualified Append4.Convert as Convert
import qualified Append4.Flat as F
import qualified AppStorage as App
import qualified Data.Map.Strict as Map
import qualified V00_02_00 as Upgrade
import qualified V00_02_03Files as NewFiles
import Control.Concurrent (threadDelay)
import System.IO (stdout, hFlush)
import Control.Exception
import Control.Monad
import Data.Aeson
import qualified Data.Text
import qualified Data.ByteString.Lazy.Char8 as L
import MigrationPlan
import MigrationVersion
import qualified Migrations
import qualified V00_02_01 as Lifecycle
import qualified V00_02_02Files as TypedFiles
import System.FilePath
import System.Environment

main :: IO ()
main = do
 args <- getArgs
 case args of
  ["at",path,number] -> bracket (A.openStore path) A.closeStore $ \store -> A.snapshotAt store (read number) >>= L.putStrLn . encode . fmap A.entries
  ["duplicate",path] -> bracket (A.openStore path) A.closeStore $ \store -> do
    result <- A.submit store (A.Transaction "customer-id" [] [A.Set "document" Null]) >>= A.await
    unless (result==A.Duplicate) (fail "Duplicate ledger lost")
    L.putStrLn "true"
  ["files",root] -> do
    TypedFiles.migrateConfig root
    TypedFiles.readValue (root </> "storage.json") Null >>= L.putStrLn . encode
  ["history",path] -> bracket (A.openStore path) A.closeStore (A.history >=> L.putStrLn . encode)
  ["corpora",root] -> dumpNew (Migrations.openCorpora root)
  ["corpora02",root] -> dump (Migrations.openPreviousVersioned A.defaultOptions (root </> "corpora.lmdb") (root </> "corpora.sqlite3") (Upgrade.corpora root) (Upgrade.corporaFingerprint root) Lifecycle.corpora)
  ["relay02",path] -> do
   let source=replaceExtension path "sqlite3";target=replaceExtension path "lmdb"
   dump (Migrations.openPreviousVersioned A.defaultOptions {A.historyPolicy=A.CurrentOnly} target source (Upgrade.relay source) (Upgrade.relayFingerprint source) Lifecycle.relay)
  ["raw",path] -> dumpNew (K.openStore path)
  ["prior-history",path] -> Convert.readPriorHistory path >>= L.putStrLn . encode
  ["history03",path] -> bracket (K.openStore path) K.closeStore (K.history >=> L.putStrLn . encode)
  ["at03",path,number] -> bracket (K.openStore path) K.closeStore $ \store -> K.snapshotAt store (read number) >>= L.putStrLn . encode . fmap K.entries
  ["record-at03",path,number,key] -> bracket (K.openStore path) K.closeStore $ \store -> do
   snap<-K.snapshotAt store(read number) >>= maybe(fail "Missing historical snapshot") pure
   Key.unflatten (Data.Text.pack key)(K.entries snap) >>= L.putStrLn . encode
  ["record03",path,key] -> App.getOfflineValue path (Data.Text.pack key) >>= L.putStrLn . encode
  [mode,root] | mode `elem` ["identity03","identity02"] -> do
   let path=root </> "corpora-identity.json"
   (if mode=="identity03" then NewFiles.writeValue else TypedFiles.writeValue) path(object ["version" .= (1::Int),"corpora_id" .= ("00000000-0000-4000-8000-000000000001"::Data.Text.Text)])
   bracket(Migrations.openCorpora root)K.closeStore(const(pure()))
   NewFiles.readValue path Null >>= L.putStrLn . encode
  ["files03",root] -> NewFiles.migrateConfig root >> NewFiles.readValue (root </> "storage.json") Null >>= L.putStrLn . encode
  ["set-version03",path,value] -> bracket (K.openStore path) K.closeStore $ \store -> K.putValue store versionKey (F.TextAtom(Data.Text.pack value))
  ["seed-id",path] -> bracket (A.openStore path) A.closeStore $ \store -> do
   result<-A.submit store(A.Transaction "customer-id/~escaped" [] [A.Set "dedup/receipt" (Bool True)]) >>= A.await
   case result of A.Accepted _->L.putStrLn "true";_->fail "Seed id rejected"
  ["duplicate03",path] -> bracket (K.openStore path) K.closeStore $ \store -> do
   result<-K.submit store(K.Transaction "customer-id/~escaped" [] [K.Set "document" F.NullAtom]) >>= K.await
   unless(result==K.Duplicate)(fail "Duplicate ledger lost")
   L.putStrLn "true"
  ["check03",path] -> checkScalar path
  ["relay",path] -> dumpNew (Migrations.openRelay path)
  ["plan",path] -> checkPlan path
  ["inspect",path] -> dump (A.openStore path)
  ["crash",path] -> bracket (A.openStore path) A.closeStore $ \store -> do
   A.putValue store versionKey (String (versionText baselineVersion))
   A.transaction store $ do
    A.putValue store "partial" (Bool True)
    A.putValue store versionKey (String (versionText currentVersion))
    putStrLn "READY"
    hFlush stdout
    forever (threadDelay 1000000)
  ["seed",path,input] -> do
   entries <- L.readFile input >>= either fail pure . eitherDecode
   bracket (A.openStore path) A.closeStore $ \store -> A.transaction store $
    mapM_ (uncurry (A.putValue store)) (entries :: [(Data.Text.Text,Value)])
  ["crash-cleanup",path] -> bracket (A.openStore path) A.closeStore $ \store ->
   runMigrations [Migration Lifecycle.sourceVersion Lifecycle.targetVersion (\s -> do
    Lifecycle.corpora s
    putStrLn "READY"
    hFlush stdout
    forever (threadDelay 1000000))] Lifecycle.sourceVersion Lifecycle.targetVersion store
  ["set-version",path,value] -> bracket (A.openStore path) A.closeStore $ \store -> A.putValue store versionKey (toJSON value)
  _ -> fail "corpora ROOT | relay PATH | plan PATH | set-version PATH VERSION"
 where
  dumpNew open = bracket open K.closeStore (K.readSnapshot >=> L.putStrLn . encode . K.entries)
  dump open = bracket open A.closeStore (A.readSnapshot >=> L.putStrLn . encode . A.entries)

checkPlan :: FilePath -> IO ()
checkPlan path = bracket (A.openStore path) A.closeStore $ \store -> do
 let current=AppVersion "v00.02.00"
     next=AppVersion "v00.02.01"
     later=AppVersion "v00.02.02"
     first=Migration baselineVersion current (\s -> A.putValue s "first" (Bool True))
     second=Migration current next (\s -> A.getValue s "first" >>= \v -> unless (v==Just (Bool True)) (fail "order") >> A.putValue s "second" (Bool True))
     failing=Migration next later (\s -> A.putValue s "partial" (Bool True) >> fail "injected failure")
     steps=[second,first,failing]
     denied result=case result of Left _ -> pure (); Right _ -> fail "invalid plan accepted"
 denied (plan [first] baselineVersion next)
 denied (plan [first,first] baselineVersion current)
 denied (plan [] next current)
 A.putValue store versionKey (String (versionText baselineVersion))
 result <- try (runMigrations steps baselineVersion later store) :: IO (Either IOException ())
 case result of Left _ -> pure (); Right _ -> fail "failure swallowed"
 version <- A.getValue store versionKey
 partial <- A.getValue store "partial"
 unless (version==Just (String (versionText next)) && partial==Nothing) $ fail "failed step was partially committed"
 let fixed=Migration next later (\s -> A.putValue s "third" (Bool True))
 runMigrations [fixed] next later store
 before <- A.readSnapshot store
 runMigrations [fixed] later later store
 after <- A.readSnapshot store
 unless (before==after) $ fail "repeat changed state"
 putStrLn "PASS sequential steps, rollback, resume, no-op, gaps, ambiguity, downgrade"

checkScalar :: FilePath -> IO ()
checkScalar path = bracket (K.openStore path) K.closeStore $ \store -> do
 let value=object ["a/b" .= object ["~i0" .= ([Null,object [],Array mempty,Bool False]::[Value])],"" .= ("value"::Data.Text.Text)]
 App.putValue store "record/id" value
 App.getValue store "record/id" >>= \actual->unless(actual==Just value)(fail "record round trip")
 rows<-K.values store "record/id"
 when(any((=="record/id").fst)rows)(fail "Nonempty document root persisted")
 App.putValue store "record/id" (object ["name" .= ("updated"::Data.Text.Text)])
 changed<-K.values store "record/id/"
 unless(changed==[("record/id/name",F.TextAtom "updated")])(fail "Old scalar keys retained")
 before<-K.readSnapshot store
 failure<-try(K.transaction store(K.putValue store "bad" F.NullAtom >> fail "rollback")) :: IO(Either IOException ())
 case failure of Right _->fail "Rollback did not fail";Left _->pure()
 after<-K.readSnapshot store
 unless(before==after)(fail "Partial transaction committed")
 invalid<-try(K.putValue store "bad" (F.ArrayAtom 2)) :: IO(Either IOException ())
 case invalid of Right _->fail "Non-scalar accepted";Left _->pure()
 App.deleteValue store "record/id"
 K.values store "record/id" >>= \rest->unless(null rest)(fail "Deletion left cells")
 L.putStrLn "true"
