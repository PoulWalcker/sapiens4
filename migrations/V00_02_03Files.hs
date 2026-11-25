-- v00.02.03 settings: complete UTF-8 keys mapped directly to typed values.
-- Earlier bytes are retained verbatim; old codecs are migration readers only.
module V00_02_03Files (readValue,writeValue,migrateConfig,migrateIdentity) where
import qualified V00_02_02Files as Old
import qualified Append4.Flat as F
import qualified Append4.Key as Key
import Control.Monad
import Data.Aeson
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.Map.Strict as M
import System.Directory
import System.FilePath

magic :: B.ByteString
magic = "SAPIENS-SINGLE-KEY-3\NUL"
readValue :: FromJSON a => FilePath -> a -> IO a
readValue path def = do
 let sidecar=replaceExtension path "typed"
 selected <- if takeFileName path=="corpora-identity.json" then do
  present<-doesFileExist sidecar
  pure(if present then sidecar else path)
  else pure path
 present<-doesFileExist selected
 if not present then pure def else do
  bytes<-B.readFile selected
  if magic `B.isPrefixOf` bytes then do
   rows<-F.decode(B.drop(B.length magic)bytes)
   value<-Key.unflatten "value" (M.fromList rows) >>= maybe(fail "Empty settings file") pure
   case fromJSON value of Error e->fail e;Success a->pure a
  else Old.readValue selected def
writeValue :: ToJSON a => FilePath -> a -> IO ()
writeValue path value = Old.atomicBytes path (L.fromStrict(magic<>F.encode(M.toAscList(Key.flatten "value" (toJSON value)))))
migrateFile :: FilePath -> IO ()
migrateFile path = do
 exists<-doesFileExist path
 when exists $ do
  linked<-pathIsSymbolicLink path
  when linked(fail "Refusing linked settings migration")
  bytes<-B.readFile path
  unless(magic `B.isPrefixOf` bytes)$do
   Old.migrateFile path
   original<-B.readFile path
   value<-Old.readValue path Null
   let backup=path<>".v00.02.02"
   backed<-doesFileExist backup
   if backed then B.readFile backup >>= \saved->unless(saved==original)(fail "Conflicting settings backup")
    else Old.atomicBytes backup(L.fromStrict original)
   writeValue path value
   actual<-readValue path Null
   unless(actual==value)(fail "Single-key settings verification failed")
migrateConfig :: FilePath -> IO ()
migrateConfig root = do
 mapM_ (migrateFile . (root </>)) ["storage.json","terms.json","telemetry.json"]
 let clients=root </> "client-preferences"
 exists<-doesDirectoryExist clients
 when exists $ do
  linked<-pathIsSymbolicLink clients
  when linked(fail "Refusing linked preference migration")
  listDirectory clients >>= mapM_ (\name->when(takeExtension name==".json")(migrateFile(clients </> name)))
migrateIdentity :: FilePath -> IO ()
migrateIdentity path = do
 let sidecar=replaceExtension path "typed"
 exists<-doesFileExist sidecar
 if exists then migrateFile sidecar else do
  present<-doesFileExist path
  when present $ do
   bytes<-B.readFile path
   -- Fresh v3 workspaces already have the current representation. Only old
   -- identities need the frozen reader and a fingerprint-preserving sidecar.
   if magic `B.isPrefixOf` bytes then void(readValue path Null)
    else do
     Old.migrateIdentity path
     prepared<-doesFileExist sidecar
     unless prepared (Old.readValue path Null >>= Old.writeValue sidecar)
     migrateFile sidecar
