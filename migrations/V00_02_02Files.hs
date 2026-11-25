{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
-- Frozen v00.02.02 file migration. Compatibility filenames are retained, but
-- active application-owned values use binary scalar cells, never JSON encoding.
module V00_02_02Files (readValue,writeValue,atomicBytes,migrateFile,migrateIdentity,migrateConfig) where
import qualified Append4.Flat as F
import Control.Exception
import Control.Monad
import Data.Aeson hiding (encode)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.Map.Strict as M
import System.Directory
import System.FilePath
import System.IO
#ifdef mingw32_HOST_OS
import Foreign.C
import Control.Concurrent (threadDelay)
import System.IO.Error (isPermissionError)
foreign import ccall unsafe "_wopen" syncOpen :: CWString -> CInt -> CInt -> IO CInt
foreign import ccall unsafe "_commit" syncCommit :: CInt -> IO CInt
foreign import ccall unsafe "_close" syncClose :: CInt -> IO CInt
#else
import qualified System.Posix.IO as P
import qualified System.Posix.Unistd as P
#endif

magic :: B.ByteString
magic = "SAPIENS-TYPED-CELLS-1\NUL"
encoded :: ToJSON a => a -> L.ByteString
encoded = L.fromStrict . (magic <>) . F.encode . M.toAscList . F.flatten . toJSON
decoded :: FromJSON a => B.ByteString -> IO a
decoded bytes = do
 value <- if magic `B.isPrefixOf` bytes
  then F.decode (B.drop (B.length magic) bytes) >>= F.unflatten . M.fromList
  else either fail pure (eitherDecodeStrict' bytes :: Either String Value)
 case fromJSON value of Error e -> fail e; Success a -> pure a
readValue :: FromJSON a => FilePath -> a -> IO a
readValue path def = do
 let sidecar=replaceExtension path "typed"
 selected <- if takeFileName path=="corpora-identity.json" then do
   exists <- doesFileExist sidecar
   pure (if exists then sidecar else path)
  else pure path
 doesFileExist selected >>= \exists -> if exists then B.readFile selected >>= decoded else pure def
writeValue :: ToJSON a => FilePath -> a -> IO ()
writeValue path = atomicBytes path . encoded
atomicBytes :: FilePath -> L.ByteString -> IO ()
atomicBytes path bytes = do
 createDirectoryIfMissing True (takeDirectory path)
 bracketOnError (openBinaryTempFile (takeDirectory path) ".typed-writing-")
  (\(tmp,h) -> do hClose h `catch` (\(_::IOException)->pure ()); removeFile tmp `catch` (\(_::IOException)->pure ())) $ \(tmp,h) -> do
   L.hPut h bytes
   hFlush h
#ifdef mingw32_HOST_OS
   hClose h
   withCWString tmp $ \name -> bracket (syncOpen name 32770 0) (void . syncClose) $ \fd -> do
    when (fd<0) (fail "Cannot open durable typed temporary file")
    code <- syncCommit fd
    unless (code==0) (fail "Cannot synchronize typed temporary file")
#else
   fd <- P.handleToFd h
   P.fileSynchronise fd `finally` P.closeFd fd
#endif
#ifdef mingw32_HOST_OS
   let publish attempt = renameFile tmp path `catch` \(e::IOException) ->
         if isPermissionError e && attempt < (100::Int) then threadDelay 10000 >> publish (attempt+1) else throwIO e
   publish 0
#else
   renameFile tmp path
#endif
#ifndef mingw32_HOST_OS
   bracket (P.openFd (takeDirectory path) P.ReadOnly P.defaultFileFlags) P.closeFd P.fileSynchronise
#endif
migrateFile :: FilePath -> IO ()
migrateFile path = do
 exists <- doesFileExist path
 when exists $ do
  link <- pathIsSymbolicLink path
  when link (fail "Refusing to replace linked persistent settings")
  bytes <- B.readFile path
  unless (magic `B.isPrefixOf` bytes) $ do
   value <- decoded bytes :: IO Value
   let backup=path<>".v00.02.01"
   backed <- doesFileExist backup
   if backed then do
    original <- B.readFile backup
    unless (original==bytes) (fail "Conflicting settings migration backup")
   else atomicBytes backup (L.fromStrict bytes)
   writeValue path value
migrateConfig :: FilePath -> IO ()
migrateConfig root = do
 mapM_ (migrateFile . (root </>)) ["storage.json","terms.json","telemetry.json"]
 let clients=root </> "client-preferences"
 exists <- doesDirectoryExist clients
 when exists $ do
  linked <- pathIsSymbolicLink clients
  when linked (fail "Refusing linked preference migration")
  listDirectory clients >>= mapM_ (\name -> when (takeExtension name==".json") (migrateFile (clients </> name)))

-- Corpora identity is also part of the frozen SQLite source fingerprint. Keep
-- that exact legacy input in place; the typed sidecar becomes authoritative.
migrateIdentity :: FilePath -> IO ()
migrateIdentity path = do
 exists <- doesFileExist path
 when exists $ do
  bytes <- B.readFile path
  unless (magic `B.isPrefixOf` bytes) $ do
   value <- decoded bytes :: IO Value
   let target=replaceExtension path "typed"
   present <- doesFileExist target
   if present then do
    current <- readValue target Null
    unless (current==value) (fail "Conflicting Corpora identity")
   else writeValue target value
