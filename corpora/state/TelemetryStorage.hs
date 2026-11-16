{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TelemetryStorage
  ( PrivateFileError (..)
  , atomicWritePrivate
  , checkPrivate
  , createPrivateDirectory
  , isTelemetryPrivatePath
  , isTelemetryPrivateURL
  , privatePathExists
  , readPrivateFile
  , withPrivateLock
  ) where

import Control.Exception (Exception, IOException, bracketOnError, catch, mask, throwIO)
import Control.Monad (filterM, unless, when)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Char (toLower)
import Data.List (dropWhileEnd)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.HTTP.Types.URI (urlDecode)
import Network.URI (parseURI, uriAuthority, uriRegName, uriPath, uriScheme)
import System.Directory (canonicalizePath, doesFileExist, makeAbsolute, pathIsSymbolicLink, removeFile)
import System.FileLock (SharedExclusive (Exclusive), withFileLock)
import System.FilePath ((</>), equalFilePath, joinPath, splitDirectories, splitDrive, takeDirectory, takeFileName)
import RuntimePaths (configDirectory, defaultConfigDirectory)
import System.IO (Handle, hClose)
import System.IO.Error (isDoesNotExistError)
import System.Info (os)
#ifdef mingw32_HOST_OS
import System.IO (IOMode (ReadMode), withBinaryFile)
import qualified TelemetryWindows as Windows
#else
import Control.Exception (bracket, mask_, onException)
import Data.Bits ((.&.))
import System.Directory (renameFile)
import System.IO (openBinaryTempFile)
import System.IO.Error (isAlreadyExistsError)
import qualified System.Posix.Directory as Posix
import qualified System.Posix.Files as Posix
import qualified System.Posix.IO as Posix
import System.Posix.User (getEffectiveUserID)
#endif
#ifdef linux_HOST_OS
import Foreign.C.String (CString, peekCString)
import Foreign.C.Types (CInt (..))
#endif
#ifdef darwin_HOST_OS
import Control.Monad (void)
import Foreign (Ptr, alloca, nullPtr)
import Foreign.C.Error (eINVAL, eNOENT, getErrno, throwErrno, throwErrnoIfMinus1_)
import Foreign.C.String (CString, withCString)
import Foreign.C.Types (CInt (..))
#endif

data PrivateFileError = InsecurePrivateFile | PrivateFileTooLarge
  deriving (Eq, Show)

instance Exception PrivateFileError

privatePathExists :: FilePath -> IO Bool
privatePathExists path = (True <$ pathIsSymbolicLink path) `catch` \(err :: IOException) ->
  if isDoesNotExistError err then pure False else throwIO err

-- Compare actual private locations, including aliases and Windows stream syntax.
-- The Corpora identity is core application data; only .telemetry is private.
isTelemetryPrivatePath :: FilePath -> IO Bool
isTelemetryPrivatePath path = do
  absolute <- makeAbsolute (nativePath path)
  resolved <- canonicalizePath absolute
  profiles <- mapM (>>= canonicalizePath . (</> "telemetry")) [configDirectory, defaultConfigDirectory]
  let candidates = filter ((== ".telemetry") . foldCase . takeFileName)
        (ancestors absolute <> ancestors resolved)
  corpora <- filterM (doesFileExist . (</> "corpora-identity.json") . takeDirectory) candidates
    >>= mapM canonicalizePath
  pure (any (\root -> any (matches root) (ancestors resolved)) (profiles <> corpora))
  where
    matches a b = equalFilePath (foldCase (nativePath a)) (foldCase (nativePath b))
    foldCase = if os `elem` ["mingw32", "darwin"] then map toLower else id

ancestors :: FilePath -> [FilePath]
ancestors path = path : if parent == path then [] else ancestors parent
  where parent = takeDirectory path

nativePath :: FilePath -> FilePath
nativePath path
  | os == "mingw32" = let (drive, rest) = splitDrive (namespace path)
      in drive <> joinPath (map component (splitDirectories rest))
  | otherwise = path
  where
    namespace value = case value of
      '\\':'\\':'?':'\\':u:n:c:'\\':rest | map toLower [u,n,c] == "unc" -> "\\\\" <> rest
      '\\':'\\':device:'\\':rest | device `elem` ['?', '.'] -> rest
      _ -> value
    component value
      | value `elem` [".", ".."] = value
      | otherwise = dropWhileEnd (`elem` (" ." :: String)) (takeWhile (/= ':') value)

isTelemetryPrivateURL :: String -> IO Bool
isTelemetryPrivateURL address = case parseURI address of
  Just uri | map toLower (uriScheme uri) `elem` ["file:", "sapiens-file:"] ->
    case TE.decodeUtf8' (urlDecode False (TE.encodeUtf8 (T.pack (uriPath uri)))) of
      Left _ -> pure True
      Right value ->
        let host = maybe "" uriRegName (uriAuthority uri)
            path = case T.unpack value of
              '/':drive:':':rest | os == "mingw32" -> drive:':':rest
              decoded | os == "mingw32", host /= "", map toLower host /= "localhost" -> "//" <> host <> decoded
                      | otherwise -> decoded
        in isTelemetryPrivatePath path
  _ -> pure False

readPrivateFile :: FilePath -> IO BS.ByteString
readPrivateFile path = checkPrivate path False >> readPrivateBytes path

readBoundedBytes :: Handle -> IO BS.ByteString
readBoundedBytes stream = do
  bytes <- BS.hGet stream (64 * 1024 + 1)
  when (BS.length bytes > 64 * 1024) (throwIO PrivateFileTooLarge)
  pure bytes

withPrivateLock :: FilePath -> IO a -> IO a
withPrivateLock path action = do
  checkPrivate (takeDirectory path) True
  createPrivateLock path
  withFileLock path Exclusive $ \_ -> checkPrivate path False >> action

atomicWritePrivate :: FilePath -> BL.ByteString -> IO () -> IO ()
atomicWritePrivate destination bytes beforePublish = mask $ \restore -> do
  let root = takeDirectory destination
  checkPrivate root True
  bracketOnError (openPrivateTemp root) cleanup $ \(path, stream) -> do
    restore $ do
      checkPrivate path False
      BL.hPut stream bytes
      hClose stream
    beforePublish
    checkPrivate root True
    checkPrivate path False
    exists <- privatePathExists destination
    when exists (checkPrivate destination False)
    replacePrivateFile path destination
  where
    cleanup (path, stream) = do
      hClose stream `catch` \(_ :: IOException) -> pure ()
      removeFile path `catch` \(_ :: IOException) -> pure ()

#ifdef mingw32_HOST_OS
openPrivateTemp :: FilePath -> IO (FilePath, Handle)
openPrivateTemp = Windows.createPrivateTempFile

checkPrivate :: FilePath -> Bool -> IO ()
checkPrivate path directory = do
  safe <- Windows.checkPrivate path directory
  unless safe (throwIO InsecurePrivateFile)

readPrivateBytes :: FilePath -> IO BS.ByteString
readPrivateBytes path = withBinaryFile path ReadMode readBoundedBytes

createPrivateDirectory :: FilePath -> IO ()
createPrivateDirectory path = Windows.createPrivateDirectory path >> checkPrivate path True

createPrivateLock :: FilePath -> IO ()
createPrivateLock path = Windows.createPrivateLock path >> checkPrivate path False

replacePrivateFile :: FilePath -> FilePath -> IO ()
replacePrivateFile = Windows.replaceProfile
#else
openPrivateTemp :: FilePath -> IO (FilePath, Handle)
openPrivateTemp root = openBinaryTempFile root ".telemetry-writing-"

checkPrivate :: FilePath -> Bool -> IO ()
checkPrivate path directory = do
  Posix.getSymbolicLinkStatus path >>= checkPrivateStatus directory
#ifdef darwin_HOST_OS
  withCString path $ \name -> withoutExtendedACL (aclGetFile name 0x100)
#endif

checkPrivateStatus :: Bool -> Posix.FileStatus -> IO ()
checkPrivateStatus directory status = do
  owner <- getEffectiveUserID
  let validType = if directory
        then Posix.isDirectory status
        else Posix.isRegularFile status && Posix.linkCount status == 1
  unless (validType && Posix.fileOwner status == owner
      && Posix.fileMode status .&. 0o077 == 0) (throwIO InsecurePrivateFile)

readPrivateBytes :: FilePath -> IO BS.ByteString
readPrivateBytes path = bracket open hClose readBoundedBytes
  where
    open = mask_ $ do
      fd <- Posix.openFd path Posix.ReadOnly Posix.defaultFileFlags
        { Posix.nofollow = True, Posix.nonBlock = True, Posix.cloexec = True }
      (do
        Posix.getFdStatus fd >>= checkPrivateStatus False
#ifdef darwin_HOST_OS
        withoutExtendedACL (aclGetFd (fromIntegral fd) 0x100)
#endif
        Posix.fdToHandle fd) `onException` Posix.closeFd fd

createPrivateDirectory :: FilePath -> IO ()
createPrivateDirectory path = Posix.createDirectory path 0o700 `catch` \(err :: IOException) ->
  unless (isAlreadyExistsError err) (throwIO err)

createPrivateLock :: FilePath -> IO ()
createPrivateLock path = do
  exists <- privatePathExists path
  unless exists $
    bracket (Posix.openFd path Posix.ReadWrite Posix.defaultFileFlags
      { Posix.creat = Just 0o600, Posix.exclusive = True, Posix.cloexec = True })
      Posix.closeFd (const (pure ()))
      `catch` \(err :: IOException) -> unless (isAlreadyExistsError err) (throwIO err)
  checkPrivate path False

replacePrivateFile :: FilePath -> FilePath -> IO ()
replacePrivateFile = renameFile
#endif

#ifdef linux_HOST_OS
foreign export ccall "sapiens_telemetry_private_url"
  telemetryPrivateURL :: CString -> IO CInt

telemetryPrivateURL :: CString -> IO CInt
telemetryPrivateURL address = do
  private <- peekCString address >>= isTelemetryPrivateURL
  pure (if private then 1 else 0)
#endif

#ifdef darwin_HOST_OS
foreign import ccall unsafe "acl_get_file"
  aclGetFile :: CString -> CInt -> IO (Ptr ())
foreign import ccall unsafe "acl_get_fd_np"
  aclGetFd :: CInt -> CInt -> IO (Ptr ())
foreign import ccall unsafe "acl_valid"
  aclValid :: Ptr () -> IO CInt
foreign import ccall unsafe "acl_get_entry"
  aclGetEntry :: Ptr () -> CInt -> Ptr (Ptr ()) -> IO CInt
foreign import ccall unsafe "acl_free"
  aclFree :: Ptr () -> IO CInt

withoutExtendedACL :: IO (Ptr ()) -> IO ()
withoutExtendedACL acquire =
  bracket open release $ \acl -> unless (acl == nullPtr) $ do
    throwErrnoIfMinus1_ "telemetry ACL" (aclValid acl)
    alloca $ \entry -> do
      result <- aclGetEntry acl 0 entry
      if result == 0 then throwIO InsecurePrivateFile else do
        err <- getErrno
        unless (err == eINVAL) (throwErrno "telemetry ACL")
  where
    open = do
      acl <- acquire
      when (acl == nullPtr) $ do
        err <- getErrno
        unless (err == eNOENT) (throwErrno "telemetry ACL")
      pure acl
    release acl = unless (acl == nullPtr) (void (aclFree acl))
#endif
