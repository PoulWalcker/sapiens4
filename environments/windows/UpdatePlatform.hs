{-# LANGUAGE ForeignFunctionInterface #-}
module UpdatePlatform where

import Control.Exception
import Control.Monad
import qualified Data.ByteString as B
import Data.Word
import Foreign hiding (void)
import Foreign.C
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async
import Data.Char (toLower)
import Data.List (isPrefixOf)
import System.Directory hiding (executable)
import System.Environment
import System.Exit
import System.FilePath
import System.Info (arch)
import System.IO
import System.Process
import System.Process.Internals (ProcessHandle__(..),withProcessHandle)
import UpdateState

newtype Lease = Lease (Ptr ())
foreign import ccall safe "MessageBoxW" cMessageBox :: Ptr () -> CWString -> CWString -> Word32 -> IO CInt
foreign import ccall safe "WaitForSingleObject" waitProcess :: Ptr () -> Word32 -> IO Word32
foreign import ccall safe "sapiens_update_validate" cValidate :: CWString -> IO Word32
foreign import ccall safe "sapiens_update_open_root" cOpenRoot :: CWString -> Ptr (Ptr ()) -> Ptr Word64 -> IO Word32
foreign import ccall safe "sapiens_update_lease" cLease :: CWString -> CInt -> IO (Ptr ())
foreign import ccall unsafe "sapiens_update_close" cClose :: Ptr () -> IO ()
foreign import ccall safe "sapiens_update_downgrade" cDowngrade :: Ptr () -> IO Word32
foreign import ccall safe "sapiens_update_publish" cPublish :: CWString -> CWString -> Ptr Word8 -> Word32 -> CInt -> IO Word32
foreign import ccall safe "sapiens_update_copy" cCopy :: CWString -> CWString -> IO Word32
foreign import ccall safe "sapiens_update_metadata" cMetadata :: CWString -> Ptr Word8 -> Word32 -> IO Word32
foreign import ccall safe "sapiens_update_copy_metadata" cCopyMetadata :: CWString -> CWString -> IO Word32
foreign import ccall safe "sapiens_update_job" cJob :: Ptr () -> CInt -> IO Word32

check :: String -> Word32 -> IO ()
check operation code = unless (code == 0) (fail (operation ++ " failed (Windows error " ++ show code ++ ")"))
acquireLease :: FilePath -> Bool -> IO Lease
acquireLease path exclusive = leaseWithMode path (if exclusive then 1 else 0)
leaseWithMode :: FilePath -> CInt -> IO Lease
leaseWithMode path mode = withCWString path $ \p -> do
 handle <- cLease p mode
 when (handle == nullPtr) (fail "Could not acquire the installation lease. Close other Sapiens instances and retry.")
 pure (Lease handle)
releaseLease :: Lease -> IO ()
releaseLease (Lease handle) = cClose handle
withLease :: FilePath -> Bool -> (Lease -> IO a) -> IO a
withLease path exclusive = bracket (acquireLease path exclusive) releaseLease
withIdleLease :: FilePath -> IO a -> IO a
withIdleLease path = bracket (leaseWithMode path 2) releaseLease . const
downgradeLease :: Lease -> IO ()
downgradeLease (Lease handle) = cDowngrade handle >>= check "Lease handoff"
publish, publishNew :: FilePath -> FilePath -> B.ByteString -> IO ()
publish = publishWith 1
publishNew = publishWith 0
publishWith :: CInt -> FilePath -> FilePath -> B.ByteString -> IO ()
publishWith replace path temporary bytes = withCWString path $ \p -> withCWString temporary $ \t ->
 B.useAsCStringLen bytes $ \(buffer,len) -> cPublish p t (castPtr buffer) (fromIntegral len) replace >>= check "Durable update ownership or decision"
copyOwned :: FilePath -> FilePath -> IO ()
copyOwned source destination = withCWString source $ \s -> withCWString destination $ \d -> cCopy s d >>= check "Owned file copy"
copyMetadata :: FilePath -> FilePath -> IO ()
copyMetadata source destination = withCWString source $ \s -> withCWString destination $ \d -> cCopyMetadata s d >>= check "Owned metadata copy"
metadata :: FilePath -> IO B.ByteString
metadata path = withCWString path $ \p -> allocaBytes 65536 $ \buffer -> do
 count <- cMetadata p buffer 65536
 when (count == 0 || count > 65536) (fail ("Unsupported or unreadable Windows metadata: " ++ path))
 B.packCStringLen (castPtr buffer,fromIntegral count)
showUpdateError :: String -> IO ()
showUpdateError message = withCWString message $ \body -> withCWString "Sapiens update" $ \title -> void (cMessageBox nullPtr body title 0x10)

validatePath :: FilePath -> IO ()
validatePath path = pathPresent path >>= \exists -> unless exists (fail ("Owned path is missing: " ++ path))

withRootIdentity :: FilePath -> (String -> IO a) -> IO a
withRootIdentity path action = do
 validatePath path
 bracket (withCWString path $ \p -> alloca $ \handle -> allocaArray 2 $ \identity -> do
  cOpenRoot p handle identity >>= check ("Open Data or Config root: " ++ path)
  (,) <$> peek handle <*> (show <$> peekArray 2 identity)) (cClose . fst) (action . snd)

pathPresent :: FilePath -> IO Bool
pathPresent path = do
 let parent = takeDirectory (dropTrailingPathSeparator path)
 present <- if equalFilePath parent path then pure True else pathPresent parent
 if not present then pure False else do
  code <- withCWString path cValidate
  if code `elem` [2,3] then pure False else check ("Owned path validation: " ++ path) code >> pure True

containsPath :: FilePath -> FilePath -> Bool
containsPath parent path = let p = map toLower (normalise parent); q = map toLower (normalise path) in p == q || addTrailingPathSeparator p `isPrefixOf` q

profileExclusions :: [FilePath]
profileExclusions = ["WebView2/Shell","WebView2/Guests"]

payloadName :: String
payloadName = "Sapiens-Payload-" ++ (if arch=="aarch64" then "win-arm64" else "win-x64") ++ ".exe"
versionExecutable :: FilePath -> Version -> FilePath
versionExecutable root version = versionPath root version </> "Sapiens4.exe"
starterDirectory :: IO FilePath
starterDirectory = getExecutablePath >>= canonicalizePath . takeDirectory

stagePayload :: FilePath -> FilePath -> IO ()
stagePayload payload destination = do
 createDirectory destination
 (_,_,_,child) <- createProcess (proc payload ["/VERYSILENT","/SUPPRESSMSGBOXES","/NORESTART","/DIR="++destination]) {close_fds=True}
 code <- waitForProcess child
 unless (code == ExitSuccess) (fail "Update payload could not be staged")
 exists <- doesFileExist (destination </> "Sapiens4.exe")
 unless exists (fail "Update payload is incomplete")

launchVersion :: FilePath -> Version -> [String] -> IO ()
launchVersion root version args = do
 inherited <- getEnvironment
 void $ createProcess (proc (versionExecutable root version) args)
  {env=Just (filter (\(key,_) -> key /= "SAPIENS_UPDATE_ATTEMPT") inherited),close_fds=True}

data Candidate = Candidate ProcessHandle Handle Handle
withCandidate :: FilePath -> [String] -> [(String,String)] -> (Candidate -> IO a) -> IO a
withCandidate executable args overrides = bracket acquire close
 where
 acquire = do
  inherited <- getEnvironment
  (Just input,Just output,_,child) <- createProcess (proc executable args)
   {std_in=CreatePipe,std_out=CreatePipe,std_err=Inherit,use_process_jobs=True,create_group=True,close_fds=True
   ,env=Just (overrides ++ filter (\(key,_) -> key `notElem` map fst overrides) inherited)}
  let candidate = Candidate child input output
  (mapM_ (\stream -> hSetEncoding stream utf8 >> hSetNewlineMode stream universalNewlineMode) [input,output]
   >> pure candidate) `onException` (candidateOwnership candidate False >> close candidate)
 close (Candidate _ input output) = mapM_ (\h -> hClose h `catch` \(_::IOException) -> pure ()) [input,output]

candidateMessage :: Candidate -> IO String
candidateMessage candidate@(Candidate _ _ output) = race exited (hGetLine output) >>= either (const (fail "Candidate exited")) pure
 where
 exited = do
  alive <- candidateAlive candidate
  when alive (threadDelay 100000 >> exited)

candidateAlive :: Candidate -> IO Bool
candidateAlive (Candidate child _ _) = withProcessHandle child $ \state -> case state of
 OpenExtHandle handle _ -> (/=0) <$> waitProcess handle 0
 _ -> pure False

sendCandidate :: Candidate -> String -> IO ()
sendCandidate (Candidate _ input _) message = hPutStrLn input message >> hFlush input

candidateOwnership :: Candidate -> Bool -> IO ()
candidateOwnership (Candidate child _ _) commit = withProcessHandle child $ \state -> case state of
 OpenExtHandle _ handle -> cJob handle (if commit then 1 else 0) >>= check "Candidate process ownership"
 _ -> fail "Candidate has no owned Windows job"
