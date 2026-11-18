{-# LANGUAGE ForeignFunctionInterface #-}
module UpdatePlatform where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async
import Control.Exception
import Control.Monad
import qualified Data.ByteString as B
import Data.Char (toLower)
import Data.List (isPrefixOf)
import Foreign hiding (void)
import Foreign.C
import System.Directory hiding (executable)
import System.Environment
import System.FilePath
import System.IO
import System.Posix.Process (getProcessID)
import System.Posix.Signals
import System.Process
import System.Timeout
import UpdateState

newtype Lease = Lease CInt
foreign import ccall safe "sapiens_update_validate" cValidate :: CString -> IO CInt
foreign import ccall safe "sapiens_update_open_root" cOpenRoot :: CString -> Ptr CInt -> Ptr Word64 -> IO CInt
foreign import ccall safe "sapiens_update_lease" cLease :: CString -> CInt -> IO CInt
foreign import ccall unsafe "close" cClose :: CInt -> IO CInt
foreign import ccall safe "sapiens_update_downgrade" cDowngrade :: CInt -> IO CInt
foreign import ccall safe "sapiens_update_publish" cPublish :: CString -> CString -> Ptr Word8 -> CSize -> CInt -> IO CInt
foreign import ccall safe "sapiens_update_flush" cFlush :: CString -> CInt -> IO CInt
foreign import ccall safe "sapiens_update_copy" cCopy :: CString -> CString -> CInt -> IO CInt
foreign import ccall safe "sapiens_update_metadata" cMetadata :: CString -> Ptr Word8 -> IO CInt
foreign import ccall safe "sapiens_update_error" cError :: CString -> IO ()
foreign import ccall safe "sapiens_update_activate" cActivate :: CString -> IO CInt
foreign import ccall unsafe "sapiens_update_hangup" cHangup :: IO CInt

check :: String -> CInt -> IO ()
check operation code = unless (code == 0) (fail (operation ++ " failed (macOS error " ++ show code ++ ")"))
acquireLease :: FilePath -> Bool -> IO Lease
acquireLease path exclusive = leaseWithMode path (if exclusive then 1 else 0)
leaseWithMode :: FilePath -> CInt -> IO Lease
leaseWithMode path mode = withCString path $ \p -> do
 fd <- cLease p mode
 when (fd < 0) (fail "Could not acquire the installation lease. Close other Sapiens instances and retry.")
 pure (Lease fd)
releaseLease :: Lease -> IO ()
releaseLease (Lease fd) = void (cClose fd)
withLease :: FilePath -> Bool -> (Lease -> IO a) -> IO a
withLease path exclusive = bracket (acquireLease path exclusive) releaseLease
withIdleLease :: FilePath -> IO a -> IO a
withIdleLease path = bracket (leaseWithMode path 2) releaseLease . const
downgradeLease :: Lease -> IO ()
downgradeLease (Lease fd) = cDowngrade fd >>= check "Lease handoff"
publish, publishNew :: FilePath -> FilePath -> B.ByteString -> IO ()
publish = publishWith 1
publishNew = publishWith 0
publishWith :: CInt -> FilePath -> FilePath -> B.ByteString -> IO ()
publishWith replace path temporary bytes = withCString path $ \p -> withCString temporary $ \t ->
 B.useAsCStringLen bytes $ \(buffer,len) -> cPublish p t (castPtr buffer) (fromIntegral len) replace >>= check "Durable update ownership or decision"
copyOwned, copyMetadata :: FilePath -> FilePath -> IO ()
copyOwned = copyWith 0
copyMetadata = copyWith 1
copyWith :: CInt -> FilePath -> FilePath -> IO ()
copyWith mode source destination = withCString source $ \s -> withCString destination $ \d -> cCopy s d mode >>= check "Owned file copy"
metadata :: FilePath -> IO B.ByteString
metadata path = withCString path $ \p -> allocaBytes 32 $ \buffer -> do
 cMetadata p buffer >>= check ("Owned metadata: " ++ path)
 B.packCStringLen (castPtr buffer,32)

showUpdateError :: String -> IO ()
showUpdateError message = hPutStrLn stderr message >> withCString message cError
validatePath :: FilePath -> IO ()
validatePath path = pathPresent path >>= \exists -> unless exists (fail ("Owned path is missing: " ++ path))

withRootIdentity :: FilePath -> (String -> IO a) -> IO a
withRootIdentity path action = do
 validatePath path
 bracket (withCString path $ \p -> alloca $ \handle -> allocaArray 2 $ \identity -> do
  cOpenRoot p handle identity >>= check ("Open Data or Config root: " ++ path)
  (,) <$> peek handle <*> (show <$> peekArray 2 identity)) (void . cClose . fst) (action . snd)
pathPresent :: FilePath -> IO Bool
pathPresent path = do
 let parent = takeDirectory (dropTrailingPathSeparator path)
 present <- if equalFilePath parent path then pure True else pathPresent parent
 if not present then pure False else do
  code <- withCString path cValidate
  if code == 2 then pure False else check ("Owned path validation: " ++ path) code >> pure True
containsPath :: FilePath -> FilePath -> Bool
containsPath parent path = let p = map toLower (normalise parent); q = map toLower (normalise path) in p == q || addTrailingPathSeparator p `isPrefixOf` q
profileExclusions :: [FilePath]
profileExclusions = []
payloadName :: String
payloadName = "Sapiens-Payload-mac-arm64.dmg"
versionExecutable :: FilePath -> Version -> FilePath
versionExecutable root version = versionPath root version </> "Sapiens4.app/Contents/MacOS/Sapiens4"
starterDirectory :: IO FilePath
starterDirectory = do
 override <- lookupEnv "SAPIENS_INSTALL_DIR"
 maybe ((</> "Library/Application Support/Sapiens4/installation") <$> getHomeDirectory) makeAbsolute override

appBundle :: FilePath -> FilePath
appBundle = takeDirectory . takeDirectory . takeDirectory

verifyBundle :: FilePath -> IO ()
verifyBundle path = callProcess "/usr/bin/codesign" ["--verify","--deep","--strict",path]

stagePayload :: FilePath -> FilePath -> IO ()
stagePayload payload destination = do
 createDirectory destination
 let mount = destination </> "mount"
 createDirectory mount
 bracket_ (void (readProcess "/usr/bin/hdiutil" ["attach","-readonly","-nobrowse","-mountpoint",mount,payload] ""))
  (void (readProcess "/usr/bin/hdiutil" ["detach",mount] "")) $ do
   let source = mount </> "Sapiens4.app"
   verifyBundle source
   callProcess "/usr/bin/ditto" [source,destination </> "Sapiens4.app"]
 removeDirectory mount
 let app = destination </> "Sapiens4.app"
 verifyBundle app
 ident <- readProcess "/usr/libexec/PlistBuddy" ["-c","Print :CFBundleIdentifier",app </> "Contents/Info.plist"] ""
 unless (lines ident == ["com.sapiens4.desktop"]) (fail "Unexpected application identity")
 release <- readProcess "/usr/libexec/PlistBuddy" ["-c","Print :CFBundleShortVersionString",app </> "Contents/Info.plist"] ""
 writeFile (destination </> "sapiens-release.version") release
 let flush path full = withCString path (\p -> cFlush p full >>= check "Staged bundle durability")
     walk path = do
      validatePath path
      directory <- doesDirectoryExist path
      when directory (listDirectory path >>= mapM_ (walk . (path </>)))
      flush path 0
 walk destination
 flush (takeDirectory destination) 1

launchVersion :: FilePath -> Version -> [String] -> IO ()
launchVersion root version args = do
 let executable = versionExecutable root version
 verifyBundle (appBundle executable)
 active <- if null args then withCString executable cActivate else pure 0
 inherited <- getEnvironment
 when (active == 0) $ void $ createProcess (proc executable args)
  {env=Just (filter ((/= "SAPIENS_UPDATE_ATTEMPT") . fst) inherited),close_fds=True,new_session=True}

data Candidate = Candidate ProcessHandle Handle Handle
withCandidate :: FilePath -> [String] -> [(String,String)] -> (Candidate -> IO a) -> IO a
withCandidate executable args overrides = bracket acquire close
 where
 acquire = do
  inherited <- getEnvironment
  (Just input,Just output,_,child) <- createProcess (proc executable args)
   {std_in=CreatePipe,std_out=CreatePipe,std_err=Inherit,close_fds=True,new_session=True
   ,env=Just (overrides ++ filter ((`notElem` map fst overrides) . fst) inherited)}
  mapM_ (\stream -> hSetEncoding stream utf8 >> hSetNewlineMode stream universalNewlineMode) [input,output]
  pure (Candidate child input output)
 close (Candidate _ input output) = mapM_ (\h -> hClose h `catch` \(_::IOException) -> pure ()) [input,output]
candidateMessage :: Candidate -> IO String
candidateMessage (Candidate _ _ output) = hGetLine output
candidateAlive :: Candidate -> IO Bool
candidateAlive (Candidate child _ _) = (==Nothing) <$> getProcessExitCode child
sendCandidate :: Candidate -> String -> IO ()
sendCandidate (Candidate _ input _) message = hPutStrLn input message >> hFlush input
candidateOwnership :: Candidate -> Bool -> IO ()
candidateOwnership (Candidate child _ _) commit = unless commit $ do
 terminateProcess child
 done <- timeout 10000000 (waitForProcess child)
 when (done == Nothing) $ do
  getPid child >>= mapM_ (signalProcess sigKILL)
  joined <- timeout 10000000 (waitForProcess child)
  when (joined == Nothing) (fail "Candidate process did not exit after termination")

watchCoordinator :: IO ()
watchCoordinator = do
 candidate <- lookupEnv "SAPIENS_UPDATE_ATTEMPT"
 forM_ candidate $ \_ -> void $ async $ do
  let wait = cHangup >>= \closed -> if closed == 0 then threadDelay 100000 >> wait else pure ()
  wait
  installed <- getExecutablePath >>= installationOf
  selected <- (case installed of
   Nothing -> pure False
   Just (root,version) -> readUpdateState root >>= \state -> pure $ case state of
    Settled s -> current s == version
    Pending _ -> False) `catch` \(_::IOException) -> pure False
  unless selected (signalProcess sigTERM =<< getProcessID)
