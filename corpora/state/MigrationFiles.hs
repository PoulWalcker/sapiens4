{-# LANGUAGE CPP, ForeignFunctionInterface #-}
module MigrationFiles
  ( privateDirectory, fileIdentity, describeFile, copyEntry, preserveAccess, syncEntry, durableMove ) where

import Control.Exception
import Control.Monad
import System.Directory
import System.FilePath
#ifdef mingw32_HOST_OS
import Control.Concurrent (threadDelay)
import Data.Bits
import Data.Word
import Foreign hiding (void)
import Foreign.C.String
import System.Environment (getEnvironment)
import System.Exit
import System.Process
import qualified System.Win32.File as W
import qualified System.Win32.Types as W

withNode path access = bracket
  (W.createFile path access (W.fILE_SHARE_READ .|. W.fILE_SHARE_WRITE .|. W.fILE_SHARE_DELETE)
    Nothing W.oPEN_EXISTING (W.fILE_FLAG_BACKUP_SEMANTICS .|. 0x00200000) Nothing) W.closeHandle

fileIdentity path = withNode path 0 $ \h -> do
  info <- W.getFileInformationByHandle h
  pure (show (W.bhfiVolumeSerialNumber info, W.bhfiFileIndex info))

-- Read the self-relative owner, group and DACL without the audit privilege.
foreign import stdcall unsafe "GetFileSecurityW" getSecurity
  :: CWString -> Word32 -> Ptr Word8 -> Word32 -> Ptr Word32 -> IO Int32
foreign import stdcall unsafe "SetFileSecurityW" setSecurity
  :: CWString -> Word32 -> Ptr Word8 -> IO Int32
foreign import stdcall unsafe "GetSecurityDescriptorControl" getControl
  :: Ptr Word8 -> Ptr Word16 -> Ptr Word32 -> IO Int32
foreign import stdcall unsafe "SetSecurityDescriptorControl" setControl
  :: Ptr Word8 -> Word16 -> Word16 -> IO Int32
foreign import stdcall unsafe "ConvertSecurityDescriptorToStringSecurityDescriptorW" securityText
  :: Ptr Word8 -> Word32 -> Word32 -> Ptr CWString -> Ptr Word32 -> IO Int32
foreign import stdcall unsafe "LocalFree" localFree :: Ptr a -> IO (Ptr a)
foreign import stdcall unsafe "MoveFileExW" moveFile
  :: CWString -> CWString -> Word32 -> IO Int32

withSecurity path use = withCWString path $ \name -> alloca $ \size -> do
  poke size 0
  _ <- getSecurity name 7 nullPtr 0 size
  count <- peek size
  when (count == 0) $ W.failWith ("GetFileSecurity " ++ path) =<< W.getLastError
  allocaBytes (fromIntegral count) $ \buffer -> do
    W.failIfFalse_ ("GetFileSecurity " ++ path) $ (/=0) <$> getSecurity name 7 buffer count size
    use buffer

descriptor fields sd = alloca $ \result -> do
  W.failIfFalse_ "ConvertSecurityDescriptorToStringSecurityDescriptor" $
    (/=0) <$> securityText sd 1 fields result nullPtr
  bracket (peek result) localFree peekCWString

describeFile path = withNode path 0 $ \h -> do
  info <- W.getFileInformationByHandle h
  let flags = W.bhfiFileAttributes info
      folder = flags .&. W.fILE_ATTRIBUTE_DIRECTORY /= 0
  when (flags .&. W.fILE_ATTRIBUTE_REPARSE_POINT /= 0 || not folder && W.bhfiNumberOfLinks info /= 1) $
    fail ("Cannot migrate reparse points or multiply linked files: " ++ path)
  acl <- withSecurity path (descriptor 7)
  pure (if folder then 'd' else 'f', show (flags .&. 7) ++ " " ++ acl)

privateDirectory path = do
  env <- getEnvironment
  let command = unwords
        ["$ErrorActionPreference='Stop'; $d=New-Object System.Security.AccessControl.DirectorySecurity;"
        ,"$id=[Security.Principal.WindowsIdentity]::GetCurrent().User;"
        ,"$d.SetAccessRuleProtection($true,$false);"
        ,"$d.AddAccessRule([Security.AccessControl.FileSystemAccessRule]::new($id,'FullControl','ContainerInherit,ObjectInherit','None','Allow'));"
        ,"if ([IO.Directory]::Exists($env:SAPIENS_MIGRATION_DIRECTORY)) {exit 2};"
        ,"[void][IO.Directory]::CreateDirectory($env:SAPIENS_MIGRATION_DIRECTORY,$d)"]
  (code, _, err) <- readCreateProcessWithExitCode ((proc "powershell.exe" ["-NoProfile","-NonInteractive","-Command",command])
    { env = Just (("SAPIENS_MIGRATION_DIRECTORY",path):env) }) ""
  unless (code == ExitSuccess) $ fail ("Cannot create private directory " ++ path ++ ": " ++ err)

copyEntry dir src dst = do
  unless dir $ W.copyFile src dst True
  preserveAccess src dst

preserveAccess src dst = do
  withSecurity src $ \original -> withSecurity dst $ \current -> do
    wanted <- descriptor 7 original
    previous <- descriptor 7 current
    when (wanted /= previous) $ do
      owner <- descriptor 3 original
      existing <- descriptor 3 current
      -- NTFS clears AUTO_INHERITED on assignment unless the descriptor
      -- also requests automatic inheritance. Preserve the source's choice.
      alloca $ \flags -> alloca $ \revision -> do
        W.failIfFalse_ "GetSecurityDescriptorControl" $ (/=0) <$> getControl original flags revision
        inherited <- (.&. 0x0400) <$> peek flags
        when (inherited /= 0) $ W.failIfFalse_ "SetSecurityDescriptorControl" $
          (/=0) <$> setControl original 0x0100 0x0100
      withCWString dst $ \name -> W.failIfFalse_ ("SetFileSecurity " ++ dst) $
        (/=0) <$> setSecurity name (if owner == existing then 4 else 7) original
  W.getFileAttributes src >>= W.setFileAttributes dst

syncEntry dir path = unless dir $ withNode path 0x40000000 W.flushFileBuffers
durableMove src dst = withCWString src $ \s -> withCWString dst $ \d -> do
  let move attempts = do
        result <- moveFile s d (W.mOVEFILE_REPLACE_EXISTING .|. 8)
        when (result == 0) $ do
          code <- W.getLastError
          -- Win32's wrapper retries only sharing violations; a reader denying
          -- delete sharing can also produce access denied during replacement.
          if code `elem` [5,32] && attempts > (0 :: Int)
            then threadDelay 100000 >> move (attempts-1)
            else W.failWith ("MoveFileEx " ++ src ++ " -> " ++ dst) code
  move 20
#else
import qualified System.Posix as P
#ifdef darwin_HOST_OS
import Data.Bits
import Foreign.C
import Foreign.Ptr
import System.Posix.Internals (withFilePath)
foreign import ccall safe "copyfile" copyfile :: CString -> CString -> Ptr () -> CUInt -> IO CInt
foreign import ccall unsafe "acl_get_link_np" getAcl :: CString -> CInt -> IO (Ptr ())
foreign import ccall unsafe "acl_to_text" aclText :: Ptr () -> Ptr CLong -> IO CString
foreign import ccall unsafe "acl_free" freeAcl :: Ptr a -> IO CInt

accessRules path = withFilePath path $ \p -> do
  acl <- getAcl p 0x100 -- ACL_TYPE_EXTENDED
  if acl == nullPtr then do
    err <- getErrno
    if err == eNOENT then pure "" else ioError (errnoToIOError "acl_get_link_np" err Nothing (Just path))
  else bracket (pure acl) freeAcl $ \a ->
    bracket (throwErrnoPathIfNull "acl_to_text" path $ aclText a nullPtr) freeAcl peekCString

copyEntry dir src dst = withFilePath src $ \s -> withFilePath dst $ \d ->
  -- COPYFILE_NOFOLLOW, plus METADATA for directories or ALL|EXCL for a new leaf.
  throwErrnoPathIfMinus1_ "copyfile" dst $
    copyfile s d nullPtr (0xc0000 .|. if dir then 7 else 0x2000f)
#else
import System.Process (readProcess)
accessRules path = readProcess "getfacl" ["-cpn","--",path] ""
copyEntry dir src dst
  | dir = do
      preserveAccess src dst
  | otherwise = void $ readProcess "cp" ["-a","--",src,dst] ""
#endif
preserveAccess src dst = do
  status <- P.getFileStatus src
  P.setOwnerAndGroup dst (P.fileOwner status) (P.fileGroup status)
  P.setFileMode dst (P.fileMode status)
#ifdef darwin_HOST_OS
  withFilePath src $ \s -> withFilePath dst $ \d ->
    throwErrnoPathIfMinus1_ "copyfile ACL" dst $ copyfile s d nullPtr 0xc0001
#else
  accessRules src >>= void . readProcess "setfacl" ["--set-file=-","--",dst]
#endif
describeFile path = do
  s <- P.getSymbolicLinkStatus path
  kind <- if P.isDirectory s then pure 'd' else if P.isSymbolicLink s then pure 'l'
          else if P.isRegularFile s && P.linkCount s == 1 then pure 'f'
          else fail ("Cannot migrate special or multiply linked file: " ++ path)
  acl <- accessRules path
  pure (kind, show (P.fileMode s,P.fileOwner s,P.fileGroup s) ++ " " ++ acl)

privateDirectory path = P.createDirectory path 0o700
fileIdentity path = do
  s <- P.getSymbolicLinkStatus path
  pure (show (P.deviceID s,P.fileID s))
syncEntry _ path = bracket (P.openFd path P.ReadOnly P.defaultFileFlags) P.closeFd P.fileSynchronise
durableMove src dst = do
  renameFile src dst
  syncEntry True (takeDirectory dst)
  when (takeDirectory src /= takeDirectory dst) $ syncEntry True (takeDirectory src)
#endif

privateDirectory :: FilePath -> IO ()
fileIdentity :: FilePath -> IO String
describeFile :: FilePath -> IO (Char,String)
copyEntry :: Bool -> FilePath -> FilePath -> IO ()
preserveAccess :: FilePath -> FilePath -> IO ()
syncEntry :: Bool -> FilePath -> IO ()
durableMove :: FilePath -> FilePath -> IO ()
