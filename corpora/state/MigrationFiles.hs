{-# LANGUAGE CPP, ForeignFunctionInterface #-}
module MigrationFiles
  ( privateDirectory, fileIdentity, snapshotTree, restoreTree, syncTree, preserveAccess, syncEntry, durableMove ) where

import Control.Exception
import Control.Monad
import System.Directory
import System.FilePath
import System.Exit
import System.Process
import System.IO.Error (isDoesNotExistError)
#ifdef mingw32_HOST_OS
import Control.Concurrent (threadDelay)
import Data.Bits
import Data.List (isPrefixOf)
import Data.Word
import Foreign hiding (void)
import Foreign.C.String
import System.Environment (getEnvironment)
import qualified System.Win32.File as W
import qualified System.Win32.Types as W

-- Backup paths can exceed MAX_PATH even when the original Data Root does not.
nativePath path = do
  absolute <- normalise <$> makeAbsolute path
  pure $ if "\\\\?\\" `isPrefixOf` absolute then absolute
    else if "\\\\" `isPrefixOf` absolute then "\\\\?\\UNC\\" ++ drop 2 absolute
    else "\\\\?\\" ++ absolute

withNode path access use = do
  native <- nativePath path
  bracket (W.createFile native access (W.fILE_SHARE_READ .|. W.fILE_SHARE_WRITE .|. W.fILE_SHARE_DELETE)
    Nothing W.oPEN_EXISTING (W.fILE_FLAG_BACKUP_SEMANTICS .|. 0x00200000) Nothing) W.closeHandle use

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

withSecurity path use = nativePath path >>= \native -> withCWString native $ \name -> alloca $ \size -> do
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

entryKind path = withNode path 0 $ \h -> do
  info <- W.getFileInformationByHandle h
  let flags = W.bhfiFileAttributes info
      directory = flags .&. W.fILE_ATTRIBUTE_DIRECTORY /= 0
  when (flags .&. W.fILE_ATTRIBUTE_REPARSE_POINT /= 0 || not directory && W.bhfiNumberOfLinks info /= 1) $
    fail ("Cannot preserve reparse points or multiply linked files: " ++ path)
  pure (if directory then 'd' else 'f')

copyEntry directory src dst = do
  unless directory $ copyFileWithMetadata src dst
  preserveAccess src dst

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

preserveAccess source target = do
  src <- nativePath source
  dst <- nativePath target
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

syncEntry dir path = unless dir $ do
  native <- nativePath path
  flags <- W.getFileAttributes native
  let readonly = flags .&. W.fILE_ATTRIBUTE_READONLY /= 0
      restoreFlags = when readonly (W.setFileAttributes native flags)
  mask $ \restore -> do
    when readonly $ W.setFileAttributes native (flags .&. complement W.fILE_ATTRIBUTE_READONLY)
    (withNode native 0x40000000 $ \handle -> restoreFlags >> restore (W.flushFileBuffers handle))
      `onException` restoreFlags
durableMove source target = do
  src <- nativePath source
  dst <- nativePath target
  withCWString src $ \s -> withCWString dst $ \d -> move s d 20
  where
    move s d attempts = do
        result <- moveFile s d (W.mOVEFILE_REPLACE_EXISTING .|. 8)
        when (result == 0) $ do
          code <- W.getLastError
          -- Win32's wrapper retries only sharing violations; a reader denying
          -- delete sharing can also produce access denied during replacement.
          if code `elem` [5,32] && attempts > (0 :: Int)
            then threadDelay 100000 >> move s d (attempts-1)
            else W.failWith ("MoveFileEx " ++ source ++ " -> " ++ target) code
#else
import qualified System.Posix as P
#ifdef darwin_HOST_OS
import Foreign.C
import Foreign.Ptr
import System.Posix.Internals (withFilePath)
foreign import ccall safe "copyfile" copyfile :: CString -> CString -> Ptr () -> CUInt -> IO CInt
copyEntry directory src dst = withFilePath src $ \s -> withFilePath dst $ \d ->
  throwErrnoPathIfMinus1_ "copyfile" dst $
    copyfile s d nullPtr (if directory then 0xc0007 else 0xe000f)
#else
copyEntry directory src dst
  | directory = preserveAccess src dst
  | otherwise = void $ readProcess "cp" ["-a","--",src,dst] ""
accessRules path = readProcess "getfacl" ["-cpn","--",path] ""
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
entryKind path = do
  status <- P.getSymbolicLinkStatus path
  if P.isDirectory status then pure 'd'
  else if P.isSymbolicLink status then pure 'l'
  else if P.isRegularFile status && P.linkCount status == 1 then pure 'f'
  else fail ("Cannot preserve special or multiply linked files: " ++ path)

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
preserveAccess :: FilePath -> FilePath -> IO ()
syncEntry :: Bool -> FilePath -> IO ()
durableMove :: FilePath -> FilePath -> IO ()

-- Snapshot destinations are unpublished, disposable preparations until the
-- caller commits its receipt. A copy outliving its caller cannot modify live data.
snapshotTree :: [FilePath] -> FilePath -> FilePath -> IO ()
snapshotTree locks source target = do
  let check path = do
        kind <- entryKind path
        when (kind == 'd') $ listDirectory path >>= mapM_ (check . (path </>))
  check source
#ifdef mingw32_HOST_OS
  let command = proc "robocopy.exe" ([source,target,"/E",
        "/COPY:DATSO","/DCOPY:DAT","/SECFIX","/IS","/IT","/R:0","/W:0","/SL","/SJ","/XF"] ++
        [base </> path | base <- [source,target], path <- locks])
      success ExitSuccess = True
      success (ExitFailure n) = n < 8
#else
  let command = proc "/usr/bin/rsync" ([
#ifdef darwin_HOST_OS
        "-aE",
#else
        "-aAX",
#endif
        "--ignore-times"] ++
        ["--exclude=/" ++ path | path <- locks] ++
        [addTrailingPathSeparator source,addTrailingPathSeparator target])
      success code = code == ExitSuccess
#endif
  (code,out,err) <- readCreateProcessWithExitCode command {use_process_jobs = True} ""
  unless (success code) $ fail ("Data copy failed: " ++ out ++ err)
  syncTree locks target

-- Flush the completed tree before publishing a migration or restoration result.
-- Native copy owns reconciliation; this traversal neither inventories nor edits it.
syncTree :: [FilePath] -> FilePath -> IO ()
syncTree locks root = walk "" where
  walk relative = unless (relative `elem` locks) $ do
    let path = root </> relative
    kind <- entryKind path
    when (kind /= 'l') $ do
      let directory = kind == 'd'
      when directory $ listDirectory path >>= mapM_ (walk . (relative </>))
      syncEntry directory path

-- Only restoration writes a live tree. Keep those writes in the lock-owning
-- process: an external mirror could outlive it and race a subsequent recovery.
-- Protect exact lock files and their ancestors, without excluding child contents.
restoreTree :: [FilePath] -> FilePath -> FilePath -> IO ()
restoreTree locks source target = copyDirectory "" where
  protected path = any (\lock -> path == lock || addTrailingPathSeparator path `prefixOf` lock) locks
  prefixOf prefix value = take (length prefix) value == prefix
  erase relative = unless (relative `elem` locks) $ do
    let path = target </> relative
    kind <- entryKind path
    if kind == 'd' then do
      listDirectory path >>= mapM_ (erase . (relative </>))
      unless (protected relative) (removeDirectory path)
    else removePathForcibly path
  copyDirectory relative = do
    let from = source </> relative; to = target </> relative
    names <- listDirectory from
    existing <- listDirectory to
    mapM_ (erase . (relative </>)) [name | name <- existing, name `notElem` names]
    forM_ names $ \name -> do
      let rel = relative </> name; src = source </> rel; dst = target </> rel
      unless (rel `elem` locks) $ do
        kind <- entryKind src
        present <- catchJust (\e -> if isDoesNotExistError e then Just () else Nothing)
          (Just <$> entryKind dst) (const $ pure Nothing)
        forM_ present $ \current -> do
          unless (kind == 'd' && current == 'd') (erase rel)
        if kind == 'd' then createDirectoryIfMissing False dst >> copyDirectory rel
        else copyEntry False src dst >> when (kind == 'f') (syncEntry False dst)
    copyEntry True from to
    syncEntry True to
