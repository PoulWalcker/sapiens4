{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module TelemetryWindows
  ( checkPrivate, createPrivateDirectory, createPrivateLock, replaceProfile
  ) where

import Control.Exception (IOException, bracket, catch)
import Control.Monad (forM, unless, void)
import Data.Bits ((.&.), (.|.))
import Data.Word (Word8, Word16, Word32)
import Foreign
  ( Ptr, alloca, allocaArray, allocaBytes, castPtr, nullPtr, peek, peekByteOff
  , peekElemOff, plusPtr, poke, sizeOf, with
  )
import Foreign.C.String (CWString, peekCWString, withCWString)
import Foreign.C.Types (CInt (..))
import System.IO.Error (isAlreadyExistsError)
import qualified System.Win32.File as Win
import System.Win32.Types (DWORD, HANDLE)

#if defined(i386_HOST_ARCH)
#define WINDOWS_CALL stdcall
#else
#define WINDOWS_CALL ccall
#endif

foreign import WINDOWS_CALL unsafe "GetCurrentProcess"
  currentProcess :: IO HANDLE
foreign import WINDOWS_CALL unsafe "OpenProcessToken"
  openProcessToken :: HANDLE -> DWORD -> Ptr HANDLE -> IO CInt
foreign import WINDOWS_CALL unsafe "GetTokenInformation"
  tokenInformation :: HANDLE -> CInt -> Ptr () -> DWORD -> Ptr DWORD -> IO CInt
foreign import WINDOWS_CALL unsafe "ConvertSidToStringSidW"
  sidString :: Ptr () -> Ptr CWString -> IO CInt
foreign import WINDOWS_CALL unsafe "ConvertStringSecurityDescriptorToSecurityDescriptorW"
  stringDescriptor :: CWString -> DWORD -> Ptr (Ptr ()) -> Ptr DWORD -> IO CInt
foreign import WINDOWS_CALL unsafe "GetSecurityInfo"
  securityInfo :: HANDLE -> CInt -> DWORD -> Ptr (Ptr ()) -> Ptr (Ptr ())
               -> Ptr (Ptr ()) -> Ptr (Ptr ()) -> Ptr (Ptr ()) -> IO DWORD
foreign import WINDOWS_CALL unsafe "LocalFree"
  localFree :: Ptr a -> IO (Ptr a)
foreign import WINDOWS_CALL unsafe "IsValidAcl"
  validAcl :: Ptr () -> IO CInt
foreign import WINDOWS_CALL unsafe "GetAclInformation"
  aclInformation :: Ptr () -> Ptr Word32 -> DWORD -> CInt -> IO CInt
foreign import WINDOWS_CALL unsafe "GetAce"
  getAce :: Ptr () -> DWORD -> Ptr (Ptr ()) -> IO CInt
foreign import WINDOWS_CALL unsafe "IsValidSid"
  validSid :: Ptr () -> IO CInt
foreign import WINDOWS_CALL unsafe "GetLengthSid"
  lengthSid :: Ptr () -> IO DWORD
foreign import WINDOWS_CALL unsafe "EqualSid"
  equalSid :: Ptr () -> Ptr () -> IO CInt

checkBool :: IO CInt -> IO ()
checkBool operation = operation >>= \ok -> unless (ok /= 0) $
  ioError (userError "Telemetry Windows filesystem operation failed")

withLocal :: (Ptr (Ptr a) -> IO ()) -> (Ptr a -> IO b) -> IO b
withLocal acquire = bracket
  (alloca $ \out -> acquire out >> peek out)
  (void . localFree)

withUser :: (Ptr () -> IO a) -> IO a
withUser action = bracket open Win.closeHandle $ \token -> alloca $ \needed -> do
  poke needed 0
  -- TOKEN_QUERY = 8; TokenUser = 1. The first call obtains the buffer size.
  void (tokenInformation token 1 nullPtr 0 needed)
  size <- peek needed
  unless (size > 0) (ioError (userError "Telemetry token information unavailable"))
  allocaBytes (fromIntegral size) $ \buffer -> do
    checkBool (tokenInformation token 1 buffer size needed)
    -- TOKEN_USER begins with SID_AND_ATTRIBUTES, whose first member is PSID.
    user <- peek (castPtr buffer)
    action user
  where
    open = alloca $ \out -> do
      process <- currentProcess
      checkBool (openProcessToken process 8 out)
      peek out

withAttributes :: Ptr () -> (Ptr Win.SECURITY_ATTRIBUTES -> IO a) -> IO a
withAttributes user action = do
  sid <- withLocal (\out -> checkBool (sidString user out)) peekCWString
  let sddl = "O:" ++ sid ++ "D:P(A;OICI;FA;;;" ++ sid ++ ")"
  withCWString sddl $ \text ->
    withLocal (\out -> checkBool (stringDescriptor text 1 out nullPtr)) $ \descriptor -> do
      let attributes = Win.SECURITY_ATTRIBUTES
            (fromIntegral (sizeOf (undefined :: Win.SECURITY_ATTRIBUTES))) descriptor False
      with attributes action

createPrivateDirectory :: FilePath -> IO ()
createPrivateDirectory path = withUser $ \user -> withAttributes user $ \attributes ->
  Win.createDirectory path (Just attributes) `catch` \err ->
    unless (isAlreadyExistsError (err :: IOException)) (ioError err)

createPrivateLock :: FilePath -> IO ()
createPrivateLock path = withUser $ \user -> withAttributes user $ \attributes ->
  bracket (openFile path (Just attributes) Win.oPEN_ALWAYS)
    Win.closeHandle (const (pure ()))

openFile :: FilePath -> Maybe Win.LPSECURITY_ATTRIBUTES -> Win.CreateMode -> IO HANDLE
openFile path attributes mode = Win.createFile path
  (Win.rEAD_CONTROL .|. Win.fILE_READ_ATTRIBUTES)
  (Win.fILE_SHARE_READ .|. Win.fILE_SHARE_WRITE .|. Win.fILE_SHARE_DELETE)
  attributes mode
  -- FILE_FLAG_OPEN_REPARSE_POINT is absent from older Win32 exports.
  (Win.fILE_FLAG_BACKUP_SEMANTICS .|. 0x00200000) Nothing

checkPrivate :: FilePath -> Bool -> IO Bool
checkPrivate path directory = withUser $ \user ->
  bracket (openFile path Nothing Win.oPEN_EXISTING) Win.closeHandle $ \file -> do
    info <- Win.getFileInformationByHandle file
    let attributes = Win.bhfiFileAttributes info
        isDirectory = attributes .&. Win.fILE_ATTRIBUTE_DIRECTORY /= 0
    if attributes .&. Win.fILE_ATTRIBUTE_REPARSE_POINT /= 0
        || isDirectory /= directory || not directory && Win.bhfiNumberOfLinks info /= 1
      then pure False
      else ownerOnly file user

ownerOnly :: HANDLE -> Ptr () -> IO Bool
ownerOnly file user = alloca $ \ownerOut -> alloca $ \aclOut ->
  withLocal (acquire ownerOut aclOut) $ \_ -> do
    owner <- peek ownerOut
    acl <- peek aclOut
    sameOwner <- if owner == nullPtr then pure 0 else equalSid owner user
    valid <- if acl == nullPtr then pure 0 else validAcl acl
    if sameOwner == 0 || valid == 0 then pure False else onlyUserAces acl user
  where
    acquire owner acl descriptor = do
      -- SE_FILE_OBJECT = 1; OWNER_SECURITY_INFORMATION | DACL_SECURITY_INFORMATION = 5.
      result <- securityInfo file 1 5 owner nullPtr acl nullPtr descriptor
      unless (result == 0) (ioError (userError "Telemetry security descriptor unavailable"))

onlyUserAces :: Ptr () -> Ptr () -> IO Bool
onlyUserAces acl user = allocaArray 3 $ \information -> do
  -- ACL_SIZE_INFORMATION consists of three DWORDs; AclSizeInformation = 2.
  checkBool (aclInformation acl information 12 2)
  count <- peekElemOff information 0
  entries <- forM [1 .. count] $ \index -> alloca $ \out -> do
    checkBool (getAce acl (index - 1) out)
    ace <- peek out
    kind <- peekByteOff ace 0 :: IO Word8
    case kind of
      0 -> do -- ACCESS_ALLOWED_ACE
        flags <- peekByteOff ace 1 :: IO Word8
        size <- peekByteOff ace 2 :: IO Word16
        if size < 16 then pure (False, False) else do
          -- ACE_HEADER is BYTE, BYTE, WORD; DWORD Mask follows; SID starts at byte 8.
          let sid = ace `plusPtr` 8
          valid <- validSid sid
          lengthBytes <- if valid == 0 then pure 0 else lengthSid sid
          same <- if lengthBytes == 0 || lengthBytes > fromIntegral size - 8
            then pure 0 else equalSid sid user
          rights <- peekByteOff ace 4 :: IO Word32
          pure (same /= 0, rights /= 0 && flags .&. 0x08 == 0)
      1 -> pure (True, False) -- Deny ACEs grant no access to another principal.
      _ -> pure (False, False) -- Unsupported ACE types fail closed.
  pure (all fst entries && any snd entries)

replaceProfile :: FilePath -> FilePath -> IO ()
replaceProfile source destination = Win.moveFileEx source (Just destination) Win.mOVEFILE_REPLACE_EXISTING
