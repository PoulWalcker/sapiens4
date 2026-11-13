{-# LANGUAGE OverloadedStrings #-}

module Auth
  ( BasicCredentials
  , BasicPassword
  , InstallationId
  , ServerKey
  , ServerKeyError (..)
  , derivePassword
  , enrollmentJSON
  , installationIdText
  , issueCredentials
  , loadServerKey
  , mkServerKey
  , newInstallationId
  ) where

import Control.Exception (IOException, bracket, try)
import Crypto.Hash (SHA256)
import Crypto.MAC.HMAC (HMAC, hmac)
import Data.Aeson (Value, object, (.=))
import Data.Bits ((.&.))
import Data.ByteArray (ScrubbedBytes)
import qualified Data.ByteArray as BA
import Data.ByteArray.Encoding (Base (Base64URLUnpadded), convertToBase)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUIDv4
import System.Posix.Files (FileStatus, fileMode, fileOwner, getFdStatus, isRegularFile)
import System.Posix.IO
  ( OpenMode (ReadOnly)
  , closeFd
  , defaultFileFlags
  , fdReadBuf
  , nonBlock
  , openFd
  )
import System.Posix.Types (UserID)
import System.Posix.User (getEffectiveUserID)

data BasicCredentials = BasicCredentials InstallationId BasicPassword

newtype BasicPassword = BasicPassword ScrubbedBytes

newtype InstallationId = InstallationId UUID

newtype ServerKey = ServerKey ScrubbedBytes

data ServerKeyError
  = KeyReadFailed
  | InvalidKeyLength
  | InsecureKeyFile
  deriving (Eq, Show)

derivePassword :: ServerKey -> InstallationId -> BasicPassword
derivePassword (ServerKey key) (InstallationId uuid) =
  BasicPassword (BA.convert ("v1." <> encoded))
  where
    message =
      "sapiens4.telemetry.basic.v1\0"
        <> BL.toStrict (UUID.toByteString uuid)
    mac = hmac key message :: HMAC SHA256
    encoded = convertToBase Base64URLUnpadded mac :: BS.ByteString

enrollmentJSON :: BasicCredentials -> Value
enrollmentJSON (BasicCredentials identifier (BasicPassword password)) =
  object
    [ "protocol_version" .= (1 :: Int)
    , "installation_id" .= installationIdText identifier
    , "basic_password" .= B8.unpack (BA.convert password)
    ]

installationIdText :: InstallationId -> String
installationIdText (InstallationId uuid) = UUID.toString uuid

isPrivateKeyFile :: UserID -> FileStatus -> Bool
isPrivateKeyFile owner status =
  isRegularFile status
    && fileOwner status == owner
    && fileMode status .&. 0o077 == 0

issueCredentials :: ServerKey -> IO BasicCredentials
issueCredentials key = do
  identifier <- newInstallationId
  let password = derivePassword key identifier
  pure (BasicCredentials identifier password)

loadServerKey :: FilePath -> IO (Either ServerKeyError ServerKey)
loadServerKey path = do
  let flags = defaultFileFlags { nonBlock = True }
  result <- try @IOException $
    bracket (openFd path ReadOnly flags) closeFd $ \fd -> do
      status <- getFdStatus fd
      owner <- getEffectiveUserID
      if isPrivateKeyFile owner status
        then do
          (count, bytes) <- BA.allocRet 33 (\pointer -> fdReadBuf fd pointer 33)
          pure $ case mkServerKey (BA.take (fromIntegral count) bytes) of
            Nothing -> Left InvalidKeyLength
            Just key -> Right key
        else pure (Left InsecureKeyFile)
  pure $ case result of
    Left _ -> Left KeyReadFailed
    Right loaded -> loaded

mkServerKey :: ScrubbedBytes -> Maybe ServerKey
mkServerKey bytes
  | BA.length bytes == 32 = Just (ServerKey bytes)
  | otherwise = Nothing

newInstallationId :: IO InstallationId
newInstallationId = do
  uuid <- UUIDv4.nextRandom
  pure (InstallationId uuid)
