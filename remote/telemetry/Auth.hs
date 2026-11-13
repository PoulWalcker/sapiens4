{-# LANGUAGE OverloadedStrings #-}

module Auth
  ( AuthPrincipal
  , AuthError (..)
  , BasicCredentials
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
  , verifyBasic
  ) where

import Control.Exception (IOException, bracket, try)
import Crypto.Hash (SHA256)
import Crypto.MAC.HMAC (HMAC, hmac)
import Data.Aeson (Value, object, (.=))
import Data.Bits ((.&.))
import Data.ByteArray (ScrubbedBytes)
import qualified Data.ByteArray as BA
import Data.ByteArray.Encoding (Base (Base64, Base64URLUnpadded), convertFromBase, convertToBase)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL
import Data.Char (toLower)
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

newtype BasicPassword = BasicPassword ScrubbedBytes

newtype InstallationId = InstallationId UUID

newtype ServerKey = ServerKey ScrubbedBytes

newtype AuthPrincipal = AuthenticatedInstallation InstallationId

data BasicCredentials = BasicCredentials InstallationId BasicPassword

data AuthError = MissingBasic | InvalidBasic
  deriving (Eq, Show)

data ServerKeyError
  = KeyReadFailed
  | InvalidKeyLength
  | InsecureKeyFile
  deriving (Eq, Show)

decodeCanonical :: Base -> BS.ByteString -> Either AuthError BS.ByteString
decodeCanonical base encoded = do
  decoded <-
    case convertFromBase base encoded of
      Left _ -> Left InvalidBasic
      Right bytes -> Right bytes
  if convertToBase base decoded == encoded
    then Right decoded
    else Left InvalidBasic

deriveMac :: ServerKey -> InstallationId -> HMAC SHA256
deriveMac (ServerKey key) (InstallationId uuid) =
  hmac key message
  where
    message =
      "sapiens4.telemetry.basic.v1\0"
      <> BL.toStrict (UUID.toByteString uuid)

derivePassword :: ServerKey -> InstallationId -> BasicPassword
derivePassword key identifier =
  BasicPassword (BA.convert ("v1." <> encoded))
  where
    encoded =
      convertToBase Base64URLUnpadded (deriveMac key identifier) :: BS.ByteString


enrollmentJSON :: BasicCredentials -> Value
enrollmentJSON (BasicCredentials identifier (BasicPassword password)) =
  object
    [ "protocol_version" .= (1 :: Int)
    , "installation_id"  .= installationIdText identifier
    , "basic_password"   .= B8.unpack (BA.convert password)
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

parseBasicPayload :: BS.ByteString -> Either AuthError (InstallationId, BS.ByteString)
parseBasicPayload encoded = do
  decoded <- decodeCanonical Base64 encoded
  let (username, passwordWithColon) = B8.break (== ':') decoded
  identifier <- parseInstallationId username
  encodedMac <-
    case BS.stripPrefix ":v1." passwordWithColon of
      Nothing -> Left InvalidBasic
      Just bytes -> Right bytes
  mac <- decodeCanonical Base64URLUnpadded encodedMac
  if BS.length mac == 32
    then Right (identifier, mac)
    else Left InvalidBasic

parseInstallationId :: BS.ByteString -> Either AuthError InstallationId
parseInstallationId username =
  case UUID.fromASCIIBytes username of
    Nothing -> Left InvalidBasic
    Just identifier ->
      let (_, versionWord, variantWord, _) = UUID.toWords identifier
          canonical = UUID.toASCIIBytes identifier == username
          version4 = (versionWord .&. 0xf000) == 0x4000
          rfcVariant = (variantWord .&. 0xc0000000) == 0x80000000
      in
        if canonical && version4 && rfcVariant
          then Right (InstallationId identifier)
          else Left InvalidBasic

verifyBasic :: ServerKey -> BS.ByteString -> Either AuthError AuthPrincipal
verifyBasic key header
  | BS.null header = Left MissingBasic
  | BS.length header > 256 = Left InvalidBasic
  | otherwise = do
      let (scheme, suffix) = B8.break (== ' ') header
      payload <-
        if B8.map toLower scheme == "basic" && not (BS.null suffix)
          then Right (BS.dropWhile (== 32) suffix)
          else Left InvalidBasic
      (identifier, suppliedMac) <- parseBasicPayload payload
      if BA.constEq suppliedMac (deriveMac key identifier)
        then Right (AuthenticatedInstallation identifier)
        else Left InvalidBasic
