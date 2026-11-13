{-# LANGUAGE OverloadedStrings #-}

module Auth
    ( ServerKey
    , ServerKeyError(..)
    , mkServerKey
    , loadServerKey
    , InstallationId
    , newInstallationId
    , installationIdText
    , BasicPassword
    , derivePassword
    ) where

import Control.Exception (IOException, try)
import Data.ByteArray (ScrubbedBytes)
import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import System.IO (IOMode(ReadMode), withBinaryFile)
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUIDv4
import Crypto.Hash (SHA256)
import Crypto.MAC.HMAC (HMAC, hmac)
import Data.ByteArray.Encoding (Base(Base64URLUnpadded), convertToBase)
import qualified Data.ByteString.Lazy as BL

newtype InstallationId = InstallationId UUID
newtype ServerKey = ServerKey ScrubbedBytes
newtype BasicPassword = BasicPassword ScrubbedBytes

data ServerKeyError
  = KeyReadFailed
  | InvalidKeyLength
  deriving (Eq, Show)

newInstallationId :: IO InstallationId
newInstallationId = do
    uuid <- UUIDv4.nextRandom
    pure (InstallationId uuid)

installationIdText :: InstallationId -> String
installationIdText (InstallationId uuid) = UUID.toString uuid

mkServerKey :: ScrubbedBytes -> Maybe ServerKey
mkServerKey bytes
  | BA.length bytes == 32 = Just (ServerKey bytes)
  | otherwise = Nothing

loadServerKey :: FilePath -> IO (Either ServerKeyError ServerKey)
loadServerKey path = do
  result <- try @IOException $
    withBinaryFile path ReadMode (\handle -> BS.hGet handle 33)
  pure $ case result of
    Left _ -> Left KeyReadFailed
    Right bytes ->
      case mkServerKey (BA.convert bytes) of
        Nothing -> Left InvalidKeyLength
        Just key -> Right key

derivePassword :: ServerKey -> InstallationId -> BasicPassword
derivePassword (ServerKey key) (InstallationId uuid) =
    BasicPassword (BA.convert ("v1." <> encoded))
    where
        message =
            "sapiens4.telemetry.basic.v1\0"
            <> BL.toStrict (UUID.toByteString uuid)
        mac = hmac key message :: HMAC SHA256
        encoded = convertToBase Base64URLUnpadded mac :: BS.ByteString