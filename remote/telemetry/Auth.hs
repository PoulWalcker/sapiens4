module Auth
    ( ServerKey
    , ServerKeyError(..)
    , mkServerKey
    , loadServerKey
    , InstallationId
    , newInstallationId
    , installationIdText
    ) where

import Control.Exception (IOException, try)
import Data.ByteArray (ScrubbedBytes)
import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import System.IO (IOMode(ReadMode), withBinaryFile)
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUIDv4

newtype InstallationId = InstallationId UUID
newtype ServerKey = ServerKey ScrubbedBytes

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
