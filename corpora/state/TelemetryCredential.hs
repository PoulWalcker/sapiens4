{-# LANGUAGE OverloadedStrings #-}

module TelemetryCredential
  ( Token, InstallationId, installationIdText, installationBytes, installationFromUUID, parseToken, tokenBytes, tokenInstallation, tokenKid
  , credentialResponse, decodeCredentialResponse, parseUUIDv4, validKid
  ) where

import Control.Monad (guard)
import Data.Aeson (Value, object, withObject, (.:), (.=))
import Data.Aeson.Types (parseEither)
import Data.Bits ((.&.))
import qualified Data.ByteArray as BA
import Data.ByteArray.Encoding (Base (Base64URLUnpadded), convertFromBase, convertToBase)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import TelemetryJSON (decodeJSON, exactFields)

-- Deliberately no Show instance: credentials must never enter diagnostics.
data Token = Token BS.ByteString InstallationId
instance Eq Token where
  Token a _ == Token b _ = BA.constEq a b

tokenBytes :: Token -> BS.ByteString
tokenBytes (Token bytes _) = bytes

tokenKid :: Token -> BS.ByteString
tokenKid = BS.take 16 . tokenBytes

newtype InstallationId = InstallationId UUID deriving (Eq, Ord, Show)
installationIdText :: InstallationId -> T.Text
installationIdText (InstallationId uuid) = UUID.toText uuid
installationBytes :: InstallationId -> BS.ByteString
installationBytes (InstallationId uuid) = BL.toStrict (UUID.toByteString uuid)
installationFromUUID :: UUID -> Maybe InstallationId
installationFromUUID uuid = InstallationId <$> parseUUIDv4 (UUID.toText uuid)
tokenInstallation :: Token -> InstallationId
tokenInstallation (Token _ identifier) = identifier

validKid :: BS.ByteString -> Bool
validKid bytes = BS.length bytes == 16 && B8.all (`elem` ("0123456789abcdef" :: String)) bytes

parseUUIDv4 :: T.Text -> Maybe UUID
parseUUIDv4 input = do
  uuid <- UUID.fromText input
  let (_, version, variant, _) = UUID.toWords uuid
  guard (UUID.toText uuid == input && version .&. 0xf000 == 0x4000
    && variant .&. 0xc0000000 == 0x80000000)
  pure uuid

parseToken :: BS.ByteString -> Maybe Token
parseToken bytes = do
  guard (BS.length bytes == 97)
  case B8.split '.' bytes of
    [kid, identifier, tag] -> do
      guard (validKid kid && BS.length identifier == 36 && BS.length tag == 43)
      input <- either (const Nothing) Just (TE.decodeUtf8' identifier)
      identifierUUID <- parseUUIDv4 input
      mac <- either (const Nothing) Just
        (convertFromBase Base64URLUnpadded tag :: Either String BS.ByteString)
      guard (BS.length mac == 32 && convertToBase Base64URLUnpadded mac == tag)
      pure (Token bytes (InstallationId identifierUUID))
    _ -> Nothing

credentialResponse :: Token -> Value
credentialResponse token = object ["token" .= TE.decodeUtf8 (tokenBytes token)]

decodeCredentialResponse :: BS.ByteString -> Either String Token
decodeCredentialResponse bytes = decodeJSON 128 bytes >>= parseEither
  (withObject "credential" $ \fields -> do
    exactFields ["token"] fields
    value <- fields .: "token"
    maybe (fail "Invalid credential") pure (parseToken (TE.encodeUtf8 value)))
