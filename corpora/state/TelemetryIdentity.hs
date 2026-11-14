{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TelemetryIdentity
  ( CorporaIdentityError (..)
  , PublicCorporaId
  , preparePublicCorporaId
  , publicCorporaIdText
  ) where

import Control.Exception (Exception, Handler (..), IOException, catches, throwIO)
import Control.Monad (unless)
import Crypto.Hash (SHA256)
import Crypto.MAC.HMAC (HMAC, hmac)
import Data.Aeson (Object, Value (..), eitherDecodeStrict, encode)
import qualified Data.Aeson.KeyMap as KM
import Data.ByteArray (ScrubbedBytes)
import qualified Data.ByteArray as BA
import Data.ByteArray.Encoding (Base (Base64URLUnpadded), convertFromBase, convertToBase)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import RuntimePaths (externalStoragePath)
import System.Entropy (getEntropy)
import System.FilePath ((</>), equalFilePath)
import TelemetryProfile
  ( Consent (Granted), ConsentEpoch, ConsentRecord (..), ProfileLocation
  , loadProfile, profileConsent, withGrantedConsent
  )
import TelemetryStorage
  ( PrivateFileError, atomicWritePrivate, checkPrivate, privatePathExists
  , readPrivateFile, withPrivateLock
  )

newtype LocalCorporaId = LocalCorporaId UUID

newtype CorporaPrivacyKey = CorporaPrivacyKey ScrubbedBytes

newtype PublicCorporaId = PublicCorporaId BS.ByteString
  deriving (Eq, Show)

data CorporaIdentityError
  = MissingCorporaIdentity
  | InvalidCorporaIdentity
  | InvalidCorporaPrivacyKey
  | CorporaIdentityIOError
  | ConsentUnavailable
  deriving (Eq, Show)

instance Exception CorporaIdentityError

data CorporaIdentity = CorporaIdentity Object LocalCorporaId (Maybe CorporaPrivacyKey)

parseIdentity :: BS.ByteString -> Either CorporaIdentityError CorporaIdentity
parseIdentity bytes = do
  value <- either (const (Left InvalidCorporaIdentity)) Right (eitherDecodeStrict bytes)
  fields <- case value of
    Object object | KM.lookup "version" object == Just (Number 1) -> Right object
    _ -> Left InvalidCorporaIdentity
  identifier <- case KM.lookup "corpora_id" fields of
    Just (String text) | Just uuid <- UUID.fromText text, UUID.toText uuid == text ->
      Right (LocalCorporaId uuid)
    _ -> Left InvalidCorporaIdentity
  key <- case KM.lookup "telemetry_privacy_key_v1" fields of
    Nothing -> Right Nothing
    Just (String text) -> Just <$> parsePrivacyKey text
    _ -> Left InvalidCorporaPrivacyKey
  pure (CorporaIdentity fields identifier key)

parsePrivacyKey :: Text -> Either CorporaIdentityError CorporaPrivacyKey
parsePrivacyKey text = do
  let encoded = TE.encodeUtf8 text
  unless (BS.length encoded == 43) (Left InvalidCorporaPrivacyKey)
  bytes <- either (const (Left InvalidCorporaPrivacyKey)) Right
    (convertFromBase Base64URLUnpadded encoded :: Either String BS.ByteString)
  unless (BS.length bytes == 32 && convertToBase Base64URLUnpadded bytes == encoded)
    (Left InvalidCorporaPrivacyKey)
  pure (CorporaPrivacyKey (BA.convert bytes))

publicCorporaId :: CorporaPrivacyKey -> LocalCorporaId -> PublicCorporaId
publicCorporaId (CorporaPrivacyKey key) (LocalCorporaId uuid) =
  let message = "sapiens4.telemetry.corpora.v1\0" <> BL.toStrict (UUID.toByteString uuid)
      digest = hmac key message :: HMAC SHA256
  in PublicCorporaId (BA.convert digest)

publicCorporaIdText :: PublicCorporaId -> Text
publicCorporaIdText (PublicCorporaId digest) =
  TE.decodeUtf8 (convertToBase Base64URLUnpadded digest)

identityAction :: IO a -> IO (Either CorporaIdentityError a)
identityAction action = (Right <$> action) `catches`
  [ Handler (\(err :: CorporaIdentityError) -> pure (Left err))
  , Handler (\(_ :: PrivateFileError) -> pure (Left CorporaIdentityIOError))
  , Handler (\(_ :: IOException) -> pure (Left CorporaIdentityIOError))
  ]

preparePublicCorporaId :: ProfileLocation -> ConsentEpoch -> FilePath
                      -> IO (Either CorporaIdentityError PublicCorporaId)
preparePublicCorporaId location expected directory = do
  result <- withGrantedConsent location expected $ identityAction $ do
    root <- externalStoragePath directory
    let path = root </> "corpora-identity.json"
    exists <- privatePathExists path
    unless exists (throwIO MissingCorporaIdentity)
    withPrivateLock (root </> ".telemetry-identity.lock") $ do
      bytes <- readPrivateFile path
      CorporaIdentity fields identifier stored <- either throwIO pure (parseIdentity bytes)
      key <- case stored of
        Just existing -> pure existing
        Nothing -> do
          created@(CorporaPrivacyKey secret) <- CorporaPrivacyKey . BA.convert <$> getEntropy 32
          let encoded = TE.decodeUtf8 (convertToBase Base64URLUnpadded secret)
              updated = Object (KM.insert "telemetry_privacy_key_v1" (String encoded) fields)
              beforePublish = do
                current <- externalStoragePath directory
                unless (equalFilePath current root) (throwIO CorporaIdentityIOError)
                profile <- loadProfile location
                case profile of
                  Right value | Just (ConsentRecord (Granted _) epoch) <- profileConsent value
                              , epoch == expected -> pure ()
                  _ -> throwIO ConsentUnavailable
          atomicWritePrivate path (encode updated) beforePublish
          pure created
      checkPrivate root True
      pure (publicCorporaId key identifier)
  pure $ case result of
    Right (Just prepared) -> prepared
    _ -> Left ConsentUnavailable
