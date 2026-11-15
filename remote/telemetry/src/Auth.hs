{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Auth
  ( Auth, Principal, loadAuth, issueCredential, verifyAuthorization
  , principalInstallation, replacementCredential
  ) where

import Control.Exception (IOException, bracket, mask_, onException, try)
import Control.Monad (unless, when)
import Crypto.Hash (SHA256)
import Crypto.MAC.HMAC (HMAC, hmac)
import Data.Aeson (Value (Null), withObject, (.:))
import Data.Aeson.Types (Parser, parseEither)
import Data.Bits ((.&.))
import qualified Data.ByteArray as BA
import Data.ByteArray (ScrubbedBytes)
import Data.ByteArray.Encoding (Base (Base64URLUnpadded), convertFromBase, convertToBase)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import Data.Char (toLower)
import qualified Data.Set as Set
import qualified Data.Text.Encoding as TE
import qualified Data.UUID.V4 as UUID
import System.IO (hClose)
import System.Posix.Files (fileMode, fileOwner, getFdStatus, isRegularFile, linkCount)
import System.Posix.IO (OpenMode (ReadOnly), OpenFileFlags (..), closeFd, defaultFileFlags, fdToHandle, openFd)
import System.Posix.User (getEffectiveUserID)
import TelemetryCredential
import TelemetryJSON (decodeJSON, exactFields)

data Key = Key BS.ByteString ScrubbedBytes
data Auth = Auth Key (Maybe Key) (Set.Set InstallationId)
-- Only verified principals can be handed to the ingestion parser.
data KeyAge = ActiveKey | PreviousKey
data Principal = Principal InstallationId KeyAge

principalInstallation :: Principal -> InstallationId
principalInstallation (Principal identifier _) = identifier

parseKey :: Value -> Parser Key
parseKey = withObject "key" $ \fields -> do
  exactFields ["kid", "key"] fields
  kid <- TE.encodeUtf8 <$> fields .: "kid"
  encoded <- TE.encodeUtf8 <$> fields .: "key"
  unless (validKid kid && BS.length encoded == 43) (fail "Invalid key")
  key <- either (const (fail "Invalid key")) pure
    (convertFromBase Base64URLUnpadded encoded :: Either String BS.ByteString)
  unless (BS.length key == 32 && convertToBase Base64URLUnpadded key == encoded)
    (fail "Invalid key")
  pure (Key kid (BA.convert key))

parseAuth :: Value -> Parser Auth
parseAuth = withObject "auth" $ \fields -> do
  exactFields ["active", "previous", "revoked_installations"] fields
  active@(Key kid key) <- fields .: "active" >>= parseKey
  previousValue <- fields .: "previous"
  previous <- case previousValue of
    Null -> pure Nothing
    value -> Just <$> parseKey value
  case previous of
    Just (Key oldKid oldKey) -> when (kid == oldKid || BA.constEq key oldKey) (fail "Duplicate key")
    Nothing -> pure ()
  revoked <- fields .: "revoked_installations"
  identifiers <- traverse (\value -> maybe (fail "Invalid revocation") pure
    (parseUUIDv4 value >>= installationFromUUID)) revoked
  unless (Set.size (Set.fromList identifiers) == length identifiers) (fail "Duplicate revocation")
  pure (Auth active previous (Set.fromList identifiers))

loadAuth :: FilePath -> IO (Either String Auth)
loadAuth path = do
  loaded <- try $ bracket open hClose $ \stream -> do
    bytes <- BS.hGet stream (1024 * 1024 + 1)
    pure (decodeJSON (1024 * 1024) bytes >>= parseEither parseAuth)
  pure $ case loaded of
    Left (_ :: IOException) -> Left "Cannot read auth configuration"
    Right value -> value
  where
    open = mask_ $ do
      fd <- openFd path ReadOnly defaultFileFlags
        { nofollow = True, nonBlock = True, cloexec = True }
      (do
        status <- getFdStatus fd
        owner <- getEffectiveUserID
        unless (isRegularFile status && fileOwner status == owner
          && linkCount status == 1 && fileMode status .&. 0o077 == 0)
          (ioError (userError "Insecure auth configuration"))
        fdToHandle fd) `onException` closeFd fd

deriveMac :: Key -> InstallationId -> HMAC SHA256
deriveMac (Key kid key) identifier = hmac key
  ("sapiens4.telemetry.credential\0" <> kid <> installationBytes identifier)

sign :: Key -> InstallationId -> Maybe Token
sign key@(Key kid _) identifier = parseToken
  (kid <> "." <> TE.encodeUtf8 (installationIdText identifier) <> "."
    <> convertToBase Base64URLUnpadded (deriveMac key identifier))

issueCredential :: Auth -> IO (Either String Token)
issueCredential (Auth key _ revoked) = do
  generated <- UUID.nextRandom
  pure $ case installationFromUUID generated of
    Just identifier | not (Set.member identifier revoked) ->
      maybe (Left "Issuance failed") Right (sign key identifier)
    _ -> Left "Issuance failed"

verifyAuthorization :: Auth -> [BS.ByteString] -> Maybe Principal
verifyAuthorization (Auth active@(Key activeKid _) previous revoked) headers = do
  header <- case headers of [value] -> Just value; _ -> Nothing
  let (scheme, suffix) = B8.break (== ' ') header
  guardMaybe (B8.map toLower scheme == "bearer")
  bytes <- BS.stripPrefix " " suffix
  token <- parseToken bytes
  let kid = tokenKid token
      identifier = tokenInstallation token
  key <- if kid == activeKid then Just active else case previous of
    Just candidate@(Key oldKid _) | kid == oldKid -> Just candidate
    _ -> Nothing
  supplied <- either (const Nothing) Just
    (convertFromBase Base64URLUnpadded (BS.drop 54 bytes) :: Either String BS.ByteString)
  guardMaybe (BA.constEq supplied (deriveMac key identifier))
  guardMaybe (not (Set.member identifier revoked))
  pure (Principal identifier (if kid == activeKid then ActiveKey else PreviousKey))
  where guardMaybe True = Just (); guardMaybe False = Nothing

replacementCredential :: Auth -> Principal -> Maybe Token
replacementCredential (Auth active _ _) (Principal identifier age) = case age of
  ActiveKey -> Nothing
  PreviousKey -> sign active identifier
