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
import qualified Data.ByteString.Lazy as BL
import Data.Char (toLower)
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import System.IO (hClose)
import System.Posix.Files (fileMode, fileOwner, getFdStatus, isRegularFile, linkCount)
import System.Posix.IO (OpenMode (ReadOnly), OpenFileFlags (..), closeFd, defaultFileFlags, fdToHandle, openFd)
import System.Posix.User (getEffectiveUserID)
import TelemetryCredential
import TelemetryJSON (decodeJSON, exactFields)

data Key = Key BS.ByteString ScrubbedBytes
data Auth = Auth Key (Maybe Key) (Set.Set T.Text)
-- Only verified principals can be handed to the ingestion parser.
data Principal = Principal T.Text Bool

principalInstallation :: Principal -> T.Text
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
  unless (all (maybe False (const True) . parseUUIDv4) revoked
    && Set.size (Set.fromList revoked) == length revoked) (fail "Invalid revocations")
  pure (Auth active previous (Set.fromList revoked))

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

sign :: Key -> T.Text -> Token
sign (Key kid key) identifier =
  case UUID.fromText identifier of
    Nothing -> error "Internal installation identifier"
    Just uuid ->
      let message = "sapiens4.telemetry.credential\0" <> kid <> BL.toStrict (UUID.toByteString uuid)
          tag = convertToBase Base64URLUnpadded (hmac key message :: HMAC SHA256)
      in case parseToken (kid <> "." <> TE.encodeUtf8 identifier <> "." <> tag) of
          Just token -> token
          Nothing -> error "Internal credential encoding"

issueCredential :: Auth -> IO (Either String Token)
issueCredential (Auth key _ revoked) = do
  identifier <- UUID.toText <$> UUID.nextRandom
  pure $ if Set.member identifier revoked then Left "Issuance failed" else Right (sign key identifier)

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
  guardMaybe (BA.constEq (BS.drop 54 bytes) (BS.drop 54 (tokenBytes (sign key identifier))))
  guardMaybe (not (Set.member identifier revoked))
  pure (Principal identifier (kid /= activeKid))
  where guardMaybe True = Just (); guardMaybe False = Nothing

replacementCredential :: Auth -> Principal -> Maybe Token
replacementCredential (Auth active _ _) (Principal identifier old) =
  if old then Just (sign active identifier) else Nothing
