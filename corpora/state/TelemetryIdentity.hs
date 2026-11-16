{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TelemetryIdentity (prepareCorporaId) where

import Control.Exception (IOException, catch)
import Control.Monad (unless)
import Data.Aeson (Value (..), encode, object, (.=))
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import qualified Data.UUID as UUID
import RuntimePaths (externalStoragePath)
import System.FilePath ((</>), equalFilePath)
import System.IO (IOMode (ReadMode), withBinaryFile)
import Telemetry (CorporaId, parseCorporaId, newCorporaId, corporaIdText)
import TelemetryJSON (decodeJSON)
import TelemetryProfile (ProfileLocation, ConsentEpoch, withGrantedConsent)
import TelemetryStorage

-- Optional telemetry owns only .telemetry, never the Corpora root or its identity.
-- Moving or copying a complete Corpora preserves this independent pseudonym.
prepareCorporaId :: ProfileLocation -> ConsentEpoch -> FilePath -> IO (Maybe CorporaId)
prepareCorporaId location epoch directory = do
  result <- withGrantedConsent location epoch $ recover $ do
    root <- externalStoragePath directory
    bytes <- withBinaryFile (root </> "corpora-identity.json") ReadMode (\stream -> BS.hGet stream (64 * 1024 + 1))
    case decodeJSON (64 * 1024) bytes of
      Right (Object fields) | KM.lookup "version" fields == Just (Number 1)
        , Just (String local) <- KM.lookup "corpora_id" fields
        , Just identifier <- UUID.fromText local, UUID.toText identifier == local -> pure ()
      _ -> ioError (userError "Invalid Corpora identity")
    let private = root </> ".telemetry"
        path = private </> "identity.json"
        checkLocation = do
          current <- externalStoragePath directory
          resolved <- externalStoragePath private
          unless (equalFilePath current root && equalFilePath resolved private)
            (ioError (userError "Corpora path changed"))
    exists <- privatePathExists private
    unless exists (createPrivateDirectory private)
    checkLocation
    withPrivateLock (private </> "identity.lock") $ do
      present <- privatePathExists path
      if present then do
        saved <- readPrivateFile path
        pure $ case decodeJSON (64 * 1024) saved of
          Right (Object fields) | Just (String value) <- KM.lookup "corpora_id" fields -> parseCorporaId value
          _ -> Nothing
      else do
        value <- newCorporaId
        atomicWritePrivate path (encode (object ["corpora_id" .= corporaIdText value])) checkLocation
        pure (Just value)
  pure (either (const Nothing) (>>= id) result)
  where
    recover action = (action `catch` (\(_ :: IOException) -> pure Nothing))
      `catch` (\(_ :: PrivateFileError) -> pure Nothing)
