{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TelemetryIdentity (prepareCorporaId) where

import Control.Exception (IOException, catch)
import Control.Monad (unless)
import Data.Aeson (Value (..), encode)
import qualified Data.Aeson.KeyMap as KM
import qualified Data.UUID as UUID
import RuntimePaths (externalStoragePath)
import System.FilePath ((</>), equalFilePath)
import Telemetry (CorporaId, parseCorporaId, newCorporaId, corporaIdText)
import TelemetryJSON (decodeJSON)
import TelemetryProfile (ProfileLocation, ConsentEpoch, withGrantedConsent)
import TelemetryStorage

-- The ordinary Corpora UUID stays local. A separate random UUID moves with
-- the Corpora and is only created after an explicit grant.
prepareCorporaId :: ProfileLocation -> ConsentEpoch -> FilePath -> IO (Maybe CorporaId)
prepareCorporaId location epoch directory = do
  result <- withGrantedConsent location epoch $ recover $ do
    root <- externalStoragePath directory
    checkPrivate root True
    withPrivateLock (root </> ".telemetry-identity.lock") $ do
      let path = root </> "corpora-identity.json"
      bytes <- readPrivateFile path
      fields <- case decodeJSON (64 * 1024) bytes of
        Right (Object object) | KM.lookup "version" object == Just (Number 1)
          , Just (String local) <- KM.lookup "corpora_id" object
          , Just identifier <- UUID.fromText local, UUID.toText identifier == local -> pure object
        _ -> ioError (userError "Invalid Corpora identity")
      case KM.lookup "telemetry_id" fields of
        Just (String value) -> pure (parseCorporaId value)
        Nothing -> do
          value <- newCorporaId
          atomicWritePrivate path (encode (Object (KM.insert "telemetry_id" (String (corporaIdText value)) fields))) $ do
            current <- externalStoragePath directory
            unless (equalFilePath current root) (ioError (userError "Corpora path changed"))
          pure (Just value)
        _ -> pure Nothing
  pure (either (const Nothing) (>>= id) result)
  where
    recover action = (action `catch` (\(_ :: IOException) -> pure Nothing))
      `catch` (\(_ :: PrivateFileError) -> pure Nothing)
