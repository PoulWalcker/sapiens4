{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TelemetryEndpoint (Endpoint, mkEndpoint, endpointOrigin, configuredEndpoint, configuredAppVersion) where

import Control.Exception (IOException, catch)
import Data.Aeson (Value, withObject, (.:), (.:?))
import Data.Aeson.Types (parseEither)
import qualified Data.ByteString as BS
import qualified Data.Text as T
import Data.Version (showVersion)
import qualified Paths_sapiens_domain as Package
import Network.URI (URI (..), URIAuth (..), parseURI)
import System.Directory (doesFileExist)
import System.Environment (getExecutablePath, lookupEnv)
import System.FilePath ((</>), takeDirectory)
import System.IO (IOMode (ReadMode), withBinaryFile)
import TelemetryJSON (decodeJSON)
import Text.Read (readMaybe)

newtype Endpoint = Endpoint String

endpointOrigin :: Endpoint -> String
endpointOrigin (Endpoint origin) = origin

mkEndpoint :: String -> Maybe Endpoint
mkEndpoint address = do
  uri <- parseURI address
  authority <- uriAuthority uri
  let portOK = case uriPort authority of
        "" -> True
        ':':digits | length digits <= 5, all (`elem` ['0'..'9']) digits ->
          maybe False (\number -> number > (0 :: Int) && number <= 65535) (readMaybe digits)
        _ -> False
  if uriScheme uri == "https:" && null (uriUserInfo authority) && not (null (uriRegName authority))
    && portOK && uriPath uri `elem` ["", "/"] && null (uriQuery uri) && null (uriFragment uri)
    then Just (Endpoint (show uri { uriPath = "" })) else Nothing

-- Both backend layouts put the manifest two directories above .build/bin.
-- Read the release version and endpoint from the same packaged metadata.
packageManifest :: IO (Maybe Value)
packageManifest = do
  executable <- getExecutablePath
  let path = takeDirectory (takeDirectory (takeDirectory executable)) </> "package-manifest.json"
  exists <- doesFileExist path
  if not exists then pure Nothing else do
    bytes <- withBinaryFile path ReadMode (\stream -> BS.hGet stream (64 * 1024 + 1))
    Just <$> either (ioError . userError) pure (decodeJSON (64 * 1024) bytes)

configuredAppVersion :: IO (Maybe T.Text)
configuredAppVersion = resolve `catch` \(_ :: IOException) -> pure Nothing
  where
    resolve = do
      manifest <- packageManifest
      case manifest of
        Nothing -> pure (Just (T.pack (showVersion Package.version)))
        Just value -> pure $ either (const Nothing) Just $ parseEither
          (withObject "package manifest" (.: "app_version")) value

configuredEndpoint :: IO (Maybe Endpoint)
configuredEndpoint = resolve `catch` \(_ :: IOException) -> pure Nothing
  where
    resolve = do
      supplied <- lookupEnv "SAPIENS_TELEMETRY_ENDPOINT"
      case supplied of
        Just address -> pure (mkEndpoint address)
        Nothing -> do
          manifest <- packageManifest
          case manifest of
            Nothing -> pure Nothing
            Just value -> pure $ either (const Nothing) id $ parseEither
              (withObject "package manifest" $ \fields -> do
                address <- fields .:? "telemetry_endpoint"
                traverse (maybe (fail "Invalid endpoint") pure . mkEndpoint) address) value
