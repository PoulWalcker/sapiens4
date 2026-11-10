module Setup where

import Types
import Crypto
import Data.Aeson hiding (pairs)
import qualified Data.ByteString as B
import Data.Char
import qualified Data.Text as T
import Data.Text (Text)
import Control.Exception
import System.Directory
import System.FilePath
import Network.URI

connectionFormat :: Text
connectionFormat = "sapiens-relay-connection-v1"
validateConnection :: Value -> IO Value
validateConnection = validated . connectionValue
connectionValue :: Value -> Validation Value
connectionValue value = do
 check (at value "format" == connectionFormat) 400 "Choose a Sapiens relay connection file"
 relay <- relayURL <$> decodeRelayOrigin (get value "relay")
 if boolean (get value "beta") then do
  check (T.null (at value "token")) 400 "Beta connections do not use an admin token"
  pure (obj [("format",str connectionFormat),("relay",str relay),("beta",Bool True)])
 else do
  let credential = at value "token"
  check (T.length credential >= 32 && T.length credential <= 256 && T.all (\c -> ord c >= 33 && ord c <= 126) credential) 400 "Invalid relay provisioning token"
  pure (obj [("format",str connectionFormat),("relay",str relay),("token",str credential)])
readConnection :: FilePath -> IO Value
readConnection path = do
 raw <- B.readFile path
 require (B.length raw <= 4096) 400 "Relay connection file is too large"
 either (const (failure 400 "Invalid relay connection")) validateConnection (eitherDecodeStrict raw)
provision :: FilePath -> Text -> Text -> IO Value
provision path relay credential = do
 exists <- doesFileExist path
 secret <- if not (T.null credential) then pure credential else if exists then (`at` "token") <$> readConnection path else token
 value <- validateConnection (obj [("format",str connectionFormat),("relay",str relay),("token",str secret)])
 writeJSON path value
 pure value
localConnection :: FilePath -> IO Value
localConnection repo = (do
 value <- readConnection (repo </> ".sapiens4/relay.connection.json")
 let local = case parseURI (T.unpack (at value "relay")) of Just u -> uriScheme u == "http:"; _ -> False
 pure (if local then value else empty)) `catch` \(_::SomeException) -> pure empty
