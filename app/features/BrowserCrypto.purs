module Sapiens.BrowserCrypto where

import Prelude
import Data.Argonaut.Core (Json, toNumber, toString)
import Data.Either (Either(..), either)
import Data.Int as I
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Nullable (Nullable, toMaybe)
import Data.String.CodeUnits as S
import Effect (Effect)
import Effect.Exception as E
import Sapiens.Data as D

foreign import data Bytes :: Type
foreign import decodeBytes :: String -> Nullable Bytes
foreign import encodeBytes :: Bytes -> String
foreign import bytesLength :: Bytes -> Int
foreign import textBytes :: String -> Bytes
foreign import bytesText :: Bytes -> Nullable String
foreign import nativeKeyPair :: Effect { publicKey :: Bytes, secretKey :: Bytes }
foreign import nativePublicKey :: Bytes -> Bytes
foreign import nativeRandom :: Int -> Effect Bytes
foreign import lowOrder :: Bytes -> Bytes -> Boolean
foreign import wipe :: Bytes -> Effect Unit
foreign import sealBytes :: Bytes -> Bytes -> Bytes -> Bytes -> Bytes
foreign import openBytes :: Bytes -> Bytes -> Bytes -> Bytes -> Nullable Bytes
foreign import failValue :: forall a. String -> a
foreign import attemptValue :: forall a. (Unit -> a) -> Nullable a
foreign import publish ::
  { keyPair :: Effect Json, publicKey :: String -> String, randomBytes :: Number -> Effect String
  , box :: String -> String -> String -> String -> String
  , unbox :: String -> String -> String -> String -> String
  , perform :: String -> Json -> Effect Json
  } -> Effect Unit

decode :: Maybe Int -> String -> Either String Bytes
decode size value = do
  when (S.length value > 22000000) $ Left "Invalid encoded value"
  bytes <- maybe (Left "Invalid encoded value") Right $ toMaybe $ decodeBytes value
  when (maybe false (_ /= bytesLength bytes) size || encodeBytes bytes /= value) $ Left "Invalid encoded value"
  pure bytes

keyPair :: Effect Json
keyPair = do
  pair <- nativeKeyPair
  let result = D.json {publicKey: encodeBytes pair.publicKey, secretKey: encodeBytes pair.secretKey}
  wipe pair.secretKey
  pure result

publicKey :: String -> String
publicKey = either failValue (encodeBytes <<< nativePublicKey) <<< decode (Just 32)

randomBytes :: Int -> Effect String
randomBytes size = do
  when (size < 0 || size > 65536) $ E.throw "Invalid random length"
  encodeBytes <$> nativeRandom size

keys :: String -> String -> Either String { peer :: Bytes, secret :: Bytes }
keys peer secret = do
  pk <- decode (Just 32) peer
  sk <- decode (Just 32) secret
  when (lowOrder pk sk) $ Left "Invalid peer key"
  pure {peer: pk, secret: sk}

encrypt :: Bytes -> String -> String -> String -> Either String String
encrypt message nonce peer secret = do
  when (bytesLength message > 16000000) $ Left "Remote message is too large"
  n <- decode (Just 24) nonce
  k <- keys peer secret
  pure $ encodeBytes $ sealBytes message n k.peer k.secret

decrypt :: String -> String -> String -> String -> Either String Bytes
decrypt ciphertext nonce peer secret = do
  cipher <- decode Nothing ciphertext
  when (bytesLength cipher < 16 || bytesLength cipher > 16000016) $ Left "Invalid encrypted message"
  n <- decode (Just 24) nonce
  k <- keys peer secret
  maybe (Left "Invalid encrypted message") Right $ toMaybe $ openBytes cipher n k.peer k.secret

box :: String -> String -> String -> String -> String
box text nonce peer secret = either failValue identity $ encrypt (textBytes text) nonce peer secret

unbox :: String -> String -> String -> String -> String
unbox ciphertext nonce peer secret = fromMaybe "" $ toMaybe $ attemptValue \_ ->
  either (const "") (fromMaybe "" <<< toMaybe <<< bytesText) $ decrypt ciphertext nonce peer secret

perform :: String -> Json -> Effect Json
perform operation input = case operation of
  "keypair" -> keyPair
  "random" -> case toNumber (D.get "size" input) >>= I.fromNumber of
    Just size -> D.text <$> randomBytes size
    Nothing -> E.throw "Invalid random length"
  "seal" -> do
    nonce <- randomBytes 24
    plaintext <- either E.throw pure $ maybe (Left "Invalid encoded value") (decode Nothing) $ toString $ D.get "plaintext" input
    ciphertext <- either E.throw pure $ encrypt plaintext nonce (D.s "peer" input) (D.s "secret" input)
    pure $ D.json {sender: publicKey $ D.s "secret" input, nonce, ciphertext}
  "open" -> do
    let frame = D.get "frame" input
    when (D.s "sender" frame /= D.s "peer" input) $ E.throw "Unexpected sender"
    D.text <<< encodeBytes <$> either E.throw pure (decrypt (D.s "ciphertext" frame) (D.s "nonce" frame) (D.s "peer" input) (D.s "secret" input))
  _ -> E.throw "Unsupported crypto operation"

main :: Effect Unit
main = do
  publish {keyPair, publicKey, randomBytes: \value -> case I.fromNumber value of
    Nothing -> E.throw "Invalid random length"
    Just size -> randomBytes size
    , box, unbox, perform}
