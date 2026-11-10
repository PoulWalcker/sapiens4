{-# LANGUAGE ForeignFunctionInterface #-}
module Crypto where

import Types
import Data.Aeson hiding (pairs)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Base64.URL as Base64URL
import qualified Crypto.Hash.SHA256 as SHA
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Text (Text)
import Data.Time.Clock.POSIX
import qualified Data.UUID as UUID
import Foreign
import Foreign.C
import Network.URI
import Numeric (showHex)

foreign import ccall unsafe "sodium_init" sodiumInit :: IO CInt
foreign import ccall unsafe "randombytes_buf" randomBytes :: Ptr Word8 -> CSize -> IO ()
foreign import ccall unsafe "crypto_box_keypair" boxKeypair :: Ptr Word8 -> Ptr Word8 -> IO CInt
foreign import ccall unsafe "crypto_scalarmult_base" boxPublic :: Ptr Word8 -> Ptr Word8 -> IO CInt
foreign import ccall unsafe "crypto_box_easy" boxEasy :: Ptr Word8 -> Ptr Word8 -> CULLong -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO CInt
foreign import ccall unsafe "crypto_box_open_easy" boxOpen :: Ptr Word8 -> Ptr Word8 -> CULLong -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO CInt
foreign import ccall unsafe "sodium_memcmp" sodiumCompare :: Ptr Word8 -> Ptr Word8 -> CSize -> IO CInt

protocol :: Text
protocol = "sapiens-remote-box-v1"
data Direction = PhoneToDesktop | DesktopToPhone deriving (Eq, Show, Enum, Bounded)
instance Wire Direction where
 wireName direction = case direction of PhoneToDesktop -> "phone-to-desktop"; DesktopToPhone -> "desktop-to-phone"
instance ToJSON Direction where toJSON = str . wireName
instance FromJSON Direction where parseJSON = parseWire

data RelayOrigin = RelayOrigin { relayURL :: Text, relayHost :: String, relayPort :: Int, relaySecure :: Bool } deriving (Eq, Show)
maxFrame, maxPlaintext, pairingTTL :: Int
maxFrame = 22000000
maxPlaintext = 16000000
pairingTTL = 259200
seconds :: IO Double
seconds = realToFrac <$> getPOSIXTime
b64 :: B.ByteString -> Text
b64 = TE.decodeUtf8 . Base64.encode
unb64 :: Value -> Maybe Int -> IO B.ByteString
unb64 value = validated . decodeBase64 value
decodeBase64 :: Value -> Maybe Int -> Validation B.ByteString
decodeBase64 value size = case value of
 String s | T.length s <= maxFrame -> case Base64.decode (TE.encodeUtf8 s) of
  Right b | maybe True (== B.length b) size -> pure b
  _ -> Left . APIError 400 $ "Invalid key, nonce or encoded value"
 _ -> Left . APIError 400 $ "Invalid encoded value"
withBytes :: B.ByteString -> (Ptr Word8 -> IO a) -> IO a
withBytes b f = B.useAsCString b (f . castPtr)
randomData :: Int -> IO B.ByteString
randomData n = do
 result <- sodiumInit
 require (result >= 0) 500 "Cryptographic initialization failed"
 allocaBytes n $ \p -> randomBytes p (fromIntegral n) >> B.packCStringLen (castPtr p,n)
token :: IO Text
token = TE.decodeUtf8 . Base64URL.encodeUnpadded <$> randomData 32
keypair :: IO (Text,Text)
keypair = do
 _ <- sodiumInit
 allocaBytes 32 $ \pub -> allocaBytes 32 $ \secret -> do
  result <- boxKeypair pub secret
  require (result == 0) 500 "Key generation failed"
  (,) <$> (b64 <$> B.packCStringLen (castPtr secret,32)) <*> (b64 <$> B.packCStringLen (castPtr pub,32))
hex :: B.ByteString -> Text
hex = T.pack . concatMap (\w -> let s = showHex w "" in if length s == 1 then '0':s else s) . B.unpack
hash :: B.ByteString -> Text
hash = hex . SHA.hash
fingerprint :: Value -> IO Text
fingerprint v = T.take 32 . hash <$> unb64 v (Just 32)
confirmationCode :: Value -> Value -> Text -> IO Text
confirmationCode desktop phone room = do
 d <- unb64 desktop (Just 32)
 p <- unb64 phone (Just 32)
 let digest = SHA.hash ("sapiens-confirm-v1\0" <> TE.encodeUtf8 room <> "\0" <> d <> p)
     alphabet = "ABCDEFGHJKMNPQRSTUVWXYZabcdefghjkmnpqrstuvwxyz"
     base = toInteger (length alphabet)
     value = B.foldl' (\n b -> n*256 + fromIntegral b) 0 (B.take 6 digest) `mod` (base^ (8 :: Int))
     digits 0 _ = ""
     digits n x = digits (n-1) (x `div` base) ++ [alphabet !! fromInteger (x `mod` base)]
     code = T.pack (digits (8 :: Int) value)
 pure (T.take 4 code <> " · " <> T.drop 4 code)
publicId :: Text -> IO Text
publicId = validated . corporaPublicId
corporaPublicId :: Text -> Validation Text
corporaPublicId identity = case UUID.fromText identity of
 Nothing -> Left . APIError 400 $ "Invalid CORPORA ID"
 Just u -> do
  let value = B.foldl' (\n b -> n*256+fromIntegral b) 0 (SHA.hash (BL.toStrict (UUID.toByteString u))) `mod` (26^(6::Int)*1000)
      digits (-1) _ = ""
      digits i n = let alphabet = if i >= 3 && i < 6 then "0123456789" else "abcdefghijklmnopqrstuvwxyz"
                       base = toInteger (length alphabet)
                   in digits (i-1) (n `div` base) ++ [alphabet !! fromInteger (n `mod` base)]
  pure ("sapi4_" <> T.pack (digits (8::Int) value))
origin :: Value -> IO Text
origin = fmap relayURL . parseRelayOrigin
parseRelayOrigin :: Value -> IO RelayOrigin
parseRelayOrigin = validated . decodeRelayOrigin
decodeRelayOrigin :: Value -> Validation RelayOrigin
decodeRelayOrigin (String value) = case parseURI (T.unpack value) of
 Just u | Just auth <- uriAuthority u, uriScheme u `elem` ["http:","https:"], null (uriUserInfo auth),
          not (null (uriRegName auth)), uriPath u `elem` ["","/"], null (uriQuery u), null (uriFragment u),
          uriScheme u == "https:" || uriRegName auth `elem` ["localhost","127.0.0.1","[::1]"] -> do
   let secure = uriScheme u == "https:"
   port <- case uriPort auth of
    "" -> pure (if secure then 443 else 80)
    ':':raw | [(n,"")] <- reads raw, n >= 1 && n <= 65535 -> pure n
    _ -> Left . APIError 400 $ "Invalid relay port"
   pure (RelayOrigin (T.dropWhileEnd (=='/') value) (filter (`notElem` ("[]" :: String)) (uriRegName auth)) port secure)
 _ -> Left . APIError 400 $ "Use an HTTPS relay origin (HTTP is allowed only on loopback for testing)"
decodeRelayOrigin _ = Left . APIError 400 $ "Relay must be an HTTPS origin"
constantEqual :: Text -> Text -> IO Bool
constantEqual a b = let x = TE.encodeUtf8 a; y = TE.encodeUtf8 b in
 if B.length x /= B.length y then pure False else withBytes x $ \px -> withBytes y $ \py -> (==0) <$> sodiumCompare px py (fromIntegral (B.length x))
pack :: Value -> Value -> Text -> Direction -> Value -> IO Value
pack secret peer room direction payload = do
 sk <- unb64 secret (Just 32)
 pk <- unb64 peer (Just 32)
 nonce <- randomData 24
 let raw = BL.toStrict (encode (merge (obj [("v",str protocol),("room",str room),("direction",toJSON direction)]) payload))
 require (B.length raw <= maxPlaintext) 400 "Remote response is too large"
 withBytes sk $ \ps -> withBytes pk $ \pp -> withBytes nonce $ \pn -> withBytes raw $ \pm ->
  allocaBytes (B.length raw+16) $ \pc -> allocaBytes 32 $ \pub -> do
   code <- boxEasy pc pm (fromIntegral (B.length raw)) pn pp ps
   require (code == 0) 400 "Encryption failed"
   _ <- boxPublic pub ps
   sender <- b64 <$> B.packCStringLen (castPtr pub,32)
   cipher <- b64 <$> B.packCStringLen (castPtr pc,B.length raw+16)
   pure (obj [("sender",str sender),("nonce",str (b64 nonce)),("ciphertext",str cipher)])
unpack :: Value -> Value -> Text -> Direction -> Value -> IO Value
unpack secret peer room direction frame = fst <$> unpackWithPlaintext secret peer room direction frame
unpackWithPlaintext :: Value -> Value -> Text -> Direction -> Value -> IO (Value,B.ByteString)
unpackWithPlaintext secret peer room direction frame = do
 require (get frame "sender" == peer) 400 "Unexpected sender"
 sk <- unb64 secret (Just 32)
 pk <- unb64 peer (Just 32)
 nonce <- unb64 (get frame "nonce") (Just 24)
 cipher <- unb64 (get frame "ciphertext") Nothing
 require (B.length cipher >= 16 && B.length cipher <= maxPlaintext+16) 400 "Remote request is too large"
 raw <- withBytes sk (\ps -> withBytes pk (\pp -> withBytes nonce (\pn -> withBytes cipher (\pc ->
  allocaBytes (B.length cipher-16) $ \pm -> do
   code <- boxOpen pm pc (fromIntegral (B.length cipher)) pn pp ps
   require (code == 0) 400 "Invalid encrypted message"
   B.packCStringLen (castPtr pm,B.length cipher-16)))))
 body <- either (const (failure 400 "Invalid JSON")) pure (eitherDecodeStrict raw)
 time <- seconds
 validated (validateEnvelope time room direction body)
 pure (body,raw)

validateEnvelope :: Double -> Text -> Direction -> Value -> Validation ()
validateEnvelope time room direction body = do
 check (get body "v" == str protocol && at body "room" == room && get body "direction" == toJSON direction) 400 "Wrong message context"
 let expiry = case get body "expires" of Number n -> realToFrac n; _ -> 0
 check (not (T.null (at body "id")) && T.length (at body "id") <= 80 && expiry > time && expiry <= time + fromIntegral (if at body "kind" == "pair" then pairingTTL else 86400)) 400 "Expired or invalid message"
