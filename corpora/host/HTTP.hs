module HTTP where

import Types
import Control.Exception
import Data.Aeson hiding (pairs)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Text (Text)
import Network.HTTP.Types
import Network.Wai
import System.Timeout

headers :: Text -> ResponseHeaders
headers mime = [(hContentType,TE.encodeUtf8 (mime<>"; charset=utf-8")),(hCacheControl,"no-store"),("X-Content-Type-Options","nosniff"),("X-Frame-Options","DENY"),("Referrer-Policy","no-referrer")]
response :: Int -> BL.ByteString -> Text -> Response
response code body mime = responseLBS (mkStatus code "") (headers mime) body
jsonResponse :: Int -> Value -> Response
jsonResponse code v = response code (encode v) "application/json"
respondError :: IO Response -> IO Response
respondError action = action `catches`
 [Handler (\(APIError n msg) -> pure (jsonResponse n (obj [("error",str msg)]))),
  Handler (\(e::IOException) -> pure (jsonResponse 500 (obj [("error",str (T.pack (displayException e)))])))]
bodyObject :: Int -> Request -> IO Value
bodyObject maximumSize req = do
 validated (validateContentLength maximumSize (lookup hContentLength (requestHeaders req)))
 let consume total chunks = do
      chunk <- getRequestBodyChunk req
      require (total+B.length chunk <= maximumSize) 413 "Request body is empty or too large"
      if B.null chunk then pure (B.concat (reverse chunks)) else consume (total+B.length chunk) (chunk:chunks)
 raw <- timeout 10000000 (consume 0 []) >>= maybe (failure 408 "Request body timed out") pure
 validated (decodeBody raw)

validateContentLength :: Int -> Maybe B.ByteString -> Validation ()
validateContentLength maximumSize lengthValue = do
 let size = lengthValue >>= \b -> case reads (T.unpack (TE.decodeUtf8 b)) of [(n,"")] -> Just (n::Int); _ -> Nothing
 check (lengthValue == Nothing || size /= Nothing) 400 "Invalid content length"
 check (maybe False (\n -> n > 0 && n <= maximumSize) size) 413 "Request body is empty or too large"

decodeBody :: B.ByteString -> Validation Value
decodeBody raw =
 case eitherDecodeStrict raw of
  Right v@(Object _) -> Right v
  Right _ -> Left . APIError 400 $ "Expected a JSON object"
  Left _ -> Left . APIError 400 $ "Invalid JSON"
requestTarget :: Request -> Text
requestTarget req = TE.decodeUtf8 (rawPathInfo req <> rawQueryString req)
