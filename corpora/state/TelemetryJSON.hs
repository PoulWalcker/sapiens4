{-# LANGUAGE OverloadedStrings #-}

module TelemetryJSON (decodeJSON, exactFields) where

import Control.Monad (unless)
import Data.Aeson (Value, Object, eitherDecodeStrict)
import Data.Aeson.Decoding.ByteString (bsToTokens)
import Data.Aeson.Decoding.Tokens (Tokens (..), TkArray (..), TkRecord (..))
import Data.Aeson.Key (Key)
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Types (Parser)
import qualified Data.ByteString as BS

-- Aeson objects alone lose duplicate keys. Check depth and duplicates before
-- materializing bounded, untrusted input; semantic decoding happens once.
decodeJSON :: Int -> BS.ByteString -> Either String Value
decodeJSON limit bytes
  | BS.length bytes > limit = Left "JSON too large"
  | otherwise = checkDepth 0 (bsToTokens bytes) >> eitherDecodeStrict bytes
  where
    checkDepth :: Int -> Tokens k e -> Either String k
    checkDepth depth tokens
      | depth > 32 = Left "JSON too deep"
      | otherwise = case tokens of
          TkLit _ next -> Right next
          TkText _ next -> Right next
          TkNumber _ next -> Right next
          TkArrayOpen items -> array (depth + 1) items
          TkRecordOpen fields -> record (depth + 1) KM.empty fields
          TkErr _ -> Left "Invalid JSON"
    array depth (TkItem item) = checkDepth depth item >>= array depth
    array _ (TkArrayEnd next) = Right next
    array _ (TkArrayErr _) = Left "Invalid JSON"
    record depth seen (TkPair key value)
      | KM.member key seen = Left "Duplicate JSON field"
      | otherwise = checkDepth depth value >>= record depth (KM.insert key () seen)
    record _ _ (TkRecordEnd next) = Right next
    record _ _ (TkRecordErr _) = Left "Invalid JSON"

exactFields :: [Key] -> Object -> Parser ()
exactFields names fields =
  unless (KM.size fields == length names && all (`KM.member` fields) names)
    (fail "Invalid fields")
