module Sapiens.Environment where

import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (JsonDecodeError, decodeJson)
import Data.Either (Either)
import Foreign.Object (Object)

type Environment =
  { origin :: String, href :: String, path :: String, hash :: String, search :: String
  , contentType :: String, body :: Object String, userAgent :: String, platform :: String
  , touchPoints :: Number, installed :: Boolean, dark :: Boolean, touch :: Boolean
  , width :: Number, height :: Number, orientation :: String
  , viewportHeight :: Number, viewportScale :: Number, hidden :: Boolean
  , secure :: Boolean, native :: Boolean, online :: Boolean
  }

decode :: Json -> Either JsonDecodeError Environment
decode = decodeJson
