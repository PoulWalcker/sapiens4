module Sapiens.Platform where

import Prelude
import Data.Argonaut.Decode (printJsonDecodeError)
import Data.Either (either)
import Effect (Effect)
import Effect.Exception (error, throwException)
import Libs4.Browser as B
import Sapiens.Browser as Browser
import Sapiens.Environment as E

environment :: Effect E.Environment
environment = B.environment >>= either (throwException <<< error <<< printJsonDecodeError) pure <<< E.decode

measure :: String -> Effect Browser.Box
measure selector = Browser.box <$> B.measure selector

measureChildren :: String -> Effect (Array Browser.Box)
measureChildren selector = map Browser.box <$> B.measureChildren selector
