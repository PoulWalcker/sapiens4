module Main (main) where

import Data.Aeson (encode)
import qualified Data.ByteString.Lazy.Char8 as Bytes
import System.Environment (getArgs)
import System.Exit (die)
import System.IO (hSetEncoding, hSetNewlineMode, noNewlineTranslation, stdin, utf8)
import Wire (response)

main :: IO ()
main = do
    attached <- getArgs >>= either die pure . attachmentOption
    hSetEncoding stdin utf8
    hSetNewlineMode stdin noNewlineTranslation
    text <- getContents
    Bytes.putStrLn (encode (response attached text))

attachmentOption :: [String] -> Either String Bool
attachmentOption ["routine", "0"] = Right False
attachmentOption ["routine", "1"] = Right True
attachmentOption _ = Left "Usage: sapiens-domain routine 0|1"
