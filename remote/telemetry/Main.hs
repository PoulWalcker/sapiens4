module Main where

import Auth (ServerKeyError(..), loadServerKey)
import System.Environment
import System.Exit (die)

main :: IO ()
main = do
    args <- getArgs
    case args of
      [keyPath] -> do
        result <- loadServerKey keyPath
        case result of
          Left KeyReadFailed ->
            die "Cannot read telemetry server key"
          Left InvalidKeyLength ->
            die "Telemetry server key must contain exactly 32 bytes"
          Right _ ->
            putStrLn "Telemetry collector"
      _ ->
        die "Usage: telemetry KEY_FILE"