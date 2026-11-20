module Main where

import Collector
import Control.Concurrent (forkIOWithUnmask, killThread, threadDelay)
import Control.Exception (bracket, finally)
import Control.Monad (forever, void)
import Data.Aeson (eitherDecodeFileStrict)
import Data.String (fromString)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Network.Wai.Handler.Warp
import System.Environment (getArgs)
import System.Posix.Signals (installHandler, sigTERM, sigINT, Handler(Catch))

main :: IO ()
main = do
 args <- getArgs
 path <- case args of [file] -> pure file; _ -> fail "Usage: sapiens-telemetry CONFIG.json"
 config <- either fail pure =<< eitherDecodeFileStrict path
 collector <- newCollector config
 let settings = setInstallShutdownHandler installShutdown $ setGracefulShutdownTimeout (Just 5) $ setHost (fromString (bindAddress config)) $ setPort (listenPort config) defaultSettings
     flushExpired = forever $ do
      threadDelay 1000000
      now <- getPOSIXTime
      flushCollector collector (due config now)
 (bracket (forkIOWithUnmask (\unmask -> unmask flushExpired)) killThread $ \_ -> runSettings settings (collectorApplication collector))
  `finally` flushCollector collector (const True)

installShutdown :: IO () -> IO ()
installShutdown action = do
 void (installHandler sigTERM (Catch action) Nothing)
 void (installHandler sigINT (Catch action) Nothing)
