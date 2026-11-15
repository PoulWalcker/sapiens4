{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module DesktopTelemetry
  ( Channel
  , ConsentContext (..)
  , consent
  , newChannel
  , opened
  , receiveReply
  , unavailableReply
  ) where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar
import Control.Exception (IOException, catch, finally)
import Control.Monad (void, when)
import Data.Aeson (Value (..), decodeStrict, encode, object, (.=))
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.IORef
import Data.List (stripPrefix)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import System.IO (Handle, hFlush)
import System.Timeout (timeout)
import TelemetryControl

data Channel = Channel Handle (MVar Integer) (MVar ()) (IORef (Maybe (Integer, MVar Value)))

data ConsentContext = BootstrapConsent | WorkspaceConsent

unavailableReply :: Value
unavailableReply = object
  [ "consent" .= Null
  , "locally_disabled" .= True
  , "error" .= ("native_channel_unavailable" :: T.Text)
  ]

newChannel :: Handle -> IO Channel
newChannel input = Channel input <$> newMVar 0 <*> newMVar () <*> newIORef Nothing

receiveReply :: Channel -> String -> IO ()
receiveReply (Channel _ _ _ pending) line =
  case stripPrefix "Sapiens4 telemetry: " line >>= decodeStrict . TE.encodeUtf8 . T.pack of
    Just (Object fields) | KM.size fields == 2
      , Just (Number identifier) <- KM.lookup "request" fields
      , Just result <- KM.lookup "result" fields -> do
          current <- readIORef pending
          case current of
            Just (expected, reply) | identifier == fromInteger expected -> void (tryPutMVar reply result)
            _ -> pure ()
    _ -> pure ()

consent :: ConsentContext -> Maybe Channel -> ConsentAction -> IO Value
consent BootstrapConsent Nothing action = localConsent action
consent WorkspaceConsent Nothing _ = pure unavailableReply
consent _ (Just (Channel input serial requests pending)) action =
  withMVar requests $ \_ -> do
    reply <- newEmptyMVar
    result <- (do
      modifyMVar_ serial $ \previous -> do
        let identifier = previous + 1
        writeIORef pending (Just (identifier, reply))
        BL.hPutStrLn input ("telemetry " <> encode (object ["request" .= identifier, "command" .= consentActionJSON action]))
        hFlush input
        pure identifier
      maybe unavailableReply id <$> timeout 5000000 (takeMVar reply))
      `catch` (\(_ :: IOException) -> pure unavailableReply)
      `finally` writeIORef pending Nothing
    pure result

opened :: IORef Bool -> Maybe Channel -> IO ()
opened consumed channel = do
  first <- atomicModifyIORef' consumed (\done -> (True, not done))
  when first $ case channel of
    Nothing -> pure ()
    Just (Channel input serial _ _) -> void $ forkIO $ withMVar serial $ \_ ->
      (BL.hPutStrLn input "telemetry opened" >> hFlush input)
        `catch` \(_ :: IOException) -> pure ()
