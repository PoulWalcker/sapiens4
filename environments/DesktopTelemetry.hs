{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module DesktopTelemetry
  ( Channel
  , Session
  , newSession
  , isBlocked
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

-- The native session owns only the latch carried into a new backend process.
data Session = Session (MVar ()) (IORef Bool)

newSession :: IO Session
newSession = Session <$> newMVar () <*> newIORef False

isBlocked :: Session -> IO Bool
isBlocked (Session _ blocked) = readIORef blocked

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

consent :: Session -> ConsentContext -> Maybe Channel -> ConsentAction -> IO Value
consent (Session lock blocked) context channel action = withMVar lock $ \_ ->
  consentLocked blocked context channel action

consentLocked :: IORef Bool -> ConsentContext -> Maybe Channel -> ConsentAction -> IO Value
consentLocked blocked BootstrapConsent Nothing action =
  maybe unavailableReply id <$> timeout 5000000 (localConsent blocked action)
consentLocked _ WorkspaceConsent Nothing _ = pure unavailableReply
consentLocked blocked _ (Just (Channel input serial requests pending)) action =
  withMVar requests $ \_ -> do
    case action of
      ReadConsent -> pure ()
      _ -> writeIORef blocked True
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
    -- This is a reply from another process, so inspect JSON at the boundary.
    case result of
      Object fields | Just (Bool disabled) <- KM.lookup "locally_disabled" fields
        , KM.lookup "error" fields /= Just (String "native_channel_unavailable") -> do
            -- Reads and rejected mutations cannot undo an unconfirmed opt-out.
            let saved = KM.lookup "error" fields `elem` map Just
                  [Null, String "endpoint_unavailable", String "credential_update_failed", String "scoped_identity_unavailable"]
                changed = case action of ReadConsent -> False; _ -> saved
            when (disabled || changed) (writeIORef blocked disabled)
      _ -> pure ()
    pure result

opened :: IORef Bool -> Maybe Channel -> IO ()
opened consumed channel = do
  first <- atomicModifyIORef' consumed (\done -> (True, not done))
  when first $ case channel of
    Nothing -> pure ()
    Just (Channel input serial _ _) -> void $ forkIO $ withMVar serial $ \_ ->
      (BL.hPutStrLn input "telemetry opened" >> hFlush input)
        `catch` \(_ :: IOException) -> pure ()
