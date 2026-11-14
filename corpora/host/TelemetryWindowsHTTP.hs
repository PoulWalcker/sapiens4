{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TelemetryWindowsHTTP (managerSettings) where

import Control.Concurrent (threadDelay)
import Control.Exception (IOException, allowInterrupt, bracket, bracketOnError, catch)
import Control.Monad (unless, when)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Internal as Bytes
import qualified Data.ByteString.Lazy as BL
import Data.Default.Class (def)
import Foreign
import Foreign.C.String (CWString, withCWString)
import Foreign.C.Types
import qualified Network.Connection as Connection
import Network.Connection.Internal (ConnectionContext (..))
import Network.HTTP.Client (ManagerSettings (..))
import Network.HTTP.Client.Internal (Connection, makeConnection, managerTlsProxyConnection)
import Network.HTTP.Client.TLS (mkManagerSettings)
import qualified Network.Socket as Socket
import Network.Socket.Address (SocketAddress (..))
import qualified Network.TLS as TLS

-- The pinned network package uses synchronous Winsock calls on Windows.
-- System.Timeout/cancel cannot interrupt those calls. Nonblocking sockets yield
-- in Haskell instead, including while http-client masks response acquisition.
managerSettings :: TLS.Supported -> IO ManagerSettings
managerSettings supported = do
  trust <- Connection.initConnectionContext
  let settings = mkManagerSettings
        (Connection.TLSSettingsSimple False False True supported) Nothing
      secure socket host port = do
        let parameters = (TLS.defaultParamsClient (strip host) (B8.pack (show port)))
              { TLS.clientSupported = supported
              , TLS.clientShared = def {TLS.sharedCAStore = globalCertificateStore trust}
              }
        context <- TLS.contextNew (backend socket) parameters
        TLS.handshake context
        makeConnection (TLS.recvData context) (TLS.sendData context . BL.fromStrict)
          (TLS.contextClose context)
  pure settings
    { managerTlsConnection = pure $ \_ host port ->
        withSocket host port (\socket -> secure socket host port)
    , managerTlsProxyConnection = pure $ \connectBytes checkProxy serverName _ host port ->
        withSocket host port $ \socket -> do
          raw <- makeConnection (receiveSome socket 16384) (sendAll socket) (Socket.close socket)
          sendAll socket connectBytes
          checkProxy raw
          secure socket serverName port
    }
  where
    strip = filter (`notElem` ("[]" :: String))

connectionFailure :: IO a
connectionFailure = ioError (userError "Telemetry TLS connection failed")

-- Address lookup is also asynchronous/cancellable; a blocking getAddrInfo
-- would otherwise escape the same connection deadline before a socket exists.
resolve :: String -> Int -> IO [Socket.SockAddr]
resolve host port = withCWString host $ \name -> withCWString (show port) $ \service ->
  bracket (do query <- dnsStart name service
              if query == nullPtr then connectionFailure else pure query)
    dnsFree $ \query -> do
      let wait = do
            allowInterrupt
            status <- dnsPoll query
            if status == 0 then threadDelay 10000 >> wait
              else unless (status == 1) connectionFailure
          addresses index = allocaBytes 128 $ \buffer -> do
            found <- dnsAddress query index buffer
            case found of
              0 -> pure []
              1 -> (:) <$> peekSocketAddress (castPtr buffer) <*> addresses (index + 1)
              _ -> connectionFailure
      wait
      addresses 0

withSocket :: String -> Int -> (Socket.Socket -> IO a) -> IO a
withSocket host port action = Socket.withSocketsDo $ do
  addresses <- resolve (filter (`notElem` ("[]" :: String)) host) port
  let open [] = connectionFailure
      open (address:rest) = attempt `catch` \(_ :: IOException) -> open rest
        where
          family = case address of
            Socket.SockAddrInet {} -> Socket.AF_INET
            Socket.SockAddrInet6 {} -> Socket.AF_INET6
            _ -> Socket.AF_UNSPEC
          attempt = bracketOnError (Socket.socket family Socket.Stream Socket.defaultProtocol)
            Socket.close $ \socket -> do
              Socket.withFdSocket socket $ \fd -> do
                result <- nonblockingSocket (fromIntegral fd)
                when (result /= 0) connectionFailure
              Socket.withFdSocket socket $ \fd ->
                allocaBytes (sizeOfSocketAddress address) $ \buffer -> do
                  pokeSocketAddress buffer address
                  result <- connectSocket (fromIntegral fd) buffer (fromIntegral (sizeOfSocketAddress address))
                  when (result /= 0) $ do
                    unless (result `elem` [-10035,-10036,-10037]) connectionFailure
                    waitWrite socket
              action socket
  open addresses

waitWrite :: Socket.Socket -> IO ()
waitWrite socket = do
  allowInterrupt
  result <- Socket.withFdSocket socket (writeReady . fromIntegral)
  when (result < 0) connectionFailure
  if result == 0 then threadDelay 10000 >> waitWrite socket else do
    err <- Socket.getSocketOption socket Socket.SoError
    when (err /= 0) connectionFailure

backend :: Socket.Socket -> TLS.Backend
backend socket = TLS.Backend (pure ()) (Socket.close socket) (sendAll socket) receive
  where
    -- TLS requires exactly the requested record bytes, unless the peer closes.
    receive count = BS.concat <$> loop count
    loop 0 = pure []
    loop count = do
      chunk <- receiveSome socket count
      if BS.null chunk then pure [] else (chunk :) <$> loop (count - BS.length chunk)

receiveSome :: Socket.Socket -> Int -> IO BS.ByteString
receiveSome socket count = Bytes.createAndTrim count $ \buffer ->
  Socket.withFdSocket socket $ \fd ->
    let readChunk = do
          allowInterrupt
          result <- recvSocket (fromIntegral fd) buffer (fromIntegral count)
          if result >= 0 then pure (fromIntegral result) else do
            if result == -10035 then threadDelay 10000 >> readChunk else connectionFailure
    in readChunk

sendAll :: Socket.Socket -> BS.ByteString -> IO ()
sendAll socket bytes = unless (BS.null bytes) $ do
  allowInterrupt
  sent <- BS.useAsCStringLen bytes $ \(buffer,count) -> Socket.withFdSocket socket $ \fd ->
    sendSocket (fromIntegral fd) (castPtr buffer) (fromIntegral count)
  if sent > 0 then sendAll socket (BS.drop (fromIntegral sent) bytes) else do
    if sent == -10035 then threadDelay 10000 >> sendAll socket bytes else connectionFailure

foreign import ccall unsafe "sapiens_telemetry_nonblocking"
  nonblockingSocket :: WordPtr -> IO CInt
foreign import ccall unsafe "sapiens_telemetry_connect"
  connectSocket :: WordPtr -> Ptr () -> CInt -> IO CInt
foreign import ccall unsafe "sapiens_telemetry_recv"
  recvSocket :: WordPtr -> Ptr Word8 -> CInt -> IO CInt
foreign import ccall unsafe "sapiens_telemetry_send"
  sendSocket :: WordPtr -> Ptr Word8 -> CInt -> IO CInt
foreign import ccall unsafe "sapiens_telemetry_write_ready"
  writeReady :: WordPtr -> IO CInt
foreign import ccall unsafe "sapiens_telemetry_dns_start"
  dnsStart :: CWString -> CWString -> IO (Ptr ())
foreign import ccall unsafe "sapiens_telemetry_dns_poll"
  dnsPoll :: Ptr () -> IO CInt
foreign import ccall unsafe "sapiens_telemetry_dns_address"
  dnsAddress :: Ptr () -> CUInt -> Ptr () -> IO CInt
foreign import ccall safe "sapiens_telemetry_dns_free"
  dnsFree :: Ptr () -> IO ()
