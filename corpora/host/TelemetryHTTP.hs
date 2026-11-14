{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TelemetryHTTP (managerSettings, newManagedClient) where

import Control.Concurrent.MVar
import Control.Exception (IOException, bracketOnError, catch, finally, mask_)
import Data.IORef
import qualified Data.Map.Strict as Map
import Data.Unique (newUnique)
import Data.Default.Class (def)
import Network.HTTP.Client (Manager, ManagerSettings (..), newManager)
import Network.HTTP.Client.Internal (Connection (..), managerTlsProxyConnection)
import Network.TLS (Supported (..), Version (TLS12, TLS13))
import Network.TLS.Extra.Cipher (ciphersuite_default)
#ifdef mingw32_HOST_OS
import qualified TelemetryWindowsHTTP as Windows
#else
import Network.Connection (TLSSettings (..))
import Network.HTTP.Client.TLS (mkManagerSettings)
#endif

-- Keep the TLS policy identical across the platform-specific IO backends.
managerSettings :: IO ManagerSettings
managerSettings =
#ifdef mingw32_HOST_OS
  Windows.managerSettings supported
#else
  pure (mkManagerSettings (TLSSettingsSimple False False True supported) Nothing)
#endif
  where
    supported = def
      { supportedVersions = [TLS13, TLS12], supportedCiphers = ciphersuite_default }

-- http-client 0.7.19's closeManager is a no-op. Keep explicit ownership of
-- connections, and release the Manager reference even if emit/stop are retained.
-- Entries are removed on normal close, so this ledger does not grow over time.
newManagedClient :: ManagerSettings -> IO (IORef (Maybe Manager), IO ())
newManagedClient settings = do
  connections <- newMVar Map.empty
  let track acquire = bracketOnError acquire connectionClose $ \connection -> do
        identity <- newUnique
        let close = connectionClose connection `finally`
              modifyMVar_ connections (pure . Map.delete identity)
        modifyMVar_ connections (pure . Map.insert identity close)
        pure connection {connectionClose = close}
      tracked = settings
        { managerRawConnection = do
            open <- managerRawConnection settings
            pure (\address host port -> track (open address host port))
        , managerTlsConnection = do
            open <- managerTlsConnection settings
            pure (\address host port -> track (open address host port))
        , managerTlsProxyConnection = do
            open <- managerTlsProxyConnection settings
            pure (\bytes check host address proxy port -> track (open bytes check host address proxy port))
        }
      closeConnections = do
        close <- modifyMVar connections (\current -> pure (Map.empty, Map.elems current))
        mapM_ (\action -> action `catch` \(_ :: IOException) -> pure ()) close
  client <- newIORef . Just =<< newManager tracked
  let cleanup = mask_ (closeConnections `finally` writeIORef client Nothing)
  pure (client, cleanup)
