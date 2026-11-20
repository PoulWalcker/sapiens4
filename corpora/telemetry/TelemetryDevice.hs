module TelemetryDevice
 ( nativeOS, readNativeDevice ) where

import Control.Exception
import Crypto.Hash.SHA256 (hmac)
import qualified Data.ByteString as B
import Data.Char (isHexDigit)
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.UUID as UUID
import Numeric (showHex)
import System.Environment (getEnvironment)
import System.Exit (ExitCode(..))
import System.Info (os)
import System.Process
import System.Timeout (timeout)
import TelemetryModel

nativeOS :: ClientOS
nativeOS = case os of "darwin" -> MacOS; "mingw32" -> Windows; "linux" -> Linux; _ -> UnknownOS

deviceNamespace :: B.ByteString
deviceNamespace = "sapiens4.telemetry.device.v1:"

deriveDevice :: DeviceSource -> Text -> Maybe DeviceIdentity
deriveDevice source input = do
 normalized <- if source==LinuxMachineID then Just (T.toLower (T.strip input))
  else T.filter (/='-') . UUID.toText <$> UUID.fromText (T.strip input)
 if T.length normalized/=32 || not (T.all isHexDigit normalized) || T.all (=='0') normalized || T.all (=='f') normalized then Nothing
 else let digest = hmac (TE.encodeUtf8 normalized) (deviceNamespace<>TE.encodeUtf8 (deviceSourceName source))
          hex byte = let value=showHex byte "" in if length value==1 then '0':value else value
      in Just (DeviceIdentity source (T.pack (concatMap hex (B.unpack digest))))

nativeLookupTimeoutMicros :: Int
nativeLookupTimeoutMicros = 2 * 1000000

readNativeDevice :: IO (Maybe DeviceIdentity)
readNativeDevice = do
 result <- timeout nativeLookupTimeoutMicros (lookupDevice `catch` \(_::IOException) -> pure Nothing)
 pure (result >>= id)
 where
 command binary arguments = do
  environment <- filter ((/="PSModulePath") . fst) <$> getEnvironment
  (status,output,_) <- readCreateProcessWithExitCode (proc binary arguments)
   {env=Just environment,use_process_jobs=True,detach_console=os=="mingw32"} ""
  pure (if status==ExitSuccess then Just (T.strip (T.pack output)) else Nothing)
 lookupDevice = case os of
  "darwin" -> do
   output <- command "/usr/sbin/ioreg" ["-rd1","-c","IOPlatformExpertDevice"]
   let extract line = case T.splitOn "\"" line of
        _:"IOPlatformUUID":_:value:_ -> Just value
        _ -> Nothing
   pure (output >>= listToMaybe . mapMaybe extract . T.lines >>= deriveDevice MacPlatformUUID)
  "mingw32" -> do
   output <- command "powershell.exe" ["-NoProfile","-NonInteractive","-Command","$ErrorActionPreference='Stop'; (Get-CimInstance -ClassName Win32_ComputerSystemProduct -Property UUID).UUID"]
   pure (output >>= deriveDevice WindowsSMBIOSUUID)
  "linux" -> either (const Nothing) (deriveDevice LinuxMachineID) . TE.decodeUtf8' <$> B.readFile "/etc/machine-id"
  _ -> pure Nothing
