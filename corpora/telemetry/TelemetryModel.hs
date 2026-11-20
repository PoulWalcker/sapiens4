module TelemetryModel where

import Data.Int (Int64)
import Data.Text (Text)
import Data.Time (NominalDiffTime)
import Data.UUID (UUID)
import Domain (AgentId, Flow)

data RuntimeKind = Desktop | Headless deriving (Eq, Show)
data RequestTransport = Direct | Remote | UnknownTransport deriving (Eq, Show)
data ClientKind = Native | Browser | InstalledWebApp | UnknownClient deriving (Eq, Show, Enum, Bounded)
data DeviceFamily = DesktopDevice | Phone | Tablet | UnknownFamily deriving (Eq, Show, Enum, Bounded)
data ClientOS = Windows | MacOS | Linux | Android | IOS | IPadOS | UnknownOS deriving (Eq, Show, Enum, Bounded)
data UIMode = LocalUI | RemoteControl | UnknownUIMode deriving (Eq, Show, Enum, Bounded)
data TargetLocation = LocalTarget | RemoteTarget | UnknownTarget deriving (Eq, Show, Enum, Bounded)

data DeviceSource = MacPlatformUUID | WindowsSMBIOSUUID | LinuxMachineID deriving (Eq, Show, Enum, Bounded)
data DeviceIdentity = DeviceIdentity DeviceSource Text deriving (Eq, Show)

-- These stable tags are also part of the device identity's HMAC namespace.
deviceSourceName :: DeviceSource -> Text
deviceSourceName source = case source of
 MacPlatformUUID -> "macos_platform_uuid"
 WindowsSMBIOSUUID -> "windows_smbios_uuid"
 LinuxMachineID -> "linux_machine_id"

data ClientContext = ClientContext
 { clientKind :: ClientKind
 , clientFamily :: DeviceFamily
 , clientOS :: ClientOS
 , clientDevice :: Maybe DeviceIdentity
 } deriving (Eq, Show)

heartbeatInterval :: NominalDiffTime
heartbeatInterval = 60 * 60

newtype CorporaId = CorporaId Text deriving (Eq, Show)

data AttachmentCounts = AttachmentCounts
 { voiceAttachments :: Int
 , imageAttachments :: Int
 , documentAttachments :: Int
 , linkAttachments :: Int
 , filepathAttachments :: Int
 } deriving (Eq, Show)

data RequestDetails = RequestDetails
 { requestAgent :: AgentId
 , requestFlow :: Flow
 , requestClarification :: Bool
 , requestProvider :: Text
 , requestModel :: Text
 , requestEffort :: Text
 , requestAttachments :: AttachmentCounts
 , requestTransport :: RequestTransport
 , requestClient :: Maybe ClientContext
 } deriving (Eq, Show)

data HeartbeatDetails = HeartbeatDetails
 { heartbeatCorpora :: Maybe CorporaId
 , heartbeatClient :: Maybe ClientContext
 , heartbeatMode :: UIMode
 , heartbeatTarget :: TargetLocation
 , heartbeatTimestamp :: Maybe Int64
 } deriving (Eq, Show)

data Event
 = AppStarted
 | AppUpdated
 | CorporaCreated CorporaId
 | RequestAccepted CorporaId RequestDetails
 | RemotePaired CorporaId (Maybe ClientContext)
 | PulseId
 | UIHeartbeat HeartbeatDetails
 | TelemetryConsentChanged Bool
 deriving (Eq, Show)

data EventRecord = EventRecord
 { eventId :: UUID
 , occurredAt :: Int64
 , recordEvent :: Event
 } deriving (Eq, Show)

data HostContext = HostContext
 { appVersion :: Text
 , hostOS :: ClientOS
 , hostArch :: Text
 , hostRuntime :: RuntimeKind
 } deriving (Eq, Show)

data BatchContext = BatchContext
 { batchHost :: HostContext
 , batchInstallation :: Text
 , batchDevice :: Maybe DeviceIdentity
 } deriving (Eq, Show)

data Batch = Batch BatchContext [EventRecord] deriving (Eq, Show)
