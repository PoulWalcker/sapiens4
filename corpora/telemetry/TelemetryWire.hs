module TelemetryWire
 ( Saved(..), ConsentView(..), encodeSaved, decodeSaved, decodeCredentials, encodeBatch, recordFits
 , parseClientContext, clientContextValue, parseHeartbeat
 ) where

import Control.Applicative ((<|>))
import Control.Monad (guard)
import Data.Aeson
import Data.Aeson.Types (Pair, parseMaybe)
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as BL
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.UUID as UUID
import TelemetryModel
import TelemetryProtocol (Credentials(..), batchLimit, requestByteLimit)

-- Persistence and HTTP representations stay here; the runtime keeps typed data.
data Saved = Saved Bool (Maybe Credentials)
instance ToJSON Saved where
 toJSON (Saved on credentials) = object $ ["enabled" .= on] ++ case credentials of
  Nothing -> []
  Just (Credentials ident password) -> ["installation_id" .= ident,"password" .= password]
instance FromJSON Saved where
 parseJSON = withObject "telemetry settings" $ \fields -> do
  on <- fields .: "enabled"
  credentials <- if KM.member "installation_id" fields || KM.member "password" fields
   then Just <$> parseJSON (Object fields) else pure Nothing
  pure (Saved on credentials)

encodeSaved :: Saved -> BL.ByteString
encodeSaved = encode

decodeSaved :: BL.ByteString -> Either String Saved
decodeSaved = eitherDecode

decodeCredentials :: BL.ByteString -> Maybe Credentials
decodeCredentials = decode

data ConsentView = ConsentView
 { consentEnabled :: Bool, consentAvailable :: Bool, consentError :: Text }
instance ToJSON ConsentView where
 toJSON view = object
  ["enabled" .= consentEnabled view,"available" .= consentAvailable view
  ,"heartbeat_interval_ms" .= (realToFrac heartbeatInterval * 1000 :: Double),"error" .= consentError view]

clientKindName :: ClientKind -> Text
clientKindName kind = case kind of
 Native -> "native"
 Browser -> "browser"
 InstalledWebApp -> "installed_webapp"
 UnknownClient -> "unknown"

familyName :: DeviceFamily -> Text
familyName family = case family of
 DesktopDevice -> "desktop"
 Phone -> "phone"
 Tablet -> "tablet"
 UnknownFamily -> "unknown"

osName :: ClientOS -> Text
osName platform = case platform of
 Windows -> "windows"
 MacOS -> "macos"
 Linux -> "linux"
 Android -> "android"
 IOS -> "ios"
 IPadOS -> "ipados"
 UnknownOS -> "unknown"

-- Missing, malformed and future categories retain the protocol's "unknown" behavior.
category :: a -> [(Text,a)] -> Value -> a
category unknown choices value = case value of
 String name -> fromMaybe unknown (lookup name choices)
 _ -> unknown

field :: Key -> Object -> Value
field key = fromMaybe Null . KM.lookup key

parseClientContext :: Value -> Maybe ClientContext
parseClientContext (Object fields) = Just (ClientContext kind family platform identity)
 where
 kind = category UnknownClient [("native",Native),("browser",Browser),("installed_webapp",InstalledWebApp)] (field "client_kind" fields)
 family = category UnknownFamily [("desktop",DesktopDevice),("phone",Phone),("tablet",Tablet)] (field "client_device_family" fields)
 platform = category UnknownOS [("windows",Windows),("macos",MacOS),("linux",Linux),("android",Android),("ios",IOS),("ipados",IPadOS)] (field "client_os" fields)
 identity = do
  guard (kind==Native)
  String key <- pure (field "source_device_id" fields)
  String name <- pure (field "source_device_id_source" fields)
  source <- lookup name [(deviceSourceName value,value) | value <- [MacPlatformUUID,WindowsSMBIOSUUID,LinuxMachineID]]
  guard (T.length key==64 && T.all (`elem` ("0123456789abcdef"::String)) key)
  pure (DeviceIdentity source key)
parseClientContext _ = Nothing

deviceFields :: Text -> Maybe DeviceIdentity -> [Pair]
deviceFields prefix identity =
 [K.fromText (prefix<>"device_id") .= fmap (\(DeviceIdentity _ key) -> key) identity
 ,K.fromText (prefix<>"device_id_source") .= fmap (\(DeviceIdentity source _) -> deviceSourceName source) identity]

clientFields :: Maybe ClientContext -> [Pair]
clientFields value =
 let context = fromMaybe (ClientContext UnknownClient UnknownFamily UnknownOS Nothing) value
 in ["client_kind" .= clientKindName (clientKind context)
    ,"client_device_family" .= familyName (clientFamily context)
    ,"client_os" .= osName (clientOS context)] ++ deviceFields "source_" (clientDevice context)

clientContextValue :: Maybe ClientContext -> Value
clientContextValue Nothing = Null
clientContextValue (Just context) = object (clientFields (Just context))

parseHeartbeat :: Maybe CorporaId -> Maybe ClientContext -> Value -> Maybe HeartbeatDetails
parseHeartbeat corpora context value@(Object fields) = do
 guard (field "ui_visible" fields==Bool True && BL.length (encode value)<=8192)
 let selected = do
      String ident <- pure (field "corpora_id" fields)
      CorporaId . UUID.toText <$> UUID.fromText ident
     mode = category UnknownUIMode [("local",LocalUI),("remote_control",RemoteControl)] (field "ui_mode" fields)
     target = category UnknownTarget [("local",LocalTarget),("remote",RemoteTarget)] (field "target_location" fields)
 pure (HeartbeatDetails (corpora <|> selected) context mode target (parseMaybe parseJSON (field "timestamp" fields)))
parseHeartbeat _ _ _ = Nothing

recordValue :: EventRecord -> Value
recordValue record = object $
 ["event_id" .= UUID.toText (eventId record),"timestamp" .= occurredAt record] ++ eventFields (recordEvent record)

eventFields :: Event -> [Pair]
eventFields event = case event of
 AppStarted -> named "app_started" []
 AppUpdated -> named "app_update" []
 CorporaCreated corpora -> named "corpora_created" (scoped corpora [])
 RequestAccepted corpora details -> named "request_accepted" $ scoped corpora $
  ["agent_id" .= requestAgent details,"flow" .= requestFlow details
  ,"is_clarification" .= requestClarification details,"selected_provider" .= requestProvider details
  ,"selected_model" .= requestModel details,"selected_effort" .= requestEffort details
  ,"request_transport" .= transportName (requestTransport details)] ++
  attachmentFields (requestAttachments details) ++ clientFields (requestClient details)
 RemotePaired corpora context -> named "remote_paired" (scoped corpora (clientFields context))
 PulseId -> named "pulse_id" []
 UIHeartbeat details -> named "ui_heartbeat" $
  maybe id scoped (heartbeatCorpora details)
   (["ui_mode" .= modeName (heartbeatMode details),"target_location" .= targetName (heartbeatTarget details)] ++ clientFields (heartbeatClient details))
 TelemetryConsentChanged on -> named "telemetry_consent_changed" ["enabled" .= on]
 where
 named name fields = ("event_type" .= (name::Text)):fields
 scoped (CorporaId ident) fields = ("corpora_id" .= ident):fields
 transportName transport = case transport of Direct -> "direct"; Remote -> "remote"; UnknownTransport -> "unknown" :: Text
 modeName mode = case mode of LocalUI -> "local"; RemoteControl -> "remote_control"; UnknownUIMode -> "unknown" :: Text
 targetName target = case target of LocalTarget -> "local"; RemoteTarget -> "remote"; UnknownTarget -> "unknown" :: Text

attachmentFields :: AttachmentCounts -> [Pair]
attachmentFields counts =
 ["voice_attachment_count" .= voiceAttachments counts,"image_attachment_count" .= imageAttachments counts
 ,"document_attachment_count" .= documentAttachments counts,"link_attachment_count" .= linkAttachments counts
 ,"filepath_attachment_count" .= filepathAttachments counts]

hostFields :: HostContext -> [Pair]
hostFields context =
 ["app_version" .= appVersion context,"host_os" .= osName (hostOS context),"host_arch" .= hostArch context
 ,"runtime_kind" .= (case hostRuntime context of Desktop -> "desktop"; Headless -> "headless" :: Text)]

encodeBatch :: Batch -> BL.ByteString
encodeBatch (Batch context events) = encode $ object
 ["context" .= object (hostFields (batchHost context) ++ ["installation_id" .= batchInstallation context] ++ deviceFields "host_" (batchDevice context))
 ,"events" .= map recordValue events]

recordFits :: HostContext -> EventRecord -> Bool
recordFits context record = BL.length (encode (recordValue record))<=fromIntegral limit
 where
 -- Preserve the existing per-event budget, including room for installation/device context.
 limit = (requestByteLimit - fromIntegral (BL.length (encode (object ["context" .= object (hostFields context),"events" .= ([]::[Value])]))) - 384 - batchLimit) `div` batchLimit
