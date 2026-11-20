module TelemetryProtocol where

import Control.Monad (unless)
import Data.Aeson
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Types (Parser)
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.UUID as UUID
import qualified Data.Vector as V

eventsPath :: String
eventsPath = "/events"

batchLimit, requestByteLimit, objectFieldLimit, fieldNameLimit :: Int
batchLimit = 10
requestByteLimit = 65536
objectFieldLimit = 64
fieldNameLimit = 128

uuidText :: Text -> Parser Text
uuidText ident = maybe (fail "Expected a UUID") (pure . UUID.toText) (UUID.fromText ident)

boundedObject :: Value -> Parser Object
boundedObject = withObject "bounded object" $ \o -> do
 unless (KM.size o <= objectFieldLimit && all (\k -> let t=K.toText k in not (T.null t) && T.length t<=fieldNameLimit) (KM.keys o)) (fail "Too many fields or invalid field name")
 pure o

envelope :: Value -> Parser (Text, Object, [Object])
envelope = withObject "telemetry envelope" $ \o -> do
 context <- o .: "context" >>= boundedObject
 version <- context .: "app_version"
 unless (not (T.null version) && T.length version<=200) (fail "Invalid app_version")
 installation <- context .: "installation_id" >>= uuidText
 events <- o .: "events"
 unless (not (V.null events) && V.length events<=batchLimit) (fail "Expected 1..10 events")
 rows <- mapM event (V.toList events)
 pure (installation,KM.insert "installation_id" (String installation) context,rows)
 where
 event value = do
  row <- boundedObject value
  _ <- row .: "event_id" >>= uuidText
  kind <- row .: "event_type"
  unless (not (T.null kind) && T.length kind<=200) (fail "Invalid event_type")
  _ <- row .: "timestamp" :: Parser Int64
  pure row
