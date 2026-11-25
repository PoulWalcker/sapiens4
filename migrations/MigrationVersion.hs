-- Public app/data versions are independent of installer UUIDs and domain schemas.
module MigrationVersion where
import Data.Text (Text)
newtype AppVersion = AppVersion Text deriving (Eq, Ord, Show)
baselineVersion, currentVersion :: AppVersion
baselineVersion = AppVersion "v00.01.99"
currentVersion = AppVersion "v00.02.03"
versionText :: AppVersion -> Text
versionText (AppVersion value) = value
versionKey :: Text
versionKey = "local/app-version"
