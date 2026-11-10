{-# LANGUAGE OverloadedStrings #-}
module Format
 ( Inventory(..), InventoryEntry(..), ProviderIdentity(..), Provider, providerIdentity, providerEntries
 , ProviderLayout, layoutRelease, layoutPlatform, layoutEntry, layoutExec, layoutResources, layoutPath
 , ProviderLock(..), Download(..), lockAsset, newProvider, packagedProvider
 , Provenance(..), PackageIdentity(..), ProviderSummary, summaryVersion, Manifest, manifestIdentity, manifestProvider
 , manifestSigning, manifestProviderHash, FinalizedManifest, finalizedManifest, finalizedSigning, providerSummary, newManifest, sealManifest, compatibilityManifest
 , Artifact(..), ArtifactPair(..), Receipt(..)
 , CompatibilityRule(..), BinaryAudit(..), CompatibilityAudit(..)
 , readJSON, writeJSON, printJSON, obj, canonical, hash, hex, safeRelativePath ) where

import Control.Monad
import Data.Aeson
import qualified Data.Aeson.Encoding as E
import Data.Aeson.Types (Parser)
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Crypto.Hash.SHA256 as SHA
import Data.Char (isHexDigit, isLower, isDigit)
import Data.Foldable (toList)
import Data.List (sortOn, isPrefixOf)
import Data.Maybe (isJust)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Numeric (showHex)
import System.FilePath
import Options hiding (required)

instance FromJSON Version where
 parseJSON = withText "SemVer" (either fail pure . parseVersion . T.unpack)
instance ToJSON Version where toJSON = toJSON . versionText

obj :: [(T.Text,Value)] -> Value
obj = Object . KM.fromList . map (\(key,value) -> (K.fromText key,value))
readJSON :: FromJSON a => FilePath -> IO a
readJSON path = L.readFile path >>= either (fail . ((path++": ")++)) pure . eitherDecode
writeJSON :: ToJSON a => FilePath -> a -> IO ()
writeJSON path value = L.writeFile path (encode value<>"\n")
printJSON :: ToJSON a => a -> IO ()
printJSON = L.putStr . (<>"\n") . encode

-- Optional means absent; a present null still goes through its field parser.
optional :: FromJSON a => Object -> Key -> Parser (Maybe a)
optional fields key = if KM.member key fields then Just <$> fields .: key else pure Nothing
check :: Bool -> String -> Parser ()
check condition message = unless condition (fail message)
checksum :: Int -> T.Text -> Parser T.Text
checksum size value = do
 check (T.length value==size && T.all (\c -> isDigit c || isHexDigit c && isLower c) value) "Invalid SHA256/commit digest"
 pure value
nonempty :: String -> Parser String
nonempty value = check (not $ null value) "Empty required string" >> pure value
safeRelativePath :: FilePath -> Bool
safeRelativePath path = not (null path) && not (isAbsolute path) && not (any (`elem` path) ['\\',':','\0','\n','\r']) && ".." `notElem` splitDirectories path
relative :: FilePath -> Parser FilePath
relative path = check (safeRelativePath path) "Unsafe relative package path" >> pure path
platformJSON :: Value -> Parser Platform
platformJSON = withText "platform" (either fail pure . parsePlatform . T.unpack)
providerPlatformJSON :: Value -> Parser Platform
providerPlatformJSON = withText "provider platform" (either fail pure . parseProviderPlatform . T.unpack)
signingJSON :: Value -> Parser Signing
signingJSON = withText "signing mode" (either fail pure . parseSigning . T.unpack)

-- These fields are used by integrity checks; malformed paths/sizes/hashes never
-- enter those checks. The list decoder also rejects duplicates and sorts once.
data InventoryEntry = InventoryEntry { inventoryPath :: FilePath, inventorySize :: Integer, inventoryHash :: T.Text } deriving (Eq,Show)
instance FromJSON InventoryEntry where
 parseJSON = withObject "inventory entry" $ \o -> do
  check (KM.size o==3) "Inventory entries require exactly path, size and sha256"
  path<-o .: "path" >>= relative;size<-o .: "size";check (size>=0) "Negative inventory size"
  InventoryEntry path size <$> (o .: "sha256" >>= checksum 64)
instance ToJSON InventoryEntry where
 toJSON e = obj [("path",toJSON $ inventoryPath e),("size",toJSON $ inventorySize e),("sha256",toJSON $ inventoryHash e)]
decodeEntries :: Value -> Parser [InventoryEntry]
decodeEntries value = do
 entries<-parseJSON value
 let ordered=sortOn inventoryPath entries;paths=map inventoryPath ordered
 check (and $ zipWith (/=) paths (drop 1 paths)) "Duplicate inventory paths"
 pure ordered

data Inventory = Inventory [InventoryEntry]
instance FromJSON Inventory where
 parseJSON = withObject "package inventory" $ \o -> do
  schema <- o .: "schemaVersion"
  check (schema == (1 :: Int)) "Unsupported package inventory schema"
  Inventory <$> (o .: "files" >>= decodeEntries)
instance ToJSON Inventory where
 toJSON (Inventory entries) = obj [("schemaVersion",toJSON (1::Int)),("files",toJSON entries)]

data ProviderIdentity = ProviderIdentity
 { identityVersion :: String, identityPlatform :: Platform, identityEntry :: FilePath
 , identityArchive :: T.Text, identityCommit :: Maybe T.Text } deriving (Eq,Show)
instance FromJSON ProviderIdentity where
 parseJSON = withObject "provider identity" $ \o -> ProviderIdentity
  <$> (o .: "version" >>= nonempty) <*> (o .: "platform" >>= providerPlatformJSON)
  <*> (o .: "entrypoint" >>= relative) <*> (o .: "archiveSha256" >>= checksum 64)
  <*> (optional o "upstreamCommit" >>= traverse (checksum 40))

data ProviderLayout = ProviderLayout
 { layoutRelease :: String, layoutPlatform :: Platform, layoutEntry :: FilePath
 , layoutExec :: Maybe FilePath, layoutResources :: FilePath, layoutPath :: FilePath, layoutWire :: Object }
instance FromJSON ProviderLayout where
 parseJSON = withObject "Codex package layout" $ \o -> do
  schema<-o .: "layoutVersion";check (schema==(1::Int)) "Unsupported provider layoutVersion"
  variant<-o .: "variant";check (variant==("codex"::String)) "Unexpected provider variant"
  target<-o .: "target"
  platform<-case target :: String of "aarch64-apple-darwin"->pure MacOS;"x86_64-pc-windows-msvc"->pure Windows;_->fail "Unsupported provider target"
  ProviderLayout <$> (o .: "version" >>= nonempty) <*> pure platform <*> (o .: "entrypoint" >>= relative)
   <*> (o .:? "execServer" >>= traverse relative) <*> (o .: "resourcesDir" >>= relative) <*> (o .: "pathDir" >>= relative) <*> pure o
instance ToJSON ProviderLayout where toJSON = Object . layoutWire

-- Opaque original objects preserve upstream extension fields on serialization;
-- packaging code only has access to validated identity/layout/inventory fields.
data Provider = Provider { providerIdentity :: ProviderIdentity, providerEntries :: [InventoryEntry], providerWire :: Object }
instance FromJSON Provider where
 parseJSON value = withObject "provider metadata" (\o -> do
  schema<-o .: "schema";check (schema==(1::Int)) "Unsupported provider metadata schema"
  identity<-parseJSON value;entries<-o .: "files" >>= decodeEntries
  layout<-o .: "upstreamPackage"
  check (layoutRelease layout==identityVersion identity && layoutPlatform layout==identityPlatform identity && layoutEntry layout==identityEntry identity) "Provider identity/layout mismatch"
  stage<-optional o "inventoryStage"
  forM_ stage $ \s -> check (s `elem` ["packaged","signed-package"::String]) "Invalid provider inventory stage"
  void (optional o "upstreamInventory" :: Parser (Maybe String))
  pure (Provider identity entries o)) value
instance ToJSON Provider where toJSON = Object . providerWire
newProvider :: ProviderIdentity -> ProviderLayout -> [InventoryEntry] -> Provider
newProvider identity layout entries = Provider identity entries $ KM.fromList
 [("schema",toJSON (1::Int)),("version",toJSON $ identityVersion identity),("upstreamCommit",toJSON $ identityCommit identity)
 ,("platform",toJSON $ providerPlatform $ identityPlatform identity),("entrypoint",toJSON $ identityEntry identity)
 ,("archiveSha256",toJSON $ identityArchive identity),("upstreamPackage",toJSON layout),("files",toJSON entries)]
packagedProvider :: Signing -> [InventoryEntry] -> Provider -> Provider
packagedProvider signing entries p = p
 { providerEntries=entries
 , providerWire=KM.insert "files" (toJSON entries) $
    KM.insert "upstreamInventory" (String "sapiens-provider-upstream.json") $
    KM.insert "inventoryStage" (String $ if signing==Unsigned then "packaged" else "signed-package") (providerWire p) }

data Download = Download { downloadURL :: String, downloadHash :: T.Text, downloadSize :: Maybe Integer }
instance FromJSON Download where
 parseJSON = withObject "locked download" $ \o -> do
  size<-optional o "size";check (maybe True (>=0) size) "Negative locked download size"
  Download <$> (o .: "url" >>= nonempty) <*> (o .: "sha256" >>= checksum 64) <*> pure size
data ProviderLock = ProviderLock
 { lockVersion :: String, lockCommit :: T.Text, lockMac :: (FilePath,Download), lockWindows :: (FilePath,Download), lockNotices :: [(FilePath,Download)] }
instance FromJSON ProviderLock where
 parseJSON = withObject "provider lock" $ \o -> do
  schema<-o .: "schema";check (schema==(1::Int)) "Unsupported provider lock schema"
  platforms<-o .: "platforms"
  let asset value = withObject "provider asset" (\a -> (,) <$> (a .: "entrypoint" >>= relative) <*> parseJSON value) value
      notice value = withObject "provider notice" (\a -> (,) <$> (a .: "path" >>= relative) <*> parseJSON value) value
  ProviderLock <$> (o .: "version" >>= nonempty) <*> (o .: "upstreamCommit" >>= checksum 40)
   <*> (platforms .: "darwin-arm64" >>= asset) <*> (platforms .: "windows-x64" >>= asset) <*> (o .: "notices" >>= mapM notice)
lockAsset :: Platform -> ProviderLock -> (FilePath,Download)
lockAsset MacOS = lockMac
lockAsset Windows = lockWindows

data Provenance = Provenance { sourceCommit :: T.Text, sourceDirty :: Bool, sourceSubmodules :: M.Map T.Text T.Text }
instance FromJSON Provenance where
 parseJSON = withObject "source provenance" $ \o -> Provenance <$> (o .: "sourceCommit" >>= checksum 40) <*> o .: "sourceDirty" <*> (o .: "submodules" >>= traverse (checksum 40))
instance ToJSON Provenance where
 toJSON p = obj [("sourceCommit",toJSON $ sourceCommit p),("sourceDirty",Bool $ sourceDirty p),("submodules",toJSON $ sourceSubmodules p)]

data ProviderSummary = ProviderSummary { summaryVersion :: String, summaryEntry :: FilePath, summaryArchive :: T.Text } deriving Eq
instance FromJSON ProviderSummary where
 parseJSON = withObject "manifest provider" $ \o -> do
  name<-o .: "name";check (name==("codex"::String)) "Unexpected provider name"
  entry<-o .: "entrypoint" >>= relative;check ("codex/" `isPrefixOf` entry) "Provider entrypoint must stay inside codex/"
  ProviderSummary <$> (o .: "version" >>= nonempty) <*> pure entry <*> (o .: "archiveSha256" >>= checksum 64)
instance ToJSON ProviderSummary where
 toJSON p = obj [("name",String "codex"),("version",toJSON $ summaryVersion p),("entrypoint",toJSON $ summaryEntry p),("archiveSha256",toJSON $ summaryArchive p)]

data PackageIdentity = PackageIdentity
 { packageChannel :: Channel, packageVersion :: Version, packagePlatform :: Platform, packageBuildId :: T.Text, packageCommit :: T.Text, packageDirty :: Bool, packageComplete :: Bool } deriving Eq
instance FromJSON PackageIdentity where
 parseJSON = withObject "package identity" $ \o -> do
  app<-o .: "appId";repository<-o .: "repository";channel<-o .: "channel" >>= either fail pure . parseChannel;support<-o .: "supportStatus"
  check (app==appId channel && repository==("PoulWalcker/sapiens4"::String) && support==("declared-target"::String)) "Unexpected package identity/support status"
  platform<-o .: "platform" >>= platformJSON;arch<-o .: "architecture";floor<-o .: "minimumOS"
  check (arch==architecture platform && floor==(if platform==MacOS then Just "13.0" else Nothing :: Maybe String)) "Platform/architecture/minimumOS mismatch"
  status<-o .: "packageStatus";complete<-case status :: String of "provider-complete"->pure True;"validation-provider-incomplete"->pure False;_->fail "Invalid package status"
  version<-o .: "version"
  PackageIdentity channel version platform <$> (o .: "buildId" >>= checksum 64) <*> (o .: "sourceCommit" >>= checksum 40) <*> o .: "sourceDirty" <*> pure complete

data Manifest = Manifest { manifestIdentity :: PackageIdentity, manifestProvider :: Maybe ProviderSummary, manifestSigning :: Maybe Signing, manifestProviderHash :: Maybe T.Text, manifestWire :: Object }
instance FromJSON Manifest where
 parseJSON value = withObject "package manifest" (\o -> do
  schema<-o .: "schemaVersion";check (schema==(1::Int)) "Unsupported package manifest schema"
  identity<-parseJSON value;provider<-o .: "provider";check (packageComplete identity==maybe False (const True) provider) "Provider status mismatch"
  void (o .: "submodules" >>= traverse (checksum 40) :: Parser (M.Map T.Text T.Text))
  void (o .: "sourceTreeSha256" >>= checksum 64)
  void (o .: "toolchain" :: Parser (M.Map T.Text String))
  discovery<-o .: "updateDiscovery";check (not discovery) "D1 update discovery must be disabled"
  signing<-traverse signingJSON (KM.lookup "signing" o);digest<-optional o "providerInventorySha256" >>= traverse (checksum 64)
  forM_ signing $ either fail pure . validateSigning (packagePlatform identity)
  check (isJust digest == (isJust signing && isJust provider)) "Inconsistent provider inventory seal"
  pure (Manifest identity provider signing digest o)) value
instance ToJSON Manifest where toJSON = Object . manifestWire

-- A receipt/verification boundary cannot accidentally accept a staged manifest.
data FinalizedManifest = FinalizedManifest { finalizedManifest :: Manifest, finalizedSigning :: Signing }
instance FromJSON FinalizedManifest where
 parseJSON value = do
  manifest<-parseJSON value
  signing<-maybe (fail "Finalized manifest signing mode missing") pure (manifestSigning manifest)
  pure (FinalizedManifest manifest signing)

providerSummary :: Provider -> ProviderSummary
providerSummary p = let i=providerIdentity p in ProviderSummary (identityVersion i) ("codex/"++identityEntry i) (identityArchive i)

-- Build identity hashes the current deterministic input document.
newManifest :: Channel -> Provenance -> T.Text -> Platform -> Version -> Maybe Provider -> [(T.Text,String)] -> Manifest
newManifest channel source tree platform version provider tools = Manifest identity summary Nothing Nothing document
 where
 summary = providerSummary <$> provider
 inputs = KM.fromList
  [("sourceTreeSha256",String tree)
  ,("platform",toJSON $ platformName platform)
  ,("architecture",toJSON $ architecture platform)
  ,("version",toJSON version)
  ,("repository",String "PoulWalcker/sapiens4")
  ,("channel",toJSON $ channelName channel)
  ,("provider",toJSON summary)
  ,("minimumOS",if platform==MacOS then String "13.0" else Null)
  ,("supportStatus",String "declared-target")
  ,("toolchain",toJSON $ M.fromList tools)
  ,("sourceCommit",toJSON $ sourceCommit source)
  ,("sourceDirty",Bool $ sourceDirty source)
  ,("submodules",toJSON $ sourceSubmodules source)]
 buildId = hash $ canonical $ Object inputs
 identity = PackageIdentity channel version platform buildId (sourceCommit source) (sourceDirty source) (isJust provider)
 document = KM.union (KM.fromList
  [("schemaVersion",toJSON (1::Int))
  ,("appId",toJSON $ appId channel)
  ,("buildId",String buildId)
  ,("packageStatus",String $ if isJust provider then "provider-complete" else "validation-provider-incomplete")
  ,("updateDiscovery",Bool False)]) inputs

sealManifest :: Signing -> Maybe T.Text -> Manifest -> Manifest
sealManifest signing digest m = m
 { manifestSigning=Just signing, manifestProviderHash=digest
 , manifestWire=KM.insert "signing" (toJSON $ signingName signing) $
    maybe id (KM.insert "providerInventorySha256" . String) digest (manifestWire m) }
compatibilityManifest :: CompatibilityAudit -> Manifest -> Manifest
compatibilityManifest report m = m {manifestWire=KM.insert "macOSCompatibility" (toJSON report) (manifestWire m)}

data Artifact = Artifact FilePath Integer T.Text T.Text
data ArtifactPair = ArtifactPair { installerArtifact :: Artifact, portableArtifact :: Artifact }
data Receipt = Receipt PackageIdentity Signing ArtifactPair
instance ToJSON Receipt where
 toJSON (Receipt i signing artifacts) = obj
  [("schemaVersion",toJSON (1::Int))
  ,("appId",toJSON $ appId channel)
  ,("version",toJSON $ packageVersion i)
  ,("repository",String "PoulWalcker/sapiens4")
  ,("channel",toJSON $ channelName channel)
  ,("sourceCommit",toJSON $ packageCommit i)
  ,("sourceDirty",Bool $ packageDirty i)
  ,("packageStatus",String $ if packageComplete i then "provider-complete" else "validation-provider-incomplete")
  ,("artifacts",toJSON [item "installer" (installerArtifact artifacts), item "portable-zip" (portableArtifact artifacts)])]
  where
  channel = packageChannel i
  item kind (Artifact path size digest manifestHash) = obj
   [("platform",toJSON $ platformName $ packagePlatform i)
   ,("architecture",toJSON $ architecture $ packagePlatform i)
   ,("buildId",toJSON $ packageBuildId i)
   ,("minimumOS",if packagePlatform i==MacOS then String "13.0" else Null)
   ,("supportStatus",String "declared-target")
   ,("signing",toJSON $ signingName signing)
   ,("kind",String kind)
   ,("assetName",toJSON $ takeFileName path)
   ,("size",toJSON size)
   ,("sha256",String digest)
   ,("packageManifestSha256",String manifestHash)]

instance FromJSON Receipt where
 parseJSON = withObject "release receipt" $ \o -> do
  schema<-o .: "schemaVersion";check (schema==(1::Int)) "Unsupported receipt schema"
  let keys=["appId","version","repository","channel","sourceCommit","sourceDirty","packageStatus"]
  check (all (`KM.member` o) keys) "Missing receipt identity fields"
  values<-o .: "artifacts"
  parsed<-mapM (withObject "release artifact" $ \a -> do
   kind<-a .: "kind" :: Parser String
   -- Top-level identity fields cannot be overridden by artifact fields.
   let header=KM.filterWithKey (\key _ -> key `elem` keys) o
   identity<-parseJSON (Object $ KM.union header a)
   signing<-a .: "signing" >>= signingJSON
   either fail pure $ validateSigning (packagePlatform identity) signing
   path<-a .: "assetName" >>= relative;check (takeFileName path==path) "Artifact must be a basename"
   size<-a .: "size";check (size>=0) "Negative artifact size"
   digest<-a .: "sha256" >>= checksum 64
   manifestHash<-a .: "packageManifestSha256" >>= checksum 64
   pure (kind,(identity,signing,Artifact path size digest manifestHash))) values
  case parsed of
   [("installer",a),("portable-zip",b)] -> pair a b
   [("portable-zip",a),("installer",b)] -> pair b a
   _ -> fail "Exactly one installer and one portable ZIP required"
  where
  pair (i,s,a@(Artifact _ _ _ h)) (j,t,b@(Artifact _ _ _ k)) = do
   check (i==j && s==t && h==k) "Contradictory release artifact bindings"
   pure (Receipt i s $ ArtifactPair a b)

data CompatibilityRule = CompatibilityRule
 { ruleIdentity :: ProviderIdentity, ruleFingerprint :: T.Text, ruleRoot :: FilePath, ruleMaximum :: String, ruleId :: String, ruleSelected :: [String], ruleCommit :: T.Text }
instance FromJSON CompatibilityRule where
 parseJSON = withObject "optional voice rule" $ \o -> do
  schema<-o .: "schemaVersion";check (schema==(1::Int)) "Unsupported compatibility rule schema"
  identity<-o .: "providerIdentity"
  commit<-maybe (fail "Compatibility rule requires upstreamCommit") pure (identityCommit identity)
  CompatibilityRule identity <$> (o .: "providerFilesSha256" >>= checksum 64) <*> (o .: "optionalRoot" >>= relative) <*> (o .: "maximumMinimumOS" >>= \v -> either fail (const $ pure v) (osVersion v)) <*> o .: "ruleId" <*> o .: "selectedPaths" <*> pure commit
data BinaryAudit = BinaryAudit FilePath String [FilePath]
instance ToJSON BinaryAudit where toJSON (BinaryAudit path minimum dependencies) = obj [("path",toJSON path),("minimumOS",toJSON minimum),("dependencies",toJSON dependencies)]
data CompatibilityAudit = CompatibilityAudit String [BinaryAudit] (Maybe (CompatibilityRule,String,[BinaryAudit]))
instance ToJSON CompatibilityAudit where
 toJSON (CompatibilityAudit floor required optionalVoice) = obj [("declaredMinimumOS",String "13.0")
   ,("requiredMinimumOS",toJSON floor)
   ,("requiredBinaries",toJSON required)
   ,("optionalComponents",toJSON optionalRecords)
   ,("targetOSAcceptance",String "not-run")]
  where
  optionalRecords = maybe [] (\record -> [optionalRecord record]) optionalVoice
  optionalRecord (rule,minimum,entries) = obj
   [("name",String "codex-voice")
   ,("ruleId",toJSON $ ruleId rule)
   ,("minimumOS",toJSON minimum)
   ,("notUsedBySelectedPaths",toJSON $ ruleSelected rule)
   ,("binaries",toJSON entries)
   ,("evidenceCommit",toJSON $ ruleCommit rule)]

hex :: B.ByteString -> T.Text
hex = T.pack . concatMap (\byte -> let value=showHex byte "" in if length value==1 then '0':value else value) . B.unpack
hash :: L.ByteString -> T.Text
hash = hex . SHA.hashlazy

-- Stable key ordering; Aeson owns JSON escaping and scalar encoding.
canonical :: Value -> L.ByteString
canonical = E.encodingToLazyByteString . ordered
 where
 ordered (Object values) = E.pairs $ foldMap (\(key,value) -> E.pair key (ordered value)) (sortOn (K.toText . fst) $ KM.toList values)
 ordered (Array values) = E.list ordered (toList values)
 ordered value = toEncoding value
