{-# LANGUAGE OverloadedStrings #-}
module Format
 ( Inventory(..), InventoryEntry(..), ProviderIdentity(..), Provider, providerIdentity, providerEntries
 , ProviderLayout, layoutRelease, layoutPlatform, layoutEntry, layoutExec, layoutResources, layoutPath
 , ProviderLock(..), Download(..), lockAsset, newProvider, packagedProvider
 , Provenance(..), PackageIdentity(..), ProviderSummary, summaryVersion, Manifest, manifestIdentity, manifestProvider
 , manifestSigning, manifestProviderHash, newManifest, sealManifest, compatibilityManifest
 , ReceiptInput(..), ArtifactKind(..), Artifact(..), Receipt(..)
 , CompatibilityRule(..), BinaryAudit(..), CompatibilityAudit(..)
 , readJSON, writeJSON, printJSON, obj, canonical, hash, hex, safeRelativePath ) where

import Control.Monad
import Data.Aeson
import Data.Aeson.Types (Parser)
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Crypto.Hash.SHA256 as SHA
import Data.Char (isHexDigit, isLower, isDigit, ord)
import Data.Foldable (toList)
import Data.List (sortOn, nub, isPrefixOf)
import Data.Maybe (isJust, isNothing)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Numeric (showHex)
import System.FilePath
import Options hiding (required)

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
optional fields key = traverse parseJSON (KM.lookup key fields)
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
providerPlatformJSON = withText "provider platform" $ \value -> case value of
 "darwin-arm64" -> pure MacOS
 "windows-x64" -> pure Windows
 _ -> fail "Unsupported provider platform"
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
 let paths=map inventoryPath entries
 check (length paths==length (nub paths)) "Duplicate inventory paths"
 pure (sortOn inventoryPath entries)

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
  layout<-optional o "upstreamPackage"
  forM_ layout $ \l -> check (layoutRelease l==identityVersion identity && layoutPlatform l==identityPlatform identity && layoutEntry l==identityEntry identity) "Provider identity/layout mismatch"
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
 { packageVersion :: String, packagePlatform :: Platform, packageBuildId :: T.Text, packageCommit :: T.Text, packageDirty :: Bool, packageComplete :: Bool }
instance FromJSON PackageIdentity where
 parseJSON = withObject "package identity" $ \o -> do
  app<-o .: "appId";repository<-o .: "repository";channel<-o .: "channel";support<-o .: "supportStatus"
  check (app==("com.poulwalcker.sapiens4.development"::String) && repository==("PoulWalcker/sapiens4"::String) && channel==("development"::String) && support==("declared-target"::String)) "Unexpected package identity/support status"
  platform<-o .: "platform" >>= platformJSON;arch<-o .: "architecture";floor<-o .: "minimumOS"
  check (arch==architecture platform && floor==(if platform==MacOS then Just "13.0" else Nothing :: Maybe String)) "Platform/architecture/minimumOS mismatch"
  status<-o .: "packageStatus";complete<-case status :: String of "provider-complete"->pure True;"validation-provider-incomplete"->pure False;_->fail "Invalid package status"
  version<-o .: "version";check (validVersion version) "Invalid package SemVer"
  PackageIdentity version platform <$> (o .: "buildId" >>= checksum 64) <*> (o .: "sourceCommit" >>= checksum 40) <*> o .: "sourceDirty" <*> pure complete

data Manifest = Manifest { manifestIdentity :: PackageIdentity, manifestProvider :: Maybe ProviderSummary, manifestSigning :: Maybe Signing, manifestProviderHash :: Maybe T.Text, manifestWire :: Object }
instance FromJSON Manifest where
 parseJSON value = withObject "package manifest" (\o -> do
  schema<-o .: "schemaVersion";check (schema==(1::Int)) "Unsupported package manifest schema"
  identity<-parseJSON value;provider<-o .: "provider";check (packageComplete identity==maybe False (const True) provider) "Provider status mismatch"
  void (parseJSON value :: Parser Provenance);void (o .: "sourceTreeSha256" >>= checksum 64)
  void (o .: "toolchain" :: Parser (M.Map T.Text String))
  epochs<-o .: "dataEpoch";mapM_ (\key -> void (epochs .: key :: Parser (Maybe Integer))) ["minReadable","maxReadable","target"]
  void (o .: "migrationSetId" :: Parser (Maybe String));void (o .: "legacyAdoption" :: Parser String);void (o .: "updateDiscovery" :: Parser Bool)
  signing<-traverse signingJSON (KM.lookup "signing" o);digest<-optional o "providerInventorySha256" >>= traverse (checksum 64)
  check (isNothing signing || isNothing provider || isJust digest) "Signed provider inventory hash missing"
  pure (Manifest identity provider signing digest o)) value
instance ToJSON Manifest where toJSON = Object . manifestWire

-- Build identity uses precisely the legacy canonical JSON input document.
newManifest :: Provenance -> T.Text -> Platform -> String -> Maybe Provider -> [(T.Text,String)] -> Manifest
newManifest source tree platform version provider tools = Manifest identity summary Nothing Nothing document
 where
 summary = (\p -> let i=providerIdentity p in ProviderSummary (identityVersion i) ("codex/"++identityEntry i) (identityArchive i)) <$> provider
 inputs = KM.fromList
  [("sourceTreeSha256",String tree)
  ,("platform",toJSON $ platformName platform)
  ,("architecture",toJSON $ architecture platform)
  ,("version",toJSON version)
  ,("repository",String "PoulWalcker/sapiens4")
  ,("channel",String "development")
  ,("provider",toJSON summary)
  ,("minimumOS",if platform==MacOS then String "13.0" else Null)
  ,("supportStatus",String "declared-target")
  ,("toolchain",toJSON $ M.fromList tools)
  ,("sourceCommit",toJSON $ sourceCommit source)
  ,("sourceDirty",Bool $ sourceDirty source)
  ,("submodules",toJSON $ sourceSubmodules source)]
 buildId = hash $ canonical $ Object inputs
 identity = PackageIdentity version platform buildId (sourceCommit source) (sourceDirty source) (isJust provider)
 document = KM.union (KM.fromList
  [("schemaVersion",toJSON (1::Int))
  ,("appId",String "com.poulwalcker.sapiens4.development")
  ,("buildId",String buildId)
  ,("packageStatus",String $ if isJust provider then "provider-complete" else "validation-provider-incomplete")
  ,("dataEpoch",obj [("minReadable",Null),("maxReadable",Null),("target",Null)])
  ,("migrationSetId",Null)
  ,("legacyAdoption",String "not-implemented")
  ,("updateDiscovery",Bool False)]) inputs

sealManifest :: Signing -> Maybe T.Text -> Manifest -> Manifest
sealManifest signing digest m = m
 { manifestSigning=Just signing, manifestProviderHash=digest
 , manifestWire=KM.insert "signing" (toJSON $ signingName signing) $
    maybe id (KM.insert "providerInventorySha256" . String) digest (manifestWire m) }
compatibilityManifest :: CompatibilityAudit -> Manifest -> Manifest
compatibilityManifest report m = m {manifestWire=KM.insert "macOSCompatibility" (toJSON report) (manifestWire m)}

-- Receipts consume only the identity/signing portion, including older fixtures
-- that do not contain a complete staging manifest.
data ReceiptInput = ReceiptInput PackageIdentity Signing
instance FromJSON ReceiptInput where
 parseJSON value = withObject "receipt source" (\o -> ReceiptInput <$> parseJSON value <*> (o .: "signing" >>= signingJSON)) value

data ArtifactKind = Installer | PortableZip
data Artifact = Artifact ArtifactKind FilePath Integer T.Text T.Text
data Receipt = Receipt PackageIdentity Signing [Artifact]
instance ToJSON Receipt where
 toJSON (Receipt i signing artifacts) = obj
  [("schemaVersion",toJSON (1::Int))
  ,("appId",String "com.poulwalcker.sapiens4.development")
  ,("version",toJSON $ packageVersion i)
  ,("repository",String "PoulWalcker/sapiens4")
  ,("channel",String "development")
  ,("sourceCommit",toJSON $ packageCommit i)
  ,("sourceDirty",Bool $ packageDirty i)
  ,("packageStatus",String $ if packageComplete i then "provider-complete" else "validation-provider-incomplete")
  ,("artifacts",toJSON $ map item artifacts)]
  where
  item (Artifact kind path size digest manifestHash) = obj
   [("platform",toJSON $ platformName $ packagePlatform i)
   ,("architecture",toJSON $ architecture $ packagePlatform i)
   ,("buildId",toJSON $ packageBuildId i)
   ,("minimumOS",if packagePlatform i==MacOS then String "13.0" else Null)
   ,("supportStatus",String "declared-target")
   ,("signing",toJSON $ signingName signing)
   ,("kind",String $ case kind of Installer->"installer";PortableZip->"portable-zip")
   ,("assetName",toJSON $ takeFileName path)
   ,("size",toJSON size)
   ,("sha256",String digest)
   ,("packageManifestSha256",String manifestHash)]

data CompatibilityRule = CompatibilityRule
 { ruleIdentity :: ProviderIdentity, ruleFingerprint :: T.Text, ruleRoot :: FilePath, ruleMaximum :: String, ruleId :: String, ruleSelected :: [String], ruleCommit :: T.Text }
instance FromJSON CompatibilityRule where
 parseJSON = withObject "optional voice rule" $ \o -> do
  schema<-o .: "schemaVersion";check (schema==(1::Int)) "Unsupported compatibility rule schema"
  identity<-o .: "providerIdentity";raw<-o .: "providerIdentity"
  CompatibilityRule identity <$> (o .: "providerFilesSha256" >>= checksum 64) <*> (o .: "optionalRoot" >>= relative) <*> (o .: "maximumMinimumOS" >>= \v -> either fail (const $ pure v) (osVersion v)) <*> o .: "ruleId" <*> o .: "selectedPaths" <*> (raw .: "upstreamCommit" >>= checksum 40)
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

-- Python's existing sort_keys/separators/ensure_ascii encoding is part of the
-- provider fingerprint and buildId contract, not the pretty-printing format.
canonical :: Value -> L.ByteString
canonical input = L.fromStrict $ TE.encodeUtf8 $ render input
 where
 render (Object values) = "{"<>T.intercalate "," [quoted (K.toText key)<>":"<>render item | (key,item)<-sortOn (K.toText.fst) (KM.toList values)]<>"}"
 render (Array values) = "["<>T.intercalate "," (map render $ toList values)<>"]"
 render (String value) = quoted value
 render other = TE.decodeUtf8 (L.toStrict $ encode other)
 quoted value = "\""<>T.concatMap escape value<>"\""
 escape '"' = "\\\""
 escape '\\' = "\\\\"
 escape '\b' = "\\b"
 escape '\f' = "\\f"
 escape '\n' = "\\n"
 escape '\r' = "\\r"
 escape '\t' = "\\t"
 escape character
  | code < 32 || code > 126 = if code <= 65535 then unicode code else unicode (55296 + (code-65536) `div` 1024) <> unicode (56320 + (code-65536) `mod` 1024)
  | otherwise = T.singleton character
  where code=ord character
 unicode code = let value=showHex code "" in T.pack ("\\u"++replicate (4-length value) '0'++value)

