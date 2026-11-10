module Options
 ( Channel(..), channelName, parseChannel, releaseRepository, appId, displayName, artifactPrefix
 , Platform(..), BuildMode(..), Signing(..), MacSigning(..), MacOptions(..)
 , platformName, providerPlatform, architecture, executableSuffix, parsePlatform, parseProviderPlatform
 , signingName, parseSigning, validateSigning, signIdentity, notaryProfile, signingMode
 , parseMac, osVersion, Version, parseVersion, versionText, macUsage, option, required ) where

import Control.Monad
import Data.Char (isAscii, isAlphaNum)
import System.Environment
import Text.Read (readMaybe)

releaseRepository :: String
releaseRepository = "aleski-green/sapiens4"

-- Channel binds package identity and prevents mixing stable and development receipts.
data Channel = Development | Stable deriving (Eq, Show)
channelName :: Channel -> String
channelName Development = "development"
channelName Stable = "stable"
parseChannel :: String -> Either String Channel
parseChannel "development" = Right Development
parseChannel "stable" = Right Stable
parseChannel _ = Left "Expected development or stable channel"
appId :: Channel -> String
appId Development = "com.poulwalcker.sapiens4.development"
appId Stable = "com.poulwalcker.sapiens4"
displayName :: Channel -> String
displayName Development = "Sapiens4 Development"
displayName Stable = "Sapiens4"
artifactPrefix :: Channel -> String
artifactPrefix Development = "sapiens4-development-"
artifactPrefix Stable = "sapiens4-"

data Platform = MacOS | Windows deriving (Eq, Show)
data BuildMode = BuildAndPackage | BuildOnly | PackageOnly deriving (Eq, Show)
data Signing = Unsigned | AdHoc | DeveloperID | Authenticode deriving (Eq, Show)
data MacSigning = MacAdHoc | MacDeveloperID String (Maybe String) deriving Show

platformName :: Platform -> String
platformName MacOS = "macos"
platformName Windows = "windows"
providerPlatform :: Platform -> String
providerPlatform MacOS = "darwin-arm64"
providerPlatform Windows = "windows-x64"
architecture :: Platform -> String
architecture MacOS = "arm64"
architecture Windows = "x64"
executableSuffix :: Platform -> String
executableSuffix MacOS = ""
executableSuffix Windows = ".exe"
parsePlatform :: String -> Either String Platform
parsePlatform "macos" = Right MacOS
parsePlatform "windows" = Right Windows
parsePlatform _ = Left "Supported primary targets: macOS arm64 and Windows x64"
parseProviderPlatform :: String -> Either String Platform
parseProviderPlatform "darwin-arm64" = Right MacOS
parseProviderPlatform "windows-x64" = Right Windows
parseProviderPlatform _ = Left "Unsupported provider platform"
signingName :: Signing -> String
signingName Unsigned = "none"
signingName AdHoc = "ad-hoc"
signingName DeveloperID = "developer-id"
signingName Authenticode = "authenticode"
parseSigning :: String -> Either String Signing
parseSigning name = case name of
 "none" -> Right Unsigned
 "ad-hoc" -> Right AdHoc
 "developer-id" -> Right DeveloperID
 "authenticode" -> Right Authenticode
 _ -> Left "Unsupported signing mode"

validateSigning :: Platform -> Signing -> Either String ()
validateSigning platform signing = case (platform,signing) of
 (_,Unsigned) -> Right ()
 (MacOS,AdHoc) -> Right ()
 (MacOS,DeveloperID) -> Right ()
 (Windows,Authenticode) -> Right ()
 _ -> Left (signingName signing++" signing is not supported for "++platformName platform)

-- Publishing credentials cannot be attached to an ad-hoc signing plan.
data MacOptions = MacOptions
 { channel :: Channel, version :: Version, outputDirectory :: Maybe FilePath, providerDirectory :: Maybe FilePath
 , buildMode :: BuildMode, allowMissingProvider :: Bool, macSigning :: MacSigning } deriving Show

signIdentity :: MacSigning -> String
signIdentity MacAdHoc = "-"
signIdentity (MacDeveloperID identity _) = identity
notaryProfile :: MacSigning -> Maybe String
notaryProfile MacAdHoc = Nothing
notaryProfile (MacDeveloperID _ profile) = profile
signingMode :: MacSigning -> Signing
signingMode MacAdHoc = AdHoc
signingMode (MacDeveloperID _ _) = DeveloperID

parseMac :: [String] -> IO MacOptions
parseMac args = do
 identity <- maybe "-" id <$> lookupEnv "SAPIENS_MAC_SIGN_IDENTITY"
 notary <- lookupEnv "SAPIENS_MAC_NOTARY_PROFILE"
 either fail pure $ parse (MacOptions Development (Version "0.1.0-dev") Nothing Nothing BuildAndPackage False MacAdHoc) identity (nonempty =<< notary) args
 where
 parse value identity notary [] = do
  plan <- if buildMode value==BuildOnly then Right MacAdHoc else case (identity,notary) of
   ("-",Nothing) -> Right MacAdHoc
   ("-",Just _) -> Left "Notarization requires Developer ID signing"
   ("",_) -> Left "Signing identity must not be empty"
   _ -> Right (MacDeveloperID identity notary)
  Right value {macSigning=plan}
 parse value identity notary ("--build-only":rest) = mode BuildOnly value >>= \next -> parse next identity notary rest
 parse value identity notary ("--skip-build":rest) = mode PackageOnly value >>= \next -> parse next identity notary rest
 parse value identity notary ("--allow-missing-provider":rest) = parse value {allowMissingProvider=True} identity notary rest
 parse value identity notary (key:argument:rest) = case key of
  "--channel" -> parseChannel argument >>= \c -> parse value {channel=c} identity notary rest
  "--version" -> parseVersion argument >>= \v -> parse value {version=v} identity notary rest
  "--output-directory" -> parse value {outputDirectory=Just argument} identity notary rest
  "--provider" -> parse value {providerDirectory=Just argument} identity notary rest
  "--sign-identity" -> parse value argument notary rest
  "--notary-profile" -> parse value identity (nonempty argument) rest
  _ -> Left ("Unknown option: "++key)
 parse _ _ _ rest = Left ("Missing or unknown option: "++unwords rest)
 mode next value
  | buildMode value `elem` [BuildAndPackage,next] = Right value {buildMode=next}
  | otherwise = Left "Choose --build-only or --skip-build"
 nonempty "" = Nothing
 nonempty value = Just value

osVersion :: String -> Either String [Integer]
osVersion value = do
 numbers <- traverse number (split value)
 unless (not (null numbers) && length numbers<=3) $ Left ("Malformed macOS version: "++value)
 pure $ take 3 (numbers++repeat 0)
 where
 number digits = case readMaybe digits of
  Just n | not (null digits) && all (`elem` ['0'..'9']) digits -> Right n
  _ -> Left ("Malformed macOS version: "++value)
 split input = case break (=='.') input of (a,[])->[a];(a,_:rest)->a:split rest

-- SemVer 2.0: ASCII identifiers; leading zeroes are forbidden only in numeric
-- core/prerelease identifiers, not build metadata.
newtype Version = Version String deriving (Eq, Show)

versionText :: Version -> String
versionText (Version value) = value

parseVersion :: String -> Either String Version
parseVersion input = if valid then Right (Version input) else Left "Version must be SemVer"
 where
 valid = case split '+' input of
  [core] -> release core
  [core,metadata] -> release core && identifiers (const True) metadata
  _ -> False
 release value = case break (=='-') value of
  (core,[]) -> numbers core
  (core,_:pre) -> numbers core && identifiers (\v -> not (all digit v) || number v) pre
 numbers value = case split '.' value of [a,b,c] -> all number [a,b,c]; _ -> False
 number "0" = True
 number (c:cs) = c>='1' && c<='9' && all digit cs
 number [] = False
 digit c = c>='0' && c<='9'
 identifiers extra = all (\v -> not (null v) && all (\c -> isAscii c && (isAlphaNum c || c=='-')) v && extra v) . split '.'
 split separator value = case break (==separator) value of (a,[]) -> [a]; (a,_:rest) -> a:split separator rest

macUsage :: String
macUsage = "Usage: environments/macos/build.sh [--channel development|stable] [--version SEMVER] [--output-directory DIR] [--provider DIR] [--build-only | --skip-build] [--allow-missing-provider] [--sign-identity ID] [--notary-profile PROFILE]"

option :: String -> [String] -> Maybe String
option name values = foldl select Nothing (zip values $ drop 1 values)
 where select found (key,value) = if key=="--"++name then Just value else found

required :: String -> [String] -> IO String
required name values = maybe (fail $ "Missing --"++name) pure (option name values)
