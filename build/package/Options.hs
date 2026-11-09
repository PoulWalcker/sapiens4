module Options where

import Control.Monad
import Data.Char (isDigit, isAscii, isAlphaNum)
import System.Environment

-- Shared by the base-only launcher and the compiled packager.
data MacOptions = MacOptions
 { version :: String, outputDirectory :: Maybe FilePath, providerDirectory :: Maybe FilePath
 , buildOnly :: Bool, skipBuild :: Bool, allowMissingProvider :: Bool
 , signIdentity :: String, notaryProfile :: Maybe String } deriving Show

parseMac :: [String] -> IO MacOptions
parseMac args = do
 identity <- maybe "-" id <$> lookupEnv "SAPIENS_MAC_SIGN_IDENTITY"
 notary <- lookupEnv "SAPIENS_MAC_NOTARY_PROFILE"
 result <- parse (MacOptions "0.1.0-dev" Nothing Nothing False False False identity (nonempty =<< notary)) args
 when (buildOnly result && skipBuild result) $ fail "Choose --build-only or --skip-build"
 unless (validVersion $ version result) $ fail "--version must be SemVer"
 when (not (buildOnly result) && notaryProfile result /= Nothing && signIdentity result == "-") $
  fail "Notarization requires Developer ID signing"
 pure result
 where
 parse value [] = pure value
 parse value ("--build-only":rest) = parse value {buildOnly=True} rest
 parse value ("--skip-build":rest) = parse value {skipBuild=True} rest
 parse value ("--allow-missing-provider":rest) = parse value {allowMissingProvider=True} rest
 parse value (key:argument:rest) = case key of
  "--version" -> parse value {version=argument} rest
  "--output-directory" -> parse value {outputDirectory=Just argument} rest
  "--provider" -> parse value {providerDirectory=Just argument} rest
  "--sign-identity" -> parse value {signIdentity=argument} rest
  "--notary-profile" -> parse value {notaryProfile=nonempty argument} rest
  _ -> fail ("Unknown option: "++key)
 parse _ rest = fail ("Missing or unknown option: "++unwords rest)
 nonempty "" = Nothing
 nonempty value = Just value

-- Same accepted version form as the existing packaging CLI.
validVersion :: String -> Bool
validVersion input = case split '+' input of
 [core] -> release core
 [core,metadata] -> release core && identifiers metadata
 _ -> False
 where
 release value = case break (=='-') value of
  (core,[]) -> numbers core
  (core,_:pre) -> numbers core && identifiers pre
 numbers value = case split '.' value of
  [a,b,c] -> all (\v -> not (null v) && all isDigit v) [a,b,c]
  _ -> False
 identifiers value = not (null value) && all (\c -> isAscii c && (isAlphaNum c || c `elem` ".-")) value
 split separator value = case break (==separator) value of
  (a,[]) -> [a]
  (a,_:rest) -> a:split separator rest

macUsage :: String
macUsage = "Usage: environments/macos/build.sh [--version SEMVER] [--output-directory DIR] [--provider DIR] [--build-only | --skip-build] [--allow-missing-provider] [--sign-identity ID] [--notary-profile PROFILE]"

option :: String -> [String] -> Maybe String
option name values = case [value | (key,value) <- zip values (drop 1 values), key == "--"++name] of
 [] -> Nothing
 found -> Just (last found)

required :: String -> [String] -> IO String
required name values = maybe (fail $ "Missing --"++name) pure (option name values)
