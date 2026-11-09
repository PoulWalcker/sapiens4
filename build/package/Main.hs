{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Exception (bracket_)
import Control.Monad
import Data.Aeson
import qualified Data.ByteString as B
import Data.List (sortOn, isPrefixOf)
import Data.Maybe
import qualified Data.Set as Set
import qualified Data.Text as T
import System.Directory
import System.Environment
import System.FilePath
import System.IO
import Options
import qualified Package as P

main :: IO ()
main = do
 root <- getCurrentDirectory
 args <- getArgs
 case args of
  "macos":rest -> parseMac rest >>= \options -> when (buildMode options /= BuildOnly) (packageMac root options)
  _ -> P.command root args

macho :: FilePath -> IO Bool
macho path = do
 present <- doesFileExist path
 link <- pathIsSymbolicLink path
 if not present || link then pure False else withBinaryFile path ReadMode $ \handle -> do
  magic <- B.hGet handle 4
  pure $ magic `elem` map B.pack [[254,237,250,206],[206,250,237,254],[254,237,250,207],[207,250,237,254],[202,254,186,190],[190,186,254,202],[202,254,186,191],[191,186,254,202]]

binaries :: FilePath -> IO [FilePath]
binaries root = P.files root >>= filterM macho

dependencies :: FilePath -> IO [String]
dependencies binary = map (T.unpack . fst . T.breakOn " (" . T.strip . T.pack) . drop 1 . lines <$> P.capture "otool" ["-L",binary]

loadFields :: String -> String -> String -> [String]
loadFields command name input = go "" (map (T.strip . T.pack) $ lines input)
 where
 go _ [] = []
 go current (line:rest)
  | Just value <- T.stripPrefix "cmd " line = go (T.unpack value) rest
  | current==command, Just value <- T.stripPrefix (T.pack name<>" ") line = T.unpack (T.strip $ fst $ T.breakOn " (offset" value):go current rest
  | otherwise = go current rest

expand :: FilePath -> FilePath -> String -> FilePath
expand app binary = T.unpack . T.replace "@executable_path" (T.pack $ app</>"Contents/MacOS") . T.replace "@loader_path" (T.pack $ takeDirectory binary) . T.pack

systemDependency :: FilePath -> Bool
systemDependency path = any (`isPrefixOf` path) ["/usr/lib/","/System/Library/"]

relativeBetween :: FilePath -> FilePath -> FilePath
relativeBetween source destination = dropCommon (splitDirectories $ normalise source) (splitDirectories $ normalise destination)
 where
 dropCommon (a:as) (b:bs) | a==b = dropCommon as bs
 dropCommon left right = joinPath (replicate (length left) ".."++right)

firstFile :: [FilePath] -> IO (Maybe FilePath)
firstFile [] = pure Nothing
firstFile (path:rest) = doesFileExist path >>= \present -> if present then pure (Just path) else firstFile rest

relocate :: FilePath -> IO ()
relocate input = do
 app <- canonicalizePath input
 binaries app >>= loop app Set.empty
 where
 loop _ _ [] = pure ()
 loop app seen (binary:pending)
  | Set.member binary seen = loop app seen pending
  | otherwise = do
   let frameworks=app</>"Contents/Frameworks"
   edges <- dependencies binary
   added <- fmap concat $ forM edges $ \dependency -> if systemDependency dependency || takeFileName dependency==takeFileName binary then pure [] else do
    let name=takeFileName dependency
    commands <- P.capture "otool" ["-l",binary]
    let candidates=case strip "@rpath/" dependency of
         Just suffix -> [expand app binary path</>suffix | path<-loadFields "LC_RPATH" "path" commands]
         Nothing -> [expand app binary dependency]
    resolved <- firstFile candidates
    internal <- maybe (pure False) (fmap (P.inside app) . canonicalizePath) resolved
    if "@rpath/" `isPrefixOf` dependency && internal then pure [] else
     if "@loader_path/" `isPrefixOf` dependency && isJust resolved then do
      unless internal $ fail ("Dependency escapes application: "++dependency)
      pure []
     else do
      source <- maybe (fail $ "Unresolved non-system dependency "++dependency++" in "++binary) pure resolved
      doesFileExist source >>= flip unless (fail $ "Unresolved non-system dependency "++dependency++" in "++binary)
      createDirectoryIfMissing True frameworks
      let target=frameworks</>name
      present <- doesFileExist target
      unless present $ copyFile source target
      P.run "install_name_tool" ["-change",dependency,"@loader_path/"++relativeBetween (takeDirectory binary) target,binary]
      pure [target | not present]
   when (takeDirectory binary==frameworks) $ P.run "install_name_tool" ["-id","@rpath/"++takeFileName binary,binary]
   loop app (Set.insert binary seen) (added++pending)
 strip prefix value = T.unpack <$> T.stripPrefix (T.pack prefix) (T.pack value)

-- An optional component always carries the exact audited rule that permits it.
data VoicePolicy = RequiredOnly | OptionalVoice P.CompatibilityRule (Set.Set FilePath)

optionalVoice :: FilePath -> FilePath -> IO VoicePolicy
optionalVoice root app = do
 let provider=app</>"Contents/Resources/runtime/codex";metadata=provider</>"sapiens-provider.json"
 present <- doesFileExist metadata
 if not present then pure RequiredOnly else do
  rule <- P.readJSON (root</>"build/package/codex-macos-compatibility.json")
  info <- P.readJSON metadata
  if P.providerIdentity info /= P.ruleIdentity rule then pure RequiredOnly else do
   observed <- P.inventory provider ["sapiens-provider.json"]
   unless (P.hash (P.canonical $ toJSON observed)==P.ruleFingerprint rule) $ fail "Provider bytes differ from the exact audited optional-voice release"
   paths <- binaries (provider</>P.ruleRoot rule) >>= mapM canonicalizePath
   pure (OptionalVoice rule $ Set.fromList paths)

maximumVersion :: [String] -> Either String String
maximumVersion [] = Right "13.0"
maximumVersion (first:rest) = do
 initial <- (,) <$> osVersion first <*> pure first
 versions <- traverse (\value -> (,) <$> osVersion value <*> pure value) rest
 pure $ snd $ foldr (\a b -> if fst a>fst b then a else b) initial versions

audit :: FilePath -> VoicePolicy -> IO P.CompatibilityAudit
audit input policy = do
 app <- canonicalizePath input
 paths <- binaries app >>= mapM canonicalizePath
 records <- forM paths $ \binary -> do
  architectures <- words <$> P.capture "lipo" ["-archs",binary]
  unless (architectures==["arm64"]) $ fail ("Expected an audited arm64-only payload: "++binary)
  commands <- P.capture "otool" ["-l",binary]
  unless (all (=="1") $ loadFields "LC_BUILD_VERSION" "platform" commands) $ fail ("Non-macOS Mach-O platform in "++binary)
  let minimums=loadFields "LC_BUILD_VERSION" "minos" commands++loadFields "LC_VERSION_MIN_MACOSX" "version" commands
  when (null minimums) $ fail ("No macOS minimum found for "++binary)
  minimum <- either fail pure $ maximumVersion minimums
  let voice=isVoice binary
      allowed=case policy of OptionalVoice rule _ | voice -> P.ruleMaximum rule;_ -> "13.0"
  minimumVersion <- either fail pure $ osVersion minimum
  allowedVersion <- either fail pure $ osVersion allowed
  when (minimumVersion>allowedVersion) $ fail (P.portable (makeRelative app binary)++" requires macOS "++minimum++"; allowed floor is "++allowed)
  edges <- dependencies binary
  let identities=loadFields "LC_ID_DYLIB" "name" commands;rpaths=loadFields "LC_RPATH" "path" commands
  resolved <- fmap catMaybes $ forM edges $ \dependency -> if dependency `elem` identities || systemDependency dependency then pure Nothing else do
   let candidates=case T.stripPrefix "@rpath/" (T.pack dependency) of
        Just suffix -> [expand app binary path</>T.unpack suffix | path<-rpaths]
        Nothing -> [expand app binary dependency]
   target <- firstFile candidates >>= maybe (fail $ "Unresolved or external packaged dependency "++dependency++" in "++binary) canonicalizePath
   unless (P.inside app target) $ fail ("Unresolved or external packaged dependency "++dependency++" in "++binary)
   macho target >>= flip unless (fail $ "Unrecognized packaged Mach-O dependency: "++target)
   unless (voice==isVoice target) $ fail ("Dependency crosses audited optional voice boundary: "++binary++" -> "++target)
   pure (Just $ P.portable $ makeRelative app target)
  pure (voice,minimum,P.BinaryAudit (P.portable $ makeRelative app binary) minimum (sortOn id resolved))
 let required=[value | (False,_,value)<-records];voices=[value | (True,_,value)<-records]
 optionalReport <- case policy of
  OptionalVoice rule _ | not (null voices) -> do
   minimum <- either fail pure $ maximumVersion [v | (True,v,_)<-records]
   pure (Just (rule,minimum,voices))
  _ -> pure Nothing
 floor <- either fail pure $ maximumVersion ("13.0":[v | (False,v,_)<-records])
 pure $ P.CompatibilityAudit floor required optionalReport
 where
 isVoice path = case policy of RequiredOnly->False;OptionalVoice _ paths->Set.member path paths

sign :: FilePath -> MacSigning -> IO ()
sign app plan = do
 let identity=signIdentity plan
     flags=["--force","--sign",identity]++[value | signingMode plan==DeveloperID,value<-["--options","runtime","--timestamp"]]
 binaries app >>= mapM_ (\binary->P.run "codesign" (flags++[binary])) . sortOn (negate . length . splitDirectories)
 P.finalize (app</>"Contents/Resources/runtime") (signingMode plan)
 P.run "codesign" (flags++[app])
 P.run "codesign" ["--verify","--deep","--strict",app]

diskImage :: FilePath -> FilePath -> FilePath -> MacSigning -> IO FilePath
diskImage app output temporary plan = do
 let image=output</>"Sapiens.dmg";source=temporary</>"disk-image"
 P.exists image >>= flip when (fail "Sapiens.dmg already exists; choose a new output directory")
 createDirectory source
 P.run "ditto" [app,source</>takeFileName app]
 createDirectoryLink "/Applications" (source</>"Applications")
 P.run "hdiutil" ["create","-volname","Sapiens4 Development","-srcfolder",source,"-format","UDZO",image]
 when (signingMode plan==DeveloperID) $ do
  P.run "codesign" ["--force","--sign",signIdentity plan,"--timestamp",image]
  P.run "codesign" ["--verify","--strict",image]
 forM_ (notaryProfile plan) $ \profile -> do
  P.run "xcrun" ["notarytool","submit",image,"--keychain-profile",profile,"--wait"]
  P.run "xcrun" ["stapler","staple",image]
  P.run "xcrun" ["stapler","validate",image]
  P.run "spctl" ["--assess","--type","open","--context","context:primary-signature",image]
 P.run "hdiutil" ["verify",image]
 let mount=temporary</>"mounted"
 createDirectory mount
 bracket_ (P.run "hdiutil" ["attach","-readonly","-nobrowse","-mountpoint",mount,image]) (P.run "hdiutil" ["detach",mount]) $ do
  target<-getSymbolicLinkTarget (mount</>"Applications")
  unless (target=="/Applications") $ fail "DMG Applications link is incorrect"
  let installed=temporary</>"installed copy"</>takeFileName app
  P.run "ditto" [mount</>takeFileName app,installed]
  P.run "codesign" ["--verify","--deep","--strict",installed]
  P.verify (installed</>"Contents/Resources/runtime")
 pure image

packageMac :: FilePath -> MacOptions -> IO ()
packageMac root options = do
 output <- canonicalizePath $ fromMaybe (root</>".build/macos") (outputDirectory options)
 createDirectoryIfMissing True output
 let appName="Sapiens4 Development.app";destination=output</>appName;artifact=output</>"sapiens4-development-"++versionText (version options)++"-macos-arm64.zip"
 P.exists destination >>= flip when (fail "Output app already exists; choose a new output directory")
 P.exists artifact >>= flip when (fail "Versioned artifact already exists")
 P.exists (output</>"Sapiens.dmg") >>= flip when (fail "Sapiens.dmg already exists; choose a new output directory")
 P.withTemporary output "sapiens-package" $ \temporary -> do
  let app=temporary</>appName;resources=app</>"Contents/Resources";macos=app</>"Contents/MacOS";runtime=resources</>"runtime"
  createDirectoryIfMissing True resources
  createDirectory macos
  copyFile (root</>".build/bin/sapiens-macos") (macos</>"Sapiens4")
  manifest <- P.stage root runtime (version options) MacOS (providerDirectory options) (allowMissingProvider options)
  let iconset=temporary</>"AppIcon.iconset"
  createDirectory iconset
  P.run (root</>".build/bin/sapiens-macos") ["--icon",iconset,root</>"app/shell/favicon.svg"]
  P.run "iconutil" ["-c","icns",iconset,"-o",resources</>"AppIcon.icns"]
  let bundleVersion=takeWhile (`notElem` ['-','+']) (versionText $ version options)
      info=P.obj [("CFBundleIdentifier",String "com.poulwalcker.sapiens4.development"),("CFBundleName",String "Sapiens4 Development"),("CFBundleDisplayName",String "Sapiens4 Development"),("CFBundleExecutable",String "Sapiens4"),("CFBundlePackageType",String "APPL"),("CFBundleVersion",toJSON bundleVersion),("CFBundleShortVersionString",toJSON bundleVersion),("CFBundleIconFile",String "AppIcon"),("NSHighResolutionCapable",Bool True),("LSMinimumSystemVersion",String "13.0"),("NSAppTransportSecurity",P.obj [("NSAllowsLocalNetworking",Bool True),("NSAllowsArbitraryLoadsInWebContent",Bool True)])]
  let infoJSON=temporary</>"Info.json"
  P.writeJSON infoJSON info
  P.run "plutil" ["-convert","xml1","-o",app</>"Contents/Info.plist",infoJSON]
  voicePolicy<-optionalVoice root app
  relocate app
  compatibility<-audit app voicePolicy
  let manifestPath=runtime</>"package-manifest.json"
  P.writeJSON manifestPath (P.compatibilityManifest compatibility manifest)
  sign app (macSigning options)
  forM_ (notaryProfile (macSigning options)) $ \profile -> do
   let submitted=temporary</>"notarization.zip"
   P.run "ditto" ["-c","-k","--keepParent",app,submitted]
   P.run "xcrun" ["notarytool","submit",submitted,"--keychain-profile",profile,"--wait"]
   P.run "xcrun" ["stapler","staple",app]
   P.run "xcrun" ["stapler","validate",app]
   P.run "spctl" ["--assess","--type","execute","--verbose=2",app]
  renameDirectory app destination
  P.run "ditto" ["-c","-k","--keepParent",destination,artifact]
  image<-diskImage destination output temporary (macSigning options)
  P.receipt artifact image (destination</>"Contents/Resources/runtime/package-manifest.json") (replaceExtension artifact ".release.json")
  putStrLn ("Built "++destination++"\nInstaller "++image++"\nPortable "++artifact)
