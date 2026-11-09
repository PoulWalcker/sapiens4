{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Exception
import Control.Monad
import Data.Aeson
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
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
  "macos":rest -> parseMac rest >>= \options -> unless (buildOnly options) (packageMac root options)
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

relocate :: FilePath -> FilePath -> IO ()
relocate input root = do
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
      source <- maybe (pure $ root</>"submodules/libs4/haskell/.runtime"</>name) pure resolved
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

optionalVoice :: FilePath -> FilePath -> IO (Set.Set FilePath,Maybe Value)
optionalVoice root app = do
 let provider=app</>"Contents/Resources/runtime/codex";metadata=provider</>"sapiens-provider.json"
 present <- doesFileExist metadata
 if not present then pure (Set.empty,Nothing) else do
  rule <- P.readJSON (root</>"build/package/codex-macos-compatibility.json")
  info <- P.readJSON metadata
  let identity=P.field "providerIdentity" rule
  keys <- case identity of Object values -> pure (map K.toText $ KM.keys values);_->fail "Invalid optional voice rule"
  if any (\key->P.field key info/=P.field key identity) keys then pure (Set.empty,Nothing) else do
   observed <- P.inventory provider ["sapiens-provider.json"]
   unless (String (P.hash $ P.canonical observed)==P.field "providerFilesSha256" rule) $ fail "Provider bytes differ from the exact audited optional-voice release"
   subtree <- P.get "optionalRoot" rule
   paths <- binaries (provider</>subtree) >>= mapM canonicalizePath
   pure (Set.fromList paths,Just rule)
osVersion :: String -> [Int]
osVersion value = take 3 (numbers value++repeat 0)
 where numbers input = case break (=='.') input of (a,[])->[read a];(a,_:rest)->read a:numbers rest

maximumVersion :: [String] -> String
maximumVersion [] = "13.0"
maximumVersion (first:rest) = foldr (\a b->if osVersion a>osVersion b then a else b) first rest

audit :: FilePath -> Set.Set FilePath -> Maybe Value -> IO Value
audit input optional rule = do
 app <- canonicalizePath input
 paths <- binaries app >>= mapM canonicalizePath
 records <- forM paths $ \binary -> do
  architectures <- words <$> P.capture "lipo" ["-archs",binary]
  unless (architectures==["arm64"]) $ fail ("Expected an audited arm64-only payload: "++binary)
  commands <- P.capture "otool" ["-l",binary]
  unless (all (=="1") $ loadFields "LC_BUILD_VERSION" "platform" commands) $ fail ("Non-macOS Mach-O platform in "++binary)
  let minimums=loadFields "LC_BUILD_VERSION" "minos" commands++loadFields "LC_VERSION_MIN_MACOSX" "version" commands
  when (null minimums) $ fail ("No macOS minimum found for "++binary)
  let minimum=maximumVersion minimums
      voice=Set.member binary optional
  allowed <- if voice then maybe (fail "Missing optional voice rule") (P.get "maximumMinimumOS") rule else pure "13.0"
  when (osVersion minimum>osVersion allowed) $ fail (P.portable (makeRelative app binary)++" requires macOS "++minimum++"; allowed floor is "++allowed)
  edges <- dependencies binary
  let identities=loadFields "LC_ID_DYLIB" "name" commands;rpaths=loadFields "LC_RPATH" "path" commands
  resolved <- fmap catMaybes $ forM edges $ \dependency -> if dependency `elem` identities || systemDependency dependency then pure Nothing else do
   let candidates=case T.stripPrefix "@rpath/" (T.pack dependency) of
        Just suffix -> [expand app binary path</>T.unpack suffix | path<-rpaths]
        Nothing -> [expand app binary dependency]
   target <- firstFile candidates >>= maybe (fail $ "Unresolved or external packaged dependency "++dependency++" in "++binary) canonicalizePath
   unless (P.inside app target) $ fail ("Unresolved or external packaged dependency "++dependency++" in "++binary)
   macho target >>= flip unless (fail $ "Unrecognized packaged Mach-O dependency: "++target)
   unless (voice==Set.member target optional) $ fail ("Dependency crosses audited optional voice boundary: "++binary++" -> "++target)
   pure (Just $ P.portable $ makeRelative app target)
  pure (voice,minimum,P.obj [("path",toJSON $ P.portable $ makeRelative app binary),("minimumOS",toJSON minimum),("dependencies",toJSON $ sortOn id resolved)])
 let required=[value | (False,_,value)<-records];voices=[value | (True,_,value)<-records]
 optionalRecords <- if null voices then pure [] else do
  policy<-maybe (fail "Missing optional voice rule") pure rule
  pure [P.obj [("name",String "codex-voice"),("ruleId",P.field "ruleId" policy),("minimumOS",toJSON $ maximumVersion [v | (True,v,_)<-records]),("notUsedBySelectedPaths",P.field "selectedPaths" policy),("binaries",toJSON voices),("evidenceCommit",P.field "upstreamCommit" $ P.field "providerIdentity" policy)]]
 pure $ P.obj [("declaredMinimumOS",String "13.0"),("requiredMinimumOS",toJSON $ maximumVersion ("13.0":[v | (False,v,_)<-records])),("requiredBinaries",toJSON required),("optionalComponents",toJSON optionalRecords),("targetOSAcceptance",String "not-run")]

sign :: FilePath -> String -> IO ()
sign app identity = do
 let flags=["--force","--sign",identity]++[value | identity/="-",value<-["--options","runtime","--timestamp"]]
 binaries app >>= mapM_ (\binary->P.run "codesign" (flags++[binary])) . sortOn (negate . length . splitDirectories)
 P.finalize (app</>"Contents/Resources/runtime") (if identity=="-" then "ad-hoc" else "developer-id")
 P.run "codesign" (flags++[app])
 P.run "codesign" ["--verify","--deep","--strict",app]

diskImage :: FilePath -> FilePath -> FilePath -> MacOptions -> IO FilePath
diskImage app output temporary options = do
 let image=output</>"Sapiens.dmg";source=temporary</>"disk-image"
 P.exists image >>= flip when (fail "Sapiens.dmg already exists; choose a new output directory")
 createDirectory source
 P.run "ditto" [app,source</>takeFileName app]
 createDirectoryLink "/Applications" (source</>"Applications")
 P.run "hdiutil" ["create","-volname","Sapiens4 Development","-srcfolder",source,"-format","UDZO",image]
 when (signIdentity options/="-") $ do
  P.run "codesign" ["--force","--sign",signIdentity options,"--timestamp",image]
  P.run "codesign" ["--verify","--strict",image]
 forM_ (notaryProfile options) $ \profile -> do
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
 let appName="Sapiens4 Development.app";destination=output</>appName;artifact=output</>"sapiens4-development-"++version options++"-macos-arm64.zip"
 P.exists destination >>= flip when (fail "Output app already exists; choose a new output directory")
 P.exists artifact >>= flip when (fail "Versioned artifact already exists")
 P.exists (output</>"Sapiens.dmg") >>= flip when (fail "Sapiens.dmg already exists; choose a new output directory")
 P.withTemporary output "sapiens-package" $ \temporary -> do
  let app=temporary</>appName;resources=app</>"Contents/Resources";macos=app</>"Contents/MacOS";runtime=resources</>"runtime"
  createDirectoryIfMissing True resources
  createDirectory macos
  copyFile (root</>".build/bin/sapiens-macos") (macos</>"Sapiens4")
  void $ P.stage root runtime (version options) "macos" (providerDirectory options) (allowMissingProvider options)
  let iconset=temporary</>"AppIcon.iconset"
  createDirectory iconset
  P.run (root</>".build/bin/sapiens-macos") ["--icon",iconset,root</>"app/shell/favicon.svg"]
  P.run "iconutil" ["-c","icns",iconset,"-o",resources</>"AppIcon.icns"]
  let bundleVersion=takeWhile (`notElem` ['-','+']) (version options)
      info=P.obj [("CFBundleIdentifier",String "com.poulwalcker.sapiens4.development"),("CFBundleName",String "Sapiens4 Development"),("CFBundleDisplayName",String "Sapiens4 Development"),("CFBundleExecutable",String "Sapiens4"),("CFBundlePackageType",String "APPL"),("CFBundleVersion",toJSON bundleVersion),("CFBundleShortVersionString",toJSON bundleVersion),("CFBundleIconFile",String "AppIcon"),("NSHighResolutionCapable",Bool True),("LSMinimumSystemVersion",String "13.0"),("NSAppTransportSecurity",P.obj [("NSAllowsLocalNetworking",Bool True),("NSAllowsArbitraryLoadsInWebContent",Bool True)])]
  let infoJSON=temporary</>"Info.json"
  P.writeJSON infoJSON info
  P.run "plutil" ["-convert","xml1","-o",app</>"Contents/Info.plist",infoJSON]
  (optional,rule)<-optionalVoice root app
  relocate app root
  compatibility<-audit app optional rule
  let manifestPath=runtime</>"package-manifest.json"
  manifest<-P.readJSON manifestPath
  P.writeJSON manifestPath (P.put "macOSCompatibility" compatibility manifest)
  sign app (signIdentity options)
  forM_ (notaryProfile options) $ \profile -> do
   let submitted=temporary</>"notarization.zip"
   P.run "ditto" ["-c","-k","--keepParent",app,submitted]
   P.run "xcrun" ["notarytool","submit",submitted,"--keychain-profile",profile,"--wait"]
   P.run "xcrun" ["stapler","staple",app]
   P.run "xcrun" ["stapler","validate",app]
   P.run "spctl" ["--assess","--type","execute","--verbose=2",app]
  renameDirectory app destination
  P.run "ditto" ["-c","-k","--keepParent",destination,artifact]
  image<-diskImage destination output temporary options
  P.receipt artifact (Just image) (destination</>"Contents/Resources/runtime/package-manifest.json") (replaceExtension artifact ".release.json")
  putStrLn ("Built "++destination++"\nInstaller "++image++"\nPortable "++artifact)
