{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Main where

import Control.Exception
import Control.Monad
import qualified Data.ByteString as B
import Data.Char (isDigit)
import Data.List (sortOn, sort, isPrefixOf, nub)
import Data.Maybe
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Assets (allAssets, assetFile)
import Foreign.C.Types (CInt(..), CUInt(..))
#ifdef mingw32_HOST_OS
import Control.Concurrent (runInBoundThread)
import Foreign.Ptr (Ptr, castPtr)
#else
import Foreign.C.Error (throwErrnoIfMinus1_)
import Foreign.C.String (CString)
import GHC.Foreign (withCString)
import GHC.IO.Encoding (getFileSystemEncoding)
#endif
import System.CPUTime
import System.Directory
import System.Environment
import System.FilePath
import System.Info
import System.IO
import System.Process

main :: IO ()
main = do
 args <- getArgs
 root <- getCurrentDirectory >>= canonicalizePath
 case args of
  ["stage", destination] -> stage root destination
  ["macos", version, output] -> packageMac root version output False
  ["macos", version, output,"--release"] -> packageMac root version output True
  _ -> fail "Usage: Package.hs stage NEW_RUNTIME | macos VERSION NEW_OUTPUT_DIRECTORY"

run :: FilePath -> [String] -> IO ()
run = callProcess
capture :: FilePath -> [String] -> IO String
capture program args = readProcess program args ""
exists :: FilePath -> IO Bool
exists path = doesPathExist path >>= \present -> if present then pure True else pathIsSymbolicLink path `catch` \(_::IOException) -> pure False
inside :: FilePath -> FilePath -> Bool
inside root path = let relative=makeRelative root path in not (isAbsolute relative) && ".." `notElem` splitDirectories relative

renameNew :: FilePath -> FilePath -> IO ()
#ifdef mingw32_HOST_OS
foreign import ccall unsafe "MoveFileExW" moveFileNew :: Ptr () -> Ptr () -> CUInt -> IO CInt
foreign import ccall unsafe "GetLastError" lastFileError :: IO CUInt
renameNew source destination = runInBoundThread $ wide source $ \from -> wide destination $ \to -> do
 result <- moveFileNew from to 0
 when (result==0) $ do
  code <- lastFileError
  fail ("Cannot publish "++source++" to "++destination++" (Windows error "++show code++")")
 where
 wide path action = do
  parent <- canonicalizePath (takeDirectory path)
  let absolute=parent</>takeFileName path
      extended | "\\\\?\\" `isPrefixOf` absolute = absolute
               | "\\\\" `isPrefixOf` absolute = "\\\\?\\UNC\\"++drop 2 absolute
               | otherwise = "\\\\?\\"++absolute
  B.useAsCString (TE.encodeUtf16LE (T.pack extended)<>B.pack [0,0]) (action . castPtr)
#else
#ifdef darwin_HOST_OS
foreign import ccall unsafe "renamex_np" renameExclusive :: CString -> CString -> CUInt -> IO CInt
#elif defined(linux_HOST_OS)
foreign import ccall unsafe "renameat2" renameExclusiveAt :: CInt -> CString -> CInt -> CString -> CUInt -> IO CInt
#endif
renameNew source destination = do
 encoding <- getFileSystemEncoding
 withCString encoding source $ \from -> withCString encoding destination $ \to ->
#ifdef darwin_HOST_OS
  throwErrnoIfMinus1_ ("publish "++destination) (renameExclusive from to 4)
#elif defined(linux_HOST_OS)
  throwErrnoIfMinus1_ ("publish "++destination) (renameExclusiveAt (-100) from (-100) to 1)
#else
  fail "Exclusive package publication is unsupported on this platform"
#endif
#endif

files :: FilePath -> IO [FilePath]
files root = do
 present <- doesDirectoryExist root
 if not present then pure [] else sort . concat <$> (listDirectory root >>= mapM visit)
 where
 visit name = do
  let path=root</>name
  link <- pathIsSymbolicLink path
  when link $ fail ("Package payload may not contain symbolic links: "++path)
  directory <- doesDirectoryExist path
  if directory then files path else pure [path]

copy :: FilePath -> FilePath -> IO ()
copy source target = createDirectoryIfMissing True (takeDirectory target) >> copyFile source target
macho :: FilePath -> IO Bool
macho path = do
 present <- doesFileExist path
 link <- pathIsSymbolicLink path
 if not present || link then pure False else withBinaryFile path ReadMode $ \handle -> do
  magic <- B.hGet handle 4
  pure $ magic `elem` map B.pack [[254,237,250,206],[206,250,237,254],[254,237,250,207],[207,250,237,254],[202,254,186,190],[190,186,254,202],[202,254,186,191],[191,186,254,202]]

binaries :: FilePath -> IO [FilePath]
binaries root = files root >>= filterM macho

dependencies :: FilePath -> IO [String]
dependencies binary = map (T.unpack . fst . T.breakOn " (" . T.strip . T.pack) . drop 1 . lines <$> capture "otool" ["-L",binary]

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
 paths <- binaries app
 loop app Set.empty Map.empty [(path,path) | path<-paths]
 where
 loop _ _ _ [] = pure ()
 loop app seen origins ((binary,source):pending)
  | Set.member binary seen = loop app seen origins pending
  | otherwise = do
   let frameworks=app</>"Contents/Frameworks"
   commands <- capture "otool" ["-l",source]
   edges <- dependencies source
   (nextOrigins,added) <- foldM (\(known,queued) dependency ->
    if systemDependency dependency || dependency `elem` loadFields "LC_ID_DYLIB" "name" commands then pure (known,queued) else do
     let candidates=case strip "@rpath/" dependency of
          Just suffix -> [expand app source path</>suffix | path<-loadFields "LC_RPATH" "path" commands]
          Nothing -> [expand app source dependency]
     resolved <- firstFile candidates >>= maybe (fail $ "Unresolved non-system dependency "++dependency++" in "++source) canonicalizePath
     if source==binary && inside app resolved && any (`isPrefixOf` dependency) ["@rpath/","@loader_path/","@executable_path/"] then pure (known,queued) else do
      let internal=inside app resolved
          target=if internal then resolved else frameworks</>takeFileName resolved
      present <- exists target
      when (present && not internal) $ do
       identity <- maybe (canonicalizePath target) pure (Map.lookup target known)
       unless (identity==resolved) $ fail ("Library filename collision at "++target++": "++identity++" and "++resolved)
      unless present $ createDirectoryIfMissing True frameworks >> copyFile resolved target
      run "install_name_tool" ["-change",dependency,"@loader_path/"++relativeBetween (takeDirectory binary) target,binary]
      pure (if internal then known else Map.insert target resolved known,[(target,resolved) | not present]++queued)) (origins,[]) edges
   when (takeDirectory binary==frameworks) $ run "install_name_tool" ["-id","@rpath/"++takeFileName binary,binary]
   loop app (Set.insert binary seen) nextOrigins (added++pending)
 strip prefix value = T.unpack <$> T.stripPrefix (T.pack prefix) (T.pack value)

audit :: FilePath -> IO ()
audit input = do
 app <- canonicalizePath input
 paths <- binaries app >>= mapM canonicalizePath
 forM_ paths $ \binary -> do
  architectures <- words <$> capture "lipo" ["-archs",binary]
  unless (architectures==["arm64"]) $ fail ("Expected arm64-only payload: "++binary)
  commands <- capture "otool" ["-l",binary]
  unless (all (=="1") $ loadFields "LC_BUILD_VERSION" "platform" commands) $ fail ("Non-macOS Mach-O platform: "++binary)
  let minimums=loadFields "LC_BUILD_VERSION" "minos" commands++loadFields "LC_VERSION_MIN_MACOSX" "version" commands
      parse value = case traverse readNumber (splitVersion value) of
       Just parts -> pure (take 3 (parts++repeat 0))
       Nothing -> fail ("Invalid macOS version: "++value)
      readNumber value = case reads value of [(n,"")] -> Just (n::Int); _ -> Nothing
  when (null minimums) $ fail ("No macOS minimum: "++binary)
  forM_ minimums $ \value -> parse value >>= \minimum -> when (minimum>[13,0,0]) (fail (binary++" requires macOS "++value))
  edges <- dependencies binary
  let identities=loadFields "LC_ID_DYLIB" "name" commands;rpaths=loadFields "LC_RPATH" "path" commands
  forM_ edges $ \dependency -> unless (dependency `elem` identities || systemDependency dependency) $ do
   let candidates=case T.stripPrefix "@rpath/" (T.pack dependency) of
        Just suffix -> [expand app binary path</>T.unpack suffix | path<-rpaths]
        Nothing -> [expand app binary dependency]
   target <- firstFile candidates >>= maybe (fail $ "Unresolved packaged dependency: "++dependency) canonicalizePath
   unless (inside app target) $ fail ("External packaged dependency: "++dependency)
   macho target >>= flip unless (fail $ "Unrecognized Mach-O dependency: "++target)

splitVersion :: String -> [String]
splitVersion value = case break (=='.') value of (part,[]) -> [part];(part,_:rest) -> part:splitVersion rest

sign :: FilePath -> FilePath -> Maybe String -> IO ()
sign root app identity = do
 let flags=["--force","--sign",fromMaybe "-" identity]++[value | isJust identity,value<-["--options","runtime","--timestamp"]]
 binaries app >>= mapM_ (\binary->run "codesign" (flags++[binary])) . sortOn (negate . length . splitDirectories)
 run "codesign" (flags++["--entitlements",root</>"build/package/macos-entitlements.plist",app])
 run "codesign" ["--verify","--deep","--strict",app]

diskImage :: FilePath -> FilePath -> Maybe String -> IO FilePath
diskImage app temporary plan = do
 profile <- if isJust plan then lookupEnv "SAPIENS_MAC_NOTARY_PROFILE" else pure Nothing
 let image=temporary</>"Sapiens.dmg";source=temporary</>"disk-image"
 createDirectory source
 run "ditto" [app,source</>takeFileName app]
 createDirectoryLink "/Applications" (source</>"Applications")
 run "hdiutil" ["create","-volname",dropExtension (takeFileName app),"-srcfolder",source,"-format","UDZO",image]
 when (isJust plan) $ do
  run "codesign" ["--force","--sign",fromMaybe "-" plan,"--timestamp",image]
  run "codesign" ["--verify","--strict",image]
 forM_ profile $ \profile -> do
  run "xcrun" ["notarytool","submit",image,"--keychain-profile",profile,"--wait"]
  run "xcrun" ["stapler","staple",image]
  run "xcrun" ["stapler","validate",image]
  run "spctl" ["--assess","--type","open","--context","context:primary-signature",image]
 run "hdiutil" ["verify",image]
 let mount=temporary</>"mounted"
 createDirectory mount
 bracket_ (run "hdiutil" ["attach","-readonly","-nobrowse","-mountpoint",mount,image]) (run "hdiutil" ["detach",mount]) $ do
  target<-getSymbolicLinkTarget (mount</>"Applications")
  unless (target=="/Applications") $ fail "DMG Applications link is incorrect"
  let installed=temporary</>"installed copy"</>takeFileName app
  run "ditto" [mount</>takeFileName app,installed]
  run "codesign" ["--verify","--deep","--strict",installed]
  audit installed
  forM_ profile $ \_ -> run "spctl" ["--assess","--type","execute","--verbose=2",installed]
 pure image

withTemporary :: FilePath -> (FilePath -> IO a) -> IO a
withTemporary parent action = do
 nonce <- getCPUTime
 let temporary=parent</>(".sapiens-package."++show nonce)
 bracket (createDirectory temporary >> pure temporary) removePathForcibly action

stage :: FilePath -> FilePath -> IO ()
stage root destination = do
 exists destination >>= flip when (fail "Runtime destination must be new")
 output <- makeAbsolute destination
 when (inside output root) $ fail "Runtime destination must not contain the source checkout"
 createDirectory destination
 flip onException (removePathForcibly destination) $ do
  notices <- concat <$> mapM (files . (root</>)) ["license","submodules/libs4/license","submodules/blindly4/license"]
  prompts <- files (root</>"corpora/prompts")
  let suffix=if os=="mingw32" then ".exe" else ""
      blindly=if os=="mingw32" then "windows/blindly4.exe" else "release/blindly4"
      payload=nub (map assetFile allAssets++[".build/bin/sapiens4"++suffix,"submodules/blindly4/.build/"++blindly]++map (makeRelative root) (notices++prompts))
  forM_ payload $ \path -> copy (root</>path) (destination</>path)
  when (os=="mingw32") $ forM_ ["AppIcon.ico","AppIconDark.ico"] $ \icon -> copy (root</>"environments/windows"</>icon) (destination</>"environments/windows"</>icon)

packageMac :: FilePath -> String -> FilePath -> Bool -> IO ()
packageMac root version outputInput public = do
 unless (os=="darwin" && arch=="aarch64") $ fail "macOS ARM64 build host required"
 unless (length (splitVersion version)==3 && all (\v -> not (null v) && all isDigit v) (splitVersion version)) $ fail "Version must be MAJOR.MINOR.PATCH"
 identity <- if public then lookupEnv "SAPIENS_MAC_SIGN_IDENTITY" else pure Nothing
 profile <- if public then lookupEnv "SAPIENS_MAC_NOTARY_PROFILE" else pure Nothing
 unless ((not public || isJust identity && isJust profile) && isJust identity==isJust profile && maybe True (not . null) identity && maybe True (not . null) profile) $ fail "Set both signing identity and notary profile for a public release, or neither for a local build"
 output <- makeAbsolute outputInput
 exists output >>= flip when (fail "Output must be a new directory")
 when (inside output root) $ fail "Output must not contain the source checkout"
 createDirectoryIfMissing True (takeDirectory output)
 withTemporary (takeDirectory output) $ \temporary -> do
  let product=temporary</>"product";app=temporary</>"Sapiens4.app";resources=app</>"Contents/Resources";macos=app</>"Contents/MacOS"
  createDirectory product
  createDirectoryIfMissing True resources
  createDirectory macos
  copy (root</>".build/bin/sapiens-macos") (macos</>"Sapiens4")
  stage root (resources</>"runtime")
  let iconset=temporary</>"AppIcon.iconset"
  createDirectory iconset
  run (root</>".build/bin/sapiens-macos") ["--icon",iconset,root</>"app/shell/favicon.svg"]
  run "iconutil" ["-c","icns",iconset,"-o",resources</>"AppIcon.icns"]
  writeFile (app</>"Contents/Info.plist") $ "<?xml version=\"1.0\" encoding=\"UTF-8\"?><!DOCTYPE plist PUBLIC \"-//Apple//DTD PLIST 1.0//EN\" \"http://www.apple.com/DTDs/PropertyList-1.0.dtd\"><plist version=\"1.0\"><dict>"++
   concat ["<key>"++key++"</key><string>"++value++"</string>" | (key,value)<-[("CFBundleIdentifier","com.sapiens4.desktop"),("CFBundleName","Sapiens4"),("CFBundleDisplayName","Sapiens4"),("CFBundleExecutable","Sapiens4"),("CFBundlePackageType","APPL"),("CFBundleVersion",version),("CFBundleShortVersionString",version),("CFBundleIconFile","AppIcon"),("LSMinimumSystemVersion","13.0"),("NSMicrophoneUsageDescription","Sapiens4 uses the microphone to record voice messages.")]]++
   "<key>NSHighResolutionCapable</key><true/><key>NSAppTransportSecurity</key><dict><key>NSAllowsLocalNetworking</key><true/><key>NSAllowsArbitraryLoadsInWebContent</key><true/></dict></dict></plist>"
  run "plutil" ["-lint",app</>"Contents/Info.plist"]
  relocate app
  audit app
  sign root app identity
  forM_ profile $ \credential -> do
   let submitted=temporary</>"notarization.zip"
   run "ditto" ["-c","-k","--keepParent",app,submitted]
   run "xcrun" ["notarytool","submit",submitted,"--keychain-profile",credential,"--wait"]
   run "xcrun" ["stapler","staple",app]
   run "xcrun" ["stapler","validate",app]
   run "spctl" ["--assess","--type","execute","--verbose=2",app]
  image <- diskImage app temporary identity
  let artifact=product</>"Sapiens.dmg"
  renameFile image artifact
  checksum <- capture "shasum" ["-a","256",artifact]
  writeFile (product</>"SHA256SUMS") (take 64 checksum++"  Sapiens.dmg\n")
  renameNew product output
  putStrLn ("Installer "++output</>"Sapiens.dmg")
