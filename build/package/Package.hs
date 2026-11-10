{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}
module Package (module Package, module Format) where

import Control.Exception
import Control.Monad
import Data.Aeson (toJSON, Value(String, Bool))
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Crypto.Hash.SHA256 as SHA
import Data.Char (isSpace)
import Data.List (sort, sortOn, nub, isPrefixOf, isSuffixOf, dropWhileEnd)
import Data.Maybe
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Word (Word64)
import System.Directory
import System.FilePath
import System.IO.Error (isAlreadyExistsError)
import System.Process
import System.Random (randomIO)
#ifndef mingw32_HOST_OS
import qualified System.Posix.Directory as Posix
#endif
import Options
import Format

digest :: FilePath -> IO T.Text
digest path = hash <$> L.readFile path

run :: FilePath -> [String] -> IO ()
run = callProcess
capture :: FilePath -> [String] -> IO String
capture program args = trim <$> readProcess program args ""
 where trim = dropWhileEnd isSpace . dropWhile isSpace
atRoot :: FilePath -> FilePath -> [String] -> IO String
atRoot root program args = withCurrentDirectory root (capture program args)
exists :: FilePath -> IO Bool
exists path = doesPathExist path >>= \present -> if present then pure True else pathIsSymbolicLink path `catch` \(_::IOException) -> pure False
inside :: FilePath -> FilePath -> Bool
inside root path = let relative=makeRelative root path in not (isAbsolute relative) && ".." `notElem` splitDirectories relative
portable :: FilePath -> FilePath
portable = map (\c -> if isPathSeparator c then '/' else c)

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

inventory :: FilePath -> [FilePath] -> IO [InventoryEntry]
inventory root excluded = do
 entries <- sortOn (portable . makeRelative root) <$> files root
 forM (filter (\p -> portable (makeRelative root p) `notElem` excluded) entries) $ \path ->
  InventoryEntry (portable $ makeRelative root path) <$> getFileSize path <*> digest path

verifyInventory :: FilePath -> [InventoryEntry] -> [FilePath] -> IO ()
verifyInventory root expected excluded = do
 observed <- inventory root excluded
 unless (observed == expected) $ fail "Inventory differs from final package bytes"

copy :: FilePath -> FilePath -> IO ()
copy source target = createDirectoryIfMissing True (takeDirectory target) >> copyFile source target
copyTree :: FilePath -> FilePath -> IO ()
copyTree source destination = do
 createDirectoryIfMissing True destination
 names <- listDirectory source
 forM_ names $ \name -> do
  let path=source</>name;target=destination</>name
  pathIsSymbolicLink path >>= flip when (fail "Package payload may not contain symbolic links")
  directory <- doesDirectoryExist path
  if directory then copyTree path target else copy path target

sourceIsDirty :: FilePath -> IO Bool
sourceIsDirty root = not . null <$> atRoot root "git" ["status","--porcelain","--untracked-files=normal","--ignore-submodules=untracked","--","app","build","corpora","remote","environments","license","docs","spec",".github","submodules","README.md",".gitmodules",".gitignore"]
provenance :: FilePath -> IO Provenance
provenance root = do
 commit <- T.pack <$> atRoot root "git" ["rev-parse","HEAD"]
 dirty <- sourceIsDirty root
 pins <- forM ["libs4","blindly4"] $ \name -> do
  pin <- atRoot root "git" ["-C","submodules/"++name,"rev-parse","HEAD"]
  pure (T.pack name,T.pack pin)
 pure $ Provenance commit dirty (M.fromList pins)

-- Validate a provider before any of its files enter the staged runtime.
stageProvider :: FilePath -> FilePath -> Platform -> Maybe FilePath -> Bool -> IO (Maybe Provider)
stageProvider root destination platform providerInput allowMissing = do
 provider <- canonicalizePath (fromMaybe (root</>".build/codex") providerInput)
 present <- doesDirectoryExist provider
 if not present && (isJust providerInput || allowMissing) then
  if allowMissing then pure Nothing else fail "Complete pinned Codex payload required; acquire the provider first"
 else withPinnedProvider root platform Nothing $ \payload expected -> do
  unless present $ installProvider payload provider
  metadata <- validateProvider payload expected provider
  copyTree provider (destination</>"codex")
  pure (Just metadata)

stage :: Channel -> FilePath -> FilePath -> Version -> Platform -> Maybe FilePath -> Bool -> IO Manifest
stage channel rootInput destinationInput version platform providerInput allowMissing = do
 root <- canonicalizePath rootInput
 output <- canonicalizePath destinationInput
 exists output >>= flip when (fail "Runtime destination must be new; refusing to mix old and new payloads")
 when (inside output root) $ fail "Runtime destination must not contain the source checkout"
 withTemporary (takeDirectory output) ".runtime-stage" $ \temporary -> do
  let destination=temporary</>"runtime"
  createDirectory destination
  let windows=platform==Windows;suffix=executableSuffix platform
  appFiles <- concat <$> mapM (files . (root</>)) ["app/theme","app/shell","corpora/prompts"]
  notices <- concat <$> mapM (files . (root</>)) ["license","submodules/libs4/license","submodules/blindly4/license"]
  let relative = [".build/bin/sapiens4"++suffix,".build/web/app.js",".build/web/guest.js",".build/web/sw.js","submodules/libs4/web/remote-vendor.js","submodules/blindly4/.build/"++if windows then "windows/blindly4.exe" else "release/blindly4"] ++ map (makeRelative root) (filter ((/=".purs").takeExtension) appFiles ++ notices)
  forM_ (sort $ nub relative) $ \path -> copy (root</>path) (destination</>path)
  provider <- stageProvider root destination platform providerInput allowMissing
  when windows $ forM_ ["AppIcon.ico","AppIconDark.ico"] $ \icon -> copy (root</>"environments/windows"</>icon) (destination</>"environments/windows"</>icon)
  git <- doesPathExist (root</>".git")
  source <- if git then provenance root else readJSON (root</>"build/source-provenance.json")
  tracked <- if git then splitNul <$> atRoot root "git" ["ls-files","--cached","--others","--exclude-standard","-z"] else map (portable . makeRelative root) . concat <$> mapM (files . (root</>)) ["app","build","corpora","remote","environments","license","docs"]
  let relevant path = any (`isPrefixOf` portable path) ["app/","build/","corpora/","remote/","environments/","license/","docs/",".github/"] || path `elem` ["README.md",".gitmodules",".gitignore"]
  tree <- forM (sort $ filter (\p -> not (null p) && relevant p && takeExtension p `notElem` [".pyc",".pyo"]) tracked) $ \path -> do
   present <- doesFileExist (root</>path)
   if present then do checksum<-SHA.hashlazy <$> L.readFile (root</>path);pure (L.fromStrict $ TE.encodeUtf8 (T.pack path)<>B.singleton 0<>checksum) else pure L.empty
  versions <- forM [("ghc",["--numeric-version"]),("cabal",["--numeric-version"]),("rustc",["--version"]),("node",["--version"])] $ \(name,args) -> do
   value<-atRoot root name args
   first<-case lines value of line:_->pure line;[]->fail ("Missing toolchain version: "++name)
   pure (T.pack name,first)
  let manifest=newManifest channel source (hash $ L.concat tree) platform version provider versions
  writeJSON (destination</>"package-manifest.json") manifest
  exists output >>= flip when (fail "Runtime destination already exists")
  renameDirectory destination output
  printJSON $ obj [("runtime",toJSON output),("manifest",toJSON $ output</>"package-manifest.json"),("buildId",toJSON $ packageBuildId $ manifestIdentity manifest),("status",toJSON (if isJust provider then "provider-complete" else "validation-provider-incomplete" :: String))]
  pure manifest
 where
 splitNul value = case break (=='\0') value of (a,[]) -> [a];(a,_:rest)->a:splitNul rest

finalize :: FilePath -> Signing -> IO ()
finalize runtime signing = do
 let manifestPath=runtime</>"package-manifest.json";provider=runtime</>"codex";metadataPath=provider</>"sapiens-provider.json";upstream=provider</>"sapiens-provider-upstream.json"
 manifest <- readJSON manifestPath
 either fail pure $ validateSigning (packagePlatform $ manifestIdentity manifest) signing
 when (isJust $ manifestSigning manifest) $ fail "Already finalized; assemble a fresh package before signing again"
 providerHash <- forM (manifestProvider manifest) $ \_ -> do
  exists upstream >>= flip when (fail "Already finalized; assemble a fresh package before signing again")
  metadata <- readJSON metadataPath
  copyFile metadataPath upstream
  entries <- inventory provider ["sapiens-provider.json"]
  writeJSON metadataPath (packagedProvider signing entries metadata)
  digest metadataPath
 writeJSON manifestPath (sealManifest signing providerHash manifest)
 entries <- inventory runtime ["package-inventory.json"]
 writeJSON (runtime</>"package-inventory.json") (Inventory entries)
 verify runtime

verify :: FilePath -> IO ()
verify runtime = do
 finalized <- readJSON (runtime</>"package-manifest.json")
 let manifest=finalizedManifest finalized
 Inventory entries <- readJSON (runtime</>"package-inventory.json")
 verifyInventory runtime entries ["package-inventory.json"]
 forM_ (manifestProvider manifest) $ \summary -> do
  let provider=runtime</>"codex";path=provider</>"sapiens-provider.json"
  metadata <- readJSON path
  checksum <- digest path
  unless (manifestProviderHash manifest==Just checksum && providerSummary metadata==summary && identityPlatform (providerIdentity metadata)==packagePlatform (manifestIdentity manifest)) $ fail "Provider inventory differs from package manifest"
  verifyInventory provider (providerEntries metadata) ["sapiens-provider.json"]
 printJSON $ obj [("runtime",toJSON runtime),("inventory",String "verified"),("files",toJSON $ length entries)]

receipt :: FilePath -> FilePath -> FilePath -> FilePath -> IO ()
receipt artifact installer manifestPath output = do
 unless (takeFileName manifestPath=="package-manifest.json") $ fail "Receipt must use the verified runtime package-manifest.json"
 finalized <- readJSON manifestPath
 let manifest=finalizedManifest finalized;signing=finalizedSigning finalized
 verify (takeDirectory manifestPath)
 let identity=manifestIdentity manifest
 manifestHash <- digest manifestPath
 let artifactRecord path = Artifact path <$> getFileSize path <*> digest path <*> pure manifestHash
 records <- ArtifactPair <$> artifactRecord installer <*> artifactRecord artifact
 writeJSON output (Receipt identity signing records)

-- Receipts bind trusted build-host outputs; this checks consistency and bytes,
-- not publisher signatures (which the native build hosts verify).
release :: String -> FilePath -> String -> String -> String -> FilePath -> IO ()
release repository directory commit versionInput mode output = do
 exists output >>= flip when (fail "Release list output already exists; choose a new output file")
 unless (repository=="PoulWalcker/sapiens4") $ fail "Expected release repository"
 unless (mode `elem` ["dev","prod"] && length commit==40 && all (`elem` ['0'..'9']++['a'..'f']) commit) $ fail "Expected dev/prod, full source commit and SemVer"
 version <- either fail pure $ parseVersion versionInput
 root <- canonicalizePath directory
 receipts <- filter (".release.json" `isSuffixOf`) <$> files root
 unless (length receipts==2) $ fail "Exactly two platform receipts required"
 verified <- forM receipts $ \path -> do
  Receipt identity signing pair <- readJSON path
  let platform=packagePlatform identity
      expectedSigning=if platform==MacOS then [DeveloperID] else [Authenticode]
      allowed=if mode=="prod" then expectedSigning else if platform==MacOS then [Unsigned,AdHoc] else [Unsigned]
  unless (packageChannel identity==(if mode=="prod" then Stable else Development) && packageComplete identity && not (packageDirty identity) && packageCommit identity==T.pack commit && packageVersion identity==version && signing `elem` allowed) $ fail "Incomplete, dirty, wrong-channel/source/version or incorrectly signed release"
  let installerName=if platform==MacOS then "Sapiens.dmg" else "Sapiens-Setup.exe"
      portableName=artifactPrefix (packageChannel identity)++versionText version++"-"++platformName platform++"-"++architecture platform++".zip"
  assets <- forM [(installerName,installerArtifact pair),(portableName,portableArtifact pair)] $ \(wanted,Artifact name size checksum _) -> do
   let artifact=takeDirectory path</>name
   unless (name==wanted) $ fail "Unexpected release artifact name"
   actualSize<-getFileSize artifact;actualHash<-digest artifact
   unless (actualSize==size && actualHash==checksum) $ fail "Final artifact does not match receipt"
   pure artifact
  pure (platform,assets++[path])
 unless (sort (map (platformName . fst) verified)==["macos","windows"]) $ fail "Both supported platforms required"
 withTemporary (takeDirectory output) ".release-list" $ \temporary -> do
  let staged=temporary</>"assets"
  writeFile staged (unlines $ concatMap snd verified)
  exists output >>= flip when (fail "Release list output already exists; choose a new output file")
  renameFile staged output


withTemporary :: FilePath -> String -> (FilePath -> IO a) -> IO a
withTemporary parent prefix action = do
 createDirectoryIfMissing True parent
 bracket create removePathForcibly action
 where
 create = do
  nonce <- randomIO :: IO Word64
  let path=parent</>prefix++"."++show nonce
  -- Allocate atomically; a pre-existing file/link is never used as a workspace.
  (do
#ifdef mingw32_HOST_OS
   createDirectory path
#else
   Posix.createDirectory path 0o700
#endif
   pure path) `catch` \errorValue ->
    if isAlreadyExistsError errorValue then create else throwIO (errorValue :: IOException)

download :: Download -> FilePath -> IO ()
download asset destination = do
 run "curl" ["--fail","--location","--max-time","120","--output",destination,downloadURL asset]
 checksum <- digest destination
 unless (checksum == downloadHash asset) $ fail "Pinned provider download checksum mismatch"
 size <- getFileSize destination
 forM_ (downloadSize asset) $ \expected -> unless (size==expected) $ fail "Pinned provider download size mismatch"

cachedDownload :: Download -> FilePath -> IO ()
cachedDownload asset destination = do
 present <- doesFileExist destination
 valid <- if not present then pure False else do
  size<-getFileSize destination;checksum<-digest destination
  pure (checksum==downloadHash asset && maybe True (==size) (downloadSize asset))
 unless valid $ withTemporary (takeDirectory destination) ".download" $ \temporary -> do
  let partial=temporary</>"payload"
  download asset partial
  renameFile partial destination

-- Compare against bytes reconstructed from the hash-pinned archive and notices,
-- never against a caller-authored inventory alone.
validateProvider :: FilePath -> Provider -> FilePath -> IO Provider
validateProvider payload expected directory = do
 actual <- readJSON (directory</>"sapiens-provider.json")
 unless (toJSON actual==toJSON expected) $ fail "Provider metadata does not match the complete pinned release"
 verifyInventory directory (providerEntries expected) ["sapiens-provider.json"]
 -- Byte hashes do not capture executable permissions. Use the pinned archive,
 -- not caller-supplied metadata, to identify every required macOS executable.
 when (identityPlatform (providerIdentity expected)==MacOS) $ do
  layout <- readJSON (payload</>"codex-package.json")
  forM_ (providerEntries expected) $ \entry -> do
   let path=inventoryPath entry
   requiredExec <- executable <$> getPermissions (payload</>path)
   when (requiredExec || path `elem` providerExecutables MacOS layout) $ do
    runnable <- executable <$> getPermissions (directory</>path)
    unless runnable $ fail ("Provider executable permission missing: "++path)
 pure actual

acquireProvider :: FilePath -> Platform -> FilePath -> Bool -> Maybe FilePath -> IO ()
acquireProvider root platform destinationInput ensure cacheInput = do
 destination <- canonicalizePath destinationInput
 present <- exists destination
 if present then do
  unless ensure $ fail "Provider destination already exists; use --ensure to verify it"
  withPinnedProvider root platform cacheInput $ \payload expected -> void $ validateProvider payload expected destination
 else withPinnedProvider root platform cacheInput $ \payload expected -> do
  installProvider payload destination
  void $ validateProvider payload expected destination
 printJSON $ obj [("destination",toJSON destination),("verified",Bool True)]

installProvider :: FilePath -> FilePath -> IO ()
installProvider payload destination =
 -- Publish on the destination filesystem so explicit directories work across volumes.
 withTemporary (takeDirectory destination) ".codex-copy" $ \temporary -> do
  let staged=temporary</>"payload"
  copyTree payload staged
  renameDirectory staged destination

providerExecutables :: Platform -> ProviderLayout -> [FilePath]
providerExecutables platform layout = nub $ [layoutEntry layout] ++ maybeToList (layoutExec layout) ++
 ["bin/codex-code-mode-host"++executableSuffix platform,"codex-path/rg"++executableSuffix platform]

withPinnedProvider :: FilePath -> Platform -> Maybe FilePath -> (FilePath -> Provider -> IO a) -> IO a
withPinnedProvider root platform cacheInput action = do
  lock <- readJSON (root</>"build/codex-provider-lock.json")
  let (entry,asset)=lockAsset platform lock
      identity=ProviderIdentity (lockVersion lock) platform entry (downloadHash asset) (Just $ lockCommit lock)
  let parent=root</>".build";cache=fromMaybe (parent</>".downloads") cacheInput
  createDirectoryIfMissing True cache
  let archive=cache</>T.unpack (downloadHash asset)++".tar.gz"
  cachedDownload asset archive
  withTemporary parent ".codex-stage" $ \temporary -> do
   -- Inspect every member before extraction: reject links, devices and unsafe paths.
   names <- lines <$> capture "tar" ["-tzf",archive]
   modes <- lines <$> capture "tar" ["-tvzf",archive]
   unless (not (null names) && all safeRelativePath names && length names==length modes && all (\line->case line of mode:_->mode `elem` ['-','d'];[]->False) modes) $ fail "Unsafe provider archive entry"
   let unpacked=temporary</>"payload"
   createDirectory unpacked
   run "tar" ["-xzf",archive,"-C",unpacked]
   entries <- files unpacked
   manifestPath <- case filter ((=="codex-package.json").takeFileName) entries of
    [path] -> pure path
    _ -> fail "Complete Codex package manifest missing or ambiguous"
   layout <- readJSON manifestPath
   let payload=takeDirectory manifestPath
   unless (layoutRelease layout==lockVersion lock && layoutPlatform layout==platform && layoutEntry layout==entry) $ fail "Pinned provider package identity/layout mismatch"
   forM_ (providerExecutables platform layout) $ \path ->
    doesFileExist (payload</>path) >>= flip unless (fail $ "Incomplete provider package: "++path)
   forM_ [layoutResources layout,layoutPath layout] $ \path -> do
    directory <- doesDirectoryExist (payload</>path)
    nonempty <- if directory then not . null <$> listDirectory (payload</>path) else pure False
    unless nonempty $ fail ("Missing provider resource directory: "++path)
   forM_ (lockNotices lock) $ \(path,notice) -> do
    let cachedNotice=cache</>T.unpack (downloadHash notice)
    cachedDownload notice cachedNotice
    copy cachedNotice (payload</>path)
   records <- inventory payload ["sapiens-provider.json"]
   writeJSON (payload</>"sapiens-provider.json") (newProvider identity layout records)
   action payload (newProvider identity layout records)

command :: FilePath -> [String] -> IO ()
command root args = case args of
 "stage":rest -> do
  source<-required "root" rest;destination<-required "destination" rest;version<-required "version" rest >>= either fail pure . parseVersion
  platform<-required "platform" rest >>= either fail pure . parsePlatform
  channel<-either fail pure $ parseChannel $ fromMaybe "development" (option "channel" rest)
  void $ stage channel source destination version platform (option "provider" rest) ("--allow-missing-provider" `elem` rest)
 "finalize":rest -> do
  runtime<-required "runtime" rest;signing<-required "signing" rest >>= either fail pure . parseSigning
  finalize runtime signing
 "verify":rest -> required "runtime" rest >>= verify
 "receipt":rest -> do artifact<-required "artifact" rest;manifest<-required "manifest" rest;output<-required "output" rest;required "installer" rest >>= \installer -> receipt artifact installer manifest output
 "release":rest -> do
  repository<-required "repository" rest
  directory<-required "directory" rest;commit<-required "source-commit" rest;version<-required "version" rest;output<-required "output" rest
  release repository directory commit version (fromMaybe "prod" $ option "mode" rest) output
 "provenance":rest -> do output<-required "output" rest;provenance root >>= writeJSON output
 "provider":rest -> do
  platform<-required "platform" rest >>= either fail pure . parseProviderPlatform
  destination<-required "destination" rest
  acquireProvider root platform destination ("--ensure" `elem` rest) (option "cache" rest)
 _ -> fail "Usage: runghc -ibuild/package build/Package.hs {macos|stage|finalize|verify|receipt|release|provenance|provider} ..."
