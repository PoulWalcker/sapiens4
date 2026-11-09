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
import Data.List (sort, sortOn, nub, isPrefixOf, dropWhileEnd)
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
 when (isNothing providerInput && not allowMissing) $
  acquireProvider root platform provider True Nothing
 present <- doesDirectoryExist provider
 if not present then
  if allowMissing then pure Nothing else fail "Complete pinned Codex payload required; acquire the provider first"
 else do
  metadata <- readJSON (provider</>"sapiens-provider.json")
  let identity=providerIdentity metadata
  unless (identityPlatform identity==platform) $ fail "Provider platform does not match package"
  verifyInventory provider (providerEntries metadata) ["sapiens-provider.json"]
  copyTree provider (destination</>"codex")
  doesFileExist (destination</>"codex"</>identityEntry identity) >>= flip unless (fail "Provider entrypoint is missing")
  pure (Just metadata)

stage :: FilePath -> FilePath -> String -> Platform -> Maybe FilePath -> Bool -> IO Manifest
stage rootInput destinationInput version platform providerInput allowMissing = do
 unless (validVersion version) $ fail "Package version must be SemVer"
 root <- canonicalizePath rootInput
 destination <- canonicalizePath destinationInput
 exists destination >>= flip when (fail "Runtime destination must be new; refusing to mix old and new payloads")
 when (inside destination root) $ fail "Runtime destination must not contain the source checkout"
 createDirectoryIfMissing True destination
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
 let manifest=newManifest source (hash $ L.concat tree) platform version provider versions
 writeJSON (destination</>"package-manifest.json") manifest
 printJSON $ obj [("runtime",toJSON destination),("manifest",toJSON $ destination</>"package-manifest.json"),("buildId",toJSON $ packageBuildId $ manifestIdentity manifest),("status",toJSON (if isJust provider then "provider-complete" else "validation-provider-incomplete" :: String))]
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
 manifest <- readJSON (runtime</>"package-manifest.json")
 when (isNothing $ manifestSigning manifest) $ fail "Finalized package signing mode missing"
 Inventory entries <- readJSON (runtime</>"package-inventory.json")
 verifyInventory runtime entries ["package-inventory.json"]
 forM_ (manifestProvider manifest) $ \summary -> do
  let provider=runtime</>"codex";path=provider</>"sapiens-provider.json"
  metadata <- readJSON path
  checksum <- digest path
  unless (manifestProviderHash manifest==Just checksum && identityVersion (providerIdentity metadata)==summaryVersion summary) $ fail "Provider inventory differs from package manifest"
  verifyInventory provider (providerEntries metadata) ["sapiens-provider.json"]
 printJSON $ obj [("runtime",toJSON runtime),("inventory",String "verified"),("files",toJSON $ length entries)]

receipt :: FilePath -> Maybe FilePath -> FilePath -> FilePath -> IO ()
receipt artifact installer manifestPath output = do
 ReceiptInput identity signing <- readJSON manifestPath
 manifestHash <- digest manifestPath
 records <- forM (maybe [] (\path -> [(Installer,path)]) installer ++ [(PortableZip,artifact)]) $ \(kind,path) ->
  Artifact kind path <$> getFileSize path <*> digest path <*> pure manifestHash
 writeJSON output (Receipt identity signing records)

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

acquireProvider :: FilePath -> Platform -> FilePath -> Bool -> Maybe FilePath -> IO ()
acquireProvider root platform destinationInput ensure cacheInput = do
 lock <- readJSON (root</>"build/codex-provider-lock.json")
 let (entry,asset)=lockAsset platform lock
     identity=ProviderIdentity (lockVersion lock) platform entry (downloadHash asset) (Just $ lockCommit lock)
 destination <- canonicalizePath destinationInput
 present <- exists destination
 if present then do
  unless ensure $ fail "Provider destination already exists; use --ensure to verify it"
  metadata <- readJSON (destination</>"sapiens-provider.json")
  unless (providerIdentity metadata==identity) $ fail "Existing provider does not match the pinned release; use a fresh destination"
  verifyInventory destination (providerEntries metadata) ["sapiens-provider.json"]
  printJSON $ obj [("version",toJSON $ lockVersion lock),("destination",toJSON destination),("verified",Bool True)]
 else do
  let parent=takeDirectory destination;cache=fromMaybe (parent</>".downloads") cacheInput
  createDirectoryIfMissing True cache
  let archive=cache</>T.unpack (downloadHash asset)++".tar.gz"
  cached <- doesFileExist archive
  correct <- if cached then (==downloadHash asset) <$> digest archive else pure False
  unless correct $ do
   let partial=replaceExtension archive ".downloading"
   download asset partial
   renameFile partial archive
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
   forM_ ([layoutEntry layout]++maybeToList (layoutExec layout)) $ \path ->
    doesFileExist (payload</>path) >>= flip unless (fail $ "Incomplete provider package: "++path)
   forM_ [layoutResources layout,layoutPath layout] $ \path -> do
    directory <- doesDirectoryExist (payload</>path)
    nonempty <- if directory then not . null <$> listDirectory (payload</>path) else pure False
    unless nonempty $ fail ("Missing provider resource directory: "++path)
   forM_ [entry,"bin/codex-code-mode-host"++executableSuffix platform,"codex-path/rg"++executableSuffix platform] $ \path ->
    doesFileExist (payload</>path) >>= flip unless (fail $ "Incomplete provider package: "++path)
   forM_ (lockNotices lock) $ \(path,notice) -> download notice (payload</>path)
   records <- inventory payload ["sapiens-provider.json"]
   writeJSON (payload</>"sapiens-provider.json") (newProvider identity layout records)
   renameDirectory payload destination
   printJSON $ obj [("version",toJSON $ lockVersion lock),("platform",toJSON $ providerPlatform platform),("files",toJSON $ length records),("destination",toJSON destination)]

command :: FilePath -> [String] -> IO ()
command root args = case args of
 "stage":rest -> do
  source<-required "root" rest;destination<-required "destination" rest;version<-required "version" rest
  platform<-required "platform" rest >>= either fail pure . parsePlatform
  void $ stage source destination version platform (option "provider" rest) ("--allow-missing-provider" `elem` rest)
 "finalize":rest -> do
  runtime<-required "runtime" rest;signing<-required "signing" rest >>= either fail pure . parseSigning
  finalize runtime signing
 "verify":rest -> required "runtime" rest >>= verify
 "receipt":rest -> do artifact<-required "artifact" rest;manifest<-required "manifest" rest;output<-required "output" rest;receipt artifact (option "installer" rest) manifest output
 "provenance":rest -> do output<-required "output" rest;provenance root >>= writeJSON output
 "provider":rest -> do
  platform<-required "platform" rest >>= either fail pure . parseProviderPlatform
  destination<-required "destination" rest
  acquireProvider root platform destination ("--ensure" `elem` rest) (option "cache" rest)
 _ -> fail "Usage: runghc -ibuild/package build/Package.hs {macos|stage|finalize|verify|receipt|provenance|provider} ..."
