{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Package where

import Control.Exception
import Control.Monad
import Data.Aeson
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Crypto.Hash.SHA256 as SHA
import Data.Char (ord, isSpace)
import Data.Foldable (toList)
import Data.List (sort, sortOn, nub, isPrefixOf, dropWhileEnd)
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Numeric (showHex)
import System.Directory
import System.FilePath
import System.Process
import Options (option, required)

obj :: [(T.Text,Value)] -> Value
obj = Object . KM.fromList . map (\(key,value) -> (K.fromText key,value))
field :: T.Text -> Value -> Value
field key (Object values) = fromMaybe Null (KM.lookup (K.fromText key) values)
field _ _ = Null
put :: T.Text -> Value -> Value -> Value
put key value (Object values) = Object (KM.insert (K.fromText key) value values)
put _ _ _ = error "Expected JSON object"
text :: Value -> IO T.Text
text (String value) = pure value
text _ = fail "Expected JSON string"
string :: Value -> IO String
string value = T.unpack <$> text value
array :: Value -> IO [Value]
array (Array values) = pure (toList values)
array _ = fail "Expected JSON array"
get :: T.Text -> Value -> IO String
get key value = string (field key value)
readJSON :: FilePath -> IO Value
readJSON path = L.readFile path >>= either fail pure . eitherDecode
writeJSON :: FilePath -> Value -> IO ()
writeJSON path value = L.writeFile path (encode value <> "\n")
printJSON :: Value -> IO ()
printJSON = L.putStr . (<> "\n") . encode
hex :: B.ByteString -> T.Text
hex = T.pack . concatMap (\byte -> let value=showHex byte "" in if length value==1 then '0':value else value) . B.unpack
hash :: L.ByteString -> T.Text
hash = hex . SHA.hashlazy
digest :: FilePath -> IO T.Text
digest path = hash <$> L.readFile path

-- Python's existing sort_keys/separators/ensure_ascii encoding is part of the
-- provider fingerprint and buildId contract, not the pretty-printing format.
canonical :: Value -> L.ByteString
canonical value = L.fromStrict $ TE.encodeUtf8 $ render value
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

inventory :: FilePath -> [FilePath] -> IO Value
inventory root excluded = do
 entries <- sortOn (portable . makeRelative root) <$> files root
 toJSON <$> forM (filter (\p -> portable (makeRelative root p) `notElem` excluded) entries) (\path -> do
  size <- getFileSize path
  checksum <- digest path
  pure (obj [("path",toJSON $ portable $ makeRelative root path),("size",toJSON size),("sha256",String checksum)]))

verifyInventory :: FilePath -> Value -> [FilePath] -> IO ()
verifyInventory root record excluded = do
 entries <- array record
 paths <- mapM (get "path") entries
 unless (length paths == length (nub paths)) $ fail "Duplicate inventory paths"
 forM_ paths $ \path -> when (isAbsolute path || ".." `elem` splitDirectories path) $ fail "Inventory path escapes payload"
 observed <- inventory root excluded
 expected <- map snd . sortOn fst <$> mapM (\entry -> do key<-get "path" entry; pure (key,entry)) entries
 unless (observed == toJSON expected) $ fail "Inventory differs from final package bytes"

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

sourceDirty :: FilePath -> IO Bool
sourceDirty root = not . null <$> atRoot root "git" ["status","--porcelain","--untracked-files=normal","--ignore-submodules=untracked","--","app","build","corpora","remote","environments","license","docs","spec",".github","submodules","README.md",".gitmodules",".gitignore"]
provenance :: FilePath -> IO Value
provenance root = do
 commit <- atRoot root "git" ["rev-parse","HEAD"]
 dirty <- sourceDirty root
 pins <- forM ["libs4","blindly4"] $ \name -> do pin<-atRoot root "git" ["-C","submodules/"++name,"rev-parse","HEAD"];pure (T.pack name,toJSON pin)
 pure $ obj [("sourceCommit",toJSON commit),("sourceDirty",Bool dirty),("submodules",obj pins)]

stage :: FilePath -> FilePath -> String -> String -> Maybe FilePath -> Bool -> IO Value
stage rootInput destinationInput version platform providerInput allowMissing = do
 unless (platform `elem` ["macos","windows"]) $ fail "Supported primary targets: macOS arm64 and Windows x64"
 root <- canonicalizePath rootInput
 destination <- canonicalizePath destinationInput
 exists destination >>= flip when (fail "Runtime destination must be new; refusing to mix old and new payloads")
 when (inside destination root) $ fail "Runtime destination must not contain the source checkout"
 createDirectoryIfMissing True destination
 let windows=platform=="windows";suffix=if windows then ".exe" else "";architecture=if windows then "x64" else "arm64"::String
 appFiles <- concat <$> mapM (files . (root</>)) ["app/theme","app/shell","corpora/prompts"]
 notices <- concat <$> mapM (files . (root</>)) ["license","submodules/libs4/license","submodules/blindly4/license"]
 let relative = [".build/bin/sapiens4"++suffix,".build/web/app.js",".build/web/guest.js",".build/web/sw.js","submodules/libs4/web/remote-vendor.js","submodules/blindly4/.build/"++if windows then "windows/blindly4.exe" else "release/blindly4"] ++ map (makeRelative root) (filter ((/=".purs").takeExtension) appFiles ++ notices)
 forM_ (sort $ nub relative) $ \path -> copy (root</>path) (destination</>path)
 provider <- canonicalizePath (fromMaybe (root</>".build/codex") providerInput)
 when (isNothing providerInput && not allowMissing) $ if windows then do
  let script=root</>"build/package/acquire-provider.py"
  available <- doesFileExist script
  when available $ run "python" [script,"--platform","windows-x64","--destination",provider,"--ensure"]
  else do
   locked <- doesFileExist (root</>"submodules/libs4/tools/codex-provider-lock.json")
   when locked $ void (acquireProvider root "darwin-arm64" provider True Nothing)
 hasProvider <- doesDirectoryExist provider
 providerInfo <- if hasProvider then do
  metadata <- readJSON (provider</>"sapiens-provider.json")
  unless (field "schema" metadata == toJSON (1::Int) && field "platform" metadata == toJSON (if windows then "windows-x64" else "darwin-arm64"::String)) $ fail "Provider metadata schema/platform does not match package"
  entry <- get "entrypoint" metadata
  when (isAbsolute entry || ".." `elem` splitDirectories entry) $ fail "Provider entrypoint must stay inside codex/"
  verifyInventory provider (field "files" metadata) ["sapiens-provider.json"]
  copyTree provider (destination</>"codex")
  doesFileExist (destination</>"codex"</>entry) >>= flip unless (fail "Provider entrypoint is missing")
  pure $ obj [("name",String "codex"),("version",field "version" metadata),("entrypoint",toJSON $ "codex/"++portable entry),("archiveSha256",field "archiveSha256" metadata)]
  else if allowMissing then pure Null else fail "Complete pinned Codex payload required; acquire the provider first"
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
  pure (T.pack name,toJSON first)
 let inputs=foldr (uncurry put) source [("sourceTreeSha256",String $ hash $ L.concat tree),("platform",toJSON platform),("architecture",toJSON architecture),("version",toJSON version),("repository",String "PoulWalcker/sapiens4"),("channel",String "development"),("provider",providerInfo),("minimumOS",if windows then Null else String "13.0"),("supportStatus",String "declared-target"),("toolchain",obj versions)]
     manifest=foldr (uncurry put) inputs [("schemaVersion",toJSON (1::Int)),("appId",String "com.poulwalcker.sapiens4.development"),("buildId",String $ hash $ canonical inputs),("packageStatus",String $ if hasProvider then "provider-complete" else "validation-provider-incomplete"),("dataEpoch",obj [("minReadable",Null),("maxReadable",Null),("target",Null)]),("migrationSetId",Null),("legacyAdoption",String "not-implemented"),("updateDiscovery",Bool False)]
 writeJSON (destination</>"package-manifest.json") manifest
 printJSON $ obj [("runtime",toJSON destination),("manifest",toJSON $ destination</>"package-manifest.json"),("buildId",field "buildId" manifest),("status",field "packageStatus" manifest)]
 pure manifest
 where
 splitNul value = case break (=='\0') value of (a,[]) -> [a];(a,_:rest)->a:splitNul rest

finalize :: FilePath -> String -> IO ()
finalize runtime signing = do
 unless (signing `elem` ["none","ad-hoc","developer-id","authenticode"]) $ fail "Unsupported signing mode"
 let manifestPath=runtime</>"package-manifest.json";provider=runtime</>"codex";metadataPath=provider</>"sapiens-provider.json";upstream=provider</>"sapiens-provider-upstream.json"
 manifest <- readJSON manifestPath
 updated <- if field "provider" manifest /= Null then do
  exists upstream >>= flip when (fail "Already finalized; assemble a fresh package before signing again")
  copyFile metadataPath upstream
  metadata <- readJSON metadataPath
  entries <- inventory provider ["sapiens-provider.json"]
  writeJSON metadataPath $ put "files" entries $ put "inventoryStage" (toJSON $ if signing=="none" then "packaged" else "signed-package"::String) $ put "upstreamInventory" (toJSON $ takeFileName upstream) metadata
  checksum <- digest metadataPath
  pure (put "providerInventorySha256" (String checksum) manifest)
  else pure manifest
 writeJSON manifestPath (put "signing" (toJSON signing) updated)
 entries <- inventory runtime ["package-inventory.json"]
 writeJSON (runtime</>"package-inventory.json") (obj [("schemaVersion",toJSON (1::Int)),("files",entries)])
 verify runtime

verify :: FilePath -> IO ()
verify runtime = do
 manifest <- readJSON (runtime</>"package-manifest.json")
 record <- readJSON (runtime</>"package-inventory.json")
 unless (field "schemaVersion" record == toJSON (1::Int)) $ fail "Unsupported package inventory"
 verifyInventory runtime (field "files" record) ["package-inventory.json"]
 when (field "provider" manifest /= Null) $ do
  let provider=runtime</>"codex";path=provider</>"sapiens-provider.json"
  metadata <- readJSON path
  checksum <- digest path
  unless (field "providerInventorySha256" manifest == String checksum && field "version" metadata == field "version" (field "provider" manifest)) $ fail "Provider inventory differs from package manifest"
  verifyInventory provider (field "files" metadata) ["sapiens-provider.json"]
 count <- length <$> array (field "files" record)
 printJSON $ obj [("runtime",toJSON runtime),("inventory",String "verified"),("files",toJSON count)]

receipt :: FilePath -> Maybe FilePath -> FilePath -> FilePath -> IO ()
receipt artifact installer manifestPath output = do
 manifest <- readJSON manifestPath
 records <- forM (maybe [] (\path -> [(path,"installer")]) installer ++ [(artifact,"portable-zip")]) $ \(path,kind) -> do
  size <- getFileSize path;checksum<-digest path;manifestHash<-digest manifestPath
  pure $ obj ([(key,field key manifest) | key <- ["platform","architecture","buildId","minimumOS","supportStatus","signing"]] ++ [("kind",toJSON (kind::String)),("assetName",toJSON $ takeFileName path),("size",toJSON size),("sha256",String checksum),("packageManifestSha256",String manifestHash)])
 writeJSON output $ obj ([("schemaVersion",toJSON (1::Int)),("artifacts",toJSON records)] ++ [(key,field key manifest) | key <- ["appId","version","repository","channel","sourceCommit","sourceDirty","packageStatus"]])

withTemporary :: FilePath -> String -> (FilePath -> IO a) -> IO a
withTemporary parent prefix action = do
 createDirectoryIfMissing True parent
 bracket (capture "mktemp" ["-d",parent</>prefix++".XXXXXXXX"]) removePathForcibly action

safeProviderPath :: FilePath -> Bool
safeProviderPath value = not (null value) && not (isAbsolute value) && not (any (`elem` value) ['\\',':','\0','\n','\r']) && ".." `notElem` splitDirectories value

download :: Value -> FilePath -> IO ()
download asset destination = do
 address <- get "url" asset
 expected <- text (field "sha256" asset)
 run "curl" ["--fail","--location","--max-time","120","--output",destination,address]
 checksum <- digest destination
 unless (checksum == expected) $ fail "Pinned provider download checksum mismatch"
 size <- getFileSize destination
 when (field "size" asset /= Null && field "size" asset /= toJSON size) $ fail "Pinned provider download size mismatch"

acquireProvider :: FilePath -> String -> FilePath -> Bool -> Maybe FilePath -> IO Value
acquireProvider root platform destinationInput ensure cacheInput = do
 unless (platform=="darwin-arm64") $ fail "Windows provider acquisition remains in acquire-provider.py"
 lock <- readJSON (root</>"submodules/libs4/tools/codex-provider-lock.json")
 let asset=field (T.pack platform) (field "platforms" lock)
 destination <- canonicalizePath destinationInput
 present <- exists destination
 if present then do
  unless ensure $ fail "Provider destination already exists; use --ensure to verify it"
  metadata <- readJSON (destination</>"sapiens-provider.json")
  let expected=[("schema",toJSON (1::Int)),("version",field "version" lock),("platform",toJSON platform),("archiveSha256",field "sha256" asset),("entrypoint",field "entrypoint" asset)]
  unless (all (\(key,value)->field key metadata==value) expected) $ fail "Existing provider does not match the pinned release; use a fresh destination"
  verifyInventory destination (field "files" metadata) ["sapiens-provider.json"]
  let result=obj [("version",field "version" lock),("destination",toJSON destination),("verified",Bool True)]
  printJSON result
  pure result
  else do
   let parent=takeDirectory destination;cache=fromMaybe (parent</>".downloads") cacheInput
   createDirectoryIfMissing True cache
   checksum <- get "sha256" asset
   let archive=cache</>checksum++".tar.gz"
   cached <- doesFileExist archive
   correct <- if cached then (==T.pack checksum) <$> digest archive else pure False
   unless correct $ do
    let partial=replaceExtension archive ".downloading"
    download asset partial
    renameFile partial archive
   withTemporary parent ".codex-stage" $ \temporary -> do
    -- Inspect every archive member before extraction: no links, devices, or
    -- absolute/traversing/platform-dependent paths may enter the payload.
    names <- lines <$> capture "tar" ["-tzf",archive]
    modes <- lines <$> capture "tar" ["-tvzf",archive]
    unless (not (null names) && all safeProviderPath names && length names==length modes && all (\line->case line of mode:_->mode `elem` ['-','d'];[]->False) modes) $ fail "Unsafe provider archive entry"
    let unpacked=temporary</>"payload"
    createDirectory unpacked
    run "tar" ["-xzf",archive,"-C",unpacked]
    entries <- files unpacked
    manifestPath <- case filter ((=="codex-package.json").takeFileName) entries of
     [path] -> pure path
     _ -> fail "Complete Codex package manifest missing or ambiguous"
    manifest <- readJSON manifestPath
    let payload=takeDirectory manifestPath
    unless (field "layoutVersion" manifest==toJSON (1::Int) && field "version" manifest==field "version" lock && field "target" manifest==String "aarch64-apple-darwin" && field "variant" manifest==String "codex" && field "entrypoint" manifest==field "entrypoint" asset) $ fail "Pinned provider package identity/layout mismatch"
    forM_ ["entrypoint","execServer","resourcesDir","pathDir"] $ \key -> unless (key=="execServer" && field key manifest==Null) $ do
     declared <- get key manifest
     unless (safeProviderPath declared) $ fail ("Unsafe or missing declared provider path: "++T.unpack key)
     if key `elem` ["resourcesDir","pathDir"] then do
      directory <- doesDirectoryExist (payload</>declared)
      nonempty <- if directory then not . null <$> listDirectory (payload</>declared) else pure False
      unless nonempty $ fail ("Missing provider resource directory: "++declared)
      else doesFileExist (payload</>declared) >>= flip unless (fail $ "Incomplete provider package: "++declared)
    entry <- get "entrypoint" asset
    forM_ [entry,"bin/codex-code-mode-host","codex-path/rg"] $ \path -> doesFileExist (payload</>path) >>= flip unless (fail $ "Incomplete provider package: "++path)
    notices <- array (field "notices" lock)
    forM_ notices $ \notice -> do
     path <- get "path" notice
     unless (safeProviderPath path) $ fail "Unsafe provider notice path"
     download notice (payload</>path)
    records <- inventory payload ["sapiens-provider.json"]
    writeJSON (payload</>"sapiens-provider.json") $ obj [("schema",toJSON (1::Int)),("version",field "version" lock),("upstreamCommit",field "upstreamCommit" lock),("platform",toJSON platform),("entrypoint",field "entrypoint" asset),("archiveSha256",field "sha256" asset),("upstreamPackage",manifest),("files",records)]
    renameDirectory payload destination
    count <- length <$> array records
    let result=obj [("version",field "version" lock),("platform",toJSON platform),("files",toJSON count),("destination",toJSON destination)]
    printJSON result
    pure result

command :: FilePath -> [String] -> IO ()
command root args = case args of
 "stage":rest -> do
  source<-required "root" rest;destination<-required "destination" rest;version<-required "version" rest;platform<-required "platform" rest
  void $ stage source destination version platform (option "provider" rest) ("--allow-missing-provider" `elem` rest)
 "finalize":rest -> do runtime<-required "runtime" rest;signing<-required "signing" rest;finalize runtime signing
 "verify":rest -> required "runtime" rest >>= verify
 "receipt":rest -> do artifact<-required "artifact" rest;manifest<-required "manifest" rest;output<-required "output" rest;receipt artifact (option "installer" rest) manifest output
 "provenance":rest -> do output<-required "output" rest;provenance root >>= writeJSON output
 "provider":rest -> do platform<-required "platform" rest;destination<-required "destination" rest;void $ acquireProvider root platform destination ("--ensure" `elem` rest) (option "cache" rest)
 _ -> fail "Usage: runghc -ibuild/package build/Package.hs {macos|stage|finalize|verify|receipt|provenance|provider} ..."
