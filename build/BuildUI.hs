module BuildUI where

import Types
import Crypto (hash)
import Control.Monad
import Data.Aeson hiding (pairs)
import qualified Data.ByteString as B
import Data.List
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import System.Directory
import System.Environment
import System.FilePath
import System.Info
import System.Process

filesUnder :: FilePath -> IO [FilePath]
filesUnder root = do
 entries <- listDirectory root
 concat <$> mapM (\entry -> let p=root</>entry in doesDirectoryExist p >>= \d -> if d then filesUnder p else pure [p]) entries
buildUI :: FilePath -> IO ()
buildUI root = do
 let libs=root</>"submodules/libs4"; work=libs</>"purescript"; output=root</>".build/web"; stamp=output</>"source.sha256"
 source <- filter ((`elem` [".purs",".js"]).takeExtension) <$> filesUnder (root</>"app")
 support <- filesUnder (work</>"src")
 let inputs = sort source ++ sort support ++ [work</>"spago.yaml",work</>"spago.lock",libs</>"package-lock.json",root</>"build/BuildUI.hs"]
 raw <- B.concat <$> mapM (\p -> (TE.encodeUtf8 (T.pack (makeRelative root p)) <>) <$> B.readFile p) inputs
 let fingerprint=hash raw
 saved <- doesFileExist stamp >>= \exists -> if exists then readText stamp else pure ""
 present <- and <$> mapM (doesFileExist . (output</>)) ["app.js","sw.js","guest.js"]
 unless (saved==fingerprint && present) $ do
  version <- readProcess "node" ["--version"] ""
  let components = map (reads . T.unpack) (T.splitOn "." (T.drop 1 (T.strip (T.pack version)))) :: [[(Int,String)]]
  require (case components of [(major,"")]:[(minor,"")]:_ -> (major,minor)>=(22,5); _ -> False) 500 "Building the UI requires Node.js 22.5 or newer on PATH."
  path <- getEnv "PATH"
  -- Spago prefers purs.cmd even when purs.exe precedes it. Keep npm shims out
  -- of its Windows PATH so later shell-free compiler calls use the native exe.
  setEnv "PATH" (if os=="mingw32" then path else (libs</>"node_modules/.bin")++[searchPathSeparator]++path)
  setEnv "XDG_CACHE_HOME" (libs</>".cache")
  setEnv "npm_config_cache" (libs</>".cache/npm")
  when (os=="mingw32") (setEnv "LOCALAPPDATA" (libs</>".cache"))
  installed <- doesFileExist (libs</>"node_modules/.bin/purs")
  unless installed (tool "npm" ["ci","--prefix",libs])
  when (os=="mingw32") $ do
   let bin=libs</>".cache/bin"
   createDirectoryIfMissing True bin
   copyFile (libs</>"node_modules/purescript/purs.bin") (bin</>"purs.exe")
   current <- getEnv "PATH"
   setEnv "PATH" (bin++[searchPathSeparator]++current)
  withCurrentDirectory work $ do
   tool "spago" ["install","--quiet"]
   sourceJSON <- readTool "spago" ["sources","--json","--quiet"]
   sources <- either (failure 500 . T.pack) pure (eitherDecodeStrict (TE.encodeUtf8 (T.pack sourceJSON)))
   createDirectoryIfMissing True output
   tool "purs" (["compile"] ++ (sources::[String]) ++ [root</>"app/**/*.purs","--output",output</>"modules"])
  forM_ [("Sapiens.Main","app.js"),("Sapiens.ServiceWorker","sw.js"),("Sapiens.Guest","guest.js")] $ \(modName,filename) -> do
   let entry=output</>filename<>".entry.js"
   writeFile entry ("import {main} from \"./modules/"<>modName<>"/index.js\";main();\n")
   tool "esbuild" [entry,"--bundle","--minify","--format=iife","--target=es2020","--legal-comments=none","--outfile="<>output</>filename]
   removeFile entry
  writeText stamp fingerprint
 where
 tool name args = command name args >>= \(program,arguments) -> callProcess program arguments
 readTool name args = command name args >>= \(program,arguments) -> readProcess program arguments ""
 command name args
  | os=="mingw32", Just entry <- lookup name [("spago","spago/bin/bundle.js"),("esbuild","esbuild/bin/esbuild")] =
     pure ("node",(root</>"submodules/libs4/node_modules"</>entry):args)
  | os=="mingw32" && name=="npm" = do
     script <- findExecutable (name<>".cmd") >>= maybe (ioError $ userError $ "Missing build tool: "<>name) pure
     pure ("cmd",["/d","/c",script]++args)
  | otherwise = pure (name,args)
