module Main where

import Control.Monad
import Data.List
import System.Directory
import System.Environment
import System.FilePath
import System.Info
import System.Process

main :: IO ()
main = do
 root <- getCurrentDirectory >>= buildPath
 -- Environment changes alone do not invalidate Cabal's cached executables.
 -- Keep the declared target fixed for both direct source and package builds.
 when (os=="darwin") $ setEnv "MACOSX_DEPLOYMENT_TARGET" "13.0"
 -- Source releases omit test helpers; development checkouts build the probes.
 helpers <- filterM (doesFileExist . (root </>) . snd)
  ([("sapiens-peer","spec/support/Peer.hs")] ++
   if os=="mingw32" then [("sapiens-windows-probe","spec/support/WindowsProbeMain.hs"),("sapiens-provider","spec/support/WindowsProviderMain.hs")] else [])
 let project = root </> "submodules/libs4/haskell"
     cache = project </> ".cabal"
     build = project </> (if os=="darwin" then ".build/macos-13.0" else ".build")
     suffix = if os=="mingw32" then ".exe" else ""
     targets = ["sapiens4","sapiens-domain"] ++ map fst helpers ++ if os=="darwin" then ["sapiens-macos"] else if os=="mingw32" then ["sapiens-windows"] else []
 setEnv "CABAL_DIR" cache
 createDirectoryIfMissing True cache
 index <- doesFileExist (cache </> "packages/hackage.haskell.org/01-index.tar")
 unless index (callProcess "cabal" ["update"])
 sodium <- doesFileExist (project </> "sodium/src/libsodium/.libs/libsodium.a")
 unless sodium $ do
  extracted <- doesDirectoryExist (project </> "sodium")
  unless extracted $ do
   callProcess "tar" ["-xzf",project </> "vendor/libsodium-1.0.20.tar.gz","-C",project]
   renameDirectory (project </> "libsodium-1.0.20") (project </> "sodium")
  let sodiumDirectory = project </> "sodium"
  -- Autotools rejects spaces even when the compiler accepts the checkout path.
  directory <- buildPath sodiumDirectory
  withCurrentDirectory directory $ do
   when (os=="mingw32") $ do
    setEnv "CONFIG_SHELL" "sh"
    settings <- read <$> readProcess "ghc" ["--info"] "" :: IO [(String,String)]
    compiler <- mapM (\key -> maybe (ioError $ userError $ "GHC did not report "<>key) pure (lookup key settings)) ["C compiler command","C compiler flags"]
    setEnv "CC" (map (\c -> if c=='\\' then '/' else c) (unwords compiler))
   callProcess "sh" ["configure","--disable-shared","--enable-static","--disable-dependency-tracking"]
   -- Keep libtool's shell invocation literal on Windows.
   callProcess "make" (["-j4"] ++ if os=="mingw32"
    then ["SHELL=sh","LIBTOOL=sh $(top_builddir)/libtool"] else [])
 when (os=="mingw32") $ do
  createDirectoryIfMissing True build
  callProcess "windres" ["-I",root</>"environments/windows",root</>"environments/windows/resources.rc","-O","coff","-o",build</>"windows-resources.o"]
 let cabal args = callProcess "cabal" (["--project-file="<>project</>"cabal.project","--builddir="<>build]++args)
 cabal (["build"]++map ("exe:"++) (filter (/="sapiens-windows") targets)++
  ["--ghc-options=-optc-mmacosx-version-min=13.0 -optl-mmacosx-version-min=13.0" | os=="darwin"])
 -- The desktop resource already contains the application's DPI/UAC manifest.
 when (os=="mingw32") $ cabal ["build","exe:sapiens-windows","--ghc-options=-fno-gen-manifest"]
 createDirectoryIfMissing True (root </> ".build/bin")
 createDirectoryIfMissing True (root </> ".build/domain")
 forM_ targets $ \name -> do
  source <- trim <$> readProcess "cabal" ["--project-file="<>project</>"cabal.project","--builddir="<>build,"list-bin","exe:"<>name] ""
  let destination = root </> (if name=="sapiens-domain" then ".build/domain" else ".build/bin") </> name<>suffix
  copyFile source (destination<>".installing")
  renameFile (destination<>".installing") destination
 callProcess (root </> ".build/bin/sapiens4"<>suffix) ["build-ui"]
trim :: String -> String
trim = dropWhileEnd (`elem` "\r\n \t") . dropWhile (`elem` "\r\n \t")
buildPath :: FilePath -> IO FilePath
buildPath path = if os=="mingw32" then trim <$> readCreateProcess (shell ("for %I in (\""<>normalise path<>"\") do @echo %~sI")) "" else pure path
