module Main where

import Control.Exception
import Control.Monad
import Data.List
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.Info
import System.Process

main :: IO ()
main = do
 args <- getArgs
 root <- getCurrentDirectory >>= buildPath
 -- Source releases omit test helpers; development checkouts build the probes.
 helpers <- filterM (doesFileExist . (root </>) . snd)
  ([("sapiens-peer","spec/support/Peer.hs")] ++
   if os=="mingw32" then [("sapiens-windows-probe","spec/support/WindowsProbeMain.hs"),("sapiens-provider","spec/support/WindowsProviderMain.hs")] else [])
 let project = root </> "submodules/libs4/haskell"
     cache = project </> ".cabal"
     build = project </> ".build"
     suffix = if os=="mingw32" then ".exe" else ""
     desktop = os=="linux" && "--desktop" `elem` args
     buildFlags = ["--flags=" ++ if desktop then "linux-desktop" else "-linux-desktop"]
     targets = ["sapiens4","sapiens-domain"] ++ map fst helpers ++ if os=="darwin" then ["sapiens-macos"] else if os=="mingw32" then ["sapiens-windows"] else ["sapiens-linux" | desktop]
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
   callProcess "make" ["-j4"]
 when (os=="mingw32") $ do
  createDirectoryIfMissing True build
  callProcess "windres" ["-I",root</>"environments/windows",root</>"environments/windows/resources.rc","-O","coff","-o",build</>"windows-resources.o"]
 let cabal options = callProcess "cabal" (["--project-file="<>project</>"cabal.project","--builddir="<>build]++options)
 cabal (["build"]++buildFlags++map ("exe:"++) (filter (/="sapiens-windows") targets))
 -- The desktop resource already contains the application's DPI/UAC manifest.
 when (os=="mingw32") $ cabal (["build","exe:sapiens-windows","--ghc-options=-fno-gen-manifest"]++buildFlags)
 createDirectoryIfMissing True (root </> ".build/bin")
 createDirectoryIfMissing True (root </> ".build/domain")
 forM_ targets $ \name -> do
  source <- trim <$> readProcess "cabal" (["--project-file="<>project</>"cabal.project","--builddir="<>build,"list-bin","exe:"<>name]++buildFlags) ""
  let destination = root </> (if name=="sapiens-domain" then ".build/domain" else ".build/bin") </> name<>suffix
  copyFile source (destination<>".installing")
  when (os=="darwin") (packageMac root (destination<>".installing"))
  renameFile (destination<>".installing") destination
 callProcess (root </> ".build/bin/sapiens4"<>suffix) ["build-ui"]
 when (os=="mingw32") $ do
  let runtime = case arch of "x86_64" -> "win-x64"; "aarch64" -> "win-arm64"; _ -> error ("Unsupported Windows architecture: "<>arch)
  -- Windows PowerShell must not inherit PowerShell 7's incompatible modules.
  unsetEnv "PSModulePath"
  callProcess "powershell" ["-NoProfile","-ExecutionPolicy","Bypass","-File",root</>"submodules/libs4/windows/webview2.ps1","-Runtime",runtime,"-Destination",root</>".build/bin"]
  callProcess "node" [root</>"submodules/libs4/tools/svg-to-ico.cjs",root</>"app/shell/favicon.svg",root</>"environments/windows/AppIcon.ico"]
  callProcess "node" [root</>"submodules/libs4/tools/svg-to-ico.cjs",root</>"app/shell/favicon-dark.svg",root</>"environments/windows/AppIconDark.ico"]
trim :: String -> String
trim = dropWhileEnd (`elem` "\r\n \t") . dropWhile (`elem` "\r\n \t")
buildPath :: FilePath -> IO FilePath
buildPath path = if os=="mingw32" then trim <$> readCreateProcess (shell ("for %I in (\""<>normalise path<>"\") do @echo %~sI")) "" else pure path
packageMac :: FilePath -> FilePath -> IO ()
packageMac root binary = do
 let directory = root </> "submodules/libs4/haskell/.runtime"
 createDirectoryIfMissing True directory
 output <- readProcess "otool" ["-L",binary] ""
 forM_ (drop 1 (lines output)) $ \line -> case words line of
  dependency:_ | "/" `isPrefixOf` dependency && not ("/usr/lib/" `isPrefixOf` dependency || "/System/Library/" `isPrefixOf` dependency) -> do
   let target = directory </> takeFileName dependency
   copyFile dependency (target<>".installing")
   renameFile (target<>".installing") target
   callProcess "install_name_tool" ["-change",dependency,"@loader_path/../../submodules/libs4/haskell/.runtime/"<>takeFileName dependency,binary]
  _ -> pure ()
 callProcess "codesign" ["--force","--sign","-",binary]
