module Main where

import Control.Monad
import Options
import System.Directory
import System.Environment
import System.FilePath
import System.Info
import System.Process

main :: IO ()
main = do
 args <- getArgs
 root <- getCurrentDirectory
 case args of
  "macos":rest | "--help" `elem` rest || "-h" `elem` rest -> putStrLn macUsage
  "macos":rest -> do
   options <- parseMac rest
   setEnv "MACOSX_DEPLOYMENT_TARGET" "13.0"
   when (buildMode options /= PackageOnly) $ do
    callProcess "runghc" [root</>"build/Build.hs"]
    callProcess (root</>"submodules/blindly4/macos/build.sh") []
   when (buildMode options /= BuildOnly) $ tool root args
  _ -> tool root args

tool :: FilePath -> [String] -> IO ()
tool root args = do
 let project = root</>"submodules/libs4/haskell"
     folder = root</>".build/package-tool"
     executable = folder</>"sapiens-package"++if os=="mingw32" then ".exe" else ""
 createDirectoryIfMissing True folder
 setEnv "CABAL_DIR" (project</>".cabal")
 -- GHC --make reuses the tool's own objects; never rebuild the application here.
 callProcess "cabal" ["--project-file="++project</>"cabal.project","--builddir="++project</>".build","exec","--","ghc","-v0","-O0","-ibuild/package","-outputdir",folder,"-o",executable,"build/package/Main.hs"]
 callProcess executable args
