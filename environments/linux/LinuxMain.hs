{-# LANGUAGE ForeignFunctionInterface #-}
module Main where

import Control.Concurrent (runInBoundThread)
import Control.Exception (bracket)
import Control.Monad (unless, when)
import Foreign.C
import HostServer
import System.Directory
import System.Environment
import System.Exit (exitSuccess)
import System.FilePath
import System.IO
import System.Posix.Process (executeFile, getProcessID)

foreign import ccall safe "sapiens_linux_available" available :: IO CInt
foreign import ccall safe "sapiens_linux_run" desktop :: CString -> CString -> IO CInt

main :: IO ()
main = runInBoundThread $ do
 hSetBuffering stdout LineBuffering
 args <- getArgs
 case args of
  command:_ | command `elem` ["control", "computer"] -> do
   binary <- getExecutablePath
   executeFile (takeDirectory binary </> "sapiens4") False args Nothing
  _ -> pure ()
 let usage = "Usage: sapiens-linux [--data-dir DIRECTORY]"
 when (args == ["--help"]) (putStrLn usage >> exitSuccess)
 directory <- case args of
  ["--data-dir", path] -> pure path
  [] -> lookupEnv "SAPIENS_DATA_DIR" >>= maybe (getXdgDirectory XdgData "sapiens4") pure
  _ -> ioError (userError usage)
 display <- available
 unless (display /= 0) $ ioError (userError "A graphical Linux session is required")
 root <- lookupEnv "SAPIENS_REPOSITORY" >>= maybe (takeDirectory . takeDirectory . takeDirectory <$> getExecutablePath) pure
 setEnv "SAPIENS_REPOSITORY" root
 setEnv "SAPIENS_LINUX_BACKEND" "native"
 getProcessID >>= setEnv "SAPIENS_DESKTOP_PID" . show
 setEnv "SAPIENS_BLINDLY_BINARY" (root </> "submodules/blindly4/.build/release/blindly4")
 bracket (startHost directory 0 False) hostStop $ \host -> do
  let url = "http://127.0.0.1:" ++ show (hostPort host) ++ "/workspace/"
  status <- withCString url $ \address -> withCString directory (desktop address)
  unless (status == 0) $ ioError (userError "Linux desktop could not open the workspace")
