module Main where
import System.Environment
import System.Exit
import System.Process
main :: IO ()
main = do
  ruby <- getEnv "SAPIENS_CONTRACT_RUBY"
  provider <- getEnv "SAPIENS_CONTRACT_PROVIDER"
  getExecutablePath >>= setEnv "SAPIENS_CONTRACT_EXECUTABLE"
  args <- getArgs
  (_, _, _, child) <- createProcess (proc ruby $ provider : args) {detach_console = True, use_process_jobs = True}
  waitForProcess child >>= exitWith
