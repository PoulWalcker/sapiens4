module DesktopStorage (checkStorage, storageMode, preferenceScript, validPreference) where

import Control.Monad (unless)
import Data.Aeson (Value, encode)
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import qualified Data.UUID.V4 as UUID
import DataRoot (StorageChoice(..), storageError)
import System.Exit (ExitCode(..))
import System.FilePath
import System.Info (os)
import System.Process
import System.Timeout (timeout)

storageMode :: StorageChoice -> String
storageMode StartNew = "new"
storageMode _ = "existing"

checkStorage :: FilePath -> FilePath -> StorageChoice -> IO ()
checkStorage runtime directory choice = do
  let windows = os == "mingw32"
      binary = runtime </> ".build/bin" </> if windows then "sapiens4.exe" else "sapiens4"
      command = (proc binary ["check-data", directory, storageMode choice])
        {cwd = Just runtime, detach_console = windows, use_process_jobs = windows}
  result <- timeout 35000000 (readCreateProcessWithExitCode command "")
  case result of
    Nothing -> storageError "Storage validation did not finish within 35 seconds."
    Just (code, _, message) -> unless (code == ExitSuccess) $
      storageError (if null message then "This data directory could not be opened." else message)

validPreference :: Text -> Text -> Bool
validPreference key value =
  (key == "sapi-color-theme" || key == "s4.active" || "s4.workspace." `T.isPrefixOf` key)
    && T.length key <= 128 && T.length value <= 1000000

preferenceScript :: FilePath -> Text -> FilePath -> Value -> IO Text
preferenceScript runtime address directory saved = do
  source <- TIO.readFile (runtime </> "app/shell/client-preferences.js")
  token <- UUID.nextRandom
  let arguments = TE.decodeUtf8 (BL.toStrict (encode (address, directory, saved, token)))
  pure ("(" <> source <> ")(..." <> arguments <> ");")
