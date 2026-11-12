module DesktopStorage (WorkspaceSignal(..), saveWhenReady, awaitServerAddress, storageMode, preferenceScript, validPreference) where

import Control.Exception (SomeException, try)
import Control.Monad (forM_, guard)
import Data.IORef
import Data.Aeson (Value, encode, decodeStrict)
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import qualified Data.UUID.V4 as UUID
import DataRoot (StorageChoice(..), saveDataDirectory, storageError, storageMessage, startupMessage, openingStorage, openedStorage, startupErrorPrefix)
import Data.List (stripPrefix)
import Network.URI
import System.IO (Handle, hGetLine, hIsEOF)
import System.FilePath
import System.Timeout (timeout)

data WorkspaceSignal = Navigated | WorkspaceReady deriving (Eq)

saveWhenReady :: IORef (Maybe WorkspaceSignal) -> WorkspaceSignal -> IORef (Maybe a)
              -> (a -> FilePath) -> (String -> IO ()) -> (a -> IO ()) -> IO ()
saveWhenReady first signal pending directory recover opened = do
  previous <- readIORef first
  case previous of
    Nothing -> writeIORef first (Just signal)
    Just seen | seen /= signal -> do
      selection <- readIORef pending
      forM_ selection $ \value -> do
        result <- try (saveDataDirectory (directory value))
        case result of
          Left (e :: SomeException) -> recover ("Could not save your storage selection: " ++ storageMessage e)
          Right () -> writeIORef pending Nothing >> opened value
    _ -> pure ()

storageMode :: StorageChoice -> String
storageMode StartNew = "new"
storageMode _ = "existing"

awaitServerAddress :: Handle -> IO URI
awaitServerAddress output = await False
 where
 await opening = do
  let next = hIsEOF output >>= \ended -> if ended then storageError startupMessage else hGetLine output
  line <- if opening then next else timeout 35000000 next >>= maybe (storageError "The application server did not become ready within 35 seconds.") pure
  case stripPrefix startupErrorPrefix line of
   Just encoded -> storageError (maybe startupMessage id (decodeStrict $ TE.encodeUtf8 $ T.pack encoded))
   Nothing | line==openingStorage -> await True
           | line==openedStorage -> await False
           | otherwise -> maybe (await opening) pure (address line)
 address line = do
  uri <- stripPrefix "Sapiens4: " line >>= parseURI
  auth <- uriAuthority uri
  guard (uriScheme uri=="http:" && uriRegName auth=="127.0.0.1" && null (uriUserInfo auth))
  pure uri

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
