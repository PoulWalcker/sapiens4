{-# LANGUAGE OverloadedStrings #-}
-- Frozen v00.01.99 launcher reader. Discovery is read-only; DataRoot publishes
-- the modern selection only after the migrated workspace is ready.
module V00_02_00Launcher (previousLauncherDirectory) where

import Data.Aeson
import qualified Data.Aeson.KeyMap as K
import qualified Data.Text as T
import System.Directory
import System.FilePath

previousLauncherDirectory :: FilePath -> IO (Maybe FilePath)
previousLauncherDirectory config = do
 let source = takeDirectory config </> "config.json"
 exists <- doesFileExist source
 if takeFileName config /= "config" || not exists then pure Nothing else do
  value <- either (ioError . userError . ("Could not read previous launcher settings: " ++)) pure =<< eitherDecodeFileStrict source
  case value of
   Object fields | Just (String directory) <- K.lookup "data" fields,
                   isAbsolute (T.unpack directory) -> pure (Just (T.unpack directory))
   _ -> ioError (userError "Previous launcher data location is invalid. Choose your existing workspace folder.")
