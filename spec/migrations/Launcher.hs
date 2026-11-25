{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where
import Control.Exception
import Control.Monad
import Data.Aeson
import qualified Data.ByteString.Lazy as L
import DataRoot
import System.Directory
import System.Environment
import System.FilePath

main :: IO ()
main = do
 [supplied] <- getArgs
 root <- canonicalizePath supplied
 let config = root </> "config"
     old = root </> "workspace"
     other = root </> "other"
     source = root </> "config.json"
 createDirectoryIfMissing True config
 setEnv "SAPIENS_CONFIG_DIR" config
 forM_ [old,other] $ \dir -> do
  createDirectory dir
  L.writeFile (dir </> "corpora-identity.json") (encode $ object ["corpora_id" .= ("00000000-0000-4000-8000-000000000001" :: String)])
  writeFile (dir </> "corpora.sqlite3") "fixture"
 let legacy path = L.writeFile source (encode $ object ["data" .= path])
     check label expected action = action >>= \actual -> unless (actual==expected) (fail label)
 legacy old
 before <- L.readFile source
 check "legacy discovery" (Just old) (startupDataDirectory Nothing)
 check "retry" (Just old) (startupDataDirectory Nothing)
 check "source retained" before (L.readFile source)
 check "no premature publication" False (doesFileExist $ config </> "storage.json")
 check "explicit override" (Just other) (startupDataDirectory $ Just other)
 saveDataDirectory other
 check "modern override" (Just other) (startupDataDirectory Nothing)
 removeFile (config </> "storage.json")
 legacy (root </> "missing")
 result <- try (startupDataDirectory Nothing) :: IO (Either IOException (Maybe FilePath))
 case result of Left _ -> pure (); Right _ -> fail "Missing legacy directory silently ignored"
 writeFile source "invalid"
 invalid <- try (startupDataDirectory Nothing) :: IO (Either IOException (Maybe FilePath))
 case invalid of Left _ -> pure (); Right _ -> fail "Invalid config silently ignored"
 putStrLn "Launcher discovery: 8 checks passed"
