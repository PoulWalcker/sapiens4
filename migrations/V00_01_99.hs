-- Frozen reader for the SQLite/JSON layout on dev at 7ae4825 (v00.01.99).
-- Only this migration reader depends on SQLite. Never open customer SQLite
-- files for writing: copy the database AND recovery journals before recovery.
module V00_01_99 where
import Control.Exception
import Control.Monad
import Data.Aeson
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as L
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import Database.SQLite.Simple hiding (field)
import System.Directory
import System.FilePath
import RuntimePaths (containedPath)

type Row = [(T.Text, Value)]
field :: T.Text -> Value -> Value
field key (Object fields) = maybe Null id (KM.lookup (K.fromText key) fields)
field _ _ = Null
textField :: T.Text -> Value -> IO T.Text
textField key value = case field key value of
 String t | not (T.null t) -> pure t
 _ -> fail ("Missing text field: " ++ T.unpack key)
json :: T.Text -> IO Value
json = either fail pure . eitherDecodeStrict . TE.encodeUtf8
readJSON :: FilePath -> IO Value
readJSON path = L.readFile path >>= either (fail . ((path ++ ": ") ++)) pure . eitherDecode
safePath :: FilePath -> FilePath -> IO FilePath
safePath root relative = containedPath root (root </> relative)

withDatabase :: FilePath -> (Connection -> IO a) -> IO a
withDatabase source action = do
 temporary <- getTemporaryDirectory
 nonce <- UUID.toString <$> UUID.nextRandom
 let folder = temporary </> ("sapiens-migration-" ++ nonce)
 bracket_ (createDirectory folder >> setPermissions folder emptyPermissions {readable=True,writable=True,searchable=True})
  (removePathForcibly folder) $ do
   let copy = folder </> "source.sqlite3"
   forM_ ["","-wal","-journal"] $ \suffix -> do
    let path = source ++ suffix
    void (containedPath (takeDirectory source) path)
    exists <- doesFileExist path
    when exists (copyFile path (copy ++ suffix))
   bracket (open copy) close $ \db -> do
    integrity <- query_ db "PRAGMA quick_check" :: IO [Only T.Text]
    unless (integrity == [Only "ok"]) $ fail "Legacy SQLite integrity check failed"
    execute_ db "PRAGMA query_only=ON"
    action db

-- SQL identifiers below come exclusively from the frozen migration definitions.
rows :: Connection -> String -> IO [Value]
rows db table = do
 columns <- query_ db (fromString ("PRAGMA table_info(" ++ table ++ ")")) :: IO [(Int,T.Text,T.Text,Int,Maybe T.Text,Int)]
 when (null columns) $ fail ("Missing legacy table: " ++ table)
 values <- query_ db (fromString ("SELECT * FROM " ++ table ++ " ORDER BY rowid")) :: IO [[SQLData]]
 forM values $ \values' -> do
  converted <- mapM value values'
  pure (object (zipWith (\(_,name,_,_,_,_) v -> K.fromText name .= v) columns converted))
 where
 value SQLNull = pure Null
 value (SQLText t) = pure (String t)
 value (SQLInteger n) = pure (toJSON n)
 value (SQLFloat n) = pure (toJSON n)
 value (SQLBlob _) = fail ("Unexpected binary value in legacy table " ++ table)

fromString :: String -> Query
fromString = Query . T.pack
