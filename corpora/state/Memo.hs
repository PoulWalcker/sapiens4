{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Memo where

import Crypto.Hash.SHA256 (hash)
import Numeric (showHex)
import Control.Exception
import Control.Monad
import Data.Aeson hiding (pairs)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Char
import Data.Maybe
import qualified Data.Map.Strict as M
import Text.HTML.TagSoup (Tag(..),parseTags,renderTags)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import System.Directory
import System.FilePath
import Types

escape :: Text -> Text
escape = T.concatMap (\c -> fromMaybe (T.singleton c) (lookup c [('&',"&amp;"),('<',"&lt;"),('>',"&gt;"),('"',"&quot;"),('\'',"&#x27;")]))
validate :: FilePath -> FilePath -> IO ()
validate root p = do
 link <- pathIsSymbolicLink p `catch` (\(_ :: IOException) -> pure False)
 resolved <- canonicalizePath p
 exists <- doesPathExist p
 file <- doesFileExist p
 require (not link && takeDirectory resolved == root && (not exists || file)) 409 (T.pack (takeFileName p) <> " must be a regular file inside this Sapi workspace")
ensureMemo :: FilePath -> Text -> Text -> IO ()
ensureMemo root name purpose = do
 let p = root </> "Notes.html"
 exists <- doesPathExist p
 link <- pathIsSymbolicLink p `catch` (\(_ :: IOException) -> pure False)
 unless (exists || link) $ do
  createDirectoryIfMissing True root
  sources <- filterM doesPathExist [root </> "Notes.yaml",root </> "Notes.md"]
  content <- case listToMaybe sources of
   Nothing -> pure "notes: []"
   Just source -> do
    validate root source
    old <- readText source
    pure (if takeExtension source == ".md" then "notes: " <> jsonText (str old) else old)
  writeText p (migratedMemo name purpose content)

migratedMemo :: Text -> Text -> Text -> Text
migratedMemo name purpose content =
 ("<section id=\"about\"><pre><code class=\"language-yaml\">" <> escape about <> "</code></pre></section>\n<section id=\"map\"><nav>" <> links <> "</nav></section>\n<section id=\"content\">" <> snippets <> "</section>\n")
 where
 about = "who: " <> jsonText (str name) <> "\npurpose: " <> jsonText (str purpose)
 chunks = foldl (\acc line -> if not (T.null line) && (isAlpha (T.head line) || T.head line == '_') && T.any (== ':') line then acc ++ [line] else case reverse acc of [] -> [line]; x:xs -> reverse xs ++ [x <> "\n" <> line]) [] (T.lines content)
 values = filter (not . T.null . T.strip) chunks
 title x = let t = T.takeWhile (\c -> isAlphaNum c || c `elem` ['_','-']) x in if T.isPrefixOf ":" (T.drop (T.length t) x) then t else "remembered"
 links = T.concat ["<a href=\"#entry-" <> T.pack (show i) <> "\">" <> escape (title x) <> "</a> — saved " <> escape (title x) <> "<br>" | (i,x) <- zip [0::Int ..] values]
 snippets = T.concat ["<article id=\"entry-" <> T.pack (show i) <> "\"><pre><code class=\"language-yaml\">" <> escape x <> "</code></pre></article>\n" | (i,x) <- zip [0::Int ..] values]
memoMetadata :: FilePath -> IO Value
memoMetadata root = work `catch` (\(APIError _ e) -> pure (obj [("path",str (T.pack p)),("error",str e),("revision",str "invalid")]))
 where
 p = root </> "Notes.html"
 work = do
  validate root p
  exists <- doesFileExist p
  modified <- if exists then Just <$> getModificationTime p else pure Nothing
  size <- if exists then getFileSize p else pure 0
  let historyPath=root </> "Notes.history.json"
  historyExists <- doesFileExist historyPath
  historyLink <- pathIsSymbolicLink historyPath `catch` (\(_::IOException) -> pure False)
  history <- if exists && historyExists && not historyLink then readJSON historyPath empty `catch` (\(_::IOException) -> pure empty) else pure empty
  digest <- if exists && has history "sha256" then T.pack . concatMap (\b -> let value=showHex b "" in if length value==1 then '0':value else value) . BS.unpack . hash <$> BS.readFile p else pure ""
  let historyText field=case get history field of String t->str t; _->Null
  pure $ obj [("path",str (T.pack p)),("workspace",str (T.pack root)),("revision",str (maybe "missing" (\t -> timestamp t <> ":" <> T.pack (show size)) modified)),("modified_at",maybe Null (str . timestamp) modified),("created_at",historyText "created_at"),("created_by",historyText "created_by"),("modified_by",if exists && has history "sha256" && at history "sha256"==digest then historyText "modified_by" else Null)]
readMemo :: FilePath -> IO Value
readMemo root = do
 let p = root </> "Notes.html"
 validate root p
 exists <- doesFileExist p
 require exists 409 "Notes.html is missing; check the original Notes file before recreating it."
 raw <- BS.readFile p
 content <- either (const (failure 409 "Notes.html must contain UTF-8 text")) pure (TE.decodeUtf8' raw)
 metadata <- memoMetadata root
 pure $ obj [("path",str (T.pack p)),("content",str content),("metadata",metadata)]
memoContext :: FilePath -> IO Value
memoContext root = work `catch` (\(APIError _ e) -> pure (obj [("path",str (T.pack (root </> "Notes.html"))),("error",str e)]))
 where
 work = do
  memo <- readMemo root
  validated (projectMemo memo)

projectMemo :: Value -> Validation Value
projectMemo memo = do
 let append capture value sections=case capture of Just name | name `elem` ["about","map"]->M.insertWith (flip (<>)) name value sections; _->sections
     step (sections,capture,order) tag=case tag of
      TagOpen name attributes ->
       let section=T.toLower name=="section"
           identity=lookup "id" [(T.toLower key,value) | (key,value)<-attributes]
           next=if section && identity `elem` map Just ["about","map","content"] then identity else capture
           saved=if section && identity `elem` map Just ["about","map","content"] then maybe sections (\key -> M.insert key "" sections) identity else sections
       in (append next (renderTags [tag]) saved,next,if section then order++[identity] else order)
      TagClose name -> (append capture (renderTags [TagClose (T.toLower name)]) sections,if T.toLower name=="section" then Nothing else capture,order)
      TagText value -> (append capture (escape value) sections,capture,order)
      _ -> (sections,capture,order)
     (sections,_,order)=foldl step (M.empty,Nothing,[]) (parseTags (at memo "content") :: [Tag Text])
 check (order==map Just ["about","map","content"]) 409 "Notes.html requires sections with ids about, map, and content."
 pure $ obj (("path",get memo "path"):[(key,if T.length value<=12000 then str value else Null) | (key,value)<-M.toList sections,key/="content"])
memoImage :: FilePath -> Text -> IO (BL.ByteString,Text)
memoImage root name = do
 p <- canonicalizePath (root </> T.unpack name)
 require (not (isAbsolute (T.unpack name)) && (addTrailingPathSeparator root) `isPrefix` p) 409 "Path escapes its store"
 let mime = lookup (map toLower (takeExtension p)) [(".png","image/png"),(".jpg","image/jpeg"),(".jpeg","image/jpeg"),(".gif","image/gif"),(".webp","image/webp")]
 exists <- doesFileExist p
 size <- if exists then getFileSize p else pure 0
 require (exists && size <= 10000000 && isJust mime) 404 "Notes image not found or too large"
 contentType <- maybe (failure 404 "Notes image not found or too large") pure mime
 bytes <- BL.readFile p
 pure (bytes,contentType)
 where isPrefix a b = take (length a) b == a
