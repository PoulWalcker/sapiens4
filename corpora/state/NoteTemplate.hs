module NoteTemplate (renderMemo) where
import Data.Aeson (encode, Value(String))
import qualified Data.ByteString.Lazy as L
import qualified Data.Text.Encoding as TE
import Data.Text (Text)
import qualified Data.Text as T
import Data.Char
import Data.Maybe
str :: Text -> Value
str = String
jsonText :: Value -> Text
jsonText = TE.decodeUtf8 . L.toStrict . encode
escape :: Text -> Text
escape = T.concatMap (\c -> fromMaybe (T.singleton c) (lookup c [('&',"&amp;"),('<',"&lt;"),('>',"&gt;"),('"',"&quot;"),('\'',"&#x27;")]))
renderMemo :: Text -> Text -> Text -> Text
renderMemo name purpose content =
 ("<section id=\"about\"><pre><code class=\"language-yaml\">" <> escape about <> "</code></pre></section>\n<section id=\"map\"><nav>" <> links <> "</nav></section>\n<section id=\"content\">" <> snippets <> "</section>\n")
 where
 about = "who: " <> jsonText (str name) <> "\npurpose: " <> jsonText (str purpose)
 chunks = foldl (\acc line -> if not (T.null line) && (isAlpha (T.head line) || T.head line == '_') && T.any (== ':') line then acc ++ [line] else case reverse acc of [] -> [line]; x:xs -> reverse xs ++ [x <> "\n" <> line]) [] (T.lines content)
 values = filter (not . T.null . T.strip) chunks
 title x = let t = T.takeWhile (\c -> isAlphaNum c || c `elem` ['_','-']) x in if T.isPrefixOf ":" (T.drop (T.length t) x) then t else "remembered"
 links = T.concat ["<a href=\"#entry-" <> T.pack (show i) <> "\">" <> escape (title x) <> "</a> — saved " <> escape (title x) <> "<br>" | (i,x) <- zip [0::Int ..] values]
 snippets = T.concat ["<article id=\"entry-" <> T.pack (show i) <> "\"><pre><code class=\"language-yaml\">" <> escape x <> "</code></pre></article>\n" | (i,x) <- zip [0::Int ..] values]
