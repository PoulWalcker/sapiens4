{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Workspace where

import Control.Exception
import Control.Monad
import Data.Aeson hiding (pairs)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Lazy as BL
import Data.Char
import Data.List (find,nub)
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.URI
import System.Directory
import System.FilePath
import System.Info (os)
import System.IO (IOMode(ReadMode), withBinaryFile)
import Types
import qualified Store as DB

migrateWorkspaces :: Service -> IO ()
migrateWorkspaces s = do
 prefs <- DB.singleton s DB.Preferences
 unless (get prefs "browser_version"==num 1) $ do
  workspaces <- forM (pairs (get prefs "workspaces")) $ \(owner,workspace) -> do
   tabs <- forM (items workspace "tabs") $ \tab -> do
    converted <- if not (T.null (at tab "artifact")) then pure (put "path" (str (T.pack (workspaceRoot s owner </> "artifacts" </> T.unpack (at tab "artifact")))) tab) else
     if at tab "type"=="html" then do
      identity <- newId
      let path=workspaceRoot s owner </> ("saved-tab-"++T.unpack identity++".html")
      writeText path (at tab "html")
      pure (put "path" (str (T.pack path)) tab)
     else pure tab
    identity <- newId
    let url=if not (T.null (at converted "path")) then fileURL (T.unpack (at converted "path")) else if T.null (at converted "url") then "about:blank" else at converted "url"
    pure (merge (remove ["type","artifact","html"] converted) (obj [("url",str url),("zoom",num 1),("command",obj [("seq",str identity),("action",str "navigate")])]))
   pure (owner,merge workspace (obj [("tabs",arr tabs),("bookmarks",get workspace "bookmarks" `fallback` arr [])]))
  DB.saveSingleton s DB.Preferences (merge prefs (obj [("workspaces",obj workspaces),("browser_version",num 1),("workspace_revision",num (int (get prefs "workspace_revision")+1))]))
httpURL :: Text -> Bool
httpURL s = case parseURI (T.unpack s) of
 Just u -> uriScheme u `elem` ["http:","https:"] && maybe False (\a -> not (null (uriRegName a)) && null (uriUserInfo a)) (uriAuthority u)
 _ -> False
expandHome :: FilePath -> IO FilePath
expandHome ('~':'/':rest) = (</> rest) <$> getHomeDirectory
expandHome p = pure p
fileURL :: FilePath -> Text
fileURL p = "file://" <> T.pack (escapeURIString (\c -> isUnreserved c || c `elem` ("/:" :: String)) path)
 where path = if os=="mingw32" then (if take 1 (drop 1 p)==":" then "/" else "") ++ map (\c -> if c=='\\' then '/' else c) p else p
data Destination = LocalFile FilePath | WebLocation Text Text deriving (Eq, Show)
parseDestination :: Value -> Validation Destination
parseDestination d = do
 check (has d "path" /= has d "url") 400 "Supply a file path or a URL"
 t <- validateValue 400 (textFieldP "value" 8192) (obj [("value",get d "path" `fallback` get d "url")])
 let uri=parseURI (T.unpack t)
 if has d "path" || maybe False ((=="file:") . uriScheme) uri then do
  when (not (has d "path")) $ check (maybe False (\u -> maybe True ((`elem` ["","localhost"]) . uriRegName) (uriAuthority u)) uri) 400 "Use an HTTP(S) URL or a local file path"
  let decoded=unEscapeString (maybe "" uriPath uri)
      local=case decoded of '/':drive:':':rest | os=="mingw32" && isAlpha drive -> drive:':':rest; _->decoded
  Right (LocalFile (if has d "path" then T.unpack t else local))
 else if t=="about:blank" then Right (WebLocation t "New tab") else
  check (httpURL t) 400 "Use an HTTP(S) URL or a local file path" *>
   Right (WebLocation t (T.pack (maybe "" (maybe "" uriRegName . uriAuthority) uri)))

destination :: Service -> Text -> Value -> Bool -> IO Value
destination s a d checkFile = validated (parseDestination d) >>= \location -> case location of
 WebLocation address title -> pure (obj [("url",str address),("title",str title)])
 LocalFile local -> do
  p <- expandHome local
  full <- canonicalizePath (if isAbsolute p then p else workspaceRoot s a </> p)
  when checkFile $ doesFileExist full >>= \ok -> require ok 404 "File not found"
  pure (obj [("url",str (fileURL full)),("path",str (T.pack full)),("title",str (T.pack (takeFileName full)))])
workspace :: Service -> Text -> IO Value
workspace s a = do
 p <- DB.singleton s DB.Preferences
 let w = get (get p "workspaces") a
 pure $ obj [("active_tab",get w "activeTab"),("tabs",get w "tabs" `fallback` arr []),("bookmarks",get w "bookmarks" `fallback` arr [])]
persistWorkspace :: Service -> Text -> Value -> Value -> IO ()
persistWorkspace s a w prefs = DB.saveSingleton s DB.Preferences $ merge prefs (obj [("workspaces",put a w (get prefs "workspaces")),("workspace_revision",num (int (get prefs "workspace_revision")+1)),("browser_version",num 1)])
data WorkspaceEdit
 = OpenTab Value Text (Maybe Value) | UnbookmarkTab | CloseTab | FocusTab | ZoomTab Value Value
 | BookmarkTab Text Value | ControlTab BrowserAction Value

workspaceEdit :: Value -> Text -> Value -> Validation WorkspaceEdit
workspaceEdit tab action input = case action of
 "close" -> Right CloseTab
 "focus" -> Right FocusTab
 "zoom" -> let factor=get input "factor" in
  check (case factor of Number n -> n>=0.25 && n<=5; _->False) 400 "Zoom factor must be between 0.25 and 5" *> Right (ZoomTab factor tab)
 "reload" -> Right (ControlTab Reload tab)
 "back" -> Right (ControlTab Back tab)
 "forward" -> Right (ControlTab Forward tab)
 _ -> Left (APIError 400 "Unknown browser action")

editWorkspace :: Text -> Text -> WorkspaceEdit -> Value -> Value
editWorkspace sequence identity edit w = case edit of
 OpenTab dest newIdentity found ->
  let tab=merge (remove ["path"] (fromMaybe (obj [("id",str newIdentity),("zoom",num 1)]) found)) $ merge dest (obj [("error",str ""),("loading",Bool True),("can_back",Bool False),("can_forward",Bool False),("command",command Navigate)])
  in merge w (obj [("activeTab",str newIdentity),("tabs",arr (if isJust found then map (\t -> if at t "id"==newIdentity then tab else t) tabs else tabs++[tab]))])
 UnbookmarkTab -> put "bookmarks" (arr (filter ((/=identity) . (`at` "id")) (items w "bookmarks"))) w
 CloseTab -> let rest=filter ((/=identity) . (`at` "id")) tabs in merge w (obj [("tabs",arr rest),("activeTab",if at w "activeTab"==identity then maybe Null (get `flip` "id") (listToMaybe (reverse rest)) else get w "activeTab")])
 FocusTab -> put "activeTab" (str identity) w
 ZoomTab factor tab -> update (put "zoom" factor tab)
 BookmarkTab bookmarkId tab ->
  let bookmarks=items w "bookmarks"
      existing=find (\b -> get b "url"==get tab "url") bookmarks
      saved=put "tab_id" (str identity) $ fromMaybe (obj (("id",str bookmarkId):[(k,get tab k) | k<-["url","title","path"],has tab k])) existing
  in put "bookmarks" (arr (if isJust existing then map (\b -> if get b "url"==get tab "url" then saved else b) bookmarks else bookmarks++[saved])) w
 ControlTab instruction tab -> update (put "command" (command instruction) tab)
 where
 tabs=items w "tabs"
 command instruction=toJSON (BrowserCommand sequence instruction)
 update value=put "tabs" (arr [if at t "id"==identity then value else t | t<-tabs]) w

workspaceControl :: Service -> Text -> Text -> Value -> IO Value
workspaceControl s a action d = do
 prefs <- DB.singleton s DB.Preferences
 let w=get (get prefs "workspaces") a `fallback` obj [("tabs",arr []),("bookmarks",arr [])]
     tid=text (get d "id" `fallback` get w "activeTab")
     found=if action=="open" && not (has d "id") then Nothing else findId tid (items w "tabs")
 sequence <- newId
 edit <- case action of
  "open" -> do
   dest <- destination s a d True
   when (has d "id") (require (isJust found) 404 "Unknown browser tab")
   identity <- maybe (("tab-" <>) <$> newId) (pure . (`at` "id")) found
   pure (OpenTab dest identity found)
  "unbookmark" -> pure UnbookmarkTab
  _ -> do
   tab <- maybe (failure 404 "Unknown browser tab") pure found
   if action=="bookmark" then (\identity -> BookmarkTab identity tab) <$> newId else validated (workspaceEdit tab action d)
 persistWorkspace s a (editWorkspace sequence tid edit w) prefs
 workspace s a
data BrowserObservation = BrowserObservation
 { observedTitle :: Text, observedError :: Text, observedBack :: Bool
 , observedForward :: Bool, observedLoading :: Bool
 }
-- Native clients historically omit fields; keep their established defaults here.
browserObservation :: Value -> BrowserObservation
browserObservation input = BrowserObservation (at input "title") (T.take 1000 (at input "error"))
 (boolean (get input "can_back")) (boolean (get input "can_forward")) (boolean (get input "loading"))
observeTab :: BrowserObservation -> Value -> Value -> Value
observeTab observation destination tab = merge (remove ["path"] tab) $ merge destination $ obj
 ([("can_back",Bool (observedBack observation)),("can_forward",Bool (observedForward observation))
 ,("loading",Bool (observedLoading observation)),("error",str (observedError observation))]
 ++ [("title",str (T.take 500 (observedTitle observation))) | not (T.null (T.strip (observedTitle observation)))])

workspaceObserved :: Service -> Text -> Value -> IO Value
workspaceObserved s a d = do
 prefs <- DB.singleton s DB.Preferences
 let w = get (get prefs "workspaces") a; tabs = items w "tabs"
 case findId (at d "id") tabs of
  Just tab | get d "seq" == get (get tab "command") "seq" -> do
   dest <- destination s a (obj [("url",get d "url")]) False
   let updated = observeTab (browserObservation d) dest tab
   when (updated /= tab) $ persistWorkspace s a (put "tabs" (arr [if get t "id" == get tab "id" then updated else t | t <- tabs]) w) prefs
   pure (obj [("saved",Bool True)])
  _ -> pure (obj [("saved",Bool False)])
workspaceText :: Service -> Text -> Text -> IO Value
workspaceText s a i = do
 w <- workspace s a
 tab <- maybe (failure 404 "Unknown local file tab") pure (findId i (items w "tabs"))
 require (has tab "path") 404 "Unknown local file tab"
 raw <- withBinaryFile (T.unpack (at tab "path")) ReadMode (`BS.hGet` 2000001) `catch` (\(_ :: IOException) -> failure 404 "File not found or unreadable")
 require (BS.length raw <= 2000000) 413 "This file is too large for the text preview; open it in the desktop app"
 content <- either (const (failure 415 "This file is not UTF-8 text; open it in the desktop app")) pure (TE.decodeUtf8' raw)
 require (not (T.any (== '\0') content)) 415 "This file is not UTF-8 text; open it in the desktop app"
 pure $ obj [("content",str (fromMaybe content (T.stripPrefix "\xfeff" content))),("path",get tab "path")]
createAttachment :: Service -> Text -> Value -> IO Value
createAttachment s a d = do
 let kind = at d "kind"
 require (kind `elem` ["image","document","link","filepath"]) 400 "Choose image, document, link, or filepath"
 i <- newId
 let base = obj [("id",str i),("agent",str a),("kind",str kind)]
 row <- if kind `elem` ["image","document"] then do
  name <- textField d "name" 255
  require (takeFileName (T.unpack name) == T.unpack name && name `notElem` [".",".."] && not (T.any (\c -> c == '\\' || ord c < 32) name)) 400 "Invalid filename"
  require (case get d "data" of String t -> T.length t <= 14000000; _ -> False) 413 "Files must be 10 MB or smaller"
  bytes <- either (const (failure 400 "Invalid file data")) pure (B64.decode (TE.encodeUtf8 (at d "data")))
  require (not (BS.null bytes) && BS.length bytes <= 10*1024*1024) 413 "Files must contain 1 byte to 10 MB"
  when (kind == "image") $ require (any (`BS.isPrefixOf` bytes) ["\x89PNG\r\n\x1a\n","\xff\xd8\xff","GIF87a","GIF89a"] || ("RIFF" `BS.isPrefixOf` bytes && BS.take 4 (BS.drop 8 bytes) == "WEBP")) 400 "Images must be PNG, JPEG, GIF, or WebP"
  let p = serviceRoot s </> "uploads" </> T.unpack a </> T.unpack i </> T.unpack name
  atomicBytes p (BL.fromStrict bytes)
  pure $ merge base (obj [("name",str name),("value",str (T.pack p)),("size",num (BS.length bytes))])
 else do
  value <- textField d "value" 4096
  require (not (T.any ((<32) . ord) value)) 400 "Invalid link or filepath"
  if kind == "link" then require (httpURL value) 400 "Use an http or https link without credentials" >> pure (merge base (obj [("name",str value),("value",str value)])) else do
   p <- expandHome (T.unpack value)
   exists <- doesFileExist p
   require (isAbsolute p && exists) 400 "Use an absolute path to a readable local file"
   full <- canonicalizePath p
   pure $ merge base (obj [("name",str (T.pack (takeFileName full))),("value",str (T.pack full))])
 DB.saveAttachment s row
 pure row
attachmentIds :: Value -> Validation [Text]
attachmentIds ids =
 check (case ids of Array _ -> length xs<=8 && all (\v -> case v of String _->True; _->False) xs && length (nub xs)==length xs; _->False) 400 "Attach up to 8 distinct items" *> Right (map text xs)
 where xs=list ids

resolveAttachments :: Service -> Text -> Value -> Bool -> IO [Value]
resolveAttachments s a ids checkFiles = do
 identities <- validated (attachmentIds ids)
 rows <- DB.attachments s a
 result <- validated (traverse (\i -> maybe (Left (APIError 400 "Unknown attachment for this Sapi")) Right (findId i rows)) identities)
 when checkFiles $ forM_ result $ \row -> when (at row "kind" /= "link") $ doesFileExist (T.unpack (at row "value")) >>= \ok -> require ok 400 ("Attached file is no longer available: " <> at row "name")
 pure result
attachmentPrompt :: Service -> [Value] -> IO Text
attachmentPrompt _ [] = pure ""
attachmentPrompt s rows = do
 p <- prompt s "attachments"
 pure (p <> jsonText (arr [select ["kind","name","value"] row | row <- rows]))
