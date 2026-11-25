-- v00.01.99 -> v00.02.00. All layout conversion for this release lives here.
-- Sources are retained verbatim; no customer file is removed or overwritten.
module V00_02_00 where
import MigrationVersion (AppVersion(..))
import qualified Append4 as A
import Control.Exception
import Control.Monad
import NoteTemplate (renderMemo)
import qualified Data.Text.IO as TIO
import System.IO
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.Text.Encoding as TE
import qualified Crypto.Hash.SHA256 as SHA
import Data.Aeson
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.List (sortOn)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Database.SQLite.Simple (Only(..), query_)
import System.Directory
import System.FilePath
import V00_01_99

sourceVersion, targetVersion :: AppVersion
sourceVersion = AppVersion "v00.01.99"
targetVersion = AppVersion "v00.02.00"

put :: T.Text -> Value -> Value -> Value
put key value (Object fields) = Object (KM.insert (K.fromText key) value fields)
put key value _ = object [K.fromText key .= value]
merge :: Value -> Value -> Value
merge (Object a) (Object b) = Object (b <> a)
merge a _ = a
numberText :: Value -> IO T.Text
numberText value = case fromJSON value :: Result Integer of
 Success n -> pure (T.pack (show n))
 _ -> fail "Invalid legacy sequence"
document :: Value -> IO Value
document value = textField "value" value >>= json
optional :: FilePath -> FilePath -> (Value -> IO ()) -> IO ()
optional root relative action = do
 path <- safePath root relative
 exists <- doesFileExist path
 when exists (readJSON path >>= action)

corpora :: FilePath -> A.Store -> IO ()
corpora root store = withDatabase (root </> "corpora.sqlite3") $ \db -> do
 versions <- query_ db "PRAGMA user_version" :: IO [Only Int]
 unless (versions == [Only 8]) $ fail "Expected v00.01.99 Corpora SQLite schema 8; originals retained"
 let save = A.putValue store
     keyed table prefix = rows db table >>= mapM_ (\v -> textField "id" v >>= \key -> save (prefix<>key) v)
 agents <- rows db "agents"
 when (null agents) $ fail "Legacy Corpora has no agents"
 forM_ agents $ \agent -> do
  owner <- textField "id" agent
  statePath <- safePath root ("agentpy/agents" </> T.unpack owner </> "state.json")
  state <- readJSON statePath
  unless (field "schema_version" state == Number 3 && field "agid" state == String owner) $
   fail "Expected v00.01.99 agent state schema 3 with matching identity"
  migrateNotes root owner agent
  save ("agents/"<>owner) agent
  save ("states/"<>owner) state
  optional root ("agentpy/agents" </> T.unpack owner </> "host.json") (save ("settings/"<>owner))
 keyed "turns" "turns/"
 forM_ [("workloads","Workloads/"),("groups","Groups/")] $ \(table,prefix) ->
  rows db table >>= mapM_ (\row -> do key <- textField "id" row; document row >>= save (prefix<>key))
 forM_ [("preferences","Preferences/singleton"),("system_pulse","SystemPulse/singleton")] $ \(table,key) -> do
  saved <- rows db table
  when (length saved>1) $ fail ("Multiple singleton rows in " ++ table)
  forM_ saved (\row -> document row >>= save key)
 forM_ [("pulse_calls","PulseCalls/"),("scheduled_routines","ScheduledRoutines/")] $ \(table,prefix) ->
  rows db table >>= mapM_ (\row -> do
   value <- document row
   sequenceId <- numberText (field "sequence" row)
   let key = if table=="scheduled_routines" then "routine_sch_"<>T.justifyRight 5 '0' sequenceId else "legacy-"<>T.justifyRight 20 '0' sequenceId
   save (prefix<>key) (if table=="scheduled_routines" then put "id" (String key) value else value))
 forM_ ["attachments","group_attachments"] $ \table -> rows db table >>= mapM_ (\row -> do
  owner <- textField "agent" row
  key <- textField "id" row
  value <- document row
  save ("attachments/"<>owner<>"/"<>key) value)
 rows db "message_inputs" >>= mapM_ (\row -> do
  key <- textField "job" row
  attachments <- textField "attachments" row >>= json
  save ("messages/"<>key) (object ["input" .= field "text" row,"attachments" .= attachments]))
 rows db "turn_details" >>= mapM_ (\row -> do
  key <- textField "id" row
  details <- document row
  old <- A.getValue store ("turns/"<>key)
  case old of
   Just value -> save ("turns/"<>key) (merge value details)
   Nothing -> save ("migration/v00.01.99/orphan-turn-details/"<>key) details)
 events <- rows db "activity_events"
 decodedEvents <- forM events $ \row -> do
  owner <- textField "owner" row
  details <- textField "details" row >>= json
  pure (owner,put "details" details row)
 forM_ (M.toList (M.fromListWith (++) [(owner,[v]) | (owner,v)<-decodedEvents])) $ \(owner,values) ->
  save ("events/"<>owner) (toJSON (reverse (sortOn (integer . field "id") values)))
 forM_ [("corpora-settings.json","corpora-settings"),("model-settings.json","model-settings"),
        ("agentpy/corpora/directory.json","directory"),("host-corpora.json","local/registry"),
        ("remote-device.json","local/device"),("remote-relay.json","local/relay")] $ \(path,key) -> optional root path (save key)
 walkJSON root "agentpy/corpora/archive" (const True) $ \relative value ->
  save ("audit/"<>portable (dropExtension relative)) value
 walkJSON root "uploads" ((=="progress.json") . takeFileName) $ \relative value -> case splitDirectories relative of
  [owner,"voice",recording,"progress.json"] -> save ("voice/"<>T.pack owner<>"/"<>T.pack recording) value
  _ -> pure ()
 receiptsPath <- safePath root "remote-receipts.sqlite3"
 hasReceipts <- doesFileExist receiptsPath
 when hasReceipts $ withDatabase receiptsPath $ \receipts -> rows receipts "receipts" >>= mapM_ (\row -> do
  room <- textField "room" row
  ident <- textField "id" row
  digest <- textField "hash" row
  reply <- case field "reply" row of Null -> pure Null; String raw -> json raw; _ -> fail "Invalid receipt reply"
  -- Keep the original hash version. Compatibility checks live in migrations.
  save ("receipts/"<>room<>"/"<>ident) (object ["hash" .= digest,"reply" .= reply,"legacy" .= True,"created" .= field "created" row]))
 normalizePreferences store
 normalizeRoutines store
 normalizeRelays root store
 normalizeAvatars store

-- This is an app-version transformation, not a startup-time repair.
normalizePreferences :: A.Store -> IO ()
normalizePreferences store = do
 saved <- A.getValue store "Preferences/singleton"
 forM_ saved $ \prefs -> do
  let panel = field "panel" prefs
      next = if panel==String "mindmap" then String "notes" else if panel `elem` map String ["cron","log"] then String "chat" else panel
  A.putValue store "Preferences/singleton" (put "panel" next prefs)
  -- Preserve obsolete fields as well: removing them is unnecessary for readers.

integer :: Value -> Integer
integer v = case fromJSON v of Success n -> n; Error _ -> 0
portable :: FilePath -> T.Text
portable = T.intercalate "/" . map T.pack . splitDirectories
walkJSON :: FilePath -> FilePath -> (FilePath -> Bool) -> (FilePath -> Value -> IO ()) -> IO ()
walkJSON root base accept action = do
 path <- safePath root base
 exists <- doesDirectoryExist path
 when exists (walk "")
 where
 walk relative = do
  path <- safePath root (base </> relative)
  directory <- doesDirectoryExist path
  if directory then listDirectory path >>= mapM_ (walk . (relative </>))
  else when (takeExtension path==".json" && accept relative) (readJSON path >>= action relative)

relay :: FilePath -> A.Store -> IO ()
relay source store = withDatabase source $ \db -> do
 let save = A.putValue store
     putRow kind ident value = do
      let key=kind<>"/"<>ident
      save key value
      when (kind `elem` ["rooms","messages"]) $ case field "expires" value of
       Number n | n>0 -> save ("expiry/"<>ordered (ceiling (n*1000))<>"/"<>key) (String key)
       _ -> pure ()
 rooms <- rows db "rooms"
 forM_ rooms $ \value -> textField "id" value >>= \ident -> putRow "rooms" ident value
 messages <- rows db "messages"
 forM_ messages $ \value -> do
  room <- textField "room" value
  frame <- textField "body" value >>= json
  putRow "messages" (room<>"/"<>ordered (integer (field "id" value))) (put "frame" frame value)
 -- AUTOINCREMENT may be higher than every surviving message after ACKs.
 sequences <- query_ db "SELECT seq FROM sqlite_sequence WHERE name='messages'" :: IO [Only Integer]
 save "sequence" (toJSON (maximum (0 : map fromOnly sequences ++ map (integer . field "id") messages)))
 rows db "corpora" >>= mapM_ (\value -> textField "address" value >>= \address -> save ("corpora/"<>address) value)
 rows db "beta_requests" >>= save "beta" . toJSON . map (field "created")
 where ordered = T.justifyRight 20 '0' . T.pack . show

normalizeRoutines :: A.Store -> IO ()
normalizeRoutines store = do
 routines <- A.values store "ScheduledRoutines/"
 works <- map snd <$> A.values store "Workloads/"
 groups <- map snd <$> A.values store "Groups/"
 forM_ routines $ \(key,r) -> when (field "runCount" r==Null) $ do
  let owner=field "owner" r
      matches=filter ((==owner) . field "id") groups
      tasks=case matches of
       g:_ -> case fromJSON (field "tasks" g) of Success xs -> xs; Error _ -> []
       [] -> []
      count=if null matches then length [w | w<-works,field "routine" (field "origin" w)==field "id" r]
            else length [t | t<-tasks,field "routine" t==field "id" r]
  A.putValue store key (put "runCount" (toJSON count) (put "paused" (Bool (count>=1000)) r))

normalizeRelays :: FilePath -> A.Store -> IO ()
normalizeRelays root store = do
 saved <- maybe (object []) id <$> A.getValue store "local/registry"
 when (field "relay_servers" saved==Null) $ do
  let builtin ident name url = object ["id" .= (ident::T.Text),"name" .= (name::T.Text),"address" .= (url<>"/beta"),"available" .= True,"archived" .= False,"builtin" .= True,"connection" .= object ["format" .= ("sapiens-relay-connection-v1"::T.Text),"relay" .= (url::T.Text),"beta" .= True]]
      initial=[builtin "sapiens-beta" "Sapiens4 beta" "https://remote-relay.sapiens4.ai",builtin "utana-beta" "Utana beta" "https://remote-relay.utana.group"]
      locals=case fromJSON (field "locals" saved) of Success xs -> xs; Error _ -> []
      folders="":map (("corporas" </>) . T.unpack) locals
  (servers,selected) <- foldM (\(servers,selected) folder -> do
   path <- safePath root (folder </> "remote-relay.json")
   exists <- doesFileExist path
   if not exists then pure (servers,selected) else do
    connection <- readJSON path
    let existing=filter ((==connection) . field "connection") servers
        row=case existing of
         value:_ -> value
         [] -> object ["id" .= ("imported-relay-"<>T.pack (show (length servers))),"name" .= ("Imported relay"::T.Text),"connection" .= connection,"available" .= True,"archived" .= False,"builtin" .= False]
    pure (if null existing then servers++[row] else servers,if null folder then field "id" row else selected)) (initial,String "sapiens-beta") folders
  A.putValue store "local/registry" (put "relay_servers" (object ["servers" .= servers,"selected" .= selected,"revision" .= (1::Int),"beta_accepted" .= False]) saved)

-- A completed staging file is reusable only for the exact same source bytes.
-- This catches an old app being reopened between migration attempts.
corporaFingerprint :: FilePath -> IO Value
corporaFingerprint root = do
 let top=["corpora.sqlite3","corpora.sqlite3-wal","corpora.sqlite3-journal",
          "remote-receipts.sqlite3","remote-receipts.sqlite3-wal","remote-receipts.sqlite3-journal",
          "corpora-identity.json","corpora-settings.json","model-settings.json","host-corpora.json","remote-device.json","remote-relay.json"]
 nested <- concat <$> mapM (files "") ["agentpy","uploads","corporas","workspaces"]
 fingerprint root (top++nested)
 where
 files parent name = do
  let relative=parent </> name
  path <- safePath root relative
  directory <- doesDirectoryExist path
  if directory then listDirectory path >>= fmap concat . mapM (files relative)
  else pure [relative | takeExtension relative `elem` [".json",".md",".yaml"]]

relayFingerprint :: FilePath -> IO Value
relayFingerprint path = fingerprint (takeDirectory path) [takeFileName path++suffix | suffix<-["","-wal","-journal"]]

fingerprint :: FilePath -> [FilePath] -> IO Value
fingerprint root names = do
 values <- forM (sortOn id names) $ \name -> do
  path <- safePath root name
  exists <- doesFileExist path
  digest <- if exists then Just . B.unpack . SHA.hash <$> B.readFile path else pure Nothing
  pure (name,digest)
 pure (toJSON values)

migrateNotes :: FilePath -> T.Text -> Value -> IO ()
migrateNotes root owner agent = do
 let folder="workspaces" </> T.unpack owner
 target <- safePath root (folder </> "Notes.html")
 exists <- doesFileExist target
 unless exists $ do
  sources <- filterM doesFileExist =<< mapM (safePath root . (folder </>)) ["Notes.yaml","Notes.md"]
  forM_ (take 1 sources) $ \source -> do
   raw <- TIO.readFile source
   let content=if takeExtension source==".md" then "notes: "<>jsonString raw else raw
       name=case field "name" agent of String value -> value; _ -> ""
       role=case field "role" agent of String value -> value; _ -> ""
   -- Add a new document atomically, retaining the original YAML/Markdown.
   bracketOnError (openTempFile (takeDirectory target) ".migration-notes-")
    (\(temporary,handle) -> hClose handle >> removeFile temporary) $ \(temporary,handle) -> do
     TIO.hPutStr handle (renderMemo name role content)
     hClose handle
     renameFile temporary target
 where jsonString = TE.decodeUtf8 . L.toStrict . encode

normalizeAvatars :: A.Store -> IO ()
normalizeAvatars store = do
 agents <- A.values store "agents/"
 void $ foldM (\used (key,agent) -> do
  let face=field "face" agent
      replacement=case face of String value -> value<>"-"<>T.drop 7 key; _ -> T.drop 7 key
  if face `elem` used then A.putValue store key (put "face" (String replacement) agent) >> pure (String replacement:used)
  else pure (face:used)) [] agents
