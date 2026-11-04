{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Population where

import Control.Exception
import Control.Monad
import Data.Aeson hiding (pairs)
import Data.Char
import Data.IORef
import Data.Time (UTCTime, getCurrentTime, addUTCTime)
import System.Random (randomRIO)
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Database.SQLite.Simple (SQLData(..))
import System.Directory
import System.Environment
import System.FilePath
import System.Info
import Types
import qualified Store as DB
import Memo

settings :: Service -> Text -> IO Value
settings s a = readJSON (agentRoot s a </> "host.json") empty
retired :: Service -> Text -> IO Bool
retired s a = (/= Null) . (`get` "retired_at") <$> settings s a
requireActive :: Service -> Text -> IO ()
requireActive s a = DB.agent s a >> retired s a >>= \r -> require (not r) 409 "This Sapi is retired. Ask the chief to rehire it before assigning or running work."
directory :: Service -> IO Value
directory s = readJSON (serviceRoot s </> "agentpy/corpora/directory.json") empty
saveDirectory :: Service -> Value -> IO ()
saveDirectory s = writeJSON (serviceRoot s </> "agentpy/corpora/directory.json")
resolve :: Service -> Text -> Bool -> IO Text
resolve s name includeRetired = do
 rows <- DB.agents s
 a <- validated (resolveName name rows)
 unless includeRetired $ requireActive s a
 pure a
resolveName :: Text -> [Value] -> Validation Text
resolveName name rows = case matches of
 [row] -> Right (at row "id")
 _ -> Left (APIError 400 "Sapi name must identify exactly one existing Sapi; use its ID")
 where
 exact = filter ((==name) . (`at` "id")) rows
 matches = if null exact then filter ((==T.toCaseFold name) . T.toCaseFold . (`at` "name")) rows else exact

validName :: Value -> IO Text
validName = parseValue 400 nameP
validateManager :: Service -> Text -> Value -> IO Value
validateManager s a manager = do
 roster <- DB.population s
 let chief = maybe "" (agentIdText . populationChief) roster
 if a == chief then require (manager == Null) 400 "The main orchestrator cannot have a manager" >> pure Null else do
  parent <- if manager == Null then pure chief else resolve s (text manager) False
  d <- directory s
  validated (managerChain d [a] parent)
  pure (if T.null parent then Null else str parent)
managerChain :: Value -> [Text] -> Text -> Validation ()
managerChain directory seen parent
 | T.null parent = Right ()
 | parent `elem` seen = Left (APIError 400 "A Sapi cannot report to itself or one of its descendants")
 | not (has directory parent) = Left (APIError 400 "Parent Sapi must be registered first")
 | otherwise = managerChain directory (parent:seen) (at (get directory parent) "parent")

assignManager :: Service -> Text -> Value -> IO ()
assignManager s a parent = do
 d <- directory s
 let entry = get d a; old = get entry "parent"
 saveDirectory s (put a (put "parent" parent entry) d)
 when (old /= parent) $ DB.record s [a] a "sapi.manager.changed" (obj [("previous",old),("manager",parent)]) "admin" Nothing
executionSettings :: Service -> Text -> IO ExecutionSettings
executionSettings s a = do
 let p = agentRoot s a </> "run-settings.json"
 exists <- doesFileExist p
 readJSON (if exists then p else agentRoot s a </> "execution.json") (ExecutionSettings Normal)
ensureAgent :: Service -> Value -> IO ()
ensureAgent s row = do
 let a = at row "id"; root = agentRoot s a; work = workspaceRoot s a; p = root </> "state.json"
 createDirectoryIfMissing True root
 createDirectoryIfMissing True work
 ensureMemo work (at row "name") (at row "role") `catch` (\(_::APIError) -> pure ())
 exists <- doesFileExist p
 unless exists $ writeJSON p (obj [("schema_version",num 3),("agid",str a),("revision",num 1),("turns",arr []),("chat",arr []),("events",arr []),("event_sequence",num 0),("last_output",Null)])
 v <- readJSON p empty
 require (get v "schema_version" `elem` map num [1,2,3]) 500 "Unsupported agent state schema"
 require (at v "agid" == a) 500 "Agent ID does not match saved state"
 when (int (get v "schema_version") < 3) $ do
  let version = int (get v "schema_version"); backup = root </> ("legacy-state-v" ++ show version ++ ".json")
  backed <- doesFileExist backup
  unless backed $ copyFile p backup
  validated (migrateAgentState version v) >>= saveState s a
 d <- directory s
 unless (has d a) $ saveDirectory s (put a (obj [("parent",Null),("scope",str "")]) d)
 setManifests s row
migrateAgentState :: Int -> Value -> Validation AgentState
migrateAgentState version v =
 let old = items v (if version == 1 then "jobs" else "turns")
     migrate t = let base = selectPresent ["id","flow","status","created","error","warning"] t; input = get t (if version==1 then "task" else "input") in merge (put "input" input base) (if at t "status" == "budget_blocked" then obj [("status",str "interrupted"),("error",str "Former budget block removed; retry to continue")] else empty)
     state = select ["agid","revision","chat","events","event_sequence","last_output"] v
     migrateMessage m = if has m "job" then put "turn" (get m "job") (remove ["job"] m) else m
 in validateValue 500 parseJSON (merge state (obj [("schema_version",num 3),("turns",arr [migrate t | t <- old,at t "flow" `elem` ["chat","computer"]]),("chat",arr (map migrateMessage (items v "chat"))),("events",arr (map migrateMessage (items v "events")))]))

setManifests :: Service -> Value -> IO ()
setManifests s row = do
 let a = at row "id"; root = agentRoot s a </> "manifests"
 identity <- prompt s "identity"
 writeText (root </> "identity.md") (render identity [(k,at row k) | k <- ["name","role","face"]])
 binary <- blindly s
 executable <- getExecutablePath
 computer <- prompt s "computer-use"
 writeText (root </> "computer-use.md") (render computer [("launcher",shellCommand executable <> " computer"),("binary",shellCommand binary)])
 config <- corporaSettings s
 writeText (root </> "corpora-scope.md") (corporaScope config)
blindly :: Service -> IO FilePath
blindly s = fromMaybe (repositoryRoot s </> if os == "mingw32" then "submodules/blindly4/.build/windows/blindly4.exe" else "submodules/blindly4/.build/release/blindly4") <$> lookupEnv "SAPIENS_BLINDLY_BINARY"
shellQuote :: FilePath -> Text
shellQuote p = "'" <> T.replace "'" (if os == "mingw32" then "''" else "'\"'\"'") (T.pack p) <> "'"
shellCommand :: FilePath -> Text
shellCommand p = (if os == "mingw32" then "& " else "") <> shellQuote p
randomAvatar :: [Text] -> IO (Text,Text)
randomAvatar used = do
 let faces=[l<>m<>r | (l,r)<-[("◕","◕"),("◠","◠"),("•","•"),("⌐■","■"),("≧","≦"),("◉","◉"),("^","^"),("¬","¬")],m<-["‿","ᴗ","ω","▽","ᵕ","﹏","o","∇"]]
     available=filter (`notElem` used) faces
     colors=["#d8e5f4","#dbd0f7","#fdd997","#f7d6d1","#c9f3f1","#e1edc6"]
     choices=if null available then faces else available
 index <- randomRIO (0,length choices-1)
 palette <- randomRIO (0,length colors-1)
 suffix <- if null available then ("✦"<>) . T.take 4 <$> newId else pure ""
 pure (choices!!index<>suffix,colors!!palette)
repairAvatars :: Service -> IO ()
repairAvatars s = do
 used <- newIORef []
 rows <- DB.agents s
 forM_ rows $ \row -> do
  known <- readIORef used
  face <- if at row "face" `elem` known then do
   (face,color) <- randomAvatar known
   DB.executeSQL s "UPDATE agents SET face=?,color=? WHERE id=?" [SQLText face,SQLText color,sqlId row]
   pure face
   else pure (at row "face")
  modifyIORef' used (face:)
 where sqlId row=SQLText (at row "id")
data AgentDraft = AgentDraft
 { draftName :: Text, draftRole :: Text
 , preferredColor :: Maybe Text, preferredFace :: Maybe Text
 }
agentDraft :: Value -> Validation AgentDraft
agentDraft d = do
 name <- validateValue 400 nameP d
 role <- validateValue 400 (textFieldP "role" 60) d
 let color = get d "color" `fallback` str "#d8e5f4"; face = get d "face" `fallback` str "◠‿◠"
 check (T.length (text color) == 7 && T.head (text color) == '#' && T.all isHexDigit (T.tail (text color))) 400 "color must be a six-digit hex color"
 check (T.length (text face) >= 1 && T.length (text face) <= 24) 400 "face must contain 1–24 characters"
 check (get d "kind" `fallback` str "sapi" == str "sapi") 400 "Use /api/groups to create a Group"
 pure (AgentDraft name role (if has d "color" then Just (text color) else Nothing) (if has d "face" then Just (text face) else Nothing))

createAgent :: Service -> Value -> IO Value
createAgent s = createAgentAfter s Nothing

createAgentAfter :: Service -> Maybe UTCTime -> Value -> IO Value
createAgentAfter s predecessor d = do
 AgentDraft name role color face <- validated (agentDraft d)
 a <- fresh "sapi_"
 parent <- validateManager s a (get d "manager")
 rows <- DB.agents s
 (randomFace,randomColor) <- randomAvatar (map (`at` "face") rows)
 let selected = case face of Just chosen | chosen `notElem` map (`at` "face") rows -> chosen; _ -> randomFace
     selectedColor = fromMaybe randomColor color
 clock <- getCurrentTime
 let time=timestamp (maybe clock (max clock . addUTCTime 0.000001) predecessor)
 let row = toJSON (AgentProfile (AgentId a) name role selectedColor selected time)
 DB.addAgent s row
 dir <- directory s
 saveDirectory s (put a (obj [("parent",parent),("scope",str "")]) dir)
 ensureAgent s row
 DB.record s [a] a "sapi.created" (put "manager" parent row) "admin" Nothing
 pure row
identityEdit :: Value -> Validation (Text,Text)
identityEdit d = do
 check (all (`elem` ["name","role","manager","execution"]) (keys d)) 400 "Unknown or inactive setting"
 when (has d "execution") $ check (keys (get d "execution") == ["mode"] && at (get d "execution") "mode" `elem` ["normal","deep"]) 400 "Choose Normal or Deep work; call timeouts are managed globally"
 name <- validateValue 400 nameP d
 role <- validateValue 400 (textFieldP "role" 60) d
 pure (name,role)

updateAgent :: Service -> Text -> Value -> IO Value
updateAgent s a d = do
 (name,role) <- validated (identityEdit d)
 requireActive s a
 state <- agentState s a
 require (not (any ((`elem` [Queued,Running]) . turnStatus) (agentTurns state))) 409 "Wait for this Sapi's current turn before changing its identity"
 when (has d "manager") $ validateManager s a (get d "manager") >>= assignManager s a
 when (has d "execution") $ writeJSON (agentRoot s a </> "run-settings.json") (get d "execution")
 DB.executeSQL s "UPDATE agents SET name=?,role=? WHERE id=?" [SQLText name,SQLText role,SQLText a]
 DB.record s [a] a "sapi.updated" d "admin" Nothing
 DB.agent s a >>= setManifests s
 pure (obj [("id",str a),("name",str name),("role",str role)])
team :: Service -> IO [Value]
team s = do
 rows <- DB.agents s
 d <- directory s
 fmap catMaybes $ forM rows $ \row -> do
  let a = at row "id"
  r <- retired s a
  state <- agentState s a
  pure $ if r then Nothing else Just (obj [("id",str a),("name",get row "name"),("role",get row "role"),("manager",get (get d a) "parent"),("busy",Bool (any ((`elem` [Queued,Running]) . turnStatus) (agentTurns state)))])
retiredTeam :: Service -> IO [Value]
retiredTeam s = do
 rows <- DB.agents s
 d <- directory s
 fmap catMaybes $ forM rows $ \row -> do
  cfg <- settings s (at row "id")
  pure $ if get cfg "retired_at" == Null then Nothing else Just (obj [("id",get row "id"),("name",get row "name"),("role",get row "role"),("retired_at",get cfg "retired_at"),("reason",get cfg "retirement_reason" `fallback` str ""),("manager",get (get d (at row "id")) "parent")])
data RetirementError = ChiefRetirement | LongRetirementReason | ActiveGroupMembership | PendingHandoff | UnfinishedWork | ActiveReports deriving (Eq, Show)
retirementResult :: Either RetirementError a -> IO a
retirementResult = either reject pure
 where
 reject problem = case problem of
  ChiefRetirement -> failure 400 "The main orchestrator cannot be retired"
  LongRetirementReason -> failure 400 "reason must be text, at most 500 characters"
  ActiveGroupMembership -> failure 409 "Remove this Sapi from active Groups before retiring it"
  PendingHandoff -> failure 409 "Cancel the accepted handoff before retiring this Sapi"
  UnfinishedWork -> failure 409 "Finish or cancel queued work before retiring this Sapi"
  ActiveReports -> failure 409 "Reassign this Sapi's direct reports before retiring it"
retirementRequest :: Text -> Text -> Text -> Either RetirementError ()
retirementRequest chief target reason
 | chief==target = Left ChiefRetirement
 | T.length reason>500 = Left LongRetirementReason
 | otherwise = Right ()
retirementAllowed :: Text -> AgentState -> [Value] -> [Value] -> Either RetirementError ()
retirementAllowed target state groups works
 | any (\g -> str target `elem` items g "members" && not (boolean (get g "archived"))) groups = Left ActiveGroupMembership
 | any (\call -> at call "addressedTo" == target && at call "state" == "Queued") (concatMap (`items` "calls") works) = Left PendingHandoff
 | any (activeStatus . turnStatus) (agentTurns state) = Left UnfinishedWork
 | otherwise = Right ()
retirementSettings :: Bool -> Text -> Text -> Value -> Value
retirementSettings retire reason time settings = merge settings (obj [("retired_at",if retire then str time else Null),("retirement_reason",str (if retire then reason else ""))])

changeRetirement :: Service -> Text -> Bool -> Text -> IO Value
changeRetirement s a retire reason = do
 chief <- DB.mainAgent s
 retirementResult (retirementRequest chief a reason)
 current <- retired s a
 if current == retire then pure (obj [("id",str a),("retired",Bool retire),("changed",Bool False)]) else do
  state <- agentState s a
  groups <- DB.values s DB.Groups
  works <- DB.values s DB.Workloads
  d <- directory s
  when retire $ do
   retirementResult (retirementAllowed a state groups works)
   forM_ (pairs d) $ \(child,entry) -> when (at entry "parent" == a) $ retired s child >>= \r -> retirementResult (if r then Right () else Left ActiveReports)
  time <- now
  cfg <- settings s a
  writeJSON (agentRoot s a </> "host.json") (retirementSettings retire reason time cfg)
  unless retire $ do
   let parent = at (get d a) "parent"
   unless (T.null parent) $ retired s parent >>= \r -> when r (assignManager s a (str chief))
  DB.record s [a] a (if retire then "sapi.retired" else "sapi.restored") (obj [("reason",str reason)]) "admin" Nothing
  pure (obj [("id",str a),("retired",Bool retire),("changed",Bool True)])
corporaSettings :: Service -> IO CorporaConfig
corporaSettings s = readJSON (serviceRoot s </> "corpora-settings.json") (CorporaConfig Nothing "This computer" "" Active 1 mempty Nothing)
corporaIdentity :: Service -> IO Value
corporaIdentity s = do
 cfg <- corporaSettings s
 identity <- readJSON (serviceRoot s </> "corpora-identity.json") empty
 chief <- DB.mainAgent s
 row <- if T.null chief then pure empty else DB.agent s chief
 template <- prompt s "corpora-settings-request"
 pure $ merge (remove ["operations"] (toJSON cfg)) (obj [("id",get identity "corpora_id"),("name",toJSON (corporaName cfg) `fallback` str ("CORPORA · " <> at row "name")),("chief_id",str chief),("settings_template",str (T.strip template)),("stopping",Bool False)])
