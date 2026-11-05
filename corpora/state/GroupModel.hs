{-# LANGUAGE OverloadedStrings #-}
module GroupModel where

import Control.Monad
import Data.Aeson hiding (pairs)
import Data.Aeson.Types (parseEither)
import qualified Data.Aeson.KeyMap as KM
import Data.List
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory
import System.Random
import Types
import qualified Store as DB
import qualified Population as Population
import Memo

data GroupRecord = GroupRecord
 { groupIdentity :: GroupId, groupRoster :: GroupRoster, groupRevision :: Int
 , groupArchived :: Bool, groupFields :: Object
 }
instance FromJSON GroupRecord where
 parseJSON = withObject "Group" $ \o -> do
  lead <- o .: "lead"
  people <- o .: "members"
  roster <- either (fail . show) pure (memberRoster lead people)
  GroupRecord <$> o .: "id" <*> pure roster <*> o .: "revision" <*> o .: "archived"
    <*> pure (foldr KM.delete o ["id","lead","members","revision","archived"])
instance ToJSON GroupRecord where
 toJSON group = mergeFields [("id",toJSON (groupIdentity group)),("lead",toJSON (rosterLead (groupRoster group))),
  ("members",toJSON (rosterMembers (groupRoster group))),("revision",num (groupRevision group)),("archived",Bool (groupArchived group))] (Object (groupFields group))

data TaskRevision = TaskRevision { requestTaskId :: TaskId, requestTaskRevision :: Int } deriving (Eq, Show)
data RequestLineage = RequestLineage
 { requestMessage :: MessageId, requestRoot :: MessageId, requestTask :: Maybe TaskRevision
 } deriving (Eq, Show)
data StoredLineage = ValidLineage RequestLineage | InvalidLineage Object Text deriving (Eq, Show)

lineageKeys :: [Key]
lineageKeys = ["message","root","task","task_revision"]

decodeLineage :: Object -> StoredLineage
decodeLineage fields = case parseEither parse (Object fields) of
 Right lineage -> ValidLineage lineage
 Left reason -> InvalidLineage (KM.filterWithKey (\key _ -> key `elem` lineageKeys) fields) (T.pack reason)
 where
 parse = withObject "Group request lineage" $ \value -> do
  task <- case (KM.lookup "task" value,KM.lookup "task_revision" value) of
   (Nothing,Nothing) -> pure Nothing
   (Just rawTask,Just rawRevision) -> do
    identity <- parseJSON rawTask
    unless (not (T.null (T.strip (taskIdText identity)))) (fail "Group task ID cannot be empty")
    Just . TaskRevision identity <$> parseJSON rawRevision
   _ -> fail "Group task and revision must be present together"
  RequestLineage <$> value .: "message" <*> value .: "root" <*> pure task

lineageValue :: StoredLineage -> Value
lineageValue stored = case stored of
 InvalidLineage raw _ -> Object raw
 ValidLineage lineage -> obj ([("message",toJSON (requestMessage lineage)),("root",toJSON (requestRoot lineage))]
  ++ concat [[("task",toJSON (requestTaskId task)),("task_revision",num (requestTaskRevision task))] | task <- maybeToList (requestTask lineage)])

checkedRequestLineage :: GroupRequest -> Either Text RequestLineage
checkedRequestLineage request = case requestLineage request of
 ValidLineage lineage -> Right lineage
 InvalidLineage _ reason -> Left ("Saved Group request lineage is malformed: " <> reason)

requireRequestLineage :: GroupRequest -> IO RequestLineage
requireRequestLineage = either (failure 500) pure . checkedRequestLineage

-- Raw invalid pointers are used only to quarantine possibly related operations.
invalidLineageReferences :: MessageId -> GroupRequest -> Bool
invalidLineageReferences identity request = case requestLineage request of
 ValidLineage _ -> False
 InvalidLineage raw _ -> any (==toJSON identity) [get (Object raw) "root",get (Object raw) "message"]
  || runIdText (requestId request) == messageIdText identity

requestsForRoot :: MessageId -> [GroupRequest] -> Validation [GroupRequest]
requestsForRoot identity requests = fmap catMaybes $ forM requests $ \request -> case requestLineage request of
 ValidLineage lineage -> pure (if requestRoot lineage == identity then Just request else Nothing)
 InvalidLineage _ reason | invalidLineageReferences identity request -> Left (APIError 500 ("Saved Group request lineage is malformed: " <> reason))
 InvalidLineage _ _ -> pure Nothing

newGroupRequest :: RunId -> AgentId -> StoredOrigin -> RequestLineage -> Text -> GroupRequest
newGroupRequest identity target origin lineage created = GroupRequest identity target Queued False origin (ValidLineage lineage) (KM.singleton "created" (str created))

data GroupRequest = GroupRequest
 { requestId :: RunId, requestTarget :: AgentId, requestStatus :: TurnStatus
 , requestSettled :: Bool, requestOrigin :: StoredOrigin, requestLineage :: StoredLineage, requestFields :: Object
 } deriving (Eq, Show)
instance FromJSON GroupRequest where
 parseJSON = withObject "Group request" $ \o -> GroupRequest <$> o .: "id" <*> o .: "target" <*> o .: "status" <*> o .: "settled" <*> pure (storeOrigin (KM.lookup "origin" o)) <*> pure (decodeLineage o) <*> pure (foldr KM.delete o (["id","target","status","settled","origin"] ++ lineageKeys))
instance ToJSON GroupRequest where
 toJSON request = mergeFields ([("id",toJSON (requestId request)),("target",toJSON (requestTarget request)),("status",toJSON (requestStatus request)),("settled",Bool (requestSettled request))] ++ [("origin",toJSON (requestOrigin request)) | requestOrigin request /= MissingOrigin]) (merge (Object (requestFields request)) (lineageValue (requestLineage request)))
readRequests :: Value -> IO [GroupRequest]
readRequests group = decodeValue 500 (get group "requests")

getGroup :: Service -> Text -> Bool -> IO Value
getGroup s gid active = do
 g <- DB.values s DB.Groups >>= maybe (failure 404 "Unknown Group") pure . findId gid
 require (not active || not (boolean (get g "archived"))) 409 "Restore this Group before changing its work"
 toJSON <$> (decodeValue 500 g :: IO GroupRecord)
storeGroup :: Service -> Value -> IO ()
storeGroup s raw = do
 group <- decodeValue 500 raw
 DB.putValue s DB.Groups (groupIdText (groupIdentity group)) (toJSON group)
append :: Text -> Value -> Value -> Value
append key value = appendItems key [value]
replace :: Text -> Value -> Value -> Value
replace = replaceItem
saveGroup :: Service -> Value -> Text -> Text -> Value -> IO Value
saveGroup s group actor action detail = do
 event <- eventRecord actor action detail
 let updated = append "events" event (merge group (obj [("revision",num (int (get group "revision")+1)),("updated",get event "created")]))
 storeGroup s updated
 pure updated
eventRecord :: Text -> Text -> Value -> IO Value
eventRecord actor action detail = do
 i <- newId; t <- now
 pure (obj [("id",str i),("actor",str actor),("action",str action),("detail",detail),("created",str t)])
message :: Text -> Text -> IO Value
message author content = do
 i <- newId; t <- now
 pure (obj [("id",str i),("author",str author),("text",str content),("created",str t)])
authorizeGroup :: Service -> Value -> Text -> GroupAccess -> IO ()
authorizeGroup s group actor mode = unless (actor == "admin") $ do
 Population.requireActive s actor
 chief <- DB.mainAgent s
 validated (groupPermission chief group actor mode)

groupPermission :: Text -> Value -> Text -> GroupAccess -> Validation ()
groupPermission chief group actor mode = do
 let allowed = case mode of
       ChiefOnly -> actor == chief
       Management -> actor `elem` [chief,at group "lead"]
       Member -> str actor `elem` items group "members"
 check allowed 403 (if mode == ChiefOnly then "Only Chief can perform this operation" else if mode == Management then "Only Chief or the Lead can manage members" else "Only Group members can contribute")
validateMembers :: Service -> Value -> Value -> IO GroupRoster
validateMembers s value lead = do
 members <- validated (groupMembers value lead)
 mapM_ (Population.requireActive s . agentIdText) (rosterMembers members)
 pure members
groupMembers :: Value -> Value -> Validation GroupRoster
groupMembers value lead = do
 let people = list value
 check (case value of Array _ -> length people >= 2 && length people <= 100 && all (\v -> case v of String _ -> True; _ -> False) people; _ -> False) 400 "A Group needs 2–100 distinct Sapis, including its Lead"
 -- Preserve the public validation message for non-string or absent leads.
 check (lead `elem` people) 400 "Members must be distinct and include exactly one Lead"
 roster <- either (const (Left (APIError 400 "Members must be distinct and include exactly one Lead"))) Right
   (memberRoster (AgentId (text lead)) (map (AgentId . text) people))
 pure roster

capacity :: Service -> [Value] -> Maybe Text -> IO ()
capacity s members exclude = do
 groups <- filter (\g -> not (boolean (get g "archived")) && Just (at g "id") /= exclude) <$> DB.values s DB.Groups
 validated (groupCapacity groups members)
groupCapacity :: [Value] -> [Value] -> Validation ()
groupCapacity groups members = forM_ members $ \m -> check (length (filter (elem m . (`items` "members")) groups) < 11) 409 "A Sapi can belong to at most 11 active Groups"
shuffle :: [a] -> IO [a]
shuffle = getStdRandom . uniformShuffleList
folder :: Service -> Text -> IO FilePath
folder s gid = do
 void (getGroup s gid False)
 let path = workspaceRoot s gid
 createDirectoryIfMissing True path
 pure path
introduction :: Service -> Value -> IO [Value]
introduction s group = do
 rows <- DB.agents s; chief <- DB.mainAgent s
 let name i = maybe "" (`at` "name") (findId i rows)
     lead = name (at group "lead")
     roster = T.intercalate ", " ["@" <> name (text m) | m <- get group "lead" : filter (/= get group "lead") (items group "members")]
     purpose = T.strip (at group "description")
     admin = if T.null purpose then "I need " <> at group "name" <> " to work together. Let’s define the first task here." else "I need this group to work on:\n\n" <> purpose
 messages <- sequence [message "admin" admin, message chief ("I’ve created " <> at group "name" <> " with " <> roster <> ". @" <> lead <> " will lead this group."),message (at group "lead") ("Hi! I’m " <> lead <> ", Lead of this group. Together we can manage a shared memo/wiki, workflows, a Scrum backlog or ad hoc tasks, and automated operations.")]
 pure (map (put "source" (str "group-introduction")) messages)
data GroupDraft = GroupDraft Text Text
validateGroupFields :: Value -> [Text] -> Validation ()
validateGroupFields input allowed = check (all (`elem` allowed) (keys input)) 400 "Unknown Group field"
groupDescription :: Value -> Validation Text
groupDescription (String content) | T.length content <= 2000 = Right content
groupDescription _ = Left (APIError 400 "Description must be text, at most 2000 characters")
groupDraft :: Value -> Validation GroupDraft
groupDraft input = do
 validateGroupFields input ["name","description","lead","members"]
 name <- validateValue 400 (textFieldP "name" 60) input
 description <- groupDescription (get input "description" `fallback` str "")
 pure (GroupDraft name description)
validateGroupRevision :: Value -> Value -> Validation ()
validateGroupRevision group input = check (get input "revision" == get group "revision" && get input "revision" /= Null) 409 "Group changed; refresh before saving"
validateGroupArchive :: Value -> Value -> Validation ()
validateGroupArchive group input = do
 when (has input "archived") $ check (case get input "archived" of Bool _ -> True; _ -> False) 400 "archived must be a boolean"
 check (not (boolean (get group "archived")) || get input "archived" == Bool False) 409 "Restore this Group before editing it"
groupIdentityEdit :: Value -> Value -> Validation (Value,Text)
groupIdentityEdit group input = do
 name <- if has input "name" then str <$> validateValue 400 (textFieldP "name" 60) input else pure (get group "name")
 description <- groupDescription (get input "description" `fallback` get group "description")
 pure (name,description)

createGroup :: Service -> Value -> Text -> IO Value
createGroup s input actor = do
 authorizeGroup s empty actor ChiefOnly
 GroupDraft name description <- validated (groupDraft input)
 roster <- validateMembers s (get input "members") (get input "lead")
 let members = map toJSON (rosterMembers roster); lead = toJSON (rosterLead roster)
 capacity s members Nothing
 order <- shuffle (filter (/= lead) members)
 gid <- fresh "group_"; created <- now; color <- ( ["#48c99c", "#ba85df", "#74b9ed", "#edbd65", "#e891ae"] !!) <$> randomRIO (0,4)
 let group = obj [("id",str gid),("name",str name),("description",str description),("lead",lead),("members",arr members),("strip_order",arr (lead:order)),("color",str color),("archived",Bool False),("created",str created),("updated",str created),("revision",num 0),("messages",arr []),("requests",arr []),("tasks",arr []),("events",arr [])]
 messages <- introduction s group
 saved <- saveGroup s (put "messages" (arr messages) group) actor "created" (str name)
 void (folder s gid)
 pure saved
updateGroup :: Service -> Text -> Value -> Text -> IO Value
updateGroup s gid input actor = do
 group <- getGroup s gid False
 validated (validateGroupFields input ["name","description","members","lead","archived","revision"])
 authorizeGroup s group actor Management
 validated (validateGroupRevision group input)
 when ((has input "lead" && get input "lead" /= get group "lead") || has input "archived") $ authorizeGroup s group actor ChiefOnly
 validated (validateGroupArchive group input)
 let lead = get input "lead" `fallback` get group "lead"
 roster <- validateMembers s (get input "members" `fallback` get group "members") lead
 let members = map toJSON (rosterMembers roster)
 let archived = get input "archived" `fallback` get group "archived"; removed = items group "members" \\ members
 unless (boolean archived) $ capacity s members (Just gid)
 validated (membersMayLeave removed group)
 (name,description) <- validated (groupIdentityEdit group input)
 let oldOrder = filter (\m -> m `elem` members && m /= lead) (items group "strip_order")
 additions <- shuffle [m | m <- members,m /= lead,m `notElem` oldOrder]
 requests <- if not (boolean archived) then pure (items group "requests") else forM (items group "requests") $ \r ->
  if at r "status" /= "queued" then pure r else do
   turn <- findId (at r "id") <$> DB.turns s
   let (updated,cancel) = archiveRequest r turn
   forM_ cancel $ \(owner,identity) -> do
    modifyTurns s owner (\v -> if turnId v == identity then v { turnStatus = Cancelled } else v)
    DB.project s owner
   pure updated
 let updated = merge group (obj [("lead",lead),("name",name),("description",str description),("members",arr members),("strip_order",arr (lead:oldOrder++additions)),("archived",archived),("requests",arr requests)])
 saveGroup s updated actor (if lead /= get group "lead" then "leadership transferred" else if boolean archived then "archived" else "updated") (obj [("lead",lead),("members",arr members)])
membersMayLeave :: [Value] -> Value -> Validation ()
membersMayLeave removed group = do
 check (not (any (\r -> get r "target" `elem` removed && at r "status" `elem` ["queued","running"]) (items group "requests"))) 409 "Finish or cancel the departing member’s Group calls first"
 check (not (any (\t -> get t "assignee" `elem` removed && not (boolean (get t "deleted")) && at t "state" /= "done") (items group "tasks"))) 409 "Reassign the departing member’s unfinished tasks first"

archiveRequest :: Value -> Maybe Value -> (Value,Maybe (Text,RunId))
archiveRequest request projected = case projected of
 Just turn | at turn "status" == "running" -> (put "status" (str "running") request,Nothing)
 _ -> (put "status" (str "cancelled") request, cancellation)
 where
 cancellation = case projected of
  Just turn | at turn "status" == "queued" -> Just (at request "target",RunId (at request "id"))
  _ -> Nothing

snapshotGroups :: Service -> IO [Value]
snapshotGroups s = do
 groups <- DB.values s DB.Groups; rows <- DB.agents s
 forM groups $ \g -> do
  root <- folder s (at g "id"); meta <- memoMetadata root
  let person i = fromMaybe empty (findId i rows)
  pure $ merge g (obj [("kind",str "group"),("role",str (if T.null (at g "description") then "Shared Group workspace" else at g "description")),("face",get (person (at g "lead")) "face"),("stripes",arr [get (person (text m)) "color" | m <- items g "strip_order"]),("workspace",str (T.pack root)),("notes",meta)])
groupFacts :: Service -> Text -> IO [Value]
groupFacts s actor = do
 chief <- DB.mainAgent s; groups <- DB.values s DB.Groups
 forM [g | g <- groups,not (boolean (get g "archived")),actor == chief || str actor `elem` items g "members"] $ \g -> do
  root <- folder s (at g "id")
  pure (obj ([(k,get g k) | k <- ["id","name","lead","members","revision","tasks"]] ++ [("workspace",str (T.pack root))]))
