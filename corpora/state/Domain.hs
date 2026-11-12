{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Werror=incomplete-patterns #-}
-- Pure domain data and codecs shared by the host and native shells.
module Domain
 ( Wire(..), AgentId(..), TaskId(..), RunId(..), parseWire, TurnStatus(..), Flow(..)
 , CorporaStatus(..), CorporaConfig(..), Frequency(..)
 , periodMillis, Pulse(..), pulseIdentity, TaskState(..), GroupAccess(..), BrowserAction(..)
 , BrowserCommand(..), BrowserTransition(..), browserTransition, TurnAction(..), Turn(..)
 , turnField, withTurnFields, batchLeader, retryTurn, TurnControlError(..), validateTurnAction
 , recoverTurn, newTurn, AgentState(..), stateJSON, stateField, mapTurns, mapStateFields, select
 , selectPresent, mergeFields, appendItems, replaceItem, modifyItems, obj, arr, str, num, get
 , text, int, boolean, list, pairs, keys, has, put, merge, remove, at, items, fallback, empty
 , jsonText, parseJSONText, textFieldP, stringsP, nameP, onlyFields, Population, populationRoster
 , populationChief, populationMembers, GroupRoster, MembershipError(..), memberRoster, rosterLead
 , messageText, AgentProfile(..), rosterMembers, activeStatus, retryableStatus, completedStatus
 ) where

import Control.Monad
import Data.Aeson hiding (pairs)
import Data.Aeson.Types (Parser)
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as BL
import Data.Char (isAscii, isAlphaNum)
import Data.List (find, nub)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Data.Maybe
import qualified Data.Map.Strict as M
import Data.Scientific (toBoundedInteger)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V

newtype AgentId = AgentId { agentIdText :: Text } deriving (Eq, Ord, Show, FromJSON, ToJSON)
newtype TaskId = TaskId { taskIdText :: Text } deriving (Eq, Ord, Show, FromJSON, ToJSON)
newtype RunId = RunId { runIdText :: Text } deriving (Eq, Ord, Show, FromJSON, ToJSON)

-- Wire spellings live at the codec boundary; domain code matches constructors.
class (Eq a, Enum a, Bounded a) => Wire a where
 wireName :: a -> Text
parseWire :: Wire a => Value -> Parser a
parseWire = withText "domain value" $ \name ->
 maybe (fail ("Unknown value: " ++ T.unpack name)) pure (find ((==name) . wireName) [minBound..maxBound])

data AgentProfile = AgentProfile
 { profileId :: AgentId, profileName :: Text, profileRole :: Text
 , profileColor :: Text, profileFace :: Text, profileCreated :: Text
 } deriving (Eq, Show)
instance FromJSON AgentProfile where
 parseJSON = withObject "Agent profile" $ \o -> AgentProfile <$> o .: "id" <*> o .: "name" <*> o .: "role" <*> o .: "color" <*> o .: "face" <*> o .: "created"
instance ToJSON AgentProfile where
 toJSON profile = obj [("id",toJSON (profileId profile)),("name",str (profileName profile)),("role",str (profileRole profile)),("color",str (profileColor profile)),("face",str (profileFace profile)),("created",str (profileCreated profile))]

data TurnStatus = Queued | Running | OutputPending | Done | Failed | Interrupted | Cancelled | Warning deriving (Eq, Ord, Show, Enum, Bounded)
instance Wire TurnStatus where
 wireName status = case status of Queued -> "queued"; Running -> "running"; OutputPending -> "output_pending"; Done -> "done"; Failed -> "failed"; Interrupted -> "interrupted"; Cancelled -> "cancelled"; Warning -> "warning"
instance FromJSON TurnStatus where parseJSON = parseWire
instance ToJSON TurnStatus where toJSON = String . wireName
data Flow = Chat | Computer deriving (Eq, Ord, Show, Enum, Bounded)
instance Wire Flow where wireName flow = case flow of Chat -> "chat"; Computer -> "computer"
instance FromJSON Flow where parseJSON = parseWire
instance ToJSON Flow where toJSON = String . wireName
data CorporaStatus = Active | Paused | Archived deriving (Eq, Ord, Show, Enum, Bounded)
instance Wire CorporaStatus where wireName status = case status of Active -> "active"; Paused -> "paused"; Archived -> "archived"
instance FromJSON CorporaStatus where parseJSON = parseWire
instance ToJSON CorporaStatus where toJSON = String . wireName
data CorporaConfig = CorporaConfig
 { corporaName :: Maybe Text, corporaDevice :: Text, corporaScope :: Text
 , corporaStatus :: CorporaStatus, corporaRevision :: Int, corporaOperations :: M.Map Text Value
 } deriving (Eq, Show)
instance FromJSON CorporaConfig where
 parseJSON = withObject "Corpora settings" $ \o -> CorporaConfig <$> o .:? "name" <*> o .: "device" <*> o .: "scope" <*> o .: "status" <*> o .: "revision" <*> o .:? "operations" .!= M.empty
instance ToJSON CorporaConfig where
 toJSON config = object ["name" .= corporaName config, "device" .= corporaDevice config, "scope" .= corporaScope config, "status" .= corporaStatus config, "revision" .= corporaRevision config, "operations" .= corporaOperations config]
data Frequency = Bpm120 | Bpm60 | Bph60 deriving (Eq, Ord, Show, Enum, Bounded)
instance Wire Frequency where wireName frequency = case frequency of Bpm120 -> "bpm120"; Bpm60 -> "bpm60"; Bph60 -> "bph60"
instance FromJSON Frequency where parseJSON = parseWire
instance ToJSON Frequency where toJSON = String . wireName
periodMillis :: Frequency -> Int
periodMillis frequency = case frequency of Bpm120 -> 500; Bpm60 -> 1000; Bph60 -> 60000
data Pulse = Pulse { pulseName :: Text, pulseFrequency :: Frequency, pulseNumber :: Int } deriving (Eq, Show)
pulseIdentity :: Pulse -> Text
pulseIdentity pulse = pulseName pulse <> ":" <> wireName (pulseFrequency pulse) <> ":" <> T.pack (show (pulseNumber pulse))
instance ToJSON Pulse where
 toJSON pulse = obj [("pulse",str (pulseName pulse)),("frequency",toJSON (pulseFrequency pulse)),("num",num (pulseNumber pulse)),("pulseId",str (pulseIdentity pulse))]
data TaskState = Backlog | InProgress | TaskDone deriving (Eq, Ord, Show, Enum, Bounded)
instance Wire TaskState where wireName state = case state of Backlog -> "backlog"; InProgress -> "in_progress"; TaskDone -> "done"
instance FromJSON TaskState where parseJSON = parseWire
instance ToJSON TaskState where toJSON = String . wireName
data GroupAccess = ChiefOnly | Management | Member deriving (Eq, Show)
data BrowserAction = Navigate | Reload | Back | Forward deriving (Eq, Ord, Show, Enum, Bounded)
instance Wire BrowserAction where
 wireName action = case action of Navigate -> "navigate"; Reload -> "reload"; Back -> "back"; Forward -> "forward"
instance FromJSON BrowserAction where parseJSON = parseWire
instance ToJSON BrowserAction where toJSON = String . wireName
data BrowserCommand = BrowserCommand { browserSequence :: Text, browserAction :: BrowserAction } deriving (Eq, Show)
instance FromJSON BrowserCommand where
 parseJSON = withObject "Browser command" $ \o -> BrowserCommand <$> o .: "seq" <*> o .: "action"
instance ToJSON BrowserCommand where
 toJSON command = object ["seq" .= browserSequence command, "action" .= browserAction command]
data BrowserTransition = BrowserUnchanged | BrowserExecute BrowserCommand deriving (Eq, Show)
browserTransition :: Text -> Maybe BrowserCommand -> BrowserTransition
browserTransition previous command = case command of
 Just next | browserSequence next/=previous -> BrowserExecute next
 _ -> BrowserUnchanged

data TurnAction = Retry | Cancel deriving (Eq, Ord, Show, Enum, Bounded)
instance Wire TurnAction where wireName action = case action of Retry -> "retry"; Cancel -> "cancel"
instance FromJSON TurnAction where parseJSON = parseWire
instance ToJSON TurnAction where toJSON = String . wireName

activeStatus, retryableStatus, completedStatus :: TurnStatus -> Bool
activeStatus status = status `elem` [Queued, Running, OutputPending]
retryableStatus status = status `elem` [Failed, Interrupted]
completedStatus status = status `elem` [Done, Warning]

-- Extra fields are retained for storage compatibility, but lifecycle fields have
-- one authoritative typed representation (they are removed from the extras).
data Turn = Turn { turnId :: RunId, turnFlow :: Flow, turnInput :: Text, turnStatus :: TurnStatus, turnAttempt :: Maybe Int, turnBatchId :: Maybe RunId, turnFields :: Object } deriving (Eq, Show)
instance FromJSON Turn where
 parseJSON = withObject "Turn" $ \o -> do
  attempt <- o .:? "attempt"
  unless (maybe True (>=0) attempt) (fail "Turn attempt cannot be negative")
  Turn <$> o .: "id" <*> o .: "flow" <*> o .: "input" <*> o .: "status" <*> pure attempt <*> o .:? "batch_id" <*> pure (foldr KM.delete o ["id","flow","input","status","attempt","batch_id"])
instance ToJSON Turn where
 toJSON t = Object $ KM.fromList ([("id",toJSON (turnId t)),("flow",toJSON (turnFlow t)),("input",toJSON (turnInput t)),("status",toJSON (turnStatus t))] ++ [("attempt",num n) | n <- maybeToList (turnAttempt t)] ++ [("batch_id",toJSON i) | i <- maybeToList (turnBatchId t)]) <> turnFields t
turnField :: Turn -> Text -> Value
turnField t key = get (Object (turnFields t)) key
withTurnFields :: (Value -> Value) -> Turn -> Turn
withTurnFields f t = t { turnFields = case f (Object (turnFields t)) of Object o -> foldr KM.delete o ["id","flow","input","status","attempt","batch_id"]; _ -> turnFields t }
batchLeader :: Turn -> RunId
batchLeader t = fromMaybe (turnId t) (turnBatchId t)
retryTurn :: Turn -> Turn
retryTurn t = (withTurnFields (remove ["error","batch_members","agency_kind","slot","pulseId"]) t) { turnStatus = Queued, turnAttempt=Just (fromMaybe 0 (turnAttempt t)+1), turnBatchId=Nothing }
data TurnControlError = TurnNotRetryable | TurnAlreadyComplete deriving (Eq, Show)
validateTurnAction :: TurnAction -> TurnStatus -> Either TurnControlError ()
validateTurnAction Retry status
 | retryableStatus status = Right ()
 | otherwise = Left TurnNotRetryable
validateTurnAction Cancel status
 | status `elem` [Done,OutputPending] = Left TurnAlreadyComplete
 | otherwise = Right ()
recoverTurn :: Turn -> Turn
recoverTurn turn
 | turnStatus turn == Running = (withTurnFields (put "error" (str "Runner stopped before saving a reply; inspect effects before retrying")) turn) { turnStatus=Interrupted }
 | otherwise = turn

newTurn :: RunId -> Flow -> Text -> Text -> Value -> Bool -> Turn
newTurn identity flow input created origin batchable = Turn identity flow input Queued (Just 1) Nothing (KM.fromList [("created",str created),("origin",origin),("batchable",Bool batchable)])

data AgentState = AgentState { agentTurns :: [Turn], agentFields :: Object } deriving (Eq, Show)
instance FromJSON AgentState where
 parseJSON = withObject "AgentState" $ \o -> AgentState <$> o .: "turns" <*> pure (KM.delete "turns" o)
instance ToJSON AgentState where toJSON state = Object (KM.insert "turns" (toJSON (agentTurns state)) (agentFields state))
stateJSON :: AgentState -> Value
stateJSON = toJSON
stateField :: AgentState -> Text -> Value
stateField state = get (Object (agentFields state))
mapTurns :: (Turn -> Turn) -> AgentState -> AgentState
mapTurns f state = state { agentTurns = map f (agentTurns state) }
mapStateFields :: (Value -> Value) -> AgentState -> AgentState
mapStateFields f state = state { agentFields = case f (Object (agentFields state)) of Object o -> KM.delete "turns" o; _ -> agentFields state }


select :: [Text] -> Value -> Value
select names value = obj [(key,get value key) | key <- names]
selectPresent :: [Text] -> Value -> Value
selectPresent names value = obj [(key,get value key) | key <- names, has value key]
mergeFields :: [(Text,Value)] -> Value -> Value
mergeFields fields value = merge value (obj fields)
appendItems :: Text -> [Value] -> Value -> Value
appendItems key values value = put key (arr (items value key ++ values)) value
replaceItem :: Text -> Value -> Value -> Value
replaceItem key value = modifyItems key (map (\item -> if get item "id" == get value "id" then value else item))
modifyItems :: Text -> ([Value] -> [Value]) -> Value -> Value
modifyItems key f value = put key (arr (f (items value key))) value

obj :: [(Text, Value)] -> Value
obj = Object . KM.fromList . map (\(k,v) -> (K.fromText k,v))
arr :: [Value] -> Value
arr = Array . V.fromList
str :: Text -> Value
str = String
num :: Int -> Value
num = toJSON
get :: Value -> Text -> Value
get (Object o) k = fromMaybe Null (KM.lookup (K.fromText k) o)
get _ _ = Null
text :: Value -> Text
text (String s) = s
text _ = ""
int :: Value -> Int
int (Number n) = fromMaybe 0 (toBoundedInteger n)
int _ = 0
boolean :: Value -> Bool
boolean (Bool b) = b
boolean _ = False
list :: Value -> [Value]
list (Array a) = V.toList a
list _ = []
pairs :: Value -> [(Text,Value)]
pairs (Object o) = map (\(k,v) -> (K.toText k,v)) (KM.toList o)
pairs _ = []
keys :: Value -> [Text]
keys = map fst . pairs
has :: Value -> Text -> Bool
has (Object o) k = KM.member (K.fromText k) o
has _ _ = False
put :: Text -> Value -> Value -> Value
put k v (Object o) = Object (KM.insert (K.fromText k) v o)
put k v _ = obj [(k,v)]
merge :: Value -> Value -> Value
merge a b = foldl (\x (k,v) -> put k v x) a (pairs b)
remove :: [Text] -> Value -> Value
remove ks = obj . filter ((`notElem` ks) . fst) . pairs
at :: Value -> Text -> Text
at v = text . get v
items :: Value -> Text -> [Value]
items v = list . get v
fallback :: Value -> Value -> Value
fallback Null b = b
fallback a _ = a
empty :: Value
empty = obj []
jsonText :: Value -> Text
jsonText = TE.decodeUtf8 . BL.toStrict . encode
parseJSONText :: Text -> Value
parseJSONText = fromMaybe Null . decode . BL.fromStrict . TE.encodeUtf8

messageText :: Value -> Either Text Text
messageText value = case value `fallback` str "" of
 String content | T.length content <=16000 -> Right (T.strip content)
 _ -> Left "Message must be text, at most 16000 characters"

textFieldP :: Text -> Int -> Value -> Parser Text
textFieldP k n v = case get v k of
 String s | not (T.null (T.strip s)) && T.length s <= n -> pure (T.strip s)
 _ -> fail (T.unpack (k <> " must be nonempty text, at most " <> T.pack (show n) <> " characters"))
stringsP :: Text -> Bool -> Value -> Parser [Text]
stringsP name nonempty value = do
 values <- parseJSON value
 unless (length values <= 50 && (not nonempty || not (null values)) && all (\t -> not (T.null (T.strip t)) && T.length t <= 16000) values) $ fail (T.unpack name ++ " must be a list of nonempty strings")
 pure values
nameP :: Value -> Parser Text
nameP value = do
 name <- textFieldP "name" 24 value
 unless (name == at value "name" && maybe False (\(c,_) -> c >= 'A' && c <= 'Z') (T.uncons name) && T.all (\c -> isAscii c && (isAlphaNum c || c `elem` ("_.:#+|()&$^-" :: String))) name) $ fail "Name must start with A–Z; use letters, numbers, or - _ . : # + | ( ) & $ ^ (no spaces)"
 pure name
onlyFields :: [Text] -> Value -> Parser ()
onlyFields allowed = withObject "record" $ \fields -> unless (all ((`elem` allowed) . K.toText) (KM.keys fields)) (fail "Unknown field in response")

data Population = Population AgentId [AgentId] deriving (Eq, Show)
-- Storage supplies creation/ID order. The first existing agent remains Chief.
populationRoster :: NonEmpty AgentId -> Population
populationRoster (leader :| rest) = Population leader rest
populationChief :: Population -> AgentId
populationChief (Population leader _) = leader
populationMembers :: Population -> [AgentId]
populationMembers (Population leader rest) = leader:rest

data GroupRoster = GroupRoster AgentId (NonEmpty AgentId) deriving (Eq, Show)
data MembershipError = MemberCount | DuplicateOrMissingLead deriving (Eq, Show)
memberRoster :: AgentId -> [AgentId] -> Either MembershipError GroupRoster
memberRoster leader people
 | length people < 2 || length people > 100 = Left MemberCount
 | length (nub people) /= length people || leader `notElem` people = Left DuplicateOrMissingLead
 | Just roster <- NE.nonEmpty people = Right (GroupRoster leader roster)
 | otherwise = Left MemberCount
rosterLead :: GroupRoster -> AgentId
rosterLead (GroupRoster leader _) = leader
rosterMembers :: GroupRoster -> [AgentId]
rosterMembers (GroupRoster _ people) = NE.toList people
