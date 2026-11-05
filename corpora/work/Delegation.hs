{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Werror=incomplete-patterns #-}
module Delegation where

import Control.Concurrent
import qualified Data.Map.Strict as M
import Control.Exception
import Control.Monad
import Crypto.Hash.SHA256 (hash)
import Data.Aeson hiding (pairs)
import Data.Aeson.Types (Parser, parseEither)
import qualified Data.Bifunctor as B
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Aeson.Key
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import qualified Data.ByteString as BS
import Data.IORef
import Data.List (find,nub,sort)
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Numeric (showHex)
import System.Directory
import System.FilePath
import Types
import qualified Store as DB
import Population
import Memo
import Provider

data Node = Assessing | ChiefTriage | CreatingSapi | ReadyToWork | Delegation | Execution | OutcomeNode | WaitingForAdmin deriving (Eq, Ord, Show, Enum, Bounded)
instance Wire Node where
 wireName node = case node of Assessing -> "Assessing"; ChiefTriage -> "ChiefTriage"; CreatingSapi -> "CreatingSapi"; ReadyToWork -> "ReadyToWork"; Delegation -> "Delegation"; Execution -> "Execution"; OutcomeNode -> "Outcome"; WaitingForAdmin -> "WaitingForAdmin"
instance FromJSON Node where parseJSON = parseWire
instance ToJSON Node where toJSON = str . wireName

data CallState = CallQueued | CallRunning | CallCompleted | CallUnresolved | CallDelegated | CallWaiting | CallFailed | CallInterrupted | CallCancelled deriving (Eq, Ord, Show, Enum, Bounded)
instance Wire CallState where
 wireName state = case state of CallQueued -> "Queued"; CallRunning -> "Running"; CallCompleted -> "Completed"; CallUnresolved -> "Unresolved"; CallDelegated -> "Delegated"; CallWaiting -> "WaitingForAdmin"; CallFailed -> "Failed"; CallInterrupted -> "Interrupted"; CallCancelled -> "Cancelled"
instance FromJSON CallState where parseJSON = parseWire
instance ToJSON CallState where toJSON = str . wireName

-- A finished call owns its required reply and determines its terminal node.
-- Active/stopped calls retain the node needed for retry and have no output.
data CallResult = Answered Text | UnresolvedResult Text | HandedOff Text | NeedsClarification Text deriving (Eq, Show)
data CallProgress = QueuedAt Node | RunningAt Node | FailedAt Node | InterruptedAt Node | CancelledAt Node | FinishedCall CallResult deriving (Eq, Show)
data WorkCall = WorkCall
 { callIdentity :: RunId, callOwner :: AgentId, callProgress :: CallProgress
 , callRequest :: Text, callParent :: Maybe RunId, callFields :: Object
 } deriving (Eq, Show)
callState :: WorkCall -> CallState
callState call = case callProgress call of
 QueuedAt _ -> CallQueued; RunningAt _ -> CallRunning; FailedAt _ -> CallFailed; InterruptedAt _ -> CallInterrupted; CancelledAt _ -> CallCancelled
 FinishedCall result -> case result of
  Answered _ -> CallCompleted; UnresolvedResult _ -> CallUnresolved; HandedOff _ -> CallDelegated; NeedsClarification _ -> CallWaiting
callNode :: WorkCall -> Node
callNode call = case callProgress call of
 QueuedAt node -> node; RunningAt node -> node; FailedAt node -> node; InterruptedAt node -> node; CancelledAt node -> node
 FinishedCall result -> case result of
  Answered _ -> OutcomeNode; UnresolvedResult _ -> OutcomeNode; HandedOff _ -> Delegation; NeedsClarification _ -> WaitingForAdmin
callOutput :: WorkCall -> Maybe Text
callOutput call = case callProgress call of
 QueuedAt _ -> Nothing; RunningAt _ -> Nothing; FailedAt _ -> Nothing; InterruptedAt _ -> Nothing; CancelledAt _ -> Nothing
 FinishedCall result -> Just $ case result of
  Answered output -> output; UnresolvedResult output -> output; HandedOff output -> output; NeedsClarification output -> output
instance FromJSON WorkCall where
 parseJSON = withObject "Workload call" $ \o -> do
  node <- o .: "node"
  state <- o .: "state"
  output <- o .:? "output"
  let unfinished constructor = do
       unless (isNothing output) (fail "Unfinished workload call cannot have output")
       pure (constructor node)
      finished constructor expected = do
       unless (node == expected) (fail "Workload call has an inconsistent terminal node")
       FinishedCall . constructor <$> maybe (fail "Finished workload call requires output") pure output
  progress <- case state of
   CallQueued -> unfinished QueuedAt; CallRunning -> unfinished RunningAt
   CallFailed -> unfinished FailedAt; CallInterrupted -> unfinished InterruptedAt; CallCancelled -> unfinished CancelledAt
   CallCompleted -> finished Answered OutcomeNode; CallUnresolved -> finished UnresolvedResult OutcomeNode
   CallDelegated -> finished HandedOff Delegation; CallWaiting -> finished NeedsClarification WaitingForAdmin
  WorkCall <$> o .: "callId" <*> o .: "addressedTo" <*> pure progress <*> o .: "request" <*> o .:? "causedBy" <*> pure (foldr KM.delete o ["callId","addressedTo","node","state","request","causedBy","output"])
instance ToJSON WorkCall where
 toJSON call = mergeFields ([("callId",toJSON (callIdentity call)),("addressedTo",toJSON (callOwner call)),("node",toJSON (callNode call)),("state",toJSON (callState call)),("request",str (callRequest call)),("causedBy",toJSON (callParent call))] ++ [("output",str output) | output <- maybeToList (callOutput call)]) (Object (callFields call))
callField :: WorkCall -> Text -> Value
callField call = get (Object (callFields call))
setCallFields :: [(Text,Value)] -> WorkCall -> WorkCall
setCallFields fields call = call { callFields = KM.fromList [(Data.Aeson.Key.fromText key,value) | (key,value) <- fields] <> callFields call }
newCall :: RunId -> AgentId -> Maybe RunId -> CallProgress -> Text -> WorkCall
newCall identity owner parent progress request = WorkCall identity owner progress request parent (KM.singleton "attempt" (num 1))

data Workload = Workload { workloadId :: Text, workTracked :: Bool, workCalls :: NonEmpty WorkCall, workFields :: Object } deriving (Eq, Show)
instance FromJSON Workload where
 parseJSON = withObject "Workload" $ \o -> Workload <$> o .: "workloadId" <*> o .: "tracked" <*> o .: "calls" <*> pure (foldr KM.delete o ["workloadId","tracked","calls"])
instance ToJSON Workload where
 toJSON work = mergeFields [("workloadId",str (workloadId work)),("tracked",Bool (workTracked work)),("calls",toJSON (workCalls work))] (Object (workFields work))
workField :: Workload -> Text -> Value
workField work = get (Object (workFields work))
workItems :: Workload -> Text -> [Value]
workItems work = list . workField work
setWorkField :: Text -> Value -> Workload -> Workload
setWorkField key value work = work { workFields = KM.insert (Data.Aeson.Key.fromText key) value (workFields work) }
readWorks :: Service -> IO [Workload]
readWorks s = DB.values s DB.Workloads >>= traverse (decodeValue 500)

data TaskSpec = TaskSpec { specObjective :: Text, specInputs :: [Text], specOutputs :: NonEmpty Text, specCriteria :: NonEmpty Text } deriving (Eq, Show)
instance FromJSON TaskSpec where
 parseJSON value = do
  onlyFields ["objective","inputs","expectedOutputs","completionCriteria"] value
  void (textFieldP "objective" 16000 value)
  let nonempty key = stringsP key True (get value key) >>= maybe (fail "Expected at least one item") pure . NE.nonEmpty
  TaskSpec <$> parseJSON (get value "objective") <*> stringsP "inputs" False (get value "inputs") <*> nonempty "expectedOutputs" <*> nonempty "completionCriteria"
instance ToJSON TaskSpec where
 toJSON spec = obj [("objective",str (specObjective spec)),("inputs",toJSON (specInputs spec)),("expectedOutputs",toJSON (NE.toList (specOutputs spec))),("completionCriteria",toJSON (NE.toList (specCriteria spec)))]
data OutcomeStatus = Completed | Unresolved deriving (Eq, Ord, Show, Enum, Bounded)
instance Wire OutcomeStatus where wireName status = case status of Completed -> "Completed"; Unresolved -> "Unresolved"
instance FromJSON OutcomeStatus where parseJSON = parseWire
instance ToJSON OutcomeStatus where toJSON = str . wireName
data Outcome = Outcome { outcomeStatus :: OutcomeStatus, satisfiedCriteria :: [Text], outcomeArtifacts :: [Text], notApplicable :: M.Map Text Text } deriving (Eq, Show)
instance FromJSON Outcome where
 parseJSON value = do
  onlyFields ["status","satisfiedCriteria","artifacts","notApplicableCriteria"] value
  skipped <- parseJSON (get value "notApplicableCriteria" `fallback` empty)
  forM_ (M.elems skipped) $ \reason -> void (textFieldP "reason" 4000 (obj [("reason",str reason)]))
  Outcome <$> parseJSON (get value "status") <*> stringsP "satisfiedCriteria" False (get value "satisfiedCriteria") <*> stringsP "artifacts" False (get value "artifacts") <*> pure skipped

data Decision
 = ReplyNow Text | ExecuteTask | OutsideSpecialization | SelectSpecialist Text
 | NeedSpecialist | AskAdmin Text | SpecifySapi Text Text | PrepareTask Text TaskSpec
 | Handoff Text | ReportOutcome Text Outcome
 deriving (Eq, Show)
data DecisionResponse = DecisionResponse { decisionBody :: Decision, decisionReason :: Text, decisionEvidence :: [Text], decisionRaw :: Value } deriving (Eq, Show)
parseDecision :: Node -> Value -> Parser DecisionResponse
parseDecision node value = do
 let event=at value "event"
     variant fields parser = onlyFields (fields ++ ["event","reason","evidence"]) value >> parser
     textAt key limit = textFieldP key limit value
 unless (event `elem` eventsFor node) (fail ("Unknown decision event for " ++ T.unpack (wireName node)))
 reason <- textAt "reason" 4000 >> parseJSON (get value "reason")
 evidence <- stringsP "evidence" False (get value "evidence")
 body <- case event of
  "FitsSpecialization" -> variant ["mode","reply"] $ case at value "mode" of
   "Repl" -> ReplyNow <$> textAt "reply" 64000
   "Exec" -> pure ExecuteTask
   _ -> fail "FitsSpecialization requires mode Repl or Exec"
  "OutsideSpecialization" -> variant [] (pure OutsideSpecialization)
  "SpecialistSelected" -> variant ["target"] (SelectSpecialist <$> parseJSON (get value "target"))
  "NewSpecialistNeeded" -> variant [] (pure NeedSpecialist)
  "RequestUnclear" -> variant ["question"] (AskAdmin <$> textAt "question" 4000)
  "SapiSpecified" -> variant ["name","role"] (SpecifySapi <$> nameP value <*> textAt "role" 60)
  "TaskPrepared" -> variant ["taskType","specification"] (PrepareTask <$> textAt "taskType" 60 <*> parseJSON (get value "specification"))
  "HandoffPrepared" -> variant ["request"] (Handoff <$> textAt "request" 16000)
  "Outcome" -> variant ["reply","outcome"] (ReportOutcome <$> textAt "reply" 64000 <*> parseJSON (get value "outcome"))
  _ -> fail "Unknown decision event"
 pure (DecisionResponse body reason evidence value)

promptNames :: [(Node,Text)]
promptNames = [(Assessing,"decision-triage"),(ChiefTriage,"decision-chief-triage"),(CreatingSapi,"decision-create-sapi"),(ReadyToWork,"decision-prepare-task"),(Delegation,"decision-delegation"),(Execution,"decision-execution")]
eventsFor :: Node -> [Text]
eventsFor node = case node of
 Assessing -> ["FitsSpecialization","OutsideSpecialization"]
 ChiefTriage -> ["FitsSpecialization","SpecialistSelected","NewSpecialistNeeded","RequestUnclear"]
 CreatingSapi -> ["SapiSpecified"]
 ReadyToWork -> ["TaskPrepared"]
 Delegation -> ["HandoffPrepared"]
 Execution -> ["Outcome","OutsideSpecialization"]
 OutcomeNode -> []
 WaitingForAdmin -> []
templates :: Service -> IO [Value]
templates s = forM promptNames $ \(node,name) -> do
 let local = serviceRoot s </> "decision-prompts" </> T.unpack name <.> "md"
 exists <- doesFileExist local
 let p = if exists then local else repositoryRoot s </> "corpora/prompts" </> T.unpack name <.> "md"
 content <- readText p
 pure $ obj [("node",toJSON node),("template",str (name<>".md")),("path",str (T.pack p)),("content",str content)]
editTemplate :: Service -> Text -> Value -> IO Value
editTemplate s rawNode d = do
 node <- decodeValue 400 (str rawNode)
 name <- maybe (failure 400 "Unknown decision node or field") pure (lookup node promptNames)
 require (keys d == ["content"]) 400 "Unknown decision node or field"
 content <- textField d "content" 20000
 let p = serviceRoot s </> "decision-prompts" </> T.unpack name <.> "md"
 writeText p content
 pure (obj [("saved",Bool True),("path",str (T.pack p))])
findWork :: Service -> RunId -> IO (Maybe (Workload,WorkCall))
findWork s i = do
 works <- readWorks s
 pure $ listToMaybe [(w,c) | w <- works,c <- NE.toList (workCalls w),callIdentity c == i]
getWork :: Service -> RunId -> IO (Workload,WorkCall)
getWork s i = findWork s i >>= maybe (failure 409 "Unknown workload call") pure
saveWork :: Service -> Workload -> IO ()
saveWork s w = do
 previous <- find ((==workloadId w) . workloadId) <$> readWorks s
 let call=NE.last (workCalls w)
     before=NE.last . workCalls <$> previous
     changed=maybe False (\old -> workTracked w && (not (workTracked old) || fmap (\c -> (callState c,callOwner c)) before /= Just (callState call,callOwner call))) previous
 when changed $ do
  (source,agency) <- currentActor s
  let state=callState call
      actor=if isJust agency || state==CallCancelled then source else if state==CallQueued then "system" else agentIdText (callOwner call)
      detail=obj [("owner",toJSON (callOwner call)),("task",workField w "task"),("previous",maybe Null (toJSON . callState) before),("call",toJSON (callIdentity call)),("error",callField call "error")]
  DB.record s (map (agentIdText . callOwner) (NE.toList (workCalls w))) (at (workField w "task") "taskId") ("task."<>T.toLower (wireName state)) detail actor (if state `elem` [CallQueued,CallCancelled] then Nothing else Just "chatInput")
 DB.putValue s DB.Workloads (workloadId w) (toJSON w)
withCall :: Workload -> WorkCall -> Workload
withCall w c = w { workCalls = fmap (\x -> if callIdentity x == callIdentity c then c else x) (workCalls w) }
data OutcomeError = InvalidSkippedCriterion | MissingCompletionEvidence deriving (Eq, Show)
outcomeError :: OutcomeError -> Text
outcomeError problem = case problem of
 InvalidSkippedCriterion -> "Only an unsatisfied, explicitly conditional criterion can be not applicable"
 MissingCompletionEvidence -> "Completed outcome requires evidence for all applicable completion criteria"
data CompletionCriterion = RequiredCriterion Text | ConditionalCriterion Text deriving (Eq, Show)
completionCriterion :: Text -> CompletionCriterion
completionCriterion criterion
 | any (`T.isPrefixOf` T.toCaseFold (T.stripStart criterion)) ["if ","если "] = ConditionalCriterion criterion
 | otherwise = RequiredCriterion criterion
validateOutcome :: DecisionResponse -> Outcome -> [Text] -> Either OutcomeError ()
validateOutcome response outcome criteria
 | any invalidSkip skipped = Left InvalidSkippedCriterion
 | outcomeStatus outcome == Completed && (null (decisionEvidence response) || any (`notElem` covered) criteria) = Left MissingCompletionEvidence
 | otherwise = Right ()
 where
 skipped = M.keys (notApplicable outcome)
 covered = satisfiedCriteria outcome ++ skipped
 invalidSkip criterion = criterion `elem` satisfiedCriteria outcome || case find ((==criterion) . criterionText) (map completionCriterion criteria) of
  Just (ConditionalCriterion _) -> False
  _ -> True
 criterionText form = case form of
  RequiredCriterion value -> value
  ConditionalCriterion value -> value
begin :: Service -> AgentId -> Turn -> IO ()
begin s a turn = do
 found <- findWork s (turnId turn)
 let routine=has (turnField turn "origin") "routine"
     first=newCall (turnId turn) a Nothing (RunningAt (if routine then ReadyToWork else Assessing)) (turnInput turn)
     initial=Workload (runIdText (turnId turn)) routine (first :| []) (KM.fromList [("originalIntent",str (turnInput turn)),("task",obj [("taskId",str ("task_"<>runIdText (turnId turn))),("taskType",str (if routine then "automated" else "work")),("specification",Null)]),("origin",merge (obj [("agent",toJSON a),("call",toJSON (turnId turn))]) (turnField turn "origin")),("decisions",arr []),("outcome",Null)])
     (w,c)=fromMaybe (initial,first) found
     running=if isNothing (callOutput c) then c { callProgress=RunningAt (callNode c), callFields=KM.delete "error" (callFields c) } else c
     updated=setCallFields [("attempt",num (fromMaybe 1 (turnAttempt turn)))] running
 saveWork s (withCall w updated)
conversation :: Service -> AgentId -> Turn -> AgentState -> IO Value
conversation s a turn state = do
 manifests <- fmap obj $ fmap catMaybes $ forM ["identity","computer-use","host-control","host-facts","corpora-state"] $ \name -> do
  let p = agentRoot s (agentIdText a) </> "manifests" </> T.unpack name <.> "md"
  exists <- doesFileExist p
  if exists then Just . (name,) . str <$> readText p else pure Nothing
 memo <- (get <$> readMemo (workspaceRoot s (agentIdText a)) <*> pure "content") `catch` (\(APIError _ e) -> pure (obj [("error",str e)]))
 let reviewing t = batchLeader t /= turnId turn && turnStatus t `elem` [Running,Failed,Interrupted]
     chat = takeLast 20 [let matching = find ((==get m "turn") . toJSON . turnId) (agentTurns state) in if at m "role" == "user" && maybe False reviewing matching then put "underPrevAgencyReview" (Bool True) m else m | m <- list (stateField state "chat"), not (has (get m "origin") "group")]
 pure $ obj [("manifests",remove ["corpora-state"] manifests),("staticContext",get manifests "corpora-state" `fallback` str ""),("instantContext",obj [("chat",arr chat),("memo",memo)]),("inputs",stateField state "agency_batch" `fallback` arr [obj [("content",str (turnInput turn))]])]
 where takeLast n xs = drop (length xs-n) xs
recordDecision :: Service -> Text -> Value -> IO ()
recordDecision s a record = do
 writeJSON (archivePath s a ("decisions/"<>at record "decisionId")) record
 (w,_) <- field 500 record "callId" >>= getWork s
 let update d = if get d "decisionId" == get record "decisionId" then merge d (selectPresent ["accepted","response","error","transition"] record) else d
 saveWork s (setWorkField "decisions" (arr (map update (workItems w "decisions"))) w)
accept :: Service -> Text -> Value -> Text -> Text -> IO ()
accept s a record before after = recordDecision s a (merge record (obj [("accepted",Bool True),("transition",obj [("before",str before),("event",get record "hostEvent" `fallback` get (get record "response") "event"),("after",str (transition before (at (get record "response") "event") after)),("nextNode",str after)])]))
 where transition b e def = fromMaybe def $ lookup (b,e) [(("Assessing","FitsSpecialization"),"ReadyToWork"),(("Assessing","OutsideSpecialization"),"ChiefTriage"),(("ChiefTriage","SpecialistSelected"),"Assessing"),(("ChiefTriage","NewSpecialistNeeded"),"CreatingSapi"),(("ChiefTriage","RequestUnclear"),"WaitingForAdmin"),(("ChiefTriage","FitsSpecialization"),"ReadyToWork")]
decide :: Service -> Run -> Turn -> AgentState -> Node -> IORef [Value] -> IO (DecisionResponse,Value)
decide s run turn state node logs = do
 (rendered,record) <- locked s $ do
  source <- templates s >>= maybe (failure 400 "Unknown decision node") pure . find ((==toJSON node) . (`get` "node"))
  (w,c) <- getWork s (turnId turn)
  conv <- conversation s (runOwner run) turn state
  chief <- DB.mainAgent s
  members <- team s
  extra <- if KM.member "agency_batch" (agentFields state) then ("\n\n" <>) <$> prompt s "agency-chat" else pure ""
  scheduled <- if has (workField w "origin") "routine" then ("\n\n" <>) <$> prompt s "scheduled-execution" else pure ""
  wiki <- prompt s "notes-wiki"
  envelope <- prompt s "decision-envelope"
  let instruction = at source "content" <> extra <> scheduled
      context = obj [("originalIntent",workField w "originalIntent"),("task",workField w "task"),("tracked",Bool (workTracked w)),("currentCall",toJSON c),("selfId",str (agentIdText (runOwner run))),("chiefId",str chief),("team",arr members),("routingHistory",arr [select ["callId","addressedTo","node"] (toJSON call) | call <- NE.toList (workCalls w)]),("conversation",conv),("observation",callField c "observation")]
      rendered = render envelope [("node",wireName node),("instruction",instruction),("notes_wiki",render wiki [("notes_example",T.pack (repositoryRoot s </> "corpora/prompts/examples/jarvis-notes.html"))]),("context",jsonText context),("events",T.intercalate ", " (sort (eventsFor node)))]
  (model,effort) <- modelDefaults
  policy <- executionSettings s (agentIdText (runOwner run))
  i <- newId
  t <- now
  let digest = T.pack (concatMap (\b -> let v=showHex b "" in if length v==1 then '0':v else v) (BS.unpack (hash (TE.encodeUtf8 instruction))))
      record = obj [("decisionId",str i),("callId",toJSON (turnId turn)),("node",toJSON node),("template",get source "template"),("promptHash",str digest),("templatePath",get source "path"),("renderedPrompt",str rendered),("model",str model),("effort",str (if executionMode policy == Deep then "xhigh" else effort)),("created",str t),("accepted",Bool False)]
      summary = merge (select ["decisionId","callId","node","promptHash","accepted"] record) (obj [("agent",str (agentIdText (runOwner run))),("promptTemplate",get record "template")])
  saveWork s (setWorkField "decisions" (arr (workItems w "decisions"++[summary])) w)
  writeJSON (archivePath s (agentIdText (runOwner run)) ("decisions/"<>i)) record
  pure (rendered,record)
 logIndex <- length <$> readIORef logs
 modifyIORef' logs (++[obj [("role",toJSON node),("prompt",str rendered),("session",Null)]])
 let updateLog extra = modifyIORef' logs (\entries -> [if index==logIndex then merge entry extra else entry | (index,entry)<-zip [0::Int ..] entries])
 rawResponse <- newIORef Nothing
 result <- try $ do
  raw <- invoke s run rendered
  writeIORef rawResponse (Just raw)
  activity <- readIORef (runActivity run)
  updateLog (obj ([("session",get activity "session"),("answer",str raw)]++[("warning",get activity "warning") | has activity "warning"]))
  require (T.length raw <= 100000) 400 "Decision response exceeds 100000 characters"
  response <- parseValue 400 (parseDecision node) (parseJSONText raw)
  pure (response,merge record (obj [("rawResponse",str raw),("response",decisionRaw response)]))
 case result of
  Left (e::SomeException) -> do
   activity <- readIORef (runActivity run)
   updateLog (obj [("session",get activity "session")])
   raw <- readIORef rawResponse
   let failed=put "error" (str (exceptionText e)) (maybe record (\value -> put "rawResponse" (str value) record) raw)
   locked s (recordDecision s (agentIdText (runOwner run)) failed)
   throwIO e
  Right value@(_,record') -> locked s (recordDecision s (agentIdText (runOwner run)) record') >> pure value
exceptionText :: SomeException -> Text
exceptionText e = case fromException e of Just (APIError _ m) -> m; _ -> T.pack (displayException e)
validateTarget :: Service -> Workload -> Text -> Text -> IO ()
validateTarget s w source target = do
 chief <- DB.mainAgent s
 require (target /= source && (source == chief || target == chief)) 400 "Routes are Chief → Sapi or Sapi → Chief"
 void (resolve s target False)
 state <- agentState s target
 require (not (any ((`elem` [Queued,Running]) . turnStatus) (agentTurns state))) 409 "Recipient is busy; responsibility remains with the sender"
 works <- readWorks s
 require (not (any (\c -> callState c == CallQueued && callOwner c == AgentId target) (concatMap (NE.toList . workCalls) works))) 409 "Recipient already has an accepted call"
 require (length (workCalls w) <= 8) 409 "Routing limit reached; Admin must resolve the request"
 require (target == chief || not (any ((==AgentId target) . callOwner) (workCalls w))) 409 "This specialist already assessed the request; ask Admin to resolve routing"
transfer :: Service -> Run -> Text -> Text -> Value -> IO Text
transfer s run request reason record = do
 (w,c) <- getWork s (runId run)
 stopped <- readIORef (runCancel run)
 require (not stopped) 409 "Stopped before handoff acceptance"
 let target=text (callField c "target")
 validateTarget s w (agentIdText (runOwner run)) target
 i <- newId
 chief <- DB.mainAgent s
 let child=setCallFields [("handoffReason",str reason)] (newCall (RunId i) (AgentId target) (Just (callIdentity c)) (QueuedAt (if target==chief then ChiefTriage else Assessing)) request)
     reply="Delegated to "<>target<>"."
     updated=c { callProgress=FinishedCall (HandedOff reply) }
     accepted=withCall w updated
 saveWork s accepted { workTracked=True, workCalls=workCalls accepted <> (child :| []) }
 accept s (agentIdText (runOwner run)) record "Delegation" (wireName (callNode child))
 pure reply
applyDecision :: Service -> Text -> Maybe Text -> Text -> IO Value
applyDecision s owner current decision = do
 identity <- maybe (failure 409 "Delegation requires this call's recorded Delegation decision") pure current
 (w,c) <- getWork s (RunId identity)
 require (callNode c==Delegation && any (\d -> at d "decisionId"==decision && at d "callId"==identity && at d "node"=="Delegation") (workItems w "decisions")) 409 "Delegation requires this call's recorded Delegation decision"
 record <- readJSON (archivePath s owner ("decisions/"<>decision)) empty
 require (get record "error"==Null && at (get record "response") "event"=="HandoffPrepared") 409 "Delegation decision has not produced a valid response"
 existing <- case find ((==Just (RunId identity)) . callParent) (workCalls w) of
  Just child -> pure child
  Nothing -> do
   runs <- readMVar (serviceRuns s)
   run <- maybe (failure 409 "AgencyRun is no longer active") pure (M.lookup (RunId identity) runs)
   response <- parseValue 409 (parseDecision Delegation) (get record "response")
   case decisionBody response of
    Handoff request -> void (transfer s run request (decisionReason response) record)
    _ -> failure 409 "Delegation requires a handoff decision"
   (updated,_) <- getWork s (RunId identity)
   maybe (failure 409 "Delegation acceptance was not saved") pure (find ((==Just (RunId identity)) . callParent) (workCalls updated))
 pure (obj [("saved",Bool True),("callId",toJSON (callIdentity existing)),("target",toJSON (callOwner existing))])
data DecisionError
 = AcceptedTaskNeedsExecution | OnlyChiefCreates | CriteriaRemoved
 | InvalidOutcome OutcomeError | InvalidAcceptedTask Text
 deriving (Eq, Show)
data DecisionStep
 = SaveDecision Workload WorkCall
 | ResolveSpecialist Text
 | EnsureSpecialist Text Text
 | DelegateRequest Text
 deriving (Eq, Show)
data SpecialistResolution = ExistingSpecialist AgentProfile | CreatedSpecialist AgentProfile deriving (Eq, Show)
newtype PreparedSpecialist = PreparedSpecialist { preparedSpecialistId :: AgentId }
 deriving (Eq, Show)

-- Decide from an immutable snapshot. The runtime interprets only the resulting work.
advanceDecision :: AgentId -> DecisionResponse -> Workload -> WorkCall -> Either DecisionError DecisionStep
advanceDecision chief response work call = case decisionBody response of
 ReplyNow reply
  | workTracked work -> Left AcceptedTaskNeedsExecution
  | otherwise -> Right (finish work (FinishedCall (Answered reply)))
 ExecuteTask -> Right (finish (work { workTracked=True }) (RunningAt ReadyToWork))
 OutsideSpecialization -> Right $ SaveDecision work $
  if callOwner call == chief then call { callProgress=RunningAt ChiefTriage }
  else (setCallFields [("target",toJSON chief)] call) { callProgress=RunningAt Delegation }
 SelectSpecialist name -> Right (ResolveSpecialist name)
 NeedSpecialist -> Right (finish work (RunningAt CreatingSapi))
 AskAdmin question -> Right (finish (work { workTracked=True }) (FinishedCall (NeedsClarification question)))
 SpecifySapi name role
  | callOwner call /= chief -> Left OnlyChiefCreates
  | otherwise -> Right (EnsureSpecialist name role)
 PrepareTask taskType spec
  | any (`notElem` map str (NE.toList (specCriteria spec))) acceptedCriteria -> Left CriteriaRemoved
  | otherwise ->
     let task=mergeFields [("taskType",str (if has (workField work "origin") "routine" then "automated" else taskType)),("specification",toJSON spec)] (workField work "task")
     in Right (finish (setWorkField "task" task work) (RunningAt Execution))
 Handoff request -> Right (DelegateRequest request)
 ReportOutcome reply outcome -> do
  criteria <- B.first (InvalidAcceptedTask . T.pack) (parseEither parseJSON (get (get (workField work "task") "specification") "completionCriteria"))
  B.first InvalidOutcome (validateOutcome response outcome criteria)
  let saved=mergeFields [("reply",str reply),("evidence",toJSON (decisionEvidence response)),("author",toJSON (callOwner call))] (get (decisionRaw response) "outcome")
      result=case outcomeStatus outcome of Completed -> Answered reply; Unresolved -> UnresolvedResult reply
  pure (finish (setWorkField "outcome" saved work) (FinishedCall result))
 where
 finish changed progress = SaveDecision changed (call { callProgress=progress })
 acceptedCriteria = items (get (workField work "task") "specification") "completionCriteria"

decisionFailure :: DecisionError -> IO a
decisionFailure problem = case problem of
 AcceptedTaskNeedsExecution -> failure 409 "An accepted Task must execute and report its completion criteria"
 OnlyChiefCreates -> failure 403 "Only Chief can create a Sapi"
 CriteriaRemoved -> failure 400 "A handoff cannot remove accepted completion criteria"
 InvalidOutcome reason -> failure 400 (outcomeError reason)
 InvalidAcceptedTask reason -> failure 500 reason

runDecision :: (AgentId -> IO PreparedSpecialist) -> Service -> Run -> Turn -> AgentState -> IO (Text,[Value])
runDecision prepareSpecialist s run turn state = do
 locked s (begin s (runOwner run) turn)
 logs <- newIORef []
 let loop n = do
      require (n < 12) 409 "Decision transition limit reached; Admin must resolve the request"
      (_,c) <- locked s (getWork s (turnId turn))
      case callOutput c of
       Just output -> pure output
       Nothing -> do
        let node=callNode c
        (response,record) <- decide s run turn state node logs
        result <- locked s $ do
         (current,call) <- getWork s (turnId turn)
         stopped <- readIORef (runCancel run)
         require (not stopped) 409 "Stopped by Admin"
         chief <- DB.mainAgent s
         let owner=agentIdText (runOwner run)
             finish work updated = do
              let following=setCallFields [("observation",decisionRaw response)] updated
                  next=callNode following
              saveWork s (withCall work following)
              accept s owner record (wireName node) (wireName next)
              pure (callOutput following)
         step <- either decisionFailure pure (advanceDecision (AgentId chief) response current call)
         case step of
          SaveDecision work updated -> finish work updated
          ResolveSpecialist name -> do
           target <- resolve s name False
           validateTarget s current owner target
           finish current ((setCallFields [("target",str target)] call) { callProgress=RunningAt Delegation })
          EnsureSpecialist name role -> do
           rows <- DB.agents s
           resolution <- case find ((==T.toCaseFold name) . T.toCaseFold . (`at` "name")) rows of
            Just row -> do
             require (at row "role" == role) 409 "Name already exists with a different role"
             ExistingSpecialist <$> decodeValue 500 row
            Nothing -> CreatedSpecialist <$> (decodeValue 500 =<< createAgent s (obj [("name",str name),("role",str role),("manager",str owner)]))
           let (profile,created) = case resolution of
                ExistingSpecialist existing -> (existing,False)
                CreatedSpecialist new -> (new,True)
           prepared <- prepareSpecialist (profileId profile)
           require (preparedSpecialistId prepared == profileId profile) 500 "Prepared specialist does not match the selected Sapi"
           finish current ((setCallFields [("target",toJSON (preparedSpecialistId prepared)),("creation",obj [("name",str name),("role",str role)]),("creationReceipt",obj [("agent",toJSON profile),("created",Bool created)])] call) { callProgress=RunningAt Delegation })
          DelegateRequest request -> Just <$> transfer s run request (decisionReason response) record
        maybe (loop (n+1)) pure result
 output <- loop (0::Int) `catch` (\(e::SomeException) -> do
  locked s $ do
   (work,_) <- getWork s (turnId turn)
   case listToMaybe (reverse [d | d<-workItems work "decisions",get d "callId"==toJSON (turnId turn)]) of
    Just summary | not (boolean (get summary "accepted")) -> do
     record <- readJSON (archivePath s (agentIdText (runOwner run)) ("decisions/"<>at summary "decisionId")) empty
     recordDecision s (agentIdText (runOwner run)) (put "error" (str (exceptionText e)) record)
    _ -> pure ()
  readIORef logs >>= throwIO . RunFailure (exceptionText e))
 (output,) <$> readIORef logs
reconcileCall :: [Value] -> WorkCall -> Validation WorkCall
reconcileCall turns call = case findId (runIdText (callIdentity call)) turns of
 Just turn | isNothing (callOutput call) -> do
  status <- validateValue 500 parseJSON (get turn "status")
  let failed progress = (setCallFields [("error",get turn "error")] call) { callProgress=progress (callNode call) }
  pure $ case status of
   Failed -> failed FailedAt
   Interrupted -> failed InterruptedAt
   Cancelled -> failed CancelledAt
   Queued -> call
   Running -> call
   OutputPending -> call
   Done -> call
   Warning -> call
 _ -> Right call
reconcileWorks :: Service -> IO ()
reconcileWorks s = do
 turns <- DB.turns s
 works <- readWorks s
 forM_ works $ \work -> do
  updated <- validated (traverse (reconcileCall turns) (workCalls work))
  when (updated /= workCalls work) $ saveWork s work { workCalls=updated }
pendingWorks :: Service -> IO ()
pendingWorks s = do
 works <- readWorks s
 existing <- DB.turns s
 forM_ works $ \w -> forM_ (workCalls w) $ \c -> when (callState c == CallQueued && isJust (callParent c) && isNothing (findId (runIdText (callIdentity c)) existing)) $ do
  let a=agentIdText (callOwner c)
      parent=find ((==callParent c) . Just . callIdentity) (workCalls w)
      origin=mergeFields [("workloadId",str (workloadId w)),("taskId",get (workField w "task") "taskId"),("caller",maybe Null (toJSON . callOwner) parent),("parentCall",toJSON (callParent c))] (remove ["agent","call"] (workField w "origin"))
  time <- now
  let turn=toJSON (newTurn (callIdentity c) Chat (callRequest c) time origin (has origin "routine"))
      message=obj [("role",str "user"),("content",str (callRequest c)),("turn",toJSON (callIdentity c)),("time",str time),("origin",origin)]
  modifyState s a $ \state -> if isJust (findId (runIdText (callIdentity c)) (items state "turns")) then state else appendItems "chat" [message] (appendItems "turns" [turn] state)
  DB.project s a
clarify :: Service -> Text -> Text -> Text -> IO Text
clarify s a wid message = do
 works <- readWorks s
 w <- maybe (failure 409 "Clarification requires the waiting Chief call") pure (find ((==wid) . workloadId) works)
 chief <- DB.mainAgent s
 let previous=NE.last (workCalls w)
 require (callState previous == CallWaiting && a==chief) 409 "Clarification requires the waiting Chief call"
 i <- newId
 let call=setCallFields [("activation",str "RequestClarified")] (newCall (RunId i) (AgentId a) (Just (callIdentity previous)) (QueuedAt ChiefTriage) message)
 saveWork s w { workCalls=workCalls w <> (call :| []) }
 pendingWorks s
 pure i
workTasks :: Service -> Text -> Bool -> IO Value
workTasks s a full = do
 works <- readWorks s
 rows <- forM [w | w <- works,workTracked w,any ((==AgentId a) . callOwner) (workCalls w)] $ \w -> do
  let lastCall=NE.last (workCalls w)
  decisions <- forM (workItems w "decisions") $ \d -> if full then merge d <$> readJSON (archivePath s (at d "agent") ("decisions/"<>at d "decisionId")) empty else pure d
  pure $ mergeFields [("execution",obj [("state",toJSON (callState lastCall)),("owner",toJSON (callOwner lastCall)),("currentCall",toJSON (callIdentity lastCall)),("attempt",callField lastCall "attempt")]),("decisions",arr decisions)] (select ["task","workloadId","originalIntent","calls","outcome"] (toJSON w))
 pure (obj [("tasks",arr rows)])
taskList :: Service -> Text -> IO Value
taskList s a = do
 tasks <- items <$> workTasks s a False <*> pure "tasks"
 pure $ obj [("tasks",arr (reverse [let body=get (get t "task") "specification" `fallback` obj [("objective",get t "originalIntent")]; execution=get t "execution"; calls=items t "calls" in obj [("id",get t "workloadId"),("title",get body "objective"),("taskType",get (get t "task") "taskType"),("state",get execution "state"),("owner",get execution "owner"),("sender",maybe Null (`get` "addressedTo") (listToMaybe calls)),("body",str (jsonText body <> "\n")),("result",get (get t "outcome") "reply"),("error",maybe Null (`get` "error") (listToMaybe (reverse calls)))] | t <- tasks]))]
workSummaries :: [Workload] -> [Value]
workSummaries works = [let c=NE.last (workCalls w) in obj [("taskType",get (workField w "task") "taskType"),("id",str (workloadId w)),("taskId",get (workField w "task") "taskId"),("origin",workField w "origin"),("callId",toJSON (callIdentity c)),("owner",toJSON (callOwner c)),("state",toJSON (callState c)),("participants",toJSON (nub (map callOwner (NE.toList (workCalls w))))),("output",toJSON (callOutput c)),("error",callField c "error"),("decisions",num (length (workItems w "decisions"))),("calls",num (length (workCalls w)))] | w <- works,workTracked w]
