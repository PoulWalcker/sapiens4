{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Werror=incomplete-patterns #-}
module Execution where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception
import Control.Monad
import Data.Aeson hiding (pairs)
import Data.Aeson.Types (parseEither)
import qualified Data.ByteString.Lazy as BL
import Data.IORef
import Data.List (find,nub)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Clock
import System.Directory
import System.IO (hPutStrLn, stderr)
import Types
import qualified Store as DB
import Population
import Workspace
import qualified Delegation as D
import qualified Routines as R
import Provider
import qualified GroupChat as G

data ConversationArchive = ConversationArchive Text [Value]
data Retention = Retention Value [ConversationArchive]

-- Archive decisions depend only on the snapshot. Writing archives still precedes
-- saving the reduced state, so interruption cannot discard unarchived history.
retainConversation :: Value -> Retention
retainConversation state = foldl (apply bounded) (foldl (apply counted) (Retention state []) fields) fields
 where
 fields = ["chat","events","turns"]
 pending row = at row "status" `elem` ["queued","running","output_pending"]
 apply prune (Retention current archives) field =
  let (result,removed) = prune field current
  in Retention result (archives ++ [ConversationArchive field removed | not (null removed)])
 counted field current =
  let settled = filter (not . pending) (items current field)
      removed = take (max 0 (length settled-100)) settled
  in (put field (arr (filter (`notElem` removed) (items current field))) current,removed)
 bounded field current = prune current [] (items current field)
  where
  prune value removed [] = (value,removed)
  prune value removed (row:rest)
   | BL.length (encode value)<=800000 = (value,removed)
   | pending row || (field=="turns" && Just row==listToMaybe (reverse (items value field))) = prune value removed rest
   | otherwise = prune (put field (arr (filter (/=row) (items value field))) value) (removed++[row]) rest

trimConversation :: Service -> Text -> IO ()
trimConversation s a = do
 state <- stateJSON <$> agentState s a
 let Retention final archives = retainConversation state
 forM_ archives $ \(ConversationArchive field rows) -> newId >>= \i -> writeJSON (archivePath s a (field<>"/"<>i)) (arr rows)
 when (final/=state) (decodeValue 500 final >>= saveState s a)
submitTurn :: Service -> Text -> Value -> IO Value
submitTurn s a d = do
 input <- either (failure 400) pure (messageText (get d "text"))
 let flowValue=get d "flow" `fallback` str "chat"
 attachments <- resolveAttachments s a (get d "attachments" `fallback` arr []) True
 require (not (T.null input) || not (null attachments)) 400 "Write a message or attach a file"
 flow <- decodeValue 400 flowValue
 binary <- codexBinary
 require (isJust binary) 503 "Codex CLI is missing. Install it and run codex login."
 when (flow==Computer) $ blindly s >>= doesFileExist >>= \built -> require built 503 "Blindly4 is not built. Run ./build/start.sh to build it."
 stopped <- readIORef (serviceStopping s)
 require (not stopped) 503 "Server is shutting down"
 requireActive s a
 routine <- R.command s a input (not (null attachments))
 case routine of
  Just r -> R.acknowledge s a input r
  Nothing -> do
   suffix <- attachmentPrompt s attachments
   let message=input<>suffix
   i <- if has d "workload" then D.clarify s a (at d "workload") message else do
    i <- newId
    time <- now
    agentState s a >>= saveState s a . enqueueTurn (RunId i) flow message time
    pure i
   DB.message s i input attachments
   DB.project s a
   trimConversation s a
   pure (obj [("id",str i),("agent",str a),("status",str "queued"),("flow",toJSON flow)])
enqueueTurn :: RunId -> Flow -> Text -> Text -> AgentState -> AgentState
enqueueTurn identity flow input time state = mapStateFields (appendItems "chat" [message])
 state { agentTurns=agentTurns state ++ [newTurn identity flow input time Null True] }
 where
 message = obj [("role",str "user"),("content",str input),("turn",toJSON identity),("time",str time),("origin",Null)]

turnAction :: Service -> Text -> Text -> Text -> IO Value
turnAction s a identity rawAction = do
 requireActive s a
 action <- decodeValue 404 (str rawAction)
 state <- agentState s a
 turn <- case find ((==RunId identity) . turnId) (agentTurns state) of
  Just active -> pure active
  Nothing -> do
   require (action == Retry) 404 "Unknown active turn"
   stored <- maybe (failure 404 "Unknown turn") pure . findId identity =<< DB.turns s
   origin <- readExecutionOrigin (get stored "origin")
   require (at stored "agent" == a && isJust (originPolicyRoutine origin)) 404 "Unknown active turn"
   status <- field 500 stored "status"
   turnResult (validateTurnAction Retry status)
   archived <- retainedTurn s a (RunId identity) >>= maybe (failure 409 "Retained setup turn is unavailable; ask Sapi to finish routine setup in chat") pure
   require (turnStatus archived == status && isJust (turnAttempt archived) && turnField archived "origin" == get stored "origin") 409 "Retained setup turn does not match its saved history; ask Sapi to finish routine setup in chat"
   pure archived
 let restored=if any ((==turnId turn) . turnId) (agentTurns state) then state else state { agentTurns=agentTurns state++[turn] }
 when (action == Retry) $ do
  void (executionOrigin (turnOrigin turn))
  G.checkPolicyRetry s turn
 work <- D.findWork s (RunId identity)
 require (not (maybe False (any ((==Just (RunId identity)) . D.callParent) . D.workCalls . fst) work)) 409 "This request was delegated; use the recipient call controls"
 runs <- readMVar (serviceRuns s)
 change <- turnResult (controlTurn action (M.member (batchLeader turn) runs) turn restored)
 when (action == Retry) $ do
  contexts <- G.requestOrigins s
  forM_ [member | member <- agentTurns restored, batchLeader member == batchLeader turn] $ \member -> do
   void (either (failure 500) pure (G.checkedTurnContext contexts member))
   R.ensureRetryAvailable s member
 case change of
  SaveTurns changed -> saveState s a changed
  StopTurn identity -> forM_ (M.lookup identity runs) $ \run -> writeIORef (runCancel run) True
 DB.project s a
 trimConversation s a
 D.reconcileWorks s
 updated <- agentState s a
 pure (obj [("id",str identity),("status",maybe Null (toJSON . turnStatus) (find ((==RunId identity) . turnId) (agentTurns updated)))])
data TurnChange = SaveTurns AgentState | StopTurn RunId
controlTurn :: TurnAction -> Bool -> Turn -> AgentState -> Either TurnControlError TurnChange
controlTurn action running turn state = do
 validateTurnAction action (turnStatus turn)
 let leader = batchLeader turn
     update f = mapTurns (\t -> if batchLeader t == leader then f t else t) state
 pure $ case action of
  Retry -> SaveTurns (update retryTurn)
  Cancel | running -> StopTurn leader
  Cancel -> SaveTurns (update (\t -> t { turnStatus=Cancelled }))

recover :: Service -> Text -> IO ()
recover s a = do
 modifyTurns s a recoverTurn
 DB.project s a
data RunOutcome = RunCompleted Text [Value] | RunRejected Text [Value] | RunStopped Text [Value]

runOutcome :: Bool -> Either SomeException (Text,[Value]) -> RunOutcome
runOutcome stopped result
 | stopped = RunStopped output logs
 | Just reason <- error = RunRejected reason logs
 | otherwise = RunCompleted output logs
 where
 (output,logs,error)=case result of
  Right (value,entries) -> (value,entries,Nothing)
  Left exception -> case fromException exception of
   Just (RunFailure reason entries) -> ("",entries,Just reason)
   Nothing -> ("",[],Just (D.exceptionText exception))

outcomeFields :: RunOutcome -> (Text,[Value],Maybe Text)
outcomeFields outcome = case outcome of
 RunCompleted output logs -> (output,logs,Nothing)
 RunRejected reason logs -> ("",logs,Just reason)
 RunStopped output logs -> (output,logs,Just "Stopped by Admin. Saved work is preserved; review external effects before retrying.")

settleRun :: ExecutionOrigin -> RunOutcome -> Text -> [RunId] -> Turn -> AgentState -> AgentState
settleRun parsed outcome time ids turn state = result
 where
 i=runIdText (turnId turn)
 (output,logs,failureText)=outcomeFields outcome
 failedStatus=case outcome of RunStopped _ _->Interrupted; _->Failed
 buffered=turnField turn "agency_kind"==str "chatInput"
 origin=turnField turn "origin"
 warnings=[at entry "warning" | entry<-logs,has entry "warning"]
 update t | turnId t `notElem` ids = t
          | otherwise = withTurnFields warningFields $ case failureText of
             Just e -> (withTurnFields (put "error" (str e)) t) { turnStatus = failedStatus }
             Nothing | buffered -> t { turnStatus = OutputPending }
                     | otherwise -> (withTurnFields (put "output_at" (str time)) t) { turnStatus = Done }
 warningFields = if null warnings then remove ["warning"] else put "warning" (str (T.intercalate " " warnings))
 updated=mapTurns update state
 result | isJust failureText = updated
        | buffered = mapStateFields (appendItems "output_buffer" [obj [("turn",str i),("members",toJSON ids),("content",str output),("origin",origin)]]) updated
        | isJust (originGroup parsed) = updated
        | otherwise = mapStateFields (put "last_output" (str output) . appendItems "chat" [obj [("role",str "agent"),("content",str output),("turn",str i),("time",str time),("origin",origin)]]) updated

finish :: Service -> Run -> Turn -> Either SomeException (Text,[Value]) -> IO ()
finish s run turn outcome = do
 stop <- readIORef (runCancel run)
 parsed <- executionOrigin (turnOrigin turn)
 checked <- case outcome of
  Right (output,logs) | not stop, Just group <- originGroup parsed -> do
   verification <- try (R.verifyPolicyPreparation s (GroupOwner group) (turnId turn) parsed)
   case verification of
    Right () -> pure (Right (output,logs))
    Left (problem :: SomeException)
     | isJust (fromException problem :: Maybe SomeAsyncException) -> throwIO problem
     | isJust (fromException problem :: Maybe AsyncCancelled) -> throwIO problem
     | otherwise -> pure (Left (toException (RunFailure (D.exceptionText problem) logs)))
  _ -> pure outcome
 let a=agentIdText (runOwner run); i=runIdText (runId run)
     completed=runOutcome stop checked
     (output,logs,failureText)=outcomeFields completed
 writeJSON (archivePath s a ("runs/"<>i)) (obj [("output",str output),("logs",arr logs),("error",maybe Null str failureText)])
 state <- agentState s a
 time <- now
 ids <- decodeValue 500 (turnField turn "batch_members" `fallback` toJSON [turnId turn]) :: IO [RunId]
 let result = settleRun parsed completed time ids turn state
 saveState s a result
 DB.project s a
 trimConversation s a
 D.reconcileWorks s
reserveBatch :: NonEmpty Turn -> Pulse -> Int -> Bool -> AgentState -> (AgentState,AgentState,Turn)
reserveBatch batch tick slot batching state = (marked,snapshot,execution)
 where
 first=NE.head batch; i=turnId first; ids=map turnId (NE.toList batch)
 reserve t
  | turnId t `notElem` ids = t
  | otherwise = (withTurnFields (mergeFields (batchFields ++ [("batch_members",toJSON ids) | batching && turnId t==i]) . remove ["error"]) t) { turnStatus = Running, turnBatchId = if batching then Just i else turnBatchId t }
 batchFields = if batching then [("agency_kind",str "chatInput"),("slot",num slot),("pulseId",str (pulseIdentity tick))] else []
 marked=mapTurns reserve state
 snapshot=mapStateFields (mergeFields ([("current_turn",toJSON i)] ++ [("agency_batch",arr [obj [("id",toJSON (turnId t)),("content",str (turnInput t))] | t <- NE.toList batch]) | batching])) marked
 execution=(reserve first) { turnInput = T.intercalate "\n\n" (map turnInput (NE.toList batch)) }

launch :: (AgentId -> IO ()) -> Service -> Text -> G.GroupOrigins -> NonEmpty Turn -> Pulse -> Int -> Bool -> IO ()
launch prepareSpecialist s a contexts batch tick slot batching = do
 origins <- traverse (either (failure 500) pure . G.checkedTurnContext contexts) batch
 let parsed=NE.head origins
 state <- agentState s a
 let (marked,snapshot,execution) = reserveBatch batch tick slot batching state
     i=turnId (NE.head batch)
 saveState s a marked
 cancel <- newIORef False
 process <- newMVar Nothing
 activity <- newIORef empty
 deadline <- (+1800000000000) <$> getMonotonicTimeNSec
 let run=Run (AgentId a) i slot cancel process activity deadline
 modifyMVar_ (serviceRuns s) (pure . M.insert i run)
 DB.pulseCall s tick a "chatInput" slot (nub [groupIdText group | origin <- NE.toList origins, group <- maybeToList (originGroup origin)])
 DB.project s a
 void $ forkIO $ do
  let verifyCompletion work = do
       origin <- executionOrigin (D.workOrigin work)
       let owner=PersonalOwner (D.callOwner (NE.head (D.workCalls work)))
       R.verifyPolicyPreparation s owner (RunId (D.workloadId work)) origin
  result <- try (withActor s a (Just "chatInput") (if isJust (originGroup parsed) then G.runGroup s run execution else D.runDecision prepareSpecialist verifyCompletion s run execution snapshot))
  locked s $ finish s run execution result `finally` do
   lease <- readIORef (serviceLease s)
   when (lease==Just (AgentId a,i)) $ writeIORef (serviceLease s) Nothing
   modifyMVar_ (serviceRuns s) (pure . M.delete i)
   D.pendingWorks s
   G.reconcileGroups s
   G.dispatchGroups s
bufferIdentity :: Value -> Either Text RunId
bufferIdentity item = do
 identity <- either (Left . T.pack) Right (parseEither parseJSON (get item "turn"))
 unless (not (T.null (T.strip (runIdText identity)))) (Left "Buffered output requires a Turn identity")
 pure identity

checkedBufferedOrigin :: G.GroupOrigins -> AgentState -> Value -> Either Text ExecutionOrigin
checkedBufferedOrigin contexts state item = do
 identity <- bufferIdentity item
 actual <- G.checkedOriginContext contexts identity (storeOrigin (Just (get item "origin")))
 forM_ (find ((==identity) . turnId) (agentTurns state)) $ \turn -> do
  expected <- G.checkedTurnContext contexts turn
  unless (originPurpose actual == originPurpose expected && originGroup actual == originGroup expected)
   (Left "Buffered output provenance does not match its Turn")
 pure actual

rejectInvalidOutputs :: Service -> Text -> G.GroupOrigins -> IO ()
rejectInvalidOutputs s owner contexts = do
 state <- agentState s owner
 forM_ (items (stateJSON state) "output_buffer") $ \item -> case checkedBufferedOrigin contexts state item of
  Right _ -> pure ()
  Left reason -> do
   archive <- newId
   writeJSON (archivePath s owner ("rejected-outputs/" <> archive)) (obj [("item",item),("error",str reason)])
   let identity=case bufferIdentity item of Right saved -> Just saved; Left _ -> Nothing
   DB.record s [owner] (maybe archive runIdText identity) "call.output_rejected" (obj [("item",item),("error",str reason)]) owner Nothing
   current <- agentState s owner
   let changed=mapTurns (\turn -> case identity of
        Just saved | batchLeader turn == saved || turnId turn == saved -> (withTurnFields (put "error" (str reason)) turn) { turnStatus=Failed }
        _ -> turn) current
   saveState s owner (mapStateFields (put "output_buffer" (arr (filter (/=item) (items (stateJSON changed) "output_buffer")))) changed)
   DB.project s owner

publish :: Service -> Text -> G.GroupOrigins -> Pulse -> IO ()
publish s a contexts tick = do
 rejectInvalidOutputs s a contexts
 forM_ [1,2] $ \slot -> do
  state <- agentState s a
  let document=stateJSON state
      scheduled=pulseFrequency tick==Bph60
  candidates <- forM (items document "output_buffer") $ \item -> do
   origin <- either (failure 500) pure (checkedBufferedOrigin contexts state item)
   pure (item,origin)
  case find ((==scheduled) . originIsScheduled . snd) candidates of
   Nothing -> pure ()
   Just (item,parsed) -> do
    time <- now
    ids <- field 500 item "members" :: IO [RunId]
    let sequence=int (get document "output_sequence")+1
        origin=toJSON parsed
        grouped=isJust (originGroup parsed)
        updated=mapTurns (\t -> if turnId t `elem` ids then (withTurnFields (mergeFields [("output_at",str time),("output_order",num sequence)]) t) { turnStatus = Done } else t) state
        messages=if grouped || any (\m -> get m "turn"==get item "turn" && at m "role"=="agent") (items document "chat") then items document "chat" else items document "chat"++[obj [("role",str "agent"),("content",get item "content"),("turn",get item "turn"),("time",str time),("origin",origin)]]
    saveState s a $ mapStateFields (mergeFields ([("output_buffer",arr (filter (/=item) (items document "output_buffer"))),("output_sequence",num sequence),("chat",arr messages)] ++ [("last_output",get item "content") | not grouped])) updated
    DB.pulseCall s tick a "chatOutput" slot (map groupIdText (maybeToList (originGroup parsed)))
    DB.project s a
    trimConversation s a

admit :: (AgentId -> IO ()) -> Service -> Text -> G.GroupOrigins -> Pulse -> IO ()
admit prepareSpecialist s a contexts tick = do
 runs <- readMVar (serviceRuns s)
 state <- agentState s a
 let reject turn = case (turnStatus turn,G.checkedTurnContext contexts turn) of
      (Queued,Left reason) -> (withTurnFields (put "error" (str reason)) turn) { turnStatus=Failed }
      _ -> turn
     checked=mapTurns reject state
 when (checked /= state) $ do
  saveState s a checked
  DB.project s a
  D.reconcileWorks s
 candidates <- forM [turn | turn <- agentTurns checked, turnStatus turn == Queued] $ \turn -> do
  origin <- either (failure 500) pure (G.checkedTurnContext contexts turn)
  pure (turn,origin)
 mapM_ (\(Admission batch slot batching) -> launch prepareSpecialist s a contexts batch tick slot batching)
  (admission (AgentId a) [(runOwner run,runSlot run) | run <- M.elems runs] candidates tick)

data Admission = Admission (NonEmpty Turn) Int Bool
admission :: AgentId -> [(AgentId,Int)] -> [(Turn,ExecutionOrigin)] -> Pulse -> Maybe Admission
admission owner runs candidates tick
 | owner `notElem` owners && length owners>=4 = Nothing
 | first:_ <- legacy, null mine, not scheduled = Just (Admission (first :| []) 0 False)
 | slot:_ <- available, Just batch <- NE.nonEmpty queued, all (/=0) mine = Just (Admission (if scheduled then NE.head batch :| [] else batch) slot True)
 | otherwise = Nothing
 where
 owners=nub (map fst runs)
 mine=[slot | (agent,slot)<-runs,agent==owner]
 available=[n | n<-[1,2],n `notElem` mine]
 scheduled=pulseFrequency tick==Bph60
 queued=[t | (t,origin)<-candidates,boolean (turnField t "batchable"),originIsScheduled origin==scheduled]
 legacy=[t | (t,_)<-candidates,not (boolean (turnField t "batchable"))]
onPulse :: (AgentId -> IO ()) -> Service -> Pulse -> IO ()
onPulse prepareSpecialist s tick = do
 R.dispatch s tick
 when (pulseFrequency tick==Bph60 || (pulseFrequency tick==Bpm60 && even (pulseNumber tick))) $ do
  D.pendingWorks s
  G.reconcileGroups s
  G.dispatchGroups s
  contexts <- G.requestOrigins s
  rows <- DB.agents s
  forM_ rows $ \row -> do
   let a=at row "id"
   inactive <- retired s a
   unless inactive $ do
    publish s a contexts tick
    admit prepareSpecialist s a contexts tick
    when (pulseFrequency tick==Bph60) $ admit prepareSpecialist s a contexts tick
runPulse :: (AgentId -> IO ()) -> Service -> Pulse -> IO ()
runPulse prepareSpecialist s tick = try (onPulse prepareSpecialist s tick) >>= either report pure
 where report exception
        | isJust (fromException exception :: Maybe SomeAsyncException) = throwIO exception
        | isJust (fromException exception :: Maybe AsyncCancelled) = throwIO exception
        | otherwise = hPutStrLn stderr ("Pulse " <> show tick <> ": " <> show exception)
advanceDeadline :: Frequency -> Int -> Int -> (Int,Int)
advanceDeadline frequency time due = (elapsed,due+elapsed*interval)
 where
 interval=periodMillis frequency
 elapsed=(time-due) `div` interval+1

milliseconds :: IO Int
milliseconds = fromIntegral . (`div` 1000000) <$> getMonotonicTimeNSec
startExecution :: (AgentId -> IO ()) -> Service -> IO ()
startExecution prepareSpecialist s = modifyMVar_ (serviceWorker s) $ \existing -> case existing of
 Just _ -> pure existing
 Nothing -> Just <$> async loop
 where
 loop = do
  t <- milliseconds
  deadlines <- newIORef (M.fromList [(f,t+periodMillis f) | f <- [minBound..maxBound]])
  let step = do
       stopped <- readIORef (serviceStopping s)
       unless stopped $ do
        locked s $ do
         config <- corporaSettings s
         pulse <- readIORef (servicePulse s)
         let active=corporaStatus config==Active
         writeIORef (servicePulse s) (put "running" (Bool active) pulse)
         time <- milliseconds
         if not active then writeIORef deadlines (M.fromList [(f,time+periodMillis f) | f <- [minBound..maxBound]]) else forM_ [minBound..maxBound] $ \frequency -> do
          let key=wireName frequency
          due <- fromMaybe time . M.lookup frequency <$> readIORef deadlines
          when (time>=due) $ do
           let (elapsed,next)=advanceDeadline frequency time due
           modifyIORef' deadlines (M.insert frequency next)
           current <- readIORef (servicePulse s)
           let number=int (get (get current "numbers") key)+elapsed; numbers=put key (num number) (get current "numbers"); identity=at current "id"
           writeIORef (servicePulse s) (put "numbers" numbers current)
           DB.saveSingleton s DB.SystemPulse (obj [("pulse",str identity),("numbers",numbers)])
           runPulse prepareSpecialist s (Pulse identity frequency number)
        threadDelay 50000
        step
  step
stopRuns :: Service -> IO ()
stopRuns s = readMVar (serviceRuns s) >>= mapM_ (\run -> writeIORef (runCancel run) True) . M.elems
