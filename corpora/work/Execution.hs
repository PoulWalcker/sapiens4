{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Werror=incomplete-patterns #-}
module Execution where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception
import Control.Monad
import Data.Aeson hiding (pairs)
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
import Types
import qualified Store as DB
import qualified ManagedState as Managed
import qualified Command as ScheduleCommand
import Population
import Workspace
import qualified Delegation as D
import qualified Routines as R
import Provider
import qualified GroupChat as G

trimConversation :: Service -> Text -> IO ()
trimConversation s a = do
 state <- stateJSON <$> agentState s a
 bounded <- foldM (\current field -> do
  let rows=items current field
      settled=filter (\row -> at row "status" `notElem` ["queued","running","output_pending"] && not (has row "managed")) rows
      removed=take (max 0 (length settled-100)) settled
  unless (null removed) $ newId >>= \i -> writeJSON (archivePath s a (field<>"/"<>i)) (arr removed)
  pure (put field (arr (filter (`notElem` removed) rows)) current)) state ["chat","events","turns"]
 final <- foldM (\current field -> do
  let prune value removed [] = pure (value,removed)
      prune value removed (row:rest)
       | BL.length (encode value)<=800000 = pure (value,removed)
       | has row "managed" || at row "status" `elem` ["queued","running","output_pending"] || (field=="turns" && Just row==listToMaybe (reverse (items value field))) = prune value removed rest
       | otherwise = prune (put field (arr (filter (/=row) (items value field))) value) (removed++[row]) rest
  (result,removed) <- prune current [] (items current field)
  unless (null removed) $ newId >>= \i -> writeJSON (archivePath s a (field<>"/"<>i)) (arr removed)
  pure result) bounded ["chat","events","turns"]
 when (final/=state) (decodeValue 500 final >>= saveState s a)
submitTurn :: Service -> Text -> Value -> IO Value
submitTurn s a d = do
 let raw=get d "text" `fallback` str ""; input=T.strip (text raw); flowValue=get d "flow" `fallback` str "chat"
 require (case raw of String t -> T.length t<=16000; _ -> False) 400 "Message must be text, at most 16000 characters"
 attachments <- resolveAttachments s a (get d "attachments" `fallback` arr []) True
 require (not (T.null input) || not (null attachments)) 400 "Write a message or attach a file"
 flow <- decodeValue 400 flowValue
 managed <- decodeValue 400 (get d "managed" `fallback` Bool False)
 when managed $ do
  require (flow==Chat && not (has d "workload")) 400 "Managed execution currently supports ordinary chat submissions only"
  require (ScheduleCommand.interpret (not (null attachments)) (T.unpack input)==Right Nothing) 400 "Managed execution does not accept schedule commands"
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
    let typed=newTurn (RunId i) flow message time Null (not managed)
        turn=toJSON typed
        msg=obj [("role",str "user"),("content",str message),("turn",str i),("time",str time),("origin",Null)]
    when managed (enrollManaged s a typed msg input attachments)
    modifyState s a $ \state -> merge state (obj ([("turns",arr (items state "turns"++[turn])) | not managed]++[("chat",arr (items state "chat"++[msg])) | not managed]))
    pure i
   DB.message s i input attachments
   DB.project s a
   trimConversation s a
   pure (obj [("id",str i),("agent",str a),("status",str "queued"),("flow",toJSON flow)])
turnAction :: Service -> Text -> Text -> Text -> IO Value
turnAction s a identity rawAction = turnCommand s a identity rawAction Nothing
turnCommand :: Service -> Text -> Text -> Text -> Maybe Value -> IO Value
turnCommand s a identity rawAction external = do
 requireActive s a
 action <- decodeValue 404 (str rawAction)
 state <- agentState s a
 turn <- maybe (failure 404 "Unknown active turn") pure (find ((==RunId identity) . turnId) (agentTurns state))
 when (rawAction=="retry") $ D.requireCallAllowed s turn
 work <- D.findWork s (RunId identity)
 require (not (maybe False (any ((==Just (RunId identity)) . D.callParent) . D.workCalls . fst) work)) 409 "This request was delegated; use the recipient call controls"
 command <- if has (toJSON turn) "managed" then do
  cmd <- case external of
   Just value -> pure value
   Nothing -> do i<-newId; pure (obj [("id",str i),("action",str rawAction),("revision",get (turnField turn "managed") "revision")])
  pure (Just (identity,cmd))
  else pure Nothing
 let duplicate=maybe False ((`elem` items (turnField turn "managed") "commands") . snd) command
     leader=batchLeader turn
     update f = mapTurns (\t -> if batchLeader t == leader then f t else t)
 if duplicate then saveStateCommand s a state command else case action of
  Retry -> do
   require (retryableStatus (turnStatus turn)) 409 "Only failed or interrupted batches can be retried"
   saveStateCommand s a (update (withTurnFields (remove ["cancel_requested"]) . retryTurn) state) command
  Cancel -> do
   require (turnStatus turn `notElem` [Done,OutputPending]) 409 "Completed input cannot be cancelled"
   runs <- readMVar (serviceRuns s)
   case M.lookup leader runs of
    Just run -> do
     if isJust command then saveStateCommitted s a (update (withTurnFields (put "cancel_requested" (Bool True))) state) command (writeIORef (runCancel run) True)
      else writeIORef (runCancel run) True
    Nothing -> saveStateCommand s a (update (\t -> t { turnStatus = Cancelled }) state) command
 DB.project s a
 trimConversation s a
 D.reconcileWorks s
 updated <- agentState s a
 pure (obj [("id",str identity),("status",maybe Null (toJSON . turnStatus) (find ((==RunId identity) . turnId) (agentTurns updated)))])
recover :: Service -> Text -> IO ()
recover s a = do
 restoreManaged s a
 modifyTurns s a $ \t -> if turnStatus t == Running then
  (withTurnFields (put "error" (str "Runner stopped before saving a reply; inspect effects before retrying")) t) { turnStatus = Interrupted } else t
 DB.project s a
finish :: Service -> Run -> Turn -> Either SomeException (Text,[Value]) -> IO ()
finish s run turn outcome = do
 fenced <- not <$> D.callAllowed s turn
 stop <- (||fenced) <$> readIORef (runCancel run)
 let a=agentIdText (runOwner run); i=runIdText (runId run)
     (rawOutput,logs,error)=case outcome of
      Left e -> case fromException e of
       Just (RunFailure message entries)->("",entries,Just message)
       Nothing->("",[],Just (D.exceptionText e))
      Right (value,entries)->(value,entries,Nothing)
     output=if fenced then "" else rawOutput
     failureText=if stop then Just "Stopped by Admin. Saved work is preserved; review external effects before retrying." else error
 writeJSON (archivePath s a ("runs/"<>i)) (obj [("output",str output),("logs",arr logs),("error",maybe Null str failureText)])
 state <- agentState s a
 time <- now
 ids <- decodeValue 500 (turnField turn "batch_members" `fallback` toJSON [turnId turn]) :: IO [RunId]
 let buffered=turnField turn "agency_kind"==str "chatInput"
     origin=turnField turn "origin"
     warnings=[at entry "warning" | entry<-logs,has entry "warning"]
     update t | turnId t `notElem` ids = t
              | otherwise = withTurnFields warningFields $ case failureText of
                 Just e -> (withTurnFields (put "error" (str e)) t) { turnStatus = if stop then Interrupted else Failed }
                 Nothing | buffered -> t { turnStatus = OutputPending }
                         | otherwise -> (withTurnFields (put "output_at" (str time)) t) { turnStatus = Done }
     warningFields = if null warnings then remove ["warning"] else put "warning" (str (T.intercalate " " warnings))
     updated=mapTurns update state
     result | isJust failureText = updated
            | buffered = mapStateFields (appendItems "output_buffer" [obj [("turn",str i),("members",toJSON ids),("content",str output),("origin",origin)]]) updated
            | has origin "group" = updated
            | otherwise = mapStateFields (put "last_output" (str output) . appendItems "chat" [obj [("role",str "agent"),("content",str output),("turn",str i),("time",str time),("origin",origin)]]) updated
 let receipt=obj [("status",str (if isJust failureText then if stop then "interrupted" else "failed" else "done")),("output",str output),("error",maybe Null str failureText),("recorded_at",str time)]
     recorded=mapTurns (\t -> if turnId t `elem` ids && has (toJSON t) "managed" then withTurnFields (put "managed_result" receipt) t else t) result
 saveState s a recorded
 DB.project s a
 trimConversation s a
 D.reconcileWorks s
launch :: Service -> Text -> NonEmpty Turn -> Pulse -> Int -> Bool -> IO ()
launch s a batch tick slot batching = do
 mapM_ (D.requireCallAllowed s) batch
 state <- agentState s a
 let first=NE.head batch; i=turnId first; ids=map turnId (NE.toList batch)
     reserve t
      | turnId t `notElem` ids = t
      | otherwise = (withTurnFields (mergeFields (batchFields ++ [("batch_members",toJSON ids) | batching && turnId t==i]) . remove ["error"]) t) { turnStatus = Running, turnBatchId = if batching then Just i else turnBatchId t }
     batchFields = if batching then [("agency_kind",str "chatInput"),("slot",num slot),("pulseId",str (pulseIdentity tick))] else []
     marked=mapTurns reserve state
     snapshot=mapStateFields (mergeFields ([("current_turn",toJSON i)] ++ [("agency_batch",arr [obj [("id",toJSON (turnId t)),("content",str (turnInput t))] | t <- NE.toList batch]) | batching])) marked
     execution=(reserve first) { turnInput = T.intercalate "\n\n" (map turnInput (NE.toList batch)) }
 saveState s a marked
 cancel <- newIORef False
 process <- newMVar Nothing
 activity <- newIORef empty
 deadline <- (+1800000000000) <$> getMonotonicTimeNSec
 let run=Run (AgentId a) i slot cancel process activity deadline
 modifyMVar_ (serviceRuns s) (pure . M.insert i run)
 DB.pulseCall s tick a "chatInput" slot (nub [at (turnField t "origin") "group" | t <- NE.toList batch,has (turnField t "origin") "group"])
 DB.project s a
 void $ forkIO $ do
  result <- try (withActor s a (Just "chatInput") (if has (turnField execution "origin") "group" then G.runGroup s run execution else D.runDecision s run execution snapshot))
  locked s $ finish s run execution result `finally` do
   lease <- readIORef (serviceLease s)
   when (lease==Just (AgentId a,i)) $ writeIORef (serviceLease s) Nothing
   modifyMVar_ (serviceRuns s) (pure . M.delete i)
   D.pendingWorks s
   G.reconcileGroups s
   G.dispatchGroups s
publish :: Service -> Text -> Pulse -> IO ()
publish s a tick = forM_ [1,2] $ \slot -> do
 state <- agentState s a
 let document=stateJSON state
     scheduled=pulseFrequency tick==Bph60
     eligible item=has (get item "origin") "routine" == scheduled
 case find eligible (items document "output_buffer") of
  Nothing -> pure ()
  Just item -> do
   time <- now
   ids <- field 500 item "members" :: IO [RunId]
   let sequence=int (get document "output_sequence")+1
       origin=get item "origin"
       updated=mapTurns (\t -> if turnId t `elem` ids then (withTurnFields (mergeFields [("output_at",str time),("output_order",num sequence)]) t) { turnStatus = Done } else t) state
       messages=if has origin "group" || any (\m -> get m "turn"==get item "turn" && at m "role"=="agent") (items document "chat") then items document "chat" else items document "chat"++[obj [("role",str "agent"),("content",get item "content"),("turn",get item "turn"),("time",str time),("origin",origin)]]
   saveState s a $ mapStateFields (mergeFields ([("output_buffer",arr (filter (/=item) (items document "output_buffer"))),("output_sequence",num sequence),("chat",arr messages)] ++ [("last_output",get item "content") | not (has origin "group")])) updated
   DB.pulseCall s tick a "chatOutput" slot [at origin "group" | has origin "group"]
   DB.project s a
   trimConversation s a
admit :: Service -> Text -> Pulse -> IO ()
admit s a tick = do
 runs <- readMVar (serviceRuns s)
 state <- agentState s a
 let owners=nub (map runOwner (M.elems runs))
     mine=filter ((==AgentId a) . runOwner) (M.elems runs)
     available=[n | n<-[1,2],n `notElem` map runSlot mine]
     scheduled=pulseFrequency tick==Bph60
     queued=[t | t<-agentTurns state,turnStatus t==Queued,boolean (turnField t "batchable"),has (turnField t "origin") "routine"==scheduled]
     legacy=[t | t<-agentTurns state,turnStatus t==Queued,not (boolean (turnField t "batchable"))]
 when (AgentId a `elem` owners || length owners<4) $ case (legacy,available,NE.nonEmpty queued) of
  (first:_,_,_) | null mine && not scheduled -> launch s a (first :| []) tick 0 False
  (_,slot:_,Just batch) | all ((/=0) . runSlot) mine -> launch s a (if scheduled then NE.head batch :| [] else batch) tick slot True
  _ -> pure ()
onPulse :: Service -> Pulse -> IO ()
onPulse s tick = do
 stopRoutineRuns s
 R.dispatch s tick
 when (pulseFrequency tick==Bph60 || (pulseFrequency tick==Bpm60 && even (pulseNumber tick))) $ do
  D.pendingWorks s
  G.reconcileGroups s
  G.dispatchGroups s
  rows <- DB.agents s
  forM_ rows $ \row -> do
   let a=at row "id"
   inactive <- retired s a
   unless inactive $ (do
    restoreManaged s a
    current <- agentState s a
    activeRuns <- readMVar (serviceRuns s)
    forM_ (agentTurns current) $ \t -> when (has (toJSON t) "managed" && boolean (turnField t "cancel_requested")) $
     forM_ (M.lookup (turnId t) activeRuns) $ \run -> writeIORef (runCancel run) True
    let pending=Managed.commands current
    forM_ pending $ \(identity,command) -> void (turnCommand s a identity (at command "action") (Just command))
    publish s a tick
    admit s a tick
    when (pulseFrequency tick==Bph60) $ admit s a tick)
    `catches` [Handler (\e@(APIError code _) -> if code==409 then pure () else throwIO e),Handler (\(_::IOException) -> pure ())]
-- Replay durable occurrence fences through the existing call/process runtime.
stopRoutineRuns :: Service -> IO ()
stopRoutineRuns s = do
 works <- D.readWorks s
 blockedWorks <- filterM (fmap not . DB.occurrenceAllowed s . (`D.workField` "origin")) works
 let owners=nub [agentIdText (D.callOwner call) | work<-blockedWorks,call<-NE.toList (D.workCalls work)]
 forM_ owners $ \owner -> do
  state <- agentState s owner
  blocked <- filterM (fmap not . D.callAllowed s) [t | t<-agentTurns state,turnStatus t `elem` [Queued,Running,OutputPending]]
  unless (null blocked) $ do
   live <- readMVar (serviceRuns s)
   forM_ blocked $ \t -> forM_ (M.lookup (turnId t) live) $ \r -> writeIORef (runCancel r) True
   let ids=map turnId blocked
       update t | turnId t `elem` ids && turnStatus t/=Running = t {turnStatus=Cancelled}
                | otherwise = t
   let updated=mapStateFields (put "output_buffer" (arr [b | b<-items (stateJSON state) "output_buffer",get b "turn" `notElem` map toJSON ids])) (mapTurns update state)
   when (stateJSON updated/=stateJSON state) $ do
    saveState s owner updated
    DB.project s owner
 D.reconcileWorks s

milliseconds :: IO Int
milliseconds = fromIntegral . (`div` 1000000) <$> getMonotonicTimeNSec
startExecution :: Service -> IO ()
startExecution s = modifyMVar_ (serviceWorker s) $ \existing -> case existing of
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
          let interval=periodMillis frequency; key=wireName frequency
          due <- fromMaybe time . M.lookup frequency <$> readIORef deadlines
          when (time>=due) $ do
           let elapsed=(time-due) `div` interval+1
           modifyIORef' deadlines (M.insert frequency (due+elapsed*interval))
           current <- readIORef (servicePulse s)
           let number=int (get (get current "numbers") key)+elapsed; numbers=put key (num number) (get current "numbers"); identity=at current "id"
           writeIORef (servicePulse s) (put "numbers" numbers current)
           DB.saveSingleton s DB.SystemPulse (obj [("pulse",str identity),("numbers",numbers)])
           onPulse s (Pulse identity frequency number)
        threadDelay 50000
        step
  step
stopRuns :: Service -> IO ()
stopRuns s = readMVar (serviceRuns s) >>= mapM_ (\run -> writeIORef (runCancel run) True) . M.elems
