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
import System.FileLock (unlockFile)
import Types
import qualified Store as DB
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
      settled=filter (\row -> at row "status" `notElem` ["queued","running","output_pending"]) rows
      removed=take (max 0 (length settled-100)) settled
  unless (null removed) $ newId >>= \i -> writeJSON (archivePath s a (field<>"/"<>i)) (arr removed)
  pure (put field (arr (filter (`notElem` removed) rows)) current)) state ["chat","events","turns"]
 final <- foldM (\current field -> do
  let prune value removed [] = pure (value,removed)
      prune value removed (row:rest)
       | BL.length (encode value)<=800000 = pure (value,removed)
       | at row "status" `elem` ["queued","running","output_pending"] || (field=="turns" && Just row==listToMaybe (reverse (items value field))) = prune value removed rest
       | otherwise = prune (put field (arr (filter (/=row) (items value field))) value) (removed++[row]) rest
  (result,removed) <- prune current [] (items current field)
  unless (null removed) $ newId >>= \i -> writeJSON (archivePath s a (field<>"/"<>i)) (arr removed)
  pure result) bounded ["chat","events","turns"]
 when (final/=state) (decodeValue 500 final >>= saveState s a)
submitTurn :: Service -> Text -> Value -> IO Value
submitTurn s a d = withReadyProfile $ do
 let raw=get d "text" `fallback` str ""; input=T.strip (text raw); flowValue=get d "flow" `fallback` str "chat"
 require (case raw of String t -> T.length t<=16000; _ -> False) 400 "Message must be text, at most 16000 characters"
 attachments <- resolveAttachments s a (get d "attachments" `fallback` arr []) True
 require (not (T.null input) || not (null attachments)) 400 "Write a message or attach a file"
 flow <- decodeValue 400 flowValue
 binary <- codexBinary
 require (isJust binary) 503 "Codex is missing. Reinstall Sapiens to repair it."
 when (flow==Computer) $ blindly s >>= doesFileExist >>= \built -> require built 503 "Computer helper is missing. Reinstall Sapiens to repair it."
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
    let turn=toJSON (newTurn (RunId i) flow message time Null True)
        msg=obj [("role",str "user"),("content",str message),("turn",str i),("time",str time),("origin",Null)]
    modifyState s a $ \state -> merge state (obj [("turns",arr (items state "turns"++[turn])),("chat",arr (items state "chat"++[msg]))])
    pure i
   DB.message s i input attachments
   DB.project s a
   trimConversation s a
   pure (obj [("id",str i),("agent",str a),("status",str "queued"),("flow",toJSON flow)])
turnAction :: Service -> Text -> Text -> Text -> IO Value
turnAction s a identity rawAction = (if rawAction=="retry" then withReadyProfile else id) $ do
 requireActive s a
 action <- decodeValue 404 (str rawAction)
 state <- agentState s a
 turn <- maybe (failure 404 "Unknown active turn") pure (find ((==RunId identity) . turnId) (agentTurns state))
 work <- D.findWork s (RunId identity)
 require (not (maybe False (any ((==Just (RunId identity)) . D.callParent) . D.workCalls . fst) work)) 409 "This request was delegated; use the recipient call controls"
 let leader=batchLeader turn
     update f = mapTurns (\t -> if batchLeader t == leader then f t else t)
 case action of
  Retry -> do
   require (retryableStatus (turnStatus turn)) 409 "Only failed or interrupted batches can be retried"
   saveState s a (update retryTurn state)
  Cancel -> do
   require (turnStatus turn `notElem` [Done,OutputPending]) 409 "Completed input cannot be cancelled"
   runs <- readMVar (serviceRuns s)
   case M.lookup leader runs of
    Just run -> writeIORef (runCancel run) True
    Nothing -> saveState s a (update (\t -> t { turnStatus = Cancelled }) state)
 DB.project s a
 trimConversation s a
 D.reconcileWorks s
 updated <- agentState s a
 pure (obj [("id",str identity),("status",maybe Null (toJSON . turnStatus) (find ((==RunId identity) . turnId) (agentTurns updated)))])
recover :: Service -> Text -> IO ()
recover s a = do
 modifyTurns s a $ \t -> if turnStatus t == Running then
  (withTurnFields (put "error" (str "Runner stopped before saving a reply; inspect effects before retrying")) t) { turnStatus = Interrupted } else t
 DB.project s a
finish :: Service -> Run -> Turn -> Either SomeException (Text,[Value]) -> IO ()
finish s run turn outcome = do
 stop <- readIORef (runCancel run)
 let a=agentIdText (runOwner run); i=runIdText (runId run)
     (output,logs,error)=case outcome of
      Left e -> case fromException e of
       Just (RunFailure message entries)->("",entries,Just message)
       Nothing->("",[],Just (D.exceptionText e))
      Right (value,entries)->(value,entries,Nothing)
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
 saveState s a result
 DB.project s a
 trimConversation s a
 D.reconcileWorks s
launch :: Service -> Text -> NonEmpty Turn -> Pulse -> Int -> Bool -> IO ()
launch s a batch tick slot batching = mask_ $ do
 acquired <- runLease
 forM_ acquired $ \lease -> flip onException (unlockFile lease) $ do
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
  void $ forkIOWithUnmask $ \unmask -> flip finally (unlockFile lease) $ unmask $ do
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
 R.dispatch s tick
 when (pulseFrequency tick==Bph60 || (pulseFrequency tick==Bpm60 && even (pulseNumber tick))) $ do
  D.pendingWorks s
  G.reconcileGroups s
  G.dispatchGroups s
  rows <- DB.agents s
  forM_ rows $ \row -> do
   let a=at row "id"
   inactive <- retired s a
   unless inactive $ do
    publish s a tick
    admit s a tick
    when (pulseFrequency tick==Bph60) $ admit s a tick
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
