{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
module ManagedState (Conflict(..), load, enroll, enrollOn, save, acknowledge, restore, commands) where

import Control.Exception
import Control.Monad
import Data.Aeson
import qualified Data.ByteString.Lazy as BL
import Data.Data (Typeable)
import Data.List (find)
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Database.SQLite.Simple
import Domain

-- Only opt-in turns have SQLite authority. The saved file remains authoritative
-- for every other turn and for unrelated agent fields.
data Conflict = Conflict Text deriving (Show, Typeable)
instance Exception Conflict
check :: Bool -> Text -> IO ()
check ok message = unless ok (throwIO (Conflict message))
data Record = Record Text Value Value Bool
records :: Connection -> Text -> IO [Record]
records db owner = do
 rows <- query db "SELECT id,value,projection,pending FROM managed_turns WHERE agent=? ORDER BY rowid" (Only owner) :: IO [(Text,Text,Maybe Text,Int)]
 forM rows $ \(i,v,p,n) -> Record i <$> decode v <*> maybe (pure Null) decode p <*> pure (n==1)
 where decode = either (throwIO . userError) pure . eitherDecodeStrict . TE.encodeUtf8
encodeText :: Value -> Text
encodeText = TE.decodeUtf8 . BL.toStrict . encode
base :: Value -> Value
base Null = Null
base value = remove ["managed_command"] value
revision :: Value -> Value
revision v = get (get v "managed") "revision"
validateCommand :: Value -> Value -> IO ()
validateCommand turn command = do
 when (at command "action"=="retry" && boolean (get turn "codeBound")) $
  check False "Code-run evidence is retained; start a new authorized occurrence instead of Retry"
 when (at command "action"=="retry" && (has (get turn "origin") "analysis" || has (get turn "origin") "preparation")) $
  check False "Use a new explicit Routine analysis or preparation; interrupted maintenance cannot be replayed"
 check (keys command `allIn` ["id","action","revision"] && length (keys command)==3 && not (T.null (at command "id")) && T.length (at command "id")<=128 && get command "action" `elem` map str ["cancel","retry"]) "Invalid managed command; expected id, action and revision"
 let history=items (get turn "managed") "commands"
 check (all (\old -> at old "id"/=at command "id" || old==command) history) "Managed command id was already used with different contents"
 check (get command "revision" == revision turn || command `elem` history) "Managed command revision conflict"
 unless (command `elem` history) $ check (if at command "action"=="retry" then at turn "status" `elem` ["failed","interrupted"] else at turn "status" `notElem` ["done","output_pending"]) "Managed command is not allowed in this lifecycle state"
 where allIn xs ys = all (`elem` ys) xs
reconcile :: [Record] -> AgentState -> IO AgentState
reconcile rows state = do
 let turns=map toJSON (agentTurns state); ids=[i | Record i _ _ _<-rows]
 forM_ turns $ \v -> when (has v "managed") $ check (at v "id" `elem` ids) "Managed execution cannot be introduced through state.json"
 forM_ ids $ \i -> check (length (filter ((==i) . (`at` "id")) turns)<=1) "Duplicate managed execution in state.json"
 replacements <- forM rows $ \(Record i canonical projected pending) -> do
  let observed=fromMaybe Null (find ((==i) . (`at` "id")) turns)
      command=get observed "managed_command"
  check (base observed==canonical || (pending && base observed==projected)) ("Managed execution conflict: "<>i<>"; restore the protected projection")
  when (command/=Null) $ do
   check (base observed==canonical || (pending && command `elem` items (get canonical "managed") "commands")) "Command targets an outdated managed projection"
   validateCommand canonical command
  pure (i,if command==Null then canonical else put "managed_command" command canonical)
 let replace v = fromMaybe v (lookup (at v "id") replacements)
     missing=[v | (i,v)<-replacements,not (any ((==i) . (`at` "id")) turns)]
 let managedMessages=concat [items (get v "managed") "messages" | (_,v)<-replacements]
     sameMessage x y = get x "turn"==get y "turn" && get x "role"==get y "role"
     originalChat=items (stateJSON state) "chat"
     chat=map (\m -> fromMaybe m (find (sameMessage m) managedMessages)) originalChat++[m | m<-managedMessages,not (any (sameMessage m) originalChat)]
 decodeState (merge (stateJSON state) (obj [("turns",arr (map replace turns++missing)),("chat",arr chat)]))
load :: Connection -> Text -> AgentState -> IO AgentState
load db owner state = records db owner >>= (`reconcile` state)
decodeState :: Value -> IO AgentState
decodeState = either (throwIO . userError) pure . fromResult . fromJSON
 where fromResult (Error e)=Left e; fromResult (Success x)=Right x

enroll :: Connection -> Text -> Turn -> Value -> Text -> [Value] -> IO ()
enroll db owner turn message input attachments = withTransaction db (enrollOn db owner turn message input attachments)
enrollOn :: Connection -> Text -> Turn -> Value -> Text -> [Value] -> IO ()
enrollOn db owner turn message input attachments = do
 let value=toJSON turn
     metadata=obj [("version",num 1),("revision",num 1),("pin",selectPresent ["input","flow"] value),("attempts_started",num 0),("receipts",arr []),("commands",arr []),("messages",arr [message | message/=Null]) ]
 execute db "INSERT INTO message_inputs(job,text,attachments) VALUES (?,?,?)" (runIdText (turnId turn),input,encodeText (arr attachments))
 execute db "INSERT INTO managed_turns(id,agent,value,projection,pending) VALUES (?,?,?,NULL,1)" (runIdText (turnId turn),owner,encodeText (put "managed" metadata value))

-- Canonical facts, command consumption and the projection intent commit together.
-- File I/O happens after this transaction; an interrupted projection is replayable.
save :: Connection -> Text -> AgentState -> Maybe (Text,Value) -> IO AgentState
save db owner state command = withTransaction db $ do
 rows <- records db owner
 updates <- forM rows $ \(Record i old projected pending) -> do
  next <- maybe (throwIO (Conflict ("Managed execution cannot be removed: "<>i))) (pure . toJSON) (find ((==i) . runIdText . turnId) (agentTurns state))
  let metadata=get old "managed"
      incoming=get next "managed"
  check (incoming==metadata) "Managed protected fields cannot be overwritten"
  check (selectPresent ["input","flow"] next==get metadata "pin") "Managed execution pin cannot be changed"
  forM_ command $ \(target,cmd) -> when (target==i) (validateCommand old cmd)
  let started=at old "status"/="running" && at next "status"=="running"
      result=get next "managed_result"
      receipts=items metadata "receipts"++[put "attempt" (get next "attempt") result | result/=Null]
      applied=[cmd | Just (target,cmd)<-[command],target==i,cmd `notElem` items metadata "commands"]
      messages=filter ((==i) . (`at` "turn")) (items (stateJSON state) "chat")
      nextMetadata=merge metadata (obj ([("commands",arr (items metadata "commands"++applied)),("messages",arr messages),("attempts_started",num (int (get metadata "attempts_started")+if started then 1 else 0)),("receipts",arr receipts)]++[("last_command",cmd) | cmd<-applied]))
      candidate=put "managed" nextMetadata (remove ["managed_command","managed_result"] next)
      changed=candidate/=old
      value=if changed then put "managed" (put "revision" (num (int (get metadata "revision")+1)) nextMetadata) candidate else old
  -- All user cancel interfaces revoke the same maintenance gate atomically with
  -- the managed command. Feedback invalidation does not write this stop gate.
  let preparationOrigin=get old "origin"
      maintenance=if has preparationOrigin "preparation" then "preparation" else "analysis"
  when (has preparationOrigin maintenance && any ((=="cancel") . (`at` "action")) applied) $ do
   rows_ <- query db "SELECT value FROM workloads WHERE id=?" (Only (at preparationOrigin maintenance)) :: IO [Only Text]
   work <- case rows_ of
    [Only stored] -> either (throwIO . userError) pure (eitherDecodeStrict (TE.encodeUtf8 stored))
    _ -> throwIO (Conflict "Maintenance is missing its canonical Workload")
   execute db "UPDATE workloads SET value=? WHERE id=?" (encodeText (put "executionGate" (toJSON ExecutionStopped) work),at preparationOrigin maintenance)
  when changed $ execute db "UPDATE managed_turns SET value=?,projection=?,pending=1 WHERE id=?" (encodeText value,if projected==Null then Nothing else Just (encodeText projected),i)
  pure (i,if has next "managed_command" && maybe True ((/=i) . fst) command then put "managed_command" (get next "managed_command") value else value,pending||changed)
 decodeState $ put "turns" (arr [fromMaybe (toJSON t) (lookup (runIdText (turnId t)) [(i,v) | (i,v,_)<-updates]) | t<-agentTurns state]) (stateJSON state)

acknowledge :: Connection -> Text -> IO ()
acknowledge db owner = execute db "UPDATE managed_turns SET projection=value,pending=0 WHERE agent=? AND pending=1" (Only owner)

-- Caller restores only pending managed projections, preserving unrelated fields.
restore :: Connection -> Text -> AgentState -> IO (Maybe AgentState)
restore db owner state = do
 rows <- records db owner
 result <- reconcile rows state
 pure (if any (\(Record _ _ _ pending)->pending) rows then Just result else Nothing)
commands :: AgentState -> [(Text,Value)]
commands state = [(runIdText (turnId t),cmd) | t<-agentTurns state,has (toJSON t) "managed",let cmd=turnField t "managed_command",cmd/=Null]
