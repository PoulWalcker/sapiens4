{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Workflow (Yield(..), validate, selectInput, run, runRegion, contentHash, waitSpec, resultSpec, agentRecipient) where

import Control.Exception
import Control.Monad
import Data.Aeson hiding (pairs)
import Data.Aeson.Types (Parser,parseEither)
import qualified Data.ByteString.Lazy as BL
import Data.List (intersect)
import Data.Maybe (fromMaybe)
import Data.Time.Clock (UTCTime,addUTCTime,diffUTCTime)
import Data.Text (Text)
import qualified Data.Text as T
import Text.Read (readMaybe)
import Types

data Yield = Yield Text deriving (Show)
instance Exception Yield

waitSpec :: UTCTime -> UTCTime -> Value -> Either Text (UTCTime,Value)
waitSpec clock deadline value=either (Left . T.pack) Right (parseEither checked value)
 where
 checked selected=do
  onlyFields (["progress","value"]++if has selected "at" then ["at"] else ["afterMilliseconds"]) selected
  void (textFieldP "progress" 8000 selected)
  unless (has selected "value") (fail "Defer requires a retained value")
  due<-if has selected "at" then do
   raw<-textFieldP "at" 100 selected
   let suffix=T.takeEnd 6 raw
       zoned="Z" `T.isSuffixOf` raw || T.length suffix==6 && T.take 1 suffix `elem` ["+","-"] && T.take 1 (T.drop 3 suffix)==":"
   unless zoned (fail "Defer time needs an explicit UTC/offset timezone")
   maybe (fail "Invalid defer timestamp") pure (parseTime raw)
   else do
    milliseconds<-withObject "Defer" (.: "afterMilliseconds") selected :: Parser Integer
    unless (milliseconds>=0 && milliseconds<=max 0 (floor (diffUTCTime deadline clock*1000))) (fail "Defer delay exceeds the original wall allowance")
    pure (addUTCTime (fromInteger milliseconds/1000) clock)
  unless (due<=deadline) (fail "Defer time exceeds the original wall deadline")
  pure (due,get selected "value")

-- A reference names one existing root execution, never a mutable Workflow name.
resultSpec :: Value -> Either Text Value
resultSpec value=either (Left . T.pack) Right (parseEither checked value)
 where
 checked selected=do
  onlyFields ["source","progress"] selected
  void (textFieldP "progress" 8000 selected)
  let source=get selected "source"
  onlyFields ["workload","call"] source
  void (textFieldP "workload" 100 source)
  void (textFieldP "call" 100 source)
  pure source

agentRecipient :: Value -> Either Text (Maybe Text)
agentRecipient selected=either (Left . T.pack) Right (parseEither checked selected)
 where
 checked value=if get value "recipient"==Null then pure Nothing else Just <$> textFieldP "recipient" 128 value

contentHash :: Value -> Text
contentHash=canonicalHash

-- Definitions contain data selection, not an expression evaluator. Every output
-- reference must dominate its use, including at a branch merge.
validate :: Value -> Either Text ()
validate graph=either (Left . T.pack) Right (parseEither definition graph)
 where
 definition value=do
  onlyFields ["entry","nodes"] value
  entry<-textFieldP "entry" 100 value
  let nodes=get value "nodes"; ids=keys nodes
  unless (case nodes of Object _->True; _->False) (fail "Workflow nodes must be an object")
  unless (not (null ids) && length ids<=256 && BL.length (encode value)<=65536) (fail "Workflow exceeds the finite node/source bound")
  unless (entry `elem` ids && all (\i->not (T.null i) && T.length i<=100) ids) (fail "Invalid workflow entry or node identity")
  forM_ (pairs nodes) $ \(identity,node)->do
   let kind=at node "kind"
   onlyFields (case kind of "script"->["kind","source","input","next"]; "model"->["kind","instruction","input","next"]; "branch"->["kind","input","true","false"]; "finish"->["kind","input"]; "fork"->["kind","children","join"]; "join"->["kind","fork","input","next"]; "defer"->["kind","input","next"]; "awaitResult"->["kind","input","next"]; "agent"->["kind","instruction","criteria","input","recipient","next"]; _->[]) node
   unless (kind `elem` ["script","model","branch","finish","fork","join","defer","awaitResult","agent"]) (fail "Unsupported workflow node kind")
   when (kind=="script") (void (textFieldP "source" 65536 node))
   when (kind=="model" || kind=="agent") (void (textFieldP "instruction" 16000 node))
   when (kind=="agent") $ do
    criteria<-withObject "Agent step" (.: "criteria") node :: Parser [Text]
    unless (not (null criteria) && length criteria<=100 && all (\value->not (T.null (T.strip value)) && T.length value<=8000) criteria) (fail "Agent step needs bounded nonempty criteria")
    when (has node "recipient") (selector (get node "recipient"))
   unless (kind=="fork") (selector (get node "input"))
   when (kind=="fork") $ do
    children<-withObject "Fork" (.: "children") node :: Parser [Text]
    unless (length children>=2 && length children<=256 && length (unique children)==length children) (fail "Fork needs distinct finite children")
    join<-textFieldP "join" 100 node
    unless (at (get nodes join) "kind"=="join" && at (get nodes join) "fork"==identity) (fail "Fork/join identity mismatch")
   when (kind=="join") $ unless (at (get nodes (at node "fork")) "kind"=="fork" && at (get nodes (at node "fork")) "join"==identity) (fail "Join has no matching fork")
   unless (all (`elem` ids) (edges node)) (fail "Workflow successor does not exist")
  unless (all (`elem` reachable nodes [] [entry]) ids) (fail "Workflow contains unreachable nodes")
  order<-topological nodes [] ids
  forM_ (pairs nodes) $ \(identity,node)->when (at node "kind"=="fork") $ do
   let join=at node "join"; groups=[region nodes join [] child | child<-map text (items node "children")]
   unless (all (\group->not (null group) && all (\key->at (get nodes key) "kind"/="finish" && all (\next->next==join || next `elem` group) (edges (get nodes key))) group) groups) (fail "Fork child must terminate at its matching join")
   unless (sum (map length groups)==length (unique (concat groups))) (fail "Fork child regions overlap")
   unless (all (`elem` concat groups) (predecessors nodes join)) (fail "Join has an unrelated incoming path")
  void (foldM (dominators nodes entry) [] order)
 selector value=do
  let shape=filter (/="pointer") (keys value)
  unless (length shape==1) (fail "A workflow selector must have exactly one source")
  case shape of
   ["input"]->unless (get value "input"==Bool True) (fail "Invalid input selector")
   ["output"]->void (textFieldP "output" 100 value)
   ["literal"]->pure ()
   ["object"]->do
    unless (case get value "object" of Object _->True; _->False) (fail "Object selector requires fields")
    mapM_ (selector . snd) (pairs (get value "object"))
   ["array"]->case get value "array" of Array _->mapM_ selector (items value "array"); _->fail "Array selector requires a list"
   _->fail "Unknown workflow selector"
  when (has value "pointer") $ do
   unless (shape `elem` [["input"],["output"]]) (fail "Pointer requires input or output")
   pointer<-withObject "Selector" (.: "pointer") value :: Parser Text
   unless ((T.null pointer || "/" `T.isPrefixOf` pointer) && validEscapes pointer) (fail "Invalid JSON pointer")
 topological _ done []=pure done
 topological nodes done pending=do
  let ready=filter (\i->all (`elem` done) (predecessors nodes i)) pending
  when (null ready) (fail "Workflow cycles are not supported")
  topological nodes (done++ready) (filter (`notElem` ready) pending)
 dominators nodes entry known identity=do
  let parents=predecessors nodes identity
      sets=[parent:fromMaybe [] (lookup parent known) | parent<-parents]
      available=if at (get nodes identity) "kind"=="join" then
       let fork=get nodes (at (get nodes identity) "fork")
           groups=[region nodes identity [] child | child<-map text (items fork "children")]
           guaranteed group=case [parent:fromMaybe [] (lookup parent known) | parent<-parents,parent `elem` group] of []->[]; first:rest->foldl intersect first rest
       in unique (concatMap guaranteed groups)
       else case sets of []->[]; first:rest->foldl intersect first rest
  unless (identity==entry || not (null parents)) (fail "Workflow node is unreachable")
  unless (all (`elem` available) (references (get (get nodes identity) "input")++references (get (get nodes identity) "recipient"))) (fail "Workflow output reference is unavailable on an incoming path")
  pure ((identity,available):known)
 region nodes stop initial identity=walk initial [identity]
  where
  walk visited []=visited
  walk visited (key:pending)
   | key==stop || key `elem` visited=walk visited pending
   | otherwise=walk (key:visited) (edges (get nodes key)++pending)
 unique=map head . groupItems
 groupItems []=[]
 groupItems (x:xs)=(x:filter (==x) xs):groupItems (filter (/=x) xs)
 predecessors nodes identity=[key | (key,node)<-pairs nodes,identity `elem` edges node]
 reachable _ visited []=visited
 reachable nodes visited (identity:pending)
  | identity `elem` visited=reachable nodes visited pending
  | otherwise=reachable nodes (identity:visited) (edges (get nodes identity)++pending)

validEscapes :: Text -> Bool
validEscapes value=case T.uncons value of
 Nothing->True
 Just ('~',rest)->case T.uncons rest of Just (c,remaining) | c `elem` ['0','1']->validEscapes remaining; _->False
 Just (_,rest)->validEscapes rest

edges :: Value -> [Text]
edges node=case at node "kind" of "finish"->[]; "fork"->map text (items node "children"); "branch"->[at node "true",at node "false"]; _->[at node "next"]
references :: Value -> [Text]
references value=[at value "output" | has value "output"]++concatMap (references . snd) (pairs (get value "object"))++concatMap references (items value "array")

selectInput :: Value -> Value -> Value -> Either Text Value
selectInput input receipts selector=do
 base<-case filter (/="pointer") (keys selector) of
  ["input"]->Right input
  ["literal"]->Right (get selector "literal")
  ["output"]->let receipt=get receipts (at selector "output") in if at receipt "status"=="completed" then Right (get receipt "output") else Left "Selected workflow output is not complete"
  ["object"]->obj <$> mapM (\(key,value)->(,) key <$> selectInput input receipts value) (pairs (get selector "object"))
  ["array"]->arr <$> mapM (selectInput input receipts) (items selector "array")
  _->Left "Invalid workflow selector"
 if not (has selector "pointer") || at selector "pointer"=="" then Right base else foldM field base (drop 1 (T.splitOn "/" (at selector "pointer")))
 where
 field value raw=let key=T.replace "~0" "~" (T.replace "~1" "/" raw) in case value of
  Object _ | has value key->Right (get value key)
  Array _ | Just index<-readMaybe (T.unpack key), index>=0, index<length (list value)->Right (list value!!index)
  _->Left "Workflow JSON pointer is missing or has the wrong type"

-- All authority and actual execution remain with the caller. Publication performs
-- its gate check under the existing transaction before an admitted node can run.
run :: Value -> Value -> Value -> IO Value -> (Text -> Value -> IO ()) -> (Text -> Value -> Value -> IO Value) -> IO Value
run graph input authority load publish invoke=runRegion graph input authority (at graph "entry") Nothing load publish invoke

runRegion :: Value -> Value -> Value -> Text -> Maybe Text -> IO Value -> (Text -> Value -> IO ()) -> (Text -> Value -> Value -> IO Value) -> IO Value
runRegion graph input authority entry stop load publish invoke=do
 either (failure 422) pure (validate graph)
 advance entry
 where
 advance identity | Just identity==stop=pure Null
 advance identity=do
  saved<-load
  let receipts=get saved "nodes"; node=get (get graph "nodes") identity
  selected<-either (failure 422) pure (if at node "kind"=="fork" then Right Null else selectInput input receipts (get node "input"))
  recipient<-if at node "kind"=="agent" && has node "recipient" then either (failure 422) pure (selectInput input receipts (get node "recipient")) else pure Null
  let invocationInput=if at node "kind"=="agent" then obj [("input",selected),("recipient",recipient)] else selected
      request=obj ([("recipient",recipient) | at node "kind"=="agent"]++[("node",node),("input",selected),("structureHash",str (contentHash graph)),("authority",authority)])
      hash_=canonicalHash request
      previous=get receipts identity
      store receipt=publish identity receipt
  when (previous/=Null) $ do
   require (at previous "requestHash"==hash_) 409 "Retained workflow request authority changed"
   require (not (boolean (get previous "fixtureSimulation")) || (boolean (get authority "fixtureTimer") || boolean (get authority "fixtureResult") || boolean (get authority "fixtureAgent"))) 409 "Fixture timer evidence cannot authorize business execution"
  output<-if previous/=Null && not (at node "kind"=="fork" && at previous "status"=="admitted" || at node "kind" `elem` ["defer","awaitResult","agent"] && at previous "status"=="waiting") then do
   require (at previous "requestHash"==hash_ && at previous "status"=="completed") 409 "Workflow admission is failed or uncertain; no automatic replay"
   require (at previous "outputHash"==canonicalHash (get previous "output")) 409 "Retained workflow output changed"
   pure (get previous "output")
   else do
    let admitted=obj [("requestHash",str hash_),("request",request),("status",str "admitted")]
    unless (at previous "status"=="waiting") (store admitted)
    result<-try (case at node "kind" of
     "branch"->do
      void (parseValue 422 (onlyFields ["value","reason"]) selected)
      void (field 422 selected "value" :: IO Bool)
      void (parseValue 422 (textFieldP "reason" 8000) selected)
      pure selected
     "finish"->pure selected
     "join"->pure selected
     _->invoke identity node invocationInput) :: IO (Either SomeException Value)
    case result of
     Left exception | Just (_::Yield)<-fromException exception->throwIO exception
     Left exception->store (mergeFields [("status",str "unknown"),("reason",str (T.pack (displayException exception)))] admitted) >> throwIO exception
     Right value->do
      require (BL.length (encode value)<=65536) 422 "Workflow output exceeds 64 KiB"
      let timerFields=[("fixtureSimulation",Bool True) | at node "kind" `elem` ["defer","awaitResult","agent"] && (boolean (get authority "fixtureTimer") || boolean (get authority "fixtureResult") || boolean (get authority "fixtureAgent"))]
          decisionFields=if at node "kind"=="branch" then [("selected",get node (if get value "value"==Bool True then "true" else "false")),("reason",get value "reason")] else []
      store (mergeFields ([("status",str "completed"),("output",value),("outputHash",str (canonicalHash value))]++decisionFields++timerFields) admitted)
      pure value
  case at node "kind" of
   "finish"->pure output
   "fork"->advance (at node "join")
   "branch"->advance (at node (if get output "value"==Bool True then "true" else "false"))
   _->advance (at node "next")
