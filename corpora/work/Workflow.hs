{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Workflow (validate, selectInput, run, contentHash) where

import Control.Exception
import Control.Monad
import Data.Aeson hiding (pairs)
import Data.Aeson.Types (Parser,parseEither)
import qualified Data.ByteString.Lazy as BL
import Data.List (intersect)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Text.Read (readMaybe)
import Types

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
  forM_ (pairs nodes) $ \(_,node)->do
   let kind=at node "kind"
   onlyFields (case kind of "script"->["kind","source","input","next"]; "model"->["kind","instruction","input","next"]; "branch"->["kind","input","true","false"]; "finish"->["kind","input"]; _->[]) node
   unless (kind `elem` ["script","model","branch","finish"]) (fail "Unsupported workflow node kind")
   when (kind=="script") (void (textFieldP "source" 65536 node))
   when (kind=="model") (void (textFieldP "instruction" 16000 node))
   selector (get node "input")
   unless (all (`elem` ids) (edges node)) (fail "Workflow successor does not exist")
  unless (all (`elem` reachable nodes [] [entry]) ids) (fail "Workflow contains unreachable nodes")
  order<-topological nodes [] ids
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
      available=case sets of []->[]; first:rest->foldl intersect first rest
  unless (identity==entry || not (null parents)) (fail "Workflow node is unreachable")
  unless (all (`elem` available) (references (get (get nodes identity) "input"))) (fail "Workflow output reference is unavailable on an incoming path")
  pure ((identity,available):known)
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
edges node=case at node "kind" of "finish"->[]; "branch"->[at node "true",at node "false"]; _->[at node "next"]
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
run :: Value -> Value -> Value -> IO Value -> (Value -> IO ()) -> (Text -> Value -> Value -> IO Value) -> IO Value
run graph input authority load publish invoke=do
 either (failure 422) pure (validate graph)
 advance (at graph "entry")
 where
 advance identity=do
  saved<-load
  let receipts=get saved "nodes"; node=get (get graph "nodes") identity
  selected<-either (failure 422) pure (selectInput input receipts (get node "input"))
  let request=obj [("node",node),("input",selected),("structureHash",str (contentHash graph)),("authority",authority)]
      hash_=canonicalHash request
      previous=get receipts identity
      store receipt=publish (put "current" (str identity) (put "nodes" (put identity receipt receipts) saved))
  output<-if previous/=Null then do
   require (at previous "requestHash"==hash_ && at previous "status"=="completed") 409 "Workflow admission is failed or uncertain; no automatic replay"
   require (at previous "outputHash"==canonicalHash (get previous "output")) 409 "Retained workflow output changed"
   pure (get previous "output")
   else do
    let admitted=obj [("requestHash",str hash_),("request",request),("status",str "admitted")]
    store admitted
    result<-try (case at node "kind" of
     "branch"->do
      void (parseValue 422 (onlyFields ["value","reason"]) selected)
      void (field 422 selected "value" :: IO Bool)
      void (parseValue 422 (textFieldP "reason" 8000) selected)
      pure selected
     "finish"->pure selected
     _->invoke identity node selected) :: IO (Either SomeException Value)
    case result of
     Left exception->store (mergeFields [("status",str "unknown"),("reason",str (T.pack (displayException exception)))] admitted) >> throwIO exception
     Right value->do
      require (BL.length (encode value)<=65536) 422 "Workflow output exceeds 64 KiB"
      let decisionFields=if at node "kind"=="branch" then [("selected",get node (if get value "value"==Bool True then "true" else "false")),("reason",get value "reason")] else []
      store (mergeFields ([("status",str "completed"),("output",value),("outputHash",str (canonicalHash value))]++decisionFields) admitted)
      pure value
  case at node "kind" of
   "finish"->pure output
   "branch"->advance (at node (if get output "value"==Bool True then "true" else "false"))
   _->advance (at node "next")
