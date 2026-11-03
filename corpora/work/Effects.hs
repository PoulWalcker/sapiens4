{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- Receipt-feed transport and durable protocol facts. Business content remains a Task.
module Effects
 ( configureOn, definitionOn, pinOn, profileBound, external
 , beforeContent, ContentCommit(..), acceptContentOn, deliver, finishOccurrence
 , requireRunClear, requireContentTrace, observeEvidence, recover, reconcile, summary, pendingTransport
 ) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (race)
import Control.Exception
import Control.Monad
import Data.Aeson
import qualified Data.ByteString as B
import Data.IORef
import Data.List (find,nub)
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Word (Word64)
import qualified Database.SQLite.Simple as SQL
import GHC.Clock
import qualified Network.HTTP.Client as H
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types (statusCode,renderQuery)
import Types
import qualified Store as DB

-- No ambient proxy, automatic retry or redirect may replay an external write.
requestJSON :: EffectEndpoint -> [(B.ByteString,Maybe B.ByteString)] -> Maybe Value -> IO Value
requestJSON endpoint query payload = bracket (H.newManager settings) H.closeManager $ \manager -> do
 base<-H.parseRequest (T.unpack (effectURL endpoint))
 let request=base { H.method=maybe "GET" (const "POST") payload
                  ,H.queryString=renderQuery True query,H.redirectCount=0,H.responseTimeout=H.responseTimeoutNone
                  ,H.requestHeaders=maybe [] (const [("Content-Type","application/json")]) payload
                  ,H.requestBody=maybe (H.RequestBodyBS B.empty) (H.RequestBodyLBS . encode) payload
                  ,H.checkResponse = \_ _->pure () }
 H.withResponse request manager $ \response -> do
  require (statusCode (H.responseStatus response)==200) 502 "Receipt protocol returned a non-success response"
  bytes<-collect (H.responseBody response) 0 []
  either (failure 502 . ("Invalid receipt protocol JSON: " <>) . T.pack) pure (eitherDecodeStrict bytes)
 where
 settings=H.managerSetProxy H.noProxy (tlsManagerSettings {H.managerRetryableException=const False})
 collect reader size chunks=do
  chunk<-H.brRead reader
  if B.null chunk then pure (B.concat (reverse chunks)) else do
   let next=size+B.length chunk
   require (next<=65536) 502 "Receipt protocol response exceeds 64 KiB"
   collect reader next (chunk:chunks)

bounded :: Maybe Run -> Word64 -> IO a -> IO a
bounded running deadline action = do
 result<-race watch action
 either (failure 409) pure result
 where
 watch=do
  stopped<-maybe (pure False) (readIORef . runCancel) running
  tick<-getMonotonicTimeNSec
  if stopped then pure "Routine run was stopped" else if tick>=deadline then pure "External operation deadline exceeded" else threadDelay 10000 >> watch

loadWorkOn :: SQL.Connection -> Text -> IO Value
loadWorkOn db identity=do
 rows<-DB.queryRowsOn db "SELECT value FROM workloads WHERE id=?" ["value"] [SQL.SQLText identity]
 maybe (failure 404 "Unknown routine occurrence") (DB.decodeStored . (`at` "value")) (listToMaybe rows)
worksOn :: SQL.Connection -> IO [Value]
worksOn db=DB.queryRowsOn db "SELECT value FROM workloads ORDER BY rowid" ["value"] [] >>= traverse (DB.decodeStored . (`at` "value"))
definitionOn :: SQL.Connection -> Text -> IO Value
definitionOn db identity=do
 rows<-DB.queryRowsOn db "SELECT value FROM scheduled_routines ORDER BY sequence" ["value"] [] >>= traverse (DB.decodeStored . (`at` "value"))
 maybe (failure 404 "Unknown Routine") pure (findId identity rows)
saveWorkOn :: SQL.Connection -> Value -> IO ()
saveWorkOn db work=DB.putValueOn db DB.Workloads (at work "workloadId") work
external :: Value -> Value
external work=get work "externalExecution"
profileBound :: Value -> Bool
profileBound work=external work/=Null
consumer :: Value -> (Text,Text)
consumer work=let snapshot=get work "routineSnapshot" in (at snapshot "owner",at snapshot "id")
uncertainty :: Value -> Maybe Text
uncertainty work
 | get e "opaqueEffectUnknown"/=Null=Just "Possible external action remains unresolved because response-only evidence lost tool/protocol identity"
 | delivery==Null=Nothing
 | otherwise=case fromJSON (get delivery "status") of
    Error _->Just "Saved delivery state is invalid; a new attempt cannot be authorized"
    Success DeliveryStarted->pending
    Success DeliveryUnknown->pending
    Success DeliveryPrepared->Nothing
    Success DeliveryConfirmed->Nothing
    Success DeliveryNotSent->Nothing
    Success DeliveryNotRequired->Nothing
 where
 e=external work
 delivery=get e "delivery"
 pending=Just "A previous external action remains unknown; reconcile its receipt before a new occurrence"
requireClearOn :: SQL.Connection -> (Text,Text) -> Maybe Text -> IO ()
requireClearOn db owner except=do
 works<-worksOn db
 forM_ (listToMaybe [reason | work<-works,consumer work==owner,Just (at work "workloadId")/=except,Just reason<-[uncertainty work]]) (failure 409)

initialState :: ExternalProfile -> Value
initialState profile=obj [("revision",num 1),("sourceHash",str (canonicalHash (toJSON (externalSource profile)))),("profileHash",str (canonicalHash (toJSON profile))),("cursor",str (feedInitialCursor (externalSource profile))),("seenIds",arr [])]

configureOn :: SQL.Connection -> Maybe Value -> Value -> ProfileChange -> IO Value
configureOn db old current change=case change of
 PreserveProfile->pure current
 RemoveProfile->save (put "externalProfile" Null current)
 ConfigureProfile profile->do
  let prior=fromMaybe current old; generation=get prior "contractGeneration"
  works<-worksOn db
  let awaiting work=let pin=get work "preparationPin" in at pin "owner"==at prior "owner" && at pin "routine"==at prior "id" && get pin "contractGeneration"==generation && at (get work "preparation") "status"=="awaiting_occurrence"
  require ((at (get prior "activeCode") "status"/="active" || get (get prior "activeCode") "contractGeneration"/=generation) && not (any awaiting works)) 409 "This profile cannot replace active or prepared script automation; profile/script integration is not yet supported"
  let freshState=initialState profile; previous=get prior "sourceState"
      baseState=if at previous "sourceHash"==at freshState "sourceHash" then put "profileHash" (get freshState "profileHash") previous else freshState
      changed=get prior "externalProfile"/=toJSON profile
      state=if changed then put "revision" (num (int (get previous "revision")+1)) baseState else previous
  save (put "sourceState" state (put "externalProfile" (toJSON profile) current))
 where
 save value=do
  let changed=maybe False (\previous->get previous "externalProfile"/=get value "externalProfile") old
      next=if changed && maybe False (\previous->get previous "contractGeneration"==get value "contractGeneration") old then put "contractGeneration" (num (int (get value "contractGeneration")+1)) value else value
  previousRevision<-case old of
   Just previous | has previous "externalProfileRevision" -> profileRevision previous
   _->pure 0
  let versioned=if changed || (get next "externalProfile"/=Null && previousRevision==0) then put "externalProfileRevision" (num (previousRevision+1)) next else next
  DB.saveRoutineOn db versioned

pinOn :: SQL.Connection -> Value -> Value -> IO Value
pinOn db definition work=do
 requireClearOn db (at definition "owner",at definition "id") Nothing
 if get definition "externalProfile"==Null then pure work else do
  profile<-decodeValue 409 (get definition "externalProfile") :: IO ExternalProfile
  revision<-profileRevision definition
  require (get work "codeExecution"==Null) 409 "Receipt-feed profile does not yet support script/hybrid execution"
  pure (put "externalExecution" (obj [("profile",toJSON profile),("profileHash",str (canonicalHash (toJSON profile))),("externalProfileRevision",num revision)]) work)

-- Every checkpoint reloads the canonical occurrence while holding the service
-- lock. IO uses only the immutable values returned by that checkpoint.
checkpoint :: Service -> Run -> (SQL.Connection -> Value -> IO (Value,a)) -> IO a
checkpoint s running action=locked s $ DB.connection s $ \db -> SQL.withTransaction db $ do
 work<-loadWorkOn db (runIdText (runId running))
 (next,result)<-action db work
 saveWorkOn db next
 pure result
profileRevision :: Value -> IO Int
profileRevision value=do
 revision<-decodeValue 409 (get value "externalProfileRevision")
 require (revision>0) 409 "External profile revision is invalid"
 pure revision

currentProfileOn :: SQL.Connection -> Value -> IO Value
currentProfileOn db work=do
 let (_,routine)=consumer work; e=external work
 definition<-definitionOn db routine
 require (at work "executionGate"=="open" && not (boolean (get definition "deleted"))) 409 "Routine was stopped or deleted"
 currentRevision<-profileRevision definition
 pinnedRevision<-profileRevision e
 require (currentRevision==pinnedRevision && canonicalHash (get definition "externalProfile")==at e "profileHash") 409 "External profile changed; start a new authorized occurrence"
 require (get e "opaqueEffectUnknown"==Null) 409 "Unexpected tool evidence leaves possible external effects unresolved"
 pure definition
baseMatches :: Value -> Value -> Bool
baseMatches state request= get state "revision"==get request "baseRevision" && all (\key->get state key==get request key) ["cursor","sourceHash","profileHash"]

beforeContent :: Service -> Run -> IO ()
beforeContent s running=do
 prepared<-checkpoint s running $ \db work -> if not (profileBound work) then pure (work,Nothing) else do
  definition<-currentProfileOn db work
  requireClearOn db (consumer work) (Just (at work "workloadId"))
  let e=external work; readState=get e "read"
  if at readState "status"=="received" then pure (work,Nothing) else do
   require (readState==Null) 409 "This occurrence already attempted its source read; start a new occurrence"
   profile<-decodeValue 409 (get e "profile")
   let state=get definition "sourceState"
       request=merge (select ["cursor","sourceHash","profileHash"] state) (obj [("baseRevision",get state "revision")])
       next=put "externalExecution" (put "read" (obj [("status",str "started"),("request",request)]) e) work
   pure (next,Just (profile,request))
 forM_ prepared $ \(profile,request)->do
  result<-try (bounded (Just running) (runDeadline running) (requestJSON (feedURL (externalSource profile)) [("cursor",Just (TE.encodeUtf8 (at request "cursor")))] Nothing))
  case result of
   Left (err::SomeException)->do
    checkpoint s running $ \_ work -> pure (put "externalExecution" (put "read" (merge (get (external work) "read") (obj [("status",str "failed"),("error",str (T.pack (displayException err)))])) (external work)) work,())
    throwIO err
   Right response->do
    -- Save actual observation before validation, including a stale-base response.
    checkpoint s running $ \_ work -> pure (put "externalExecution" (put "read" (merge (get (external work) "read") (obj [("response",response),("observationHash",str (canonicalHash response)),("status",str "observed")])) (external work)) work,())
    page<-decodeValue 502 response :: IO FeedPage
    require (pageSource page==feedIdentity (externalSource profile) && pageCursor page==at request "cursor") 502 "Feed response identity or cursor mismatch"
    valid<-checkpoint s running $ \db work ->do
     definition<-currentProfileOn db work
     let state=get definition "sourceState"; matches=baseMatches state request
         unseen=filter (\(identity,_)->str identity `notElem` list (get state "seenIds")) (pageRecords page)
         readState=merge (get (external work) "read") (obj [("status",str (if matches then "received" else "conflict")),("newIds",arr (map (str . fst) unseen)),("newRecords",arr [obj [("id",str identity),("value",value)] | (identity,value)<-unseen])])
     pure (put "externalExecution" (put "read" readState (external work)) work,matches)
    require valid 409 "Source cursor advanced concurrently; observation retained without acceptance or delivery"

-- Called inside the existing accepted Outcome transaction, before saving work.
data ContentCommit = ContentCommitted Value | ContentConflict Value

acceptContentOn :: SQL.Connection -> Value -> Text -> Text -> IO ContentCommit
acceptContentOn db work decision reply
 | not (profileBound work)=pure (ContentCommitted work)
 | otherwise=do
  definition<-currentProfileOn db work
  requireClearOn db (consumer work) (Just (at work "workloadId"))
  let e=external work; observed=get e "read"; request=get observed "request"; state=get definition "sourceState"
      acceptance=obj [("status",str "accepted"),("decisionId",str decision)]
  require (at observed "status"=="received") 409 "Content cannot be accepted without a complete source observation"
  if not (baseMatches state request) then pure (ContentConflict (put "externalExecution" (merge e (obj [("contentAcceptance",put "status" (str "conflict") acceptance),("cursorCommit",obj [("status",str "conflict")]),("delivery",obj [("status",toJSON DeliveryNotSent),("error",str "Source cursor changed before content acceptance")])])) work)) else do
   envelope<-expectedRequest work reply
   let ids=get observed "newIds"
       delivery=merge (select ["operationKey","scope","profileHash","requestHash"] envelope) (obj [("status",toJSON (if null (list ids) then DeliveryNotRequired else DeliveryPrepared)),("request",envelope)])
       updatedState=merge state (obj [("revision",num (int (get state "revision")+1)),("cursor",get (get observed "response") "nextCursor"),("seenIds",arr (nub (list (get state "seenIds")++list ids)))])
   void (DB.saveRoutineOn db (put "sourceState" updatedState definition))
   pure (ContentCommitted (put "externalExecution" (merge e (obj [("contentAcceptance",acceptance),("cursorCommit",obj [("status",str "committed"),("revision",get updatedState "revision")]),("delivery",delivery)])) work))

expectedRequest :: Value -> Text -> IO Value
expectedRequest work reply=do
 let e=external work; observed=get e "read"; snapshot=get work "routineSnapshot"
 profile<-decodeValue 409 (get e "profile") :: IO ExternalProfile
 let ids=get observed "newIds"
     payload=obj [("schema",num 1),("operationKey",str (canonicalHash (arr [str (fst (consumer work)),str (snd (consumer work)),get work "workloadId",str "deliver"]))),("scope",str (canonicalHash (arr [str (fst (consumer work)),str (snd (consumer work))]))),("profileHash",get e "profileHash"),("externalProfileRevision",get e "externalProfileRevision"),("contractHash",str (canonicalHash (get snapshot "acceptedTask"))),("receiver",str (receiverIdentity (externalDelivery profile))),("sourceObservationHash",get observed "observationHash"),("recordIds",ids),("content",obj [("text",str reply)])]
 pure (put "requestHash" (str (canonicalHash payload)) payload)

validateReservation :: Value -> IO Value
validateReservation work=do
 let e=external work; delivery=get e "delivery"; request=get delivery "request"
 void (profileRevision e)
 require (at (get e "contentAcceptance") "status"=="accepted" && at (get e "cursorCommit") "status"=="committed") 409 "Delivery has no accepted content/cursor reservation"
 require (canonicalHash (get e "profile")==at e "profileHash" && canonicalHash (get (get e "read") "response")==at (get e "read") "observationHash") 409 "Pinned external observation/profile identity changed"
 expected<-expectedRequest work (at (get work "outcome") "reply")
 require (request==expected && all (\key->get delivery key==get request key) ["operationKey","scope","profileHash","requestHash"]) 409 "Saved delivery payload or immutable identity changed"
 pure request

validReceipt :: Value -> Value -> Bool
validReceipt request receipt = case fromJSON receipt :: Result Receipt of
 Error _->False
 Success _->all (\key->get request key==get receipt key) ["operationKey","scope","profileHash","requestHash","receiver"]
newtype Receipt = Receipt Value
instance FromJSON Receipt where
 parseJSON value=do
  onlyFields ["schema","receiver","operationKey","scope","profileHash","requestHash","status","receiptId"] value
  unless (get value "schema"==num 1 && at value "status"=="confirmed") (fail "Receipt is not confirmed")
  mapM_ (\key->void (textFieldP key 512 value)) ["receiver","operationKey","scope","profileHash","requestHash","receiptId"]
  pure (Receipt value)

deliver :: Service -> Run -> IO ()
deliver s running=do
 prepared<-checkpoint s running $ \db work ->do
  let e=external work; delivery=get e "delivery"
  if at delivery "status"/="prepared" then pure (work,Nothing) else do
   void (currentProfileOn db work)
   stopped<-readIORef (runCancel running)
   require (not stopped) 409 "Routine stopped before delivery"
   requireClearOn db (consumer work) (Just (at work "workloadId"))
   request<-validateReservation work
   profile<-decodeValue 409 (get e "profile")
   pure (put "externalExecution" (put "delivery" (put "status" (toJSON DeliveryStarted) delivery) e) work,Just (profile,request))
 forM_ prepared $ \(profile,request)->do
  result<-try (bounded (Just running) (runDeadline running) (requestJSON (deliveryURL (externalDelivery profile)) [] (Just request)))
  checkpoint s running $ \_ work ->do
   let e=external work; old=get e "delivery"
       proof=case result of Right receipt | validReceipt request receipt ->obj [("status",toJSON DeliveryConfirmed),("receipt",receipt)]; Right response->obj [("status",toJSON DeliveryUnknown),("response",response),("error",str "Receipt identity did not match the reserved delivery")]; Left (err::SomeException)->obj [("status",toJSON DeliveryUnknown),("error",str (T.pack (displayException err)))]
   pure (put "externalExecution" (put "delivery" (merge old proof) e) work,())
  case result of Left err | Just (_::SomeAsyncException)<-fromException err->throwIO err; _->pure ()

requireContentTrace :: Value -> Text -> IO ()
requireContentTrace work decision=when (profileBound work) $ do
 let evidence=maybe Null (`get` "evidence") (find (\entry->at entry "decisionId"==decision) (items work "decisions"))
     completeness=get evidence "completeness"
 require (boolean (get evidence "finished") && get evidence "processExit"==num 0 && all (\key->boolean (get completeness key)) ["finalSeen","turnEndSeen","streamEndSeen"]) 409 "Content invocation ended without complete terminal evidence; no content acceptance or delivery"

observeEvidence :: Text -> EvidenceSnapshot -> Value -> Value
observeEvidence identity evidence work
 | not (profileBound work) || not suspicious = work
 | otherwise=put "externalExecution" (put "opaqueEffectUnknown" (obj [("decisionId",str identity),("reason",str "Response-only protocol contains tool activity or lost event identity"),("evidence",toJSON evidence)]) (external work)) work
 where suspicious=maybe True (>0) (evidencePending evidence) || evidenceMalformed evidence>0 || evidenceUnknown evidence>0 || evidenceDropped evidence>0 || evidenceOutOfOrder evidence>0 || any (\receipt->isJust (receiptSummary receipt) || "tool" `T.isInfixOf` T.toLower (receiptKind receipt)) (evidenceReceipts evidence)

-- Terminal interruption never leaves an unsent reservation looking pending.
interrupted :: Bool -> Value -> Value
interrupted startup work
 | not (profileBound work)=work
 | otherwise=put "externalExecution" updated work
 where
 e=external work
 delivery=get e "delivery"
 status=at delivery "status"
 stoppedDelivery=if status `elem` ["prepared","started"] then merge delivery (obj [("status",toJSON (if status=="prepared" then DeliveryNotSent else DeliveryUnknown)),("error",str (if status=="prepared" then "Content accepted, but delivery was not started before interruption" else "Interrupted after delivery started; receipt reconciliation is required"))]) else delivery
 reconciliations=arr [if startup && at entry "status"=="pending" then put "status" (str "interrupted") entry else entry | entry<-items e "reconciliations"]
 updated=(if has e "reconciliations" then put "reconciliations" reconciliations else id) ((if delivery/=Null then put "delivery" stoppedDelivery else id) e)

finishOccurrence :: Service -> Run -> IO ()
finishOccurrence s running=checkpoint s running (\_ work->pure (interrupted False work,()))

requireRunClear :: Service -> RunId -> IO ()
requireRunClear s identity=DB.connection s $ \db->do
 works<-worksOn db
 forM_ (find (\work->at work "workloadId"==runIdText identity) works) $ \work->
  when (has (get work "origin") "routine") (requireClearOn db (consumer work) Nothing)

recover :: Service -> IO ()
recover s=DB.connection s $ \db->SQL.withTransaction db $ do
 works<-worksOn db
 forM_ works $ \work->when (profileBound work) (saveWorkOn db (interrupted True work))

reconcile :: Service -> Text -> Text -> Text -> Value -> IO Value
reconcile s owner routine identity body=do
 operation<-textField body "operation" 128
 require (keys body==["operation"]) 400 "Reconciliation only accepts a local operation identity"
 admitted<-locked s $ DB.connection s $ \db->SQL.withTransaction db $ do
  work<-loadWorkOn db identity
  require (consumer work==(owner,routine)) 404 "Unknown routine occurrence"
  let e=external work; history=items e "reconciliations"; delivery=get e "delivery"
  case find (\entry->at entry "operation"==operation) history of
   Just entry->do
    require (at entry "status"=="completed") 409 "This read-only reconciliation is pending or interrupted; use a new explicit operation for another lookup"
    pure (Left (get entry "response"))
   Nothing->do
    require (at delivery "status" `elem` ["unknown","confirmed"]) 409 "Only an uncertain started delivery can be reconciled; not-sent content cannot be redelivered by lookup"
    request<-validateReservation work
    profile<-decodeValue 409 (get e "profile")
    saveWorkOn db (put "externalExecution" (put "reconciliations" (arr (history++[obj [("operation",str operation),("status",str "pending")]])) e) work)
    pure (Right (profile,request))
 case admitted of
  Left response->pure response
  Right (profile,request)->do
   began<-getMonotonicTimeNSec
   result<-try (bounded Nothing (began+20000000000) (requestJSON (receiptURL (externalDelivery profile)) [("operationKey",Just (TE.encodeUtf8 (at request "operationKey")))] Nothing))
   response<-locked s $ DB.connection s $ \db->SQL.withTransaction db $ do
    work<-loadWorkOn db identity
    let e=external work; delivery=get e "delivery"
    void (validateReservation work)
    require (get delivery "request"==request) 409 "Saved delivery identity changed during reconciliation"
    let updated=case result of
         Right receipt | validReceipt request receipt && (get delivery "receipt"==Null || get delivery "receipt"==receipt)->merge (remove ["error","lookupError"] delivery) (obj [("status",toJSON DeliveryConfirmed),("receipt",receipt)])
         Right receipt | validReceipt request receipt->merge delivery (obj [("lookupResponse",receipt),("lookupError",str "Conflicting receipt proof; the first confirmed receipt is retained")])
         Right response->merge delivery (obj [("lookupResponse",response),("lookupError",str "Lookup did not prove this delivery")])
         Left (err::SomeException)->put "lookupError" (str (T.pack (displayException err))) delivery
        response=obj [("delivery",updated)]
        entry=obj [("operation",str operation),("status",str "completed"),("response",response)]
    saveWorkOn db (put "externalExecution" (put "reconciliations" (arr [if at old "operation"==operation then entry else old | old<-items e "reconciliations"]) (put "delivery" updated e)) work)
    pure response

   case result of Left err | Just (_::SomeAsyncException)<-fromException err->throwIO err; _->pure response

summary :: Value -> Value
summary work
 | not (profileBound work)=Null
 | otherwise=obj [("contentAcceptance",get e "contentAcceptance"),("delivery",deliverySummary),("possibleEffectUnknown",Bool (get e "opaqueEffectUnknown"/=Null)),("readStatus",get (get e "read") "status")]
 where
 e=external work
 delivery=get e "delivery"
 reason
  | at (get e "contentAcceptance") "status"=="conflict"="Source changed before content acceptance; no delivery was started."
  | at delivery "status"=="not_sent"="Delivery was not started."
  | otherwise="Delivery has not been independently confirmed."
 lookupReason=if at delivery "status"=="confirmed" then "The earlier confirmation is retained; this lookup could not be accepted." else "The lookup did not confirm delivery."
 deliverySummary=obj ([("status",get delivery "status")]++[("error",str reason) | has delivery "error"]++[("lookupError",str lookupReason) | has delivery "lookupError"])

pendingTransport :: Value -> Bool
pendingTransport work=case fromJSON (get (get (external work) "delivery") "status") of
 Success DeliveryPrepared->True
 Success DeliveryStarted->True
 _->False
