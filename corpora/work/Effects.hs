{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- Receipt-feed transport and durable protocol facts. Business content remains a Task.
module Effects
 ( configureOn, definitionOn, pinOn, profileBound, external
 , beforeContent, ContentCommit(..), acceptContentOn, deliver, finishOccurrence
 , completeActionFree, positiveAnomaly, observedInput, preparationInput, fixtureInput, requireCandidateInput, requireCurrentInput
 , auditPublication, requireDefinitionClearOn, requireRunClear, prepareRoutineAttempt, admitRoutineAttempt, sealRoutineAttempt, requireContentTrace, observeEvidence, recover, reconcile, summary, pendingTransport
 ) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (race)
import Control.Exception
import Control.Monad
import Data.Aeson
import Data.Aeson.Types (Parser,parseEither)
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
data RoutineConsumer = RoutineConsumer Text Text deriving (Eq,Show)
data RoutineRisk = RoutineClear | RoutineUnverified | RoutineUnknown Text Text deriving (Eq,Show)

-- Only retained Routine ancestry establishes scope; a delegated child may have
-- another addressedTo owner and does not become a different consumer.
routineConsumerOn :: SQL.Connection -> Value -> IO (Maybe RoutineConsumer)
routineConsumerOn db work
 | has work "repairPin" = do
  let pin=get work "repairPin"
  definition<-definitionOn db (at pin "routine")
  require (get definition "owner"==get pin "owner" && any ((==get pin "case") . (`get` "id")) (items definition "repairCases")) 409 "Doctor ancestry contradicts its original Routine consumer"
  pure (Just (RoutineConsumer (at pin "owner") (at pin "routine")))
 | not (has origin "routine") && snapshot==Null = pure Nothing
 | has origin "group" = pure Nothing
 | otherwise = do
  let identity=if snapshot/=Null then at snapshot "id" else at origin "routine"
  require (not (T.null identity)) 409 "Routine ancestry is missing its identity"
  definition<-definitionOn db identity
  let owner=at definition "owner"
      rootCall=listToMaybe [call | call<-items work "calls",get call "causedBy"==Null]
      asserted=[at value key | (value,key)<-[(snapshot,"owner"),(origin,"routineOwner"),(origin,"agent")],has value key]
      rootOwner=maybe "" (`at` "addressedTo") rootCall
  require (not (T.null owner) && not (T.null rootOwner) && rootOwner==owner && all (==owner) asserted
    && (not (has origin "routine") || at origin "routine"==identity)) 409 "Saved Routine ancestry contradicts its retained owner"
  pure (Just (RoutineConsumer owner identity))
 where origin=get work "origin"; snapshot=get work "routineSnapshot"

completeActionFree :: Value -> Bool
completeActionFree evidence = case parseEither checked evidence of Right value->value; Left _->False
 where
 checked value=do
  finished<-withObject "Invocation evidence" (.: "finished") value
  exit<-withObject "Invocation evidence" (.: "processExit") value :: Parser Int
  events<-withObject "Invocation evidence" (.: "events") value :: Parser [Value]
  let completeness=get value "completeness"
  flags<-mapM (\key->withObject "Capture completeness" (.: key) completeness) ["finalSeen","turnEndSeen","streamEndSeen"] :: Parser [Bool]
  counts<-mapM (\key->withObject "Capture completeness" (.: key) completeness) ["pendingTools","malformedEvents","unknownEvents","droppedEvents","outOfOrderEvents"] :: Parser [Int]
  pure (finished && exit==0 && and flags && all (==0) counts && not (any toolEvent events))
 toolEvent event=get event "summary"/=Null || "tool" `T.isInfixOf` T.toLower (at event "kind")

positiveAnomaly :: Value -> Bool
positiveAnomaly evidence =
 let completeness=get evidence "completeness"
 in any (\event->get event "summary"/=Null || "tool" `T.isInfixOf` T.toLower (at event "kind")) (items evidence "events")
   || any (\key->case fromJSON (get completeness key) :: Result Int of Success count->count>0; Error _->False) ["pendingTools","malformedEvents","unknownEvents","droppedEvents","outOfOrderEvents"]
   || (has completeness "pendingTools" && get completeness "pendingTools"==Null)

data InvocationAdmission = LegacyAdmission | PreparedAdmission Text | AdmittedAdmission Text deriving (Eq,Show)
invocationAdmission :: Value -> Either String InvocationAdmission
invocationAdmission = parseEither $ \value -> if value==Null then pure LegacyAdmission else do
 onlyFields ["schema","state","policy"] value
 unless (get value "schema"==num 1) (fail "Unknown Routine invocation guard schema")
 policy<-textFieldP "policy" 40 value
 unless (policy `elem` ["unsafe_bypass","response_only"]) (fail "Unknown Routine invocation policy")
 case at value "state" of
  "prepared"->pure (PreparedAdmission policy)
  "admitted"->pure (AdmittedAdmission policy)
  _->fail "Unknown Routine invocation admission state"

routineRisk :: Bool -> Value -> RoutineRisk
routineRisk startup work
 | get work "routineEffectUnknown"/=Null = RoutineUnknown (at (get work "routineEffectUnknown") "decisionId") (at (get work "routineEffectUnknown") "reason")
 | verifiedRepairActivity = RoutineClear
 | otherwise = fromMaybe (if failedWork && null entries && get work "routineAttemptSchema"/=num 1 then RoutineUnverified else RoutineClear) (listToMaybe unknowns `mplus` listToMaybe unverified)
 where
 verifiedRepairActivity=has work "repairPin" && noAnomalies && case fromJSON (get work "repairInvocationPolicy") :: Result RepairInvocationPolicy of
  Success DoctorScopedResponseOnly->not (has work "preparation" || has work "codeExecution")
  Success RepairPreparationResponseOnly->has work "preparation" && not (has work "codeExecution")
  Success SavedAuditResponseOnly->has work "savedAudit" && not (has work "preparation")
  Success KitInspectionResponseOnly->has work "kitInspection" && not (has work "preparation" || has work "codeExecution")
  Error _->False
 noAnomalies=not (any (positiveAnomaly . (`get` "evidence")) (concatMap nested (items work "decisions"++items (get work "preparation") "operations"++items (get work "codeExecution") "operations")))
 nested entry=entry:items entry "attempts"++concatMap nested [get entry "correction" | get entry "correction"/=Null]
 entries=items work "decisions"
 failedState call=at call "state" `elem` ["Failed","Interrupted","Unresolved","Cancelled"] || startup && at call "state"=="Running"
 failedWork=any failedState (items work "calls")
 failed entry=has entry "error" || at (get entry "assessment") "status" `elem` ["Rejected","Unresolved"]
   || at (get (get entry "response") "outcome") "status"=="Unresolved"
   || not (boolean (get entry "accepted")) && any (\call->get call "callId"==get entry "callId" && failedState call) (items work "calls")
 classify entry
  | not (failed entry) = RoutineClear
  | validPrepared && not actualStart = RoutineClear
  | responseOnly && not (positiveAnomaly evidence) = RoutineClear
  | consistent && completeActionFree evidence = RoutineClear
  | admitted || actualStart = RoutineUnknown (at entry "decisionId") "An ordinary Routine invocation may have performed an external action; its failed or incomplete result has no independent absence proof"
  | otherwise = RoutineUnverified
  where
   attempt=get entry "routineAttempt"; evidence=get entry "evidence"
   decoded=invocationAdmission attempt
   recordedPolicy=case decoded of Right (PreparedAdmission policy)->Just policy; Right (AdmittedAdmission policy)->Just policy; _->Nothing
   capturedPolicy=case at evidence "policy" of "response_only"->Just "response_only"; "unsafe_bypass"->Just "unsafe_bypass"; _->Nothing
   consistent=case decoded of
    Left _->False
    Right _->case (recordedPolicy,capturedPolicy) of (Just a,Just b)->a==b; (_,Nothing)->evidence==Null; _->True
   validPrepared=consistent && case decoded of Right (PreparedAdmission _)->True; _->False
   admitted=case decoded of Right (AdmittedAdmission _)->True; _->at attempt "state"=="admitted"
   responseOnly=consistent && (recordedPolicy==Just "response_only" || capturedPolicy==Just "response_only")
   actualStart=not (null (items evidence "events")) || get evidence "processExit"/=Null
    || any (\key->case fromJSON (get (get evidence "completeness") key) :: Result Int of Success count->count>0; Error _->False) ["malformedEvents","unknownEvents","droppedEvents","outOfOrderEvents"]

 risks=map classify entries
 unknowns=[risk | risk@(RoutineUnknown _ _)<-risks]
 unverified=[RoutineUnverified | RoutineUnverified<-risks]

retainRoutineRisk :: Bool -> Value -> Value
retainRoutineRisk startup work = case routineRisk startup work of
 RoutineUnknown identity reason->put "routineEffectUnknown" (obj [("decisionId",str identity),("reason",str reason)]) work
 _->work

requireClearOn :: SQL.Connection -> RoutineConsumer -> Maybe Text -> IO ()
requireClearOn db owner@(RoutineConsumer _ identity) except=do
 works<-worksOn db
 forM_ [work | work<-works,at (get work "routineSnapshot") "id"==identity || at (get work "origin") "routine"==identity || at (get work "repairPin") "routine"==identity] $ \work->do
  actual<-routineConsumerOn db work
  when (actual==Just owner) $ do
   let retained=retainRoutineRisk False work
   when (retained/=work) (saveWorkOn db retained)
   case routineRisk False retained of RoutineUnknown _ reason->failure 409 reason; _->pure ()
   when (Just (at work "workloadId")/=except) (forM_ (uncertainty work) (failure 409))

requireConsumerClearOn :: SQL.Connection -> Value -> Maybe Text -> IO ()
requireConsumerClearOn db work except=routineConsumerOn db work >>= mapM_ (\owner->requireClearOn db owner except)

prepareRoutineAttempt :: SQL.Connection -> Value -> Bool -> Value -> IO Value
prepareRoutineAttempt db work responseOnly record=do
 scope<-routineConsumerOn db work
 pure (if isNothing scope then record else put "routineAttempt" (obj [("schema",num 1),("state",str "prepared"),("policy",str (if responseOnly then "response_only" else "unsafe_bypass"))]) record)

admitRoutineAttempt :: Value -> Value
admitRoutineAttempt entry=if get (get entry "routineAttempt") "schema"==num 1 then put "routineAttempt" (put "state" (str "admitted") (get entry "routineAttempt")) entry else entry

sealRoutineAttempt :: Service -> RunId -> IO ()
sealRoutineAttempt s identity=DB.connection s $ \db->do
 works<-worksOn db
 forM_ [work | work<-works,any (\call->get call "callId"==toJSON identity) (items work "calls")] $ \work->do
  scope<-routineConsumerOn db work
  when (isJust scope) $ do
   let retained=retainRoutineRisk False work
   when (retained/=work) (saveWorkOn db retained)

initialState :: ExternalProfile -> Value
initialState profile=obj [("revision",num 1),("sourceHash",str (canonicalHash (toJSON (externalSource profile)))),("profileHash",str (canonicalHash (toJSON profile))),("cursor",str (feedInitialCursor (externalSource profile))),("seenIds",arr [])]

configureOn :: SQL.Connection -> Maybe Value -> Value -> ProfileChange -> IO Value
configureOn db old current change=case change of
 PreserveProfile->pure current
 RemoveProfile->save (put "externalProfile" Null current)
 ConfigureProfile profile->do
  let prior=fromMaybe current old
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

requireDefinitionClearOn :: SQL.Connection -> Value -> IO ()
requireDefinitionClearOn db definition=requireClearOn db (RoutineConsumer (at definition "owner") (at definition "id")) Nothing

pinOn :: SQL.Connection -> Value -> Value -> IO Value
pinOn db definition work=do
 requireClearOn db (RoutineConsumer (at definition "owner") (at definition "id")) Nothing
 if get definition "externalProfile"==Null then pure work else do
  profile<-decodeValue 409 (get definition "externalProfile") :: IO ExternalProfile
  revision<-profileRevision definition
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
requireCurrentInput :: Service -> Value -> IO ()
requireCurrentInput s work = when (profileBound work) $ do
 void (either (failure 409 . T.pack) pure (observedInput work))
 DB.connection s (\db->void (currentProfileOn db work))

baseMatches :: Value -> Value -> Bool
baseMatches state request= get state "revision"==get request "baseRevision" && all (\key->get state key==get request key) ["cursor","sourceHash","profileHash"]

beforeContent :: Service -> Run -> IO ()
beforeContent s running=do
 prepared<-checkpoint s running $ \db work -> if not (profileBound work) then pure (work,Nothing) else do
  definition<-currentProfileOn db work
  requireConsumerClearOn db work (Just (at work "workloadId"))
  let e=external work; readState=get e "read"
  if at readState "status"=="received" then pure (work,Nothing) else do
   require (readState==Null) 409 "This occurrence already attempted its source read; start a new occurrence"
   profile<-decodeValue 409 (get e "profile")
   let state=get definition "sourceState"
   checked<-parseValue 409 (feedState profile) state
   let request=merge (select ["cursor","sourceHash","profileHash"] state) (obj [("baseRevision",get state "revision"),("priorSeenIdsHash",str (canonicalHash (toJSON (feedStateSeen checked))))])
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
     checked<-parseValue 409 (feedState profile) state
     let unseen=filter (\(identity,_)->identity `notElem` feedStateSeen checked) (pageRecords page)
         readState=merge (get (external work) "read") (obj [("status",str (if matches then "received" else "conflict")),("newIds",arr (map (str . fst) unseen)),("newRecords",arr [obj [("id",str identity),("value",value)] | (identity,value)<-unseen])])
     input<-parseValue 409 (\_ -> do
       parameters<-acceptedParameters work
       feedBusinessInput profile parameters response (feedStateSeen checked)) Null
     let binding=merge (select ["profileHash","externalProfileRevision"] (external work)) (obj [("interface",str "receipt-feed-input-v1"),("sourceObservationHash",str (canonicalHash response)),("priorSeenIdsHash",get request "priorSeenIdsHash"),("inputHash",str (canonicalHash input)),("input",input),("provenance",str "observed")])
         next=put "read" readState (external work)
     pure (put "externalExecution" (if matches then put "inputBinding" binding next else next) work,matches)
    require valid 409 "Source cursor advanced concurrently; observation retained without acceptance or delivery"

-- The accepted Task remains unchanged; this only decodes its original parameters.
acceptedParameters :: Value -> Parser Value
acceptedParameters work = do
 inputs<-parseJSON (get (get (get work "routineSnapshot") "acceptedTask") "inputs") :: Parser [Text]
 pure (case inputs of []->Null; [raw]->either (const (str raw)) id (eitherDecodeStrict (TE.encodeUtf8 raw)); values->toJSON values)

observedInput :: Value -> Either String Value
observedInput work = parseEither validate work
 where
 validate _ = do
  let e=external work; observed=get e "read"; response=get observed "response"; binding=get e "inputBinding"
  profile<-parseJSON (get e "profile")
  revision<-withObject "External execution" (.: "externalProfileRevision") e :: Parser Int
  unless (revision>0 && get e "profileHash"==str (canonicalHash (toJSON (profile::ExternalProfile))) && get observed "observationHash"==str (canonicalHash response) && at observed "status"=="received") (fail "Retained source observation identity is invalid")
  let request=get observed "request"
  baseRevision<-withObject "Source request" (.: "baseRevision") request :: Parser Int
  unless (baseRevision>0 && get request "profileHash"==get e "profileHash" && get request "sourceHash"==str (canonicalHash (toJSON (externalSource profile)))) (fail "Retained source request authority is invalid")
  page<-parseJSON response
  unless (pageCursor page==at request "cursor") (fail "Retained observation cursor changed")
  ids<-feedSeenIds (get observed "newIds")
  let pageIds=map fst (pageRecords page)
      prior=[identity | identity<-pageIds,identity `notElem` ids]
  unless (all (`elem` pageIds) ids) (fail "Retained new IDs are not part of the observation")
  parameters<-acceptedParameters work
  input<-feedBusinessInput profile parameters response prior
  unless (get input "newRecords"==get observed "newRecords" && toJSON [at record "id" | record<-items input "newRecords"]==toJSON ids) (fail "Retained source deduplication is inconsistent")
  if binding==Null then do
   unless (at (get e "contentAcceptance") "status"=="accepted" && at (get e "cursorCommit") "status"=="committed") (fail "Historical source has no accepted content/cursor proof")
   void (checkedReservation work)
   pure (obj [("interface",str "receipt-feed-input-v1"),("profileHash",get e "profileHash"),("externalProfileRevision",num revision),("sourceObservationHash",get observed "observationHash"),("priorSeenIdsHash",Null),("inputHash",str (canonicalHash input)),("input",input),("provenance",str "derived_from_v13_observation")])
  else do
   digest<-textFieldP "priorSeenIdsHash" 64 binding
   unless (T.length digest==64 && T.all (\c->c `elem` ("0123456789abcdef"::String)) digest && at binding "provenance"=="observed") (fail "Observed input lacks its prior-state hash or provenance")
   unless (at binding "interface"=="receipt-feed-input-v1" && get binding "input"==input && get binding "inputHash"==str (canonicalHash input) && all (\key->get binding key==get e key) ["profileHash","externalProfileRevision"] && get binding "sourceObservationHash"==get observed "observationHash" && get binding "priorSeenIdsHash"==get (get observed "request") "priorSeenIdsHash") (fail "Pinned source input identity changed")
   pure binding

preparationInput :: Value -> Value -> IO Value
preparationInput definition original = do
 let e=external original
 require (at (get e "contentAcceptance") "status"=="accepted" && at (get e "cursorCommit") "status"=="committed") 409 "Preparation requires accepted source content and committed cursor"
 binding<-either (failure 409 . T.pack) pure (observedInput original)
 require (get binding "profileHash"==str (canonicalHash (get definition "externalProfile")) && get binding "externalProfileRevision"==get definition "externalProfileRevision") 409 "Source observation belongs to an older profile; run the current profile first"
 pure binding

fixtureInput :: ExternalProfile -> Value -> Parser Value
fixtureInput profile value = do
 onlyFields ["taskInputs","page","priorSeenIds"] value
 unless (has value "taskInputs") (fail "Fixture taskInputs is required, including explicit null")
 seen<-feedSeenIds (get value "priorSeenIds")
 feedBusinessInput profile (get value "taskInputs") (get value "page") seen

requireCandidateInput :: Value -> Value -> IO ()
requireCandidateInput snapshot version
 | get snapshot "externalProfile"==Null = require (get version "inputBinding"==Null) 409 "Feed candidate cannot execute without its accepted source profile"
 | otherwise = do
  profile<-decodeValue 409 (get snapshot "externalProfile")
  revision<-profileRevision snapshot
  descriptor@(FeedInputDescriptor _ _ _ content)<-decodeValue 409 (get version "inputBinding")
  require (descriptor==feedDescriptor profile revision content && (content==HybridText)==has version "explanation") 409 "Candidate input interface or profile identity does not match this occurrence"

-- Called inside the existing accepted Outcome transaction, before saving work.
data ContentCommit = ContentCommitted Value | ContentConflict Value

acceptContentOn :: SQL.Connection -> Value -> Text -> Text -> IO ContentCommit
acceptContentOn db work decision reply
 | not (profileBound work)=pure (ContentCommitted work)
 | otherwise=do
  definition<-currentProfileOn db work
  requireConsumerClearOn db work (Just (at work "workloadId"))
  let e=external work; observed=get e "read"; request=get observed "request"; state=get definition "sourceState"
      acceptance=obj [("status",str "accepted"),("decisionId",str decision)]
  profile<-decodeValue 409 (get e "profile")
  void (parseValue 409 (feedState profile) state)
  void (either (failure 409 . T.pack) pure (observedInput work))
  require (at observed "status"=="received") 409 "Content cannot be accepted without a complete source observation"
  if not (baseMatches state request) then pure (ContentConflict (put "externalExecution" (merge e (obj [("contentAcceptance",put "status" (str "conflict") acceptance),("cursorCommit",obj [("status",str "conflict")]),("delivery",obj [("status",toJSON DeliveryNotSent),("error",str "Source cursor changed before content acceptance")])])) work)) else do
   envelope<-expectedRequest work reply
   let ids=get observed "newIds"
       delivery=merge (select ["operationKey","scope","profileHash","requestHash"] envelope) (obj [("status",toJSON (if null (list ids) then DeliveryNotRequired else DeliveryPrepared)),("request",envelope)])
       updatedState=merge state (obj [("revision",num (int (get state "revision")+1)),("cursor",get (get observed "response") "nextCursor"),("seenIds",arr (nub (list (get state "seenIds")++list ids)))])
   void (DB.saveRoutineOn db (put "sourceState" updatedState definition))
   pure (ContentCommitted (put "externalExecution" (merge e (obj [("contentAcceptance",acceptance),("cursorCommit",obj [("status",str "committed"),("revision",get updatedState "revision")]),("delivery",delivery)])) work))

expectedRequest :: Value -> Text -> IO Value
expectedRequest work reply=either (failure 409 . T.pack) pure (parseEither (const (requestPayload work reply)) Null)

requestPayload :: Value -> Text -> Parser Value
requestPayload work reply=do
 let e=external work; observed=get e "read"; snapshot=get work "routineSnapshot"
 profile<-parseJSON (get e "profile") :: Parser ExternalProfile
 let ids=get observed "newIds"
     payload=obj [("schema",num 1),("operationKey",str (canonicalHash (arr [str (fst (consumer work)),str (snd (consumer work)),get work "workloadId",str "deliver"]))),("scope",str (canonicalHash (arr [str (fst (consumer work)),str (snd (consumer work))]))),("profileHash",get e "profileHash"),("externalProfileRevision",get e "externalProfileRevision"),("contractHash",str (canonicalHash (get snapshot "acceptedTask"))),("receiver",str (receiverIdentity (externalDelivery profile))),("sourceObservationHash",get observed "observationHash"),("recordIds",ids),("content",obj [("text",str reply)])]
 pure (put "requestHash" (str (canonicalHash payload)) payload)

validateReservation :: Value -> IO Value
validateReservation work=either (failure 409 . T.pack) pure (parseEither (const (checkedReservation work)) Null)

checkedReservation :: Value -> Parser Value
checkedReservation work=do
 let e=external work; delivery=get e "delivery"; request=get delivery "request"
 revision<-withObject "External execution" (.: "externalProfileRevision") e :: Parser Int
 unless (revision>0) (fail "External profile revision is invalid")
 unless (at (get e "contentAcceptance") "status"=="accepted" && at (get e "cursorCommit") "status"=="committed") (fail "Delivery has no accepted content/cursor reservation")
 unless (canonicalHash (get e "profile")==at e "profileHash" && canonicalHash (get (get e "read") "response")==at (get e "read") "observationHash") (fail "Pinned external observation/profile identity changed")
 expected<-requestPayload work (at (get work "outcome") "reply")
 unless (request==expected && all (\key->get delivery key==get request key) ["operationKey","scope","profileHash","requestHash"]) (fail "Saved delivery payload or immutable identity changed")
 pure request

-- Audit-only recovery never creates a new publication or cursor reservation.
auditPublication :: Value -> Either String (Maybe Text)
auditPublication work
 | not (profileBound work)=Right Nothing
 | otherwise=parseEither (const checked) Null
 where
 checked=do
  let e=external work; delivery=get e "delivery"
  unless (at (get e "contentAcceptance") "status"=="accepted" && at (get e "cursorCommit") "status"=="committed" && at delivery "status" `elem` ["confirmed","not_required"]) (fail "Saved receipt-feed result still requires original content acceptance or publication; audit-only recovery cannot advance its cursor or send it")
  request<-checkedReservation work
  unless (if at delivery "status"=="confirmed" then validReceipt request (get delivery "receipt") else null (items request "recordIds") && get request "recordIds"==arr []) (fail "Saved publication lacks an independent confirmed receipt or a no-delivery proof")
  pure (Just (at (get request "content") "text"))

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
   requireConsumerClearOn db work (Just (at work "workloadId"))
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
 | not suspicious = work
 | has work "codeExecution" = put "routineEffectUnknown" (obj [("decisionId",str identity),("reason",str "Response-only code stage contains tool activity or lost event identity"),("evidence",toJSON evidence)]) work
 | not (profileBound work) = work
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
 forM_ (find (\work->any (\call->get call "callId"==toJSON identity) (items work "calls")) works) $ \work->do
  scope<-routineConsumerOn db work
  forM_ scope $ \owner->do
   requireClearOn db owner Nothing
   require (routineRisk False work/=RoutineUnverified) 409 "This legacy Routine attempt has no retained start/absence proof; its exact Retry is unavailable"

recover :: Service -> IO ()
recover s=DB.connection s $ \db->SQL.withTransaction db $ do
 works<-worksOn db
 forM_ works $ \work->do
  scope<-try (routineConsumerOn db work) :: IO (Either APIError (Maybe RoutineConsumer))
  let retained=case scope of
       Left (APIError _ reason)->put "routineIdentityError" (str reason) work
       Right Nothing->work
       Right (Just _)->retainRoutineRisk True work
      updated=if profileBound retained then interrupted True retained else retained
  when (updated/=work) (saveWorkOn db updated)

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
