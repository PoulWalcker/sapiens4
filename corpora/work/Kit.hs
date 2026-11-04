{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- Owner-personal immutable scripts; consumer contracts and acceptance stay local.
module Kit (catalog, resolve, resolvePreparation, descriptor, targetContext, issues, issueFor, executionAllowed, consumers, bindingPin, bindingCurrent) where

import Control.Exception
import Control.Monad
import Data.Aeson hiding (pairs)
import Data.Aeson.Types (parseEither)
import qualified Data.ByteString.Lazy as BL
import Data.List (find,nubBy,sortOn)
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Types
import qualified GeneratedCode as Code
import qualified Workflow

family :: Value -> Value
family version=obj [("input",str (if get version "inputBinding"==Null then "json" else "receipt-feed-input-v1")),("content",get (get version "inputBinding") "contentInterface"),("hybrid",Bool (has version "explanation"))]

descriptor :: Value -> Value
descriptor version=put "family" (family version) (select ["source","sourceHash","language","interface","runtime","dependencies","capabilities"] version)

targetContext :: Value -> Value
targetContext target=obj [("input",str (if get target "feedProfile" `fallback` get target "externalProfile"==Null then "json" else "receipt-feed-input-v1")),("content",get target "contentInterface"),("hybrid",Bool (get target "rubric"/=Null || at target "contentInterface"=="hybrid-explanation-v1"))]

validateLocal :: Value -> IO ()
validateLocal version=forM_ [get version "kitArtifact" | has version "kitArtifact"] $ \artifact->do
 require (at artifact "scope"=="personal" && at artifact "id"==canonicalHash (descriptor version)) 409 "Pinned Kit artifact bytes or runtime descriptor changed"
 require (get artifact "owner"==get (get version "reference") "owner") 409 "Pinned Kit artifact belongs to another owner"

resolvePreparation :: AgentId -> Value -> CandidateReference -> IO Value
resolvePreparation owner source reference@(CandidateReference refOwner preparation digest)=do
 let version=get (get source "preparation") "version"
 require (refOwner==owner && at source "workloadId"==runIdText preparation && get (get source "standalonePin" `fallback` get source "preparationPin") "owner"==toJSON owner) 409 "Candidate reference belongs to another preparation or owner"
 require (get version "reference"==toJSON reference && canonicalHash (remove ["reference"] version)==digest && (if has version "workflow" then Workflow.contentHash (get version "workflow") else sourceHash (at version "source"))==at version "sourceHash") 409 "Pinned candidate content or reference changed"
 when (has version "workflow") (either (failure 409) pure (Workflow.validate (get version "workflow")))
 void (either (failure 409 . T.pack) pure (parseEither candidateExplanation version))
 validateLocal version
 pure version

personal :: Text -> Value -> Bool
personal owner work=at (get work "preparationPin") "owner"==owner && not ("group_" `T.isPrefixOf` owner)
 && get (get work "origin") "group"==Null && get (get work "preparationPin") "group"==Null

checked :: Value -> Value -> Bool
checked work version=let prepared=get work "preparation"; checks=items prepared "checks" in
 not (has version "workflow") && at prepared "status"=="awaiting_occurrence" && get (get prepared "review") "accepted"==Bool True && not (null checks)
 && all (\check->get check "matched"==Bool True && get check "version"==get version "reference" && get check "sourceHash"==get version "sourceHash" && has check "actual" && has check "expected" && get check "actual"==get check "expected" && at check "status"=="exited" && get check "exitCode"==num 0 && not (boolean (get check "wasRunning"))) checks
 && (not (has version "explanation") || all (\check->at (get check "actual") "status"/="ok" || get check "rubricAccepted"==Bool True && has check "explanationOperation") checks)

-- One stable issue per owner/source bytes. Repeated failures do not mint work.
issues :: Text -> [Value] -> [Value]
issues owner works=nubBy (\left right->get left "id"==get right "id")
 [obj [("id",str (canonicalHash (obj [("owner",str owner),("sourceHash",str hash_)]))),("owner",str owner),("sourceHash",str hash_),("artifact",str artifact),("reference",reference),("run",get work "workloadId"),("routine",get (get work "origin") "routine"),("reason",get fault "reason"),("scope",get fault "scope")]
 | work<-works,let code=get work "codeExecution",let reference=get code "reference",at reference "owner"==owner
 ,let version=fromMaybe Null (listToMaybe [get (get source "preparation") "version" | source<-works,get (get (get source "preparation") "version") "reference"==reference])
 ,fault<-items code "faults",shared version code fault
 ,let hash_=at fault "sourceHash" `otherwiseText` at version "sourceHash",not (T.null hash_)
 ,let artifact=at fault "artifact" `otherwiseText` canonicalHash (descriptor version)]
 where
 otherwiseText left right=if T.null left then right else left
 shared version code fault=case at fault "scope" of
  "explanation"->False
  "calculation"->True
  "composition"->not (has version "explanation") || any numericFailure (items code "operations")
   where numericFailure op=any (\criterion->at criterion "status"=="fail" && any (\rubric->get rubric "id"==get criterion "id" && at rubric "kind"=="numeric") (items version "rubric")) (items (get op "response") "criteria")
  _->True -- Unattributed legacy evidence remains suspect, never silently healthy.

badIdentities :: Text -> [Value] -> ([Text],[Text])
badIdentities owner works=(map (`at` "artifact") found,map (`at` "sourceHash") found)
 where found=issues owner works

issueFor :: Text -> Value -> [Value] -> Maybe Value
issueFor owner version works=find ((==get version "sourceHash") . (`get` "sourceHash")) (issues owner works)

executionAllowed :: Value -> Value -> [Value] -> Bool
executionAllowed definition version works=case issueFor (at definition "owner") version works of
 Nothing->True
 Just issue->any (\inspection->get inspection "issue"==get issue "id" && get inspection "reference"==get version "reference" && get inspection "contractGeneration"==get definition "contractGeneration" && at inspection "status"=="completed" && get (get inspection "verdict") "affected"==Bool False) [inspection | saved<-items definition "repairCases",inspection<-items saved "kitInspections"]

consumers :: Text -> Value -> [Value] -> [Value]
consumers owner version works=[obj [("routine",get (get work "preparationPin") "routine"),("reference",get candidate "reference"),("sourceHash",get candidate "sourceHash")] | work<-works,personal owner work,let candidate=get (get work "preparation") "version",get candidate "sourceHash"==get version "sourceHash",checked work candidate]

bindingPin :: Value -> Value
bindingPin definition=mergeFields [("case",get saved "id") | saved<-items definition "repairCases",get saved "id"==get definition "repairCase",get saved "contractGeneration"==get definition "contractGeneration",any (\inspection->get inspection "reference"==get (get definition "activeCode") "reference") (items saved "kitInspections")]
 (obj [("reference",get (get definition "activeCode") "reference"),("contractGeneration",get definition "contractGeneration"),("revision",get definition "revision")])

bindingCurrent :: Value -> Value -> Bool
bindingCurrent definition pin=(not (has pin "revision") || get definition "revision"==get pin "revision") && get (get definition "activeCode") "reference"==get pin "reference" && get definition "contractGeneration"==get pin "contractGeneration"
 && (not (has pin "case") || get definition "repairCase"==get pin "case" && any (\saved->get saved "id"==get pin "case" && not (has saved "userStop") && at saved "status" `notElem` ["stopped","stale"]) (items definition "repairCases"))

reason :: Value -> Value -> Maybe Value -> Text
reason target version available
 | at version "language"/="python"="Artifact language is unavailable; no automatic translation"
 | at version "interface"/="json-stdin-result-v1"="Artifact execution interface is unsupported"
 | get version "dependencies"/=arr []="Artifact dependencies are unavailable; package installation is not authorized"
 | get version "capabilities"/=arr []="Artifact requires unsupported capabilities"
 | not (Code.acceptsProfile (get version "runtime"))="Artifact runtime/version is unavailable on this verified host"
 | get (targetContext target) "input"/=get (family version) "input"="Artifact input family differs from this consumer"
 | get (targetContext target) "content"/=Null && get (targetContext target) "content"/=get (family version) "content"="Artifact content interface differs from this consumer"
 | has target "rubric" && get (targetContext target) "hybrid"/=get (family version) "hybrid"="Artifact calculation/explanation family differs from this consumer"
 | maybe False (/=get version "runtime") available="Artifact pinned runtime differs from the currently available environment"
 | otherwise=""

provenance :: Value -> Value -> Value
provenance work version=obj [("preparation",get version "reference"),("routine",get (get work "preparationPin") "routine"),("contract",select ["objective","expectedOutputs","completionCriteria"] (get version "contract")),("reviewAccepted",Bool True)]

catalog :: Text -> Value -> [Value] -> IO Value
catalog owner target works=do
 let (badDescriptors,badSources)=badIdentities owner works
 rows<-forM [work | work<-reverse works,personal owner work,has (get work "preparation") "version",get (get (get work "preparation") "review") "accepted"==Bool True] $ \work->do
  let version=get (get work "preparation") "version"; ref=get version "reference"
      identity=canonicalHash (descriptor version)
      summary=obj [("id",str identity),("reference",ref),("owner",str owner),("scope",str "personal"),("sourceHash",get version "sourceHash"),("language",get version "language"),("interface",get version "interface"),("runtime",get version "runtime"),("dependencies",get version "dependencies"),("capabilities",get version "capabilities"),("family",family version),("provenance",provenance work version),("consumers",arr (consumers owner version works))]
  validated<-try (decodeValue 409 ref >>= resolvePreparation (AgentId owner) work) :: IO (Either APIError Value)
  let refusal=case validated of
       Left (APIError _ message)->message
       Right _ | not (checked work version)->"Artifact lacks completed independent checks and review"
               | identity `elem` badDescriptors || at version "sourceHash" `elem` badSources->"Known faulty artifact cannot be selected through another alias"
               | otherwise->reason target version Nothing
  pure (mergeFields [("compatible",Bool (T.null refusal)),("reason",str (if T.null refusal then "Structural match; actual environment and this consumer's own checks are still required" else refusal))] summary)
 let unique=nubBy (\left right->get left "id"==get right "id") (sortOn (not . boolean . (`get` "compatible")) rows)
     fit (selected,omitted) entry=if BL.length (encode (arr (selected++[entry])))<=32768 then (selected++[entry],omitted) else (selected,omitted+1)
     (selected,omitted)=foldl fit ([],0::Int) unique
 pure (obj [("owner",str owner),("scope",str "personal"),("entries",arr selected),("omittedEntries",num omitted),("limitBytes",num 32768)])

resolve :: Text -> Value -> Value -> [Value] -> Value -> IO (Value,Value)
resolve owner target available works raw=do
 reference@(CandidateReference refOwner preparation _)<-decodeValue 409 raw
 require (refOwner==AgentId owner) 409 "Kit artifact belongs to another owner or scope"
 source<-maybe (failure 409 "Selected Kit artifact is missing") pure (find ((==runIdText preparation) . (`at` "workloadId")) works)
 require (personal owner source) 409 "Kit artifact is outside the personal owner scope"
 version<-resolvePreparation (AgentId owner) source reference
 require (checked source version) 409 "Kit artifact has no completed independent checks and review"
 let (badDescriptors,badSources)=badIdentities owner works
 require (canonicalHash (descriptor version) `notElem` badDescriptors && at version "sourceHash" `notElem` badSources) 409 "Known faulty artifact cannot be selected through another alias"
 let refusal=reason target version (Just available)
 require (T.null refusal) 409 refusal
 let artifact=obj [("id",str (canonicalHash (descriptor version))),("owner",str owner),("scope",str "personal"),("sourceHash",get version "sourceHash"),("provenance",provenance source version)]
 pure (version,artifact)
