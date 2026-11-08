module Sapiens.RoutineRequest (Intent(..), SendReceipt(..), owner, label, message, decodeReceipt) where

import Prelude
import Data.Argonaut.Core (Json, toString)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String as S
import Foreign.Object as O
import Sapiens.Data as D
import Sapiens.Navigation as N
import Sapiens.RoutineData as RD

data Intent = CreateRoutine N.OwnerRef | ChangeRoutine N.RoutineRef | RequestRun N.RoutineRef | ReplyToRun N.RunRef (Maybe RD.ExactResultRef)
derive instance eqIntent :: Eq Intent

data SendReceipt = PersonalTurnReceipt String | GroupMessageReceipt String
derive instance eqSendReceipt :: Eq SendReceipt

owner :: Intent -> N.OwnerRef
owner = case _ of
  CreateRoutine ref -> ref
  ChangeRoutine ref -> ref.owner
  RequestRun ref -> ref.owner
  ReplyToRun ref _ -> ref.routine.owner

label :: Intent -> String
label = case _ of
  CreateRoutine _ -> "New routine"
  ChangeRoutine _ -> "Request a change"
  RequestRun _ -> "Ask Sapi to run"
  ReplyToRun _ _ -> "Reply to Sapi"

message :: O.Object String -> Intent -> String -> Boolean -> Either String String
message templates intent text firstRun = do
  template <- case O.lookup key templates of
    Just value | S.trim value /= "" -> Right value
    _ -> Left "Request template is unavailable. Refresh before sending."
  if needsText && S.trim text == "" then Left "Write your request before sending." else
    Right $ S.replaceAll (S.Pattern "{message}") (S.Replacement $ S.trim text)
      $ S.replaceAll (S.Pattern "{context}") (S.Replacement context) template
  where
  key = case intent of
    CreateRoutine _ -> if firstRun then "create-run" else "create"
    ChangeRoutine _ -> "change"
    RequestRun _ -> "run"
    ReplyToRun _ _ -> "reply"
  needsText = case intent of
    RequestRun _ -> false
    _ -> true
  context = case intent of
    CreateRoutine _ -> ""
    ChangeRoutine ref -> "Routine @" <> N.routineId ref.id
    RequestRun ref -> "Routine @" <> N.routineId ref.id
    ReplyToRun ref result -> "Routine @" <> N.routineId ref.routine.id <> "\nRun @" <> N.runId ref.id <> case result of
      Nothing -> "\nNo saved result selected."
      Just (RD.PersonalDecision decision) -> "\nSaved decision @" <> decision
      Just (RD.GroupResult request revision) -> "\nSaved Group result: request @" <> N.runId request <> ", revision " <> show revision

decodeReceipt :: N.OwnerRef -> Json -> Either String SendReceipt
decodeReceipt target value = do
  id <- case toString (D.get "id" value) of
    Just id | S.trim id /= "" -> Right id
    _ -> Left "The server did not confirm a message identity."
  case target of
    N.PersonalOwner agent | D.s "agent" value == agent -> Right $ PersonalTurnReceipt id
    N.GroupOwner _ | D.s "author" value == "admin" || D.s "actor" value == "admin" -> Right $ GroupMessageReceipt id
    _ -> Left "The message receipt did not match its destination."
