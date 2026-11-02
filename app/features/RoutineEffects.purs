module Sapiens.RoutineEffects (handle) where

import Prelude
import Data.Array as A
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String as S
import Effect.Class (liftEffect)
import Effect.Exception (message)
import Halogen as H
import Libs4.Browser as B
import Sapiens.Action (Action(..))
import Sapiens.Connections as C
import Sapiens.Data as D
import Sapiens.Effects as E
import Sapiens.Model as M

handle :: C.Runtime -> E.Dispatch -> Action -> E.UI Unit
handle runtime dispatch = case _ of
  RoutineNavigate page -> do
    m <- H.get
    let view = M.routineView m
    if maybeDirty view.editor && page /= view.page then do
      H.modify_ $ M.modifyRoutineView (M.routineKey m) _ {pending=Just page}
      dispatch $ OpenDialog M.RoutineDiscard D.empty
    else navigate page
  RoutineDiscardChanges -> do
    m <- H.get
    let next = fromMaybe (M.RoutineList false) $ (M.routineView m).pending
    H.modify_ $ M.modifyRoutineView (M.routineKey m) _ {editor=Nothing,pending=Nothing}
    navigate next
  RoutineField key value -> do
    m <- H.get
    operation <- liftEffect B.uuid
    H.modify_ $ M.modifyRoutineView (M.routineKey m) \view -> view {editor=map (\e -> e {fields=D.put key (D.text value) e.fields,operation=operation,dirty=true,error=""}) view.editor}
  RoutineSave -> do
    m <- H.get
    case (M.routineView m).editor of
      Nothing -> pure unit
      Just editor | editor.busy -> pure unit
      Just editor -> do
        let key = M.routineKey m
            id = D.s "id" editor.record
            path = "/api/agents/" <> B.encodeURI (D.s "owner" editor.record) <> "/routines" <> if id == "" then "" else "/" <> B.encodeURI id <> "/edit"
            body = D.json {operation:editor.operation,revision:D.get "revision" editor.record,title:D.s "title" editor.fields,goal:D.s "goal" editor.fields,criteria:A.filter (_ /= "") $ map S.trim $ S.split (S.Pattern "\n") $ D.s "criteria" editor.fields}
            update f = H.modify_ $ M.modifyRoutineView key \view -> view {editor=map (\current -> if current.identity == editor.identity then f current else current) view.editor}
        update _ {busy=true,error=""}
        E.attempt (E.api runtime m path "POST" body) \result -> E.inEpoch m.epoch \_ -> case result of
          Left failure -> update _ {busy=false,error=message failure}
          Right response -> do
            H.modify_ $ M.modifyRoutineView key \view -> if (_.identity <$> view.editor) == Just editor.identity then view {page=M.RoutineLatest (D.s "routine" response),editor=Nothing,pending=Nothing} else view
            dispatch Poll
  _ -> pure unit
  where
  maybeDirty editor = fromMaybe false $ (\e -> e.dirty) <$> editor
  navigate page = do
    m <- H.get
    case page of
      M.RoutineSettings id -> do
        operation <- liftEffect B.uuid
        let record = D.put "owner" (D.text m.selected) $ fromMaybe D.empty $ A.find (\r -> D.s "id" r == id && D.s "owner" r == m.selected) $ D.rows "personal_routines" m.snapshot
            fields = D.json {title:D.s "title" record,goal:D.s "goal" record,criteria:S.joinWith "\n" $ D.strings $ D.get "criteria" record}
            editor = {record,fields,operation,error:"",busy:false,dirty:false,identity:m.sequence+1}
        H.modify_ \current -> M.modifyRoutineView (M.routineKey m) (_ {page=page,editor=Just editor,pending=Nothing}) current {sequence=m.sequence+1,menu=Nothing}
      _ -> H.modify_ $ M.modifyRoutineView (M.routineKey m) _ {page=page,editor=Nothing,pending=Nothing}
    liftEffect $ B.scroll "#conversation-body" 0.0
