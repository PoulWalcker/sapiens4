module Sapiens.RoutineEffects (handle) where

import Sapiens.Form as F
import Sapiens.Domain as X
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
  RoutineReconcile record -> do
    m <- H.get
    case (M.routineView m).reconciliation of
      Just request | request.busy -> E.notify "The receipt lookup is still in progress."
      Just _ -> do
        operation <- liftEffect B.uuid
        sendReconciliation m {record,operation,busy:false,error:""}
      Nothing -> do
        operation <- liftEffect B.uuid
        sendReconciliation m {record,operation,busy:false,error:""}
  RoutineRetryReconciliation -> do
    m <- H.get
    case (M.routineView m).reconciliation of
      Just request | not request.busy -> sendReconciliation m request
      _ -> pure unit
  RoutineEvidence routine run -> do
    m <- H.get
    fetchEvidence m routine run
  RoutineRefreshEvidence -> do
    m <- H.get
    let view = M.routineView m
    case view.evidence of
      Just evidence | not evidence.busy && evidence.error == "" -> do
        let routine = A.find ((evidence.routine == _) <<< D.s "id") $ D.rows "personal_routines" m.snapshot.automation
            run = routine >>= A.find ((evidence.run == _) <<< D.s "id") <<< D.rows "runs"
            showing = case view.page of
              M.RoutineOccurrence rid runId -> rid == evidence.routine && runId == evidence.run
              M.RoutineLatest rid -> rid == evidence.routine && (D.s "id" <$> (routine >>= A.last <<< D.rows "runs")) == Just evidence.run
              _ -> false
        when (showing && (D.s "detailVersion" <$> run) /= Just evidence.version) $ fetchEvidence m evidence.routine evidence.run
      _ -> pure unit
  RoutineNavigate page -> do
    m <- H.get
    let view = M.routineView m
    if maybeDirty view.editor && page /= view.page then do
      H.modify_ $ M.modifyRoutineView (M.routineKey m) _ {pending=Just page}
      dispatch $ OpenDialog M.RoutineDiscard $ F.RoutineRecord D.empty
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
  RoutineReloadEditor -> do
    m <- H.get
    let page = (M.routineView m).page
        key = M.routineKey m
        identity = _.identity <$> (M.routineView m).editor
        operation = _.operation <$> (M.routineView m).editor
    E.attempt (E.api runtime m "/api/state" "GET" D.null) \result -> E.inEpoch m.epoch \_ -> case result of
      Left failure -> E.notify $ message failure
      Right snapshot -> do
        H.modify_ $ M.project snapshot
        current <- H.get
        when (M.routineKey current == key && (_.identity <$> (M.routineView current).editor) == identity && (_.operation <$> (M.routineView current).editor) == operation) $ navigate page
  RoutineSave -> do
    m <- H.get
    case (M.routineView m).editor of
      Nothing -> pure unit
      Just editor | editor.busy -> pure unit
      Just editor -> do
        let key = M.routineKey m
            id = D.s "id" editor.record
            scheduling = case (M.routineView m).page of
              M.RoutineSchedule _ -> true
              _ -> false
            path = "/api/agents/" <> B.encodeURI (D.s "owner" editor.record) <> "/routines" <> if id == "" then "" else "/" <> B.encodeURI id <> if scheduling then "/schedule" else "/edit"
            body = if scheduling then D.json {operation:editor.operation,revision:D.get "revision" editor.record,minutes:D.parse $ D.s "minutes" editor.fields} else D.json {operation:editor.operation,revision:D.get "revision" editor.record,title:D.s "title" editor.fields,goal:D.s "goal" editor.fields,criteria:A.filter (_ /= "") $ map S.trim $ S.split (S.Pattern "\n") $ D.s "criteria" editor.fields}
            update f = H.modify_ $ M.modifyRoutineView key \view -> view {editor=map (\current -> if current.identity == editor.identity then f current else current) view.editor}
        update _ {busy=true,error=""}
        E.attempt (E.api runtime m path "POST" body) \result -> E.inEpoch m.epoch \_ -> case result of
          Left failure -> update _ {busy=false,error=message failure}
          Right response -> do
            H.modify_ $ M.modifyRoutineView key \view -> if (_.identity <$> view.editor) == Just editor.identity then view {page=M.RoutineLatest (D.s "routine" response),editor=Nothing,pending=Nothing} else view
            dispatch Poll
  RoutineScheduleAction action record -> do
    m <- H.get
    case (M.routineView m).scheduleRequest of
      Just request | request.action == action && D.s "id" request.record == D.s "id" record -> sendSchedule m request
      Just _ -> E.notify "Resolve the previous schedule request before starting another."
      Nothing -> do
        operation <- liftEffect B.uuid
        sendSchedule m {action,record,operation,busy:false,error:""}
  RoutineRetrySchedule -> do
    m <- H.get
    case (M.routineView m).scheduleRequest of
      Just request -> sendSchedule m request
      Nothing -> pure unit
  RoutineRefreshSchedule -> do
    m <- H.get
    let key = M.routineKey m
        operation = _.operation <$> (M.routineView m).scheduleRequest
    E.attempt (E.api runtime m "/api/state" "GET" D.null) \result -> E.inEpoch m.epoch \_ -> case result of
      Left failure -> E.notify $ message failure
      Right snapshot -> H.modify_ \current -> M.modifyRoutineView key (\view -> if (_.operation <$> view.scheduleRequest) == operation then view {scheduleRequest=Nothing} else view) $ M.project snapshot current
  _ -> pure unit
  where
  sendReconciliation m request = do
    let key = M.routineKey m
        record = request.record
        path = "/api/agents/" <> B.encodeURI (D.s "owner" record) <> "/routines/" <> B.encodeURI (D.s "id" record) <> "/runs/" <> B.encodeURI (D.s "run" record) <> "/reconcile-delivery"
    H.modify_ $ M.modifyRoutineView key _ {reconciliation=Just request {busy=true,error=""}}
    E.attempt (E.api runtime m path "POST" $ D.json {operation:request.operation}) \result -> E.inEpoch m.epoch \_ -> case result of
      Left failure -> H.modify_ $ M.modifyRoutineView key _ {reconciliation=Just request {busy=false,error=message failure}}
      Right _ -> do
        H.modify_ $ M.modifyRoutineView key _ {reconciliation=Nothing}
        dispatch Poll
  fetchEvidence m routine run = do
    let key = M.routineKey m
        request = m.sequence + 1
        selected = A.find ((routine == _) <<< D.s "id") $ D.rows "personal_routines" m.snapshot.automation
        summary = selected >>= A.find ((run == _) <<< D.s "id") <<< D.rows "runs"
        version = fromMaybe "" $ D.s "detailVersion" <$> summary
        path = "/api/agents/" <> B.encodeURI m.selected <> "/routines/" <> B.encodeURI routine <> "/runs/" <> B.encodeURI run
    H.modify_ \current -> M.modifyRoutineView key (_ {evidence=Just {routine,run,version,record:D.null,busy:true,error:"",request}}) current {sequence=request}
    E.attempt (E.api runtime m path "GET" D.null) \result -> E.inEpoch m.epoch \current ->
      when (M.routineKey current == key && (_.request <$> (M.routineView current).evidence) == Just request) $
        H.modify_ $ M.modifyRoutineView key \view -> view {evidence=map (\evidence -> case result of
          Left failure -> evidence {busy=false,error=message failure}
          Right record -> evidence {busy=false,record=record,version=D.s "detailVersion" record}) view.evidence}

  sendSchedule m request = unless request.busy do
    let key = M.routineKey m
        action = case request.action of
          M.PauseSchedule -> "pause"
          M.ResumeSchedule -> "resume"
        path = "/api/agents/" <> B.encodeURI (D.s "owner" request.record) <> "/routines/" <> B.encodeURI (D.s "id" request.record) <> "/" <> action
        body = D.json {operation:request.operation,revision:D.get "revision" request.record}
    H.modify_ $ M.modifyRoutineView key _ {scheduleRequest=Just request {busy=true,error=""}}
    E.attempt (E.api runtime m path "POST" body) \result -> E.inEpoch m.epoch \_ -> do
      H.modify_ $ M.modifyRoutineView key \view -> if (_.operation <$> view.scheduleRequest) /= Just request.operation then view else view {scheduleRequest=case result of
        Left failure -> Just request {busy=false,error=message failure}
        Right _ -> Nothing}
      dispatch Poll

  maybeDirty editor = fromMaybe false $ (\e -> e.dirty) <$> editor
  navigate page = do
    m <- H.get
    case page of
      M.RoutineSettings id -> prepareEditor m page id false
      M.RoutineSchedule id -> prepareEditor m page id true
      _ -> H.modify_ $ M.modifyRoutineView (M.routineKey m) _ {page=page,editor=Nothing,pending=Nothing,evidence=Nothing}
    liftEffect $ B.scroll "#conversation-body" 0.0

  prepareEditor m page id scheduling = do
    operation <- liftEffect B.uuid
    let record = D.put "owner" (D.text m.selected) $ fromMaybe D.empty $ A.find (\r -> D.s "id" r == id && D.s "owner" r == m.selected) $ D.rows "personal_routines" m.snapshot.automation
        fields = if scheduling then D.json {minutes:show $ if D.n "minutes" record > 0.0 then D.int $ D.get "minutes" record else 60} else D.json {title:D.s "title" record,goal:D.s "goal" record,criteria:S.joinWith "\n" $ D.strings $ D.get "criteria" record}
        editor = {record,fields,operation,error:"",busy:false,dirty:false,identity:m.sequence+1}
    H.modify_ \current -> M.modifyRoutineView (M.routineKey m) (_ {page=page,editor=Just editor,pending=Nothing,evidence=Nothing}) current {sequence=m.sequence+1,menu=Nothing}
