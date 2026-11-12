module Sapiens.Effects where

import Prelude
import Data.Argonaut.Core (Json)
import Data.Array as A
import Data.Either (Either(..), either)
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff, Milliseconds(..), delay, try, throwError)
import Effect.Class (liftEffect)
import Sapiens.Updates as Updates
import Effect.Exception (Error, error, message)
import Halogen as H
import Sapiens.Browser as Browser
import Sapiens.Platform as Platform
import Libs4.Browser as B
import Sapiens.Action (Action(..))
import Sapiens.Connections as C
import Sapiens.Data as D
import Sapiens.Domain as X
import Sapiens.Model as M
import Sapiens.Navigation as N
import Sapiens.Workspace as W
import Sapiens.Update as U

type UI = H.HalogenM M.Model Action () Void Aff
type Dispatch = Action -> UI Unit

later :: Number -> UI Unit -> UI Unit
later ms action = void $ H.fork $ H.liftAff (delay $ Milliseconds ms) *> action

attempt :: forall a. Aff a -> (Either Error a -> UI Unit) -> UI Unit
attempt operation receive = void $ H.fork $ H.liftAff (try operation) >>= receive

inEpoch :: Int -> (M.Model -> UI Unit) -> UI Unit
inEpoch epoch receive = H.get >>= \m -> when (epoch == m.epoch) $ receive m

withDialog :: (M.Dialog -> UI Unit) -> UI Unit
withDialog receive = H.gets _.dialog >>= traverse_ receive

inDialog :: Int -> (M.Dialog -> UI Unit) -> UI Unit
inDialog id receive = withDialog \d -> when (d.identity == id) $ receive d

modifyDialog :: Int -> (M.Dialog -> M.Dialog) -> UI Unit
modifyDialog id f = H.modify_ \m -> m {dialog=map (\d -> if d.identity == id then f d else d) m.dialog}

refresh :: C.Runtime -> Dispatch -> M.Model -> Aff Json -> UI Unit
refresh runtime dispatch requested operation = do
  source <- liftEffect $ Updates.generation runtime.updates
  attempt operation \result -> inEpoch requested.epoch \m ->
    when (requested.active == m.active) $ case result of
      Left failure -> runUpdate runtime dispatch $ U.SnapshotReceived requested.epoch requested.active $ Left $ message failure
      Right raw -> do
        let content = X.snapshotContent raw
        now <- liftEffect B.now
        incoming <- if D.str (D.at ["corpora","id"] raw) /= (M.activeRecord requested).id then
          pure $ Left "Invalid snapshot response: corpora identity does not match the requested workspace"
        else if m.snapshotContent == Just content then pure $ Right U.KeepSnapshot else do
          captureScroll
          decorated <- liftEffect $ if m.snapshot.corpora.id /= "" && m.snapshot.corpora.id == D.str (D.at ["corpora","id"] raw) then pure raw else C.decorate requested.active raw
          pure $ (\snapshot -> U.ReplaceSnapshot {snapshot,content}) <$> X.decodeSnapshot decorated
        traverse_ (const $ liftEffect $ C.cacheSnapshot runtime requested.active content raw) incoming
        runUpdate runtime dispatch $ U.SnapshotReceived requested.epoch requested.active $ (\change -> {change,now,source,revision:D.n "updates_revision" raw}) <$> incoming

api :: C.Runtime -> M.Model -> String -> String -> Json -> Aff Json
api runtime m path method body = do
  when ((M.busy m) || m.active == "") $ throwError $ error "Connecting…"
  when (not (M.online m) && method /= "GET") $ throwError $ error "Offline — read only."
  C.request runtime (M.remote m) (M.activeRecord m) path method body

notify :: String -> UI Unit
notify text = do
  H.modify_ _ { toast=text }
  later 3200.0 $ H.modify_ \m -> if m.toast == text then m {toast=""} else m

persistLocal :: UI Unit
persistLocal = do
  m <- H.get
  when (m.active /= "") do
    ok <- liftEffect $ C.write ("s4.workspace." <> m.active) $ M.preferences m
    unless ok $ H.modify_ _ { feedback="Device storage is unavailable. Keep this window open to retain your draft." }

save :: Dispatch -> UI Unit
save dispatch = persistLocal *> later 300.0 (dispatch Persist)

requestDocument :: C.Runtime -> Dispatch -> UI Unit
requestDocument runtime dispatch = runUpdate runtime dispatch U.RequestDocument

preview :: C.Runtime -> Dispatch -> UI Unit
preview runtime dispatch = runUpdate runtime dispatch U.RequestPreview

runUpdate :: C.Runtime -> Dispatch -> U.Action -> UI Unit
runUpdate runtime dispatch action = do
  requested <- H.get
  case U.update action requested of
    U.Unchanged -> pure unit
    U.Changed model commands -> H.put model *> traverse_ (execute requested) commands
    U.Commands commands -> traverse_ (execute requested) commands
  where
  execute requested = case _ of
    U.AcknowledgeSnapshot source revision -> liftEffect $ Updates.applied runtime.updates source revision
    U.StopUpdates -> liftEffect $ Updates.stop runtime.updates
    U.SaveRegistry records -> void $ liftEffect $ C.write "s4.records" $ D.json records
    U.SaveActive key -> void $ liftEffect $ C.write "s4.active" $ D.text key
    U.ReplaceHistory path -> liftEffect $ B.historyReplace path
    U.ConnectRecord record -> do
      current <- H.get
      liftEffect $ Updates.stop runtime.updates
      refresh runtime dispatch current $ C.authenticate runtime (M.remote current) record *> C.request runtime (M.remote current) record "/api/state" "GET" D.null
    U.ChangeLifecycle command -> attempt (C.reconcile runtime (M.remote requested) requested.records requested.active *> C.lifecycle runtime (M.remote requested) (M.activeRecord requested) (X.lifecycleName command)) $
      runUpdate runtime dispatch <<< U.LifecycleReceived requested.epoch <<< either (Left <<< message) (Right <<< X.corpora)
    U.Dispatch next -> dispatch next
    U.ReadVisibility -> do
      env <- liftEffect Platform.environment
      execute requested $ if env.hidden then U.Suspend else U.Resume
    U.Suspend -> do
      liftEffect $ Updates.stop runtime.updates
      captureScroll
      persistLocal
      env <- liftEffect Platform.environment
      H.modify_ _ {restoringScroll=true,env=env}
      H.get >>= liftEffect <<< W.sync runtime.idleTabs
      dispatch StopScan
    U.Resume -> dispatch Resize *> restoreScroll *> later 32.0 restoreScroll *> dispatch Poll
    U.SendNative report ->
      let path = "/api/" <> (if D.cut 6 report.owner == "group_" then "groups/" else "agents/") <> B.encodeURI report.owner
          body = if report.op == "" then report.body else D.put "op" (D.text $ "workspace_" <> report.op) report.body
      in attempt (api runtime requested (path <> if report.op == "" then "/browser" else "/control") "POST" body *> api runtime requested "/api/state" "GET" D.null) $
        runUpdate runtime dispatch <<< U.NativeCompleted requested.epoch report <<< either (Left <<< message) Right
    U.FetchSnapshot generation -> refresh runtime dispatch (requested {polling=true,pollGeneration=generation}) $ api runtime requested "/api/state" "GET" D.null
    U.SchedulePoll epoch active generation ms -> do
      m <- H.get
      when (M.online m && (M.activeRecord m).location == X.Local && A.any M.blocksChat m.snapshot.turns) tickProgress
      if M.remote m || (M.activeRecord m).location /= X.Local then later ms fallback
      else do
        liftEffect $ Updates.connect runtime.updates (D.replace "^http" "ws" m.env.origin <> "/api/host/corpora/" <> B.encodeURI (M.activeRecord m).id <> "/api/updates")
        state <- liftEffect $ Updates.freshness runtime.updates
        case M.online m, state of
          true, Just (Updates.Applied _) -> when (m.fallbackDelay /= M.fastPoll) $ H.modify_ _ {fallbackDelay=M.fastPoll}
          true, Just (Updates.Pending _) -> dispatch Poll
          _, _ -> later ms do
            online <- H.gets M.online
            latest <- liftEffect $ Updates.freshness runtime.updates
            case online, latest of
              true, Just (Updates.Applied _) -> pure unit
              _, _ -> fallback
      where
      -- Elapsed time can change while a provider is silent; it needs no network request.
      tickProgress = later 1000.0 $ inEpoch epoch \m -> when (m.active == active && m.pollGeneration == generation && not m.env.hidden) do
        now <- liftEffect B.now
        H.modify_ _ {now=now}
        tickProgress
      fallback = inEpoch epoch \current -> when (current.active == active && current.pollGeneration == generation) $ dispatch Poll
    U.SaveLocal -> persistLocal
    U.StorePreferences value payload -> attempt (api runtime requested "/api/preferences" "PUT" payload) $
      runUpdate runtime dispatch <<< U.Persisted requested.epoch value <<< either (Left <<< message) Right
    U.RetryPreferences -> later 2000.0 $ dispatch Persist
    U.RestoreScroll -> restoreScroll
    U.Focus target -> liftEffect $ B.focus target
    U.SelectRange target start end -> liftEffect $ B.selectRange target start end
    U.Sync -> sync runtime
    U.PersistTheme theme -> void $ liftEffect $ B.storageWrite "sapi-color-theme" theme
    U.FetchDocument request -> attempt (api runtime requested request.path "GET" D.null) $
      runUpdate runtime dispatch <<< U.ReceiveDocument request <<< either (Left <<< message) X.decodeDocument
    U.SendMessage request ->
      let payload = D.obj $ [Tuple "text" (D.text request.text),Tuple "attachments" (D.array $ map D.text request.attachments)]
      in attempt (api runtime requested request.path "POST" payload) $
        runUpdate runtime dispatch <<< U.MessageReceived request <<< either (Left <<< message) (const $ Right unit)
    U.Notify caption -> notify caption
    U.PollNow -> dispatch Poll
    U.ResizeComposer -> resizeComposer
    U.UpdateMention -> dispatch MentionCursor
    U.SaveView -> save dispatch *> sync runtime
    U.SavePreferences -> save dispatch
    U.LoadDocument -> requestDocument runtime dispatch
    U.ScrollToStart -> liftEffect $ B.scroll "#conversation-body" 0.0
    U.ScrollToBottom -> scrollBottom
    U.FocusSearch -> liftEffect $ B.focus "#agent-search"
    U.OpenWorkspace -> dispatch $ Browser $ Browser.Open (Browser.WebAddress "about:blank") Nothing
    U.LoadPreview -> preview runtime dispatch
    U.FetchPreview request ->
      attempt (api runtime requested request.path "GET" D.null) $ runUpdate runtime dispatch <<< U.ReceivePreview request <<<
        either (Left <<< M.PreviewUnavailable <<< message) (M.decodePreview $ M.isMemoTab (M.notes requested requested.selected) (M.activeTab requested))

captureScroll :: UI Unit
captureScroll = do
  m <- H.get
  when (m.activity == N.Chat N.Conversation && not (m.env.hidden) && not m.restoringScroll) do
    viewport <- liftEffect $ Platform.measure "#conversation-body"
    when (viewport.clientHeight > 0.0) do
      children <- liftEffect $ Platform.measureChildren "#conversation-body"
      let first = A.find (\node -> Browser.chatKey node /= "" && node.bottom > viewport.top) children
          position = {selected:m.selected,bottom:viewport.scrollHeight - viewport.scrollTop - viewport.clientHeight < 2.0,key:maybe "" Browser.chatKey first,offset:maybe 0.0 _.top first - viewport.top,width:viewport.clientWidth,height:viewport.clientHeight}
          sameSize = maybe true (\saved -> saved.width == viewport.clientWidth && saved.height == viewport.clientHeight) m.scrollPosition
      when sameSize $ H.modify_ _ {scrollPosition=Just position}

restoreScroll :: UI Unit
restoreScroll = do
  m <- H.get
  when (m.activity == N.Chat N.Conversation && not m.env.hidden) $ traverse_ (restore m.selected) m.scrollPosition
  where
  restore selected saved = when (saved.selected == selected) do
    viewport <- liftEffect $ Platform.measure "#conversation-body"
    children <- liftEffect $ Platform.measureChildren "#conversation-body"
    let node = A.find ((saved.key == _) <<< Browser.chatKey) children
    when (viewport.clientHeight > 0.0 && (saved.bottom || node /= Nothing)) do
      let target = if saved.bottom then viewport.scrollHeight - viewport.clientHeight else viewport.scrollTop + maybe 0.0 _.top node - viewport.top - saved.offset
      liftEffect $ B.scroll "#conversation-body" target
      H.modify_ _ {restoringScroll=false,scrollPosition=Just $ saved {width=viewport.clientWidth,height=viewport.clientHeight}}

scrollBottom :: UI Unit
scrollBottom = do
  m <- H.get
  H.modify_ _ {scrollPosition=Just {selected:m.selected,bottom:true,key:"",offset:0.0,width:0.0,height:0.0},restoringScroll=true}
  restoreScroll

sync :: C.Runtime -> UI Unit
sync runtime = do
  m <- H.get
  liftEffect do
    B.setAttribute "html" "data-theme" m.theme
    B.setStyle "html" "color-scheme" m.theme
    B.setAttribute "link[rel=icon]" "href" $ if m.theme == "dark" then "/favicon-dark.svg" else "/favicon.svg"
    B.setAttribute "body" "data-pane-limit" $ show $ N.limit m.env
    if N.limit m.env < 3 then B.setAttribute "body" "data-mobile-ui" "" else B.removeAttribute "body" "data-mobile-ui"
    when (m.env.viewportScale == 1.0) $ B.setStyle "html" "--mobile-viewport-height" (show (m.env.viewportHeight) <> "px")
    B.showDialog (if maybe false (M.corporaDialog <<< _.kind) m.dialog then "#corpora-dialog" else "#modal") $ m.dialog /= Nothing
    W.sync runtime.idleTabs m
  resizeComposer

resizeComposer :: UI Unit
resizeComposer = do
  m <- H.get
  if N.limit m.env == 3 then liftEffect $ B.setStyle "#message-input" "height" "" else do
    liftEffect $ B.setStyle "#message-input" "height" "44px"
    box <- liftEffect $ Platform.measure "#message-input"
    when (box.height > 0.0) $ liftEffect $ B.setStyle "#message-input" "height" (show (min 140.0 $ max 44.0 $ box.scrollHeight) <> "px")
