module Sapiens.App where

import Sapiens.Form as F
import Prelude
import Sapiens.Environment (Environment)
import Sapiens.Platform as Platform
import Control.Alt ((<|>))
import Control.Parallel (parallel, sequential)
import Control.Promise (toAffE)
import Data.Array as A
import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..))
import Data.Foldable (traverse_)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff, Milliseconds(..), delay, try)
import Effect.Class (liftEffect)
import Effect.Exception (message)
import Halogen as H
import Halogen.Subscription as HS
import Libs4.Browser as B
import Sapiens.Action (Action(..), Success(..))
import Sapiens.Connections as C
import Sapiens.CorporaEffects as Corpora
import Sapiens.Data as D
import Sapiens.Browser as Browser
import Sapiens.DialogEffects as Dialog
import Sapiens.Effects as E
import Sapiens.Model as M
import Sapiens.Navigation as N
import Sapiens.View as View
import Sapiens.Work as Work
import Sapiens.Update as U
import Sapiens.Workspace as Workspace
import Web.Event.Event as Event

component :: forall q. C.Runtime -> H.Component q Environment Void Aff
component runtime = H.mkComponent
  { initialState:M.initial
  , render:View.render
  , eval:H.mkEval $ H.defaultEval {handleAction=handle runtime,initialize=Just Initialize,finalize=Just Finalize}
  }

handle :: C.Runtime -> E.Dispatch
handle runtime input = session input
  where
  dispatch = handle runtime
  session action = case action of
    Initialize -> do
      H.modify_ _ {feedback="Connecting…"}
      traverse_ (\(Tuple target event) -> void $ H.subscribe $ (Global <<< Browser.globalEvent event) <$> HS.makeEmitter (B.subscribe target event))
        [Tuple "window" "error",Tuple "window" "unhandledrejection",Tuple "window" "resize",Tuple "viewport" "resize",Tuple "window" "online",Tuple "window" "storage",Tuple "window" "pagehide",Tuple "window" "pageshow",Tuple "window" "popstate",Tuple "window" "beforeinstallprompt",Tuple "document" "visibilitychange",Tuple "document" "click"]
      void $ H.subscribe $ GlobalEvent "keydown" <$> HS.makeEmitter (B.subscribeEvent "document" "keydown")
      void $ H.subscribe $ HS.makeEmitter \emit -> B.subscribeMedia "(prefers-color-scheme: dark)" $ emit RefreshTheme
      void $ H.subscribe $ (NativeReport <<< Browser.nativeReport) <$> HS.makeEmitter (B.callback "sapiensBrowserEvent")
      now <- liftEffect B.now
      savedTheme <- liftEffect $ B.storageRead "sapi-color-theme"
      H.modify_ \m -> m {now=now,theme=if A.elem savedTheme ["dark","light"] then savedTheme else m.theme}
      m <- H.get
      let ios = N.ios m.env
          mobile = ios || D.matches "Android" (m.env.userAgent)
          required = M.remote m && mobile && not (m.env.installed)
      H.modify_ _ { installRequired=required }
      when (M.remote m) do
        liftEffect $ B.historyReplace $ m.env.path
        void $ H.fork $ void $ H.liftAff $ try $ toAffE $ B.registerWorker "/sw.js"
      unless required $ dispatch LoadRegistry
      E.sync
    CheckSetup -> do
      m <- H.get
      unless (M.remote m || m.setupChecked) do
        H.modify_ _ {setupChecked=true}
        E.attempt (C.json "/api/host/onboarding" "GET" D.null) \result -> case result of
          Right value -> unless (D.b "ready" value) $ dispatch $ OpenDialog M.Onboarding F.NoRecord
          Left _ -> pure unit
    Finalize -> liftEffect $ C.dispose runtime
    Poll -> E.runUpdate runtime dispatch U.Poll
    Copy text caption -> do
      result <- H.liftAff $ try $ toAffE $ B.clipboard text
      E.notify $ case result of
        Right _ -> caption
        Left _ -> "Could not copy. You can select the task body to copy it."
    Resize -> do
      updateEnvironment
      E.sync
    GlobalEvent name event -> do
      data_ <- liftEffect $ B.eventData event
      m <- H.get
      when (U.shortcut (Browser.key data_) m) $ liftEffect $ Event.preventDefault event
      E.runUpdate runtime dispatch $ U.Global $ Browser.globalEvent name data_
    Global event -> E.runUpdate runtime dispatch $ U.Global event
    RefreshTheme -> do
      saved <- liftEffect $ B.storageRead "sapi-color-theme"
      env <- liftEffect Platform.environment
      H.modify_ _ {theme=if A.elem saved ["dark","light"] then saved else if env.dark then "dark" else "light"}
      E.sync
    Theme -> H.get >>= \m -> E.runUpdate runtime dispatch $ U.SetTheme $ if m.theme == "dark" then "light" else "dark"
    Persist -> E.runUpdate runtime dispatch U.Persist
    Mutate path method body success -> do
      m <- H.get
      H.modify_ \s -> s {dialog=map ((M.setDialogBusy true)) s.dialog}
      E.attempt (E.api runtime m path method body) \result -> E.inEpoch m.epoch \current ->
        when ((_.identity <$> current.dialog) == (_.identity <$> m.dialog)) do
          H.modify_ \s -> s {dialog=map ((M.setDialogBusy false)) s.dialog}
          case result of
            Left failure -> E.notify $ message failure
            Right value -> case success of
              CloseAndRefresh caption -> dispatch CloseDialog *> E.notify caption *> dispatch Poll
              SavedPrompt content -> do
                H.modify_ \s -> s {dialog=map (\d -> d {fields=d.fields {path=D.s "path" value},content=F.savedPrompt d.fields.node (D.s "path" value) content d.content}) s.dialog}
                E.notify "Prompt saved for subsequent decisions."
              _ -> dispatch Poll
    Submit event next -> liftEffect (Event.preventDefault event) *> dispatch next
    Link event next -> do
      m <- H.get
      let prevent = case next of
            OpenLink _ -> Workspace.native m
            _ -> true
      when prevent $ liftEffect $ Event.preventDefault event
      liftEffect $ Event.stopPropagation event
      dispatch next
    NoteAnchor id -> liftEffect $ B.scrollIntoView $ "#" <> id
    _ -> conversation action
  conversation action = case action of
    Draft value -> E.runUpdate runtime dispatch $ U.Draft value
    MentionCursor -> do
      box <- liftEffect $ Platform.measure "#message-input"
      E.runUpdate runtime dispatch $ U.MentionAt (box.start) (box.end)
    MentionChoice index -> E.runUpdate runtime dispatch $ U.ChooseMention index
    MessageKey event -> do
      data_ <- Browser.key <$> liftEffect (B.eventData event)
      plan <- U.messageKey data_ <$> H.get
      when plan.prevent $ liftEffect $ Event.preventDefault event
      when plan.stop $ liftEffect $ Event.stopPropagation event
      traverse_ (E.runUpdate runtime dispatch) plan.actions
    Send -> E.runUpdate runtime dispatch U.Send
    ToggleExpanded key -> E.runUpdate runtime dispatch $ U.ToggleExpanded key
    Clarify work -> dispatch (Select work.owner) *> E.runUpdate runtime dispatch (U.SetClarification work)
    RemoveAttachment id -> E.runUpdate runtime dispatch $ U.RemoveAttachment id
    PrepareDraft preset placeholder -> E.captureScroll *> E.runUpdate runtime dispatch (U.PrepareDraft preset placeholder)
    Feedback -> do
      m <- H.get
      dispatch $ Navigate $ N.Chat N.Conversation
      dispatch $ Draft $ M.draft m <> (if M.draft m == "" then "" else "\n\n") <> "Feedback: "
      focusComposer
    _ -> workspace action
  workspace action = case action of
    Select id -> do
      E.captureScroll
      E.persistLocal
      E.runUpdate runtime dispatch $ U.Select id
    SetScope scope -> do
      m <- H.get
      dispatch $ Select $ m.snapshot.main_agent_id
      E.runUpdate runtime dispatch $ U.SetScope scope
    Navigate activity -> E.captureScroll *> E.runUpdate runtime dispatch (U.Navigate activity)
    DefaultWork -> H.get >>= dispatch <<< Navigate <<< Work.defaultActivity
    SetPeriod period -> E.runUpdate runtime dispatch $ U.SetPeriod period
    Search search -> E.runUpdate runtime dispatch $ U.Search search
    ToggleSearch -> E.runUpdate runtime dispatch U.ToggleSearch
    TogglePane pane -> E.runUpdate runtime dispatch $ U.TogglePane pane
    Home -> do
      m <- H.get
      if M.remote m && m.env.installed && (N.ios m.env) then do
        E.persistLocal
        void $ H.liftAff $ try $ sequential $ parallel (toAffE B.updateWorker) <|> parallel (delay $ Milliseconds 12000.0)
        E.persistLocal
        liftEffect B.reload
      else do
        H.modify_ _ {panes=[N.ChatPane,N.Sidebar,N.Workspace]}
        dispatch $ Select $ m.snapshot.main_agent_id
    RetryDocument key -> E.runUpdate runtime dispatch $ U.RetryDocument key
    Menu key -> E.runUpdate runtime dispatch $ U.SetMenu key
    CloseMenu -> E.runUpdate runtime dispatch U.CloseMenu
    Scroll -> E.captureScroll *> E.persistLocal
    UserScroll -> H.modify_ _ {scrollPosition=Nothing,restoringScroll=false}
    NativeReport report -> E.runUpdate runtime dispatch $ U.QueueNative report
    DrainNative -> E.runUpdate runtime dispatch U.DrainNative
    Browser command -> do
      m <- H.get
      let {op,fields} = Browser.request command
      E.attempt (do
        void $ E.api runtime m (M.ownerPath m <> "/control") "POST" $ D.put "op" (D.text $ "workspace_" <> op) fields
        E.api runtime m "/api/state" "GET" D.null) $ browserReceived m.epoch m.selected op
    Address text -> E.runUpdate runtime dispatch $ U.Address text
    OpenAddress -> do
      m <- H.get
      traverse_ (dispatch <<< Browser) $ Browser.addressCommand m.address $ if M.hasTab m then Just (M.activeTab m).id else Nothing
    EditAddress -> E.runUpdate runtime dispatch U.EditAddress
    OpenFile -> do
      m <- H.get
      let directory = if M.isGroup $ M.selected m then (M.selected m).workspace else D.replace "[\\\\/][^\\\\/]+$" "" $ (M.notes m).path
      dispatch CloseMenu
      if Workspace.native m then liftEffect $ B.nativeMessage $ D.json {pickFile: true, owner: m.selected, directory} else do
        H.modify_ _ {address=directory <> "/",addressOpen=true}
        liftEffect $ B.focus "#browser-address"
    OpenLink url -> do
      m <- H.get
      when (Workspace.native m) $ dispatch $ Browser $ Browser.Open (Browser.WebAddress url) Nothing
    _ -> do
      handled <- Corpora.handle runtime dispatch action
      unless handled $ Dialog.handle runtime dispatch action
  browserReceived epoch owner op = E.runUpdate runtime dispatch <<< U.BrowserReceived epoch owner op <<< either (Left <<< message) Right
  focusComposer = liftEffect $ B.focus "#message-input"
  updateEnvironment = liftEffect Platform.environment >>= \env -> H.modify_ _ {env=env}
