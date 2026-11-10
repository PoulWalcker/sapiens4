module Sapiens.IdleTabs (Tab, Timers, create, observe, stop) where

import Prelude
import Data.Foldable (traverse_)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Fiber, Milliseconds(..), delay, killFiber, launchAff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Exception (error)
import Effect.Ref as Ref
import Libs4.Browser as B
import Sapiens.Data as D

type Tab = {owner :: String, id :: String, epoch :: Int}
type Source = {path :: String, seq :: String}
type Pending = {source :: Source, timer :: Fiber Unit}
type State = {visible :: Maybe Tab, pending :: Map.Map Tab Pending}
newtype Timers = Timers (Ref.Ref State)

create :: Effect Timers
create = Timers <$> Ref.new {visible:Nothing, pending:Map.empty}

-- Only a previously visible local file can acquire a timer. The native engine
-- checks its actual loaded document again before releasing the view.
observe :: Timers -> Maybe Tab -> (Tab -> Maybe Source) -> Effect Unit
observe (Timers ref) visible sourceFor = do
  old <- Ref.read ref
  let keep tab pending
        | Just tab == visible = Nothing
        | otherwise = (\source -> pending {source=source}) <$> sourceFor tab
      kept = Map.mapMaybeWithKey keep old.pending
  Ref.write {visible, pending:kept} ref
  traverse_ (launchAff_ <<< killFiber (error "Tab is no longer idle") <<< _.timer) (Map.difference old.pending kept)
  case old.visible of
    Just tab | Just tab /= visible, Just source <- sourceFor tab -> do
      timer <- arm ref tab
      Ref.modify_ (\s -> s {pending=Map.insert tab {source,timer} s.pending}) ref
    _ -> pure unit

arm :: Ref.Ref State -> Tab -> Effect (Fiber Unit)
arm ref tab = launchAff do
  delay (Milliseconds (60.0 * 60.0 * 1000.0))
  liftEffect do
    state <- Ref.read ref
    case Map.lookup tab state.pending of
      Nothing -> pure unit
      Just pending -> do
        Ref.write (state {pending=Map.delete tab state.pending}) ref
        B.nativeMessage $ D.json {unloadIdle:{owner:tab.owner, id:tab.id, path:pending.source.path, seq:pending.source.seq}}

stop :: Timers -> Effect Unit
stop timers = observe timers Nothing (const Nothing)
