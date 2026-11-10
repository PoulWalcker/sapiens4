module Sapiens.Updates (Updates, Freshness(..), events, create, stop, connect, generation, freshness, applied) where

import Prelude
import Data.Argonaut.Core (toNumber)
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..), maybe)
import Effect (Effect)
import Effect.Aff (Milliseconds(..), delay, launchAff_)
import Effect.Class (liftEffect)
import Effect.Exception as Exception
import Effect.Ref as Ref
import Halogen.Subscription as HS
import Libs4.Browser as B
import Sapiens.Data as D

data Freshness = Applied Number | Pending Number
derive instance eqFreshness :: Eq Freshness

revision :: Freshness -> Number
revision (Applied r) = r
revision (Pending r) = r

-- A snapshot covers a notification at the same revision.
advance :: Freshness -> Freshness -> Freshness
advance old next = case next of
  Pending r | r > revision old -> next
  Applied r | r >= revision old -> next
  _ -> old

data Connection
  = Stopped
  | Reconnecting
  | Connected B.Socket (Maybe Freshness)  -- Nothing until the first revision

type Feed = { generation :: Int, connection :: Connection }
newtype Updates = Updates { feed :: Ref.Ref Feed, wake :: HS.Listener Unit, events :: HS.Emitter Unit }

events :: Updates -> HS.Emitter Unit
events (Updates updates) = updates.events

initial :: Int -> Feed
initial g = {generation:g, connection:Stopped}

create :: Effect Updates
create = do
  feed <- Ref.new (initial 0)
  {listener,emitter} <- HS.create
  pure $ Updates {feed,wake:listener,events:emitter}

stop :: Updates -> Effect Unit
stop (Updates updates) = do
  feed <- Ref.read updates.feed
  Ref.write (initial (feed.generation + 1)) updates.feed
  case feed.connection of
    Connected socket _ -> B.socketClose socket
    _ -> pure unit

generation :: Updates -> Effect Int
generation (Updates updates) = _.generation <$> Ref.read updates.feed

freshness :: Updates -> Effect (Maybe Freshness)
freshness (Updates updates) = do
  feed <- Ref.read updates.feed
  pure case feed.connection of
    Connected _ state -> state
    _ -> Nothing

connect :: Updates -> String -> Effect Unit
connect (Updates updates) address = do
  feed <- Ref.read updates.feed
  case feed.connection of
    Stopped -> attempt feed 2000.0
    _ -> pure unit
  where
  attempt feed backoff = do
    let g = feed.generation + 1
        current f = do
          s <- Ref.read updates.feed
          when (s.generation == g) (f s)
        announce r = current \s -> case s.connection of
          Connected socket state -> do
            let next = maybe (Pending r) (\old -> advance old (Pending r)) state
            when (state /= Just next) do
              Ref.write (s {connection=Connected socket (Just next)}) updates.feed
              HS.notify updates.wake unit
          _ -> pure unit
        retry s ms = do
          Ref.write (s {connection=Reconnecting}) updates.feed
          launchAff_ do
            delay (Milliseconds ms)
            liftEffect $ current \latest -> attempt latest (min 30000.0 (ms * 2.0))
        closed = current \s -> case s.connection of
          Connected _ (Just _) -> retry s 2000.0 *> HS.notify updates.wake unit
          _ -> retry s backoff
    Ref.write {generation:g, connection:Reconnecting} updates.feed
    Exception.catchException (const closed) do
      socket <- B.openSocket address (pure unit) (traverse_ announce <<< toNumber <<< D.parse) (const closed)
      Ref.modify_ (_ {connection=Connected socket Nothing}) updates.feed

applied :: Updates -> Int -> Number -> Effect Unit
applied (Updates updates) source r = Ref.modify_ acknowledge updates.feed
  where
  acknowledge feed | feed.generation == source, Connected socket (Just old) <- feed.connection =
    feed {connection=Connected socket (Just (advance old (Applied r)))}
  acknowledge feed = feed
