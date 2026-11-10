module Sapiens.Connections where

import Prelude
import Control.Promise (toAffE)
import Data.Argonaut.Core (Json)
import Data.Array as A
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..))
import Data.String as S
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff, Milliseconds(..), delay, throwError, try)
import Effect.Class (liftEffect)
import Effect.Exception (error)
import Effect.Exception as Exception
import Effect.Ref as Ref
import Foreign.Object as O
import Libs4.Browser as B
import Sapiens.Data as D
import Sapiens.Domain as X
import Sapiens.Remote as R
import Sapiens.Updates as Updates
import Sapiens.IdleTabs as IdleTabs

data Pairing = NoPairing | LocalPairing String | RemotePairing R.Transport

type Runtime = { idleTabs :: IdleTabs.Timers, updates :: Updates.Updates, clients :: Ref.Ref (O.Object R.Transport), credentials :: Ref.Ref (O.Object Json), camera :: Ref.Ref (Maybe B.Camera), pairing :: Ref.Ref Pairing, persistedSnapshot :: Ref.Ref (Maybe {key :: String, content :: X.SnapshotContent}) }
type Registry = { records :: Array X.Corpora, localAvailable :: Boolean }

create :: Effect Runtime
create = ado
  idleTabs <- IdleTabs.create
  updates <- Updates.create
  clients <- Ref.new O.empty
  credentials <- Ref.new O.empty
  camera <- Ref.new Nothing
  pairing <- Ref.new NoPairing
  persistedSnapshot <- Ref.new Nothing
  in {idleTabs,updates,clients,credentials,camera,pairing,persistedSnapshot}

read :: String -> Effect Json
read key = D.parse <$> B.storageRead key

write :: String -> Json -> Effect Boolean
write key value = do
  ok <- B.storageWrite key $ D.encode value
  if ok || S.take 12 key == "s4.snapshot." then pure ok else do
    keys <- B.storageKeys
    traverse_ B.storageRemove $ A.filter (\k -> S.take 12 k == "s4.snapshot.") keys
    B.storageWrite key $ D.encode value

json :: String -> String -> Json -> Aff Json
json path method body = do
  response <- toAffE $ B.request path method (if method == "GET" || method == "HEAD" then "" else D.encode body) $ D.json {"Content-Type": "application/json", "X-Sapiens-Local": "1"}
  let value = D.parse $ D.s "text" response
  when (D.n "status" response < 200.0 || D.n "status" response >= 300.0) $ throwError $ error $ D.fallback "Request failed" (D.s "error" value)
  unless (D.valid value) $ throwError $ error "Invalid host response"
  pure value

client :: Runtime -> X.Corpora -> Aff R.Transport
client runtime record = do
  existing <- liftEffect $ Ref.read runtime.clients
  case O.lookup (record.key) existing of
    Just transport -> pure transport
    Nothing -> do
      credentials <- liftEffect $ Ref.read runtime.credentials
      case O.lookup (record.key) credentials of
        Nothing -> throwError $ error "Connecting…"
        Just session -> do
          transport <- R.create session
          liftEffect $ Ref.modify_ (O.insert (record.key) transport) runtime.clients
          pure transport

request :: Runtime -> Boolean -> X.Corpora -> String -> String -> Json -> Aff Json
request runtime remote record path method body = do
  unless (record.id /= "") $ throwError $ error "Connect a corpora first."
  if remote then client runtime record >>= \transport -> R.api transport method path body
  else json ("/api/host/corpora/" <> B.encodeURI (record.id) <> path) method body

registry :: Runtime -> Boolean -> String -> Aff Registry
registry runtime remote origin = if remote then do
  saved <- A.filter ((_ == origin) <<< D.s "relay") <$> R.sessions
  liftEffect $ Ref.write (O.fromFoldable $ map (\s -> Tuple (R.sessionKey s) s) saved) runtime.credentials
  pure {records:map (\s -> (X.corpora $ D.get "corpora" s) {key=R.sessionKey s,location=X.Remote}) saved,localAvailable:false}
  else do
    data_ <- json "/api/host" "GET" D.null
    pure {records:map (\r -> (X.corpora r) {key=D.s "id" r}) (D.rows "corpora" data_),localAvailable:D.b "local_available" data_}

authenticate :: Runtime -> Boolean -> X.Corpora -> Aff Unit
authenticate runtime remote record = when remote $ client runtime record >>= \transport -> void $ R.authenticate transport false

decorate :: String -> Json -> Effect Json
decorate key snapshot = do
  local <- read $ "s4.workspace." <> key
  let prefs = D.get "preferences" snapshot
      merged = D.put "workspaces" (if D.valid $ D.get "workspaces" prefs then D.get "workspaces" prefs else D.get "workspaces" local) $ D.merge prefs local
  pure $ if D.valid local then D.put "preferences" merged snapshot else snapshot

-- Accepted content and successful persistence have independent lifetimes.
cacheSnapshot :: Runtime -> String -> X.SnapshotContent -> Json -> Effect Unit
cacheSnapshot runtime key content snapshot = Exception.catchException (const $ Ref.write Nothing runtime.persistedSnapshot) do
  previous <- Ref.read runtime.persistedSnapshot
  keys <- B.storageKeys
  unless (previous == Just {key,content} && A.elem ("s4.snapshot." <> key) keys) do
    let recent = D.put "turns" (D.array $ A.takeEnd 100 $ D.rows "turns" snapshot) snapshot
        reduced = if S.length (D.encode recent) > 750000 then D.put "turns" (D.array $ map (\t -> D.put "input" (D.text $ D.cut 12000 $ D.s "input" t) $ D.put "output" (D.text $ D.cut 12000 $ D.s "output" t) t) $ D.rows "turns" recent) recent else recent
    traverse_ B.storageRemove $ A.dropEnd 2 $ A.filter (\k -> S.take 12 k == "s4.snapshot." && k /= "s4.snapshot." <> key) keys
    ok <- write ("s4.snapshot." <> key) reduced
    Ref.write (if ok then Just {key,content} else Nothing) runtime.persistedSnapshot

lifecycle :: Runtime -> Boolean -> X.Corpora -> String -> Aff Json
lifecycle runtime remote record action = do
  info <- request runtime remote record "/api/corpora" "GET" D.null
  operation <- liftEffect B.uuid
  now <- liftEffect B.now
  let intent = D.json {action, revision: D.get "revision" info, operation}
  void $ liftEffect $ write "s4.lifecycle.pending" $ D.merge intent $ D.json {key: D.text record.key, expiresAt: now + 30000.0}
  result <- request runtime remote record "/api/corpora/lifecycle" "PUT" intent >>= wait 10
  void $ liftEffect $ write "s4.lifecycle.pending" D.null
  pure result
  where
  wait remaining info
    | not (D.b "stopping" info) = pure info
    | remaining <= 0 = throwError $ error "Work is still stopping. Try again shortly."
    | otherwise = delay (Milliseconds 500.0) *> (request runtime remote record "/api/corpora" "GET" D.null >>= wait (remaining - 1))

reconcile :: Runtime -> Boolean -> Array X.Corpora -> String -> Aff Unit
reconcile runtime remote records active = do
  pending <- liftEffect $ read "s4.lifecycle.pending"
  when (D.valid pending) do
    record <- findRecord (D.s "key" pending)
    outcome <- request runtime remote record ("/api/corpora/operations/" <> B.encodeURI (D.s "operation" pending)) "GET" D.null
    let info = D.get "corpora" outcome
    now <- liftEffect B.now
    when (D.b "stopping" info) $ throwError $ error "Work is still stopping. Try again shortly."
    when (not (D.b "applied" outcome) && now < D.n "expiresAt" pending) $ throwError $ error "The previous switch is still being confirmed. Try again shortly."
    void $ liftEffect $ write "s4.lifecycle.pending" D.null
    when (D.b "applied" outcome && D.s "action" pending == "activate" && record.key /= active && D.s "status" info == "active") $ void $ lifecycle runtime remote record "pause"
  switching <- liftEffect $ read "s4.switch.pending"
  when (D.s "phase" switching == "activating" && D.s "to" switching /= active) do
    record <- findRecord (D.s "to" switching)
    info <- request runtime remote record "/api/corpora" "GET" D.null
    when (D.s "status" info == "active") $ void $ lifecycle runtime remote record "pause"
  void $ liftEffect $ write "s4.switch.pending" D.null
  where
  findRecord key = case A.find ((_ == key) <<< _.key) records of
    Just r -> pure r
    Nothing -> throwError $ error "The previous switch needs its original connection."

cancelPairing :: Runtime -> Aff Unit
cancelPairing runtime = do
  liftEffect do
    Ref.read runtime.camera >>= traverse_ B.cameraStop
    Ref.write Nothing runtime.camera
  pairing <- liftEffect $ Ref.read runtime.pairing
  liftEffect $ Ref.write NoPairing runtime.pairing
  case pairing of
    NoPairing -> pure unit
    RemotePairing transport -> liftEffect $ R.close transport
    LocalPairing id -> void $ try $ json ("/api/host/pair/" <> B.encodeURI id) "POST" D.empty

register :: Runtime -> R.Transport -> Aff X.Corpora
register runtime transport = do
  session <- liftEffect $ Ref.read transport.session
  let key = R.sessionKey session
  liftEffect do
    existing <- Ref.read runtime.clients
    traverse_ R.close $ O.lookup key existing
    Ref.modify_ (O.insert key transport) runtime.clients
    Ref.modify_ (O.insert key session) runtime.credentials
    Ref.write NoPairing runtime.pairing
  pure $ (X.corpora $ D.get "corpora" session) {key=key,location=X.Remote}

dispose :: Runtime -> Effect Unit
dispose runtime = do
  IdleTabs.stop runtime.idleTabs
  Updates.stop runtime.updates
  Ref.write Nothing runtime.persistedSnapshot
  Ref.read runtime.clients >>= traverse_ R.close <<< O.values
  Ref.read runtime.camera >>= traverse_ B.cameraStop
  Ref.read runtime.pairing >>= case _ of
    RemotePairing transport -> R.close transport
    _ -> pure unit
