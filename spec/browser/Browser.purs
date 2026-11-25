module Spec.Browser where

import Prelude
import Control.Promise (fromAff, toAffE)
import Data.Argonaut.Core (Json)
import Data.Array as A
import Data.Either (either, isLeft, Either(..))
import Data.Foldable (traverse_)
import Data.Int as I
import Data.Int.Bits (xor)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Nullable (toMaybe)
import Data.String as S
import Data.Traversable (traverse)
import Effect (Effect, forE)
import Effect.Aff (Aff, attempt, finally, launchAff_)
import Effect.Class (liftEffect)
import Effect.Exception as E
import Effect.Ref as Ref
import Effect.Uncurried (EffectFn1, EffectFn2, mkEffectFn1, mkEffectFn2, runEffectFn1, runEffectFn2)
import Sapiens.BrowserAudio as Audio
import Sapiens.BrowserCrypto as Crypto
import Sapiens.BrowserNative as N
import Sapiens.ClientPreferences as Preferences
import Sapiens.Data as D
import Sapiens.Main as Main
import Sapiens.Setup as Setup
import Unsafe.Coerce (unsafeCoerce)
import Unsafe.Reference (unsafeRefEq)
import Web.Event.CustomEvent as CustomEvent
import Web.Event.Event as Event
import Web.Event.EventTarget as Target
import Web.HTML (window)
import Web.HTML.Window as Window

-- Cast only at browser FFI and mock boundaries.
get :: forall a b. a -> String -> Effect b
get object key = unsafeCoerce <$> N.property (unsafeCoerce object) key

set :: forall a b. a -> String -> b -> Effect Unit
set object key value = N.setProperty (unsafeCoerce object) key (unsafeCoerce value)

property :: String -> String -> Effect Json
property selector key = do
  nodes <- N.queryAll selector
  case nodes of
    [node] -> N.property node key
    _ -> E.throw $ "Missing " <> selector

emit :: String -> Json -> Effect Unit
emit name detail = do
  event <- CustomEvent.new' (Event.EventType name) (Just detail)
  target <- Window.toEventTarget <$> window
  void $ Target.dispatchEvent (CustomEvent.toEvent event) target

callback :: String -> Json -> Effect Unit
callback name value = do
  w <- window
  fn <- get w name
  runEffectFn1 (fn :: EffectFn1 Json Unit) value

mock :: forall a b c. a -> String -> b -> Aff c -> Aff c
mock object key value action = do
  old <- liftEffect $ get object key :: Aff Json
  liftEffect $ set object key value
  finally (liftEffect $ set object key old) action

check :: forall a. Eq a => String -> a -> a -> Effect Unit
check label actual expected = unless (actual == expected) $ E.throw label

fields :: String -> Json -> Json -> Effect Unit
fields label actual expected = check label (D.merge actual expected) actual

preferences :: Ref.Ref (Array Json) -> Effect Unit
preferences messages = do
  Preferences.main
  origin <- D.s "origin" <$> N.environment
  N.storageKeys >>= traverse_ N.storageRemove
  traverse_ (\{key, value} -> void $ N.storageWrite key value)
    [{key:"s4.native.storage",value:"old-root"},{key:"s4.workspace.old",value:"private"},{key:"s4.snapshot.old",value:"private"},{key:"unrelated",value:"keep"}]
  let install address saved token = callback "sapiensPreferences" $ D.array [D.text address,D.text "new-root",saved,D.text token]
      read key = do
        keys <- N.storageKeys
        if A.elem key keys then D.text <$> N.storageRead key else pure D.null
  install origin (D.json {"sapi-color-theme":"dark","s4.active":"owner",forbidden:"drop"}) "restore-once"
  restored <- traverse read ["sapi-color-theme","s4.active","s4.workspace.old","s4.snapshot.old","unrelated","forbidden"]
  check "Restore root" restored [D.text "dark",D.text "owner",D.null,D.null,D.text "keep",D.null]
  void $ N.storageWrite "sapi-color-theme" "light"
  install origin (D.json {"sapi-color-theme":"dark"}) "restore-once"
  restoredOnce <- (_ == "light") <$> N.storageRead "sapi-color-theme"
  before <- Ref.read messages
  install "https://untrusted.invalid" (D.json {"sapi-color-theme":"dark"}) "foreign"
  after <- Ref.read messages
  root <- N.storageRead "s4.native.storage"
  void $ N.storageWrite "forbidden" "still-local"
  notified <- Ref.read messages
  check "Restore token and origin" (restoredOnce && before == after && root == "new-root") true
  check "Publish allowed preferences" (A.all (Preferences.allowed <<< D.s "key" <<< D.get "clientPreferences") notified) true
  check "Theme reporting" (A.any (\value -> D.get "clientPreferences" value == D.json {key:"sapi-color-theme",value:"light"}) notified) true
  void $ N.storageWrite "s4.workspace.stale" "old"
  install origin D.null "restore-snapshot"
  keptWithoutSnapshot <- (_ == "old") <$> N.storageRead "s4.workspace.stale"
  install origin (D.json {"sapi-color-theme":"dark","s4.workspace.saved":"draft"}) "restore-snapshot"
  keys <- N.storageKeys
  emit "sapiens-preferences-flush" $ D.text "flush-nonce"
  inert <- property "html" "inert"
  notifications <- Ref.read messages
  emit "sapiens-preferences-resume" D.null
  resumed <- not <<< D.flag <$> property "html" "inert"
  check "Snapshot and input suspension" (keptWithoutSnapshot && not (A.elem "s4.workspace.stale" keys || A.elem "s4.active" keys) && D.flag inert && resumed) true
  let snapshots = A.filter (_ /= D.null) $ map (D.get "preferencesFlush") notifications
  check "Flush snapshot" (A.last snapshots) $ Just $ D.json {nonce:"flush-nonce",values:{"sapi-color-theme":"dark","s4.workspace.saved":"draft"}}


workers :: Aff Unit
workers = do
  w <- liftEffect window
  navigator <- liftEffect $ Window.navigator w
  serviceWorker <- liftEffect $ get navigator "serviceWorker" :: Aff Json
  added <- liftEffect $ Ref.new 0
  removed <- liftEffect $ Ref.new 0
  let update state = do
        listener <- liftEffect $ Ref.new (pure unit)
        let worker = {state:"installing"
              ,addEventListener: mkEffectFn2 \(_ :: String) action -> Ref.modify_ (_ + 1) added *> Ref.write action listener
              ,removeEventListener: mkEffectFn2 \(_ :: String) (_ :: Effect Unit) -> Ref.modify_ (_ + 1) removed}
            registration = fromAff $ pure {installing:worker,update: fromAff do
              liftEffect $ void $ N.setTimer 0 $ set worker "state" state *> (join $ Ref.read listener)}
        mock serviceWorker "getRegistration" registration $ toAffE N.updateWorker
  update "activated"
  result <- attempt $ update "redundant"
  mock serviceWorker "getRegistration" (fromAff $ pure D.null) $ toAffE N.updateWorker
  a <- liftEffect $ Ref.read added
  r <- liftEffect $ Ref.read removed
  liftEffect $ check "Worker lifecycle" {failure:either E.message (const "") result,added:a,removed:r} {failure:"Update unavailable",added:2,removed:2}

sound :: Aff Unit
sound = do
  w <- liftEffect window
  navigator <- liftEffect $ Window.navigator w
  media <- liftEffect $ get navigator "mediaDevices" :: Aff Json
  stopped <- liftEffect $ Ref.new 0
  closed <- liftEffect $ Ref.new 0
  let idle = pure unit :: Effect Unit
      connection = {connect: idle,disconnect: idle}
      port = {onmessage:D.null}
      node = {port,connect:idle,disconnect:idle}
      context = {sampleRate:48000.0,audioWorklet:{addModule:fromAff $ pure unit},createMediaStreamSource:(pure connection :: Effect _),close:fromAff $ liftEffect $ Ref.modify_ (_ + 1) closed}
      microphone = fromAff $ pure {getTracks:(pure [{stop:Ref.modify_ (_ + 1) stopped}] :: Effect _)}
  mock media "getUserMedia" microphone $ mock w "AudioContext" (mkEffectFn1 \(_ :: Json) -> pure context) $
    mock w "AudioWorkletNode" (mkEffectFn2 \(_ :: Json) (_ :: String) -> pure node) do
      pieces <- liftEffect $ Ref.new []
      voice <- toAffE $ Audio.voiceStart "/voice-worklet.js" 1 (\piece -> Ref.modify_ (_ <> [piece]) pieces)
      liftEffect do
        input <- Audio.allocate 48000
        forE 0 48000 $ \i -> Audio.writeSample input i 0.5
        traverse_ (\i -> Audio.writeSample input i $ I.toNumber i) [0,1,2]
        send <- get port "onmessage"
        traverse_ (\{start,end} -> runEffectFn1 (send :: EffectFn1 {data :: Audio.Samples} Unit) {data:Audio.sliceSamples start end input})
          [{start:0,end:1},{start:1,end:2},{start:2,end:127},{start:127,end:8191},{start:8191,end:48000}]
      first <- liftEffect voice.stop
      second <- liftEffect voice.stop
      result <- toAffE voice.stop
      chunks <- liftEffect $ Ref.read pieces
      let piece i = fromMaybe D.null $ A.index chunks i
          decode value = case toMaybe (Crypto.decodeBytes value) of
            Nothing -> E.throw "Invalid wave"
            Just bytes -> pure bytes
          word bytes offset = do
            lo <- get bytes $ show offset
            hi <- get bytes $ show (offset + 1)
            pure $ (lo :: Int) + hi * 256
          sample bytes offset = do
            n <- word bytes offset
            pure $ if n >= 32768 then n - 65536 else n
      liftEffect do
        wave <- decode $ D.s "data" $ piece 0
        tail <- decode $ D.s "data" $ piece 1
        clipping <- Audio.allocate 5
        traverse_ (\i -> Audio.writeSample clipping i $ I.toNumber (i - 2)) $ A.range 0 4
        clip <- Audio.wav clipping >>= decode
        samples <- traverse (\i -> sample clip (44 + i * 2)) $ A.range 0 4
        header <- traverse (word wave) [24,40,44,46]
        ending <- traverse (word tail) [40,44]
        s <- Ref.read stopped
        c <- Ref.read closed
        check "Recording shutdown" (unsafeRefEq first second && s == 1 && c == 1) true
        fields "Recording duration" result $ D.json {durationMs:1000,segments:2}
        check "Segment count" (A.length chunks) 2
        fields "First segment" (piece 0) $ D.json {final:false,index:0,startMs:0}
        fields "Final segment" (piece 1) $ D.json {final:true,index:1,startMs:1000}
        check "Resampled WAV" header [16000,32000,32767,16383]
        check "Final silent sample" ending [2,0]
        check "WAV clipping" samples [-32767,-32767,0,32767,32767]


observe :: Ref.Ref (Array Json) -> Aff Json
observe messages = do
  malformed <- liftEffect $ E.try $ Crypto.perform "seal" $ D.json {peer:"",secret:""}
  fraction <- liftEffect $ E.try $ Crypto.perform "random" $ D.json {size:1.5}
  liftEffect do
    check "Malformed crypto" [Crypto.unbox (unsafeCoerce D.null) "" "" "",either E.message (const "") fraction,either E.message (const "") malformed] ["","Invalid random length","Invalid encoded value"]
    let markup = N.parseMarkup "<p id=\"one\">hello <strong>café</strong><!--ignore--></p>"
    case D.arr markup of
      [paragraph] -> do
        fields "Paragraph" paragraph $ D.json {tag:"p",attrs:{id:"one"},text:"hello café"}
        check "Child markup" (D.get "children" paragraph) $ D.array [D.json {text:"hello "},D.json {tag:"strong",attrs:D.empty,text:"café",children:[D.json {text:"café"}],source:"<strong>café</strong>"},D.empty]
      _ -> E.throw "Expected one paragraph"
  prompted <- liftEffect $ Ref.new 0
  stop <- liftEffect $ N.subscribe "window" "beforeinstallprompt" $ const $ pure unit
  event <- liftEffect $ CustomEvent.newWithOptions (Event.EventType "beforeinstallprompt") {detail:Nothing,bubbles:false,cancelable:true,composed:false}
  liftEffect do
    set event "prompt" $ fromAff $ liftEffect $ Ref.modify_ (_ + 1) prompted
    target <- Window.toEventTarget <$> window
    void $ Target.dispatchEvent (CustomEvent.toEvent event) target
  toAffE N.installPrompt
  toAffE N.installPrompt
  liftEffect stop
  count <- liftEffect $ Ref.read prompted
  prevented <- liftEffect $ Event.defaultPrevented $ CustomEvent.toEvent event
  liftEffect $ check "Consume install prompt once" (prevented && count == 1) true
  workers
  qr <- toAffE $ N.qrImage "sapiens-purescript"
  decodedQR <- toAffE $ N.decodeQR qr 1600
  liftEffect $ fields "QR roundtrip" decodedQR $ D.json {data:"sapiens-purescript",width:320,height:320}
  id <- liftEffect N.uuid
  origin <- liftEffect $ D.s "origin" <$> N.environment
  let cache = "sapiens-spec-" <> id
  finally (toAffE $ N.cacheDelete cache) do
    toAffE $ N.cacheAll cache ["/favicon.svg"]
    cached <- toAffE $ N.cacheResponse cache $ origin <> "/favicon.svg?migration=1"
    fallback <- toAffE $ N.cacheResponse cache $ origin <> "/manifest.webmanifest"
    cachedStatus <- liftEffect $ get cached "status" :: Aff Int
    fallbackStatus <- liftEffect $ get fallback "status" :: Aff Int
    w <- liftEffect window
    svg <- mock w "fetch" (fromAff $ pure cached) $ toAffE $ N.request origin "GET" "" D.empty
    liftEffect $ check "Cache and fallback" (cachedStatus == 200 && fallbackStatus == 200 && S.contains (S.Pattern "<svg") (D.s "text" svg)) true
  let access operation key value = toAffE $ N.database cache 1 ["records"] operation "records" key value
  void $ access "put" "entry" $ D.json {text:"café 🧬"}
  saved <- access "get" "entry" D.null
  all <- access "getAll" "" D.null
  void $ access "delete" "entry" D.null
  removed <- access "get" "entry" D.null
  invalid <- attempt $ access "invalid" "" D.null
  liftEffect do
    check "IndexedDB round trip" [saved,all,removed] [D.json {text:"café 🧬"},D.json [{text:"café 🧬"}],D.null]
    check "Invalid storage operation" (isLeft invalid) true
    preferences messages
  sound
  pure $ D.bool true

verify :: Effect Unit
verify = do
  let alice = "dwdtCnMYpX08FsFyUbJmRd9ML4frwJkqsXf7pR25LCo="
      bob = "XasIfmJKikt54X+Lg4AO5m87sSkmGLb9HC+LJ/+I4Os="
      nonce = "aWlu6VW2K3PNYr2odfxz1oIZ4ANregs3"
      plaintext = "vgdfxTyB8tXPFBMW6+sMe1IoxSpMYsvUS2aEm2QkT/zl7LqvM711GhrHKNRebGEpbNw8ASM1YfQdtmzOMUrbMQ476CUMRvBtzuo6f6E0gFfi9lVq1rExigJKg48hrx/eBIl360j1n/1JJMocYJAuUvCgibx2iXBA4IL5N3Y4SGReBwU="
      ciphertext = "8//HcD+UAOUqfftLPTMF2Y6ZO59IaBJzwpZQujL8ds5IMy6nFk2WpEdvuMUxoRhqwN/BfJjc6HtNp/AR7EjJcnHSwg+bko/iJw1vuGPVFzi0ju7jFKfMirkyFkVI5SaukCJDaFF6z+q9a7NzK8Dp2pmDK2HKAbbeViRKnojV+bN5c/YipD0UplmbH2VMtFp041Wl"
      a = Crypto.publicKey alice
      b = Crypto.publicKey bob
      open frame = Crypto.perform "open" $ D.json {secret:bob,peer:a,frame}
      rejects label action = do
        result <- E.try action
        check label (isLeft result) true
  check "Alice key vector" a "hSDwCYkwp1R0i33ctD73Wg2/Og0mOBr066SpjqqbTmo="
  check "Bob key vector" b "3p7bfXt9wbTTW2HC7OQ1Nz+DQ8hbeGdNrfx+FG+IK08="
  open (D.json {sender:a,nonce,ciphertext}) >>= flip (check "NaCl ciphertext vector") (D.text plaintext)
  check "NaCl empty message" (Crypto.box "" nonce b alice) "JTkSHY4jTmUtZR+kyM/4gA=="
  traverse_ (\message -> do
    let plain = N.base64Encode message
    frame <- Crypto.perform "seal" $ D.json {secret:alice,peer:b,plaintext:plain}
    open frame >>= flip (check "Host round trip") (D.text plain)
    let n = D.s "nonce" frame
        cipher = D.s "ciphertext" frame
    check "Client decryption" (Crypto.unbox cipher n a bob) message
    check "Matching encryption" (Crypto.box message n b alice) cipher
    bytes <- traverse (get (Crypto.decodeBytes cipher) <<< show) $ A.range 0 (N.byteLength cipher - 1)
    traverse_ (\position -> do
      let altered = N.byteKey $ D.json $ A.modifyAtIndices [position] (flip xor 1) (bytes :: Array Int)
      rejects "Tampered frame" $ open $ D.merge frame $ D.json {ciphertext:altered}) [0,15,A.length bytes - 1]
    random <- Crypto.randomBytes 24
    traverse_ (rejects "Invalid frame" <<< open <<< D.merge frame)
      [D.json {sender:b},D.json {nonce:random},D.json {ciphertext:N.byteKey $ D.json $ A.take 15 bytes}]
    wrong <- Crypto.randomBytes 32
    rejects "Wrong key" $ Crypto.perform "open" $ D.json {secret:wrong,peer:a,frame})
    ["","Remote control ✓ 👩🏽‍💻",S.joinWith "" $ A.replicate 16384 "x"]
  let rejectsBox label n peer = check label (toMaybe $ Crypto.attemptValue \_ -> Crypto.box "work" n peer alice) Nothing
  rejectsBox "Low-order peer" nonce $ N.byteKey $ D.json $ A.replicate 32 0
  short <- Crypto.randomBytes 23
  rejectsBox "Wrong nonce length" short b
  rejects "Malformed encoding" $ Crypto.perform "seal" $ D.json {secret:alice,peer:b,plaintext:"%%%"}
  rejects "Unknown operation" $ Crypto.perform "unknown" D.empty
  pair <- Crypto.keyPair
  check "Generated public key" (Crypto.publicKey $ D.s "secretKey" pair) (D.s "publicKey" pair)
  first <- Crypto.randomBytes 32
  second <- Crypto.randomBytes 32
  check "Unique tokens" (first /= second) true

cryptoRequest :: Json -> Effect Json
cryptoRequest input = case D.s "operation" input of
  "keypair" -> Crypto.keyPair
  "verify" -> verify *> pure (D.bool true)
  "request" -> do
    let config = D.get "config" input
    nonce <- Crypto.randomBytes 24
    id <- N.uuid
    now <- N.now
    let body = D.merge (D.get "body" input) $ D.json {v: "sapiens-remote-box-v1", room: D.s "room" config, direction: "phone-to-desktop", id, expires: now / 1000.0 + 300.0}
    pure $ D.json {sender: D.s "public" config, nonce, ciphertext: Crypto.box (D.encode body) nonce (D.s "desktop" config) (D.s "private" config)}
  "response" -> do
    let config = D.get "config" input
        frame = D.get "frame" input
    pure $ D.parse $ Crypto.unbox (D.s "ciphertext" frame) (D.s "nonce" frame) (D.s "desktop" config) (D.s "private" config)
  _ -> E.throw "Unknown operation"

setup :: Ref.Ref (Array Json) -> String -> Effect Json
setup messages action = do
  case action of
    "setup" -> do
      Ref.write [] messages
      Setup.main
      callback "showStorageSetup" D.null
      w <- window
      terms <- get w "setTermsState"
      runEffectFn2 (terms :: EffectFn2 Json Json Unit) (D.bool false) (D.text "terms-v1")
      N.setAttribute "#terms-consent" "aria-label" "Accept terms"
    "picked" -> callback "storagePicked" $ D.text "C:\\Users\\Ada\\Sapiens"
    "error" -> callback "storageError" $ D.text "Try another folder"
    _ -> pure unit
  disabled <- D.flag <$> property "#get-started" "disabled"
  check ("Consent: " <> action) disabled $ not $ A.elem action ["accepted","picked","error"]
  notifications <- Ref.read messages
  let submissions = A.filter (\entry -> D.s "storage" entry == "existing") notifications
  case action of
    "setup" -> property "main" "hidden" >>= flip (check "Visible setup") (D.bool false)
    "picked" -> property "#storage-directory" "textContent" >>= flip (check "Folder label") (D.text "…/Ada/Sapiens")
    "error" -> property "#error" "textContent" >>= flip (check "Storage recovery") (D.text "Try another folder")
    "submitted" -> check "Setup submission" submissions [D.json {storage:"existing",directory:"C:\\Users\\Ada\\Sapiens",terms_accepted:true,terms_version:"terms-v1"}]
    "legal" -> do
      check "Consent gating" (A.length submissions) 1
      fields "Legal request" (fromMaybe D.null $ A.last notifications) $ D.json {storage:"document",document:"privacy-policy",terms_accepted:false,terms_version:"terms-v1"}
    _ -> pure unit
  pure $ D.bool true

main :: Effect Unit
main = do
  environment <- N.environment
  if D.s "hash" environment /= "#spec" then Main.main
  else do
    N.removeAttribute "#spec-controls" "hidden"
    N.styleSheet "fixture" "body{overflow:auto}textarea{display:block}"
    messages <- Ref.new []
    w <- window
    set w "webkit" {messageHandlers: {browser: {postMessage: mkEffectFn1 \value -> Ref.modify_ (_ <> [value]) messages}}}
    launchAff_ $ toAffE N.startHostCrypto
    N.listen "#spec-request" "input" $ const do
      raw <- property "#spec-request" "value"
      let request = D.parse $ D.str raw
      unless (D.get "id" request == D.null) do
        let input = D.get "input" request
            id = D.get "id" request
        launchAff_ do
          result <- attempt $ case D.s "operation" input of
            "application" -> observe messages
            action | A.elem action ["setup","accepted","picking","picked","submitted","error","revoked","legal"] -> liftEffect $ setup messages action
            _ -> liftEffect $ cryptoRequest input
          liftEffect do
            let reply = case result of
                  Right value -> D.json {id,value}
                  Left failure -> D.json {id,error:E.message failure}
            Setup.set "#spec-result" "value" $ D.text $ D.encode reply
