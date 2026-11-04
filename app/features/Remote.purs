module Sapiens.Remote where

import Prelude
import Sapiens.Environment (Environment)
import Sapiens.Platform as Platform
import Control.Promise (toAffE)
import Data.Argonaut.Core (Json, toArray)
import Data.Argonaut.Decode (decodeJson)
import Data.Array as A
import Data.Either (Either(..), either)
import Data.Foldable (foldl, traverse_)
import Data.Int as I
import Data.Maybe (Maybe(..), fromMaybe, isNothing)
import Data.Number as N
import Data.String as S
import Data.String.CodeUnits as CU
import Effect (Effect)
import Effect.Aff (Aff, Milliseconds(..), delay, effectCanceler, launchAff_, makeAff, throwError)
import Effect.Class (liftEffect)
import Effect.Exception (Error, error, throwException)
import Effect.Exception as Exception
import Effect.Ref as Ref
import Foreign.Object as O
import Libs4.Browser as B
import Sapiens.Data as D
import Sapiens.Browser as Browser
import Sapiens.Domain as X

protocol :: String
protocol = "sapiens-remote-box-v1"

data LinkState = Disconnected | Connecting | Ready | Blocked String

type Transport =
  { session :: Ref.Ref Json
  , socket :: Ref.Ref (Maybe B.Socket)
  , pending :: Ref.Ref (O.Object (Either Error Json -> Effect Unit))
  , state :: Ref.Ref LinkState
  , serial :: Ref.Ref Int
  , connection :: Ref.Ref String
  , generation :: Ref.Ref Int
  }

sessionKey :: Json -> String
sessionKey value = D.s "relay" value <> "|" <> D.str (D.at ["corpora", "id"] value)

store :: String -> String -> String -> Json -> Aff Json
store operation table key value = toAffE $ B.database "sapiens-remote" 2 ["device", "connections"] operation table key value

sessions :: Aff (Array Json)
sessions = do
  old <- store "get" "device" "trusted" D.null
  when (D.valid $ D.at ["corpora", "id"] old) do
    void $ store "put" "connections" (sessionKey old) old
    void $ store "delete" "device" "trusted" D.null
  D.arr <$> store "getAll" "connections" "" D.null

save :: Json -> Aff Unit
save value = void $ store "put" "connections" (sessionKey value) (D.remove "secret" value)

clear :: String -> Aff Unit
clear key = void $ store "delete" "connections" key D.null

url64 :: String -> String
url64 = D.replace "=+$" "" <<< D.replace "/" "_" <<< D.replace "\\+" "-"

base64 :: String -> String
base64 input = let value = D.replace "_" "/" $ D.replace "-" "+" input in value <> CU.fromCharArray (A.replicate (mod (4 - mod (CU.length value) 4) 4) '=')

invalid :: String
invalid = "Use the QR from the host corpora’s New Pairing."

type Invitation = X.Invitation

decode :: String -> Either String Invitation
decode raw = decodeValue (S.trim raw) >>= \value ->
  either (const $ Left invalid) Right $ decodeJson $
    if D.valid (D.get "public_id" value) then value else D.put "public_id" (D.text "") value

decodeValue :: String -> Either String Json
decodeValue value
  | CU.length value > 4096 = Left invalid
  | S.take 1 value == "{" = if D.valid (D.parse value) then Right (D.parse value) else Left invalid
  | D.matches "^https?://" value =
      let u = Browser.url "https://invalid.local" value in
      case decodeValue (S.trim $ S.drop 1 $ u.hash) of
        Right parsed | u.origin == D.s "relay" parsed && u.path == "/" && u.search == "" && u.username == "" && u.password == "" -> Right parsed
        _ -> Left "Invalid pairing link."
  | otherwise =
      let parts = S.split (S.Pattern ".") (D.replace "\\s" "" value)
          part i = fromMaybe "" (A.index parts i)
          relay = B.base64Decode $ base64 $ part 1
          expiration = fromMaybe 0 (I.fromStringAs (fromMaybe I.decimal (I.radix 36)) (part 6))
      in if D.valid relay && A.elem (A.length parts) [7,8] && part 0 == "s4" && A.all (D.matches "^[A-Za-z0-9_-]+$") (A.take 5 $ A.drop 1 parts) && D.matches "^[0-9a-z]{1,10}$" (part 6) && (A.length parts == 7 || D.matches "^sapi4_[a-z0-9]{9}$" (part 7)) then
        Right $ D.json {v: protocol, relay, room: part 2, token: part 3, secret: part 4, desktop: base64 $ part 5, expires: expiration, public_id: part 7}
      else Left invalid

invitationLink :: Invitation -> String
invitationLink v = v.relay <> "/#" <> S.joinWith "." (["s4", url64 $ B.base64Encode $ v.relay, v.room, v.token, v.secret, url64 $ v.desktop, I.toStringAs (fromMaybe I.decimal (I.radix 36)) (I.floor $ v.expires)] <> if v.public_id == "" then [] else [v.public_id])

confirmation :: Json -> Aff String
confirmation session = do
  let desktop = D.s "desktop" session
      phone = D.str $ D.at ["keys", "publicKey"] session
  when (B.byteLength desktop /= 32 || B.byteLength phone /= 32) $ throwError $ error "Invalid device key."
  bytes <- toAffE $ B.digest ("sapiens-confirm-v1\x0000" <> D.s "room" session <> "\x0000") [desktop,phone]
  let alphabet = CU.toCharArray "ABCDEFGHJKMNPQRSTUVWXYZabcdefghjkmnpqrstuvwxyz"
      base = I.toNumber $ A.length alphabet
      total = foldl (\n x -> n * 256.0 + I.toNumber x) 0.0 (A.take 6 bytes)
      initial = total - N.floor (total / N.pow base 8.0) * N.pow base 8.0
      step {value,chars} _ = {value:N.floor (value / base), chars:A.cons (fromMaybe 'A' $ A.index alphabet $ I.floor (value - N.floor (value / base) * base)) chars}
      code = CU.fromCharArray (foldl step {value:initial,chars:[]} (A.range 1 8)).chars
  pure $ S.take 4 code <> " · " <> S.drop 4 code

deviceInfo :: Environment -> Json
deviceInfo env =
  let ua = env.userAgent
      ipad = D.matches "iPad" ua || (D.matches "Macintosh" ua && env.touchPoints > 1.0)
      name = if ipad then "iPad" else if D.matches "iPhone" ua then "iPhone" else if D.matches "Android" ua then "Android device" else if D.matches "Windows" ua then "Windows PC" else if D.matches "Macintosh" ua then "Mac" else if D.matches "Linux" ua then "Linux device" else "Remote device"
      versionPattern = "(?:CPU (?:iPhone )?OS|iPhone OS|Android|Windows NT|Mac OS X) ([0-9_.]+)"
      version = if D.matches versionPattern ua then " " <> D.replace "_" "." (D.replace ("^.*?" <> versionPattern <> ".*$") "$1" ua) else ""
      platform = case name of
        "iPad" -> "iPadOS"
        "iPhone" -> "iOS"
        "Android device" -> "Android"
        "Windows PC" -> "Windows"
        "Mac" -> "macOS"
        "Linux device" -> "Linux"
        _ -> "Unknown platform"
  in D.json {name, platform: platform <> version, client: if env.installed then "Sapiens4 Home Screen app" else "Sapiens4 browser"}

create :: Json -> Aff Transport
create source = do
  let keys = D.get "keys" source
      secret = B.byteKey $ D.get "secretKey" keys
      public = B.byteKey $ D.get "publicKey" keys
  when (B.byteLength secret /= 32 || B.byteLength public /= 32 || B.publicKey secret /= public) $ throwError $ error "Saved device keys are invalid. Connect this corpora again."
  liftEffect do
    session <- Ref.new $ D.put "keys" (D.json {publicKey: public, secretKey: secret}) source
    socket <- Ref.new Nothing
    pending <- Ref.new O.empty
    state <- Ref.new Disconnected
    serialRef <- Ref.new 0
    connection <- Ref.new ""
    generation <- Ref.new 0
    pure {session,socket,pending,state,serial:serialRef,connection,generation}

data InvitationError = InsecureOrigin | ExpiredInvitation

invitationError :: InvitationError -> String
invitationError = case _ of
  InsecureOrigin -> "Open Sapiens4 over HTTPS."
  ExpiredInvitation -> "Invitation expired or belongs to another server. Use the latest QR from your host."

validateInvitation :: Environment -> Number -> Invitation -> Either InvitationError Unit
validateInvitation env time value
  | not env.secure = Left InsecureOrigin
  | value.v /= protocol || value.relay /= env.origin || value.expires <= time || value.expires > time + 259260.0 || not (A.all (D.matches "^[A-Za-z0-9_-]{32,64}$") [value.room,value.token,value.secret]) || B.byteLength value.desktop /= 32 = Left ExpiredInvitation
  | otherwise = Right unit

invitation :: Invitation -> Aff Transport
invitation data_ = do
  env <- liftEffect Platform.environment
  time <- (_ / 1000.0) <$> liftEffect B.now
  either (throwError <<< error <<< invitationError) pure $ validateInvitation env time data_
  when (data_.public_id /= "") do
    result <- toAffE $ B.request ("/api/corpora/" <> B.encodeURI (data_.public_id)) "GET" "" D.empty
    let found = D.parse $ D.s "text" result
    unless (D.n "status" result == 200.0 && D.s "desktop" found == data_.desktop && D.n "expires" found > time) $ throwError $ error "Invitation replaced. Use the latest QR from your host."
  keys <- liftEffect B.keyPair
  create $ D.put "keys" keys $ D.json data_

rejectAll :: Transport -> String -> Effect Unit
rejectAll client message = do
  pending <- Ref.read client.pending
  Ref.write O.empty client.pending
  traverse_ (\done -> done $ Left $ error message) (O.values pending)

send :: Transport -> Json -> Effect Unit
send client value = do
  socket <- Ref.read client.socket
  case socket of
    Nothing -> throwException $ error "Connection closed"
    Just s -> B.socketSend s $ D.encode value

serial :: Transport -> Effect String
serial client = do
  number <- Ref.modify (_ + 1) client.serial
  pure $ show number

consume :: Transport -> Json -> Effect Unit
consume client item = do
  session <- Ref.read client.session
  time <- (_ / 1000.0) <$> B.now
  let frame = D.get "frame" item
      message = D.parse $ B.unbox (D.s "ciphertext" frame) (D.s "nonce" frame) (D.s "desktop" session) (D.str $ D.at ["keys","secretKey"] session)
  when (D.s "sender" frame == D.s "desktop" session && D.s "v" message == protocol && D.s "room" message == D.s "room" session && D.s "direction" message == "desktop-to-phone" && D.s "kind" message == "response" && D.n "expires" message > time && D.n "expires" message <= time + 86400.0) do
    pending <- Ref.read client.pending
    case O.lookup (D.s "id" message) pending of
      Nothing -> pure unit
      Just done -> do
        Ref.modify_ (O.delete $ D.s "id" message) client.pending
        let result = D.get "result" message
        done $ if D.s "error" result == "" then Right result else Left $ error $ D.s "error" result

data RelayEvent = RelayReady | RelayRejected String | RelayMessages (Array Json) | RelayIgnored

decodeRelayEvent :: String -> Either String RelayEvent
decodeRelayEvent raw =
  let value = D.parse raw
  in if CU.length raw > 22008192 || not (D.valid value) then Left "Invalid relay frame" else case D.s "type" value of
    "ready" -> Right RelayReady
    "result" -> Right $ if D.s "error" value == "" then RelayIgnored else RelayRejected (D.s "error" value)
    "messages" -> case toArray $ D.get "messages" value of
      Just items | A.length items <= 20 -> Right $ RelayMessages items
      _ -> Left "Invalid relay messages"
    _ -> Right RelayIgnored

connect :: Transport -> Aff Unit
connect client = do
  state <- liftEffect $ Ref.read client.state
  case state of
    Blocked reason -> throwError $ error reason
    _ -> pure unit
  existing <- liftEffect $ Ref.read client.socket
  when (isNothing existing) do
    session <- liftEffect $ Ref.read client.session
    version <- liftEffect $ Ref.modify (_ + 1) client.generation
    liftEffect do
      Ref.write Connecting client.state
      B.uuid >>= flip Ref.write client.connection
    let current action = do
          generation <- Ref.read client.generation
          when (generation == version) action
    socket <- liftEffect $ B.openSocket (D.replace "^http" "ws" (D.s "relay" session) <> "/api/socket")
      (current $ send client $ D.json {type: "auth", room: D.get "room" session, token: D.get "token" session})
      (\raw -> current do
        result <- Exception.try do
          case decodeRelayEvent raw of
            Left _ -> close client
            Right RelayReady -> Ref.write Ready client.state
            Right (RelayRejected reason) -> rejectAll client reason
            Right (RelayMessages items) -> traverse_ (\item -> do
              consume client item
              id <- serial client
              send client $ D.json {type: "ack", id, message: D.get "id" item}) items
            Right RelayIgnored -> pure unit
        case result of
          Left _ -> close client
          Right _ -> pure unit)
      (\code -> current do
        Ref.modify_ disconnect client.state
        Ref.write Nothing client.socket
        let message = if code == 4001 then "Access revoked. Connect this corpora again." else if code == 4002 then "This connection is open in another window." else "This device is unreachable. Try again later."
        when (code == 4001 || code == 4002) $ Ref.write (Blocked message) client.state
        when (code == 4001) $ launchAff_ $ clear $ sessionKey session
        rejectAll client message)
    liftEffect $ Ref.write (Just socket) client.socket
  version <- liftEffect $ Ref.read client.generation
  wait version 1000
  where
  wait version count = do
    state <- liftEffect $ Ref.read client.state
    current <- liftEffect $ Ref.read client.generation
    if current /= version then throwError $ error "Connection closed" else case state of
      Ready -> pure unit
      Blocked reason -> liftEffect (close client) *> throwError (error reason)
      _ | count <= 0 -> liftEffect (close client) *> throwError (error "This device is unreachable. Try again later.")
        | otherwise -> delay (Milliseconds 10.0) *> wait version (count - 1)

disconnect :: LinkState -> LinkState
disconnect = case _ of
  Blocked reason -> Blocked reason
  _ -> Disconnected

request :: Transport -> Int -> Json -> Aff Json
request client timeout payload = do
  connect client
  session <- liftEffect $ Ref.read client.session
  pending <- liftEffect $ Ref.read client.pending
  when (O.size pending >= 128) $ throwError $ error "Connection busy. Try again shortly."
  id <- liftEffect B.uuid
  nonce <- liftEffect $ B.randomBytes 24
  time <- liftEffect B.now
  env <- liftEffect Platform.environment
  connection <- liftEffect $ Ref.read client.connection
  let plain = D.encode $ D.merge (D.json {v: protocol, room: D.get "room" session, direction: "phone-to-desktop", corpora_id: D.at ["corpora","id"] session, connection_id: connection, device_info: deviceInfo env, id, expires: (time + I.toNumber timeout) / 1000.0}) payload
  when (B.byteLength (B.base64Encode plain) > 16000000) $ throwError $ error "Request is too large."
  let frame = D.json {sender: D.at ["keys","publicKey"] session, nonce, ciphertext: B.box plain nonce (D.s "desktop" session) (D.str $ D.at ["keys","secretKey"] session)}
  makeAff \done -> do
    Ref.modify_ (O.insert id done) client.pending
    serialId <- serial client
    sent <- Exception.try $ send client $ D.json {type: "send", id: serialId, frame}
    case sent of
      Left failure -> Ref.modify_ (O.delete id) client.pending *> done (Left failure)
      Right _ -> pure unit
    launchAff_ do
      delay $ Milliseconds $ I.toNumber timeout
      liftEffect do
        current <- Ref.read client.pending
        traverse_ (\callback_ -> callback_ $ Left $ error "This device is unreachable. Try again later.") (O.lookup id current)
        Ref.modify_ (O.delete id) client.pending
    pure $ effectCanceler $ Ref.modify_ (O.delete id) client.pending

authenticate :: Transport -> Boolean -> Aff Json
authenticate client pairing = do
  session <- liftEffect $ Ref.read client.session
  result <- request client (if pairing then 180000 else 15000) $ if pairing then D.json {kind: "pair", secret: D.get "secret" session} else D.json {kind: "request", op: "resume"}
  let corpora = D.get "corpora" result
      previous = D.str $ D.at ["corpora","id"] session
  unless (D.b "paired" result && D.s "name" corpora /= "" && CU.length (D.s "name" corpora) <= 200 && D.matches "^[0-9a-f]{8}(-[0-9a-f]{4}){3}-[0-9a-f]{12}$" (D.s "id" corpora) && (previous == "" || previous == D.s "id" corpora)) $ throwError $ error "Corpora identity changed. Connect again."
  let updated = D.remove "secret" $ D.put "corpora" corpora session
  liftEffect $ Ref.write updated client.session
  save updated
  pure corpora

api :: Transport -> String -> String -> Json -> Aff Json
api client method path data_ = do
  response <- request client 15000 $ D.json {kind: "request", op: "api", method, path, data: data_}
  let value = D.get "body" response
  when (D.n "status" response < 200.0 || D.n "status" response >= 300.0) $ throwError $ error $ D.fallback "Request failed" $ D.s "error" value
  pure value

close :: Transport -> Effect Unit
close client = do
  Ref.modify_ (_ + 1) client.generation
  socket <- Ref.read client.socket
  Ref.write Nothing client.socket
  Ref.modify_ disconnect client.state
  traverse_ B.socketClose socket
  rejectAll client "Connection closed"
