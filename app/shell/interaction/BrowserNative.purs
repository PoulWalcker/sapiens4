module Sapiens.BrowserNative where

import Prelude
import Data.Array as A
import Data.Either (Either(..))
import Data.Int as I
import Data.Number (abs)
import Data.String as S
import Effect.Class (liftEffect)
import Effect.Exception (error)
import Effect.Ref as Ref
import Effect.Unsafe (unsafePerformEffect)
import Data.Foldable (traverse_)
import Web.Event.Event as Event
import Sapiens.BrowserDatabase as Database
import Sapiens.Data as D
import Control.Promise (Promise, fromAff, toAffE)
import Data.Argonaut.Core (Json)
import Data.Maybe (Maybe(..), maybe)
import Data.Nullable (Nullable, toMaybe)
import Effect (Effect)
import Effect.Aff (Aff, attempt, throwError, makeAff, effectCanceler)
import Web.Event.Event (Event)

foreign import data Socket :: Type
foreign import data Camera :: Type
foreign import data Response :: Type
foreign import environment :: Effect Json
foreign import now :: Effect Number
foreign import uuid :: Effect String
foreign import storageRead :: String -> Effect String
foreign import storageWrite :: String -> String -> Effect Boolean
foreign import storageRemove :: String -> Effect Unit
foreign import storageKeys :: Effect (Array String)
foreign import openSocket :: String -> Effect Unit -> (String -> Effect Unit) -> (Int -> Effect Unit) -> Effect Socket
foreign import socketSend :: Socket -> String -> Effect Unit
foreign import socketClose :: Socket -> Effect Unit
foreign import callback :: String -> (Json -> Effect Unit) -> Effect (Effect Unit)
foreign import eventData :: Event -> Effect Json
foreign import focus :: String -> Effect Unit
foreign import selectRange :: String -> Int -> Int -> Effect Unit
foreign import measure :: String -> Effect Json
foreign import measureChildren :: String -> Effect (Array Json)
foreign import scrollIntoView :: String -> Effect Unit
foreign import cssProperty :: String -> String -> Effect String
foreign import setStyle :: String -> String -> String -> Effect Unit
foreign import setAttribute :: String -> String -> String -> Effect Unit
foreign import title :: String -> Effect Unit
foreign import nativeMessage :: Json -> Effect Unit
foreign import historyPush :: Json -> Effect Unit
foreign import historyReplace :: String -> Effect Unit
foreign import historyBack :: Effect Unit
foreign import reload :: Effect Unit
foreign import confirm :: String -> Effect Boolean
foreign import clipboard :: String -> Effect (Promise Unit)
type FileSelection =
  { name :: String
  , mime :: Maybe String
  , size :: Number
  , data :: String
  }

foreign import cameraStop :: Camera -> Effect Unit
foreign import qrImageImpl :: String -> Json -> Effect (Promise String)
foreign import parseURL :: String -> String -> Json
foreign import encodeURI :: String -> String
foreign import decodeURI :: String -> String
foreign import base64Encode :: String -> String
foreign import base64Decode :: String -> Json
foreign import byteKey :: Json -> String
foreign import byteLength :: String -> Int
foreign import parseTime :: String -> Number
foreign import formatDate :: String -> Json -> String -> String
foreign import registerWorker :: String -> Effect (Promise Unit)
foreign import workerInstall :: String -> (Json -> Effect (Promise Unit)) -> Effect Unit
foreign import workerFetch :: (Json -> Boolean) -> (Json -> Effect (Promise Response)) -> Effect Unit
foreign import cacheAll :: String -> Array String -> Effect (Promise Unit)
foreign import cacheKeys :: Effect (Promise (Array String))
foreign import cacheDelete :: String -> Effect (Promise Unit)
foreign import fetchResponse :: String -> Effect (Promise Response)
foreign import activateWorker :: Effect (Promise Unit)

foreign import removeAttribute :: String -> String -> Effect Unit
foreign import timestamp :: Number -> String

foreign import styleSheet :: String -> String -> Effect Unit
foreign import callbackResult :: String -> (Json -> Effect Json) -> Effect Unit

foreign import subscribeEvent :: String -> String -> (Event -> Effect Unit) -> Effect (Effect Unit)

foreign import subscribeMedia :: String -> Effect Unit -> Effect (Effect Unit)

foreign import data Timer :: Type
foreign import data Node :: Type
foreign import data File :: Type
foreign import data Image :: Type
foreign import socketState :: Socket -> Effect Int
foreign import sameSocket :: Socket -> Socket -> Boolean
foreign import setTimer :: Int -> Effect Unit -> Effect Timer
foreign import clearTimer :: Timer -> Effect Unit
foreign import queryAll :: String -> Effect (Array Node)
foreign import property :: Node -> String -> Effect Json
foreign import setProperty :: Node -> String -> Json -> Effect Unit
foreign import nodeAttribute :: Node -> String -> String -> Effect Unit
foreign import listen :: String -> String -> (Event -> Effect Unit) -> Effect Unit
foreign import detail :: Event -> Int
foreign import callback2 :: String -> (Json -> Json -> Effect Unit) -> Effect Unit
foreign import rootAttribute :: String -> Effect String
foreign import topFrame :: Effect Boolean
foreign import sessionRead :: String -> Effect String
foreign import sessionWrite :: String -> String -> Effect Unit
foreign import observeStorage :: (String -> String -> Effect Unit) -> Effect Unit
foreign import nativeAvailable :: Effect Boolean
foreign import pickFile :: String -> Effect (Promise (Nullable File))
foreign import fileInfo :: File -> {name :: String, mime :: String, size :: Number}
foreign import readFile :: File -> Effect (Promise String)
foreign import canvasQR :: Image -> Int -> Int -> Effect String
foreign import loadImage :: String -> Effect (Promise Image)
foreign import imageSize :: Image -> {width :: Int, height :: Int}
foreign import videoImage :: String -> Effect (Nullable Image)
foreign import videoSize :: Image -> {width :: Int, height :: Int}
foreign import getCamera :: Json -> Effect (Promise Camera)
foreign import attachCamera :: String -> Camera -> Effect (Promise Unit)
foreign import requestImpl :: String -> Json -> Effect (Promise Json)
foreign import requestBytesImpl :: String -> Json -> Effect (Promise Json)

request :: String -> String -> String -> Json -> Effect (Promise Json)
request url method body headers = requestImpl url $ D.json
  {method, headers, body: if method == "GET" || method == "HEAD" then D.null else D.text body, timeout: 20000, redirect: "error", cache: "no-store"}

requestBytes :: String -> Json -> Effect (Promise Json)
requestBytes url headers = requestBytesImpl url $ D.json {headers, timeout: 20000, redirect: "error", cache: "no-store"}

chooseFile :: String -> Int -> Aff (Maybe FileSelection)
chooseFile accept limit = do
  selected <- toAffE $ pickFile accept
  case toMaybe selected of
    Nothing -> pure Nothing
    Just file -> do
      let info = fileInfo file
      when (info.size > I.toNumber limit) $ throwError $ error "File is too large."
      content <- toAffE $ readFile file
      case S.indexOf (S.Pattern ",") content of
        Nothing -> throwError $ error "Could not read this file."
        Just index -> pure $ Just {name: info.name, mime: if info.mime == "" then Nothing else Just info.mime, size: info.size, data: S.drop (index + 1) content}

readQR :: Image -> {width :: Int, height :: Int} -> Int -> Effect Json
readQR image size limit = do
  let scale = min 1.0 $ I.toNumber limit / I.toNumber (max size.width size.height)
  result <- canvasQR image (I.round $ I.toNumber size.width * scale) (I.round $ I.toNumber size.height * scale)
  pure $ D.json {data: result, width: size.width, height: size.height}

decodeQR :: String -> Int -> Effect (Promise Json)
decodeQR source limit = fromAff do
  image <- toAffE $ loadImage source
  let size = imageSize image
  when (I.toNumber size.width * I.toNumber size.height > 20000000.0) $ throwError $ error "Image dimensions are too large."
  liftEffect $ readQR image size limit

cameraQR :: String -> Int -> Effect Json
cameraQR selector limit = do
  image <- toMaybe <$> videoImage selector
  maybe (pure $ D.json {data: ""}) (\value -> readQR value (videoSize value) limit) image

cameraStart :: String -> Effect (Promise Camera)
cameraStart selector = fromAff do
  camera <- toAffE $ getCamera $ D.json {video: {facingMode: {ideal: "environment"}}, audio: false}
  nodes <- liftEffect $ queryAll selector
  result <- attempt do
    when (A.null nodes) $ throwError $ error "Camera view closed."
    toAffE $ attachCamera selector camera
  case result of
    Right _ -> pure camera
    Left failure -> do
      liftEffect $ cameraStop camera
      throwError failure

foreign import data Registration :: Type
foreign import data Worker :: Type
foreign import subscribeRaw :: String -> String -> (Event -> Effect Unit) -> Effect (Effect Unit)
foreign import promptInstall :: Event -> Effect (Promise Unit)
foreign import registration :: Effect (Promise (Nullable Registration))
foreign import registrationUpdate :: Registration -> Effect (Promise Unit)
foreign import registrationWorkers :: Registration -> Array (Nullable Worker)
foreign import workerState :: Worker -> Effect String
foreign import workerStateChange :: Worker -> Effect Unit -> Effect (Effect Unit)
foreign import cachedResponse :: String -> String -> Boolean -> Effect (Promise (Nullable Response))
foreign import scrollTo :: String -> Number -> Effect Unit
foreign import dialogOpen :: Node -> Effect Boolean
foreign import openDialog :: Node -> Effect Unit
foreign import closeDialog :: Node -> Effect Unit
foreign import digestImpl :: String -> Array String -> Effect (Promise (Array Int))
foreign import markupNodes :: String -> Array Node
foreign import nodeType :: Node -> Int
foreign import nodeText :: Node -> String
foreign import nodeTag :: Node -> String
foreign import nodeAttrs :: Node -> Json
foreign import nodeChildren :: Node -> Array Node
foreign import nodeSource :: Node -> String

database :: String -> Int -> Array String -> String -> String -> String -> Json -> Effect (Promise Json)
database = Database.database

installEvent :: Ref.Ref (Maybe Event)
installEvent = unsafePerformEffect $ Ref.new Nothing

subscribe :: String -> String -> (Json -> Effect Unit) -> Effect (Effect Unit)
subscribe target name emit = subscribeRaw target name \event -> do
  when (name == "beforeinstallprompt") do
    Event.preventDefault event
    Ref.write (Just event) installEvent
  eventData event >>= emit

installPrompt :: Effect (Promise Unit)
installPrompt = fromAff do
  event <- liftEffect $ Ref.read installEvent
  liftEffect $ Ref.write Nothing installEvent
  traverse_ (toAffE <<< promptInstall) event

updateWorker :: Effect (Promise Unit)
updateWorker = fromAff do
  current <- toMaybe <$> toAffE registration
  traverse_ (\r -> do
    toAffE $ registrationUpdate r
    traverse_ (\worker -> makeAff \done -> do
      cleanup <- Ref.new (pure unit)
      let check = do
            state <- workerState worker
            when (state == "activated" || state == "redundant") do
              join $ Ref.read cleanup
              done $ if state == "activated" then Right unit else Left $ error "Update unavailable"
      remove <- workerStateChange worker check
      Ref.write remove cleanup
      check
      pure $ effectCanceler remove) $ A.head $ A.mapMaybe toMaybe $ registrationWorkers r) current

cacheResponse :: String -> String -> Effect (Promise Response)
cacheResponse key url = fromAff do
  cached <- toMaybe <$> toAffE (cachedResponse key url true)
  maybe (toAffE $ fetchResponse url) pure cached

scroll :: String -> Number -> Effect Unit
scroll selector value = do
  current <- measure selector
  when (abs (D.n "scrollTop" current - value) > 1.0) $ scrollTo selector value

showDialog :: String -> Boolean -> Effect Unit
showDialog selector open = queryAll selector >>= traverse_ (\node -> do
  opened <- dialogOpen node
  when (open /= opened) $ if open then openDialog node else closeDialog node)

digest :: String -> Array String -> Effect (Promise (Array Int))
digest prefix keys = digestImpl "SHA-256" $ [base64Encode prefix] <> keys

parseMarkup :: String -> Json
parseMarkup = D.array <<< map read <<< markupNodes
  where
  read node = case nodeType node of
    3 -> D.json {text: nodeText node}
    1 -> D.json {tag: nodeTag node, attrs: nodeAttrs node, children: map read $ nodeChildren node, text: nodeText node, source: nodeSource node}
    _ -> D.empty

qrImage :: String -> Effect (Promise String)
qrImage text = qrImageImpl text $ D.json {width: 320, margin: 3}
