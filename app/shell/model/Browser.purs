module Sapiens.Browser where

import Prelude
import Data.Argonaut.Core (Json)
import Data.Array as A
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.String as S
import Data.Tuple (Tuple(..))
import Foreign.Object as O
import Libs4.Browser as B
import Sapiens.Data as D

type Key = {key :: String, tag :: String, composing :: Boolean, shift :: Boolean, ctrl :: Boolean, meta :: Boolean}

key :: Json -> Key
key value = {key:D.s "key" value,tag:D.s "tag" value,composing:D.b "composing" value,shift:D.b "shift" value,ctrl:D.b "ctrl" value,meta:D.b "meta" value}

editing :: Key -> Boolean
editing value = A.elem value.tag ["INPUT","TEXTAREA","SELECT"]

type URL = {href :: String, origin :: String, protocol :: String, host :: String, path :: String, search :: String, hash :: String, username :: String, password :: String}

url :: String -> String -> URL
url base value = decodeURL $ B.parseURL base value

decodeURL :: Json -> URL
decodeURL value = {href:D.s "href" value,origin:D.s "origin" value,protocol:D.s "protocol" value,host:D.s "host" value,path:D.s "path" value,search:D.s "search" value,hash:D.s "hash" value,username:D.s "username" value,password:D.s "password" value}

type WorkerRequest = {method :: String, origin :: String, localOrigin :: String, path :: String, mode :: String, url :: String}

workerRequest :: Json -> WorkerRequest
workerRequest value = {method:D.s "method" value,origin:D.s "origin" value,localOrigin:D.s "localOrigin" value,path:D.s "path" value,mode:D.s "mode" value,url:D.s "url" value}

type Box = {x :: Number, y :: Number, width :: Number, height :: Number, top :: Number, bottom :: Number, clientWidth :: Number, clientHeight :: Number, scrollTop :: Number, scrollHeight :: Number, start :: Int, end :: Int, dataset :: O.Object String}

box :: Json -> Box
box value = {x:D.n "x" value,y:D.n "y" value,width:D.n "width" value,height:D.n "height" value,top:D.n "top" value,bottom:D.n "bottom" value,clientWidth:D.n "clientWidth" value,clientHeight:D.n "clientHeight" value,scrollTop:D.n "scrollTop" value,scrollHeight:D.n "scrollHeight" value,start:D.int $ D.get "start" value,end:D.int $ D.get "end" value,dataset:D.objectStrings $ D.get "dataset" value}

chatKey :: Box -> String
chatKey value = fromMaybe "" $ O.lookup "chatKey" value.dataset

type ScrollPosition = {selected :: String, bottom :: Boolean, key :: String, offset :: Number, width :: Number, height :: Number}

scrollPosition :: Json -> ScrollPosition
scrollPosition value = {selected:D.s "selected" value,bottom:D.b "bottom" value,key:D.s "key" value,offset:D.n "offset" value,width:D.n "width" value,height:D.n "height" value}

type ThemeSettings = {dark :: Boolean, background :: String, foreground :: String}

themeSettings :: Boolean -> Json -> ThemeSettings
themeSettings preferred value =
  let dark = if D.valid $ D.get "dark" value then D.b "dark" value else preferred
  in {dark,background:D.fallback (if dark then "#171717" else "#f6f6f6") $ D.s "background" value,foreground:D.fallback (if dark then "#eee" else "#171717") $ D.s "foreground" value}

data GlobalEvent = ReportError String | Resize | Online | Resume | Visibility | Suspend | Back | Storage String | KeyDown Key | Click (Array String) | Ignore

globalEvent :: String -> Json -> GlobalEvent
globalEvent name value = case name of
  "error" -> ReportError $ D.s "message" value
  "unhandledrejection" -> ReportError $ D.s "message" value
  "resize" -> Resize
  "online" -> Online
  "pageshow" -> Resume
  "visibilitychange" -> Visibility
  "pagehide" -> Suspend
  "popstate" -> Back
  "storage" -> Storage $ D.s "storageKey" value
  "keydown" -> KeyDown $ key value
  "click" -> Click $ D.strings $ D.get "ancestors" value
  _ -> Ignore

type NativeReport = {owner :: String, op :: String, body :: Json}

nativeReport :: Json -> NativeReport
nativeReport value =
  {owner:D.s "owner" value
  ,op:if D.s "open" value /= "" || D.s "file" value /= "" then "open" else D.s "action" value
  ,body:if D.s "open" value /= "" then D.json {url:D.get "open" value} else if D.s "file" value /= "" then D.json {path:D.get "file" value} else if D.s "action" value == "" then value else D.get "fields" value}

data Destination = WebAddress String | FilePath String | WorkspaceLink String
derive instance eqDestination :: Eq Destination

data Command = OpenMemo | Open Destination (Maybe String) | FocusTab String | CloseTab String | GoBack | GoForward | Reload | Zoom Number | SaveBookmark String | RemoveBookmark String
derive instance eqCommand :: Eq Command

openDestination :: String -> Command
openDestination value = Open (destination value) Nothing

destination :: String -> Destination
destination value = if D.matches "^(https?:|file:|about:)" value then WebAddress value else FilePath value

addressCommand :: String -> Maybe String -> Maybe Command
addressCommand raw id = let value = S.trim raw in if value == "" then Nothing else Just $ Open (destination value) id

request :: Command -> {op :: String, fields :: Json}
request = case _ of
  OpenMemo -> {op:"memo",fields:D.empty}
  Open target id -> {op:"open",fields:D.obj $ [case target of
    WebAddress value -> Tuple "url" $ D.text value
    FilePath value -> Tuple "path" $ D.text value
    WorkspaceLink value -> Tuple "href" $ D.text value] <> maybe [] (\value -> [Tuple "id" $ D.text value]) id}
  FocusTab id -> {op:"focus",fields:D.json {id}}
  CloseTab id -> {op:"close",fields:D.json {id}}
  GoBack -> {op:"back",fields:D.empty}
  GoForward -> {op:"forward",fields:D.empty}
  Reload -> {op:"reload",fields:D.empty}
  Zoom factor -> {op:"zoom",fields:D.json {factor}}
  SaveBookmark id -> {op:"bookmark",fields:D.json {id}}
  RemoveBookmark id -> {op:"unbookmark",fields:D.json {id}}
