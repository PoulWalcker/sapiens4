module Sapiens.Html where

import Sapiens.Form as F
import Prelude
import Data.Tuple (Tuple)
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as C
import Halogen.HTML.Properties as P
import Halogen.HTML.Events as E
import Sapiens.Action (Action(..))
import Web.Event.Event (Event, EventType(..))

type Html = H.ComponentHTML Action () Aff
type Prop = P.IProp () Action

node :: String -> Array Prop -> Array Html -> Html
node tag = C.element (C.ElemName tag)

keyed :: String -> Array Prop -> Array (Tuple String Html) -> Html
keyed tag = C.keyed (C.ElemName tag)

text :: String -> Html
text = C.text

textNode :: String -> Array Prop -> String -> Html
textNode tag props caption = node tag props [text caption]

span :: Array Prop -> String -> Html
span = textNode "span"

control :: Array Prop -> String -> Action -> Html
control props caption = controlWith props [text caption]

controlWith :: Array Prop -> Array Html -> Action -> Html
controlWith props children action = node "button" ([attr "type" "button",click action] <> props) children

chevron :: Boolean -> Html
chevron sideways = svg "svg" [cls $ "activity-chevron" <> if sideways then " sideways" else "",attr "viewBox" "0 0 16 16",attr "width" "12",attr "height" "12",attr "fill" "none",attr "aria-hidden" "true",attr "focusable" "false"]
  [svg "path" [attr "d" "m4.5 6.25 3.5 3.5 3.5-3.5",attr "stroke" "currentColor",attr "stroke-width" "1.5",attr "stroke-linecap" "round",attr "stroke-linejoin" "round"] []]
  where
  svg tag = C.elementNS (C.Namespace "http://www.w3.org/2000/svg") (C.ElemName tag)

attr :: String -> String -> Prop
attr name = P.attr (C.AttrName name)

bool :: String -> Boolean -> Prop
bool name = P.prop (C.PropName name)

value :: String -> Prop
value = P.prop (C.PropName "value")

cls :: String -> Prop
cls = attr "class"

id :: String -> Prop
id = attr "id"

label :: String -> Prop
label = attr "aria-label"

hidden :: Boolean -> Prop
hidden = bool "hidden"

disabled :: Boolean -> Prop
disabled = bool "disabled"

style :: String -> Prop
style = attr "style"

on :: String -> (Event -> Action) -> Prop
on name action = E.handler (EventType name) action

click :: Action -> Prop
click action = on "click" (const action)

input :: (String -> Action) -> Prop
input action = case E.onValueInput action of P.IProp prop -> P.IProp prop

change :: (String -> Action) -> Prop
change action = case E.onValueChange action of P.IProp prop -> P.IProp prop

check :: (Boolean -> Action) -> Prop
check action = case E.onChecked action of P.IProp prop -> P.IProp prop

submit :: Action -> Prop
submit action = on "submit" \event -> Submit event action

button :: String -> String -> Action -> Html
button classes = control [cls classes]

icon :: String -> String -> Action -> Html
icon caption = control [cls "icon-button",label caption,attr "title" caption]

div :: String -> Array Html -> Html
div classes = node "div" [cls classes]

p :: String -> String -> Html
p classes = textNode "p" [cls classes]

empty :: Html
empty = text ""

when :: Boolean -> Html -> Html
when yes content = if yes then content else empty

field :: String -> String -> F.Field -> String -> Array Prop -> Array Html -> Html
field tag caption key current props children = node "label" []
  [text caption,node tag ([attr "name" $ F.name key,label caption,value current,(if tag == "select" then change else input) (Field key)] <> props) children]

choice :: Boolean -> String -> Action -> Html
choice selected = control [cls $ if selected then "selected" else "",attr "aria-pressed" $ show selected]

link :: String -> String -> Html
link href caption = node "a" [attr "href" href,attr "target" "_blank",attr "rel" "noopener noreferrer"] [text caption]
