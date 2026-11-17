module Sapiens.DesktopUpdate (Slots, component, main) where

import Prelude
import Control.Monad.Except (runExcept)
import Data.Argonaut.Core (Json)
import Data.Const (Const)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Foldable (traverse_)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Foreign (readBoolean, unsafeToForeign)
import Foreign.Index (readProp)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HTML
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Subscription as HS
import Halogen.VDom.Driver (runUI)
import Libs4.Browser as B
import Sapiens.Data as D
import Web.DOM.ParentNode (QuerySelector(..))
import Web.Event.Event (Event, EventType(..))

type Slots = (desktopUpdate :: H.Slot (Const Void) Void Unit)

data Display = Hidden | Offer String | Progress String
data Action = Listen | Display Json | Install String Event

component :: forall q. H.Component q Unit Void Aff
component = H.mkComponent
  { initialState: const Hidden
  , render: case _ of
      Hidden -> HTML.text ""
      Offer version -> HTML.button
        [ HP.classes [HTML.ClassName "desktop-update-button"], HP.type_ HP.ButtonButton
        , HP.title ("Install version " <> version), HE.handler (EventType "click") (Install version)
        ] [HTML.text "Update & Restart"]
      Progress label -> HTML.button
        [HP.classes [HTML.ClassName "desktop-update-button"], HP.disabled true] [HTML.text label]
  , eval: H.mkEval H.defaultEval {initialize = Just Listen, handleAction = handle}
  }
  where
  handle = case _ of
    Listen -> do
      void $ H.subscribe $ Display <$> HS.makeEmitter (B.callback "setDesktopUpdate")
      liftEffect $ B.nativeMessage $ D.json {desktopUpdateReady: true}
    Display value -> H.put case D.s "version" value, D.s "label" value of
      "", "" -> Hidden
      "", label -> Progress label
      version, _ -> Offer version
    Install version event ->
      -- This DOM library omits isTrusted; read and validate the browser's property at the event boundary.
      when (runExcept (readProp "isTrusted" (unsafeToForeign event) >>= readBoolean) == Right true) $
        liftEffect $ B.nativeMessage $ D.json {updateAndRestart: version}

-- The storage picker is a local file and has no workspace Halogen root.
main :: Effect Unit
main = HA.runHalogenAff do
  target <- HA.selectElement (QuerySelector "#desktop-update")
  traverse_ (runUI component unit) target
