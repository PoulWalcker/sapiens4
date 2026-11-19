module Sapiens.Main where

import Prelude
import Sapiens.Platform as Platform
import Effect (Effect)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Sapiens.App as App
import Sapiens.Connections as Connections
import Effect.Class (liftEffect)

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runtime <- liftEffect Connections.create
  environment <- liftEffect Platform.environment
  void $ runUI (App.component runtime) environment body
