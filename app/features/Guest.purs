module Sapiens.Guest where

import Prelude
import Sapiens.Platform as Platform
import Data.Argonaut.Core (Json)
import Data.Array as A
import Effect (Effect)
import Libs4.Browser as B
import Sapiens.Data as D
import Sapiens.Environment (Environment)
import Sapiens.Browser as Browser

applyTheme :: Json -> Effect Json
applyTheme settings = do
  env <- Platform.environment
  let theme = Browser.themeSettings env.dark settings
      managed = managedDocument env
  when managed $ B.styleSheet "sapiens-document-theme" $ style theme
  pure $ D.bool managed

managedDocument :: Environment -> Boolean
managedDocument env = env.href == "about:blank" || A.elem env.contentType ["text/plain","application/json","application/xml","text/xml"]

style :: Browser.ThemeSettings -> String
style theme = "html,body{background:" <> theme.background <> ";color:" <> theme.foreground <> ";color-scheme:" <> (if theme.dark then "dark" else "light") <> "}pre{white-space:pre-wrap;overflow-wrap:anywhere}"

main :: Effect Unit
main = do
  B.callbackResult "sapiensDocumentTheme" applyTheme
  void $ applyTheme D.empty
