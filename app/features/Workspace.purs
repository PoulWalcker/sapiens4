module Sapiens.Workspace where

import Prelude
import Data.Argonaut.Core (Json)
import Data.Array as A
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Int as I
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Libs4.Browser as B
import Sapiens.Action (Action(..))
import Sapiens.Data as D
import Sapiens.Html as V
import Sapiens.Model as M
import Sapiens.Navigation as N
import Sapiens.Text as T

native :: M.Model -> Boolean
native m = D.b "native" m.env && D.s "location" (M.activeRecord m) /= "Remote"

bookmarks :: M.Model -> Array Json
bookmarks m = D.arr $ D.at [m.selected,"bookmarks"] m.workspaces

bookmark :: M.Model -> Json -> Json
bookmark m tab = fromMaybe D.null $ A.find (\b -> D.s "url" b == D.s "url" tab && D.s "id" (fromMaybe (fromMaybe D.null $ A.find (( _ == D.s "url" b) <<< D.s "url") (M.tabs m)) $ A.find (\t -> D.s "id" t == D.s "tab_id" b && D.s "url" t == D.s "url" b) (M.tabs m)) == D.s "id" tab) $ bookmarks m

view :: M.Model -> V.Html
view m = V.node "main" [V.cls "workspace",V.id "workspace-panel",V.label "Agent workspace",V.hidden $ not $ A.elem N.Workspace $ M.visiblePanes m]
  [V.div "browser-tabs-wrap"
    [V.node "div" [V.id "browser-tabs",V.cls "browser-tabs",V.label "Workspace tabs"] $ if A.null tabs then [V.div "workspace-tab active" [V.button "tab-select" "New tab" newTab]] else map (tabView false) $ A.sortBy (\a b -> compare (D.valid $ bookmark m b) (D.valid $ bookmark m a)) tabs
    ,V.control [V.id "browser-overflow",V.label $ "All " <> show (A.length tabs) <> " open tabs",V.hidden $ A.length tabs < 2,V.attr "aria-expanded" $ show $ m.menu == Just "browser-tabs"] ("⌄ " <> show (A.length tabs)) (Menu "browser-tabs")
    ,V.control [V.id "add-tab",V.cls "add-tab",V.label "Add workspace tab"] "＋" (newTab)
    ,V.control [V.id "workspace-menu",V.cls "icon-button",V.label "Workspace options",V.attr "aria-expanded" $ show $ m.menu == Just "browser-options"] "···" (Menu "browser-options")]
  ,V.node "form" [V.id "browser-address-form",V.cls "browser-address-bar",V.hidden $ D.valid tab && D.s "url" tab /= "about:blank" && not m.addressOpen,V.submit OpenAddress]
    [V.node "input" [V.id "browser-address",V.label "URL or file path",V.attr "placeholder" "URL or file path",V.attr "autocomplete" "off",V.bool "spellcheck" false,V.value m.address,V.input Address] [],V.textNode "button" [V.attr "type" "submit",V.label "Open address"] "→"]
  ,V.node "div" [V.id "browser-menu",V.cls "browser-menu",V.attr "role" "dialog",V.label "Workspace options",V.hidden $ not $ A.elem m.menu [Just "browser-options",Just "browser-tabs"]]
    [V.node "div" [V.id "browser-options",V.hidden $ m.menu /= Just "browser-options"]
      [V.div "workspace-owner" [T.mention m m.selected,V.span [] "’s browser"]
      ,V.button "" "URL or file path…" EditAddress,V.button "" "Open file…" OpenFile
      ,V.div "browser-menu-row" $ map (\(Tuple action caption) -> V.control [V.disabled $ not (D.valid tab) || (action == "back" && not (D.b "can_back" tab)) || (action == "forward" && not (D.b "can_forward" tab))] (caption) (Browser action D.empty)) [Tuple "back" "← Back",Tuple "forward" "Forward →",Tuple "reload" "Reload"]
      ,V.div "browser-menu-row" [V.span [] "Zoom",zoomButton "Zoom out" "−" (max 0.25 $ zoom-0.1),zoomButton "Reset zoom" (show (I.round $ zoom*100.0) <> "%") 1.0,zoomButton "Zoom in" "＋" (min 5.0 $ zoom+0.1)]
      ,V.div "browser-menu-label" [V.text "Bookmarks"],V.node "div" [V.id "browser-bookmarks"] $ if A.null (bookmarks m) then [V.p "" "No bookmarks yet."] else map bookmarkRow (bookmarks m)]
    ,V.node "div" [V.id "browser-all-tabs",V.hidden $ m.menu /= Just "browser-tabs"] $ [V.div "browser-menu-label" [V.text $ "Open tabs · " <> show (A.length tabs)]] <> map (tabView true) tabs
    ,V.span [V.id "browser-status",V.attr "role" "status"] (D.fallback (if D.b "loading" tab then "Loading…" else "") $ D.s "error" tab)]
  ,V.node "div" [V.id "workspace-content",V.cls "workspace-content"] $ if native m then [V.text $ if D.valid tab then "" else "Open a URL or file to get started."] else
      if D.s "path" tab == "" then [V.text $ if D.valid tab && D.s "url" tab /= "about:blank" then "Open Sapiens4 desktop to browse this web page." else "Open a URL or file to get started."] else [V.textNode "pre" [V.cls "workspace-text",V.label $ D.s "title" tab,V.style $ "font-size:" <> show ((if N.limit m.env < 3 then 12.6 else 13.0) * zoom) <> "px"] (case m.preview of
        M.Ready _ value -> D.s "content" value
        M.Loading _ (Just value) -> D.s "content" value
        M.Failed _ error -> error
        _ -> "Loading…")]
  ,V.node "footer" [V.cls "workspace-footer"] [V.link "https://sapiens4.ai" "sapiens4.ai ↗"]]
  where
  tabs = M.tabs m
  tab = M.activeTab m
  zoom = if D.n "zoom" tab > 0.0 then D.n "zoom" tab else 1.0
  newTab = Browser "open" $ D.json {url: "about:blank"}
  zoomButton caption glyph factor = V.control [V.label caption] (glyph) (Browser "zoom" $ D.json {factor})
  tabView all t =
    let active = D.s "id" t == D.s "id" tab
        saved = bookmark m t
    in V.node "div" [V.cls $ "workspace-tab " <> if active then "active" else "",V.hidden $ not all && N.limit m.env < 3 && not active]
      [V.control [V.cls "tab-star",V.label $ (if D.valid saved then "Unbookmark " else "Bookmark ") <> D.s "title" t,V.attr "aria-pressed" $ show $ D.valid saved,V.disabled $ D.s "url" t == "about:blank"] (if D.valid saved then "★" else "☆") (Browser (if D.valid saved then "unbookmark" else "bookmark") $ D.json {id: if D.valid saved then D.get "id" saved else D.get "id" t})
      ,V.control [V.cls "tab-select",V.attr "aria-pressed" $ show active,V.attr "title" $ D.fallback (D.s "url" t) $ D.s "path" t] (D.fallback "New tab" $ D.s "title" t) (Browser "focus" $ D.json {id: D.get "id" t})
      ,V.control [V.cls "tab-close",V.label $ "Close " <> D.s "title" t] "×" (Browser "close" $ D.json {id: D.get "id" t})]
  bookmarkRow b = let open = A.find (( _ == D.s "url" b) <<< D.s "url") tabs in V.div "bookmark-row"
    [V.button "" ("★ " <> D.s "title" b) $ case open of
      Just t -> Browser "focus" $ D.json {id: D.get "id" t}
      Nothing -> Browser "open" $ D.json {url: D.get "url" b}
    ,V.icon ("Remove " <> D.s "title" b <> " bookmark") "×" $ Browser "unbookmark" $ D.json {id: D.get "id" b}]

sync :: M.Model -> Effect Unit
sync m = when (D.b "native" m.env) do
  box <- B.measure "#workspace-content"
  background <- B.cssProperty ".workspace" "--sapi-white"
  foreground <- B.cssProperty ".workspace" "--sapi-ink"
  caption <- B.cssProperty ".workspace" "--sapi-bg"
  B.nativeMessage $ D.json {onboarding: M.setupRequired m, onboardingStep: D.s "step" m.account, workspaces: if native m then m.workspaces else D.empty, owner: if native m then m.selected else "", active: D.get "id" $ M.activeTab m, visible: not (M.setupRequired m) && native m && A.elem N.Workspace (M.visiblePanes m) && m.dialog == Nothing && m.menu == Nothing, dark: m.theme == "dark", background, foreground, captionBackground: caption, rect:{x:D.n "x" box,y:D.n "y" box,width:D.n "width" box,height:D.n "height" box}}
