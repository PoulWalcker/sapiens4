module Sapiens.Workspace where

import Prelude
import Data.Array as A
import Data.Maybe (Maybe(..))
import Data.Int as I
import Data.Tuple (Tuple(..))
import Foreign.Object as O
import Effect (Effect)
import Libs4.Browser as B
import Sapiens.Action (Action(..))
import Sapiens.Data as D
import Sapiens.Browser as Browser
import Sapiens.Platform as Platform
import Sapiens.Domain as X
import Sapiens.Html as V
import Sapiens.Model as M
import Sapiens.Navigation as N
import Sapiens.Text as T

native :: M.Model -> Boolean
native = M.nativeWorkspace

bookmarks :: M.Model -> Array X.Bookmark
bookmarks m = (M.workspace m).bookmarks

bookmark :: M.Model -> X.Tab -> Maybe X.Bookmark
bookmark m tab = A.find (\b -> b.url == tab.url && matching b == Just tab.id) $ bookmarks m
  where
  matching b = _.id <$> case A.find (\t -> t.id == b.tab_id && t.url == b.url) (M.tabs m) of
    Just t -> Just t
    Nothing -> A.find ((b.url == _) <<< _.url) (M.tabs m)

view :: M.Model -> V.Html
view m = V.node "main" [V.cls "workspace",V.id "workspace-panel",V.label "Agent workspace",V.hidden $ not $ A.elem N.Workspace $ M.visiblePanes m]
  [V.div "browser-tabs-wrap"
    [V.node "div" [V.id "browser-tabs",V.cls "browser-tabs",V.label "Workspace tabs"] $ if A.null tabs then [V.div "workspace-tab active" [V.button "tab-select" "New tab" newTab]] else map (tabView false) $ A.sortBy (\a b -> compare (bookmark m b /= Nothing) (bookmark m a /= Nothing)) tabs
    ,V.control [V.id "browser-overflow",V.label $ "All " <> show (A.length tabs) <> " open tabs",V.hidden $ A.length tabs < 2,V.attr "aria-expanded" $ show $ m.menu == Just "browser-tabs"] ("⌄ " <> show (A.length tabs)) (Menu "browser-tabs")
    ,V.control [V.id "add-tab",V.cls "add-tab",V.label "Add workspace tab"] "＋" (newTab)
    ,V.control [V.id "workspace-menu",V.cls "icon-button",V.label "Workspace options",V.attr "aria-expanded" $ show $ m.menu == Just "browser-options"] "···" (Menu "browser-options")]
  ,V.node "form" [V.id "browser-address-form",V.cls "browser-address-bar",V.hidden $ tab.id /= "" && tab.url /= "about:blank" && not m.addressOpen,V.submit OpenAddress]
    [V.node "input" [V.id "browser-address",V.label "URL or file path",V.attr "placeholder" "URL or file path",V.attr "autocomplete" "off",V.bool "spellcheck" false,V.value m.address,V.input Address] [],V.textNode "button" [V.attr "type" "submit",V.label "Open address"] "→"]
  ,V.node "div" [V.id "browser-menu",V.cls "browser-menu",V.attr "role" "dialog",V.label "Workspace options",V.hidden $ not $ A.elem m.menu [Just "browser-options",Just "browser-tabs"]]
    [V.node "div" [V.id "browser-options",V.hidden $ m.menu /= Just "browser-options"]
      [V.div "workspace-owner" [T.mention m m.selected,V.span [] "’s browser"]
      ,V.button "" "URL or file path…" EditAddress,V.button "" "Open file…" OpenFile
      ,V.div "browser-menu-row" $ map (\(Tuple action caption) -> V.control [V.disabled $ not (tab.id /= "") || (action == Browser.GoBack && not (tab.can_back)) || (action == Browser.GoForward && not (tab.can_forward))] (caption) (Browser action)) [Tuple Browser.GoBack "← Back",Tuple Browser.GoForward "Forward →",Tuple Browser.Reload "Reload"]
      ,V.div "browser-menu-row" [V.span [] "Zoom",zoomButton "Zoom out" "−" (max 0.25 $ zoom-0.1),zoomButton "Reset zoom" (show (I.round $ zoom*100.0) <> "%") 1.0,zoomButton "Zoom in" "＋" (min 5.0 $ zoom+0.1)]
      ,V.div "browser-menu-label" [V.text "Bookmarks"],V.node "div" [V.id "browser-bookmarks"] $ if A.null (bookmarks m) then [V.p "" "No bookmarks yet."] else map bookmarkRow (bookmarks m)]
    ,V.node "div" [V.id "browser-all-tabs",V.hidden $ m.menu /= Just "browser-tabs"] $ [V.div "browser-menu-label" [V.text $ "Open tabs · " <> show (A.length tabs)]] <> map (tabView true) tabs
    ,V.span [V.id "browser-status",V.attr "role" "status"] (D.fallback (if tab.loading then "Loading…" else "") $ tab.error)]
  ,V.node "div" [V.id "workspace-content",V.cls "workspace-content"] $ if native m then [V.text $ if tab.id /= "" then "" else "Open a URL or file to get started."] else
      if tab.path == "" then [V.text $ if tab.id /= "" && tab.url /= "about:blank" then "Open Sapiens4 desktop to browse this web page." else "Open a URL or file to get started."] else [V.textNode "pre" [V.cls "workspace-text",V.label $ tab.title,V.style $ "font-size:" <> show ((if N.limit m.env < 3 then 12.6 else 13.0) * zoom) <> "px"] (case m.preview of
        M.Ready _ value -> value
        M.Loading _ (Just value) -> value
        M.Failed _ error -> M.previewError error
        _ -> "Loading…")]
  ,V.node "footer" [V.cls "workspace-footer"] [V.link "https://sapiens4.ai" "sapiens4.ai ↗"]]
  where
  tabs = M.tabs m
  tab = M.activeTab m
  zoom = if tab.zoom > 0.0 then tab.zoom else 1.0
  newTab = Browser $ Browser.Open (Browser.WebAddress "about:blank") Nothing
  zoomButton caption glyph factor = V.control [V.label caption] (glyph) (Browser $ Browser.Zoom factor)
  tabView all t =
    let active = t.id == tab.id
        saved = bookmark m t
    in V.node "div" [V.cls $ "workspace-tab " <> if active then "active" else "",V.hidden $ not all && N.limit m.env < 3 && not active]
      [V.control [V.cls "tab-star",V.label $ (if saved /= Nothing then "Unbookmark " else "Bookmark ") <> t.title,V.attr "aria-pressed" $ show $ saved /= Nothing,V.disabled $ t.url == "about:blank"] (if saved /= Nothing then "★" else "☆") (Browser $ case saved of
        Just item -> Browser.RemoveBookmark item.id
        Nothing -> Browser.SaveBookmark t.id)
      ,V.control [V.cls "tab-select",V.attr "aria-pressed" $ show active,V.attr "title" $ D.fallback (t.url) $ t.path] (D.fallback "New tab" $ t.title) (Browser $ Browser.FocusTab t.id)
      ,V.control [V.cls "tab-close",V.label $ "Close " <> t.title] "×" (Browser $ Browser.CloseTab t.id)]
  bookmarkRow b = let open = A.find (( _ == b.url) <<< _.url) tabs in V.div "bookmark-row"
    [V.button "" ("★ " <> b.title) $ case open of
      Just t -> Browser $ Browser.FocusTab t.id
      Nothing -> Browser $ Browser.Open (Browser.WebAddress b.url) Nothing
    ,V.icon ("Remove " <> b.title <> " bookmark") "×" $ Browser $ Browser.RemoveBookmark b.id]

sync :: M.Model -> Effect Unit
sync m = when (m.env.native) do
  box <- Platform.measure "#workspace-content"
  background <- B.cssProperty ".workspace" "--sapi-white"
  foreground <- B.cssProperty ".workspace" "--sapi-ink"
  caption <- B.cssProperty ".workspace" "--sapi-bg"
  B.nativeMessage $ D.json {workspaces: if native m then m.workspaces else O.empty, owner: if native m then m.selected else "", active: D.json (M.activeTab m).id, visible: native m && A.elem N.Workspace (M.visiblePanes m) && m.dialog == Nothing && m.menu == Nothing, dark: m.theme == "dark", background, foreground, captionBackground: caption, rect:{x:box.x,y:box.y,width:box.width,height:box.height}}
