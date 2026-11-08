module Sapiens.Memo where

import Prelude
import Data.Array as A
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String as S
import Data.String.CodeUnits as CU
import Data.Tuple (Tuple(..))
import Foreign.Object as O
import Libs4.Browser as B
import Sapiens.Action (Action(..))
import Sapiens.Data as D
import Sapiens.Browser as Browser
import Sapiens.Domain as X
import Sapiens.Html as V
import Sapiens.Model as M
import Sapiens.Text as T

attribute :: String -> Browser.Node -> String
attribute = Browser.attribute

walk :: Array Browser.Node -> Array Browser.Node
walk nodes = nodes <> A.concatMap (walk <<< Browser.children) nodes

imagePaths :: X.MemoDocument -> Array String
imagePaths document = A.nub $ A.mapMaybe (\n -> let source = attribute "src" n in if Browser.tag n == "img" && source /= "" && not (D.matches "^(?:[a-zA-Z]+:|//|/)" source) then Just source else Nothing) $ walk $ Browser.markup $ document.content

allowedTags :: Array String
allowedTags = S.split (S.Pattern " ") "section article nav div p span h2 h3 h4 blockquote sup sub pre code a img table thead tbody tfoot tr td th caption ul ol li dl dt dd details summary figure figcaption strong em b i small br hr"

emptyEntry :: Browser.Node -> Boolean
emptyEntry node =
  let nodes = walk $ Browser.children node
      codes = A.filter (( _ == "code") <<< Browser.tag) nodes
      placeholder code = A.all (\line -> S.trim line == "" || D.matches "^\\s*[\\w-]+:\\s*(?:[\"']?\\*\\*\\*[\"']?|\\{\\}|\\[\\])\\s*$" line) $ S.split (S.Pattern "\n") $ Browser.text code
  in A.elem (Browser.tag node) ["article","details"] && not (A.null codes) && A.all placeholder codes && not (A.any (\n -> A.elem (Browser.tag n) ["img","a","p","table","blockquote"]) nodes)

facts :: Boolean -> String -> Maybe V.Html
facts about source = if A.null lines || not (A.all (\line -> S.trim line == "" || D.matches "^\\s*(?:[\\w-]+\\s*:|[-#] |[ \\t]+\\S)" line) lines) then Nothing else Just $ V.div "memo-facts" $ renderLines lines
  where
  lines = S.split (S.Pattern "\n") $ S.trim source
  scalar value = D.fallback value $ D.str $ D.parse value
  renderLines remaining = case A.uncons remaining of
    Nothing -> []
    Just {head:line,tail} -> case D.match "^(\\s*)([\\w-]+):\\s*(.*)$" line of
      Nothing -> [V.p "" $ S.trim line] <> renderLines tail
      Just hit ->
        let group i = fromMaybe "" $ A.index hit.groups i
            value = group 3
            indent = CU.length $ group 1
            block = A.takeWhile (\l -> S.trim l == "" || CU.length (D.replace "[^ ].*$" "" l) > indent) tail
            multiline = D.matches "^[|>][+-]?$" value
            body = if multiline then S.joinWith (if S.take 1 value == ">" then " " else "\n") $ map S.trim block else scalar value
        in [V.node "p" [] $ (if about then [] else [V.textNode "strong" [] (D.replace "_" " " (group 2) <> ": ")]) <> [V.text body]] <> renderLines (if multiline then A.drop (A.length block) tail else tail)

highlight :: String -> String -> Array V.Html
highlight language source = case pattern language of
  "" -> [V.text source]
  regex -> case D.match regex source of
    Nothing -> [V.text source]
    Just hit -> [V.text hit.before,V.span [V.cls $ "syntax-" <> kind hit.groups] (hit.full)] <> highlight language hit.after
  where
  kind groups = if fromMaybe "" (A.index groups 1) /= "" then "string" else if fromMaybe "" (A.index groups 2) /= "" then "comment" else if fromMaybe "" (A.index groups 3) /= "" then "keyword" else "literal"
  pattern = case _ of
    "yaml" -> "(\"(?:\\\\.|[^\"\\\\])*\"|'(?:''|[^'])*')|((?<!\\S)#[^\\n]*)|(\\b[A-Za-z_][\\w-]*(?=:\\s|:$))|(\\b(?:true|false|null|\\d+(?:\\.\\d+)?)\\b|[{}[\\]:,|>])"
    "python" -> "(\"(?:\\\\.|[^\"\\\\])*\"|'(?:\\\\.|[^'\\\\])*')|(#[^\\n]*)|(\\b(?:def|class|import|from|as|return|if|elif|else|for|while|in|is|not|and|or|with|try|except|raise|yield|async|await|pass|lambda)\\b)|(\\b(?:True|False|None|\\d+(?:\\.\\d+)?)\\b)"
    "haskell" -> "(\"(?:\\\\.|[^\"\\\\])*\")|(--[^\\n]*|\\{-[\\s\\S]*?-\\})|(\\b(?:data|type|newtype|where|let|in|case|of|if|then|else|do|module|import|deriving)\\b|::|->|<-|=>)|(\\b(?:[A-Z]\\w*|\\d+)\\b)"
    "html" -> "(\"[^\"]*\"|'[^']*')|(<!--[\\s\\S]*?-->)|(<\\/?[\\w-]+|/?>)|(\\b[\\w-]+(?=\\s*=))"
    "svg" -> pattern "html"
    _ -> ""

view :: M.Model -> V.Html
view m = case document of
  M.Failed _ error -> V.div "" [V.p "empty" error,V.button "button" "Try again" $ RetryDocument key]
  _ -> case M.valueOf document of
    Nothing -> if info.revision == "missing" then V.p "empty" "No shared Memo yet." else V.text "Loading Memo…"
    Just (X.MemoDocument row) -> case wiki m row info of
      Left error -> V.p "empty" error
      Right html -> html
    Just _ -> V.p "empty" "Invalid Memo response"
  where
  key = "memo:" <> m.selected
  document = fromMaybe M.NotAsked $ O.lookup key m.documents
  info = M.notes m

wiki :: M.Model -> X.MemoDocument -> X.Notes -> Either String V.Html
wiki m document info =
  if A.length sections /= 3 || map (Browser.tag) sections /= ["section","section","section"] || map (attribute "id") sections /= ["about","map","content"] then Left "Notes.html requires exactly three sections: about, map, content."
  else if A.length ids /= A.length (A.nub ids) then Left "Notes.html section anchors must be unique."
  else Right $ V.div "notes-wiki" [header,about,toc,content]
  where
  profile = M.selected m
  sections = A.filter (( _ /= "") <<< Browser.tag) $ Browser.markup $ document.content
  nodes = walk sections
  ids = A.filter (_ /= "") $ map (attribute "id") nodes
  section id = fromMaybe (Browser.Text "") $ A.find (( _ == id) <<< attribute "id") sections
  metadata = X.withMetadata info document.metadata
  emptyNodes = A.filter emptyEntry $ walk [section "content"]
  emptyIds = A.filter (_ /= "") $ map (attribute "id") $ walk emptyNodes
  headingName id = let link = A.find (( _ == "#" <> id) <<< attribute "href") $ walk [section "map"] in D.fallback (D.replace "[-_]" " " id) $ Browser.text $ fromMaybe (Browser.Text "") link
  language node = fromMaybe "text" $ A.find (\lang -> S.contains (S.Pattern $ "language-" <> lang) $ attribute "class" node) ["yaml","python","html","svg","haskell"]
  copy isAbout node
    | Browser.tag node == "" = V.text $ Browser.text node
    | emptyEntry node = V.empty
    | Browser.tag node == "svg" = V.node "img" [V.attr "src" $ "data:image/svg+xml," <> B.encodeURI (Browser.source node),V.attr "alt" $ D.fallback "Diagram" $ attribute "aria-label" node] []
    | not (A.elem (Browser.tag node) allowedTags) = V.empty
    | otherwise = renderNode isAbout node
  renderNode isAbout node =
    let tag = Browser.tag node
        id = attribute "id" node
        attrs = (if D.matches "^[a-zA-Z][\\w-]*$" id then [V.id $ "note-" <> id] else []) <> [V.cls $ S.joinWith " " $ A.filter (\cls -> A.elem cls ["citation","permalink","references","origin","backref","wiki-table","file-list","stub-note","draft-note"]) $ S.split (S.Pattern " ") $ attribute "class" node] <> A.mapMaybe (\key -> if D.matches "^[1-9]\\d?$" $ attribute key node then Just $ V.attr key $ attribute key node else Nothing) ["colspan","rowspan"]
        children = Browser.children node
        heading = if tag == "article" && not (A.any (( _ == "h2") <<< Browser.tag) children) then [V.textNode "h2" [] (headingName id)] else []
    in case tag of
      "code" -> V.node "code" (attrs <> [V.attr "data-language" $ language node]) $ highlight (language node) $ Browser.text node
      "pre" -> case A.head $ A.filter (( _ == "code") <<< Browser.tag) children of
        Just code | language code == "yaml" -> fromMaybe (V.node "pre" (attrs <> [V.attr "data-language" "yaml"]) $ map (copy isAbout) children) $ facts isAbout $ Browser.text code
        Just code -> V.node "pre" (attrs <> [V.attr "data-language" $ language code]) $ map (copy isAbout) children
        _ -> V.node "pre" attrs $ map (copy isAbout) children
      "a" ->
        let href = attribute "href" node
            url = Browser.url ("file://" <> document.path) href
        in if D.matches "^#[a-zA-Z][\\w-]*$" href then V.node "a" (attrs <> [V.attr "href" $ "#note-" <> S.drop 1 href,V.on "click" \e -> Link e (NoteAnchor $ "note-" <> S.drop 1 href)]) $ map (copy isAbout) children
        else if url.protocol == "file:" then V.node "a" (attrs <> [V.attr "href" "#",V.on "click" \e -> Link e $ Browser $ Browser.Open (Browser.FilePath $ B.decodeURI url.path) Nothing]) $ map (copy isAbout) children
        else if A.elem (url.protocol) ["http:","https:"] then V.node "a" (attrs <> [V.cls "chat-link",V.attr "href" $ url.href,V.attr "target" "_blank",V.attr "rel" "noopener noreferrer",V.on "click" \e -> Link e $ OpenLink $ url.href]) $ map (copy isAbout) children
        else V.node "span" attrs $ map (copy isAbout) children
      "img" ->
        let src = attribute "src" node
            safe = if D.matches "^data:image/(?:png|jpeg|gif|webp|svg\\+xml)[;,]" src then src else fromMaybe "" $ O.lookup (m.selected <> ":" <> src) m.imageUrls
        in V.node "img" (attrs <> [V.attr "alt" $ attribute "alt" node] <> if safe == "" then [] else [V.attr "src" safe]) []
      "details" -> V.node "div" attrs $ map (copy isAbout) children
      "summary" -> V.node "h3" attrs [V.text $ Browser.text node]
      _ -> V.node tag attrs $ heading <> map (copy isAbout) children
  displayName = D.fallback (D.fallback "Sapi" $ profile.name) $ attribute "data-memo-name" $ section "about"
  history label date author = V.node "span" [] [V.text $ label <> " ",V.textNode "time" [V.attr "datetime" date] (D.fallback "not recorded" $ B.formatDate "en-GB" (D.json {day: "numeric", month: "short", year: "numeric", hour: "2-digit", minute: "2-digit", timeZoneName: "short"}) date),V.text " by ",V.span [V.cls "memo-actor"] (D.fallback "unrecorded actor" author)]
  header = V.node "header" [V.cls "memo-header"] [V.node "p" [V.cls "memo-meta"] [history "Last modified at" (metadata.modified_at) (metadata.modified_by),history "Created at" (metadata.created_at) (metadata.created_by)]]
  turns = A.length $ A.filter (\t -> (if M.isGroup profile then t.group else t.agent) == m.selected) $ m.snapshot.turns
  infobox = V.node "aside" [V.cls "memo-infobox",V.label $ displayName <> " at a glance"]
    [V.div "memo-identity" [V.span [V.cls "memo-avatar",V.style $ "background-color:" <> T.color (profile.color)] ("(" <> D.fallback "◠‿◠" (profile.face) <> ")"),V.textNode "strong" [] (displayName)]
    ,V.node "table" [] $ A.mapMaybe (\(Tuple key value) -> if value == "" then Nothing else Just $ V.node "tr" [] [V.textNode "th" [] (key),V.textNode "td" [V.cls $ if S.contains (S.Pattern " ID") key then "memo-id" else ""] (value)]) [Tuple "Full name" (if displayName == profile.name then "" else profile.name),Tuple "Role" (D.fallback (profile.role) $ attribute "data-memo-role" $ section "about"),Tuple "Assists" (attribute "data-memo-assists" $ section "about"),Tuple "History" (show turns <> " recorded turns"),Tuple (if M.isGroup profile then "Group ID" else "Sapi ID") m.selected]
    ,V.div "memo-workspace" [V.textNode "strong" [] "Workspace",V.textNode "code" [] (D.fallback (D.replace "/[^/]+$" "" $ document.path) $ metadata.workspace)]]
  about = V.node "section" [V.id "note-about",V.cls "memo-lead"] $ [infobox] <> map (copy true) (Browser.children $ section "about")
  entries = A.nubBy (\a b -> compare a.id b.id) $ A.mapMaybe entry $ walk [section "content"]
  entry n =
    let tag = Browser.tag n
        id = attribute "id" n
    in if A.elem tag ["article","h2"] && id /= "" && not (A.elem id emptyIds) then Just {id,text:if tag == "article" then headingName id else Browser.text n} else Nothing
  toc = V.when (A.length entries > 1) $ V.node "nav" [V.id "note-map",V.cls "memo-toc",V.label "Contents"] [V.textNode "strong" [] "Contents",V.node "ol" [] $ map (\item -> V.node "li" [] [V.textNode "a" [V.attr "href" $ "#note-" <> item.id,V.on "click" \e -> Link e $ NoteAnchor $ "note-" <> item.id] (item.text)]) entries]
  populated = A.filter (not <<< emptyEntry) $ Browser.children $ section "content"
  content = V.node "section" [V.id "note-content"] $ (if A.any (\n -> A.elem (Browser.tag n) ["article","h2","h3","p","pre","table","img"]) (walk populated) then [] else [V.p "memo-stub" "This Memo is a stub. No populated memory entries have been saved yet."]) <> map (copy false) populated <> [V.when (not $ A.null emptyIds) $ V.node "aside" [V.cls "memo-unrecorded"] $ [V.text "Unrecorded fields have no saved content yet."] <> map (\id -> V.node "span" [V.id $ "note-" <> id] []) emptyIds]
