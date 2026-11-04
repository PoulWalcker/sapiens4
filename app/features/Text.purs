module Sapiens.Text where

import Prelude
import Data.Array as A
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String as S
import Data.String.CodeUnits as CU
import Data.String.Regex.Flags (unicode)
import Libs4.Browser as B
import Sapiens.Action (Action(..))
import Sapiens.Data as D
import Sapiens.Browser as Browser
import Sapiens.Domain as X
import Sapiens.Html as V
import Sapiens.Model as M

color :: String -> String
color value = if D.matches "^#[a-fA-F0-9]{6}$" value then value else "#74b9ed"

avatar :: X.Entity -> String -> Boolean -> V.Html
avatar a size presence = V.node "span" [V.cls $ "avatar " <> (if M.isGroup a then "group-avatar " else "") <> size,V.style (if M.isGroup a then "background:linear-gradient(90deg," <> bands <> ")" else "--avatar-color:" <> color (a.color)),V.attr "aria-hidden" "true"]
  [V.span [] ("(" <> a.face <> ")"),V.when presence $ V.node "i" [V.cls $ "presence " <> if a.busy then "busy" else ""] []]
  where
  colors = if A.null $ a.stripes then [a.color] else a.stripes
  bands = S.joinWith "," $ A.mapWithIndex (\i c -> color c <> " " <> show (i * 100 / A.length colors) <> "% " <> show ((i + 1) * 100 / A.length colors) <> "%") colors

mention :: M.Model -> String -> V.Html
mention m id = let a = fromMaybe X.emptyEntity $ X.findEntity id m.agents in if a.id /= "" then V.control [V.cls "entity-mention",V.label $ "Open chat with " <> a.name] ("@" <> a.name) (Select id) else V.text $ if id == "you" then "You" else id

groupLabels :: M.Model -> X.Entity -> Boolean -> V.Html
groupLabels m sapi compact = V.node "span" [V.cls $ "group-labels " <> if compact then "compact" else "",V.style $ "--label-count:" <> show (A.length groups) <> ";--label-step:22px;--label-min:22px"] $ map chip groups
  where
  groups = A.filter (\g -> M.isGroup g && not (g.archived) && D.has (sapi.id) (D.json g.members)) m.agents
  chip g = let lead = g.lead == sapi.id
               title = g.name <> if lead then " · Lead" else ""
           in V.node "button" [V.attr "type" "button",V.cls "group-chip",V.style $ "--group-color:" <> color (g.color),V.label title,V.attr "title" title,V.click $ Select $ g.id] [V.span [] ((if compact then S.take 1 $ g.name else g.name) <> if lead then "★" else "")]

mentions :: M.Model -> String -> Array V.Html
mentions m value = if A.null m.agents then [V.text value] else case D.matchWith unicode pattern value of
  Nothing -> [V.text value]
  Just hit -> [V.text hit.before,if A.length candidates == 1 then mention m (fromMaybe "" $ _.id <$> A.head candidates) else V.text hit.full] <> mentions m hit.after
    where
    candidates = A.filter (\a -> a.name == D.replace "^@" "" hit.full) m.agents
  where
  names = A.sortBy (\a b -> compare (CU.length b) (CU.length a)) $ A.nub $ map (_.name) m.agents
  escape = D.replace "[.*+?^${}()|[\\]\\\\]" "\\$&"
  pattern = "(?<![\\p{L}\\p{N}_@.-])@?(?:" <> S.joinWith "|" (map escape names) <> ")(?![\\p{L}\\p{N}_@:-])"

link :: String -> String -> V.Html
link destination caption =
  if S.take 1 destination == "/" || S.take 7 destination == "file://" then V.control [V.cls "entity-mention"] (D.fallback destination caption) (Browser $ Browser.openDestination destination)
  else if A.elem (url.protocol) ["http:","https:"] then V.textNode "a" [V.cls "chat-link",V.attr "href" (url.href),V.attr "title" destination,V.attr "target" "_blank",V.attr "rel" "noopener noreferrer",V.on "click" \event -> Link event (OpenLink $ url.href)] (label <> " ↗")
  else V.text $ D.fallback destination caption
  where
  url = Browser.url "about:blank" destination
  tail = B.decodeURI $ fromMaybe "" $ A.last $ A.filter (_ /= "") $ S.split (S.Pattern "/") $ url.path
  label = D.fallback (url.host <> if tail == "" then "" else " · " <> if CU.length tail > 48 then CU.take 45 tail <> "…" else tail) caption

format :: M.Model -> String -> Array V.Html
format m value = go (collect value) value
  where
  group hit i = fromMaybe "" $ A.index hit.groups i
  collect source = case D.match pattern source of
    Nothing -> []
    Just hit -> (if group hit 1 == "" then [] else [D.fallback (group hit 3) $ group hit 2]) <> collect hit.after
  citation hit = do
    parsed <- D.match "(?:^|\\s)path=\"((?:\\\\.|[^\"\\\\])*)\"" $ group hit 6
    let path = D.replace "\\\\([\\\\_\"])" "$1" $ group parsed 1
    if S.take 1 path == "/" || S.take 7 path == "file://" then Just path else Nothing
  go files source = case D.match pattern source of
    Nothing -> mentions m source
    Just hit -> case citation hit of
      Just path -> mentions m hit.before <> (if A.elem path files then [] else [link path (fromMaybe path $ A.last $ S.split (S.Pattern "/") path)]) <> go (A.snoc files path) hit.after
      Nothing -> mentions m hit.before <> token hit <> go files hit.after
  trimURL url = if CU.drop (CU.length url-1) url == ")" && A.length (S.split (S.Pattern ")") url) > A.length (S.split (S.Pattern "(") url) then trimURL (CU.take (CU.length url-1) url) else url
  pattern = "\\[([^\\]\\n]+)\\]\\((?:<([^>\\n]+)>|((?:[^()\\s]|\\([^()\\s]*\\))+))\\)|https?://[^\\s<>]+|`([^`\\n]+)`|\\*\\*([^*\\n]+)\\*\\*|:codex-file-citation\\{((?:[^{}\"\\n]|\"(?:\\\\.|[^\"\\\\\\n])*\")*)\\}"
  token hit =
    let g i = fromMaybe "" $ A.index hit.groups i
    in if g 1 /= "" then [link (D.fallback (g 3) $ g 2) (g 1)]
    else if g 4 /= "" then [V.textNode "code" [] (g 4)]
    else if g 5 /= "" then [V.node "strong" [] $ mentions m $ g 5]
    else if g 6 /= "" then [V.text hit.full]
    else let url = trimURL $ D.replace "[.,;:!?]+$" "" hit.full in [link url "",V.text $ CU.drop (CU.length url) hit.full]

paragraphs :: M.Model -> String -> Array V.Html
paragraphs m = map (V.node "p" [] <<< A.intercalate [V.node "br" [] []] <<< map (format m) <<< S.split (S.Pattern "\n")) <<< S.split (S.Pattern "\n\n")

time :: String -> String
time = B.formatDate "" $ D.json {hour: "2-digit", minute: "2-digit"}

attachment :: X.Attachment -> V.Html
attachment item = if item.kind == X.LinkAttachment then V.link (item.value) (caption <> " ↗") else V.span [V.cls "attachment-item",V.attr "title" (item.value)] (caption)
  where
  kind = X.attachmentKindName item.kind
  caption = S.toUpper (S.take 1 kind) <> S.drop 1 kind <> " · " <> item.name
