{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Browser where

import Control.Exception
import Control.Monad
import Data.Aeson
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as M
import qualified Data.ByteString as B
import Data.IORef
import qualified Data.Map.Strict as Map
import Data.Maybe
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Foreign hiding (void, new)
import ObjC
import Domain (BrowserCommand(..), BrowserAction(..))
import System.Directory (doesFileExist)
import System.FilePath

field name (Object value) = fromMaybe Null (M.lookup (K.fromText name) value)
field _ _ = Null
text (String value) = value
text _ = ""
bool (Bool value) = value
bool _ = False
numeric fallback (Number n) = realToFrac n
numeric fallback _ = fallback
array (Array value) = foldr (:) [] value
array _ = []
entries (Object value) = [(K.toText k,v) | (k,v) <- M.toList value]
entries _ = []


data Tab = Tab { tabOwner :: Text, tabID :: Text, tabView :: Obj, tabSequence :: Text, tabDocument :: Maybe Text, tabRequested :: Maybe Text, tabError :: Text, tabAwaiting :: Bool, tabFiles :: Set.Set Text }
data Browser = Browser { shell :: IORef Obj, delegate :: IORef Obj, tabs :: IORef (Map.Map Text Tab), current :: IORef Text, dark :: IORef Bool, origin :: Obj, guest :: Text }

newBrowser address guestScript = do
    sh <- newIORef nullPtr; de <- newIORef nullPtr; ts <- newIORef Map.empty
    ac <- newIORef ""; da <- newIORef False; base <- url address
    pure (Browser sh de ts ac da base guestScript)

key tab = tabOwner tab <> ":" <> tabID tab
save browser tab = modifyIORef' (tabs browser) (Map.insert (key tab) tab)
lookupView browser view = listToMaybe . filter ((== view) . tabView) . Map.elems <$> readIORef (tabs browser)
withTab browser view f = lookupView browser view >>= mapM_ f
emit browser value = readIORef (shell browser) >>= \view -> script view ("window.sapiensBrowserEvent?.(" <> jsonText value <> ")")
fileAddress value = do u <- url value; scheme <- textProp u "scheme"; if scheme == "sapiens-file" then textProp u "path" >>= fileURL >>= \f -> textProp f "absoluteString" else pure value
report browser tab = unless (tabAwaiting tab) $ do
    let view = tabView tab
    address <- if T.null (tabError tab) then obj0 view "URL" >>= \u -> textProp u "absoluteString" else pure ""
    normal <- fileAddress address
    let target = if T.null normal then fromMaybe "" (tabRequested tab) else normal
    unless (T.null target) $ do
        title <- case tabDocument tab of
            Just path -> pure (T.pack (takeFileName (T.unpack path)))
            Nothing -> do value <- textProp view "title"; if T.null value then url target >>= \u -> textProp u "lastPathComponent" else pure value
        back <- booleanValue view "canGoBack"
        forward <- booleanValue view "canGoForward"
        loading <- booleanValue view "isLoading"
        emit browser (object ["owner" .= tabOwner tab, "id" .= tabID tab, "seq" .= tabSequence tab, "url" .= target, "title" .= title, "can_back" .= back, "can_forward" .= forward, "loading" .= loading, "error" .= tabError tab])

createTab browser owner ident = do
    configuration <- new "WKWebViewConfiguration"
    target <- readIORef (delegate browser)
    scheme <- str "sapiens-file"
    void (obj2 configuration "setURLSchemeHandler:forURLScheme:" target scheme)
    user <- obj0 configuration "userContentController"
    source <- str (guest browser)
    scriptObject <- allocated "WKUserScript" >>= \o -> obj3 o "initWithSource:injectionTime:forMainFrameOnly:" source (int 1) (int 1)
    void (obj1 user "addUserScript:" scriptObject)
    view <- allocated "WKWebView" >>= \o -> rectObj o "initWithFrame:configuration:" (0,0,0,0) configuration
    void (obj1 view "setNavigationDelegate:" target)
    void (obj1 view "setUIDelegate:" target)
    void (obj1 view "setAllowsBackForwardNavigationGestures:" (int 1))
    void (obj1 view "setHidden:" (int 1))
    forM_ ["title", "URL"] $ \name -> str name >>= \s -> void (obj4 view "addObserver:forKeyPath:options:context:" target s (int 1) nullPtr)
    void (obj0 scriptObject "release")
    void (obj0 configuration "release")
    pure (Tab owner ident view "" Nothing Nothing "" False Set.empty)

returnFocus browser view = do
    sh <- readIORef (shell browser)
    win <- obj0 view "window"
    responder <- obj0 win "firstResponder"
    viewClass <- cls "NSView"
    isView <- booleanArgument responder "isKindOfClass:" viewClass
    when isView $ do
        descendant <- booleanArgument responder "isDescendantOf:" view
        when (responder == view || descendant) $ void (obj1 win "makeFirstResponder:" sh)

load browser tab address = do
    u <- url address
    scheme <- textProp u "scheme"
    let pending = tab { tabDocument = Nothing, tabRequested = Just address, tabAwaiting = True }
    save browser pending
    if scheme == "file" then do
        ext <- T.toLower <$> textProp u "pathExtension"
        path <- textProp u "path"
        if ext `elem` ["html","htm","pdf","svg","png","jpg","jpeg","gif","webp","heic","mp4","mov","mp3","wav"]
          then obj0 u "URLByDeletingLastPathComponent" >>= \parent -> void (obj2 (tabView tab) "loadFileURL:allowingReadAccessToURL:" u parent)
          else do
            save browser pending { tabDocument = Just path, tabFiles = Set.insert path (tabFiles tab) }
            parts <- new "NSURLComponents"
            setText parts "setScheme:" "sapiens-file"; setText parts "setHost:" "local"; setText parts "setPath:" path
            obj0 parts "URL" >>= \local -> textProp local "absoluteString" >>= loadURL (tabView tab)
            void (obj0 parts "release")
      else when (scheme `elem` ["http","https"] || address == "about:blank") $ loadURL (tabView tab) address

sync browser input = do
    sh <- readIORef (shell browser)
    container <- obj0 sh "superview"
    let workspaces = entries (field "workspaces" input)
        isDark = bool (field "dark" input)
        selected = text (field "owner" input) <> ":" <> text (field "active" input)
        visible = bool (field "visible" input)
    previousDark <- atomicModifyIORef' (dark browser) (\old -> (isDark,old))
    writeIORef (current browser) selected
    existing <- readIORef (tabs browser)
    let specs = [(owner,item) | (owner,workspace) <- workspaces, item <- array (field "tabs" workspace)]
        identity owner item = owner <> ":" <> text (field "id" item)
        alive = Set.fromList [identity owner item | (owner,item) <- specs]
    forM_ specs $ \(owner,item) -> do
        let ident = text (field "id" item); address = text (field "url" item)
            tabKey = identity owner item
            stored = Map.lookup tabKey existing
            shown = visible && tabKey == selected
        when (shown || isJust stored) $ do
            command <- case field "command" item of
                Null -> pure Nothing
                value -> case fromJSON value of
                    Success parsed -> pure (Just parsed)
                    Error reason -> ioError (userError reason)
            let sequence = maybe "" browserSequence command
            tab <- maybe (createTab browser owner ident) pure stored
            when (isNothing stored) $ do
                save browser tab
                void (obj3 container "addSubview:positioned:relativeTo:" (tabView tab) (int 1) sh)
            appearance <- cls "NSAppearance" >>= \c -> str (if isDark then "NSAppearanceNameDarkAqua" else "NSAppearanceNameAqua") >>= obj1 c "appearanceNamed:"
            void (obj1 (tabView tab) "setAppearance:" appearance)
            color <- cls "NSColor" >>= \c -> obj0 c (if isDark then "blackColor" else "whiteColor")
            void (obj1 (tabView tab) "setUnderPageBackgroundColor:" color)
            when (isDark /= previousDark) $ script (tabView tab) ("window.sapiensDocumentTheme?.({dark:" <> (if isDark then "true" else "false") <> "})")
            setNumber (tabView tab) "setPageZoom:" (numeric 1 (field "zoom" item) :: Double)
            when (isNothing stored || tabSequence tab /= sequence) $ do
                let pending = tab { tabSequence = sequence, tabError = "", tabAwaiting = True }
                save browser pending
                if isNothing stored || fmap browserAction command == Just Navigate then load browser pending address else case fmap browserAction command of
                    Just Reload -> do
                        u <- obj0 (tabView tab) "URL"; raw <- textProp u "absoluteString"; normal <- fileAddress raw; actual <- url normal; scheme <- textProp actual "scheme"
                        if scheme == "file" then load browser pending normal else void (obj0 (tabView tab) "reload")
                    Just Back -> history browser pending "canGoBack" "goBack"
                    Just Forward -> history browser pending "canGoForward" "goForward"
                    Just Navigate -> load browser pending address
                    Nothing -> pure ()
            let hidden = not shown
            when hidden $ returnFocus browser (tabView tab)
            void (obj1 (tabView tab) "setHidden:" (flag hidden))
    forM_ (Map.toList existing) $ \(identity,tab) -> unless (Set.member identity alive) $ do
        releaseTab browser identity tab
    let bounds = field "rect" input
        x = numeric 0 (field "x" bounds); y = numeric 0 (field "y" bounds)
        w = numeric 0 (field "width" bounds); h = numeric 0 (field "height" bounds)
    when (visible && w > 0 && h > 0) $ do
        selectedTab <- Map.lookup selected <$> readIORef (tabs browser)
        forM_ selectedTab $ \tab -> do
            flipped <- booleanValue sh "isFlipped"; shHeight <- height sh
            converted <- convertRect container sh (x,if flipped then y else shHeight-y-h,w,h)
            void (rect (tabView tab) "setFrame:" converted)
            void (obj1 (tabView tab) "setHidden:" (int 0))

releaseTab browser identity tab = do
    returnFocus browser (tabView tab)
    target <- readIORef (delegate browser)
    forM_ ["title","URL"] $ \name -> str name >>= \s -> void (obj2 (tabView tab) "removeObserver:forKeyPath:" target s)
    void (obj0 (tabView tab) "stopLoading"); void (obj0 (tabView tab) "removeFromSuperview")
    modifyIORef' (tabs browser) (Map.delete identity)
    void (obj1 (tabView tab) "setNavigationDelegate:" nullPtr)
    void (obj1 (tabView tab) "setUIDelegate:" nullPtr)
    void (obj0 (tabView tab) "release")

unloadIdle browser input = do
    let identity = text (field "owner" input) <> ":" <> text (field "id" input)
        path = text (field "path" input)
    stored <- Map.lookup identity <$> readIORef (tabs browser)
    forM_ stored $ \tab -> when (tabDocument tab == Just path && tabSequence tab == text (field "seq" input)
                                && not (tabAwaiting tab) && T.null (tabError tab)) $ do
        let view = tabView tab
        hidden <- booleanValue view "isHidden"
        loading <- booleanValue view "isLoading"
        back <- booleanValue view "canGoBack"
        forward <- booleanValue view "canGoForward"
        u <- obj0 view "URL"
        scheme <- textProp u "scheme"
        actual <- textProp u "path"
        -- This scheme serves our escaped, read-only text wrapper. Other URLs,
        -- unsaved pages, and tabs with navigation history keep their view.
        when (hidden && not (loading || back || forward) && scheme == "sapiens-file" && actual == path) $ do
            exists <- doesFileExist (T.unpack path)
            when exists $ releaseTab browser identity tab

history browser tab predicate action = do
    let pending = tab { tabDocument = Nothing }
    allowed <- booleanValue (tabView tab) predicate
    save browser pending
    if allowed then void (obj0 (tabView tab) action) else do
        let done = pending { tabAwaiting = False }
        save browser done; report browser done

receive browser message = do
    sh <- readIORef (shell browser)
    sourceView <- obj0 message "webView"
    frame <- obj0 message "frameInfo"
    mainFrame <- booleanValue frame "isMainFrame"
    source <- obj0 frame "request" >>= \r -> obj0 r "URL"
    trusted <- sameOrigin source (origin browser)
    path <- textProp source "path"
    when (sourceView == sh && mainFrame && trusted && path `elem` ["/workspace","/workspace/","/workspace/index.html"]) $ do
        raw <- obj0 message "body"
        dictionary <- cls "NSDictionary"
        valid <- booleanArgument raw "isKindOfClass:" dictionary
        body <- if valid then jsonObject raw else pure Nothing
        forM_ body $ \input -> if bool (field "pickFile" input) then do
            panel <- new "NSOpenPanel"
            void (obj1 panel "setCanChooseDirectories:" (int 0)); void (obj1 panel "setAllowsMultipleSelection:" (int 1)); void (obj1 panel "setShowsHiddenFiles:" (int 1))
            let directory = text (field "directory" input)
            when (T.isPrefixOf "/" directory) $ fileURL directory >>= void . obj1 panel "setDirectoryURL:"
            result <- long panel "runModal"
            when (result == 1) $ obj0 panel "URLs" >>= objects >>= mapM_ (\u -> textProp u "path" >>= \p -> emit browser (object ["owner" .= text (field "owner" input),"file" .= p]))
          else case field "unloadIdle" input of
            request@(Object _) -> unloadIdle browser request
            _ -> sync browser input

sameOrigin left right = and <$> mapM (\property -> (==) <$> textProp left property <*> textProp right property) ["scheme","host"] >>= \same -> do
    lp <- obj0 left "port" >>= \o -> long o "integerValue"; rp <- obj0 right "port" >>= \o -> long o "integerValue"
    pure (same && lp == rp)

data GuestNavigation = RejectControlServer | RejectGuestNavigation | OpenGuestWindow | AcceptGuestNavigation deriving (Eq, Show)
data NavigationFacts = NavigationFacts
 { navigationScheme :: Text, navigationHost :: Text, navigationPath :: Text
 , navigationAtControlPort :: Bool, navigationRequestedFile :: Bool
 , navigationHistory :: Bool, navigationNewWindow :: Bool }

planNavigation :: Set.Set Text -> NavigationFacts -> GuestNavigation
planNavigation files facts
 | navigationAtControlPort facts && navigationHost facts `elem` ["127.0.0.1","localhost","::1"] = RejectControlServer
 | not allowed = RejectGuestNavigation
 | navigationNewWindow facts = OpenGuestWindow
 | otherwise = AcceptGuestNavigation
 where
 scheme=navigationScheme facts
 allowed=scheme `elem` ["http","https","about","blob","data"]
  || (scheme=="file" && (navigationRequestedFile facts || navigationHistory facts))
  || (scheme=="sapiens-file" && Set.member (navigationPath facts) files)

navigation browser view action completion = do
    item <- lookupView browser view
    case item of
        Nothing -> shellNavigation browser action completion
        Just tab -> do
            request <- obj0 action "request"; u <- obj0 request "URL"
            address <- textProp u "absoluteString"; host <- textProp u "host"; scheme <- textProp u "scheme"; path <- textProp u "path"
            port <- obj0 u "port" >>= \p -> long p "integerValue"; basePort <- obj0 (origin browser) "port" >>= \p -> long p "integerValue"
            frame <- obj0 action "targetFrame"; navigationType <- long action "navigationType"
            requestedFile <- maybe (pure False) (\v -> url v >>= \x -> (== "file") <$> textProp x "scheme") (tabRequested tab)
            case planNavigation (tabFiles tab) (NavigationFacts scheme host path (port==basePort) requestedFile (navigationType==2) (frame==nullPtr)) of
                RejectControlServer -> do
                    let denied = tab { tabError = "The Sapiens control server cannot open in a browser tab.", tabAwaiting = False }
                    save browser denied; report browser denied; invokeLong completion 0
                RejectGuestNavigation -> invokeLong completion 0
                OpenGuestWindow -> do
                    when (scheme `elem` ["http","https"]) $ emit browser (object ["owner" .= tabOwner tab,"open" .= address])
                    invokeLong completion 0
                AcceptGuestNavigation -> do
                    mainFrame <- booleanValue frame "isMainFrame"
                    when (mainFrame && scheme `notElem` ["about","blob","data"]) $ fileAddress address >>= \normal -> save browser tab { tabRequested = Just normal }
                    invokeLong completion 1

shellNavigation browser action completion = do
    u <- obj0 action "request" >>= \r -> obj0 r "URL"
    frame <- obj0 action "targetFrame"
    mainFrame <- if frame == nullPtr then pure Nothing else Just <$> booleanValue frame "isMainFrame"
    local <- sameOrigin u (origin browser)
    address <- textProp u "absoluteString"; scheme <- textProp u "scheme"
    let allowed = local || (mainFrame == Just True && address == "about:blank") || (mainFrame == Just False && (address `elem` ["about:blank","about:srcdoc"] || scheme `elem` ["http","https","blob","data"]))
    if allowed then invokeLong completion 1 else do
        when (scheme `elem` ["http","https","mailto"]) $ cls "NSWorkspace" >>= \c -> obj0 c "sharedWorkspace" >>= \w -> void (obj1 w "openURL:" u)
        invokeLong completion 0

finished browser view = withTab browser view $ \tab -> do
    let done = tab { tabAwaiting = False }
    save browser done; report browser done
failed browser view err = do
    code <- long err "code"
    when (code /= -999) $ withTab browser view $ \tab -> do
        reason <- textProp err "localizedDescription"
        let done = tab { tabAwaiting = False, tabError = reason }
        save browser done; report browser done

textDocument content title = "<!doctype html><meta charset=\"utf-8\"><meta name=\"color-scheme\" content=\"light dark\"><meta http-equiv=\"Content-Security-Policy\" content=\"default-src 'none'; style-src 'unsafe-inline'\"><title>" <> escape title <> "</title><style>html{color-scheme:light dark;background:#fff;color:#171717}body{margin:24px}pre{white-space:pre-wrap;overflow-wrap:anywhere;font:15px/1.6 ui-monospace,monospace}@media(prefers-color-scheme:dark){html{background:#000;color:#eee}}</style><pre>" <> escape content <> "</pre>"
  where escape = T.replace "\"" "&quot;" . T.replace ">" "&gt;" . T.replace "<" "&lt;" . T.replace "&" "&amp;"

serveFile browser view task = do
    u <- obj0 task "request" >>= \r -> obj0 r "URL"
    path <- textProp u "path"
    found <- lookupView browser view
    result <- try $ if maybe False (Set.member path . tabFiles) found then do
        content <- TE.decodeUtf8' <$> B.readFile (T.unpack path)
        either throwIO (\t -> pure (TE.encodeUtf8 (textDocument t (T.pack (takeFileName (T.unpack path)))))) content
      else ioError (userError "Permission denied")
    case result of
        Left (_ :: SomeException) -> do
            domain <- str "NSURLErrorDomain"
            e <- cls "NSError" >>= \c -> obj3 c "errorWithDomain:code:userInfo:" domain (int (-1102)) nullPtr
            void (obj1 task "didFailWithError:" e)
        Right content -> do
            mime <- str "text/html"; encoding <- str "utf-8"
            response <- allocated "NSURLResponse" >>= \o -> obj4 o "initWithURL:MIMEType:expectedContentLength:textEncodingName:" u mime (int (B.length content)) encoding
            body <- dataObject content
            void (obj1 task "didReceiveResponse:" response); void (obj1 task "didReceiveData:" body); void (obj0 task "didFinish")
            void (obj0 response "release")

openPanel parameters completion = do
    panel <- new "NSOpenPanel"
    multiple <- obj0 parameters "allowsMultipleSelection"
    void (obj1 panel "setAllowsMultipleSelection:" multiple); void (obj1 panel "setCanChooseDirectories:" (int 0))
    result <- long panel "runModal"
    selected <- if result == 1 then obj0 panel "URLs" else pure nullPtr
    invokeObj completion selected

activeAction browser action delta = do
    identity <- readIORef (current browser)
    tab <- Map.lookup identity <$> readIORef (tabs browser)
    forM_ tab $ \value -> do
        zoom <- number (tabView value) "pageZoom"
        let fields = ["id" .= tabID value] ++ if action == "zoom" then ["factor" .= (if delta == 0 then 1 else min 5 (max 0.25 (zoom+delta)) :: Double)] else []
        emit browser (object ["owner" .= tabOwner value,"action" .= (action :: Text),"fields" .= object fields])
