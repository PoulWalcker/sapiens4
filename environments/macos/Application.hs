{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Application where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception
import Control.Monad
import Data.Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Aeson.Key as Key
import qualified Crypto.Hash.SHA256 as SHA256
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as L
import DataRoot
import qualified Data.Text.IO as TIO
import Data.IORef
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Foreign hiding (void, new)
import Network.HTTP.Client hiding (method)
import Network.URI (parseURI, uriScheme, uriAuthority, uriRegName, uriUserInfo)
import ObjC
import Browser
import System.Directory
import System.Environment
import System.FilePath
import System.IO
import System.Process hiding (shell)
import System.Timeout
import System.Exit (ExitCode(..))
import System.Posix.Process (getProcessID)
import Data.List (stripPrefix)
import Numeric (showHex)


data Application = Application { native :: Obj, browser :: IORef Browser, windowRef :: IORef Obj, pending :: IORef [IO ()], fixture :: Bool, child :: IORef (Maybe ServerProcess), address :: IORef Text, startup :: IORef (Maybe (Async ())), starting :: IORef Bool, localLoaded :: IORef Bool, runtime :: FilePath, selectedRoot :: IORef (Maybe FilePath), recoveryError :: IORef (Maybe Text), clientPreferences :: Obj }

data ServerProcess = ServerProcess { serverHandle :: ProcessHandle, serverInput :: Handle, serverOutput :: Handle, serverErrors :: Async (), serverDrain :: Maybe (Async ()), serverDirectory :: FilePath }

post :: Application -> IO () -> IO ()
post application action = atomicModifyIORef' (pending application) (\actions -> (actions ++ [action],()))

-- Only the child started by this application is trusted. Never attach to a
-- fixed port, which might belong to another installation or data directory.
check :: Application -> IO ()
check application = do
    current <- readIORef (child application)
    case current of
        Nothing -> pure ()
        Just server -> do
            alive <- isNothing <$> getProcessExitCode (serverHandle server)
            if alive then do
                b <- readIORef (browser application)
                view <- readIORef (shell b)
                readyAddress <- readIORef (address application)
                unless (T.null readyAddress) (loadURL view readyAddress)
              else alert "The application server stopped. Quit and reopen Sapiens4."

startServer :: Application -> StorageChoice -> FilePath -> FilePath -> IO (ServerProcess, Text)
startServer application choice runtime destination = do
    configRoot <- configDirectory
    let binary = runtime </> ".build/bin/sapiens4"
    present <- doesFileExist binary
    unless present $ ioError (userError "The application is incomplete. Extract the complete Sapiens4 Development package.")
    createDirectoryIfMissing True configRoot
    environment <- getEnvironment
    pid <- getProcessID
    let overrides = [("SAPIENS_REPOSITORY",runtime),("SAPIENS_CONFIG_DIR",configRoot),("SAPIENS_DATA_DIR",destination),("SAPIENS_DESKTOP_PID",show pid),("SAPIENS_STORAGE_MODE",if choice==StartNew then "new" else "existing")]
    let command = (proc binary ["--port","0","--data-dir",destination,"--desktop"])
            { cwd = Just runtime, env = Just (overrides ++ filter ((`notElem` map fst overrides) . fst) environment)
            , std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe, create_group = True }
    launchServer application destination command

launchServer :: Application -> FilePath -> CreateProcess -> IO (ServerProcess, Text)
launchServer application destination command = mask $ \restore -> do
    (Just input, Just output, Just errors, process) <- createProcess command
    let close h = hClose h `catch` (\(_ :: IOException) -> pure ())
    reader <- async $ (do
        hSetEncoding errors utf8
        hSetNewlineMode errors universalNewlineMode
        withFile (destination </> "desktop-server.log") AppendMode $ \logFile ->
            (forever $ do
                line <- hGetLine errors
                hPutStrLn logFile line
                hFlush logFile) `catch` (\(_ :: IOException) -> pure ())) `finally` close errors
    let server = ServerProcess process input output reader Nothing destination
        await = do
            ended <- hIsEOF output
            when ended $ do
                threadDelay 100000
                ioError (userError startupMessage)
            line <- hGetLine output
            case stripPrefix "Sapiens4: " line >>= parseURI of
                Just uri | uriScheme uri == "http:", Just auth <- uriAuthority uri,
                           uriRegName auth == "127.0.0.1", null (uriUserInfo auth) -> pure (T.pack (show uri))
                _ -> await
    writeIORef (child application) (Just server)
    mapM_ (\h -> hSetEncoding h utf8 >> hSetNewlineMode h universalNewlineMode) [input,output] `onException` stopOwnedServer application
    ready <- restore (timeout 35000000 await >>= maybe (ioError (userError "The application server did not start within 35 seconds. See desktop-server.log in the application data folder.")) pure)
        `onException` stopOwnedServer application
    -- The address is read from our private stdout pipe, never discovered by port.
    drain <- async $ (forever (void (hGetLine output))) `catch` (\(_ :: IOException) -> close output)
    let running=server {serverDrain=Just drain}
    writeIORef (child application) (Just running)
    pure (running, ready)

stopServer :: ServerProcess -> IO ()
stopServer server = stop `finally` (do
    cancel (serverErrors server)
    mapM_ cancel (serverDrain server)
    mapM_ (\h -> hClose h `catch` (\(_ :: IOException) -> pure ())) [serverInput server,serverOutput server])
    where
        process=serverHandle server
        input=serverInput server
        stop = do
            (hPutStrLn input "stop" >> hFlush input >> hClose input) `catch` (\(_ :: IOException) -> pure ())
            ended <- timeout 10000000 (waitForProcess process)
            when (isNothing ended) $ do
                terminateProcess process
                confirmed <- timeout 5000000 (waitForProcess process)
                when (isNothing confirmed) (ioError (userError "The application server could not be stopped."))

alert :: Text -> IO ()
alert message = do
    panel <- new "NSAlert"
    setText panel "setMessageText:" "Sapiens4 could not open"
    setText panel "setInformativeText:" message
    void (long panel "runModal")

reload :: Application -> IO ()
reload application = if fixture application then readIORef (browser application) >>= readIORef . shell >>= void . (`obj0` "reload") else do
    connected <- not . T.null <$> readIORef (address application)
    if connected then check application else loadBootstrap application

showWindow :: Application -> IO ()
showWindow application = do win <- readIORef (windowRef application); void (obj1 win "deminiaturize:" nullPtr); void (obj1 win "makeKeyAndOrderFront:" nullPtr); void (obj1 (native application) "activateIgnoringOtherApps:" (int 1))

installMethods :: Application -> IO Obj
installMethods application = do
    c <- subclass "SapiensHaskellDesktop"
    let withBrowser action = readIORef (browser application) >>= action
        on0 name action = wrap0 (\_ _ -> action) >>= method c name "v@:"
        on1 name action = wrap1 (\_ _ value -> action value) >>= method c name "v@:@"
        on2 name action = wrap2 (\_ _ a d -> action a d) >>= method c name "v@:@@"
        on3 name action = wrap3 (\_ _ a d e -> action a d e) >>= method c name "v@:@@@"
        on4 name action = wrap4 (\_ _ a d e f -> action a d e f) >>= method c name "v@:@@@@"
    on1 "tick:" $ \_ -> atomicModifyIORef' (pending application) (\items -> ([],items)) >>= sequence_
    on1 "applicationDidFinishLaunching:" (\_ -> showWindow application)
    on0 "reloadWorkspace" (reload application)
    on0 "quit" (void (obj1 (native application) "terminate:" nullPtr))
    on1 "applicationWillTerminate:" (\_ -> closeApplication application)
    on0 "reloadTab" (withBrowser $ \b -> activeAction b "reload" 0)
    on0 "zoomIn" (withBrowser $ \b -> activeAction b "zoom" 0.1)
    on0 "restoreWorkspace" $ do
        win <- readIORef (windowRef application)
        void (obj1 win "deminiaturize:" nullPtr)
        showWindow application
    on0 "zoomOut" (withBrowser $ \b -> activeAction b "zoom" (-0.1))
    on0 "resetZoom" (withBrowser $ \b -> activeAction b "zoom" 0)
    wrapBool2 (\_ _ _ _ -> showWindow application >> pure 1) >>= method c "applicationShouldHandleReopen:hasVisibleWindows:" "B@:@B"
    wrapBool1 (\_ _ _ -> do
        busy <- readIORef (starting application)
        connected <- not . T.null <$> readIORef (address application)
        pure (if not (fixture application) && (busy || not connected) then 1 else 0)) >>= method c "applicationShouldTerminateAfterLastWindowClosed:" "B@:@"
    on2 "userContentController:didReceiveScriptMessage:" (\_ message -> receiveMessage application message)
    on3 "webView:decidePolicyForNavigationAction:decisionHandler:" (\view action completion -> withBrowser $ \b -> do
        connected <- not . T.null <$> readIORef (address application)
        if connected || fixture application then navigation b view action completion else do
            u <- obj0 action "request" >>= (`obj0` "URL")
            path <- textProp u "path"
            scheme <- textProp u "scheme"
            invokeLong completion (if scheme=="file" && path==T.pack (bootstrapPath application) then 1 else 0))
    on2 "webView:didCommitNavigation:" (\view _ -> withBrowser $ \b -> finished b view)
    on2 "webView:didFinishNavigation:" $ \view _ -> do
        withBrowser $ \b -> finished b view
        unless (fixture application) $ do
            path <- obj0 view "URL" >>= (\u -> textProp u "path")
            when (path==T.pack (bootstrapPath application)) $ do
                loaded <- atomicModifyIORef' (localLoaded application) (\old -> (True,old))
                problem <- atomicModifyIORef' (recoveryError application) (\old -> (Nothing,old))
                writeIORef (starting application) False
                forM_ problem (showStorageError application)
                unless (loaded || isJust problem) $ do
                    selection <- try savedDataDirectory
                    case selection of
                        Left (e :: IOException) -> showStorageError application (T.pack (storageMessage e))
                        Right Nothing -> pure ()
                        Right (Just directory) -> startSelected application AttachExisting directory
            sh <- withBrowser (readIORef . shell)
            selection <- readIORef (selectedRoot application)
            readyAddress <- readIORef (address application)
            trusted <- obj0 view "URL" >>= (\u -> withBrowser (sameOrigin u . origin))
            when (view==sh && trusted && path `elem` ["/workspace","/workspace/","/workspace/index.html"] && not (T.null readyAddress)) $ forM_ selection $ \directory -> do
                committed <- try (saveDataDirectory directory)
                case committed of
                    Left (e :: SomeException) -> recoverStartup application ("Could not save your storage selection: " <> T.pack (storageMessage e))
                    Right () -> do
                        atomicModifyIORef' (startup application) (\old -> (Nothing,old)) >>= mapM_ cancel
                        writeIORef (selectedRoot application) Nothing
                        writeIORef (starting application) False
                        workspaceWindow application
    let navigationFailed view err = withBrowser $ \b -> do
            sh <- readIORef (shell b)
            busy <- readIORef (starting application)
            code <- long err "code"
            connected <- not . T.null <$> readIORef (address application)
            when (code /= -999) $
                if view==sh && busy && connected then recoverStartup application "We couldn't open your workspace. Your saved storage selection is unchanged. Choose a folder to try again."
                else if view==sh && not connected then do
                    writeIORef (starting application) False
                    alert "We couldn't open the storage screen. Choose View → Reload App to try again. If the problem continues, reinstall the complete application. Your saved data is unchanged."
                else failed b view err
    on3 "webView:didFailNavigation:withError:" (\view _ err -> navigationFailed view err)
    on3 "webView:didFailProvisionalNavigation:withError:" (\view _ err -> navigationFailed view err)
    on1 "webViewWebContentProcessDidTerminate:" $ \view -> withBrowser $ \b -> do
        item <- lookupView b view
        case item of
            Nothing -> reload application
            Just tab -> do
                let updated = tab { tabError = "Page process stopped. Reload to continue." }
                save b updated; report b updated
    on4 "observeValueForKeyPath:ofObject:change:context:" (\_ view _ _ -> withBrowser $ \b -> withTab b view (report b))
    on4 "webView:runOpenPanelWithParameters:initiatedByFrame:completionHandler:" (\_ parameters _ completion -> openPanel parameters completion)
    on2 "webView:startURLSchemeTask:" (\view task -> withBrowser $ \b -> serveFile b view task)
    on2 "webView:stopURLSchemeTask:" (\_ _ -> pure ())
    wrapObj4 (\_ _ view _ action _ -> withBrowser $ \b -> do
        sh <- readIORef (shell b)
        when (view == sh) $ do
            request <- obj0 action "request"; u <- obj0 request "URL"
            local <- sameOrigin u (origin b)
            when local $ void (obj1 view "loadRequest:" request)
        pure nullPtr) >>= method c "webView:createWebViewWithConfiguration:forNavigationAction:windowFeatures:" "@@:@@@@"
    registerClass c
    target <- obj0 c "new"
    withBrowser $ \b -> writeIORef (delegate b) target
    pure target

menuItem :: Obj -> Obj -> Text -> String -> Text -> IO ()
menuItem menu target title action keyEquivalent = do
    titleObject <- str title; actionSelector <- sel action; keyObject <- str keyEquivalent
    item <- allocated "NSMenuItem" >>= \o -> obj3 o "initWithTitle:action:keyEquivalent:" titleObject actionSelector keyObject
    void (obj1 item "setTarget:" target); void (obj1 menu "addItem:" item)

subMenu :: Obj -> Text -> IO Obj
subMenu root title = do
    item <- new "NSMenuItem"; menu <- allocated "NSMenu" >>= \o -> str title >>= obj1 o "initWithTitle:"
    void (obj1 root "addItem:" item); void (obj1 item "setSubmenu:" menu)
    pure menu

menus :: Application -> Obj -> IO ()
menus application target = do
    root <- new "NSMenu"
    appMenu <- subMenu root "Sapiens4"
    if fixture application then do
        menuItem appMenu target "Reload workspace" "reloadWorkspace" "r"
        menuItem appMenu target "Restore workspace" "restoreWorkspace" "1"
        menuItem appMenu target "Quit" "quit" "q"
      else do
        menuItem appMenu (native application) "About Sapiens4" "orderFrontStandardAboutPanel:" ""
        separator <- cls "NSMenuItem" >>= \c -> obj0 c "separatorItem"; void (obj1 appMenu "addItem:" separator)
        menuItem appMenu target "Quit Sapiens4" "quit" "q"
    edit <- subMenu root "Edit"
    forM_ [("Undo","undo:","z"),("Cut","cut:","x"),("Copy","copy:","c"),("Paste","paste:","v"),("Select All","selectAll:","a")] $ \(title,action,key) -> menuItem edit nullPtr title action key
    unless (fixture application) $ do
        view <- subMenu root "View"
        menuItem view target "Show Sapiens4" "restoreWorkspace" "1"
        forM_ [("Reload Tab","reloadTab","r"),("Reload App","reloadWorkspace",""),("Zoom In","zoomIn","="),("Zoom Out","zoomOut","-"),("Actual Size","resetZoom","0")] $ \(title,action,key) -> menuItem view target title action key
    void (obj1 (native application) "setMainMenu:" root)

initialize :: Application -> Obj -> Text -> IO ()
initialize application target address = do
    void (obj1 (native application) "setActivationPolicy:" (int 0))
    void (obj1 (native application) "setDelegate:" target)
    menus application target
    view <- createShell application target
    container <- allocated "NSView" >>= \o -> rect o "initWithFrame:" (0,0,1180,820)
    void (obj1 container "addSubview:" view)
    win <- allocated "NSWindow" >>= \o -> window o 1180 820 15
    writeIORef (windowRef application) win
    setText win "setTitle:" (if fixture application then "Sapiens4 behavior fixture" else "Sapiens4")
    void (obj1 win "setContentView:" container); void (obj1 win "setReleasedWhenClosed:" (int 0))
    unless (fixture application) $ do
        size win "setMinSize:" 560 340
        size win "setContentSize:" 560 600
    void (obj0 win "center")
    showWindow application
    timerClass <- cls "NSTimer"; timerSelector <- sel "scheduledTimerWithTimeInterval:target:selector:userInfo:repeats:"; tick <- sel "tick:"
    void (callTimer timerClass timerSelector 0.05 target tick nullPtr 1)
    if fixture application then do
        u <- url address; path <- textProp u "path"
        loadURL view (if path == "/" || T.null path then T.dropWhileEnd (== '/') address <> "/workspace/" else address)
        void $ forkIO $ (forever $ do
            line <- BC.hGetLine stdin
            forM_ (decodeStrict line) $ \value -> post application $ do
                let width = numeric 1180 (field "width" value); h = numeric 820 (field "height" value); top = numeric 0 (field "top" value)
                size win "setContentSize:" width h
                void (rect view "setFrame:" (0,0,width,h-top))) `catch` (\(_ :: IOException) -> post application (void (obj1 (native application) "terminate:" nullPtr)))
        putStrLn "READY"; hFlush stdout
      else do
        loadBootstrap application
        -- Start only after the local document is loaded; didFinishNavigation
        -- handles saved-location startup and missing-location recovery.

runApplication :: Bool -> Text -> IO ()
runApplication isFixture address = do
    loadFrameworks
    bracket (new "NSAutoreleasePool") (\pool -> void (obj0 pool "drain")) $ \_ -> (runSession isFixture address `catch` (\(e :: SomeException) -> alert (T.pack (storageMessage e))))

runSession :: Bool -> Text -> IO ()
runSession isFixture initialAddress = do
    executable <- getExecutablePath
    let packaged = takeDirectory executable </> "../Resources/runtime"
    bundled <- doesDirectoryExist packaged
    runtime <- if bundled then canonicalizePath packaged else do
        bundle <- cls "NSBundle" >>= (`obj0` "mainBundle")
        resource <- str "Launcher"; extension <- str "plist"
        path <- obj2 bundle "pathForResource:ofType:" resource extension
        configuration <- cls "NSDictionary" >>= \c -> obj1 c "dictionaryWithContentsOfFile:" path
        key <- str "Repository"
        root <- obj1 configuration "objectForKey:" key >>= string
        pathKey <- str "Path"
        search <- obj1 configuration "objectForKey:" pathKey >>= string
        unless (T.null search) (setEnv "PATH" (T.unpack search))
        canonicalizePath =<< if T.null root then getCurrentDirectory else pure (T.unpack root)
    application <- cls "NSApplication" >>= \c -> obj0 c "sharedApplication"
    http <- newManager defaultManagerSettings
    guestScript <- if isFixture then
        (do r <- parseRequest (T.unpack (T.dropWhileEnd (== '/') initialAddress <> "/workspace/guest.js")); response <- httpLbs r http; pure (TE.decodeUtf8With (\_ _ -> Just '\xfffd') (L.toStrict (responseBody response)))) `catch` (\(_ :: HttpException) -> pure "")
      else pure ""
    b <- newBrowser (if isFixture then initialAddress else "about:blank") guestScript >>= newIORef
    win <- newIORef nullPtr; tasks <- newIORef []
    process <- newIORef Nothing; address <- newIORef (if isFixture then initialAddress else "")
    worker <- newIORef Nothing; busy <- newIORef False; loaded <- newIORef False; selection <- newIORef Nothing
    problem <- newIORef Nothing
    preferences <- if isFixture then pure nullPtr else do
        config <- configDirectory >>= canonicalizePath
        let hex byte = let value=showHex byte "" in replicate (2-length value) '0' ++ value
            suite="com.sapiens4.client." <> T.pack (concatMap hex (BS.unpack (SHA256.hash (TE.encodeUtf8 (T.pack config)))))
        allocated "NSUserDefaults" >>= \o -> str suite >>= obj1 o "initWithSuiteName:"
    let state = Application application b win tasks isFixture process address worker busy loaded runtime selection problem preferences
    target <- installMethods state
    initialize state target initialAddress
    void (obj0 application "run") `finally` do
        closeApplication state
        readIORef b >>= \current -> void (obj0 (origin current) "release")
        unless isFixture $ void (obj0 preferences "release")

createShell :: Application -> Obj -> IO Obj
createShell application target = do
    configuration <- new "WKWebViewConfiguration"
    when (fixture application) $ cls "WKWebsiteDataStore" >>= \c -> obj0 c "nonPersistentDataStore" >>= void . obj1 configuration "setWebsiteDataStore:"
    user <- obj0 configuration "userContentController"; channel <- str "browser"
    void (obj2 user "addScriptMessageHandler:name:" target channel)
    view <- allocated "WKWebView" >>= \o -> rectObj o "initWithFrame:configuration:" (0,0,1180,820) configuration
    b <- readIORef (browser application)
    writeIORef (shell b) view
    -- Fixtures keep native navigation off but need the UI delegate for file pickers.
    void (obj1 view "setUIDelegate:" target)
    unless (fixture application) $ void (obj1 view "setNavigationDelegate:" target)
    void (obj1 view "setAutoresizingMask:" (int 18))
    pure view

bootstrapPath :: Application -> FilePath
bootstrapPath application = runtime application </> "app/shell/bootstrap.html"

loadBootstrap :: Application -> IO ()
loadBootstrap application = do
    view <- readIORef (browser application) >>= readIORef . shell
    file <- fileURL (T.pack (bootstrapPath application))
    parent <- obj0 file "URLByDeletingLastPathComponent" >>= (`obj0` "URLByDeletingLastPathComponent")
    void (obj2 view "loadFileURL:allowingReadAccessToURL:" file parent)

showStorageError :: Application -> Text -> IO ()
showStorageError application message = do
    b <- readIORef (browser application)
    view <- readIORef (shell b)
    script view ("window.storageError?.(" <> jsonText (String message) <> ")")

closeApplication :: Application -> IO ()
closeApplication application = do
    unless (fixture application) $ void (long (clientPreferences application) "synchronize")
    atomicModifyIORef' (startup application) (\old -> (Nothing,old)) >>= mapM_ cancel
    stopOwnedServer application

receiveMessage :: Application -> Obj -> IO ()
receiveMessage application message = do
    b <- readIORef (browser application)
    connected <- not . T.null <$> readIORef (address application)
    if connected || fixture application then receive b message (saveClientPreferences application) else do
        sh <- readIORef (shell b)
        sourceView <- obj0 message "webView"
        frame <- obj0 message "frameInfo"
        mainFrame <- booleanValue frame "isMainFrame"
        source <- obj0 frame "request" >>= (`obj0` "URL")
        path <- textProp source "path"
        scheme <- textProp source "scheme"
        when (sourceView==sh && mainFrame && scheme=="file" && path==T.pack (bootstrapPath application)) $ do
            raw <- obj0 message "body" >>= jsonObject
            forM_ raw $ \input -> do
              case text (field "storage" input) of
                "new" -> defaultDataDirectory >>= \directory -> startSelected application StartNew directory
                "pick" -> do
                    busy <- atomicModifyIORef' (starting application) (\old -> (True,old))
                    unless busy $ do
                        selected <- chooseDataDirectory `finally` writeIORef (starting application) False
                        script sh ("window.storagePicked?.(" <> jsonText (maybe Null (String . T.pack) selected) <> ")")
                "existing" -> do
                    let directory=T.unpack (text (field "directory" input))
                    if isAbsolute directory then startSelected application AttachExisting directory
                      else showStorageError application "Choose a folder before continuing."
                _ -> pure ()

chooseDataDirectory :: IO (Maybe FilePath)
chooseDataDirectory = do
    panel <- new "NSOpenPanel"
    void (obj1 panel "setCanChooseDirectories:" (int 1))
    void (obj1 panel "setCanChooseFiles:" (int 0))
    void (obj1 panel "setAllowsMultipleSelection:" (int 0))
    void (obj1 panel "setShowsHiddenFiles:" (int 1))
    setText panel "setPrompt:" "Choose data"
    result <- long panel "runModal"
    if result/=1 then pure Nothing else Just . T.unpack <$> (obj0 panel "URL" >>= (\u -> textProp u "path"))

-- Keep device-local UI state outside the changing loopback origin and outside
-- Corpora snapshots. NSUserDefaults batches writes; shutdown flushes the suite.
readClientPreferences :: Application -> FilePath -> IO (KeyMap.KeyMap Value)
readClientPreferences application directory = do
    value <- str (T.pack directory) >>= obj1 (clientPreferences application) "stringForKey:" >>= string
    pure (fromMaybe KeyMap.empty (decodeStrict (TE.encodeUtf8 value)))

saveClientPreferences :: Application -> Value -> IO ()
saveClientPreferences application input = unless (fixture application) $ do
    active <- readIORef (child application)
    forM_ active $ \server -> do
        let name=text (field "key" input); value=text (field "value" input)
        when ((name=="sapi-color-theme" || name=="s4.active" || T.isPrefixOf "s4.workspace." name) && T.length name<=128 && T.length value<=1000000) $ do
            previous <- readClientPreferences application (serverDirectory server)
            let stored=Object (KeyMap.insert (Key.fromText name) (String value) previous)
            content <- str (jsonText stored)
            nameObject <- str (T.pack (serverDirectory server))
            void (obj2 (clientPreferences application) "setObject:forKey:" content nameObject)

installClientPreferences :: Application -> Obj -> Text -> FilePath -> IO ()
installClientPreferences application view readyAddress directory = unless (fixture application) $ do
    saved <- readClientPreferences application directory
    let source=T.unlines
            [ "(() => {"
            , "if (location.origin !== new URL(" <> jsonText (String readyAddress) <> ").origin) return;"
            , "try {"
            , "const root=" <> jsonText (String (T.pack directory)) <> ", saved=" <> jsonText (Object saved) <> ";"
            , "const allowed=k => k==='sapi-color-theme'||k==='s4.active'||k.startsWith('s4.workspace.');"
            , "const previous=localStorage.getItem('s4.native.storage');"
            , "if(previous && previous!==root) for(const k of Object.keys(localStorage)) if(allowed(k)||k.startsWith('s4.snapshot.')) localStorage.removeItem(k);"
            , "for(const [k,v] of Object.entries(saved)) if(allowed(k)&&localStorage.getItem(k)===null) localStorage.setItem(k,v);"
            , "localStorage.setItem('s4.native.storage',root);"
            , "const store=Storage.prototype.setItem;"
            , "const notify=(key,value)=>window.webkit.messageHandlers.browser.postMessage({clientPreferences:{key,value}});"
            , "Storage.prototype.setItem=function(key,value){store.call(this,key,value);if(this===localStorage&&allowed(String(key)))notify(String(key),String(value));};"
            , "for(const k of Object.keys(localStorage)) if(allowed(k)) notify(k,localStorage.getItem(k));"
            , "} catch (_) {}"
            , "})();"
            ]
    user <- obj0 view "configuration" >>= (`obj0` "userContentController")
    void (obj0 user "removeAllUserScripts")
    content <- str source
    injection <- allocated "WKUserScript" >>= \o -> obj3 o "initWithSource:injectionTime:forMainFrameOnly:" content (int 0) (int 1)
    void (obj1 user "addUserScript:" injection)

serverRunning :: Maybe ServerProcess -> IO Bool
serverRunning Nothing = pure False
serverRunning (Just server) = isNothing <$> getProcessExitCode (serverHandle server)

-- Clear ownership only after a confirmed exit, including failed startup cleanup.
stopOwnedServer :: Application -> IO ()
stopOwnedServer application = mask_ $ do
    current <- readIORef (child application)
    mapM_ stopServer current `finally` do
        alive <- serverRunning current
        unless alive $ writeIORef (child application) Nothing

-- Navigation completion, rather than loadURL, commits the selected directory.
installServer :: Application -> Text -> Text -> FilePath -> IO ()
installServer application readyAddress guestScript directory = do
    previous <- readIORef (browser application)
    view <- readIORef (shell previous)
    next <- newBrowser readyAddress guestScript
    readIORef (delegate previous) >>= writeIORef (delegate next)
    writeIORef (shell next) view
    writeIORef (browser application) next
    void (obj0 (origin previous) "release")
    writeIORef (selectedRoot application) (Just directory)
    installClientPreferences application view readyAddress directory
    loadURL view readyAddress
    worker <- async $ threadDelay 35000000 >> post application (do
        selection <- readIORef (selectedRoot application)
        when (selection==Just directory) $ recoverStartup application "Your workspace did not load within 35 seconds. Your saved storage selection is unchanged.")
    writeIORef (startup application) (Just worker)

recoverStartup :: Application -> Text -> IO ()
recoverStartup application message = do
    atomicModifyIORef' (startup application) (\old -> (Nothing,old)) >>= mapM_ cancel
    writeIORef (selectedRoot application) Nothing
    worker <- async $ do
        stopped <- try (stopOwnedServer application)
        let detail = case stopped of
                Left (e :: SomeException) -> message <> " Cleanup failed: " <> T.pack (storageMessage e)
                Right () -> message
        post application $ do
            writeIORef (address application) ""
            previous <- readIORef (browser application)
            view <- readIORef (shell previous)
            next <- newBrowser "about:blank" ""
            readIORef (delegate previous) >>= writeIORef (delegate next)
            writeIORef (shell next) view
            sync previous (object [])
            writeIORef (browser application) next
            void (obj0 (origin previous) "release")
            writeIORef (recoveryError application) (Just detail)
            writeIORef (localLoaded application) True
            win <- readIORef (windowRef application)
            setText win "setFrameAutosaveName:" ""
            size win "setMinSize:" 560 340
            size win "setContentSize:" 560 600
            loadBootstrap application
    writeIORef (startup application) (Just worker)

startSelected :: Application -> StorageChoice -> FilePath -> IO ()
startSelected application choice directory = mask $ \unmask -> do
    busy <- atomicModifyIORef' (starting application) (\old -> (True,old))
    unless busy $ do
        b <- readIORef (browser application)
        view <- readIORef (shell b)
        script view "window.storageLoading?.()"
        worker <- async $ handle (\(e :: SomeException) -> case fromException e :: Maybe SomeAsyncException of
            Just _ -> throwIO e
            Nothing -> post application (recoverStartup application (T.pack (storageMessage e)))) $ unmask $ do
                -- Retry must first reap any child retained after failed cleanup.
                stopOwnedServer application
                selected <- canonicalizePath directory
                let binary=runtime application </> ".build/bin/sapiens4"
                (code,_,errors) <- readProcessWithExitCode binary ["check-data",selected,if choice==StartNew then "new" else "existing"] ""
                unless (code==ExitSuccess) $ ioError (userError errors)
                guestScript <- TIO.readFile (runtime application </> ".build/web/guest.js")
                mask $ \restore -> do
                    (_,readyAddress) <- restore (startServer application choice (runtime application) selected)
                    writeIORef (address application) readyAddress
                    post application $ installServer application readyAddress guestScript selected
                        `catch` (\(e :: SomeException) -> recoverStartup application (T.pack (storageMessage e)))
        writeIORef (startup application) (Just worker)

workspaceWindow :: Application -> IO ()
workspaceWindow application = do
    win <- readIORef (windowRef application)
    size win "setMinSize:" 720 540
    size win "setContentSize:" 1180 820
    name <- str "Sapiens4Workspace"
    void (booleanArgument win "setFrameUsingName:" name)
    setText win "setFrameAutosaveName:" "Sapiens4Workspace"
