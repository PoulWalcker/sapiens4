{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Application where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception
import Control.Monad
import Data.Aeson
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as L
import Data.IORef
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Foreign hiding (void, new)
import Network.HTTP.Client hiding (method)
import Network.URI (parseURI, uriScheme, uriAuthority, uriRegName, uriUserInfo, uriPath, uriQuery, uriFragment)
import ObjC
import Browser
import qualified RuntimePaths as Paths
import System.Directory
import System.Environment
import System.FilePath
import System.IO
import System.Process hiding (shell)
import System.Timeout
import System.Posix.Process (getProcessID)
import Data.List (stripPrefix, nub, intercalate)


data Application = Application { native :: Obj, browser :: Browser, windowRef :: IORef Obj, pending :: IORef [IO ()], fixture :: Bool, child :: IORef (Maybe ServerProcess), address :: IORef Text, startupWorker :: IORef (Maybe (Async ())), quitting :: IORef Bool }

data ServerProcess = ServerProcess ProcessHandle Handle Handle Handle [Async ()]

post :: Application -> IO () -> IO ()
post application action = atomicModifyIORef' (pending application) (\actions -> (actions ++ [action],()))

-- Only the child started by this application is trusted. Never attach to a
-- fixed port, which might belong to another installation or data directory.
check :: Application -> IO ()
check application = do
    destination <- readIORef (address application)
    current <- readIORef (child application)
    alive <- case current of
        Nothing -> pure False
        Just (ServerProcess process _ _ _ _) -> isNothing <$> getProcessExitCode process
    unless (T.null destination) $
        if alive then readIORef (shell (browser application)) >>= \view -> loadURL view destination
          else alert "The application server stopped. Quit and reopen Sapiens4."

startServer :: IORef (Maybe ServerProcess) -> IO Text
startServer owner = mask $ \restore -> do
    executable <- getExecutablePath
    let resources = takeDirectory executable </> "../Resources"
    runtime <- canonicalizePath (resources </> "runtime")
    configRoot <- Paths.configDirectory runtime
    destination <- Paths.defaultDataDirectory runtime
    let binary = runtime </> ".build/bin/sapiens4"
    present <- doesFileExist binary
    unless present $ ioError (userError "The application is incomplete. Extract the complete Sapiens4 package.")
    mapM_ (createDirectoryIfMissing True) [destination, configRoot]
    environment <- getEnvironment
    pid <- getProcessID
    -- System tools take precedence; expose standard Homebrew installs and the
    -- caller's tool managers without running shell startup scripts.
    let toolPath = intercalate [searchPathSeparator] $ nub $
            ["/usr/bin","/bin","/usr/sbin","/sbin","/opt/homebrew/bin","/usr/local/bin"] ++
            filter isAbsolute (maybe [] splitSearchPath $ lookup "PATH" environment)
        overrides = [("PATH",toolPath),("SAPIENS_REPOSITORY",runtime),("SAPIENS_CONFIG_DIR",configRoot),("SAPIENS_DATA_DIR",destination),("SAPIENS_DESKTOP_PID",show pid)]
    (Just input, Just output, Just errors, process) <- createProcess (proc binary ["--port","0","--data-dir",destination,"--desktop"])
        { cwd = Just runtime, env = Just (overrides ++ filter ((`notElem` map fst overrides) . fst) environment)
        , std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe, create_group = True }
    let own readers = writeIORef owner (Just (ServerProcess process input output errors readers))
    -- Publish the child before any cancellable initialization/readiness work.
    own []
    mapM_ (\h -> hSetEncoding h utf8 >> hSetNewlineMode h universalNewlineMode) [input,output,errors]
    reader <- async $ withFile (destination </> "desktop-server.log") AppendMode $ \logFile ->
        (forever (hGetLine errors >>= hPutStrLn logFile >> hFlush logFile)) `catch` (\(_ :: IOException) -> pure ())
    own [reader]
    let await = do
            line <- hGetLine output
            case stripPrefix "Sapiens4: " line >>= parseURI of
                Just uri | uriScheme uri == "http:", Just auth <- uriAuthority uri,
                           uriRegName auth == "127.0.0.1", null (uriUserInfo auth) -> pure (T.pack (show uri))
                _ -> await
    readiness <- newEmptyMVar
    outputReader <- async $ do
        result <- try await :: IO (Either IOException Text)
        putMVar readiness result
        case result of
            Left _ -> pure ()
            Right _ -> (forever (void (hGetLine output))) `catch` (\(_ :: IOException) -> pure ())
    own [reader,outputReader]
    restore (timeout 35000000 (takeMVar readiness) >>= maybe (ioError (userError "The application server did not start within 35 seconds. See desktop-server.log in the application data folder.")) (either throwIO pure))

closeServer :: IORef (Maybe ServerProcess) -> IO ()
closeServer owner = mask_ $ do
    readIORef owner >>= mapM_ stopServer
    writeIORef owner Nothing

stopApplication :: Application -> IO ()
stopApplication application = do
    -- Joining startup prevents a later acquisition from racing shutdown.
    readIORef (startupWorker application) >>= mapM_ cancel
    writeIORef (startupWorker application) Nothing
    closeServer (child application)

stopServer :: ServerProcess -> IO ()
stopServer (ServerProcess process input output errors readers) = mask_ $ do
    stop
    -- Failed termination retains stdout/stderr supervision and ownership.
    mapM_ cancel readers `finally` mapM_ close [input,output,errors]
    where
        close h = hClose h `catch` (\(_ :: IOException) -> pure ())
        stop = do
            (hPutStrLn input "stop" >> hFlush input >> hClose input) `catch` (\(_ :: IOException) -> pure ())
            ended <- timeout 10000000 (waitForProcess process)
            when (isNothing ended) $ do
                terminateProcess process
                confirmed <- timeout 5000000 (waitForProcess process)
                when (isNothing confirmed) (ioError (userError "The application server could not be stopped. Quit was cancelled."))

alert :: Text -> IO ()
alert message = do
    application <- cls "NSApplication" >>= \c -> obj0 c "sharedApplication"
    void (obj1 application "setActivationPolicy:" (int 0))
    running <- booleanValue application "isRunning"
    unless running $ void (obj0 application "finishLaunching")
    void (obj1 application "activateIgnoringOtherApps:" (int 1))
    panel <- new "NSAlert"
    setText panel "setMessageText:" "Sapiens4 could not open"
    setText panel "setInformativeText:" message
    void (long panel "runModal")

reload :: Application -> IO ()
reload application = if fixture application then readIORef (shell (browser application)) >>= void . (`obj0` "reload") else do
    current <- readIORef (child application)
    alive <- case current of
        Nothing -> pure False
        Just (ServerProcess process _ _ _ _) -> isNothing <$> getProcessExitCode process
    if alive then check application else beginStartup application
showWindow :: Application -> IO ()
showWindow application = do win <- readIORef (windowRef application); void (obj1 win "makeKeyAndOrderFront:" nullPtr); void (obj1 (native application) "activateIgnoringOtherApps:" (int 1))

installMethods :: Application -> IO Obj
installMethods application = do
    c <- subclass "SapiensHaskellDesktop"
    let b = browser application
        on0 name action = wrap0 (\_ _ -> action) >>= method c name "v@:"
        on1 name action = wrap1 (\_ _ value -> action value) >>= method c name "v@:@"
        on2 name action = wrap2 (\_ _ a d -> action a d) >>= method c name "v@:@@"
        on3 name action = wrap3 (\_ _ a d e -> action a d e) >>= method c name "v@:@@@"
        on4 name action = wrap4 (\_ _ a d e f -> action a d e f) >>= method c name "v@:@@@@"
    on1 "tick:" $ \_ -> atomicModifyIORef' (pending application) (\items -> ([],items)) >>= sequence_
    on0 "reloadWorkspace" (reload application)
    on0 "quit" (void (obj1 (native application) "terminate:" nullPtr))
    wrapLong1 (\_ _ _ -> do
        busy <- atomicModifyIORef' (quitting application) (\value -> (True,value))
        unless busy $ void $ async $ do
            result <- try (stopApplication application)
            post application $ do
                case result of
                    Left (e :: SomeException) -> writeIORef (quitting application) False >> alert (T.pack (displayException e))
                    Right () -> pure ()
                void (obj1 (native application) "replyToApplicationShouldTerminate:" (flag (either (const False) (const True) result)))
        pure 2) >>= method c "applicationShouldTerminate:" "Q@:@"
    on0 "reloadTab" (activeAction b "reload" 0)
    on0 "zoomIn" (activeAction b "zoom" 0.1)
    on0 "restoreWorkspace" $ do
        win <- readIORef (windowRef application)
        void (obj1 win "deminiaturize:" nullPtr)
        showWindow application
    on0 "zoomOut" (activeAction b "zoom" (-0.1))
    on0 "resetZoom" (activeAction b "zoom" 0)
    wrapBool2 (\_ _ _ _ -> showWindow application >> pure 1) >>= method c "applicationShouldHandleReopen:hasVisibleWindows:" "B@:@B"
    wrapBool1 (\_ _ _ -> pure 0) >>= method c "applicationShouldTerminateAfterLastWindowClosed:" "B@:@"
    on2 "userContentController:didReceiveScriptMessage:" (\_ message -> receive b message)
    on3 "webView:decidePolicyForNavigationAction:decisionHandler:" (navigation b)
    on2 "webView:didCommitNavigation:" (\view _ -> finished b view)
    on2 "webView:didFinishNavigation:" (\view _ -> finished b view)
    on3 "webView:didFailNavigation:withError:" (\view _ err -> failed b view err)
    on3 "webView:didFailProvisionalNavigation:withError:" (\view _ err -> failed b view err)
    on1 "webViewWebContentProcessDidTerminate:" $ \view -> do
        item <- lookupView b view
        case item of
            Nothing -> reload application
            Just tab -> do
                let updated = tab { tabError = "Page process stopped. Reload to continue." }
                save b updated; report b updated
    on4 "observeValueForKeyPath:ofObject:change:context:" (\_ view _ _ -> withTab b view (report b))
    on4 "webView:runOpenPanelWithParameters:initiatedByFrame:completionHandler:" (\_ parameters _ completion -> openPanel parameters completion)
    on2 "webView:startURLSchemeTask:" (serveFile b)
    on2 "webView:stopURLSchemeTask:" (\_ _ -> pure ())
    wrapObj4 (\_ _ view _ action _ -> do
        sh <- readIORef (shell b)
        when (view == sh) $ do
            request <- obj0 action "request"; u <- obj0 request "URL"
            local <- sameOrigin u =<< readIORef (origin b)
            when local $ void (obj1 view "loadRequest:" request)
        pure nullPtr) >>= method c "webView:createWebViewWithConfiguration:forNavigationAction:windowFeatures:" "@@:@@@@"
    registerClass c
    target <- obj0 c "new"
    writeIORef (delegate b) target
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
        forM_ [("Reload Tab","reloadTab","r"),("Reload App","reloadWorkspace",""),("Zoom In","zoomIn","="),("Zoom Out","zoomOut","-"),("Actual Size","resetZoom","0")] $ \(title,action,key) -> menuItem view target title action key
    void (obj1 (native application) "setMainMenu:" root)

initialize :: Application -> Obj -> Text -> IO ()
initialize application target address = do
    void (obj1 (native application) "setActivationPolicy:" (int 0))
    void (obj1 (native application) "setDelegate:" target)
    menus application target
    configuration <- new "WKWebViewConfiguration"
    when (fixture application) $ cls "WKWebsiteDataStore" >>= \c -> obj0 c "nonPersistentDataStore" >>= void . obj1 configuration "setWebsiteDataStore:"
    user <- obj0 configuration "userContentController"; channel <- str "browser"
    void (obj2 user "addScriptMessageHandler:name:" target channel)
    view <- allocated "WKWebView" >>= \o -> rectObj o "initWithFrame:configuration:" (0,0,1180,820) configuration
    writeIORef (shell (browser application)) view
    -- Fixtures keep native navigation off but need the UI delegate for file pickers.
    void (obj1 view "setUIDelegate:" target)
    unless (fixture application) $ void (obj1 view "setNavigationDelegate:" target)
    void (obj1 view "setAutoresizingMask:" (int 18))
    container <- allocated "NSView" >>= \o -> rect o "initWithFrame:" (0,0,1180,820)
    void (obj1 container "addSubview:" view)
    win <- allocated "NSWindow" >>= \o -> window o 1180 820 15
    writeIORef (windowRef application) win
    setText win "setTitle:" (if fixture application then "Sapiens4 behavior fixture" else "Sapiens4")
    void (obj1 win "setContentView:" container); void (obj1 win "setReleasedWhenClosed:" (int 0))
    unless (fixture application) $ do
        size win "setMinSize:" 720 540
        setText win "setFrameAutosaveName:" "Sapiens4Workspace"
    void (obj0 win "center")
    showWindow application
    timerClass <- cls "NSTimer"; timerSelector <- sel "scheduledTimerWithTimeInterval:target:selector:userInfo:repeats:"; tick <- sel "tick:"
    timer <- callTimer timerClass timerSelector 0.05 target tick nullPtr 1
    -- NSTerminateLater runs a modal loop. Keep the completion queue serviced
    -- there so confirmed backend exit can finish (or cancel) the Quit request.
    loop <- cls "NSRunLoop" >>= \c -> obj0 c "currentRunLoop"
    modal <- str "NSModalPanelRunLoopMode"
    void (obj2 loop "addTimer:forMode:" timer modal)
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
        html <- str "<body style='background:#191919;color:#eee;font:20px system-ui;padding:48px'>Starting Sapiens4…</body>"
        void (obj2 view "loadHTMLString:baseURL:" html nullPtr)

fetchGuestScript :: Text -> IO Text
fetchGuestScript address =
    let base = maybe address (T.pack . show . (\u -> u { uriPath = "", uriQuery = "", uriFragment = "" })) (parseURI (T.unpack address))
    in bracket (newManager defaultManagerSettings) closeManager $ \http ->
        (do r <- parseRequest (T.unpack (T.dropWhileEnd (== '/') base <> "/workspace/guest.js"))
            response <- httpLbs r { responseTimeout = responseTimeoutMicro 2000000 } http
            pure (TE.decodeUtf8With (\_ _ -> Just '\xfffd') (L.toStrict (responseBody response))))
        `catch` (\(_ :: HttpException) -> pure "")

beginStartup :: Application -> IO ()
beginStartup application = do
    closing <- readIORef (quitting application)
    worker <- readIORef (startupWorker application)
    unless (closing || isJust worker) $ do
        worker <- async $ do
            result <- try $ (do
                closeServer (child application)
                destination <- startServer (child application)
                guestScript <- fetchGuestScript destination
                pure (destination,guestScript)) `onException` closeServer (child application)
            post application $ do
                closing <- readIORef (quitting application)
                unless closing $ do
                    writeIORef (startupWorker application) Nothing
                    case result of
                        Left (e :: SomeException) -> alert (T.pack (displayException e))
                        Right (destination,guestScript) -> do
                            configureBrowser (browser application) destination guestScript
                            writeIORef (address application) destination
                            check application
        writeIORef (startupWorker application) (Just worker)

runApplication :: Bool -> Text -> IO ()
runApplication isFixture address = do
    loadFrameworks
    withAutoreleasePool $ (runSession isFixture address `catch` (\(e :: SomeException) -> alert (T.pack (displayException e))))

runSession :: Bool -> Text -> IO ()
runSession isFixture initialAddress = do
    application <- cls "NSApplication" >>= \c -> obj0 c "sharedApplication"
    guestScript <- if isFixture then fetchGuestScript initialAddress else pure ""
    b <- newBrowser (if isFixture then initialAddress else "about:blank") guestScript
    win <- newIORef nullPtr; tasks <- newIORef []; owner <- newIORef Nothing
    destination <- newIORef ""; worker <- newIORef Nothing; closing <- newIORef False
    let state = Application application b win tasks isFixture owner destination worker closing
    flip finally (stopApplication state) $ do
        target <- installMethods state
        initialize state target initialAddress
        -- The first timer tick starts the worker only after AppKit is running.
        unless isFixture $ post state (beginStartup state)
        void (obj0 application "run")
