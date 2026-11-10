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


data Application = Application { native :: Obj, browser :: IORef Browser, windowRef :: IORef Obj, pending :: IORef [IO ()], fixture :: Bool, child :: IORef (Maybe ServerProcess), address :: IORef Text, startup :: IORef (Maybe (Async ())), starting :: IORef Bool, localLoaded :: IORef Bool, runtime :: FilePath, windowSetup :: IORef (Maybe Text) }

data ServerProcess = ServerProcess ProcessHandle Handle (Async ())

post :: Application -> IO () -> IO ()
post application action = atomicModifyIORef' (pending application) (\actions -> (actions ++ [action],()))

-- Only the child started by this application is trusted. Never attach to a
-- fixed port, which might belong to another installation or data directory.
check :: Application -> IO ()
check application = do
    current <- readIORef (child application)
    case current of
        Nothing -> pure ()
        Just (ServerProcess process _ _) -> do
            alive <- isNothing <$> getProcessExitCode process
            if alive then do
                b <- readIORef (browser application)
                view <- readIORef (shell b)
                readIORef (address application) >>= loadURL view
              else alert "The application server stopped. Quit and reopen Sapiens4."

startServer :: StorageChoice -> FilePath -> FilePath -> IO (ServerProcess, Text)
startServer choice runtime destination = mask $ \restore -> do
    configRoot <- configDirectory
    let binary = runtime </> ".build/bin/sapiens4"
    present <- doesFileExist binary
    unless present $ ioError (userError "The application is incomplete. Extract the complete Sapiens4 Development package.")
    createDirectoryIfMissing True configRoot
    environment <- getEnvironment
    pid <- getProcessID
    let overrides = [("PATH","/usr/bin:/bin:/usr/sbin:/sbin"),("SAPIENS_REPOSITORY",runtime),("SAPIENS_CONFIG_DIR",configRoot),("SAPIENS_DATA_DIR",destination),("SAPIENS_DESKTOP_PID",show pid),("SAPIENS_STORAGE_MODE",if choice==StartNew then "new" else "existing")]
    (Just input, Just output, Just errors, process) <- createProcess (proc binary ["--port","0","--data-dir",destination,"--desktop"])
        { cwd = Just runtime, env = Just (overrides ++ filter ((`notElem` map fst overrides) . fst) environment)
        , std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe, create_group = True }
    let close h = hClose h `catch` (\(_ :: IOException) -> pure ())
        cleanup = terminateProcess process `finally` mapM_ close [input, output, errors]
    mapM_ (\h -> hSetEncoding h utf8 >> hSetNewlineMode h universalNewlineMode) [input,output,errors] `onException` cleanup
    reader <- async $ withFile (destination </> "desktop-server.log") AppendMode $ \logFile ->
        (forever (hGetLine errors >>= hPutStrLn logFile >> hFlush logFile)) `catch` (\(_ :: IOException) -> pure ())
    let server = ServerProcess process input reader
        await = do
            ended <- hIsEOF output
            when ended $ do
                threadDelay 100000
                detail <- TIO.readFile (destination </> "desktop-server.log") `catch` (\(_ :: IOException) -> pure "")
                ioError (userError ("Sapiens4 could not open this data. " ++ T.unpack (T.takeEnd 3000 detail)))
            line <- hGetLine output
            case stripPrefix "Sapiens4: " line >>= parseURI of
                Just uri | uriScheme uri == "http:", Just auth <- uriAuthority uri,
                           uriRegName auth == "127.0.0.1", null (uriUserInfo auth) -> pure (T.pack (show uri))
                _ -> await
    ready <- restore (timeout 35000000 await >>= maybe (ioError (userError "The application server did not start within 35 seconds. See desktop-server.log in the application data folder.")) pure)
        `onException` stopServer server
    -- The address is read from our private stdout pipe, never discovered by port.
    void $ async $ (forever (void (hGetLine output))) `catch` (\(_ :: IOException) -> close output)
    pure (server, ready)

stopServer :: ServerProcess -> IO ()
stopServer (ServerProcess process input reader) = stop `finally` (cancel reader >> (hClose input `catch` (\(_ :: IOException) -> pure ())))
    where
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
reload application = if fixture application then readIORef (browser application) >>= readIORef . shell >>= void . (`obj0` "reload") else check application
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
    wrapBool1 (\_ _ _ -> pure 0) >>= method c "applicationShouldTerminateAfterLastWindowClosed:" "B@:@"
    on2 "userContentController:didReceiveScriptMessage:" (\_ message -> receiveMessage application message)
    on3 "webView:decidePolicyForNavigationAction:decisionHandler:" (\view action completion -> withBrowser $ \b -> do
        current <- readIORef (child application)
        if isJust current || fixture application then navigation b view action completion else do
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
                unless loaded $ do
                    selection <- try savedDataDirectory
                    case selection of
                        Left (e :: IOException) -> showStorageError application (T.pack (storageMessage e))
                        Right Nothing -> pure ()
                        Right (Just directory) -> startSelected application AttachExisting directory
    on3 "webView:didFailNavigation:withError:" (\view _ err -> withBrowser $ \b -> failed b view err)
    on3 "webView:didFailProvisionalNavigation:withError:" (\view _ err -> withBrowser $ \b -> failed b view err)
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
        size win "setContentSize:" 560 340
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
        file <- fileURL (T.pack (bootstrapPath application))
        parent <- obj0 file "URLByDeletingLastPathComponent" >>= (`obj0` "URLByDeletingLastPathComponent")
        void (obj2 view "loadFileURL:allowingReadAccessToURL:" file parent)
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
        canonicalizePath =<< if T.null root then getCurrentDirectory else pure (T.unpack root)
    application <- cls "NSApplication" >>= \c -> obj0 c "sharedApplication"
    http <- newManager defaultManagerSettings
    guestScript <- if isFixture then
        (do r <- parseRequest (T.unpack (T.dropWhileEnd (== '/') initialAddress <> "/workspace/guest.js")); response <- httpLbs r http; pure (TE.decodeUtf8With (\_ _ -> Just '\xfffd') (L.toStrict (responseBody response)))) `catch` (\(_ :: HttpException) -> pure "")
      else pure ""
    b <- newBrowser (if isFixture then initialAddress else "about:blank") guestScript >>= newIORef
    win <- newIORef nullPtr; tasks <- newIORef []
    process <- newIORef Nothing; address <- newIORef initialAddress
    worker <- newIORef Nothing; busy <- newIORef False; loaded <- newIORef False; setup <- newIORef (Just "welcome")
    let state = Application application b win tasks isFixture process address worker busy loaded runtime setup
    target <- installMethods state
    initialize state target initialAddress
    void (obj0 application "run") `finally` (closeApplication state >> readIORef b >>= \current -> void (obj0 (origin current) "release"))

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

showStorageError :: Application -> Text -> IO ()
showStorageError application message = do
    b <- readIORef (browser application)
    view <- readIORef (shell b)
    script view ("window.storageError?.(" <> jsonText (String message) <> ")")

closeApplication :: Application -> IO ()
closeApplication application = do
    atomicModifyIORef' (startup application) (\old -> (Nothing,old)) >>= mapM_ cancel
    atomicModifyIORef' (child application) (\old -> (Nothing,old)) >>= mapM_ stopServer

receiveMessage :: Application -> Obj -> IO ()
receiveMessage application message = do
    b <- readIORef (browser application)
    active <- readIORef (child application)
    if isJust active || fixture application then receive b message (setSetupWindow application) else do
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
              when (text (field "storageStage" input)=="storage") $ do
                win <- readIORef (windowRef application)
                size win "setContentSize:" 560 400
              case text (field "storage" input) of
                "new" -> defaultDataDirectory >>= startSelected application StartNew
                "existing" -> do
                    panel <- new "NSOpenPanel"
                    void (obj1 panel "setCanChooseDirectories:" (int 1))
                    void (obj1 panel "setCanChooseFiles:" (int 0))
                    void (obj1 panel "setAllowsMultipleSelection:" (int 0))
                    void (obj1 panel "setShowsHiddenFiles:" (int 1))
                    setText panel "setPrompt:" "Open data"
                    result <- long panel "runModal"
                    when (result==1) $ obj0 panel "URL" >>= (\u -> textProp u "path") >>= startSelected application AttachExisting . T.unpack
                _ -> pure ()

startSelected :: Application -> StorageChoice -> FilePath -> IO ()
startSelected application choice directory = do
    busy <- atomicModifyIORef' (starting application) (\old -> (True,old))
    unless busy $ do
        b <- readIORef (browser application)
        view <- readIORef (shell b)
        script view "window.storageLoading?.()"
        task <- async $ do
            result <- try $ do
                let binary=runtime application </> ".build/bin/sapiens4"
                (code,_,errors) <- readProcessWithExitCode binary ["check-data",directory,if choice==StartNew then "new" else "existing"] ""
                unless (code==ExitSuccess) $ ioError (userError errors)
                guestScript <- TIO.readFile (runtime application </> ".build/web/guest.js")
                mask $ \restore -> do
                    (server,readyAddress) <- restore (startServer choice (runtime application) directory)
                    writeIORef (child application) (Just server)
                    restore (saveDataDirectory directory) `onException`
                        (writeIORef (child application) Nothing >> stopServer server)
                    writeIORef (address application) readyAddress
                    post application $ do
                        previous <- readIORef (browser application)
                        old <- readIORef (shell previous)
                        let recover (e :: SomeException) = do
                                writeIORef (browser application) previous
                                atomicModifyIORef' (child application) (\current -> (Nothing,current)) >>= mapM_ stopServer
                                writeIORef (starting application) False
                                showStorageError application (T.pack (storageMessage e))
                        (do
                            next <- newBrowser readyAddress guestScript
                            target <- readIORef (delegate previous)
                            writeIORef (delegate next) target
                            writeIORef (browser application) next
                            view <- createShell application target
                            container <- obj0 old "superview"
                            void (obj1 container "addSubview:" view)
                            void (obj1 view "setTranslatesAutoresizingMaskIntoConstraints:" (int 0))
                            forM_ ["leadingAnchor","trailingAnchor","topAnchor","bottomAnchor"] $ \name -> do
                                edge <- obj0 view name
                                parentEdge <- obj0 container name
                                constraint <- obj1 edge "constraintEqualToAnchor:" parentEdge
                                void (obj1 constraint "setActive:" (int 1))
                            win <- readIORef (windowRef application)
                            size win "setContentSize:" 560 600
                            loadURL view readyAddress
                            void (obj0 old "removeFromSuperview")
                            void (obj0 (origin previous) "release")
                            writeIORef (starting application) False) `catch` recover
            case result of
                Left (e :: SomeException) -> case fromException e :: Maybe SomeAsyncException of
                    Just _ -> throwIO e
                    Nothing -> do
                        atomicModifyIORef' (child application) (\current -> (Nothing,current)) >>= mapM_ stopServer
                        post application $ writeIORef (starting application) False >> showStorageError application (T.pack (storageMessage e))
                Right () -> pure ()
        writeIORef (startup application) (Just task)

-- Setup size is transient. Save/restore the normal workspace frame only after
-- completion; all stages continue to use the same NSWindow.
setSetupWindow :: Application -> Bool -> Text -> IO ()
setSetupWindow application setup step = unless (fixture application) $ do
    let next=if setup then Just step else Nothing
    previous <- atomicModifyIORef' (windowSetup application) (\old -> (next,old))
    when (next/=previous) $ do
        win <- readIORef (windowRef application)
        if setup then do
            setText win "setFrameAutosaveName:" ""
            size win "setMinSize:" 560 340
            size win "setContentSize:" 560 (case step of "permissions" -> 360; "ready" -> 400; "corpora" -> 500; _ -> 600)
          else do
            size win "setMinSize:" 720 540
            size win "setContentSize:" 1180 820
            name <- str "Sapiens4Workspace"
            void (booleanArgument win "setFrameUsingName:" name)
            setText win "setFrameAutosaveName:" "Sapiens4Workspace"
            void (obj0 win "center")
