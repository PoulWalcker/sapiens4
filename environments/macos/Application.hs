{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Application where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception
import Control.Monad
import Data.Aeson
import Data.Aeson.Types (parseMaybe)
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Aeson.Key as Key
import qualified Crypto.Hash.SHA256 as SHA256
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as L
import DataRoot
import DesktopStorage
import qualified Data.Text.IO as TIO
import Data.IORef
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Foreign hiding (void, new)
import Foreign.C.Error (throwErrnoIfMinus1Retry)
import Foreign.C.Types (CInt(..), CLong)
import Network.HTTP.Client hiding (method)
import Network.HTTP.Types.Status (statusCode)
import Network.URI (parseURI, URI(..))
import ObjC
import Browser
import System.Directory
import System.Environment
import System.FilePath
import System.IO
import System.Process hiding (shell)
import System.Process.Internals (withProcessHandle, ProcessHandle(waitpidLock), ProcessHandle__(..))
import System.Timeout
import System.Exit (ExitCode(..))
import System.Posix.Process (getProcessID, getProcessGroupIDOf)
import System.Posix.Signals (signalProcess, signalProcessGroup, sigKILL)
import System.Posix.Types (ProcessID, CPid(..))
import System.IO.Error (isDoesNotExistError)
import Data.List (nub, intercalate)
import Numeric (showHex)
import Text.Read (readMaybe)


data Application = Application { native :: Obj, browser :: IORef Browser, windowRef :: IORef Obj, pending :: IORef [IO ()], fixture :: Bool, child :: IORef (Maybe ServerProcess), address :: IORef Text, startup :: IORef (Maybe (Async ())), starting :: IORef Bool, localLoaded :: IORef Bool, runtime :: FilePath, selectedRoot :: IORef (Maybe FilePath), recoveryError :: IORef (Maybe Text), firstSignal :: IORef (Maybe WorkspaceSignal), attemptNumber :: IORef Integer, recovering :: IORef Bool, baselineRoot :: Maybe FilePath, clientPreferences :: Obj, quitting :: IORef Bool }

data ServerProcess = ServerProcess { serverHandle :: ProcessHandle, serverSession :: ProcessID, serverInput :: Handle, serverOutput :: Handle, serverErrors :: Async (), serverDrain :: Maybe (Async ()), serverDirectory :: FilePath }

post :: Application -> IO () -> IO ()
post application action = atomicModifyIORef' (pending application) (\actions -> (actions ++ [action],()))

check :: Application -> IO ()
check application = do
    current <- readIORef (child application)
    case current of
        Nothing -> pure ()
        Just server -> do
            alive <- not <$> serverExited (serverHandle server)
            if alive then do
                b <- readIORef (browser application)
                view <- readIORef (shell b)
                readyAddress <- readIORef (address application)
                unless (T.null readyAddress) (loadURL view readyAddress)
              else recoverStartup application "The application server stopped. Choose a folder to try again."

startServer :: Application -> DesktopRoot -> FilePath -> FilePath -> IO (ServerProcess, Text)
startServer application choice runtime destination = do
    configRoot <- configDirectory
    let binary = runtime </> ".build/bin/sapiens4"
    present <- doesFileExist binary
    unless present $ ioError (userError "The application is incomplete. Extract the complete Sapiens4 package.")
    createDirectoryIfMissing True configRoot
    environment <- getEnvironment
    pid <- getProcessID
    let toolPath = intercalate [searchPathSeparator] $ nub $
            ["/usr/bin","/bin","/usr/sbin","/sbin","/opt/homebrew/bin","/usr/local/bin"] ++
            filter isAbsolute (maybe [] splitSearchPath $ lookup "PATH" environment)
        overrides = [("PATH",toolPath),("SAPIENS_REPOSITORY",runtime),("SAPIENS_CONFIG_DIR",configRoot),("SAPIENS_DATA_DIR",destination),("SAPIENS_DESKTOP_PID",show pid),("SAPIENS_STORAGE_MODE",storageMode choice)]
    let command = (proc binary ["--port","0","--data-dir",destination,"--desktop"])
            { cwd = Just runtime, env = Just (overrides ++ filter ((`notElem` map fst overrides) . fst) environment) }
    launchServer application destination command

launchServer :: Application -> FilePath -> CreateProcess -> IO (ServerProcess, Text)
launchServer application destination command = mask $ \restore -> do
    (Just input, Just output, Just errors, process) <- createProcess command {std_in=CreatePipe, std_out=CreatePipe, std_err=CreatePipe, new_session=True, create_group=False}
    let close h = hClose h `catch` (\(_ :: IOException) -> pure ())
    let cleanup = do
            killServerGroup process
                `finally` (void (waitForProcess process) `finally` mapM_ close [input,output,errors])
    group <- (getPid process >>= maybe (ioError (userError "Could not own the application server process group.")) pure) `onException` cleanup
    reader <- (async $ (do
        hSetEncoding errors utf8
        hSetNewlineMode errors universalNewlineMode
        config <- configDirectory
        logPath <- containedPath config (config </> "desktop-server.log")
        withFile logPath AppendMode $ \logFile ->
            (forever $ do
                line <- hGetLine errors
                hPutStrLn logFile line
                hFlush logFile) `catch` (\(_ :: IOException) -> pure ())) `finally` close errors) `onException` cleanup
    let server = ServerProcess process group input output reader Nothing destination
    writeIORef (child application) (Just server)
    mapM_ (\h -> hSetEncoding h utf8 >> hSetNewlineMode h universalNewlineMode) [input,output] `onException` stopOwnedServer application
    ready <- restore (T.pack . show <$> awaitServerAddress output)
        `onException` stopOwnedServer application
    drain <- async $ (forever (void (hGetLine output))) `catch` (\(_ :: IOException) -> close output)
    let running=server {serverDrain=Just drain}
    writeIORef (child application) (Just running)
    pure (running, ready)

foreign import ccall unsafe "getsid" processSession :: ProcessID -> IO ProcessID

foreign import ccall unsafe "sapiens_process_exited" processExited :: ProcessID -> IO CInt

withServerHandle :: ProcessHandle -> (ProcessHandle__ -> IO a) -> IO a
withServerHandle process action = withMVar (waitpidLock process) $ \_ -> withProcessHandle process action

exited :: ProcessID -> IO Bool
exited pid = (/=0) <$> throwErrnoIfMinus1Retry "waitid" (processExited pid)

serverExited :: ProcessHandle -> IO Bool
serverExited process = withServerHandle process $ \state -> case state of
    OpenHandle pid -> exited pid
    _ -> pure True

serverProcesses :: ServerProcess -> IO [ProcessID]
serverProcesses server = withServerHandle (serverHandle server) $ \state -> case state of
    OpenHandle pid | pid==serverSession server -> do
        ended <- exited pid
        unless ended $ do
            session <- processSession pid
            when (session/=pid) $ do
                racedExit <- exited pid
                unless racedExit (ioError (userError "The application server no longer owns its session."))
        let session=serverSession server
        result <- timeout 2000000 (readProcessWithExitCode "/bin/ps" ["-axo","pid=,stat="] "")
        pids <- case result of
            Just (ExitSuccess,output,_) -> pure [p | line <- lines output, [raw,status] <- [words line], 'Z' `notElem` status, Just p <- [readMaybe raw]]
            _ -> ioError (userError "Could not enumerate the application server session.")
        filterM (fmap (==session) . processSession) pids
    _ -> pure []

killServerGroup :: ProcessHandle -> IO ()
killServerGroup process = withServerHandle process $ \state -> case state of
    OpenHandle pid -> do
        ended <- exited pid
        session <- processSession pid
        when (not ended && session==pid) $ do
            group <- getProcessGroupIDOf pid
            when (group==pid) (signalProcessGroup sigKILL group)
    _ -> pure ()

stopServer :: ServerProcess -> IO ()
stopServer server = mask_ $ do
    stop
    (cancel (serverErrors server) `finally` mapM_ cancel (serverDrain server))
        `finally` mapM_ (\h -> hClose h `catch` (\(_ :: IOException) -> pure ())) [serverInput server,serverOutput server]
    where
        process=serverHandle server
        input=serverInput server
        signal action = action `catch` (\(e :: IOException) -> unless (isDoesNotExistError e) (throwIO e))
        stop = do
            void $ timeout 10000000 $ do
                (hPutStrLn input "stop" >> hFlush input >> hClose input) `catch` (\(_ :: IOException) -> pure ())
                let await = do
                        ended <- serverExited process
                        unless ended (threadDelay 50000 >> await)
                await
            confirmed <- timeout 5000000 $ do
                signal (killServerGroup process)
                let sweep = do
                        owned <- serverProcesses server
                        unless (null owned) $ do
                            withServerHandle process $ \state -> case state of
                                OpenHandle root | root==serverSession server -> forM_ owned $ \pid -> do
                                    session <- processSession pid
                                    when (session==root) (signal (signalProcess sigKILL pid))
                                _ -> pure ()
                            threadDelay 50000
                            sweep
                sweep
                void (waitForProcess process)
            when (isNothing confirmed) (ioError (userError "The application server session could not be stopped."))

alert :: Text -> IO ()
alert message = void $ prompt message ["OK"]

prompt :: Text -> [Text] -> IO CLong
prompt message buttons = do
    application <- cls "NSApplication" >>= \c -> obj0 c "sharedApplication"
    void (obj1 application "setActivationPolicy:" (int 0))
    running <- booleanValue application "isRunning"
    unless running $ void (obj0 application "finishLaunching")
    void (obj1 application "activateIgnoringOtherApps:" (int 1))
    bracket (new "NSAlert") (void . (`obj0` "release")) $ \panel -> do
        setText panel "setMessageText:" "Sapiens4 could not open"
        setText panel "setInformativeText:" message
        mapM_ (\title -> str title >>= void . obj1 panel "addButtonWithTitle:") buttons
        long panel "runModal"

serverHealthy :: Application -> Text -> IO Bool
serverHealthy application destination = do
    current <- readIORef (child application)
    alive <- maybe (pure False) (fmap not . serverExited . serverHandle) current
    if not alive || T.null destination then pure False else do
        let probe = (bracket (newManager defaultManagerSettings) closeManager $ \http -> do
                let endpoint = fmap (\uri -> uri {uriPath="/api/health",uriQuery="",uriFragment=""}) (parseURI (T.unpack destination))
                request <- parseRequest (maybe "" show endpoint)
                response <- httpLbs request {responseTimeout=responseTimeoutMicro 2000000,redirectCount=0} http
                pure (statusCode (responseStatus response)==200 &&
                    (decode (responseBody response) >>= fieldStatus)==Just ("ok" :: Text)))
                `catch` (\(_ :: IOException) -> pure False)
                `catch` (\(_ :: HttpException) -> pure False)
        fromMaybe False <$> timeout 5000000 (do
            healthy <- probe
            if healthy then pure True else threadDelay 250000 >> probe)
    where fieldStatus = parseMaybe (withObject "health" (.: "status"))

reload :: Application -> IO ()
reload application = do
    closing <- readIORef (quitting application)
    unless closing $ if fixture application then readIORef (browser application) >>= readIORef . shell >>= void . (`obj0` "reload") else do
        busy <- readIORef (starting application)
        unless busy $ do
            destination <- readIORef (address application)
            current <- readIORef (child application)
            if T.null destination then loadBootstrap application else case current of
                Nothing -> recoverStartup application "The application server stopped. Choose a folder to try again."
                Just server -> do
                    writeIORef (starting application) True
                    attempt <- readIORef (attemptNumber application)
                    worker <- async $ do
                        result <- try (serverHealthy application destination)
                        postAttempt application attempt $ do
                            closing <- readIORef (quitting application)
                            unless closing $ do
                                writeIORef (starting application) False
                                case result of
                                    Left (e :: SomeException) -> recoverStartup application (T.pack (storageMessage e))
                                    Right True -> check application
                                    Right False -> startSelected application (Just ExistingRoot) (serverDirectory server)
                    writeIORef (startup application) (Just worker)

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
    on1 "tick:" $ \_ -> do
        atomicModifyIORef' (pending application) (\items -> ([],items)) >>= sequence_
        connected <- not . T.null <$> readIORef (address application)
        closing <- readIORef (quitting application)
        when (connected && not closing && not (fixture application)) $ do
            current <- readIORef (child application)
            forM_ current $ \server -> do
                ended <- serverExited (serverHandle server)
                when ended $ recoverStartup application "The application server stopped. Choose a folder to reopen your workspace."
    on1 "applicationDidFinishLaunching:" (\_ -> showWindow application)
    on0 "reloadWorkspace" (reload application)
    on0 "quit" (void (obj1 (native application) "terminate:" nullPtr))
    wrapLong1 (\_ _ _ -> do
        busy <- atomicModifyIORef' (quitting application) (\value -> (True,value))
        unless busy $ void $ async $ do
            result <- try (closeApplication application)
            post application $ do
                case result of
                    Left (e :: SomeException) -> do
                        writeIORef (quitting application) False
                        writeIORef (starting application) False
                        alert (T.pack (storageMessage e))
                    Right () -> pure ()
                void (obj1 (native application) "replyToApplicationShouldTerminate:" (flag (either (const False) (const True) result)))
        pure 2) >>= method c "applicationShouldTerminate:" "Q@:@"
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
        closing <- readIORef (quitting application)
        unless (fixture application || closing) $ do
            path <- obj0 view "URL" >>= (\u -> textProp u "path")
            when (path==T.pack (bootstrapPath application)) $ do
                loaded <- atomicModifyIORef' (localLoaded application) (\old -> (True,old))
                writeIORef (recovering application) False
                problem <- atomicModifyIORef' (recoveryError application) (\old -> (Nothing,old))
                writeIORef (starting application) False
                forM_ problem (showStorageError application)
                unless (loaded || isJust problem) $ do
                    explicit <- lookupEnv "SAPIENS_DATA_DIR"
                    selection <- try (startupDataDirectory explicit (pure $ baselineRoot application))
                    case selection of
                        Left (e :: IOException) -> showStorageError application (T.pack (storageMessage e))
                        Right Nothing -> pure ()
                        Right (Just directory) -> startSelected application (Just ExistingRoot) directory
            sh <- withBrowser (readIORef . shell)
            readyAddress <- readIORef (address application)
            trusted <- obj0 view "URL" >>= (\u -> withBrowser (sameOrigin u . origin))
            when (view==sh && trusted && path `elem` ["/workspace","/workspace/","/workspace/index.html"] && not (T.null readyAddress)) $ do
                commitWorkspace application Navigated
    let navigationFailed view err = withBrowser $ \b -> do
            sh <- readIORef (shell b)
            code <- long err "code"
            connected <- not . T.null <$> readIORef (address application)
            closing <- readIORef (quitting application)
            when (code /= -999 && not closing) $
                if view==sh && connected then recoverStartup application "We couldn't open your workspace. Your saved storage selection is unchanged. Choose a folder to try again."
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
    timer <- callTimer timerClass timerSelector 0.05 target tick nullPtr 1
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
        loadBootstrap application

runApplication :: Bool -> Text -> IO ()
runApplication isFixture address = do
    loadFrameworks
    bracket (new "NSAutoreleasePool") (\pool -> void (obj0 pool "drain")) $ \_ -> (runSession isFixture address `catch` (\(e :: SomeException) -> alert (T.pack (storageMessage e))))

runSession :: Bool -> Text -> IO ()
runSession isFixture initialAddress = do
    executable <- getExecutablePath
    let packaged = takeDirectory executable </> "../Resources/runtime"
    bundled <- doesDirectoryExist packaged
    (runtime,baseline) <- if bundled then (,Nothing) <$> canonicalizePath packaged else do
        bundle <- cls "NSBundle" >>= (`obj0` "mainBundle")
        resource <- str "Launcher"; extension <- str "plist"
        path <- obj2 bundle "pathForResource:ofType:" resource extension
        when (takeExtension (takeDirectory (takeDirectory (takeDirectory executable))) == ".app" && path == nullPtr) $
            ioError (userError "The installed app is missing its runtime. Reinstall the complete Sapiens4 package.")
        configuration <- cls "NSDictionary" >>= \c -> obj1 c "dictionaryWithContentsOfFile:" path
        key <- str "Repository"
        root <- obj1 configuration "objectForKey:" key >>= string
        pathKey <- str "Path"
        search <- obj1 configuration "objectForKey:" pathKey >>= string
        unless (T.null search) (setEnv "PATH" (T.unpack search))
        directory <- canonicalizePath =<< if T.null root then getCurrentDirectory else pure (T.unpack root)
        source <- doesFileExist (directory </> "corpora/Main.hs")
        unless source $ ioError (userError "The source launcher cannot find its checkout. Reinstall the complete Sapiens4 app.")
        dataKey <- str "DataDirectory"
        previous <- obj1 configuration "objectForKey:" dataKey >>= string
        pure (directory, Just (if T.null previous then directory </> ".sapiens4" else T.unpack previous))
    application <- cls "NSApplication" >>= \c -> obj0 c "sharedApplication"
    http <- newManager defaultManagerSettings
    guestScript <- if isFixture then
        (do r <- parseRequest (T.unpack (T.dropWhileEnd (== '/') initialAddress <> "/workspace/guest.js")); response <- httpLbs r http; pure (TE.decodeUtf8With (\_ _ -> Just '\xfffd') (L.toStrict (responseBody response)))) `catch` (\(_ :: HttpException) -> pure "")
      else pure ""
    b <- newBrowser (if isFixture then initialAddress else "about:blank") guestScript >>= newIORef
    win <- newIORef nullPtr; tasks <- newIORef []
    process <- newIORef Nothing; address <- newIORef (if isFixture then initialAddress else "")
    worker <- newIORef Nothing; busy <- newIORef False; loaded <- newIORef False; selection <- newIORef Nothing
    signal <- newIORef Nothing; generation <- newIORef 0; recovery <- newIORef False
    problem <- newIORef Nothing
    closing <- newIORef False
    preferences <- if isFixture then pure nullPtr else do
        config <- configDirectory >>= canonicalizePath
        let hex byte = let value=showHex byte "" in replicate (2-length value) '0' ++ value
            suite="com.sapiens4.client." <> T.pack (concatMap hex (BS.unpack (SHA256.hash (TE.encodeUtf8 (T.pack config)))))
        allocated "NSUserDefaults" >>= \o -> str suite >>= obj1 o "initWithSuiteName:"
    let state = Application application b win tasks isFixture process address worker busy loaded runtime selection problem signal generation recovery baseline preferences closing
    (do
        target <- installMethods state
        initialize state target initialAddress
        void (obj0 application "run")) `finally` do
        closeApplication state `finally` do
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
    if connected || fixture application then do
        current <- readIORef (child application)
        live <- maybe (pure $ fixture application) (fmap not . serverExited . serverHandle) current
        if live then receive b message (saveClientPreferences application) (commitWorkspace application WorkspaceReady)
          else recoverStartup application "The application server stopped. Choose a folder to reopen your workspace."
      else do
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
                "new" -> (defaultDataDirectory >>= startSelected application (Just NewRoot))
                    `catch` \(e :: IOException) -> showStorageError application (T.pack (storageMessage e))
                "pick" -> do
                    busy <- atomicModifyIORef' (starting application) (\old -> (True,old))
                    unless busy $ do
                        selected <- chooseDataDirectory `finally` writeIORef (starting application) False
                        script sh ("window.storagePicked?.(" <> jsonText (maybe Null (String . T.pack) selected) <> ")")
                "existing" -> do
                    let directory=T.unpack (text (field "directory" input))
                    if isAbsolute directory then startSelected application Nothing directory
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

readClientPreferences :: Application -> FilePath -> IO (KeyMap.KeyMap Value)
readClientPreferences application directory = do
    value <- str (T.pack directory) >>= obj1 (clientPreferences application) "stringForKey:" >>= string
    pure (fromMaybe KeyMap.empty (decodeStrict (TE.encodeUtf8 value)))

saveClientPreferences :: Application -> Value -> IO ()
saveClientPreferences application input = unless (fixture application) $ do
    active <- readIORef (child application)
    forM_ active $ \server -> do
        let name=text (field "key" input); value=text (field "value" input)
        when (validPreference name value) $ do
            previous <- readClientPreferences application (serverDirectory server)
            let stored=Object (KeyMap.insert (Key.fromText name) (String value) previous)
            content <- str (jsonText stored)
            nameObject <- str (T.pack (serverDirectory server))
            void (obj2 (clientPreferences application) "setObject:forKey:" content nameObject)

installClientPreferences :: Application -> Obj -> Text -> FilePath -> IO ()
installClientPreferences application view readyAddress directory = unless (fixture application) $ do
    saved <- readClientPreferences application directory
    source <- preferenceScript (runtime application) readyAddress directory (Object saved)
    user <- obj0 view "configuration" >>= (`obj0` "userContentController")
    void (obj0 user "removeAllUserScripts")
    content <- str source
    injection <- allocated "WKUserScript" >>= \o -> obj3 o "initWithSource:injectionTime:forMainFrameOnly:" content (int 0) (int 1)
    void (obj1 user "addUserScript:" injection)

stopOwnedServer :: Application -> IO ()
stopOwnedServer application = mask_ $ do
    current <- readIORef (child application)
    mapM_ stopServer current
    writeIORef (child application) Nothing

replaceBrowser :: Application -> Text -> Text -> IO Obj
replaceBrowser application destination guestScript = do
    previous <- readIORef (browser application)
    view <- readIORef (shell previous)
    sync previous (object [])
    next <- newBrowser destination guestScript
    readIORef (delegate previous) >>= writeIORef (delegate next)
    writeIORef (shell next) view
    writeIORef (browser application) next
    void (obj0 (origin previous) "release")
    pure view

installServer :: Application -> Text -> Text -> FilePath -> IO ()
installServer application readyAddress guestScript directory = do
    closing <- readIORef (quitting application)
    unless closing $ do
        view <- replaceBrowser application readyAddress guestScript
        writeIORef (firstSignal application) Nothing
        writeIORef (selectedRoot application) (Just directory)
        installClientPreferences application view readyAddress directory
        loadURL view readyAddress
        attempt <- readIORef (attemptNumber application)
        worker <- async $ threadDelay 35000000 >> postAttempt application attempt (do
            selection <- readIORef (selectedRoot application)
            when (selection==Just directory) $ recoverStartup application "Your workspace did not load within 35 seconds. Your saved storage selection is unchanged.")
        writeIORef (startup application) (Just worker)

recoverStartup :: Application -> Text -> IO ()
recoverStartup application message = do
    active <- atomicModifyIORef' (recovering application) (\old -> (True,old))
    closing <- readIORef (quitting application)
    unless (closing || active) $ do
        modifyIORef' (attemptNumber application) (+1)
        writeIORef (starting application) True
        writeIORef (address application) ""
        atomicModifyIORef' (startup application) (\old -> (Nothing,old)) >>= mapM_ cancel
        writeIORef (selectedRoot application) Nothing
        worker <- async $ do
            stopped <- try (stopOwnedServer application)
            let detail = case stopped of
                    Left (e :: SomeException) -> message <> " Cleanup failed: " <> T.pack (storageMessage e)
                    Right () -> message
            post application $ do
                writeIORef (address application) ""
                void (replaceBrowser application "about:blank" "")
                writeIORef (recoveryError application) (Just detail)
                writeIORef (localLoaded application) True
                win <- readIORef (windowRef application)
                setText win "setFrameAutosaveName:" ""
                size win "setMinSize:" 560 340
                size win "setContentSize:" 560 600
                loadBootstrap application
        writeIORef (startup application) (Just worker)

startSelected :: Application -> Maybe DesktopRoot -> FilePath -> IO ()
startSelected application requestedChoice directory = mask $ \unmask -> do
    closing <- readIORef (quitting application)
    busy <- atomicModifyIORef' (starting application) (\old -> (True,old))
    unless (closing || busy) $ do
        attempt <- atomicModifyIORef' (attemptNumber application) (\old -> (old+1,old+1))
        let postCurrent = postAttempt application attempt
        writeIORef (address application) ""
        b <- readIORef (browser application)
        view <- readIORef (shell b)
        script view "window.storageLoading?.()"
        worker <- async $ handle (\(e :: SomeException) -> case fromException e :: Maybe SomeAsyncException of
            Just _ -> throwIO e
            Nothing -> postCurrent (recoverStartup application (T.pack (storageMessage e)))) $ unmask $ do
                stopOwnedServer application
                selected <- canonicalizePath directory
                choice <- maybe (storageChoiceForDirectory selected) pure requestedChoice
                guestScript <- TIO.readFile (runtime application </> ".build/web/guest.js")
                when (T.null (T.strip guestScript)) $ ioError (userError "The packaged guest.js is empty. Reinstall the complete Sapiens4 app.")
                mask $ \restore -> do
                    (_,readyAddress) <- restore (startServer application choice (runtime application) selected)
                    postCurrent $ do
                        writeIORef (address application) readyAddress
                        installServer application readyAddress guestScript selected
                            `catch` (\(e :: SomeException) -> recoverStartup application (T.pack (storageMessage e)))
        writeIORef (startup application) (Just worker)

postAttempt :: Application -> Integer -> IO () -> IO ()
postAttempt application attempt action = post application $ do
    current <- readIORef (attemptNumber application)
    closing <- readIORef (quitting application)
    when (current==attempt && not closing) action

commitWorkspace :: Application -> WorkspaceSignal -> IO ()
commitWorkspace application signal =
    saveWhenReady (firstSignal application) signal (selectedRoot application) id (recoverStartup application . T.pack) $ \_ -> do
        atomicModifyIORef' (startup application) (\old -> (Nothing,old)) >>= mapM_ cancel
        writeIORef (starting application) False
        workspaceWindow application

workspaceWindow :: Application -> IO ()
workspaceWindow application = do
    win <- readIORef (windowRef application)
    size win "setMinSize:" 720 540
    size win "setContentSize:" 1180 820
    name <- str "Sapiens4Workspace"
    void (booleanArgument win "setFrameUsingName:" name)
    setText win "setFrameAutosaveName:" "Sapiens4Workspace"
