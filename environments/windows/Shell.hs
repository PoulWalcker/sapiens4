{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Shell (runDesktop) where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception
import Control.Monad
import Data.Aeson hiding (Object)
import qualified Data.Aeson as JSON
import Data.Bits
import Data.IORef
import Data.Int
import Data.Maybe (isNothing, listToMaybe)
import Data.Word
import Types (readJSON, writeJSON)
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Aeson.Key as K
import qualified Crypto.Hash.SHA256 as SHA
import DataRoot
import qualified DesktopStorage
import Network.URI (URI(..), URIAuth(..), parseURI, escapeURIString, isUnreserved, unEscapeString)
import System.Directory (canonicalizePath, doesFileExist, doesDirectoryExist)
import Foreign hiding (void)
import Numeric
import Browser
import Host
import Native
import qualified Navigation as Navigation
import System.Environment
import System.FilePath
import System.IO
import System.Process (createProcess, proc)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as L
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

runDesktop :: Bool -> IO ()
runDesktop probe = runInBoundThread $
  -- Release callbacks only after COM has finished releasing its references.
  (bracket newArena freeArena $ \memory ->
    bracket_ (check =<< coInitialize nullPtr 2) coUninitialize (desktopSession memory probe))
  `catch` \(e :: SomeException) -> case fromException e :: Maybe SomeAsyncException of
    Just _ -> throwIO e
    Nothing -> withWide (storageMessage e ++ "\n\nClose the application and relaunch after correcting the error.") $ \message ->
      withWide "Sapiens4 could not start" $ \title -> void (messageBox nullPtr message title 0x10)

desktopSession :: Arena -> Bool -> IO ()
desktopSession memory probe = do
  void $ setDpiAwareness (-4)
  args <- desktopArgs <$> getArgs
  host <- prepareHost args
  configRoot <- configDirectory
  dataEnv <- lookupEnv "SAPIENS_DATA_DIR"
  let explicit = listToMaybe [value | (key,value) <- zip args (drop 1 args), key=="--data-dir"] `mplus` dataEnv
  saved <- try (startupDataDirectory explicit) :: IO (Either IOException (Maybe FilePath))
  shellProfile <- containedPath configRoot (configRoot </> "WebView2/Shell")
  activeRoot <- newIORef (dataDirectory host)
  bootstrapPath <- canonicalizePath (runtimeDirectory host </> "app/shell/bootstrap.html")
  let filePath = '/' : map (\c -> if c=='\\' then '/' else c) bootstrapPath
      bootstrapURL = "file://" ++ escapeURIString (\c -> isUnreserved c || c `elem` ("/:" :: String)) filePath
      bootstrapSource address = case parseURI address of
        Just uri -> uriScheme uri=="file:" && maybe True (\auth -> null (uriRegName auth) && null (uriUserInfo auth) && null (uriPort auth)) (uriAuthority uri)
          && equalFilePath (unEscapeString $ uriPath uri) filePath && null (uriQuery uri) && null (uriFragment uri)
        Nothing -> False
  initialStorage <- newIORef True
  recoveryError <- newIORef (Nothing :: Maybe String)
  top <- newIORef 0
  pending <- newIORef []
  legacyView <- newIORef False
  shellRef <- newIORef Nothing
  browserRef <- newIORef Nothing
  statusRef <- newIORef nullPtr
  focusRef <- newIORef nullPtr
  closing <- newIORef False
  workerRef <- newIORef Nothing
  starting <- newIORef False
  generation <- newIORef (0 :: Integer)
  firstSignal <- newIORef Nothing
  storageAttempt <- newIORef (0 :: Integer)
  selectedRoot <- newIORef Nothing
  preferenceScript <- newIORef Nothing
  preferenceValues <- newIORef KM.empty
  let resize hwnd = do
        (width, height) <- rectangle hwnd
        offset <- readIORef top
        readIORef shellRef >>= mapM_ (\view -> bounds view 0 offset width (max 0 $ height - offset))
        readIORef browserRef >>= mapM_ positionGuest
        label <- readIORef statusRef
        when (label /= nullPtr) $ void $ setWindowPos label nullPtr 0 0 (fromIntegral width) (fromIntegral height) 4
      stop = do
        writeIORef closing True
        readIORef workerRef >>= mapM_ cancel
        closeHost host
      closeViews =
        (atomicModifyIORef' browserRef (\value -> (Nothing,value)) >>= mapM_ closeBrowser) `finally`
          (atomicModifyIORef' shellRef (\value -> (Nothing,value)) >>= mapM_ closeView)
      runPending = do
        actions <- atomicModifyIORef' pending (\actions -> ([], reverse actions))
        sequence_ actions
      procedure hwnd message wparam lparam = case message of
        0x06 | wparam .&. 0xffff == 0 -> focusedWindow >>= writeIORef focusRef >> defaultProc hwnd message wparam lparam
        0x07 -> do
          -- Reactivation otherwise focuses the outer window and drops typed input.
          dead <- readIORef closing
          unless dead $ do
            previous <- readIORef focusRef
            browser <- readIORef browserRef
            maybe (readIORef shellRef >>= mapM_ (\view -> callInt (controller view) 12 0)) (`focusBrowser` previous) browser
          pure 0
        0x24 -> do
          dpi <- windowDpi hwnd
          let scaled value = round (fromIntegral value * fromIntegral (max 96 dpi) / (96 :: Double)) :: Int32
          pokeByteOff (intPtrToPtr lparam) 24 (scaled (860 :: Int))
          pokeByteOff (intPtrToPtr lparam) 28 (scaled (600 :: Int))
          pure 0
        0x05 -> resize hwnd >> pure 0
        0x47 -> do
          readIORef shellRef >>= mapM_ (\view -> call0 (controller view) 23)
          resize hwnd
          defaultProc hwnd message wparam lparam
        0x10 -> do
          busy <- atomicModifyIORef' closing (\value -> (True,value))
          unless busy $ do
            readIORef starting >>= \inStartup -> when inStartup $ modifyIORef' generation (+1)
            void $ async $ do
              result <- try stop
              let complete = case result of
                    Right () -> closeViews >> void (destroyWindow hwnd)
                    Left (e :: SomeException) -> do
                      writeIORef closing False
                      writeIORef starting False
                      label <- readIORef statusRef
                      windowTitle label ("Quit cancelled: " ++ storageMessage e)
                      void (showWindow label 5)
              atomicModifyIORef' pending (\actions -> (complete:actions,()))
              void (postMessage hwnd 0x802A 0 0)
          pure 0
        0x02 -> quitMessage 0 >> pure 0
        0x0111 -> case wparam .&. 0xffff of
          101 -> void (createProcess (proc "rundll32.exe" ["url.dll,FileProtocolHandler", "https://go.microsoft.com/fwlink/p/?LinkId=2124703"])) >> pure 0
          103 -> void (sendMessage hwnd 0x10 0 0) >> pure 0
          _ -> defaultProc hwnd message wparam lparam
        0x802A -> runPending >> pure 0
        _ -> defaultProc hwnd message wparam lparam
  flip finally (stop >> closeViews) $ do
    proc <- wrapWindowProc $ \hwnd message wparam lparam -> procedure hwnd message wparam lparam `catch` \(e :: SomeException) -> reportException e >> defaultProc hwnd message wparam lparam
    retain memory (freeHaskellFunPtr proc)
    instanceHandle <- moduleHandle nullPtr
    hwnd <- createMainWindow memory instanceHandle proc
    flip finally (void $ destroyWindow hwnd) $ do
      resizeContent hwnd 1440 960
      label <- withWide "STATIC" $ \className -> withWide "Starting Sapiens4…" $ \title ->
        createWindow 0 className title 0x50000001 0 0 1440 960 hwnd nullPtr instanceHandle nullPtr
      setupButtons <- forM [(101,"Install WebView2"),(103,"Close")] $ \(ident,title) ->
        withWide "BUTTON" $ \className -> withWide title $ \caption ->
          createWindow 0 className caption 0x40010000 (20 + (ident-101)*160) 230 150 38 hwnd (intPtrToPtr (fromIntegral ident)) instanceHandle nullPtr
      writeIORef statusRef label
      resize hwnd
      light <- loadIconFile $ runtimeDirectory host </> "environments/windows/AppIcon.ico"
      dark <- loadIconFile $ runtimeDirectory host </> "environments/windows/AppIconDark.ico"
      mapM_ (\icon -> when (icon /= nullPtr) $ retain memory (void $ destroyIcon icon)) [light, dark]
      setIcon hwnd light
      lastTheme <- newIORef Nothing
      let post action = do
            dead <- readIORef closing
            let guarded = readIORef closing >>= \closed -> unless closed action
            unless dead $ atomicModifyIORef' pending (\xs -> (guarded:xs, ())) >> void (postMessage hwnd 0x802A 0 0)
          failStart message = do
            modifyIORef' generation (+1)
            cleanup <- try (closeViews `finally` closeHost host)
            let detail = case cleanup of
                  Left (e :: SomeException) -> message ++ "\n\nStartup cleanup failed: " ++ storageMessage e
                  Right () -> message
            writeIORef starting False
            writeIORef focusRef nullPtr
            windowTitle label $ "Sapiens4 could not start.\n\n" ++ detail ++ "\n\nCorrect the error, close the application and relaunch.\nData: " ++ dataDirectory host
            void $ showWindow label 5
            mapM_ (\button -> void (showWindow button 5)) setupButtons
          storageScript source = readIORef shellRef >>= mapM_ (\view -> execute memory view source (const $ pure ()))
          storageError :: String -> IO ()
          storageError message = do
            closeHost host
            writeIORef starting False
            storageScript ("window.storageError(" ++ T.unpack (TE.decodeUtf8 $ L.toStrict $ encode message) ++ ")")
          recoverStartup message = do
            modifyIORef' storageAttempt (+1)
            writeIORef (origin host) Nothing
            atomicModifyIORef' workerRef (\old -> (Nothing,old)) >>= mapM_ cancel
            writeIORef selectedRoot Nothing
            writeIORef starting True
            worker <- async $ do
              cleanup <- try (closeHost host)
              let detail = case cleanup of
                    Left (e :: SomeException) -> message ++ " Cleanup failed: " ++ storageMessage e
                    Right () -> message
              post $ do
                writeIORef (origin host) Nothing
                atomicModifyIORef' browserRef (\value -> (Nothing,value)) >>= mapM_ closeBrowser
                writeIORef initialStorage False
                writeIORef recoveryError (Just detail)
                usedLegacy <- atomicModifyIORef' legacyView (\old -> (False,old))
                if usedLegacy then do
                  closeViews
                  writeIORef starting False
                  begin
                else readIORef shellRef >>= mapM_ (\view -> navigate view bootstrapURL)
            writeIORef workerRef (Just worker)
          preferencesPath directory = configRoot </> "client-preferences" </> concatMap (\byte -> let h=showHex byte "" in replicate (2-length h) '0' ++ h) (BS.unpack $ SHA.hash $ TE.encodeUtf8 $ T.pack directory) <.> "json"
          savePreferences input = do
            selection <- readIORef selectedRoot
            browser <- readIORef browserRef
            when (isNothing selection && not (isNothing browser)) $ do
              let key=string (field "key" input); value=string (field "value" input)
              when (DesktopStorage.validPreference (T.pack key) (T.pack value)) $ do
                directory <- readIORef activeRoot
                values <- readIORef preferenceValues
                let next=KM.insert (K.fromString key) (String $ T.pack value) values
                writeJSON (preferencesPath directory) next
                writeIORef preferenceValues next
          commitWorkspace signal =
            DesktopStorage.saveWhenReady firstSignal signal selectedRoot fst recoverStartup $ \(_,url) -> do
              atomicModifyIORef' workerRef (\old -> (Nothing,old)) >>= mapM_ cancel
              writeIORef starting False
              storageScript "window.dispatchEvent(new Event('sapiens-preferences-ready'))"
              when probe $ putStrLn "TITLE: Sapiens4" >> putStrLn ("Sapiens4: " ++ url)
          installStorage selected url = do
            attempt <- readIORef storageAttempt
            let current action = readIORef storageAttempt >>= \n -> when (n==attempt) action
            writeIORef firstSignal Nothing
            writeIORef selectedRoot (Just (dataDirectory selected,url))
            watchHost selected (post . current . recoverStartup)
            async (threadDelay 35000000 >> post (current $ do
              selection <- readIORef selectedRoot
              when (selection==Just (dataDirectory selected,url)) $ recoverStartup "Your workspace did not load within 35 seconds. Your saved storage selection is unchanged.")) >>= writeIORef workerRef . Just
            bootstrap <- readIORef shellRef >>= maybe (ioError $ userError "The application window closed.") pure
            let root = dataDirectory selected
            profile <- containedPath root (root </> "WebView2/Shell")
            canReuse <- doesDirectoryExist (profile </> "EBWebView")
            if not canReuse || equalFilePath shellProfile profile then installWorkspace selected url current bootstrap else do
              void (containedPath root (profile </> "EBWebView"))
              let valid = (&&) <$> (not <$> readIORef closing) <*> ((==attempt) <$> readIORef storageAttempt)
              gen <- readIORef generation
              let opened result = case result of
                    Left _ -> current (installWorkspace selected url current bootstrap)
                    Right view -> do
                      active <- valid
                      if not active then closeView view else do
                        closeView bootstrap
                        writeIORef legacyView True
                        loaded gen (installWorkspace selected url current) (Right view)
                  failed (e :: SomeException) = current $ recoverStartup (storageMessage e)
              newViewWhile valid memory hwnd profile (\result -> opened result `catch` failed)
                `catch` \(_ :: IOException) -> current (installWorkspace selected url current bootstrap)
          installWorkspace selected url current view = do
            browser <- newBrowser memory hwnd view selected top
            writeIORef browserRef (Just browser)
            writeIORef activeRoot (dataDirectory selected)
            values <- (KM.map String <$> (readJSON (preferencesPath $ dataDirectory selected) KM.empty :: IO (KM.KeyMap T.Text)))
              `catch` (\(e :: IOException) -> reportException (toException e) >> pure KM.empty)
            writeIORef preferenceValues values
            oldScript <- atomicModifyIORef' preferenceScript (\old -> (Nothing,old))
            mapM_ (\identity -> withWide identity $ \name -> callPtr (core view) 28 name) oldScript
            source <- DesktopStorage.preferenceScript (runtimeDirectory host) (T.pack url) (dataDirectory selected) (JSON.Object values)
            addDocumentScript memory view (T.unpack source) $ \result -> current $ do
              dead <- readIORef closing
              selection <- readIORef selectedRoot
              unless (dead || selection/=Just (dataDirectory selected,url)) $ case result of
                Left message -> recoverStartup message
                Right identity -> do
                  writeIORef preferenceScript (Just identity)
                  navigate view url
          startSelected requestedChoice directory = mask $ \unmask -> do
            busy <- atomicModifyIORef' starting (\value -> (True,value))
            unless busy $ do
              modifyIORef' storageAttempt (+1)
              storageScript "window.storageLoading?.()"
              worker <- async $ handle (\(e :: SomeException) -> case fromException e :: Maybe SomeAsyncException of
                Just _ -> throwIO e
                Nothing -> post (recoverStartup $ storageMessage e)) $ unmask $ do
                  closeHost host
                  selectedDirectory <- canonicalizePath directory
                  choice <- maybe (storageChoiceForDirectory selectedDirectory) pure requestedChoice
                  let selected = host {dataDirectory=selectedDirectory}
                  guestScript <- B.readFile (runtimeDirectory host </> ".build/web/guest.js")
                  when (B.all (`elem` (" \t\r\n" :: String)) guestScript) $ ioError $ userError "The packaged guest.js is empty. Reinstall the complete Sapiens4 app."
                  url <- startHost choice selected
                  post $ installStorage selected url `catch` (\(e :: SomeException) -> recoverStartup $ storageMessage e)
              writeIORef workerRef (Just worker)
          chooseStorage message = do
            busy <- readIORef starting
            unless busy $ case field "storage" message of
              String "new" -> startSelected (Just StartNew) (dataDirectory host)
              String "pick" -> do
                picking <- atomicModifyIORef' starting (\value -> (True,value))
                unless picking $ do
                  post $ do
                    result <- try (pickDirectory hwnd) `finally` writeIORef starting False
                    case result of
                      Left (e :: SomeException) -> storageError (storageMessage e)
                      Right selected -> storageScript ("window.storagePicked?.(" ++ T.unpack (TE.decodeUtf8 $ L.toStrict $ encode selected) ++ ")")
              String "existing" -> do
                let directory=string (field "directory" message)
                if isAbsolute directory then startSelected Nothing directory
                  else storageError "Choose a folder before continuing."
              _ -> pure ()
          loaded attempt onLoaded result = do
            view <- either (ioError . userError) pure result
            dead <- readIORef closing
            currentAttempt <- readIORef generation
            if dead || currentAttempt/=attempt then closeView view else do
              writeIORef shellRef (Just view)
              resize hwnd
              settings <- getValue (core view) 3
              mapM_ (\index -> callInt settings index 0) [10, 12, 14, 16]
              release settings
              let active action = do
                    current <- readIORef shellRef
                    dead <- readIORef closing
                    currentAttempt <- readIORef generation
                    when (not dead && currentAttempt==attempt && maybe False ((==core view) . core) current) action
                  on index iid action = onEvent memory (core view) index iid (\event -> active (action event))
              on 7 "{9ADBE429-F36D-432B-9DDC-F8881FBD76E3}" $ \event -> do
                destination <- getString event 3
                base <- readIORef (origin host)
                unless (Navigation.trusted base destination || isNothing base && bootstrapSource destination) $ callInt event 8 1
                when probe $ (getValue event 9 :: IO Word64) >>= \identity -> putStrLn $ "NAVIGATION START " ++ show identity
              on 44 "{D4C185FE-C81C-4989-97AF-2D3FA7AB5651}" $ \event -> do
                callInt event 6 1
                destination <- getString event 3
                base <- readIORef (origin host)
                when (Navigation.guest base destination) $ readIORef browserRef >>= mapM_ (\browser -> openBrowser browser destination)
              on 34 "{57213F19-00E6-49FA-8E07-898EA01ECBD2}" $ \event -> do
                source <- getString event 3
                current <- getString (core view) 4
                base <- readIORef (origin host)
                raw <- getString event 4
                case decodeStrict (TE.encodeUtf8 $ T.pack raw) of
                  Nothing -> pure ()
                  Just message
                    | isNothing base && bootstrapSource source && bootstrapSource current -> chooseStorage message
                    | Navigation.trusted base source && Navigation.trusted base current -> do
                        when (field "workspaceReady" message==Bool True) $ commitWorkspace DesktopStorage.WorkspaceReady
                        applyWindowTheme hwnd light dark lastTheme message
                        case field "clientPreferences" message of
                          input@(JSON.Object _) -> savePreferences input
                          _ -> readIORef browserRef >>= mapM_ (\browser -> syncBrowser browser message)
                    | otherwise -> pure ()
              on 15 "{D33A35BF-1C49-4F98-93AB-006E0533FE1C}" $ \event -> do
                success <- getValue event 3 :: IO Int32
                code <- getValue event 4 :: IO Int32
                if success /= 0 then do
                  void $ showWindow label 0
                  current <- getString (core view) 4
                  if bootstrapSource current then do
                    atomicModifyIORef' workerRef (\old -> (Nothing,old)) >>= mapM_ cancel
                    recovered <- atomicModifyIORef' recoveryError (\value -> (Nothing,value))
                    writeIORef starting False
                    case recovered of
                      Just message -> storageScript ("window.storageError(" ++ jsonString message ++ ")")
                      Nothing -> do
                        first <- atomicModifyIORef' initialStorage (\value -> (False,value))
                        unless first $ storageScript "window.showStorageSetup?.()"
                        when first $ do
                          case saved of
                            Left e -> storageError (storageMessage e)
                            Right Nothing -> storageScript "window.showStorageSetup?.()"
                            Right (Just directory) -> startSelected (Just AttachExisting) directory
                  else do
                    selection <- readIORef selectedRoot
                    forM_ selection $ \(directory,url) -> when (current==url) $ do
                      commitWorkspace DesktopStorage.Navigated
                else when (code/=14) $ post $ active $ do
                  base <- readIORef (origin host)
                  case base of
                    Nothing -> failStart "We couldn't open the storage screen. Close and relaunch. Your saved data is unchanged."
                    Just _ -> recoverStartup "We couldn't open your workspace. Your saved storage selection is unchanged. Choose a folder to try again."
                when probe $ do
                  identity <- getValue event 5 :: IO Word64
                  putStrLn $ "NAVIGATION END " ++ show identity ++ " " ++ (if success /= 0 then "True" else "False") ++ " " ++ show code
              onLoaded view
              callInt (controller view) 12 0
              when probe $ void $ forkIO $ probeInput post hwnd top resize
          launch attempt = do
            let valid = (&&) <$> (not <$> readIORef closing) <*> ((==attempt) <$> readIORef generation)
                active action = valid >>= flip when action
                failed message = post $ active $ failStart message
            complete <- and <$> mapM doesFileExist [bootstrapPath, runtimeDirectory host </> ".build/bin/sapiens4.exe"]
            if not complete then failed "Some Sapiens4 application files are missing. Extract the complete downloaded application and relaunch. Your saved data is unchanged."
            else do
              available <- try webViewRuntimeAvailable
              case available of
                Left (e :: SomeException) -> failed (storageMessage e)
                Right False -> post $ active $ do
                  windowTitle label "Microsoft Edge WebView2 is required.\n\nChoose Install WebView2 to open Microsoft's official installer.\nAfter installation, close and relaunch Sapiens4."
                  mapM_ (\button -> void (showWindow button 5)) setupButtons
                  writeIORef starting False
                Right True -> post $ active $ do
                  timer <- async $ do
                    threadDelay 35000000
                    failed "Sapiens4 did not finish loading the storage screen within 35 seconds."
                  writeIORef workerRef (Just timer)
                  newViewWhile valid memory hwnd shellProfile (\created -> loaded attempt (\view -> navigate view bootstrapURL) created `catch` \(e :: SomeException) -> failed (storageMessage e)) `catch` \(e :: SomeException) -> failStart (storageMessage e)
          begin = mask_ $ do
            dead <- readIORef closing
            unless dead $ do
              busy <- atomicModifyIORef' starting (\value -> (True,value))
              unless busy $ do
                mapM_ (\button -> void (showWindow button 0)) setupButtons
                windowTitle label "Starting Sapiens4…"
                attempt <- atomicModifyIORef' generation (\value -> (value+1,value+1))
                asyncWithUnmask (\unmask -> unmask (launch attempt)) >>= writeIORef workerRef . Just
      hSetBuffering stdout LineBuffering
      begin
      void $ showWindow hwnd 5
      void $ updateWindow hwnd
      void $ foregroundWindow hwnd
      pumpMessages memory

createMainWindow :: Arena -> Object -> FunPtr WindowProc -> IO Window
createMainWindow memory instanceHandle procedure = do
  hwnd <- withWide "Sapiens4HaskellWindow" $ \className -> withWide "Sapiens4" $ \title -> allocaBytes 80 $ \klass -> do
    fillBytes klass 0 80
    pokeByteOff klass 0 (80 :: Word32)
    pokeByteOff klass 4 (3 :: Word32)
    pokeByteOff klass 8 procedure
    pokeByteOff klass 24 instanceHandle
    pokeByteOff klass 48 (intPtrToPtr 6 :: Object)
    pokeByteOff klass 64 className
    registered <- registerClass klass
    when (registered == 0) $ ioError $ userError "Cannot register desktop window"
    retain memory (withWide "Sapiens4HaskellWindow" $ \name -> void $ unregisterClass name instanceHandle)
    (left,upper,screenWidth,screenHeight) <- workArea
    let width=min 1440 screenWidth; height=min 960 screenHeight
    createWindow 0 className title 0x00CF0000 (left+(screenWidth-width) `div` 2) (upper+(screenHeight-height) `div` 2) width height nullPtr nullPtr instanceHandle nullPtr
  when (hwnd == nullPtr) $ ioError $ userError "Cannot create desktop window"
  pure hwnd

resizeContent :: Window -> Int -> Int -> IO ()
resizeContent hwnd width height = do
  dpi <- windowDpi hwnd
  (clientWidth,clientHeight) <- rectangle hwnd
  (workLeft,workTop,screenWidth,screenHeight) <- workArea
  allocaArray 4 $ \rect -> do
    void $ windowRect hwnd rect
    [left,upper,right,lower] <- peekArray 4 rect
    let scaled value = round (fromIntegral value * fromIntegral (max 96 dpi) / (96 :: Double)) :: Int32
        outerWidth = min screenWidth (scaled width + right-left-fromIntegral clientWidth)
        outerHeight = min screenHeight (scaled height + lower-upper-fromIntegral clientHeight)
    void $ setWindowPos hwnd nullPtr (max workLeft $ min (workLeft+screenWidth-outerWidth) $ (left+right-outerWidth) `div` 2) (max workTop $ min (workTop+screenHeight-outerHeight) $ (upper+lower-outerHeight) `div` 2) outerWidth outerHeight 4

pumpMessages :: Arena -> IO ()
pumpMessages memory = allocaBytes 48 $ \message -> do
  let step = do
        result <- getMessage message nullPtr 0 0
        when (result<0) $ ioError $ userError "Cannot read desktop window messages"
        when (result > 0) $ do
          void $ translateMessage message
          void $ dispatchMessage message
          sweepArena memory
          step
  step

loadIconFile :: FilePath -> IO Object
loadIconFile path = withWide path $ \file -> loadImage nullPtr file 1 0 0 0x10

setIcon :: Window -> Object -> IO ()
setIcon hwnd icon = when (icon /= nullPtr) $ mapM_ (\kind -> void $ sendMessage hwnd 0x80 kind (ptrToIntPtr icon)) [0, 1]

data WindowTheme = WindowTheme { darkMode :: Bool, captionColor :: Int32, textColor :: Int32 } deriving Eq

decodeWindowTheme :: Value -> Maybe WindowTheme
decodeWindowTheme message = case field "dark" message of
  Bool mode -> Just $ WindowTheme mode
    (color $ palette "captionBackground" (if mode then "#171717" else "#f6f6f6") message)
    (color $ palette "foreground" (if mode then "#ececec" else "#202020") message)
  _ -> Nothing
  where
    color value = case readHex (drop 1 value) of
      [(rgb, "")] -> (rgb .&. 255) `shiftL` 16 .|. (rgb .&. 0xff00) .|. ((rgb `shiftR` 16) .&. 255)
      _ -> 0

applyWindowTheme :: Window -> Object -> Object -> IORef (Maybe WindowTheme) -> Value -> IO ()
applyWindowTheme hwnd light dark previous message = forM_ (decodeWindowTheme message) $ \current -> do
    let mode = darkMode current
        attribute index value = with value $ \p -> dwmAttribute hwnd index p 4
    old <- readIORef previous
    when (old /= Just current) $ do
      setIcon hwnd (if mode then dark else light)
      result <- attribute 20 (if mode then 1 else 0)
      when (result < 0) $ void $ attribute 19 (if mode then 1 else 0)
      void $ attribute 35 (captionColor current)
      void $ attribute 36 (textColor current)
      writeIORef previous (Just current)

probeInput :: (IO () -> IO ()) -> Window -> IORef Int -> (Window -> IO ()) -> IO ()
probeInput post hwnd top resize = loop `catch` \(_ :: IOException) -> post (void $ destroyWindow hwnd)
  where
    loop = do
      line <- B.getLine
      case decodeStrict line of
        Just message -> post $ do
          let width = round (number $ field "width" message) :: Int
              height = round $ number $ field "height" message
              offset = round $ number $ field "top" message
          writeIORef top offset
          (clientWidth, clientHeight) <- rectangle hwnd
          allocaArray 4 $ \rect -> do
            void $ windowRect hwnd rect
            [left, upper, right, lower] <- peekArray 4 rect
            void $ setWindowPos hwnd nullPtr 0 0 (fromIntegral width + right - left - fromIntegral clientWidth) (fromIntegral (height + offset) + lower - upper - fromIntegral clientHeight) 6
          resize hwnd
        Nothing -> pure ()
      loop

jsonString :: ToJSON a => a -> String
jsonString = T.unpack . TE.decodeUtf8 . L.toStrict . encode
