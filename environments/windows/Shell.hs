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
  -- COM may release callback references while its apartment is shutting down.
  bracket newArena freeArena $ \memory ->
    bracket_ (check =<< coInitialize nullPtr 2) coUninitialize (desktopSession memory probe)

desktopSession :: Arena -> Bool -> IO ()
desktopSession memory probe = do
  void $ setDpiAwareness (-4)
  args <- getArgs
  host <- prepareHost args
  configRoot <- configDirectory
  -- Reuse the former data-root profile in place if no newer config profile
  -- exists. Never move, overwrite or create browser files inside a fresh root.
  dataEnv <- lookupEnv "SAPIENS_DATA_DIR"
  legacyData <- legacyDataDirectory
  let explicit = listToMaybe [value | (key,value) <- zip args (drop 1 args), key=="--data-dir"] `mplus` dataEnv
  saved <- try (startupDataDirectory explicit legacyData) :: IO (Either IOException (Maybe FilePath))
  let legacyRoot = either (const $ dataDirectory host) (maybe (dataDirectory host) id) saved
      configuredProfile = configRoot </> "WebView2/Shell"
      legacyProfile = legacyRoot </> "WebView2/Shell"
  configured <- doesDirectoryExist configuredProfile
  legacy <- doesDirectoryExist legacyProfile
  let shellProfile = if not configured && legacy then legacyProfile else configuredProfile
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
  shellRef <- newIORef Nothing
  browserRef <- newIORef Nothing
  statusRef <- newIORef nullPtr
  focusRef <- newIORef nullPtr
  closing <- newIORef False
  retrySetup <- newIORef (pure ())
  workerRef <- newIORef Nothing
  starting <- newIORef False
  compactWindow <- newIORef True
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
        (readIORef browserRef >>= mapM_ closeBrowser) `finally`
          ((readIORef shellRef >>= mapM_ closeView) `finally` closeHost host)
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
          compact <- readIORef compactWindow
          dpi <- windowDpi hwnd
          let scaled value = round (fromIntegral value * fromIntegral (max 96 dpi) / (96 :: Double)) :: Int32
          pokeByteOff (intPtrToPtr lparam) 24 (scaled (if compact then 560 else 860 :: Int))
          pokeByteOff (intPtrToPtr lparam) 28 (scaled (if compact then 340 else 600 :: Int))
          pure 0
        0x05 -> resize hwnd >> pure 0
        0x47 -> do
          readIORef shellRef >>= mapM_ (\view -> call0 (controller view) 23)
          resize hwnd
          defaultProc hwnd message wparam lparam
        0x02 -> (stop `finally` quitMessage 0) >> pure 0
        0x0111 -> case wparam .&. 0xffff of
          101 -> void (createProcess (proc "rundll32.exe" ["url.dll,FileProtocolHandler", "https://go.microsoft.com/fwlink/p/?LinkId=2124703"])) >> pure 0
          102 -> readIORef retrySetup >>= id >> pure 0
          103 -> void (destroyWindow hwnd) >> pure 0
          _ -> defaultProc hwnd message wparam lparam
        0x802A -> runPending >> pure 0
        _ -> defaultProc hwnd message wparam lparam
  proc <- wrapWindowProc $ \hwnd message wparam lparam -> procedure hwnd message wparam lparam `catch` \(e :: SomeException) -> reportException e >> defaultProc hwnd message wparam lparam
  retain memory (freeHaskellFunPtr proc)
  instanceHandle <- moduleHandle nullPtr
  hwnd <- createMainWindow instanceHandle proc
  resizeContent hwnd 560 600
  label <- withWide "STATIC" $ \className -> withWide "Starting Sapiens4…" $ \title ->
    -- SS_CENTER wraps startup diagnostics; SS_CENTERIMAGE forces one line.
    createWindow 0 className title 0x50000001 0 0 1440 960 hwnd nullPtr instanceHandle nullPtr
  setupButtons <- forM [(101,"Install WebView2"),(102,"Retry"),(103,"Cancel")] $ \(ident,title) ->
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
        unless dead $ atomicModifyIORef' pending (\xs -> (action:xs, ())) >> void (postMessage hwnd 0x802A 0 0)
      failStart message = do
        cleanup <- try $
          (atomicModifyIORef' browserRef (\value -> (Nothing, value)) >>= mapM_ closeBrowser) `finally`
            ((atomicModifyIORef' shellRef (\value -> (Nothing, value)) >>= mapM_ closeView) `finally` closeHost host)
        let detail = case cleanup of
              Left (e :: SomeException) -> message ++ "\n\nStartup cleanup failed: " ++ storageMessage e
              Right () -> message
        writeIORef starting False
        windowTitle label $ "Sapiens4 could not start.\n\n" ++ detail ++ "\n\nChoose Retry or Cancel.\nData: " ++ dataDirectory host
        void $ showWindow label 5
        mapM_ (\button -> void (showWindow button 5)) (drop 1 setupButtons)
      storageScript source = readIORef shellRef >>= mapM_ (\view -> execute memory view source (const $ pure ()))
      storageError :: String -> IO ()
      storageError message = do
        closeHost host
        writeIORef compactWindow True
        resizeContent hwnd 560 600
        writeIORef starting False
        storageScript ("window.storageError(" ++ T.unpack (TE.decodeUtf8 $ L.toStrict $ encode message) ++ ")")
      recoverStartup message = do
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
            writeIORef compactWindow True
            resizeContent hwnd 560 600
            readIORef shellRef >>= mapM_ (\view -> navigate view bootstrapURL)
        writeIORef workerRef (Just worker)
      preferencesPath directory = configRoot </> "client-preferences" </> concatMap (\byte -> let h=showHex byte "" in replicate (2-length h) '0' ++ h) (BS.unpack $ SHA.hash $ TE.encodeUtf8 $ T.pack directory) <.> "json"
      savePreferences input = do
        selection <- readIORef selectedRoot
        -- Only a committed browser may persist UI state.
        browser <- readIORef browserRef
        when (isNothing selection && not (isNothing browser)) $ do
          let key=string (field "key" input); value=string (field "value" input)
          when ((key=="sapi-color-theme" || key=="s4.active" || take 13 key=="s4.workspace.") && length key<=128 && length value<=1000000) $ do
            directory <- readIORef activeRoot
            values <- readIORef preferenceValues
            let next=KM.insert (K.fromString key) (String $ T.pack value) values
            writeJSON (preferencesPath directory) next
            writeIORef preferenceValues next
      installStorage selected url = do
        view <- readIORef shellRef >>= maybe (ioError $ userError "The application window closed.") pure
        browser <- newBrowser memory hwnd view selected top
        writeIORef browserRef (Just browser)
        writeIORef selectedRoot (Just (dataDirectory selected,url))
        writeIORef activeRoot (dataDirectory selected)
        values <- (KM.map String <$> (readJSON (preferencesPath $ dataDirectory selected) KM.empty :: IO (KM.KeyMap T.Text)))
          `catch` (\(e :: IOException) -> reportException (toException e) >> pure KM.empty)
        writeIORef preferenceValues values
        oldScript <- atomicModifyIORef' preferenceScript (\old -> (Nothing,old))
        mapM_ (\identity -> withWide identity $ \name -> callPtr (core view) 28 name) oldScript
        let source = concat
              ["(() => {if(window.top!==window || location.origin!==new URL(",jsonString url,").origin)return;try{"
              ,"const root=",jsonString (dataDirectory selected),",saved=",jsonString values,";const allowed=k=>k==='sapi-color-theme'||k==='s4.active'||k.startsWith('s4.workspace.');"
              ,"const previous=localStorage.getItem('s4.native.storage');if(previous&&previous!==root)for(const k of Object.keys(localStorage))if(allowed(k)||k.startsWith('s4.snapshot.'))localStorage.removeItem(k);localStorage.setItem('s4.native.storage',root);"
              ,"for(const [k,v] of Object.entries(saved))if(allowed(k))localStorage.setItem(k,v);"
              ,"const set=Storage.prototype.setItem;const notify=(key,value)=>window.chrome.webview.postMessage({clientPreferences:{key,value}});"
              ,"Storage.prototype.setItem=function(k,v){set.call(this,k,v);if(this===localStorage&&allowed(String(k)))notify(String(k),String(v));};"
              ,"addEventListener('pagehide',()=>{for(const k of Object.keys(localStorage))if(allowed(k))notify(k,localStorage.getItem(k));});"
              ,"}catch(_) {}})();"]
        addDocumentScript memory view source $ \result -> do
          dead <- readIORef closing
          selection <- readIORef selectedRoot
          unless (dead || selection/=Just (dataDirectory selected,url)) $ case result of
            Left message -> recoverStartup message
            Right identity -> do
              writeIORef preferenceScript (Just identity)
              navigate view url
        -- Covers document-script registration and WebView navigation too.
        async (threadDelay 35000000 >> post (do
          selection <- readIORef selectedRoot
          when (selection==Just (dataDirectory selected,url)) $ recoverStartup "Your workspace did not load within 35 seconds. Your saved storage selection is unchanged.")) >>= writeIORef workerRef . Just
      startSelected requestedChoice directory = mask $ \unmask -> do
        busy <- atomicModifyIORef' starting (\value -> (True,value))
        unless busy $ do
          storageScript "window.storageLoading?.()"
          worker <- async $ handle (\(e :: SomeException) -> case fromException e :: Maybe SomeAsyncException of
            Just _ -> throwIO e
            Nothing -> post (recoverStartup $ storageMessage e)) $ unmask $ do
              closeHost host
              selectedDirectory <- canonicalizePath directory
              choice <- maybe (storageChoiceForDirectory selectedDirectory) pure requestedChoice
              let selected = host {dataDirectory=selectedDirectory}
              checkStorage selected choice
              void (B.readFile $ runtimeDirectory host </> ".build/web/guest.js")
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
              selected <- pickDirectory hwnd `finally` writeIORef starting False
              storageScript ("window.storagePicked?.(" ++ T.unpack (TE.decodeUtf8 $ L.toStrict $ encode selected) ++ ")")
          String "existing" -> do
            let directory=string (field "directory" message)
            if isAbsolute directory then startSelected Nothing directory
              else storageError "Choose a folder before continuing."
          _ -> pure ()
      loaded result = do
        view <- either (ioError . userError) pure result
        dead <- readIORef closing
        if dead then closeView view else do
          writeIORef shellRef (Just view)
          resize hwnd
          settings <- getValue (core view) 3
          mapM_ (\index -> callInt settings index 0) [10, 12, 14, 16]
          release settings
          let on index iid = onEvent memory (core view) index iid
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
                recovered <- atomicModifyIORef' recoveryError (\value -> (Nothing,value))
                writeIORef starting False
                case recovered of
                  Just message -> storageScript ("window.storageError(" ++ jsonString message ++ ")")
                  Nothing -> do
                    first <- atomicModifyIORef' initialStorage (\value -> (False,value))
                    when first $ do
                      case saved of
                        Left e -> storageError (storageMessage e)
                        Right Nothing -> pure ()
                        Right (Just directory) -> startSelected (Just AttachExisting) directory
              else do
                selection <- readIORef selectedRoot
                forM_ selection $ \(directory,url) -> when (current==url) $ do
                  committed <- try (saveDataDirectory directory)
                  case committed of
                    Left (e :: SomeException) -> recoverStartup ("Could not save your storage selection: " ++ storageMessage e)
                    Right () -> do
                      atomicModifyIORef' workerRef (\old -> (Nothing,old)) >>= mapM_ cancel
                      writeIORef selectedRoot Nothing
                      writeIORef starting False
                      writeIORef compactWindow False
                      resizeContent hwnd 1440 960
                      -- Capture values restored by the existing profile as well.
                      storageScript "for(const k of Object.keys(localStorage))if(k==='sapi-color-theme'||k==='s4.active'||k.startsWith('s4.workspace.'))window.chrome.webview.postMessage({clientPreferences:{key:k,value:localStorage.getItem(k)}});"
                      when probe $ putStrLn "TITLE: Sapiens4" >> putStrLn ("Sapiens4: " ++ url)
            else do
              base <- readIORef (origin host)
              case base of
                Nothing -> failStart "We couldn't open the storage screen. Choose Retry. Your saved data is unchanged."
                Just _ -> recoverStartup "We couldn't open your workspace. Your saved storage selection is unchanged. Choose a folder to try again."
            when probe $ do
              identity <- getValue event 5 :: IO Word64
              putStrLn $ "NAVIGATION END " ++ show identity ++ " " ++ (if success /= 0 then "True" else "False") ++ " " ++ show code
          writeIORef starting False
          navigate view bootstrapURL
          callInt (controller view) 12 0
          when probe $ void $ forkIO $ probeInput post hwnd top resize
      launch = do
        complete <- and <$> mapM doesFileExist [bootstrapPath, runtimeDirectory host </> ".build/bin/sapiens4.exe"]
        if not complete then post $ failStart "Some Sapiens4 application files are missing. Extract the complete downloaded application, then choose Retry. Your saved data is unchanged."
        else do
          available <- try webViewRuntimeAvailable
          case available of
            Left (e :: SomeException) -> post (failStart (storageMessage e))
            Right False -> post $ do
              windowTitle label "Microsoft Edge WebView2 is required.\n\nChoose Install WebView2 to open Microsoft's official installer.\nRun the downloaded installer, then choose Retry.\nCancel closes Sapiens4 without starting its server."
              mapM_ (\button -> void (showWindow button 5)) setupButtons
              writeIORef starting False
            Right True -> post $
              newView memory hwnd shellProfile (\created -> loaded created `catch` \(e :: SomeException) -> failStart (storageMessage e)) `catch` \(e :: SomeException) -> failStart (storageMessage e)
      begin = do
        dead <- readIORef closing
        unless dead $ do
          busy <- atomicModifyIORef' starting (\value -> (True,value))
          unless busy $ do
            mapM_ (\button -> void (showWindow button 0)) setupButtons
            windowTitle label "Starting Sapiens4…"
            async launch >>= writeIORef workerRef . Just
  hSetBuffering stdout LineBuffering
  writeIORef retrySetup begin
  begin
  void $ showWindow hwnd 5
  void $ updateWindow hwnd
  void $ foregroundWindow hwnd
  pumpMessages memory `finally` ((readIORef workerRef >>= mapM_ cancel) `finally` closeHost host)

createMainWindow :: Object -> FunPtr WindowProc -> IO Window
createMainWindow instanceHandle procedure = do
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
    screenWidth <- systemMetric 0
    screenHeight <- systemMetric 1
    createWindow 0 className title 0x00CF0000 ((screenWidth - 560) `div` 2) ((screenHeight - 340) `div` 2) 560 340 nullPtr nullPtr instanceHandle nullPtr
  when (hwnd == nullPtr) $ ioError $ userError "Cannot create desktop window"
  pure hwnd

-- Keep the same HWND centered as setup gives way to the workspace.
resizeContent :: Window -> Int -> Int -> IO ()
resizeContent hwnd width height = do
  dpi <- windowDpi hwnd
  (clientWidth,clientHeight) <- rectangle hwnd
  screenWidth <- systemMetric 0
  screenHeight <- systemMetric 1
  allocaArray 4 $ \rect -> do
    void $ windowRect hwnd rect
    [left,upper,right,lower] <- peekArray 4 rect
    let scaled value = round (fromIntegral value * fromIntegral (max 96 dpi) / (96 :: Double)) :: Int32
        outerWidth = min screenWidth (scaled width + right-left-fromIntegral clientWidth)
        outerHeight = min screenHeight (scaled height + lower-upper-fromIntegral clientHeight)
    void $ setWindowPos hwnd nullPtr (max 0 $ min (screenWidth-outerWidth) $ (left+right-outerWidth) `div` 2) (max 0 $ min (screenHeight-outerHeight) $ (upper+lower-outerHeight) `div` 2) outerWidth outerHeight 4

pumpMessages :: Arena -> IO ()
pumpMessages memory = allocaBytes 48 $ \message -> do
  let step = do
        result <- getMessage message nullPtr 0 0
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
