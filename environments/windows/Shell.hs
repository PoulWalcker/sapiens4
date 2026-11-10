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
import Data.Maybe (isNothing)
import Data.Word
import DataRoot
import Network.URI (URI(..), URIAuth(..), parseURI, escapeURIString, isUnreserved, unEscapeString)
import System.Directory (canonicalizePath)
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
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as L
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Aeson.KeyMap as KeyMap

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
  bootstrapPath <- canonicalizePath (runtimeDirectory host </> "app/shell/bootstrap.html")
  let filePath = '/' : map (\c -> if c=='\\' then '/' else c) bootstrapPath
      bootstrapURL = "file://" ++ escapeURIString (\c -> isUnreserved c || c `elem` ("/:" :: String)) filePath
      bootstrapSource address = case parseURI address of
        Just uri -> uriScheme uri=="file:" && maybe True (\auth -> null (uriRegName auth) && null (uriUserInfo auth) && null (uriPort auth)) (uriAuthority uri)
          && equalFilePath (unEscapeString $ uriPath uri) filePath && null (uriQuery uri) && null (uriFragment uri)
        Nothing -> False
  initialStorage <- newIORef True
  recoveryError <- newIORef Nothing
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
  onboarding <- newIORef True
  onboardingStep <- newIORef ""
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
          compact <- readIORef onboarding
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
    createWindow 0 className title 0x50000201 0 0 1440 960 hwnd nullPtr instanceHandle nullPtr
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
      storageError message = do
        closeHost host
        writeIORef onboarding True
        resizeContent hwnd 560 600
        writeIORef starting False
        storageScript ("window.storageError(" ++ T.unpack (TE.decodeUtf8 $ L.toStrict $ encode message) ++ ")")
      storageReply request fields = readIORef shellRef >>= mapM_ (\view -> do
        let payload = case fields of JSON.Object values -> JSON.Object (KeyMap.insert "request" (field "request" request) values); _ -> fields
        execute memory view ("window.sapiensBrowserEvent?.(" ++ T.unpack (TE.decodeUtf8 $ L.toStrict $ encode $ object ["action" .= ("storage" :: String),"fields" .= payload]) ++ ")") (const $ pure ()))
      storageFailure request e = case request of
        Nothing -> storageError (storageMessage e)
        Just input -> do
          available <- hostRunning host
          storageReply input (object ["kind" .= ("error" :: String),"error" .= storageMessage e,"available" .= available])
      startSelected choice directory request = do
        busy <- atomicModifyIORef' starting (\value -> (True,value))
        unless busy $ do
          storageScript "window.storageLoading?.()"
          previous <- readIORef (serverProcess host)
          previousOrigin <- readIORef (origin host)
          task <- async $ do
            result <- try $ do
              selectedDirectory <- canonicalizePath directory
              current <- activeDataDirectory host
              available <- hostRunning host
              if choice==AttachExisting && available && current==Just selectedDirectory then pure Nothing else do
                let selected = host {dataDirectory=selectedDirectory}
                checkStorage selected choice
                closeHost host
                -- Keep the previous directory available for explicit recovery
                -- if the replacement process cannot start.
                started <- try (startHost choice selected)
                url <- case started of
                  Left (e :: SomeException) -> writeIORef (serverProcess host) previous >> writeIORef (origin host) previousOrigin >> throwIO e
                  Right address -> pure address
                saveDataDirectory selectedDirectory `onException` (closeHost host >> writeIORef (serverProcess host) previous >> writeIORef (origin host) previousOrigin)
                pure (Just (selected,url))
            post $ do
              writeIORef starting False
              case result of
                Left (e :: SomeException) -> storageFailure request e
                Right Nothing -> mapM_ (\input -> storageReply input (object ["kind" .= ("unchanged" :: String)])) request
                Right (Just (selected,url)) -> flip catch (\(e :: SomeException) -> storageFailure request e) $ do
                  view <- readIORef shellRef >>= maybe (ioError $ userError "The application window closed.") pure
                  atomicModifyIORef' browserRef (\value -> (Nothing,value)) >>= mapM_ closeBrowser
                  browser <- newBrowser memory hwnd view selected top
                  writeIORef browserRef (Just browser)
                  writeIORef onboardingStep ""
                  resizeContent hwnd 560 600
                  navigate view url
                  when probe $ do
                    putStrLn "TITLE: Sapiens4"
                    putStrLn $ "Sapiens4: " ++ url
          writeIORef workerRef (Just task)
      storageMessageReceived request = do
        step <- readIORef onboardingStep
        when (step=="provider") $ case string (field "operation" request) of
          "info" -> do
            directory <- activeDataDirectory host
            available <- hostRunning host
            storageReply request (object ["kind" .= ("info" :: String),"current" .= maybe "" id directory,"default" .= dataDirectory host,"available" .= available])
          "pick" -> do
            busy <- atomicModifyIORef' starting (\value -> (True,value))
            unless busy $ flip finally (writeIORef starting False) $ do
              selected <- pickDirectory hwnd
              storageReply request $ case selected of
                Nothing -> object ["kind" .= ("cancelled" :: String)]
                Just directory -> object ["kind" .= ("picked" :: String),"directory" .= directory]
          "apply" -> do
            let choice=if string (field "mode" request)=="new" then StartNew else AttachExisting
                directory=if choice==StartNew then dataDirectory host else string (field "directory" request)
            when (isAbsolute directory) $ startSelected choice directory (Just request)
          _ -> pure ()
      chooseStorage message = do
        busy <- readIORef starting
        unless busy $ case field "storage" message of
          _ | field "storageStage" message == String "storage" -> resizeContent hwnd 560 600
          String "new" -> startSelected StartNew (dataDirectory host) Nothing
          String "existing" -> do
            picking <- atomicModifyIORef' starting (\value -> (True,value))
            unless picking $ do
              selected <- pickDirectory hwnd `finally` writeIORef starting False
              mapM_ (\directory -> startSelected AttachExisting directory Nothing) selected
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
                    case field "onboarding" message of
                      Bool compact -> do
                        previous <- atomicModifyIORef' onboarding (\old -> (compact,old))
                        let step = string (field "onboardingStep" message)
                        previousStep <- atomicModifyIORef' onboardingStep (\old -> (step,old))
                        when (compact /= previous || compact && step /= previousStep) $
                          resizeContent hwnd (if compact then 560 else 1440) (if compact then 600 else 960)
                      _ -> pure ()
                    case field "storage" message of
                      request@(JSON.Object _) -> storageMessageReceived request
                      _ -> readIORef browserRef >>= mapM_ (\browser -> syncBrowser browser message)
                | otherwise -> pure ()
          on 15 "{D33A35BF-1C49-4F98-93AB-006E0533FE1C}" $ \event -> do
            success <- getValue event 3 :: IO Int32
            code <- getValue event 4 :: IO Int32
            if success /= 0 then do
              void $ showWindow label 0
              current <- getString (core view) 4
              when (bootstrapSource current) $ do
                recovered <- atomicModifyIORef' recoveryError (\value -> (Nothing,value))
                case recovered of
                  Just message -> storageError message
                  Nothing -> do
                    first <- atomicModifyIORef' initialStorage (\value -> (False,value))
                    when first $ do
                      saved <- try savedDataDirectory
                      case saved of
                        Left (e :: SomeException) -> storageError (storageMessage e)
                        Right Nothing -> pure ()
                        Right (Just directory) -> startSelected AttachExisting directory Nothing
            else do
              base <- readIORef (origin host)
              let message = "Cannot load Sapiens4: " ++ show code
              case base of
                Nothing -> failStart message
                Just _ -> do
                  (atomicModifyIORef' browserRef (\value -> (Nothing,value)) >>= mapM_ closeBrowser) `finally` closeHost host
                  writeIORef initialStorage False
                  writeIORef recoveryError (Just message)
                  writeIORef starting True
                  writeIORef onboarding True
                  resizeContent hwnd 560 600
                  navigate view bootstrapURL
            when probe $ do
              identity <- getValue event 5 :: IO Word64
              putStrLn $ "NAVIGATION END " ++ show identity ++ " " ++ (if success /= 0 then "True" else "False") ++ " " ++ show code
          writeIORef starting False
          navigate view bootstrapURL
          callInt (controller view) 12 0
          when probe $ void $ forkIO $ probeInput post hwnd top resize
      launch = do
        available <- try webViewRuntimeAvailable
        case available of
          Left (e :: SomeException) -> post (failStart (storageMessage e))
          Right False -> post $ do
            windowTitle label "Microsoft Edge WebView2 is required.\n\nChoose Install WebView2 to open Microsoft's official installer.\nRun the downloaded installer, then choose Retry.\nCancel closes Sapiens4 without starting its server."
            mapM_ (\button -> void (showWindow button 5)) setupButtons
            writeIORef starting False
          Right True -> post $
            newView memory hwnd (configRoot </> "WebView2/Shell") (\created -> loaded created `catch` \(e :: SomeException) -> failStart (storageMessage e)) `catch` \(e :: SomeException) -> failStart (storageMessage e)
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
