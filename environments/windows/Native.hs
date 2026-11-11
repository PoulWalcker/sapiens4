{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Native where

import Control.Exception
import Control.Monad
import Data.Bits
import Data.IORef
import Data.Int
import Data.Word
import Foreign hiding (void)
import Foreign.C
import System.IO
import System.Environment (getExecutablePath)
import System.FilePath ((</>), takeDirectory)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString as B

type Object = Ptr ()
type Window = Ptr ()
type Wide = Ptr Word16
type HRESULT = Int32
data ArenaEntry = ArenaEntry { readyToDispose :: IO Bool, disposeEntry :: IO () }
newtype Arena = Arena (IORef [ArenaEntry])
type WindowProc = Window -> Word32 -> WordPtr -> IntPtr -> IO IntPtr

foreign import ccall safe "CoInitializeEx" coInitialize :: Ptr () -> Word32 -> IO HRESULT
foreign import ccall safe "CoUninitialize" coUninitialize :: IO ()
foreign import ccall unsafe "CoTaskMemFree" coFree :: Ptr a -> IO ()
foreign import ccall unsafe "CLSIDFromString" clsid :: Wide -> Ptr Word8 -> IO HRESULT
foreign import ccall unsafe "LoadLibraryW" loadLibrary :: Wide -> IO Object
foreign import ccall unsafe "FreeLibrary" freeLibrary :: Object -> IO Int32
foreign import ccall unsafe "GetProcAddress" getProcAddress :: Object -> CString -> IO (FunPtr a)
foreign import ccall unsafe "GetModuleHandleW" moduleHandle :: Wide -> IO Object
foreign import ccall unsafe "GetCurrentProcessId" processId :: IO Word32
foreign import ccall unsafe "SetProcessDpiAwarenessContext" setDpiAwareness :: IntPtr -> IO Int32
foreign import ccall unsafe "GetDpiForWindow" windowDpi :: Window -> IO Word32
foreign import ccall unsafe "RegisterClassExW" registerClass :: Ptr a -> IO Word16
foreign import ccall unsafe "UnregisterClassW" unregisterClass :: Wide -> Object -> IO Int32
foreign import ccall safe "CreateWindowExW" createWindow :: Word32 -> Wide -> Wide -> Word32 -> Int32 -> Int32 -> Int32 -> Int32 -> Window -> Object -> Object -> Ptr () -> IO Window
foreign import ccall safe "DefWindowProcW" defaultProc :: WindowProc
foreign import ccall "wrapper" wrapWindowProc :: WindowProc -> IO (FunPtr WindowProc)
foreign import ccall safe "ShowWindow" showWindow :: Window -> Int32 -> IO Int32
foreign import ccall safe "UpdateWindow" updateWindow :: Window -> IO Int32
foreign import ccall safe "DestroyWindow" destroyWindow :: Window -> IO Int32
foreign import ccall safe "SetWindowTextW" setWindowText :: Window -> Wide -> IO Int32
foreign import ccall safe "SendMessageW" sendMessage :: Window -> Word32 -> WordPtr -> IntPtr -> IO IntPtr
foreign import ccall safe "PostMessageW" postMessage :: Window -> Word32 -> WordPtr -> IntPtr -> IO Int32
foreign import ccall safe "GetMessageW" getMessage :: Ptr () -> Window -> Word32 -> Word32 -> IO Int32
foreign import ccall safe "TranslateMessage" translateMessage :: Ptr () -> IO Int32
foreign import ccall safe "DispatchMessageW" dispatchMessage :: Ptr () -> IO IntPtr
foreign import ccall unsafe "PostQuitMessage" quitMessage :: Int32 -> IO ()
foreign import ccall unsafe "GetClientRect" clientRect :: Window -> Ptr Int32 -> IO Int32
foreign import ccall unsafe "GetWindowRect" windowRect :: Window -> Ptr Int32 -> IO Int32
foreign import ccall safe "SetWindowPos" setWindowPos :: Window -> Window -> Int32 -> Int32 -> Int32 -> Int32 -> Word32 -> IO Int32
foreign import ccall unsafe "SystemParametersInfoW" systemParametersInfo :: Word32 -> Word32 -> Ptr Int32 -> Word32 -> IO Int32
foreign import ccall safe "SetForegroundWindow" foregroundWindow :: Window -> IO Int32
foreign import ccall unsafe "GetFocus" focusedWindow :: IO Window
foreign import ccall unsafe "IsChild" childWindow :: Window -> Window -> IO Int32
foreign import ccall unsafe "LoadImageW" loadImage :: Object -> Wide -> Word32 -> Int32 -> Int32 -> Word32 -> IO Object
foreign import ccall unsafe "DestroyIcon" destroyIcon :: Object -> IO Int32
foreign import ccall safe "DwmSetWindowAttribute" dwmAttribute :: Window -> Word32 -> Ptr Int32 -> Word32 -> IO HRESULT
foreign import ccall safe "GetOpenFileNameW" getOpenFileName :: Ptr () -> IO Int32
foreign import ccall safe "MessageBoxW" messageBox :: Window -> Wide -> Wide -> Word32 -> IO Int32
foreign import ccall safe "OutputDebugStringW" debugString :: Wide -> IO ()
foreign import ccall safe "hs_webview_bounds" rawBounds :: Object -> Int32 -> Int32 -> Int32 -> Int32 -> IO HRESULT
foreign import ccall "dynamic" invoke0 :: FunPtr (Object -> IO HRESULT) -> Object -> IO HRESULT
foreign import ccall "dynamic" invokeRef :: FunPtr (Object -> IO Word32) -> Object -> IO Word32
foreign import ccall "dynamic" invokePtr :: FunPtr (Object -> Ptr a -> IO HRESULT) -> Object -> Ptr a -> IO HRESULT
foreign import ccall "dynamic" invokeInt :: FunPtr (Object -> Int32 -> IO HRESULT) -> Object -> Int32 -> IO HRESULT
foreign import ccall "dynamic" invokeDouble :: FunPtr (Object -> Double -> IO HRESULT) -> Object -> Double -> IO HRESULT
foreign import ccall "dynamic" invoke2 :: FunPtr (Object -> Ptr a -> Ptr b -> IO HRESULT) -> Object -> Ptr a -> Ptr b -> IO HRESULT
foreign import ccall "dynamic" invoke3 :: FunPtr (Object -> Ptr a -> Ptr b -> Ptr c -> IO HRESULT) -> Object -> Ptr a -> Ptr b -> Ptr c -> IO HRESULT
foreign import ccall "dynamic" availableBrowserVersion :: FunPtr (Wide -> Ptr Wide -> IO HRESULT) -> Wide -> Ptr Wide -> IO HRESULT
foreign import ccall "dynamic" createEnvironment :: FunPtr (Wide -> Wide -> Object -> Object -> IO HRESULT) -> Wide -> Wide -> Object -> Object -> IO HRESULT
foreign import ccall "wrapper" wrapQuery :: (Object -> Ptr Word8 -> Ptr Object -> IO HRESULT) -> IO (FunPtr (Object -> Ptr Word8 -> Ptr Object -> IO HRESULT))
foreign import ccall "wrapper" wrapRef :: (Object -> IO Word32) -> IO (FunPtr (Object -> IO Word32))
foreign import ccall "wrapper" wrapEvent :: (Object -> Object -> Object -> IO HRESULT) -> IO (FunPtr (Object -> Object -> Object -> IO HRESULT))
foreign import ccall "wrapper" wrapCompletion :: (Object -> HRESULT -> Object -> IO HRESULT) -> IO (FunPtr (Object -> HRESULT -> Object -> IO HRESULT))

withWide :: String -> (Wide -> IO a) -> IO a
withWide value action = B.useAsCString (TE.encodeUtf16LE (T.pack value) <> B.pack [0, 0]) (action . castPtr)

readWide :: Wide -> IO String
readWide pointer | pointer == nullPtr = pure ""
readWide pointer = do
  words16 <- peekArray0 0 pointer
  pure $ T.unpack $ TE.decodeUtf16LE $ B.pack $ concatMap (\w -> [fromIntegral w, fromIntegral (w `shiftR` 8)]) words16

check :: HRESULT -> IO ()
check result = when (result < 0) $ ioError $ userError $ "Windows API failed: " ++ show result

method :: Object -> Int -> IO (FunPtr a)
method object index = peek (castPtr object :: Ptr (Ptr (FunPtr a))) >>= \table -> peekElemOff table index

call0 :: Object -> Int -> IO ()
call0 object index = method object index >>= \f -> invoke0 f object >>= check

callInt :: Object -> Int -> Int32 -> IO ()
callInt object index value = method object index >>= \f -> invokeInt f object value >>= check

callPtr :: Object -> Int -> Ptr a -> IO ()
callPtr object index value = method object index >>= \f -> invokePtr f object value >>= check

call2 :: Object -> Int -> Ptr a -> Ptr b -> IO ()
call2 object index a b = method object index >>= \f -> invoke2 f object a b >>= check

getValue :: Storable a => Object -> Int -> IO a
getValue object index = alloca $ \out -> callPtr object index out >> peek out

getString :: Object -> Int -> IO String
getString object index = bracket (getValue object index) coFree readWide

putString :: Object -> Int -> String -> IO ()
putString object index value = withWide value (callPtr object index)

addRef, release :: Object -> IO ()
addRef object = method object 1 >>= \f -> void $ invokeRef f object
release object = when (object /= nullPtr) $ method object 2 >>= \f -> void $ invokeRef f object

withGuid :: String -> (Ptr Word8 -> IO a) -> IO a
withGuid text action = allocaBytes 16 $ \guid -> withWide text (\w -> clsid w guid >>= check) >> action guid

query :: Object -> String -> IO Object
query object guid = withGuid guid $ \idPtr -> alloca $ \out -> call2 object 0 idPtr out >> peek out

newArena :: IO Arena
newArena = Arena <$> newIORef []

retain :: Arena -> IO () -> IO ()
retain (Arena entries) dispose = modifyIORef' entries (ArenaEntry (pure False) dispose :)

freeArena :: Arena -> IO ()
freeArena (Arena entries) = do
  traceLifecycle "freeArena begin"
  atomicModifyIORef' entries (\owned -> ([], owned)) >>= disposeAll
  traceLifecycle "freeArena end"

disposeAll :: [ArenaEntry] -> IO ()
disposeAll [] = pure ()
disposeAll (entry:rest) = disposeEntry entry `finally` disposeAll rest

sweepArena :: Arena -> IO ()
sweepArena (Arena arena) = do
  entries <- atomicModifyIORef' arena (\entries -> ([], entries))
  alive <- filterM keep entries
  modifyIORef' arena (alive ++)
  where
    keep entry = readyToDispose entry >>= \ready -> if ready then disposeEntry entry >> pure False else pure True

callback :: Arena -> String -> FunPtr a -> IO Object
callback (Arena arena) guid invoke = do
  count <- newIORef (1 :: Word32)
  object <- mallocBytes (sizeOf (nullPtr :: Ptr ()))
  table <- mallocArray 4
  iid <- withGuid guid (peekArray 16)
  unknown <- withGuid "{00000000-0000-0000-C000-000000000046}" (peekArray 16)
  add <- wrapRef $ \_ -> atomicModifyIORef' count (\n -> (n + 1, n + 1))
  dropRef <- wrapRef $ \_ -> atomicModifyIORef' count (\n -> let next = if n == 0 then 0 else n - 1 in (next, next))
  identify <- wrapQuery $ \self requested out -> do
    bytes <- peekArray 16 requested
    if bytes `elem` [iid, unknown]
      then poke out self >> atomicModifyIORef' count (\n -> (n + 1, ())) >> pure 0
      else poke out nullPtr >> pure (-2147467262)
  pokeArray table [castFunPtr identify, castFunPtr add, castFunPtr dropRef, castFunPtr invoke]
  poke (castPtr object) table
  let dispose = do
        remaining <- readIORef count
        traceLifecycle ("dispose callback " ++ guid ++ " refs=" ++ show remaining)
        mapM_ freeHaskellFunPtr [castFunPtr identify, castFunPtr add, castFunPtr dropRef, castFunPtr invoke]
        free table
        free object
  modifyIORef' arena (ArenaEntry ((== 0) <$> readIORef count) dispose :)
  pure object

traceLifecycle :: String -> IO ()
traceLifecycle message = hPutStrLn stderr ("[DEBUG-d1-wv2] " ++ message) `catch` (\(_ :: IOException) -> pure ())

guardCallback :: IO () -> IO HRESULT
guardCallback action = (action >> pure 0) `catch` \(e :: SomeException) -> reportException e >> pure (-2147467259)

reportException :: SomeException -> IO ()
reportException e = hPutStrLn stderr (displayException e) `catch` \(_ :: IOException) ->
  -- A GUI launch may have no stderr; never let logging escape a native callback.
  withWide (displayException e) debugString

onEvent :: Arena -> Object -> Int -> String -> (Object -> IO ()) -> IO ()
onEvent arena object index iid action = do
  handler <- wrapEvent (\_ _ args -> guardCallback $ action args) >>= callback arena iid
  (allocaBytes 8 $ call2 object index handler) `finally` release handler

completion :: Arena -> String -> (HRESULT -> Object -> IO ()) -> IO Object
completion arena iid action = wrapCompletion (\_ result object -> guardCallback $ action result object) >>= callback arena iid

data View = View { controller :: Object, core :: Object, container :: Window }

webViewLoader :: IO Object
webViewLoader = do
  executable <- getExecutablePath
  loader <- withWide (takeDirectory executable </> "WebView2Loader.dll") loadLibrary
  when (loader == nullPtr) $ ioError $ userError "WebView2Loader.dll is missing from the application package"
  pure loader

webViewRuntimeAvailable :: IO Bool
webViewRuntimeAvailable = bracket webViewLoader (void . freeLibrary) $ \loader -> do
  entry <- withCString "GetAvailableCoreWebView2BrowserVersionString" (getProcAddress loader)
  when (entry == nullFunPtr) $ ioError $ userError "WebView2 runtime detection is unavailable"
  alloca $ \version -> do
    poke version nullPtr
    result <- availableBrowserVersion entry nullPtr version
    value <- peek version
    versionText <- if value==nullPtr then pure "" else readWide value `finally` coFree value
    pure (result >= 0 && not (null versionText) && all (\c -> c `elem` ['0'..'9'] || c=='.') versionText)

newView :: Arena -> Window -> String -> (Either String View -> IO ()) -> IO ()
newView = newViewWhile (pure True)

newViewWhile :: IO Bool -> Arena -> Window -> String -> (Either String View -> IO ()) -> IO ()
newViewWhile active arena parent profile done = do
  traceLifecycle ("create environment " ++ profile)
  loader <- webViewLoader
  retain arena (traceLifecycle "unload WebView2Loader" >> void (freeLibrary loader))
  entry <- withCString "CreateCoreWebView2EnvironmentWithOptions" (getProcAddress loader)
  when (entry == nullFunPtr) $ ioError $ userError "WebView2 loader entry point is unavailable"
  environment <- completion arena "{4E8A3389-C9D8-4BD2-B6B5-124FEE6CC14D}" $ \result env -> do
    valid <- active
    traceLifecycle ("environment completion hr=" ++ show result ++ " object=" ++ show env ++ " active=" ++ show valid)
    when valid $ if result < 0 || env == nullPtr then done (Left $ "Cannot initialize WebView2 Runtime: " ++ show result) else do
      -- Give each view its own child window so guest panes can sit above the shell.
      child <- withWide "STATIC" $ \className -> withWide "" $ \title ->
        createWindow 0 className title 0x56000000 0 0 0 0 parent nullPtr nullPtr nullPtr
      when (child == nullPtr) $ ioError $ userError "Cannot create browser container"
      pendingChild <- newIORef child
      let discardChild = atomicModifyIORef' pendingChild (\value -> (nullPtr,value)) >>= \value ->
            when (value /= nullPtr) $ void (destroyWindow value)
          Arena entries = arena
      modifyIORef' entries (ArenaEntry ((||) <$> ((== nullPtr) <$> readIORef pendingChild) <*> (not <$> active)) discardChild :)
      ready <- completion arena "{6C4819F3-C9B7-4260-8127-C9F5BDE7F68C}" $ \code ctrl -> do
        valid <- active
        traceLifecycle ("controller completion hr=" ++ show code ++ " object=" ++ show ctrl ++ " active=" ++ show valid)
        if not valid then (when (ctrl /= nullPtr) $ call0 ctrl 24) `finally` discardChild else if code < 0 || ctrl == nullPtr then discardChild >> done (Left $ "Cannot create browser view: " ++ show code) else do
          addRef ctrl
          web <- getValue ctrl 25 `onException` (release ctrl >> discardChild)
          writeIORef pendingChild nullPtr
          done (Right $ View ctrl web child)
      (call2 env 3 child ready `onException` discardChild) `finally` release ready
  (withWide profile $ \path -> do
    result <- createEnvironment entry nullPtr path nullPtr environment
    traceLifecycle ("create environment returned hr=" ++ show result)
    check result) `finally` release environment

closeView :: View -> IO ()
closeView view = (traceLifecycle ("closeView " ++ show (controller view)) >> call0 (controller view) 24) `finally`
  (release (core view) `finally` (release (controller view) `finally` void (destroyWindow $ container view)))

bounds :: View -> Int -> Int -> Int -> Int -> IO ()
bounds view x y width height = do
  void $ setWindowPos (container view) nullPtr (fromIntegral x) (fromIntegral y) (fromIntegral width) (fromIntegral height) 0x0014
  rawBounds (controller view) 0 0 (fromIntegral width) (fromIntegral height) >>= check

visible :: View -> Bool -> IO ()
visible view yes = do
  callInt (controller view) 4 (if yes then 1 else 0)
  void $ showWindow (container view) (if yes then 8 else 0)
  when yes $ void $ setWindowPos (container view) nullPtr 0 0 0 0 0x0013

zoom :: View -> Double -> IO ()
zoom view factor = method (controller view) 8 >>= \f -> invokeDouble f (controller view) (max 0.25 $ min 5 factor) >>= check

execute :: Arena -> View -> String -> (String -> IO ()) -> IO ()
execute arena view script done = do
  handler <- completion arena "{49511172-CC67-4BCA-9923-137112F4C4CC}" $ \result output ->
    when (result >= 0) $ readWide (castPtr output) >>= done
  (withWide script $ \source -> call2 (core view) 29 source handler) `finally` release handler

-- Ask the engine for the committed frame's URL and MIME type. Guest script
-- cannot override this result. An unsupported or failed query leaves the tab live.
frameTree :: Arena -> View -> (String -> IO ()) -> IO ()
frameTree arena view done =
  bracket (completion arena "{5C4889F0-5EF6-4C5A-952C-D8F1B92D0574}" receive) release $ \handler ->
    withWide "Page.getFrameTree" $ \name -> withWide "{}" $ \params -> do
      f <- method (core view) 36
      void $ invoke3 f (core view) name params handler
  where
    receive result output = when (result >= 0) $ readWide (castPtr output) >>= done

navigate :: View -> String -> IO ()
navigate view = putString (core view) 5

rectangle :: Window -> IO (Int, Int)
rectangle window = allocaArray 4 $ \rect -> do
  void $ clientRect window rect
  width <- peekElemOff rect 2
  height <- peekElemOff rect 3
  pure (fromIntegral width, fromIntegral height)

workArea :: IO (Int32, Int32, Int32, Int32)
workArea = allocaArray 4 $ \rect -> do
  result <- systemParametersInfo 0x0030 0 rect 0
  when (result == 0) $ ioError $ userError "Cannot get desktop work area"
  [left, upper, right, lower] <- peekArray 4 rect
  pure (left, upper, right - left, lower - upper)

windowTitle :: Window -> String -> IO ()
windowTitle window title = withWide title $ void . setWindowText window

pickFile :: Window -> String -> IO (Maybe String)
pickFile window directory = allocaBytes 152 $ \dialog -> allocaArray 32768 $ \filename ->
  withWide directory $ \folder -> withWide "Open in Sapiens4" $ \title -> do
    fillBytes dialog 0 152
    pokeByteOff dialog 0 (152 :: Word32)
    pokeByteOff dialog 8 window
    poke filename (0 :: Word16)
    pokeByteOff dialog 48 filename
    pokeByteOff dialog 56 (32768 :: Word32)
    pokeByteOff dialog 80 folder
    pokeByteOff dialog 88 title
    pokeByteOff dialog 96 (0x00001000 .|. 0x00000800 .|. 0x00080000 :: Word32)
    selected <- getOpenFileName dialog
    if selected /= 0 then Just <$> readWide filename else pure Nothing
