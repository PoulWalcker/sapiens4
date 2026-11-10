{-# LANGUAGE ForeignFunctionInterface #-}
module ObjC where

import Control.Monad
import Control.Exception (bracket)
import Data.Maybe (fromMaybe)
import Data.Aeson
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Foreign hiding (void, new)
import Foreign.C

type Obj = Ptr ()
type Sel = Ptr ()
foreign import ccall safe "dlopen" loadLibrary :: CString -> CInt -> IO Obj

loadFrameworks :: IO ()
loadFrameworks = forM_ ["Cocoa", "WebKit"] $ \name -> withCString ("/System/Library/Frameworks/" ++ name ++ ".framework/" ++ name) $ \path -> do
    loaded <- loadLibrary path 2
    when (loaded == nullPtr) $ ioError (userError ("Could not load " ++ name))

foreign import ccall unsafe "objc_getClass" getClass :: CString -> IO Obj
foreign import ccall unsafe "sel_registerName" getSelector :: CString -> IO Sel
foreign import ccall unsafe "objc_allocateClassPair" allocateClass :: Obj -> CString -> CSize -> IO Obj
foreign import ccall unsafe "objc_registerClassPair" registerClass :: Obj -> IO ()
foreign import ccall unsafe "class_addMethod" addMethod :: Obj -> Sel -> FunPtr a -> CString -> IO CBool
foreign import ccall safe "objc_msgSend" call0 :: Obj -> Sel -> IO Obj
foreign import ccall safe "objc_msgSend" call1 :: Obj -> Sel -> Obj -> IO Obj
foreign import ccall safe "objc_msgSend" call2 :: Obj -> Sel -> Obj -> Obj -> IO Obj
foreign import ccall safe "objc_msgSend" call3 :: Obj -> Sel -> Obj -> Obj -> Obj -> IO Obj
foreign import ccall safe "objc_msgSend" call4 :: Obj -> Sel -> Obj -> Obj -> Obj -> Obj -> IO Obj
foreign import ccall safe "objc_msgSend" call5 :: Obj -> Sel -> Obj -> Obj -> Obj -> Obj -> Obj -> IO Obj
foreign import ccall safe "objc_msgSend" callBool :: Obj -> Sel -> IO CBool
foreign import ccall safe "objc_msgSend" callBool1 :: Obj -> Sel -> Obj -> IO CBool
foreign import ccall safe "objc_msgSend" callLong :: Obj -> Sel -> IO CLong
foreign import ccall safe "objc_msgSend" callDouble :: Obj -> Sel -> IO CDouble
foreign import ccall safe "objc_msgSend" callSetDouble :: Obj -> Sel -> CDouble -> IO ()
foreign import ccall safe "objc_msgSend" callBitmap :: Obj -> Sel -> Obj -> Obj -> Obj -> Obj -> Obj -> Obj -> Obj -> Obj -> Obj -> Obj -> IO Obj
foreign import ccall safe "objc_msgSend" callTimer :: Obj -> Sel -> CDouble -> Obj -> Sel -> Obj -> CBool -> IO Obj
foreign import ccall safe "sapiens_objc_rect" callRect :: Obj -> Sel -> CDouble -> CDouble -> CDouble -> CDouble -> IO Obj
foreign import ccall safe "sapiens_objc_rect_object" callRectObj :: Obj -> Sel -> CDouble -> CDouble -> CDouble -> CDouble -> Obj -> IO Obj
foreign import ccall safe "sapiens_objc_window" callWindow :: Obj -> Sel -> CDouble -> CDouble -> CULong -> IO Obj
foreign import ccall safe "sapiens_objc_convert_rect" callConvertRect :: Obj -> Sel -> Obj -> CDouble -> CDouble -> CDouble -> CDouble -> Ptr CDouble -> IO ()
foreign import ccall safe "sapiens_objc_size" callSize :: Obj -> Sel -> CDouble -> CDouble -> IO ()
foreign import ccall safe "sapiens_objc_height" callHeight :: Obj -> Sel -> IO CDouble
foreign import ccall safe "sapiens_objc_draw" callDraw :: Obj -> Sel -> CDouble -> IO ()
foreign import ccall "wrapper" wrap0 :: (Obj -> Sel -> IO ()) -> IO (FunPtr (Obj -> Sel -> IO ()))
foreign import ccall "wrapper" wrap1 :: (Obj -> Sel -> Obj -> IO ()) -> IO (FunPtr (Obj -> Sel -> Obj -> IO ()))
foreign import ccall "wrapper" wrap2 :: (Obj -> Sel -> Obj -> Obj -> IO ()) -> IO (FunPtr (Obj -> Sel -> Obj -> Obj -> IO ()))
foreign import ccall "wrapper" wrap3 :: (Obj -> Sel -> Obj -> Obj -> Obj -> IO ()) -> IO (FunPtr (Obj -> Sel -> Obj -> Obj -> Obj -> IO ()))
foreign import ccall "wrapper" wrap4 :: (Obj -> Sel -> Obj -> Obj -> Obj -> Obj -> IO ()) -> IO (FunPtr (Obj -> Sel -> Obj -> Obj -> Obj -> Obj -> IO ()))
foreign import ccall "wrapper" wrapBool1 :: (Obj -> Sel -> Obj -> IO CBool) -> IO (FunPtr (Obj -> Sel -> Obj -> IO CBool))
foreign import ccall "wrapper" wrapBool2 :: (Obj -> Sel -> Obj -> CBool -> IO CBool) -> IO (FunPtr (Obj -> Sel -> Obj -> CBool -> IO CBool))
foreign import ccall "wrapper" wrapObj4 :: (Obj -> Sel -> Obj -> Obj -> Obj -> Obj -> IO Obj) -> IO (FunPtr (Obj -> Sel -> Obj -> Obj -> Obj -> Obj -> IO Obj))
foreign import ccall "dynamic" blockLong :: FunPtr (Obj -> CLong -> IO ()) -> Obj -> CLong -> IO ()
foreign import ccall "dynamic" blockObj :: FunPtr (Obj -> Obj -> IO ()) -> Obj -> Obj -> IO ()

cls name = withCString name getClass
sel name = withCString name getSelector
obj0 o name = sel name >>= call0 o
obj1 o name a = sel name >>= \s -> call1 o s a
obj2 o name a b = sel name >>= \s -> call2 o s a b
obj3 o name a b c = sel name >>= \s -> call3 o s a b c
obj4 o name a b c d = sel name >>= \s -> call4 o s a b c d
obj5 o name a b c d e = sel name >>= \s -> call5 o s a b c d e
booleanValue o name = sel name >>= fmap (/= 0) . callBool o
booleanArgument o name arg = sel name >>= \s -> (/= 0) <$> callBool1 o s arg
long o name = sel name >>= callLong o
number o name = realToFrac <$> (sel name >>= callDouble o)
setNumber o name n = sel name >>= \s -> callSetDouble o s (realToFrac n)
int n = intPtrToPtr (fromIntegral (n :: Int))
flag b = int (if b then 1 else 0)
new name = cls name >>= \c -> obj0 c "new"
allocated name = cls name >>= \c -> obj0 c "alloc"
str :: Text -> IO Obj
str value = B.useAsCString (TE.encodeUtf8 value) $ \p -> cls "NSString" >>= \c -> obj1 c "stringWithUTF8String:" (castPtr p)
string :: Obj -> IO Text
string = fmap (fromMaybe T.empty) . optionalString

optionalString :: Obj -> IO (Maybe Text)
optionalString o | o == nullPtr = pure Nothing
optionalString o = obj0 o "UTF8String" >>= \p -> if p == nullPtr then pure Nothing else
    Just . TE.decodeUtf8With (\_ _ -> Just '\xfffd') <$> B.packCString (castPtr p)

withAutoreleasePool :: IO a -> IO a
withAutoreleasePool action = bracket (new "NSAutoreleasePool") (void . (`obj0` "drain")) (const action)
textProp o key = obj0 o key >>= string
setText o key value = str value >>= void . obj1 o key
rect o key (x,y,w,h) = sel key >>= \s -> callRect o s (realToFrac x) (realToFrac y) (realToFrac w) (realToFrac h)
rectObj o key (x,y,w,h) value = sel key >>= \s -> callRectObj o s (realToFrac x) (realToFrac y) (realToFrac w) (realToFrac h) value
window o w h style = sel "initWithContentRect:styleMask:backing:defer:" >>= \s -> callWindow o s (realToFrac w) (realToFrac h) style
size o key w h = sel key >>= \s -> callSize o s (realToFrac w) (realToFrac h)
height o = sel "bounds" >>= fmap realToFrac . callHeight o
method c name types fn = do s <- sel name; withCString types $ \t -> void (addMethod c s fn t)
subclass name = do base <- cls "NSObject"; withCString name $ \s -> allocateClass base s 0
invokeLong block value = peekByteOff block 16 >>= \fn -> blockLong fn block value
invokeObj block value = peekByteOff block 16 >>= \fn -> blockObj fn block value
bytes :: Obj -> IO B.ByteString
bytes d | d == nullPtr = pure B.empty
bytes d = do p <- obj0 d "bytes"; n <- long d "length"; B.packCStringLen (castPtr p, fromIntegral n)
dataObject :: B.ByteString -> IO Obj
dataObject b = B.useAsCStringLen b $ \(p,n) -> cls "NSData" >>= \c -> obj2 c "dataWithBytes:length:" (castPtr p) (int n)
jsonObject :: Obj -> IO (Maybe Value)
jsonObject value = do c <- cls "NSJSONSerialization"; d <- obj3 c "dataWithJSONObject:options:error:" value nullPtr nullPtr; decodeStrict <$> bytes d
url value = str value >>= \s -> cls "NSURL" >>= \c -> obj1 c "URLWithString:" s
fileURL value = str value >>= \s -> cls "NSURL" >>= \c -> obj1 c "fileURLWithPath:" s
loadURL view value = do u <- url value; c <- cls "NSURLRequest"; r <- obj1 c "requestWithURL:" u; void (obj1 view "loadRequest:" r)
script view value = str value >>= \s -> void (obj2 view "evaluateJavaScript:completionHandler:" s nullPtr)
objects array = do n <- long array "count"; mapM (obj1 array "objectAtIndex:" . int . fromIntegral) [0 .. n-1]
jsonText :: ToJSON a => a -> Text
jsonText = TE.decodeUtf8 . L.toStrict . encode

convertRect container source (x,y,w,h) = allocaArray 4 $ \result -> do
    selector <- sel "convertRect:fromView:"
    callConvertRect container selector source (realToFrac x) (realToFrac y) (realToFrac w) (realToFrac h) result
    [a,b,c,d] <- map realToFrac <$> peekArray 4 result
    pure (a,b,c,d)
