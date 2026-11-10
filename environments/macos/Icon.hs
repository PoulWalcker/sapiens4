{-# LANGUAGE OverloadedStrings #-}
module Icon where

import Control.Monad
import qualified Data.Text as T
import Foreign hiding (void, new)
import ObjC
import System.FilePath

data IconSize = IconSize { dimension :: Int, scale :: Int }

iconSizes :: [IconSize]
iconSizes = [IconSize size density | size <- [16,32,128,256,512], density <- [1,2]]

iconFile :: IconSize -> FilePath
iconFile size = "icon_" ++ show (dimension size) ++ "x" ++ show (dimension size)
    ++ (if scale size == 2 then "@2x" else "") ++ ".png"

createIcon :: FilePath -> FilePath -> IO ()
createIcon directory source = do
    loadFrameworks
    withAutoreleasePool $ do
        image <- allocated "NSImage" >>= \o -> str (T.pack source) >>= obj1 o "initWithContentsOfFile:"
        when (image == nullPtr) $ ioError (userError "Cannot load workspace favicon SVG")
        mapM_ (writeIcon directory image) iconSizes

writeIcon :: FilePath -> Obj -> IconSize -> IO ()
writeIcon directory image size = do
        let pixels = dimension size * scale size
        bitmap <- allocated "NSBitmapImageRep"
        color <- str "NSDeviceRGBColorSpace"
        selector <- sel "initWithBitmapDataPlanes:pixelsWide:pixelsHigh:bitsPerSample:samplesPerPixel:hasAlpha:isPlanar:colorSpaceName:bytesPerRow:bitsPerPixel:"
        representation <- callBitmap bitmap selector nullPtr (int pixels) (int pixels) (int 8) (int 4) (int 1) (int 0) color (int 0) (int 0)
        contextClass <- cls "NSGraphicsContext"
        void (obj0 contextClass "saveGraphicsState")
        context <- obj1 contextClass "graphicsContextWithBitmapImageRep:" representation
        void (obj1 contextClass "setCurrentContext:" context)
        drawSelector <- sel "drawInRect:fromRect:operation:fraction:"
        callDraw image drawSelector (fromIntegral pixels)
        void (obj0 contextClass "restoreGraphicsState")
        properties <- new "NSDictionary"
        dataValue <- obj2 representation "representationUsingType:properties:" (int 4) properties
        destination <- str (T.pack (directory </> iconFile size))
        result <- obj2 dataValue "writeToFile:atomically:" destination (int 1)
        when (result == nullPtr) $ ioError (userError "Could not write icon")
