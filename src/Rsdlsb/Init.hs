{-# LANGUAGE OverloadedStrings #-}

module Rsdlsb.Init (
    run
) where 

import Foreign.C.Types (CInt)
import Foreign.C.String (CString, peekCString, withCAString)
import Foreign.Ptr (nullPtr)
import GHC.Word (Word32)
import Data.Bits ((.|.))
import Linear (V4(..))
import qualified SDL as SDL
import SDL (($=))

type ScreenSize = (CInt, CInt)

withSDL :: IO () -> IO ()
withSDL op = do
    SDL.initialize [SDL.InitEverything]
    op
    SDL.quit

withWindow :: String -> ScreenSize -> (SDL.Window -> IO ()) -> IO ()
withWindow title size op = do
    window <- createWindow title size
    op window
    SDL.destroyWindow window

withRenderer :: (SDL.Renderer -> IO ()) -> SDL.Window -> IO ()
withRenderer operation window = do
    _ <- SDL.setHintWithPriority SDL.OverridePriority SDL.HintRenderScaleQuality SDL.ScaleLinear
    renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
    operation renderer
    SDL.destroyRenderer renderer

withCAString2 :: String -> String -> (CString -> CString -> IO a) -> IO a
withCAString2 a b op = withCAString a $ \a' -> withCAString b $ op a'

createWindow :: String -> ScreenSize -> IO SDL.Window
createWindow title (w, h) = SDL.createWindow "Test" SDL.defaultWindow -- TODO: replace defaultWindow

inWindow :: String -> ScreenSize -> (SDL.Window -> IO ()) -> IO ()
inWindow name size = withSDL . withWindow name size

run :: IO ()
run = inWindow "Test" (800, 600) $ withRenderer $ \renderer -> do
    SDL.rendererDrawColor renderer $= V4 0 0 255 255
    SDL.clear renderer
    SDL.present renderer
    SDL.delay 2000
