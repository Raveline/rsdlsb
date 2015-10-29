
module Rsdlsb.Init (
    run
) where 

import Foreign.C.Types (CInt)
import Foreign.C.String (CString, peekCString, withCAString)
import Foreign.Ptr (nullPtr)
import GHC.Word (Word32)
import Data.Bits ((.|.))
import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Image as Image

type ScreenSize = (CInt, CInt)
type Risky a = Either String a

initializeSDL :: [Word32] -> IO (Risky ())
initializeSDL flags = do
    initSuccess <- SDL.init $ foldl (.|.) 0 flags
    return $ if initSuccess < 0 then Left "SDL could not initialize !" else Right ()

withSDL :: IO () -> IO ()
withSDL op = do
    initializeSDL [SDL.SDL_INIT_VIDEO] >>= either throwSDLError return
    op
    SDL.quit

logWarning :: Risky Bool -> IO Bool
logWarning = either (\x -> print x >> return False) return

withWindow :: String -> ScreenSize -> (SDL.Window -> IO ()) -> IO ()
withWindow title size op = do
    window <- createWindow title size >>= either throwSDLError return
    op window
    SDL.destroyWindow window

withRenderer :: (SDL.Renderer -> IO ()) -> SDL.Window -> IO ()
withRenderer operation window = do
    _ <- setHint "SDL_RENDER_SCALE_QUALITY" "1" >>= logWarning
    renderer <- createRenderer window (-1) [SDL.SDL_RENDERER_ACCELERATED] >>= either throwSDLError return
    operation renderer
    SDL.destroyRenderer renderer

createRenderer :: SDL.Window -> CInt -> [Word32] -> IO (Risky SDL.Renderer)
createRenderer window index flags = do
    renderer <- SDL.createRenderer window index $ foldl (.|.) 0 flags
    return $ if renderer == nullPtr then Left "Renderer could not be created !" else Right renderer

setHint :: String -> String -> IO (Risky Bool)
setHint hint value = do
    result <- withCAString2 hint value SDL.setHint
    return $ if not result then Left "Warning: Linear texture filtering not enabled !" else Right result

withCAString2 :: String -> String -> (CString -> CString -> IO a) -> IO a
withCAString2 a b op = withCAString a $ \a' -> withCAString b $ op a'

throwSDLError :: String -> IO a
throwSDLError message = do
    errorString <- SDL.getError >>= peekCString
    fail (message ++ " SDL_ERROR: " ++ errorString)

createWindow :: String -> ScreenSize -> IO (Risky SDL.Window)
createWindow title (w, h) = withCAString title $ \ctitle -> do
    window <- SDL.createWindow ctitle SDL.SDL_WINDOWPOS_UNDEFINED SDL.SDL_WINDOWPOS_UNDEFINED w h SDL.SDL_WINDOW_SHOWN
    return $ if window == nullPtr then Left "Window could not be created !" else Right window

inWindow :: String -> ScreenSize -> (SDL.Window -> IO ()) -> IO ()
inWindow name size = withSDL . withWindow name size

run :: IO ()
run = inWindow "Test" (800, 600) $ withRenderer $ \renderer -> do
    _ <- SDL.setRenderDrawColor renderer 0xFF 0xFF 0xFF 0xFF
    SDL.delay 2000
