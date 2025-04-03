module Main where

import qualified SDL
import qualified SDL.Font as Font
import Data.Word (Word8)
import SDL.Vect (V4(..))
import qualified Data.Text as T
import Control.Exception (catch, SomeException)
import System.IO (hPutStrLn, stderr)

main :: IO ()
main = do
    putStrLn "Initializing SDL..."
    
    -- Initialize SDL
    SDL.initialize [SDL.InitVideo]
    Font.initialize
    
    putStrLn "Created basic window..."
    
    -- Create a simple window
    window <- SDL.createWindow "SDL Test" SDL.defaultWindow
      { SDL.windowInitialSize = SDL.V2 800 600 }
    
    -- Create a renderer
    renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
    
    putStrLn "Testing font loading..."
    
    -- Try to load a font
    fontPaths <- sequence $ map tryFont [
        "/usr/share/fonts/truetype/dejavu/DejaVuSansMono.ttf",
        "/usr/share/fonts/TTF/DejaVuSansMono.ttf",
        "/usr/share/fonts/truetype/freefont/FreeMono.ttf",
        "/usr/share/fonts/liberation/LiberationMono-Regular.ttf"
      ]
    
    let availableFonts = [font | Just font <- fontPaths]
    font <- if null availableFonts
            then error "No fonts available"
            else do
                putStrLn $ "Successfully loaded a font!"
                return (head availableFonts)
    
    putStrLn "Setting initial color..."
    
    -- Set initial color to black for clearing
    SDL.rendererDrawColor renderer SDL.$= V4 0 0 0 255
    SDL.clear renderer
    
    putStrLn "Rendering a test text..."
    
    -- Try to render some text
    textSurface <- Font.blended font (V4 255 255 255 255) (T.pack "Test Text")
    texture <- SDL.createTextureFromSurface renderer textSurface
    SDL.freeSurface textSurface
    
    -- Get text dimensions
    SDL.TextureInfo _ _ w h <- SDL.queryTexture texture
    
    -- Display the texture
    let src = SDL.Rectangle (SDL.P (SDL.V2 0 0)) (SDL.V2 w h)
        dst = SDL.Rectangle (SDL.P (SDL.V2 50 50)) (SDL.V2 w h)
    SDL.copy renderer texture (Just src) (Just dst)
    
    -- Present the rendered frame
    SDL.present renderer
    
    putStrLn "Press Enter to exit..."
    _ <- getLine
    
    -- Clean up
    SDL.destroyTexture texture
    Font.free font
    SDL.destroyRenderer renderer
    SDL.destroyWindow window
    Font.quit
    SDL.quit
    
    putStrLn "SDL test completed successfully!"

tryFont :: String -> IO (Maybe Font.Font)
tryFont path = do
    putStrLn $ "Trying to load font: " ++ path
    (Just <$> Font.load path 24) `catch` \(e :: SomeException) -> do
        hPutStrLn stderr $ "Failed to load font " ++ path ++ ": " ++ show e
        return Nothing