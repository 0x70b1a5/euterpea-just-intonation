{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NondecreasingIndentation #-}

module TrackerSDL (startSDLTracker) where

import Control.Monad (unless, forM_, when)
import Data.IORef (IORef, newIORef, readIORef, writeIORef, modifyIORef)
import Data.Maybe (fromMaybe, isJust)
import Data.List (intercalate)
import System.FilePath (takeFileName)
import System.IO (hPutStrLn, stderr, hSetBuffering, BufferMode(NoBuffering), stdin, hSetEcho)
import System.Console.ANSI
import Control.Exception (catch, SomeException)
import qualified Data.Map as Map
import Foreign.C.Types (CInt)

-- SDL imports
import qualified SDL
import qualified SDL.Font as Font
import SDL.Vect (V2(..), V4(..))
import SDL.Video (Renderer, Window, Texture)
import SDL.Video.Renderer (Rectangle(..))
import Linear.Affine (Point(P))
import Foreign.C.Types (CInt)
import Data.Word (Word8)
import qualified Data.Text as T
import Data.StateVar (($=))

-- Import project modules
import TrackerTypes
import TrackerParser
import ErrorHandler

-- | SDL UI colors
colors :: Map.Map String (V4 Word8)
colors = Map.fromList
  [ ("background", V4 0 0 128 255)       -- Dark blue background
  , ("text", V4 192 192 192 255)         -- Light gray text
  , ("header", V4 255 255 255 255)       -- White headers
  , ("row_even", V4 17 17 17 255)        -- Dark row
  , ("row_odd", V4 0 0 0 255)            -- Black row
  , ("row_current", V4 0 68 0 255)       -- Green highlight for current row
  , ("tempo", V4 255 165 0 255)          -- Orange for tempo
  , ("note", V4 255 255 0 255)           -- Yellow for notes
  , ("instrument", V4 0 255 255 255)     -- Cyan for instruments
  , ("volume", V4 0 255 0 255)           -- Green for volume
  , ("effect", V4 255 0 255 255)         -- Magenta for effects
  , ("selected", V4 50 50 70 255)        -- Selection highlight
  ]

-- | Get a color from the map with default fallback
getColor :: String -> V4 Word8
getColor name = fromMaybe (V4 255 255 255 255) (Map.lookup name colors)

-- | Convert V4 Word8 to V4 CInt (for functions that need CInt colors)
toCIntColor :: V4 Word8 -> V4 CInt
toCIntColor (V4 r g b a) = V4 (fromIntegral r) (fromIntegral g) (fromIntegral b) (fromIntegral a)

-- | SDL UI size constants
screenWidth, screenHeight :: CInt
screenWidth = 1024
screenHeight = 768

-- | Cell size constants
cellWidth, cellHeight :: CInt
cellWidth = 90
cellHeight = 24

-- | Font size
fontSize :: Int
fontSize = 14

-- | Tracker UI Data
data TrackerUI = TrackerUI
  { uiWindow :: SDL.Window
  , uiRenderer :: SDL.Renderer
  , uiFont :: Font.Font
  , uiTrackerFile :: IORef TrackerFile
  , uiFilePath :: Maybe FilePath
  , uiCurrentRow :: IORef Int
  , uiScrollOffset :: IORef Int
  , uiSelectedCell :: IORef (Int, Int, Int)  -- (row, col, type) where type: 0=tempo, 1=note, 2=instrument, 3=volume, 4=effect
  , uiEditText :: IORef String
  , uiIsEditing :: IORef Bool
  , uiIsPlaying :: IORef Bool
  , uiAudioContext :: IORef (Maybe ())  -- Placeholder for audio context
  }

-- | Initialize SDL and create window
initSDL :: Maybe FilePath -> IO TrackerUI
initSDL filePath = do
  SDL.initialize [SDL.InitVideo]
  Font.initialize
  
  -- Create window
  window <- SDL.createWindow "Just Intonation Tracker" SDL.defaultWindow
    { SDL.windowInitialSize = SDL.V2 screenWidth screenHeight
    }
  
  -- Create renderer
  renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
  
  -- Load font (try system fonts first, then fall back to default)
  font <- (Font.load "/usr/share/fonts/truetype/dejavu/DejaVuSansMono.ttf" fontSize)
      `catch` \(_ :: SomeException) -> 
          (Font.load "/usr/share/fonts/TTF/DejaVuSansMono.ttf" fontSize)
      `catch` \(_ :: SomeException) -> 
          (Font.load "C:/Windows/Fonts/consola.ttf" fontSize)
      `catch` \(_ :: SomeException) -> do
          putStrLn "Warning: Could not load preferred monospace font"
          putStrLn "Attempting to use any available font..."
          Font.initialize
          let availableFonts = [] -- In a real app, we'd list available fonts
          if null availableFonts
              then error "No fonts available. Cannot continue."
              else Font.load (head availableFonts) fontSize
  
  -- Load tracker file
  tfRef <- case filePath of
    Just path -> do
      result <- readTrackerFile path
      case result of
        Right tf -> newIORef tf
        Left err -> do
          hPutStrLn stderr $ "Error loading file: " ++ err
          newIORef defaultTrackerFile
    Nothing -> newIORef defaultTrackerFile
  
  -- Create UI state references
  currentRowRef <- newIORef 0
  scrollOffsetRef <- newIORef 0
  selectedCellRef <- newIORef (0, 0, 0)  -- Start with first cell selected
  editTextRef <- newIORef ""
  isEditingRef <- newIORef False
  isPlayingRef <- newIORef False
  audioContextRef <- newIORef Nothing
  
  return TrackerUI
    { uiWindow = window
    , uiRenderer = renderer
    , uiFont = font
    , uiTrackerFile = tfRef
    , uiFilePath = filePath
    , uiCurrentRow = currentRowRef
    , uiScrollOffset = scrollOffsetRef
    , uiSelectedCell = selectedCellRef
    , uiEditText = editTextRef
    , uiIsEditing = isEditingRef
    , uiIsPlaying = isPlayingRef
    , uiAudioContext = audioContextRef
    }

-- | Clean up SDL resources
cleanupSDL :: TrackerUI -> IO ()
cleanupSDL ui = do
  Font.free (uiFont ui)
  SDL.destroyRenderer (uiRenderer ui)
  SDL.destroyWindow (uiWindow ui)
  Font.quit
  SDL.quit

-- | Draw text with specified color
drawText :: TrackerUI -> String -> (CInt, CInt) -> V4 Word8 -> IO ()
drawText ui text (x, y) color = do
  surface <- Font.blended (uiFont ui) color (T.pack text)
  texture <- SDL.createTextureFromSurface (uiRenderer ui) surface
  SDL.freeSurface surface
  
  -- Get text dimensions
  FontInfo { textureWidth = w, textureHeight = h } <- queryFont texture
  
  -- Render the text
  let src = SDL.Rectangle (P (SDL.V2 0 0)) (SDL.V2 w h)
      dst = SDL.Rectangle (P (SDL.V2 x y)) (SDL.V2 w h)
  SDL.copy (uiRenderer ui) texture (Just src) (Just dst)
  
  -- Clean up
  SDL.destroyTexture texture

-- | Helper to query texture dimensions
data FontInfo = FontInfo
  { textureWidth :: CInt
  , textureHeight :: CInt
  }

queryFont :: SDL.Texture -> IO FontInfo
queryFont texture = do
  SDL.TextureInfo _ _ w h <- SDL.queryTexture texture
  return FontInfo { textureWidth = w, textureHeight = h }

-- | Draw a rectangle
drawRect :: TrackerUI -> SDL.Rectangle CInt -> V4 Word8 -> IO ()
drawRect ui rect color = do
    -- Set the renderer color
    SDL.rendererDrawColor (uiRenderer ui) $= color
    -- Draw the rectangle
    SDL.fillRect (uiRenderer ui) (Just rect)

-- | Draw the header row
drawHeader :: TrackerUI -> IO ()
drawHeader ui = do
  -- Background for header
  drawRect ui (SDL.Rectangle (P (SDL.V2 0 0)) (SDL.V2 screenWidth cellHeight)) (getColor "row_even")
  
  -- Column headers
  let headerColor = getColor "header"
  drawText ui "Row" (10, 5) headerColor
  drawText ui "Tempo" (80, 5) headerColor
  
  -- Channel headers
  forM_ [0..3] $ \ch -> do
    let xStart = 170 + (fromIntegral ch * cellWidth * 4)
    drawText ui ("Ch" ++ show (ch + 1)) (xStart, 5) headerColor
    drawText ui "Note" (xStart + cellWidth, 5) headerColor
    drawText ui "Inst" (xStart + cellWidth * 2, 5) headerColor
    drawText ui "Vol" (xStart + cellWidth * 3, 5) headerColor
    drawText ui "Fx" (xStart + cellWidth * 4, 5) headerColor

-- | Draw a single tracker row
drawTrackerRow :: TrackerUI -> TrackerRow -> Int -> Int -> Bool -> IO ()
drawTrackerRow ui rowData rowIdx scrollOffset isCurrentRow = do
  -- Calculate Y position
  let y = fromIntegral ((rowIdx - scrollOffset) * fromIntegral cellHeight + fromIntegral cellHeight)
  
  -- Skip if row is not visible
  when (y >= cellHeight && y < screenHeight - cellHeight) $ do
    -- Check if this is the current playing row and set background accordingly
    let rowBgColor = if isCurrentRow
                     then getColor "row_current"
                     else if even rowIdx
                          then getColor "row_even"
                          else getColor "row_odd"
    
    -- Draw row background
    drawRect ui (SDL.Rectangle (P (SDL.V2 0 y)) (SDL.V2 screenWidth cellHeight)) rowBgColor
    
    -- Draw row number
    drawText ui (show rowIdx) (10, y + 3) (getColor "text")
    
    -- Draw tempo
    let tempoStr = case rowTempo rowData of
                     Just td -> tempoInput td
                     Nothing -> ""
    drawText ui tempoStr (80, y + 3) (getColor "tempo")
    
    -- Draw channel data
    forM_ (zip [0..] (rowChannels rowData)) $ \(chanIdx, chanData) -> do
      let xStart = 170 + (fromIntegral chanIdx * cellWidth * 4)
      
      -- Draw note
      let noteStr = case channelNote chanData of
                      Just nd -> noteInput nd
                      Nothing -> ""
      drawText ui noteStr (xStart + cellWidth, y + 3) (getColor "note")
      
      -- Draw instrument
      drawText ui (channelInstrument chanData) (xStart + cellWidth * 2, y + 3) (getColor "instrument")
      
      -- Draw volume
      let volStr = show (round (channelVolume chanData * 100) :: Int)
      drawText ui volStr (xStart + cellWidth * 3, y + 3) (getColor "volume")
      
      -- Draw effect
      let fxStr = case effectCommand (channelEffect chanData) of
                    Just cmd -> [cmd] ++ fromMaybe "" (effectValue (channelEffect chanData))
                    Nothing -> ""
      drawText ui fxStr (xStart + cellWidth * 4, y + 3) (getColor "effect")

-- | Draw all tracker rows
drawTrackerRows :: TrackerUI -> IO ()
drawTrackerRows ui = do
  -- Get current tracker data and state
  tf <- readIORef (uiTrackerFile ui)
  scrollOffset <- readIORef (uiScrollOffset ui)
  currentRow <- readIORef (uiCurrentRow ui)
  
  -- Draw each visible row
  forM_ [0..(numRows tf - 1)] $ \rowIdx -> do
    let rowData = if rowIdx < length (trackerData tf)
                  then trackerData tf !! rowIdx
                  else emptyTrackerRow (numChannels tf)
    
    drawTrackerRow ui rowData rowIdx scrollOffset (rowIdx == currentRow)

-- | Draw the current selection highlight
drawSelection :: TrackerUI -> IO ()
drawSelection ui = do
  (row, col, cellType) <- readIORef (uiSelectedCell ui)
  scrollOffset <- readIORef (uiScrollOffset ui)
  isEditing <- readIORef (uiIsEditing ui)
  
  -- Calculate position of selected cell
  let y = fromIntegral ((row - scrollOffset) * fromIntegral cellHeight + fromIntegral cellHeight)
      x = if cellType == 0 
          then 80  -- Tempo column
          else 170 + (fromIntegral col * cellWidth * 4) + (fromIntegral cellType * cellWidth)
  
  -- Only draw if the selection is visible
  when (y >= cellHeight && y < screenHeight - cellHeight) $ do
    -- Draw highlight rectangle
    let highlightColor = if isEditing 
                         then getColor "selected" 
                         else V4 100 100 150 100
    
    drawRect ui (SDL.Rectangle (P (SDL.V2 x y)) (SDL.V2 cellWidth cellHeight)) highlightColor

-- | Draw the status bar
drawStatusBar :: TrackerUI -> IO ()
drawStatusBar ui = do
  tf <- readIORef (uiTrackerFile ui)
  isPlaying <- readIORef (uiIsPlaying ui)
  isEditing <- readIORef (uiIsEditing ui)
  (row, col, cellType) <- readIORef (uiSelectedCell ui)
  
  -- Draw status bar background
  let statusY = screenHeight - cellHeight
  drawRect ui (SDL.Rectangle (P (SDL.V2 0 statusY)) (SDL.V2 screenWidth cellHeight)) (getColor "row_even")
  
  -- Draw file info
  let fileInfo = case uiFilePath ui of
                   Just path -> "File: " ++ takeFileName path
                   Nothing -> "New Tracker File"
  drawText ui fileInfo (10, statusY + 3) (getColor "text")
  
  -- Draw tracker config info
  let configInfo = "BPM: " ++ show (baseTempo tf) ++ " | Rows/Beat: " ++ show (rowsPerBeat tf)
  drawText ui configInfo (250, statusY + 3) (getColor "text")
  
  -- Draw playback status
  let playStatus = if isPlaying then "Playing" else "Stopped"
  drawText ui playStatus (500, statusY + 3) (getColor "text")
  
  -- Draw editing status
  let editStatus = if isEditing 
                   then "Editing: Row " ++ show row ++ ", Col " ++ show col
                   else "Press Enter to edit selected cell, Arrows to navigate"
  drawText ui editStatus (600, statusY + 3) (getColor "text")

-- | Draw edit box when editing a cell
drawEditBox :: TrackerUI -> IO ()
drawEditBox ui = do
  isEditing <- readIORef (uiIsEditing ui)
  
  when isEditing $ do
    (row, col, cellType) <- readIORef (uiSelectedCell ui)
    scrollOffset <- readIORef (uiScrollOffset ui)
    editText <- readIORef (uiEditText ui)
    
    -- Calculate position
    let y = fromIntegral ((row - scrollOffset) * fromIntegral cellHeight + fromIntegral cellHeight)
        x = if cellType == 0 
            then 80  -- Tempo column
            else 170 + (fromIntegral col * cellWidth * 4) + (fromIntegral cellType * cellWidth)
    
    -- Only draw if the edit box is visible
    when (y >= cellHeight && y < screenHeight - cellHeight) $ do
      -- Draw edit background
      drawRect ui (SDL.Rectangle (P (SDL.V2 x y)) (SDL.V2 cellWidth cellHeight)) (getColor "selected")
      
      -- Draw edit text with cursor
      drawText ui (editText ++ "_") (x + 5, y + 3) (getColor "text")

-- | Draw the entire tracker UI
drawUI :: TrackerUI -> IO ()
drawUI ui = do
    -- Clear screen with background color
    SDL.rendererDrawColor (uiRenderer ui) $= (getColor "background")
    SDL.clear (uiRenderer ui)
    
    -- Draw components
    drawHeader ui
    drawTrackerRows ui
    drawSelection ui
    drawStatusBar ui
    drawEditBox ui
    
    -- Present the rendered frame
    SDL.present (uiRenderer ui)

-- | Get cell value
getCellValue :: TrackerUI -> Int -> Int -> Int -> IO String
getCellValue ui row col cellType = do
  tf <- readIORef (uiTrackerFile ui)
  
  let rowData = if row < length (trackerData tf)
                then trackerData tf !! row
                else emptyTrackerRow (numChannels tf)
  
  case cellType of
    0 -> -- Tempo
      return $ case rowTempo rowData of
                 Just td -> tempoInput td
                 Nothing -> ""
    1 -> -- Note
      if col < length (rowChannels rowData) 
      then return $ case channelNote (rowChannels rowData !! col) of
                      Just nd -> noteInput nd
                      Nothing -> ""
      else return ""
    2 -> -- Instrument
      if col < length (rowChannels rowData)
      then return $ channelInstrument (rowChannels rowData !! col)
      else return ""
    3 -> -- Volume
      if col < length (rowChannels rowData)
      then return $ show (round (channelVolume (rowChannels rowData !! col) * 100) :: Int)
      else return ""
    4 -> -- Effect
      if col < length (rowChannels rowData)
      then let effect = channelEffect (rowChannels rowData !! col)
           in return $ case effectCommand effect of
                         Just cmd -> [cmd] ++ fromMaybe "" (effectValue effect)
                         Nothing -> ""
      else return ""
    _ -> return ""

-- | Start cell editing
startCellEditing :: TrackerUI -> IO ()
startCellEditing ui = do
  (row, col, cellType) <- readIORef (uiSelectedCell ui)
  
  -- Get current value
  value <- getCellValue ui row col cellType
  
  -- Set edit state
  writeIORef (uiEditText ui) value
  writeIORef (uiIsEditing ui) True

-- | Apply cell edit
applyCellEdit :: TrackerUI -> IO ()
applyCellEdit ui = do
  isEditing <- readIORef (uiIsEditing ui)
  
  when isEditing $ do
    (row, col, cellType) <- readIORef (uiSelectedCell ui)
    editText <- readIORef (uiEditText ui)
    
    -- Update tracker data
    modifyIORef (uiTrackerFile ui) $ \tf -> 
      let rowsData = trackerData tf
          updateRow r = 
            if r == row && row < length rowsData
            then let currentRow = rowsData !! r
                     updatedRow = case cellType of
                       0 -> -- Tempo
                         currentRow { rowTempo = if null editText 
                                                 then Nothing 
                                                 else Just $ TempoData editText (parseTempo editText (baseTempo tf)) }
                       _ -> -- Channel data
                         if col < length (rowChannels currentRow)
                         then let channels = rowChannels currentRow
                                  currentChan = channels !! col
                                  updatedChan = case cellType of
                                    1 -> -- Note
                                      currentChan { channelNote = if null editText
                                                                  then Nothing
                                                                  else case parseNoteInput editText (baseFrequency tf) of
                                                                         Just (freq, ratio) -> Just $ NoteData editText (Just freq) ratio
                                                                         Nothing -> Just $ NoteData editText Nothing Nothing }
                                    2 -> -- Instrument
                                      currentChan { channelInstrument = if null editText then "sin" else editText }
                                    3 -> -- Volume
                                      currentChan { channelVolume = case reads editText :: [(Double, String)] of
                                                                       [(v, "")] -> max 0.0 (min 1.0 (v / 100.0))
                                                                       _ -> 1.0 }
                                    4 -> -- Effect
                                      currentChan { channelEffect = if null editText
                                                                    then emptyEffectData
                                                                    else let cmd = if not (null editText) then Just (head editText) else Nothing
                                                                             val = if length editText > 1 then Just (tail editText) else Nothing
                                                                         in EffectData cmd val editText }
                                    _ -> currentChan
                              in currentRow { rowChannels = take col channels ++ [updatedChan] ++ drop (col + 1) channels }
                         else currentRow
                 in updatedRow
            else rowsData !! r
      in tf { trackerData = if row < length rowsData
                            then map updateRow [0..(length rowsData - 1)]
                            else trackerData tf }
    
    -- Exit editing mode
    writeIORef (uiIsEditing ui) False

-- | Cancel cell editing
cancelCellEditing :: TrackerUI -> IO ()
cancelCellEditing ui = writeIORef (uiIsEditing ui) False

-- | Move selection
moveSelection :: TrackerUI -> Int -> Int -> Int -> IO ()
moveSelection ui rowDelta colDelta typeDelta = do
  (row, col, cellType) <- readIORef (uiSelectedCell ui)
  tf <- readIORef (uiTrackerFile ui)
  
  -- Calculate new position
  let newRow = max 0 (min (numRows tf - 1) (row + rowDelta))
      numCols = numChannels tf
      newCol = max 0 (min (numCols - 1) (col + colDelta))
      newCellType = max 0 (min 4 (cellType + typeDelta))
  
  -- Update selection
  writeIORef (uiSelectedCell ui) (newRow, newCol, newCellType)
  
  -- Adjust scroll if needed
  scrollOffset <- readIORef (uiScrollOffset ui)
  let visibleRows = (screenHeight - cellHeight * 2) `div` cellHeight
      
  when (newRow < scrollOffset) $
    writeIORef (uiScrollOffset ui) newRow
    
  when (newRow >= scrollOffset + fromIntegral visibleRows) $
    writeIORef (uiScrollOffset ui) (newRow - fromIntegral visibleRows + 1)

-- | Save tracker file
saveTrackerFile :: TrackerUI -> IO ()
saveTrackerFile ui = do
  tf <- readIORef (uiTrackerFile ui)
  case uiFilePath ui of
    Just path -> writeTrackerFile path tf
    Nothing -> return ()

-- | Toggle playback
togglePlayback :: TrackerUI -> IO ()
togglePlayback ui = do
  isPlaying <- readIORef (uiIsPlaying ui)
  
  if isPlaying
    then do
      -- Stop playback
      writeIORef (uiIsPlaying ui) False
      writeIORef (uiAudioContext ui) Nothing  -- Clean up audio resources
    else do
      -- Start playback
      writeIORef (uiIsPlaying ui) True
      -- TODO: Initialize proper audio playback
      writeIORef (uiAudioContext ui) (Just ())  -- Placeholder for audio context

-- | Process a single key press
handleKeyPress :: TrackerUI -> SDL.Keycode -> IO Bool
handleKeyPress ui keycode = do
  isEditing <- readIORef (uiIsEditing ui)
  
  if isEditing
    then case keycode of
      SDL.KeycodeReturn -> do
        applyCellEdit ui
        return False
      SDL.KeycodeEscape -> do
        cancelCellEditing ui
        return False
      SDL.KeycodeBackspace -> do
        modifyIORef (uiEditText ui) $ \text ->
          if not (null text) then init text else ""
        return False
      _ -> do
        -- Handle printable characters for editing
        let charMap = Map.fromList
              [ (SDL.KeycodeSpace, ' ')
              , (SDL.Keycode0, '0'), (SDL.Keycode1, '1'), (SDL.Keycode2, '2'), (SDL.Keycode3, '3'), (SDL.Keycode4, '4')
              , (SDL.Keycode5, '5'), (SDL.Keycode6, '6'), (SDL.Keycode7, '7'), (SDL.Keycode8, '8'), (SDL.Keycode9, '9')
              , (SDL.KeycodeA, 'a'), (SDL.KeycodeB, 'b'), (SDL.KeycodeC, 'c'), (SDL.KeycodeD, 'd'), (SDL.KeycodeE, 'e')
              , (SDL.KeycodeF, 'f'), (SDL.KeycodeG, 'g'), (SDL.KeycodeH, 'h'), (SDL.KeycodeI, 'i'), (SDL.KeycodeJ, 'j')
              , (SDL.KeycodeK, 'k'), (SDL.KeycodeL, 'l'), (SDL.KeycodeM, 'm'), (SDL.KeycodeN, 'n'), (SDL.KeycodeO, 'o')
              , (SDL.KeycodeP, 'p'), (SDL.KeycodeQ, 'q'), (SDL.KeycodeR, 'r'), (SDL.KeycodeS, 's'), (SDL.KeycodeT, 't')
              , (SDL.KeycodeU, 'u'), (SDL.KeycodeV, 'v'), (SDL.KeycodeW, 'w'), (SDL.KeycodeX, 'x'), (SDL.KeycodeY, 'y')
              , (SDL.KeycodeZ, 'z'), (SDL.KeycodePeriod, '.'), (SDL.KeycodeSlash, '/'), (SDL.KeycodeMinus, '-')
              , (SDL.KeycodeRightBracket, ']'), (SDL.KeycodeLeftBracket, '[')
              ]
        case Map.lookup keycode charMap of
          Just char -> do
            modifyIORef (uiEditText ui) (++ [char])
            return False
          Nothing -> return False
    else case keycode of
      SDL.KeycodeQ -> return True  -- Quit
      SDL.KeycodeUp -> moveSelection ui (-1) 0 0 >> return False
      SDL.KeycodeDown -> moveSelection ui 1 0 0 >> return False
      SDL.KeycodeLeft -> moveSelection ui 0 0 (-1) >> return False
      SDL.KeycodeRight -> moveSelection ui 0 0 1 >> return False
      SDL.KeycodeTab -> moveSelection ui 0 1 0 >> return False
      SDL.KeycodeReturn -> startCellEditing ui >> return False
      SDL.KeycodeS -> do
        -- Save when S is pressed with modifier
        saveTrackerFile ui
        return False
      SDL.KeycodeSpace -> do
        -- Toggle playback with space
        togglePlayback ui
        return False
      _ -> return False

-- | Main event loop
eventLoop :: TrackerUI -> IO ()
eventLoop ui = do
  -- Handle events
  events <- SDL.pollEvents
  let quit = any isQuitEvent events
      keyEvents = filter isKeyEvent events
  
  -- Process key presses
  quitFromKey <- mapM (processKeyEvent ui) keyEvents
  let quitFromKeys = or quitFromKey
  
  -- Update UI if playing
  isPlaying <- readIORef (uiIsPlaying ui)
  when isPlaying $ do
    -- Update current row for playback
    currentRow <- readIORef (uiCurrentRow ui)
    tf <- readIORef (uiTrackerFile ui)
    let nextRow = (currentRow + 1) `mod` numRows tf
    writeIORef (uiCurrentRow ui) nextRow
  
  -- Draw UI
  drawUI ui
  
  -- Continue loop or quit
  unless (quit || quitFromKeys) $
    SDL.delay 16 >> eventLoop ui  -- ~60 FPS
  where
    isQuitEvent event = 
      case SDL.eventPayload event of
        SDL.WindowClosedEvent _ -> True
        _ -> False
        
    isKeyEvent event =
      case SDL.eventPayload event of
        SDL.KeyboardEvent _ -> True
        _ -> False
        
    processKeyEvent ui event = 
      case SDL.eventPayload event of
        SDL.KeyboardEvent keyboardEvent ->
          if SDL.keyboardEventKeyMotion keyboardEvent == SDL.Pressed
          then handleKeyPress ui (SDL.keysymKeycode (SDL.keyboardEventKeysym keyboardEvent))
          else return False
        _ -> return False

-- | Start the SDL-based tracker interface
startSDLTracker :: Maybe FilePath -> IO ()
startSDLTracker filePath = withErrorHandling $ do
  putStrLn "Starting SDL-based tracker interface..."
  
  -- Check if SDL is available by trying to initialize
  sdlAvailable <- (SDL.initialize [SDL.InitVideo] >> return True) 
               `catch` \(_ :: SomeException) -> do
                   putStrLn "Error: Could not initialize SDL!"
                   putStrLn "Please make sure SDL2 and SDL2_ttf libraries are installed on your system."
                   putStrLn "On Ubuntu/Debian: sudo apt-get install libsdl2-dev libsdl2-ttf-dev"
                   putStrLn "On Fedora/RHEL: sudo dnf install SDL2-devel SDL2_ttf-devel"
                   putStrLn "On macOS: brew install sdl2 sdl2_ttf"
                   putStrLn "On Windows: Download from https://www.libsdl.org/download-2.0.php"
                   putStrLn "Press Enter to return to menu..."
                   _ <- getLine
                   return False
  
  when sdlAvailable $ do
    -- Clean up from our test initialization
    SDL.quit
    
    -- Now initialize properly
    ui <- initSDL filePath
    eventLoop ui
    cleanupSDL ui