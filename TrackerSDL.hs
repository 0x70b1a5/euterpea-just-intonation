{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NondecreasingIndentation #-}

module TrackerSDL (startSDLTracker, testSDL) where

import Control.Monad (forM_, when, unless)
import Data.IORef (IORef, newIORef, readIORef, writeIORef, modifyIORef)
import Data.Maybe (fromMaybe)
import System.FilePath (takeFileName, takeBaseName, takeExtension, replaceExtension, (</>))
import System.IO (hPutStrLn, stderr, hGetContents)
import Control.Exception (catch, SomeException)
import qualified Data.Map as Map
import Foreign.C.Types (CInt)
import qualified System.Process
import System.Directory (getCurrentDirectory)
import Data.List (isInfixOf)

-- SDL imports
import qualified SDL
import qualified SDL.Font as Font
import SDL.Vect (V2(..), V4(..))
import Linear.Affine (Point(P))
import Data.Word (Word8)
import qualified Data.Text as T
import Data.StateVar (($=))

-- Import project modules
import TrackerTypes
import TrackerParser
import TrackerToMusic
import JustIntonationCore

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

-- | SDL UI size constants
screenWidth, screenHeight :: CInt
screenWidth = 1280  -- Larger default width
screenHeight = 800  -- Larger default height

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
  , uiFilePath :: IORef (Maybe FilePath)   -- Changed to IORef for dynamic updates
  , uiCurrentRow :: IORef Int
  , uiScrollOffset :: IORef Int            -- Vertical scroll offset
  , uiHScrollOffset :: IORef Int           -- Horizontal scroll offset
  , uiSelectedCell :: IORef (Int, Int, Int)  -- (row, col, type) where type: 0=tempo, 1=note, 2=instrument, 3=volume, 4=effect
  , uiEditText :: IORef String
  , uiIsEditing :: IORef Bool
  , uiIsPlaying :: IORef Bool
  , uiAudioContext :: IORef (Maybe ())      -- Placeholder for audio context
  , uiWindowSize :: IORef (CInt, CInt)      -- Current window size
  , uiShowHelp :: IORef Bool                -- Whether to show the help overlay
  , uiShowFileSaveDialog :: IORef Bool      -- Whether to show the file save dialog
  , uiSaveFilename :: IORef String          -- Current filename in save dialog
  , uiShowFileOpenDialog :: IORef Bool      -- Whether to show the file open dialog
  , uiOpenFilename :: IORef String          -- Current filename in open dialog
  , uiShowRenderDialog :: IORef Bool        -- Whether to show the render dialog
  , uiRenderFilename :: IORef String        -- Current filename in render dialog
  }

-- | Helper function to try fonts in order until one works
tryLoadFont :: [FilePath] -> Int -> IO Font.Font
tryLoadFont [] size = do
  putStrLn "ERROR: No usable fonts found!"
  putStrLn "You may need to install SDL2_ttf and some TTF fonts."
  putStrLn "Press Enter to continue..."
  _ <- getLine
  error "No usable fonts found. Cannot continue."
  
tryLoadFont (path:paths) size = do
  putStrLn $ "Trying to load font: " ++ path
  result <- (Just <$> Font.load path size) `catch` \(_ :: SomeException) -> return Nothing
  case result of
    Just loadedFont -> do
      putStrLn $ "Successfully loaded font: " ++ path
      return loadedFont
    Nothing -> do
      putStrLn $ "Failed to load font: " ++ path
      tryLoadFont paths size

-- | Initialize SDL and create window
initSDL :: Maybe FilePath -> IO TrackerUI
initSDL filePath = do
  SDL.initialize [SDL.InitVideo]
  Font.initialize
  
  -- Create window
  window <- SDL.createWindow "Just Intonation Tracker" SDL.defaultWindow
    { SDL.windowInitialSize = SDL.V2 screenWidth screenHeight,
      SDL.windowResizable = True -- Make window resizable
    }
  
  -- Create renderer
  renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
  
  -- Load font (try system fonts first, then fall back to default)
  putStrLn "Loading font..."
  
  -- Try to load a font from various possible paths
  font <- tryLoadFont [
      -- Linux paths
      "/usr/share/fonts/truetype/dejavu/DejaVuSansMono.ttf",
      "/usr/share/fonts/TTF/DejaVuSansMono.ttf",
      "/usr/share/fonts/truetype/freefont/FreeMono.ttf",
      "/usr/share/fonts/liberation/LiberationMono-Regular.ttf",
      -- Windows paths
      "C:/Windows/Fonts/consola.ttf",
      "C:/Windows/Fonts/cour.ttf",
      -- macOS paths
      "/Library/Fonts/Courier New.ttf",
      "/System/Library/Fonts/Monaco.ttf"
    ] fontSize
  
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
  hScrollOffsetRef <- newIORef 0  -- Start with no horizontal scrolling
  selectedCellRef <- newIORef (0, 0, 0)  -- Start with first cell selected
  editTextRef <- newIORef ""
  isEditingRef <- newIORef False
  isPlayingRef <- newIORef False
  audioContextRef <- newIORef Nothing
  windowSizeRef <- newIORef (screenWidth, screenHeight)  -- Initialize with default size
  filePathRef <- newIORef filePath
  showHelpRef <- newIORef False
  showFileSaveDialogRef <- newIORef False
  saveFilenameRef <- newIORef (case filePath of 
                                Just path -> path
                                Nothing -> "tracker.json")
  showFileOpenDialogRef <- newIORef False
  openFilenameRef <- newIORef "tracker.json"
  showRenderDialogRef <- newIORef False
  renderFilenameRef <- newIORef "output.wav"
  
  -- Initialize tracker file and ensure it has proper rows
  modifyIORef tfRef $ \tf -> 
    if null (trackerData tf) 
    then tf { trackerData = replicate (numRows tf) (emptyTrackerRow (numChannels tf)) }
    else tf
    
  tf <- readIORef tfRef
  putStrLn $ "Loaded tracker with " ++ show (numRows tf) ++ " rows and " ++ show (numChannels tf) ++ " channels, data rows: " ++ show (length (trackerData tf))
  
  return TrackerUI
    { uiWindow = window
    , uiRenderer = renderer
    , uiFont = font
    , uiTrackerFile = tfRef
    , uiFilePath = filePathRef
    , uiCurrentRow = currentRowRef
    , uiScrollOffset = scrollOffsetRef
    , uiHScrollOffset = hScrollOffsetRef
    , uiSelectedCell = selectedCellRef
    , uiEditText = editTextRef
    , uiIsEditing = isEditingRef
    , uiIsPlaying = isPlayingRef
    , uiAudioContext = audioContextRef
    , uiWindowSize = windowSizeRef
    , uiShowHelp = showHelpRef
    , uiShowFileSaveDialog = showFileSaveDialogRef
    , uiSaveFilename = saveFilenameRef
    , uiShowFileOpenDialog = showFileOpenDialogRef
    , uiOpenFilename = openFilenameRef
    , uiShowRenderDialog = showRenderDialogRef
    , uiRenderFilename = renderFilenameRef
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
  -- Skip drawing if text is empty or just whitespace
  if null text || all isSpace text
    then return ()
    else do
      -- Render the text safely
      catch
        (do
          surface <- Font.blended (uiFont ui) color (T.pack text)
          texture <- SDL.createTextureFromSurface (uiRenderer ui) surface
          SDL.freeSurface surface
          
          -- Get text dimensions
          result <- catch
            (do
              FontInfo { textureWidth = w, textureHeight = h } <- queryFont texture
              -- Only render if text has valid dimensions
              when (w > 0 && h > 0) $ do
                -- Render the text
                let src = SDL.Rectangle (P (SDL.V2 0 0)) (SDL.V2 w h)
                    dst = SDL.Rectangle (P (SDL.V2 x y)) (SDL.V2 w h)
                SDL.copy (uiRenderer ui) texture (Just src) (Just dst)
              return True
            )
            (\(_ :: SomeException) -> return False)
          
          -- Clean up
          SDL.destroyTexture texture
        )
        (\(e :: SomeException) -> do
          -- Just log the error and continue
          putStrLn $ "Text rendering error for '" ++ text ++ "': " ++ show e
          return ()
        )
  where
    isSpace c = c == ' ' || c == '\t' || c == '\n' || c == '\r'

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
  -- Get current window size and scroll offset
  (windowWidth, _) <- readIORef (uiWindowSize ui)
  hScrollOffset <- readIORef (uiHScrollOffset ui)
  tf <- readIORef (uiTrackerFile ui)
  
  -- Background for header
  drawRect ui (SDL.Rectangle (P (SDL.V2 0 0)) (SDL.V2 windowWidth cellHeight)) (getColor "row_even")
  
  -- Column headers
  let headerColor = getColor "header"
  
  -- Row header (always visible)
  drawText ui "Row" (10, 5) headerColor
  
  -- Tempo header (visible only if not scrolled too far)
  let tempoX = 80 - fromIntegral hScrollOffset
  when (tempoX >= 0) $
    drawText ui "Tempo" (tempoX, 5) headerColor
  
  -- Channel headers with horizontal scrolling
  forM_ [0..(numChannels tf - 1)] $ \ch -> do
    let baseX = 170 + (fromIntegral ch * cellWidth * 4)
        adjustedX = baseX - fromIntegral hScrollOffset
    
    -- Only draw headers if they are visible
    when (adjustedX + cellWidth * 4 > 0 && adjustedX < windowWidth) $ do
      -- Channel header - ensure it doesn't overlap with the next cell
      when (adjustedX > 0 && adjustedX < windowWidth - cellWidth) $ 
        drawText ui ("Ch" ++ show (ch + 1)) (adjustedX, 5) headerColor
      
      -- Note header
      let noteX = adjustedX + cellWidth
      when (noteX > 0 && noteX < windowWidth - cellWidth) $ 
        drawText ui "Note" (noteX, 5) headerColor
      
      -- Instrument header
      let instX = adjustedX + cellWidth * 2
      when (instX > 0 && instX < windowWidth - cellWidth) $ 
        drawText ui "Inst" (instX, 5) headerColor
      
      -- Volume header
      let volX = adjustedX + cellWidth * 3  
      when (volX > 0 && volX < windowWidth - cellWidth) $ 
        drawText ui "Vol" (volX, 5) headerColor
      
      -- Effect header
      let fxX = adjustedX + cellWidth * 4
      when (fxX > 0 && fxX < windowWidth) $ 
        drawText ui "Fx" (fxX, 5) headerColor

-- | Draw a single tracker row
drawTrackerRow :: TrackerUI -> TrackerRow -> Int -> Int -> Int -> Bool -> IO ()
drawTrackerRow ui rowData rowIdx vScrollOffset hScrollOffset isCurrentRow = do
  -- Get window size
  (windowWidth, windowHeight) <- readIORef (uiWindowSize ui)
  
  -- Calculate Y position
  let y = fromIntegral ((rowIdx - vScrollOffset) * fromIntegral cellHeight + fromIntegral cellHeight)
  
  -- Skip if row is not visible
  when (y >= cellHeight && y < windowHeight - cellHeight) $ do
    -- Check if this is the current playing row and set background accordingly
    let rowBgColor = if isCurrentRow
                     then getColor "row_current"
                     else if even rowIdx
                          then getColor "row_even"
                          else getColor "row_odd"
    
    -- Draw row background
    drawRect ui (SDL.Rectangle (P (SDL.V2 0 y)) (SDL.V2 windowWidth cellHeight)) rowBgColor
    
    -- Draw row number (always visible regardless of horizontal scroll)
    drawText ui (show rowIdx) (10, y + 3) (getColor "text")
    
    -- Draw tempo (only if it's visible with the current hScroll)
    let tempoX = 80
    when (tempoX - fromIntegral hScrollOffset >= 0) $ do
      let tempoStr = case rowTempo rowData of
                       Just td -> tempoInput td
                       Nothing -> ""
      drawText ui tempoStr (tempoX - fromIntegral hScrollOffset, y + 3) (getColor "tempo")
    
    -- Draw channel data with horizontal scrolling
    forM_ (zip [0..] (rowChannels rowData)) $ \(chanIdx, chanData) -> do
      let baseX = 170 + (fromIntegral chanIdx * cellWidth * 4)
          adjustedX = baseX - fromIntegral hScrollOffset
      
      -- Only draw channel if it's visible
      when (adjustedX + cellWidth * 4 > 0 && adjustedX < windowWidth) $ do
        -- Draw note
        let noteX = adjustedX + cellWidth
        when (noteX + cellWidth > 0 && noteX < windowWidth) $ do
          let noteStr = case channelNote chanData of
                          Just nd -> noteInput nd
                          Nothing -> ""
          drawText ui noteStr (noteX, y + 3) (getColor "note")
        
        -- Draw instrument
        let instX = adjustedX + cellWidth * 2
        when (instX + cellWidth > 0 && instX < windowWidth) $ do
          drawText ui (channelInstrument chanData) (instX, y + 3) (getColor "instrument")
        
        -- Draw volume
        let volX = adjustedX + cellWidth * 3
        when (volX + cellWidth > 0 && volX < windowWidth) $ do
          let volStr = show (round (channelVolume chanData * 100) :: Int)
          drawText ui volStr (volX, y + 3) (getColor "volume")
        
        -- Draw effect
        let fxX = adjustedX + cellWidth * 4
        when (fxX + cellWidth > 0 && fxX < windowWidth) $ do
          let fxStr = case effectCommand (channelEffect chanData) of
                        Just cmd -> [cmd] ++ fromMaybe "" (effectValue (channelEffect chanData))
                        Nothing -> ""
          drawText ui fxStr (fxX, y + 3) (getColor "effect")

-- | Draw all tracker rows
drawTrackerRows :: TrackerUI -> IO ()
drawTrackerRows ui = do
  -- Get current tracker data and state
  tf <- readIORef (uiTrackerFile ui)
  vScrollOffset <- readIORef (uiScrollOffset ui)
  hScrollOffset <- readIORef (uiHScrollOffset ui)
  currentRow <- readIORef (uiCurrentRow ui)
  
  -- Draw each visible row
  forM_ [0..(numRows tf - 1)] $ \rowIdx -> do
    -- Get row data or create empty if needed
    let rowData = if rowIdx < length (trackerData tf)
                  then trackerData tf !! rowIdx
                  else emptyTrackerRow (numChannels tf)
                  
    -- Ensure we have enough channels in the row
    let ensuredRowData = if length (rowChannels rowData) < numChannels tf
                        then rowData { rowChannels = rowChannels rowData ++ 
                                      replicate (numChannels tf - length (rowChannels rowData)) emptyChannelData }
                        else rowData
    
    drawTrackerRow ui ensuredRowData rowIdx vScrollOffset hScrollOffset (rowIdx == currentRow)

-- | Draw the current selection highlight
drawSelection :: TrackerUI -> IO ()
drawSelection ui = do
  -- Get current state
  (row, col, cellType) <- readIORef (uiSelectedCell ui)
  vScrollOffset <- readIORef (uiScrollOffset ui)
  hScrollOffset <- readIORef (uiHScrollOffset ui)
  isEditing <- readIORef (uiIsEditing ui)
  (windowWidth, windowHeight) <- readIORef (uiWindowSize ui)
  
  -- Calculate position of selected cell
  let y = fromIntegral ((row - vScrollOffset) * fromIntegral cellHeight + fromIntegral cellHeight)
      baseX = if cellType == 0 
              then 80  -- Tempo column
              else 170 + (fromIntegral col * cellWidth * 4) + (fromIntegral cellType * cellWidth)
      x = baseX - fromIntegral hScrollOffset
  
  -- Only draw if the selection is visible on screen
  when (y >= cellHeight && y < windowHeight - cellHeight && 
        x >= 0 && x < windowWidth) $ do
    -- Draw highlight rectangle with semi-transparency
    let highlightColor = if isEditing 
                         then V4 50 50 100 180  -- More transparent while editing
                         else V4 100 100 150 100
    
    drawRect ui (SDL.Rectangle (P (SDL.V2 x y)) (SDL.V2 cellWidth cellHeight)) highlightColor
    
    -- Now redraw the cell text over the highlight to ensure visibility
    -- Get cell content
    value <- getCellValue ui row col cellType
    
    -- Choose appropriate color based on cell type
    let textColor = case cellType of
                      0 -> getColor "tempo"
                      1 -> getColor "note"
                      2 -> getColor "instrument"
                      3 -> getColor "volume"
                      4 -> getColor "effect"
                      _ -> getColor "text"
    
    -- Draw the text over the highlight
    drawText ui value (x + 5, y + 3) textColor

-- | Draw the status bar
drawStatusBar :: TrackerUI -> IO ()
drawStatusBar ui = do
  tf <- readIORef (uiTrackerFile ui)
  isPlaying <- readIORef (uiIsPlaying ui)
  isEditing <- readIORef (uiIsEditing ui)
  (row, col, cellType) <- readIORef (uiSelectedCell ui)
  filePath <- readIORef (uiFilePath ui)
  vScrollOffset <- readIORef (uiScrollOffset ui)
  hScrollOffset <- readIORef (uiHScrollOffset ui)
  (windowWidth, windowHeight) <- readIORef (uiWindowSize ui)
  
  -- Draw status bar background
  let statusY = windowHeight - cellHeight
  drawRect ui (SDL.Rectangle (P (SDL.V2 0 statusY)) (SDL.V2 windowWidth cellHeight)) (getColor "row_even")
  
  -- Calculate the total available width and number of sections
  let availableWidth = windowWidth - 20  -- Margin of 10px on each side
  
  -- Prepare status items based on window width
  let statusItems = if windowWidth < 400
                   then
                     -- Very small window: just show minimal info
                     [ case filePath of
                         Just path -> takeFileName path
                         Nothing -> "New File" ]
                   else if windowWidth < 650
                   then
                     -- Small window: basic info
                     [ case filePath of
                         Just path -> "File: " ++ takeFileName path
                         Nothing -> "New Tracker File",
                       "BPM: " ++ show (baseTempo tf),
                       if isEditing 
                         then "Editing: R" ++ show row
                         else "/ for help" ]
                   else if windowWidth < 900
                   then
                     -- Medium window: more info
                     [ case filePath of
                         Just path -> "File: " ++ takeFileName path
                         Nothing -> "New Tracker File",
                       "BPM: " ++ show (baseTempo tf) ++ " | Rows/Beat: " ++ show (rowsPerBeat tf),
                       "Row: " ++ show row ++ "/" ++ show (numRows tf - 1),
                       if isPlaying then "Playing" else "Stopped",
                       if isEditing 
                         then "Editing: R" ++ show row ++ " C" ++ show col
                         else "/ for help" ]
                   else
                     -- Large window: full info
                     let totalWidth = 170 + (fromIntegral (numChannels tf) * cellWidth * 4)
                         scrollPercent = if totalWidth <= windowWidth
                                         then "100%"
                                         else show (round (100 * (fromIntegral hScrollOffset / fromIntegral (totalWidth - windowWidth))) :: Int) ++ "%"
                     in [ case filePath of
                            Just path -> "File: " ++ takeFileName path
                            Nothing -> "New Tracker File (Ctrl+S to save)",
                          "BPM: " ++ show (baseTempo tf) ++ " | Rows/Beat: " ++ show (rowsPerBeat tf),
                          "Row: " ++ show row ++ "/" ++ show (numRows tf - 1) ++ 
                          " | Chan: " ++ show (numChannels tf) ++ 
                          " | Scroll: " ++ scrollPercent,
                          if isPlaying then "Playing" else "Stopped",
                          if isEditing 
                            then "Editing: Row " ++ show row ++ ", Col " ++ show col
                            else "/ for help" ]
  
  -- Draw status items with even spacing
  let numItems = fromIntegral (length statusItems)
      sectionWidth = availableWidth `div` numItems
      xCoordinates = [10 + (fromIntegral i * sectionWidth) | i <- [0..(length statusItems - 1)]]
  
  -- Draw each status item
  forM_ (zip statusItems xCoordinates) $ \(text, x) ->
    drawText ui text (x, statusY + 3) (getColor "text")

-- | Check if Control key is pressed
isCtrlPressed :: IO Bool
isCtrlPressed = do
  modState <- SDL.getModState
  return $ SDL.keyModifierLeftCtrl modState || SDL.keyModifierRightCtrl modState

-- | Draw edit box when editing a cell
drawEditBox :: TrackerUI -> IO ()
drawEditBox ui = do
  isEditing <- readIORef (uiIsEditing ui)
  
  when isEditing $ do
    (row, col, cellType) <- readIORef (uiSelectedCell ui)
    vScrollOffset <- readIORef (uiScrollOffset ui)
    hScrollOffset <- readIORef (uiHScrollOffset ui)
    editText <- readIORef (uiEditText ui)
    (windowWidth, windowHeight) <- readIORef (uiWindowSize ui)
    
    -- Calculate position
    let y = fromIntegral ((row - vScrollOffset) * fromIntegral cellHeight + fromIntegral cellHeight)
        baseX = if cellType == 0 
                then 80  -- Tempo column
                else 170 + (fromIntegral col * cellWidth * 4) + (fromIntegral cellType * cellWidth)
        x = baseX - fromIntegral hScrollOffset
    
    -- Only draw if the edit box is visible on screen
    when (y >= cellHeight && y < windowHeight - cellHeight && 
          x >= 0 && x < windowWidth) $ do
      -- Draw edit background (semi-transparent)
      drawRect ui (SDL.Rectangle (P (SDL.V2 x y)) (SDL.V2 cellWidth cellHeight)) (V4 50 50 100 220)
      
      -- Draw edit text with cursor
      drawText ui (editText ++ "_") (x + 5, y + 3) (getColor "header")  -- Use bright text color

-- | Draw scrollbars
drawScrollbars :: TrackerUI -> IO ()
drawScrollbars ui = do
  -- Get current state
  tf <- readIORef (uiTrackerFile ui)
  vScrollOffset <- readIORef (uiScrollOffset ui)
  hScrollOffset <- readIORef (uiHScrollOffset ui)
  (windowWidth, windowHeight) <- readIORef (uiWindowSize ui)
  
  -- Calculate total content width
  let totalWidth = 170 + (fromIntegral (numChannels tf) * cellWidth * 4)
      totalHeight = fromIntegral (numRows tf) * cellHeight + cellHeight
      visibleHeight = windowHeight - cellHeight * 2 -- Account for header and status bar
      visibleWidth = windowWidth
  
  -- Only draw horizontal scrollbar if content width exceeds window width
  when (totalWidth > visibleWidth) $ do
    -- Calculate scrollbar metrics
    let scrollbarHeight = 12 -- Height of scrollbar
        statusBarY = windowHeight - cellHeight
        scrollbarY = statusBarY - scrollbarHeight
        scrollbarWidth = windowWidth
        thumbWidth = max 40 (fromIntegral (round $ (fromIntegral visibleWidth / fromIntegral totalWidth) * fromIntegral scrollbarWidth))
        thumbX = fromIntegral (round $ (fromIntegral hScrollOffset / fromIntegral (totalWidth - visibleWidth)) * fromIntegral (scrollbarWidth - thumbWidth))
    
    -- Draw scrollbar background
    drawRect ui (SDL.Rectangle (P (SDL.V2 0 scrollbarY)) (SDL.V2 scrollbarWidth scrollbarHeight)) (V4 30 30 30 255)
    
    -- Draw scrollbar thumb
    drawRect ui (SDL.Rectangle (P (SDL.V2 thumbX scrollbarY)) (SDL.V2 thumbWidth scrollbarHeight)) (V4 100 100 100 255)
  
  -- Only draw vertical scrollbar if content height exceeds window height
  when (totalHeight > visibleHeight) $ do
    -- Calculate scrollbar metrics
    let scrollbarWidth = 12 -- Width of scrollbar
        scrollbarX = windowWidth - scrollbarWidth
        scrollbarHeight = visibleHeight
        thumbHeight = max 40 (fromIntegral (round $ (fromIntegral visibleHeight / fromIntegral totalHeight) * fromIntegral scrollbarHeight))
        thumbY = cellHeight + fromIntegral (round $ (fromIntegral vScrollOffset * fromIntegral cellHeight / fromIntegral (totalHeight - visibleHeight)) * fromIntegral (scrollbarHeight - thumbHeight))
    
    -- Draw scrollbar background
    drawRect ui (SDL.Rectangle (P (SDL.V2 scrollbarX cellHeight)) (SDL.V2 scrollbarWidth scrollbarHeight)) (V4 30 30 30 255)
    
    -- Draw scrollbar thumb
    drawRect ui (SDL.Rectangle (P (SDL.V2 scrollbarX thumbY)) (SDL.V2 scrollbarWidth thumbHeight)) (V4 100 100 100 255)

-- | Draw the entire tracker UI
drawUI :: TrackerUI -> IO ()
drawUI ui = do
    -- Clear screen with background color
    SDL.rendererDrawColor (uiRenderer ui) $= (getColor "background")
    SDL.clear (uiRenderer ui)
    
    -- Draw components
    drawHeader ui
    drawTrackerRows ui
    drawScrollbars ui  -- Draw scrollbars
    drawSelection ui
    drawStatusBar ui
    drawEditBox ui
    
    -- Draw help overlay if active
    showHelp <- readIORef (uiShowHelp ui)
    when showHelp $ drawHelpOverlay ui
    
    -- Draw file save dialog if active
    showSaveDialog <- readIORef (uiShowFileSaveDialog ui)
    when showSaveDialog $ drawSaveDialog ui
    
    -- Draw file open dialog if active
    showOpenDialog <- readIORef (uiShowFileOpenDialog ui)
    when showOpenDialog $ drawFileOpenDialog ui
    
    -- Draw render dialog if active
    showRenderDialog <- readIORef (uiShowRenderDialog ui)
    when showRenderDialog $ drawRenderDialog ui
    
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
      -- First ensure we have enough rows in the tracker data
      let ensuredTf = if null (trackerData tf) 
                      then tf { trackerData = replicate (numRows tf) (emptyTrackerRow (numChannels tf)) }
                      else if length (trackerData tf) < numRows tf
                           then tf { trackerData = trackerData tf ++ replicate (numRows tf - length (trackerData tf)) (emptyTrackerRow (numChannels tf)) }
                           else tf
          
          rowsData = trackerData ensuredTf
          
          updateRow r = 
            if r == row && row < length rowsData
            then let currentRow = rowsData !! r
                     -- Make sure we have enough channels in this row
                     ensuredRow = if length (rowChannels currentRow) < numChannels ensuredTf
                                 then currentRow { rowChannels = rowChannels currentRow ++ 
                                                  replicate (numChannels ensuredTf - length (rowChannels currentRow)) emptyChannelData }
                                 else currentRow
                                 
                     updatedRow = case cellType of
                       0 -> -- Tempo
                         ensuredRow { rowTempo = if null editText 
                                                 then Nothing 
                                                 else Just $ TempoData editText (parseTempo editText (baseTempo ensuredTf)) }
                       _ -> -- Channel data
                         if col < length (rowChannels ensuredRow)
                         then let channels = rowChannels ensuredRow
                                  currentChan = channels !! col
                                  updatedChan = case cellType of
                                    1 -> -- Note
                                      currentChan { channelNote = if null editText
                                                                  then Nothing
                                                                  else case parseNoteInput editText (baseFrequency ensuredTf) of
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
                              in ensuredRow { rowChannels = take col channels ++ [updatedChan] ++ drop (col + 1) channels }
                         else ensuredRow
                 in updatedRow
            else rowsData !! r
            
      in ensuredTf { trackerData = map updateRow [0..(length rowsData - 1)] }
    
    -- Debug output to confirm edit was applied
    putStrLn $ "Edit applied: Row " ++ show row ++ ", Col " ++ show col ++ 
               ", Type " ++ show cellType ++ ", Text: " ++ editText
               
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
  
  -- First, try to move the cell type
  let maxCellType = 4  -- 0=tempo, 1=note, 2=inst, 3=vol, 4=fx
      newCellType = cellType + typeDelta
  
  -- Handle special case: when moving right from FX (type 4) to next channel
  if newCellType > maxCellType && typeDelta > 0
    then do
      -- Move to next channel, first cell type
      let newCol = col + 1
          numCols = numChannels tf
      if newCol < numCols
        then do
          -- We can move to the next channel
          let newRow = row
          writeIORef (uiSelectedCell ui) (newRow, newCol, 1)  -- 1 = note column
        else do
          -- We're at the last channel, go to next row first column
          let newRow = min (numRows tf - 1) (row + 1)
          writeIORef (uiSelectedCell ui) (newRow, 0, 1)  -- First channel, note column
    
    -- Handle special case: when moving left from first column (note, type 1) to previous channel
    else if newCellType < 0 && typeDelta < 0
      then do
        -- Move to previous channel, last cell type
        let newCol = col - 1
        if newCol >= 0
          then do
            -- We can move to the previous channel
            let newRow = row
            writeIORef (uiSelectedCell ui) (newRow, newCol, 4)  -- 4 = fx column
          else if col == 0 && cellType == 1  -- If we're at the leftmost channel
            then do
              -- Go to the tempo column
              writeIORef (uiSelectedCell ui) (row, 0, 0)  -- Tempo column
            else if row > 0  -- If we can go to previous row
              then do
                -- Go to previous row, last channel, last cell type
                let newRow = row - 1
                writeIORef (uiSelectedCell ui) (newRow, numChannels tf - 1, 4)  -- Last channel, fx column
              else do
                -- We're at the first row, first channel - stay put
                writeIORef (uiSelectedCell ui) (row, col, max 0 newCellType)
      
      -- Normal case: move within cell types or channels
      else do
        -- Calculate new position
        let newRow = max 0 (min (numRows tf - 1) (row + rowDelta))
            numCols = numChannels tf
            newCol = max 0 (min (numCols - 1) (col + colDelta))
            clampedCellType = max 0 (min maxCellType newCellType)
        
        -- Update selection
        writeIORef (uiSelectedCell ui) (newRow, newCol, clampedCellType)
  
  -- Get the updated position (after all the logic above)
  (newRow, _, _) <- readIORef (uiSelectedCell ui)
  
  -- Adjust vertical scroll if needed
  scrollOffset <- readIORef (uiScrollOffset ui)
  (_, windowHeight) <- readIORef (uiWindowSize ui)
  let visibleRows = (windowHeight - cellHeight * 2) `div` cellHeight
      
  when (newRow < scrollOffset) $
    writeIORef (uiScrollOffset ui) newRow
    
  when (newRow >= scrollOffset + fromIntegral visibleRows) $
    writeIORef (uiScrollOffset ui) (newRow - fromIntegral visibleRows + 1)
    
  -- Adjust horizontal scroll if needed
  (newRow, newCol, newCellType) <- readIORef (uiSelectedCell ui)
  hScrollOffset <- readIORef (uiHScrollOffset ui)
  (windowWidth, _) <- readIORef (uiWindowSize ui)
  
  let baseX = if newCellType == 0
              then 80  -- Tempo column
              else 170 + (fromIntegral newCol * cellWidth * 4) + (fromIntegral newCellType * cellWidth)
      adjustedX = baseX - fromIntegral hScrollOffset
      
  -- If cell is too far right, scroll right
  when (adjustedX + cellWidth > windowWidth) $
    writeIORef (uiHScrollOffset ui) (fromIntegral (baseX + cellWidth - windowWidth + 20))
    
  -- If cell is too far left, scroll left
  when (adjustedX < 0) $
    writeIORef (uiHScrollOffset ui) (max 0 (fromIntegral baseX - 20))

-- | Draw help overlay
drawHelpOverlay :: TrackerUI -> IO ()
drawHelpOverlay ui = do
  -- Draw semi-transparent background
  let overlayColor = V4 0 0 0 200
  drawRect ui (SDL.Rectangle (P (SDL.V2 50 50)) (SDL.V2 (screenWidth - 100) (screenHeight - 100))) overlayColor
  
  -- Draw help title
  drawText ui "Keyboard Shortcuts" (100, 70) (getColor "header")
  
  -- Draw help content
  let shortcuts = [
        ("Arrow Keys", "Navigate through cells (wraps between channels)"),
        ("Ctrl+Left/Right", "Scroll horizontally"),
        ("Tab", "Move to next column/channel"),
        ("Shift+Tab", "Move to previous column/channel"),
        ("Page Up/Down", "Scroll vertically by 10 rows"),
        ("Home/End", "Scroll to start/end horizontally"),
        ("Enter", "Edit selected cell / Save edit and move down"),
        ("Escape", "Cancel editing"),
        ("Space", "Play/Pause"),
        ("Ctrl+S", "Save tracker file"),
        ("Ctrl+O", "Open tracker file"),
        ("Ctrl+R", "Render to WAV file"),
        ("/", "Toggle help dialog"),
        ("Q", "Quit")
        ]
  
  forM_ (zip [0..] shortcuts) $ \(idx, (key, desc)) -> do
    let y = 100 + idx * 30
    drawText ui key (100, y) (getColor "note")
    drawText ui desc (300, y) (getColor "text")
  
  -- Draw exit message
  drawText ui "Press / again to close help" (100, screenHeight - 100) (getColor "header")

-- | Draw file save dialog
drawSaveDialog :: TrackerUI -> IO ()
drawSaveDialog ui = do
  -- Draw dialog background
  let dialogColor = V4 0 0 0 220
  drawRect ui (SDL.Rectangle (P (SDL.V2 100 200)) (SDL.V2 (screenWidth - 200) 200)) dialogColor
  
  -- Draw dialog title
  drawText ui "Save File" (150, 220) (getColor "header")
  
  -- Draw filename input prompt
  drawText ui "Filename:" (150, 260) (getColor "text")
  
  -- Draw text input box
  let inputBoxColor = V4 50 50 50 255
  drawRect ui (SDL.Rectangle (P (SDL.V2 250 255)) (SDL.V2 400 30)) inputBoxColor
  
  -- Draw current filename
  filename <- readIORef (uiSaveFilename ui)
  drawText ui (filename ++ "_") (260, 260) (getColor "header")
  
  -- Draw instructions
  drawText ui "Press Enter to save, Escape to cancel" (150, 320) (getColor "text")

-- | Draw file open dialog
drawFileOpenDialog :: TrackerUI -> IO ()
drawFileOpenDialog ui = do
  -- Draw dialog background
  let dialogColor = V4 0 0 0 220
  drawRect ui (SDL.Rectangle (P (SDL.V2 100 200)) (SDL.V2 (screenWidth - 200) 200)) dialogColor
  
  -- Draw dialog title
  drawText ui "Open File" (150, 220) (getColor "header")
  
  -- Draw filename input prompt
  drawText ui "Filename:" (150, 260) (getColor "text")
  
  -- Draw text input box
  let inputBoxColor = V4 50 50 50 255
  drawRect ui (SDL.Rectangle (P (SDL.V2 250 255)) (SDL.V2 400 30)) inputBoxColor
  
  -- Draw current filename
  filename <- readIORef (uiOpenFilename ui)
  drawText ui (filename ++ "_") (260, 260) (getColor "header")
  
  -- Draw instructions
  drawText ui "Press Enter to open, Escape to cancel" (150, 320) (getColor "text")

-- | Draw render to WAV dialog
drawRenderDialog :: TrackerUI -> IO ()
drawRenderDialog ui = do
  -- Draw dialog background
  let dialogColor = V4 0 0 0 220
  drawRect ui (SDL.Rectangle (P (SDL.V2 100 200)) (SDL.V2 (screenWidth - 200) 200)) dialogColor
  
  -- Draw dialog title
  drawText ui "Render to WAV" (150, 220) (getColor "header")
  
  -- Draw filename input prompt
  drawText ui "Output WAV file:" (150, 260) (getColor "text")
  
  -- Draw text input box
  let inputBoxColor = V4 50 50 50 255
  drawRect ui (SDL.Rectangle (P (SDL.V2 250 255)) (SDL.V2 400 30)) inputBoxColor
  
  -- Draw current filename
  filename <- readIORef (uiRenderFilename ui)
  drawText ui (filename ++ "_") (260, 260) (getColor "header")
  
  -- Draw instructions
  drawText ui "Press Enter to render, Escape to cancel" (150, 320) (getColor "text")

-- | Handle key presses in the save dialog
handleSaveDialogKeys :: TrackerUI -> SDL.Keycode -> IO Bool
handleSaveDialogKeys ui keycode = do
  case keycode of
    SDL.KeycodeEscape -> do
      -- Close dialog without saving
      writeIORef (uiShowFileSaveDialog ui) False
      return False
      
    SDL.KeycodeReturn -> do
      -- Get filename and save
      filename <- readIORef (uiSaveFilename ui)
      when (not (null filename)) $ do
        -- Update file path
        writeIORef (uiFilePath ui) (Just filename)
        -- Save file
        tf <- readIORef (uiTrackerFile ui)
        
        -- Ensure tracker file has proper row data before saving
        let ensuredTf = if null (trackerData tf) 
                        then tf { trackerData = replicate (numRows tf) (emptyTrackerRow (numChannels tf)) }
                        else if length (trackerData tf) < numRows tf
                             then tf { trackerData = trackerData tf ++ 
                                      replicate (numRows tf - length (trackerData tf)) (emptyTrackerRow (numChannels tf)) }
                             else tf
                             
        -- Make sure all rows have proper channel data
        let fullyEnsuredTf = ensuredTf { 
              trackerData = map ensureRowHasChannels (trackerData ensuredTf) 
            }
            
            ensureRowHasChannels row =
              if length (rowChannels row) < numChannels tf
              then row { rowChannels = rowChannels row ++ 
                          replicate (numChannels tf - length (rowChannels row)) emptyChannelData }
              else row
        
        writeTrackerFile filename fullyEnsuredTf
        putStrLn $ "File saved to: " ++ filename
      
      -- Close dialog
      writeIORef (uiShowFileSaveDialog ui) False
      return False
      
    SDL.KeycodeBackspace -> do
      -- Delete last character
      modifyIORef (uiSaveFilename ui) $ \name ->
        if not (null name) then init name else ""
      return False
      
    _ -> do
      -- Handle printable characters
      handleTextInput keycode (uiSaveFilename ui)

-- | Handle key presses in the open dialog
handleOpenDialogKeys :: TrackerUI -> SDL.Keycode -> IO Bool
handleOpenDialogKeys ui keycode = do
  case keycode of
    SDL.KeycodeEscape -> do
      -- Close dialog without opening
      writeIORef (uiShowFileOpenDialog ui) False
      return False
      
    SDL.KeycodeReturn -> do
      -- Get filename and open
      filename <- readIORef (uiOpenFilename ui)
      when (not (null filename)) $ do
        -- Load file
        result <- readTrackerFile filename
        case result of
          Right tf -> do
            -- Store new tracker file
            writeIORef (uiTrackerFile ui) tf
            -- Update file path
            writeIORef (uiFilePath ui) (Just filename)
            -- Reset UI state
            writeIORef (uiCurrentRow ui) 0
            writeIORef (uiScrollOffset ui) 0
            writeIORef (uiSelectedCell ui) (0, 0, 0)
            putStrLn $ "File loaded: " ++ filename
          Left err -> do
            putStrLn $ "Error loading file: " ++ err
      
      -- Close dialog
      writeIORef (uiShowFileOpenDialog ui) False
      return False
      
    SDL.KeycodeBackspace -> do
      -- Delete last character
      modifyIORef (uiOpenFilename ui) $ \name ->
        if not (null name) then init name else ""
      return False
      
    _ -> do
      -- Handle printable characters
      handleTextInput keycode (uiOpenFilename ui)

-- | Handle key presses in the render dialog
handleRenderDialogKeys :: TrackerUI -> SDL.Keycode -> IO Bool
handleRenderDialogKeys ui keycode = do
  case keycode of
    SDL.KeycodeEscape -> do
      -- Close dialog without rendering
      writeIORef (uiShowRenderDialog ui) False
      return False
      
    SDL.KeycodeReturn -> do
      -- Get filename and render
      filename <- readIORef (uiRenderFilename ui)
      when (not (null filename)) $ do
        -- Ensure the file has .wav extension
        let wavFilename = if takeExtension filename == ".wav"
                         then filename
                         else replaceExtension filename ".wav"
        
        -- Update the shown filename in the dialog
        writeIORef (uiRenderFilename ui) wavFilename
        
        -- Get tracker file
        tf <- readIORef (uiTrackerFile ui)
        
        -- Convert tracker to music using the TrackerToMusic module
        let music = trackerToMusic tf
            
        -- Render to WAV file
        putStrLn $ "Rendering to WAV file: " ++ wavFilename
        catch 
          (do
            writeJustWav wavFilename 10.0 music  -- 10 seconds max duration
            putStrLn "Rendering complete!"
          )
          (\(e :: SomeException) -> do
            putStrLn $ "Error rendering to WAV: " ++ show e
          )
      
      -- Close dialog
      writeIORef (uiShowRenderDialog ui) False
      return False
      
    SDL.KeycodeBackspace -> do
      -- Delete last character
      modifyIORef (uiRenderFilename ui) $ \name ->
        if not (null name) then init name else ""
      return False
      
    _ -> do
      -- Handle printable characters
      handleTextInput keycode (uiRenderFilename ui)

-- | Common text input handler for dialogs
handleTextInput :: SDL.Keycode -> IORef String -> IO Bool
handleTextInput keycode ioRef = do
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
        , (SDL.KeycodeUnderscore, '_'), (SDL.KeycodeQuote, '\''), (SDL.KeycodeColon, ':')
        ]
  case Map.lookup keycode charMap of
    Just char -> do
      modifyIORef ioRef (++ [char])
      return False
    Nothing -> return False

-- | Show platform-specific native file dialog (if available)
showNativeFileDialog :: Bool -> String -> String -> IO (Maybe FilePath)
showNativeFileDialog isSave defaultName dialogTitle = do
  -- Get OS platform
  platform <- getPlatform
  
  -- Determine if this is a WAV dialog based on dialog title or defaultName extension
  let isWavDialog = ".wav" `isInfixOf` dialogTitle || takeExtension defaultName == ".wav"
  
  case platform of
    "linux" -> showLinuxFileDialog isSave defaultName dialogTitle isWavDialog
    "windows" -> showWindowsFileDialog isSave defaultName dialogTitle isWavDialog
    _ -> return Nothing  -- Fallback for unsupported platforms

-- | Get current platform
getPlatform :: IO String
getPlatform = do
  let checkWindows :: IO String
      checkWindows = catch
        (do 
          _ <- System.Process.readProcess "cmd" ["/c", "echo", "windows"] ""
          return "windows"
        )
        (\(_ :: SomeException) -> return "")
        
      checkLinux :: IO String
      checkLinux = catch
        (do
          _ <- System.Process.readProcess "uname" [] ""
          return "linux"
        )
        (\(_ :: SomeException) -> return "")

  windows <- checkWindows
  if not (null windows)
    then return windows
    else do
      linux <- checkLinux
      if not (null linux)
        then return "linux"
        else return "unknown"

-- | Show file dialog on Linux using zenity
showLinuxFileDialog :: Bool -> String -> String -> Bool -> IO (Maybe FilePath)
showLinuxFileDialog isSave defaultName dialogTitle isWavDialog = do
  -- Check if zenity is available
  hasZenity <- catch
    (do
      _ <- System.Process.readProcess "zenity" ["--version"] ""
      return True
    )
    (\(_ :: SomeException) -> return False)
  
  if not hasZenity
    then return Nothing
    else do
      result <- catch
        (do
          -- Get the current directory and current window info
          currentDir <- getCurrentDirectory
          let defaultPath = currentDir </> defaultName
              
              -- Determine appropriate filter based on dialog type
              fileFilter = if isWavDialog
                          then "*.wav"  -- WAV files for render dialog
                          else "*.json" -- JSON files for open/save dialog
              
              -- Build command based on dialog type (removed deprecated --window-icon)
              args = ["--file-selection", 
                      "--title=" ++ dialogTitle,
                      "--filename=" ++ defaultPath,
                      "--file-filter=" ++ fileFilter] ++
                     (if isSave 
                      then ["--save"] -- Removed --confirm-overwrite which is deprecated
                      else [])
          
          -- Run zenity file dialog - use simpler method to avoid display issues
          path <- System.Process.readProcess "zenity" args ""
          
          -- Trim any trailing newlines
          let cleanPath = filter (/= '\n') path
          
          -- If the path is empty, user cancelled
          if null cleanPath 
            then return Nothing
            else return (Just cleanPath)
        )
        (\(e :: SomeException) -> do
          putStrLn $ "Error showing Linux file dialog: " ++ show e
          return Nothing
        )
      return result

-- | Show file dialog on Windows using PowerShell
showWindowsFileDialog :: Bool -> String -> String -> Bool -> IO (Maybe FilePath)
showWindowsFileDialog isSave defaultName dialogTitle isWavDialog = do
  result <- catch
    (do
      -- Get the current directory
      currentDir <- getCurrentDirectory
      let defaultPath = currentDir </> defaultName
          
          -- Determine appropriate filter based on dialog type
          fileFilter = if isWavDialog
                       then "WAV Files (*.wav)|*.wav|All files (*.*)|*.*"
                       else "JSON Files (*.json)|*.json|All files (*.*)|*.*"
      
      -- Create PowerShell script for file dialog
      let psScript = if isSave
            then "[System.Reflection.Assembly]::LoadWithPartialName(\"System.windows.forms\") | Out-Null; " ++
                 "$SaveFileDialog = New-Object System.Windows.Forms.SaveFileDialog; " ++
                 "$SaveFileDialog.initialDirectory = '" ++ currentDir ++ "'; " ++
                 "$SaveFileDialog.filter = '" ++ fileFilter ++ "'; " ++
                 "$SaveFileDialog.Title = '" ++ dialogTitle ++ "'; " ++
                 "$SaveFileDialog.FileName = '" ++ defaultName ++ "'; " ++
                 "$SaveFileDialog.AddExtension = $true; " ++  -- Automatically add the extension
                 "$SaveFileDialog.ShowDialog() | Out-Null; " ++
                 "if ($SaveFileDialog.filename) { Write-Host $SaveFileDialog.filename }"
            else "[System.Reflection.Assembly]::LoadWithPartialName(\"System.windows.forms\") | Out-Null; " ++
                 "$OpenFileDialog = New-Object System.Windows.Forms.OpenFileDialog; " ++
                 "$OpenFileDialog.initialDirectory = '" ++ currentDir ++ "'; " ++
                 "$OpenFileDialog.filter = '" ++ fileFilter ++ "'; " ++
                 "$OpenFileDialog.Title = '" ++ dialogTitle ++ "'; " ++
                 "$OpenFileDialog.ShowDialog() | Out-Null; " ++
                 "if ($OpenFileDialog.filename) { Write-Host $OpenFileDialog.filename }"
      
      -- Execute PowerShell script
      path <- System.Process.readProcess "powershell" ["-Command", psScript] ""
      
      -- Trim any trailing newlines
      let cleanPath = filter (/= '\n') path
      
      -- If the path is empty, user cancelled
      if null cleanPath 
        then return Nothing
        else return (Just cleanPath)
    )
    (\(e :: SomeException) -> do
      putStrLn $ "Error showing Windows file dialog: " ++ show e
      return Nothing
    )
  return result

-- | Save tracker file
saveTrackerFile :: TrackerUI -> IO ()
saveTrackerFile ui = do
  tf <- readIORef (uiTrackerFile ui)
  filePath <- readIORef (uiFilePath ui)
  
  case filePath of
    Just path -> do
      writeTrackerFile path tf
      putStrLn $ "File saved to: " ++ path
    Nothing -> do
      -- Try to use native file dialog
      currentName <- readIORef (uiSaveFilename ui)
      result <- showNativeFileDialog True currentName "Save Tracker File"
      case result of
        Just path -> do
          -- Update file path and save
          writeIORef (uiFilePath ui) (Just path)
          writeIORef (uiSaveFilename ui) path
          
          -- Ensure tracker file has proper row data before saving
          let ensuredTf = if null (trackerData tf) 
                          then tf { trackerData = replicate (numRows tf) (emptyTrackerRow (numChannels tf)) }
                          else if length (trackerData tf) < numRows tf
                               then tf { trackerData = trackerData tf ++ 
                                        replicate (numRows tf - length (trackerData tf)) (emptyTrackerRow (numChannels tf)) }
                               else tf
                               
          -- Make sure all rows have proper channel data
          let fullyEnsuredTf = ensuredTf { 
                trackerData = map ensureRowHasChannels (trackerData ensuredTf) 
              }
              
              ensureRowHasChannels row =
                if length (rowChannels row) < numChannels tf
                then row { rowChannels = rowChannels row ++ 
                            replicate (numChannels tf - length (rowChannels row)) emptyChannelData }
                else row
          
          writeTrackerFile path fullyEnsuredTf
          putStrLn $ "File saved to: " ++ path
        Nothing -> do
          -- Fall back to our own dialog if native dialog is not available or cancelled
          writeIORef (uiShowFileSaveDialog ui) True
          putStrLn "No filename set. Please enter a filename in the save dialog."

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
        moveSelection ui 1 0 0  -- Move to next row after editing
        return False
      SDL.KeycodeTab -> do
        applyCellEdit ui
        moveSelection ui 0 0 1  -- Move to next column after editing
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
    else do
      -- Check if any dialog is active
      showSaveDialog <- readIORef (uiShowFileSaveDialog ui)
      showOpenDialog <- readIORef (uiShowFileOpenDialog ui)
      showRenderDialog <- readIORef (uiShowRenderDialog ui)
      
      if showSaveDialog
        then handleSaveDialogKeys ui keycode
        else if showOpenDialog
          then handleOpenDialogKeys ui keycode
          else if showRenderDialog
            then handleRenderDialogKeys ui keycode
            else case keycode of
          SDL.KeycodeQ -> return True  -- Quit
          SDL.KeycodeUp -> moveSelection ui (-1) 0 0 >> return False
          SDL.KeycodeDown -> moveSelection ui 1 0 0 >> return False
          SDL.KeycodeLeft -> do
            -- Check if Ctrl is pressed for horizontal scroll
            hasCtrl <- isCtrlPressed
            if hasCtrl
              then do
                -- Horizontal scroll left
                hScrollOffset <- readIORef (uiHScrollOffset ui)
                when (hScrollOffset > 0) $
                  writeIORef (uiHScrollOffset ui) (max 0 (hScrollOffset - 50))
              else
                -- Normal movement
                moveSelection ui 0 0 (-1)
            return False
          SDL.KeycodeRight -> do
            -- Check if Ctrl is pressed for horizontal scroll
            hasCtrl <- isCtrlPressed
            if hasCtrl
              then do
                -- Horizontal scroll right
                tf <- readIORef (uiTrackerFile ui)
                (windowWidth, _) <- readIORef (uiWindowSize ui)
                hScrollOffset <- readIORef (uiHScrollOffset ui)
                
                let totalWidth = 170 + (fromIntegral (numChannels tf) * cellWidth * 4)
                    maxScroll = totalWidth - windowWidth
                
                when (totalWidth > windowWidth) $
                  writeIORef (uiHScrollOffset ui) (min (fromIntegral maxScroll) (hScrollOffset + 50))
              else
                -- Normal movement
                moveSelection ui 0 0 1
            return False
          SDL.KeycodePageUp -> do
            -- Scroll up by multiple rows
            vScrollOffset <- readIORef (uiScrollOffset ui)
            when (vScrollOffset > 0) $
              writeIORef (uiScrollOffset ui) (max 0 (vScrollOffset - 10))
            return False
          SDL.KeycodePageDown -> do
            -- Scroll down by multiple rows
            tf <- readIORef (uiTrackerFile ui)
            (_, windowHeight) <- readIORef (uiWindowSize ui)
            vScrollOffset <- readIORef (uiScrollOffset ui)
            
            let visibleRows = (windowHeight - cellHeight * 2) `div` cellHeight
                maxScroll = numRows tf - fromIntegral visibleRows
            
            when (maxScroll > 0) $
              writeIORef (uiScrollOffset ui) (min maxScroll (vScrollOffset + 10))
            return False
          SDL.KeycodeHome -> do
            -- Scroll to the start horizontally
            writeIORef (uiHScrollOffset ui) 0
            return False
          SDL.KeycodeEnd -> do
            -- Scroll to the end horizontally
            tf <- readIORef (uiTrackerFile ui)
            (windowWidth, _) <- readIORef (uiWindowSize ui)
            
            let totalWidth = 170 + (fromIntegral (numChannels tf) * cellWidth * 4)
                maxScroll = totalWidth - windowWidth
            
            when (totalWidth > windowWidth) $
              writeIORef (uiHScrollOffset ui) (fromIntegral maxScroll)
            return False
          SDL.KeycodeTab -> do
            -- Check if shift is pressed
            modState <- SDL.getModState
            let isShift = SDL.keyModifierLeftShift modState || SDL.keyModifierRightShift modState
                
            if isShift
              then do
                -- Shift+Tab: Move backward (to previous channel or up a row)
                (row, col, cellType) <- readIORef (uiSelectedCell ui)
                if col > 0 || (col == 0 && cellType > 0)
                  then moveSelection ui 0 0 (-1)  -- Move left one column
                  else if row > 0
                    then do
                      -- Move to previous row, last column
                      tf <- readIORef (uiTrackerFile ui)
                      let lastCol = numChannels tf - 1
                      writeIORef (uiSelectedCell ui) (row - 1, lastCol, 4)
                    else return ()  -- At the beginning, nowhere to go
              else do
                -- Tab: Move forward (to next channel or down a row)
                (row, col, cellType) <- readIORef (uiSelectedCell ui)
                tf <- readIORef (uiTrackerFile ui)
                let lastCol = numChannels tf - 1
                
                if col < lastCol || (col == lastCol && cellType < 4)
                  then moveSelection ui 0 0 1  -- Move right one column
                  else if row < numRows tf - 1
                    then writeIORef (uiSelectedCell ui) (row + 1, 0, 1)  -- Go to next row, first channel
                    else return ()  -- At the end, nowhere to go
                    
            return False
          SDL.KeycodeReturn -> startCellEditing ui >> return False
          SDL.KeycodeS -> do
            -- Check if ctrl is pressed
            hasCtrl <- isCtrlPressed
            when hasCtrl $ saveTrackerFile ui
            return False
            
          SDL.KeycodeO -> do
            -- Check if ctrl is pressed
            hasCtrl <- isCtrlPressed
            when hasCtrl $ do
              -- Get current filename if any for default
              defaultName <- readIORef (uiOpenFilename ui)
              filePath <- readIORef (uiFilePath ui)
              case filePath of
                Just path -> writeIORef (uiOpenFilename ui) path
                Nothing -> return ()
                
              -- Try to use native file dialog
              result <- showNativeFileDialog False defaultName "Open Tracker File"
              case result of
                Just path -> do
                  -- Load file
                  loadResult <- readTrackerFile path
                  case loadResult of
                    Right tf -> do
                      -- Store new tracker file
                      writeIORef (uiTrackerFile ui) tf
                      -- Update file path
                      writeIORef (uiFilePath ui) (Just path)
                      writeIORef (uiOpenFilename ui) path
                      -- Reset UI state
                      writeIORef (uiCurrentRow ui) 0
                      writeIORef (uiScrollOffset ui) 0
                      writeIORef (uiHScrollOffset ui) 0
                      writeIORef (uiSelectedCell ui) (0, 0, 0)
                      putStrLn $ "File loaded: " ++ path
                    Left err -> do
                      putStrLn $ "Error loading file: " ++ err
                Nothing -> do
                  -- Fall back to our own dialog if native dialog is not available or cancelled
                  writeIORef (uiShowFileOpenDialog ui) True
            return False
            
          SDL.KeycodeR -> do
            -- Check if ctrl is pressed
            hasCtrl <- isCtrlPressed
            when hasCtrl $ do
              -- Prepare default WAV filename
              filePath <- readIORef (uiFilePath ui)
              let defaultWavName = case filePath of
                    Just path -> 
                      let filename = takeFileName path
                          wavName = if length filename > 5 && drop (length filename - 5) filename == ".json"
                                    then take (length filename - 5) filename ++ ".wav"  -- Replace .json with .wav
                                    else filename ++ ".wav"
                      in wavName
                    Nothing -> "output.wav"
              
              writeIORef (uiRenderFilename ui) defaultWavName
              
              -- Try to use native file dialog
              result <- showNativeFileDialog True defaultWavName "Save WAV File"
              case result of
                Just path -> do
                  -- Get tracker file
                  tf <- readIORef (uiTrackerFile ui)
                  
                  -- Convert tracker to music using the TrackerToMusic module
                  let music = trackerToMusic tf
                  
                  -- Ensure the file has .wav extension
                  let wavPath = if takeExtension path == ".wav"
                               then path
                               else replaceExtension path ".wav"
                      
                  -- Render to WAV file
                  putStrLn $ "Rendering to WAV file: " ++ wavPath
                  catch 
                    (do
                      writeJustWav wavPath 10.0 music  -- 10 seconds max duration
                      putStrLn "Rendering complete!"
                    )
                    (\(e :: SomeException) -> do
                      putStrLn $ "Error rendering to WAV: " ++ show e
                    )
                Nothing -> do
                  -- Do nothing when user cancels the native dialog
                  -- (no fallback to internal dialog as that would be confusing)
                  putStrLn "Render cancelled."
            return False
          SDL.KeycodeSpace -> do
            -- Toggle playback with space
            togglePlayback ui
            return False
          SDL.KeycodeSlash -> do
            -- Toggle help overlay with /
            modifyIORef (uiShowHelp ui) not
            return False
          _ -> return False

-- | Get current window size
getWindowSize :: TrackerUI -> IO (CInt, CInt)
getWindowSize ui = do
  size <- SDL.get (SDL.windowSize (uiWindow ui))
  let V2 w h = size
  return (w, h)

-- | Main event loop
eventLoop :: TrackerUI -> IO ()
eventLoop ui = do
  -- Handle events
  events <- SDL.pollEvents
  let quit = any isQuitEvent events
      keyEvents = filter isKeyEvent events
      windowEvents = filter isWindowEvent events
  
  -- Handle window events (especially resize)
  forM_ windowEvents $ \event ->
    case SDL.eventPayload event of
      SDL.WindowResizedEvent windowResizedData -> do
        -- Access size through SDL.windowResizedEventSize which returns a V2
        let V2 newWidth newHeight = SDL.windowResizedEventSize windowResizedData
        putStrLn $ "Window resized to: " ++ show newWidth ++ "x" ++ show newHeight
        
        -- Update the stored window size
        writeIORef (uiWindowSize ui) (fromIntegral newWidth, fromIntegral newHeight)
        
        -- Adjust horizontal scroll based on new window size
        hScrollOffset <- readIORef (uiHScrollOffset ui)
        tf <- readIORef (uiTrackerFile ui)
          
        let totalWidth = 170 + (fromIntegral (numChannels tf) * cellWidth * 4)
        
        if fromIntegral newWidth >= totalWidth 
          then
            -- Reset horizontal scroll if window is now big enough
            when (hScrollOffset > 0) $ writeIORef (uiHScrollOffset ui) 0
          else
            -- Adjust horizontal scroll if it would overflow the content
            let maxScroll = totalWidth - fromIntegral newWidth
            in when (hScrollOffset > fromIntegral maxScroll) $
               writeIORef (uiHScrollOffset ui) (fromIntegral maxScroll)
      _ -> return ()
  
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
        
    isWindowEvent event =
      case SDL.eventPayload event of
        SDL.WindowResizedEvent _ -> True
        SDL.WindowSizeChangedEvent _ -> True
        _ -> False
        
    processKeyEvent ui event = 
      case SDL.eventPayload event of
        SDL.KeyboardEvent keyboardEvent ->
          if SDL.keyboardEventKeyMotion keyboardEvent == SDL.Pressed
          then handleKeyPress ui (SDL.keysymKeycode (SDL.keyboardEventKeysym keyboardEvent))
          else return False
        _ -> return False

-- | Test SDL to determine if it's installed and working correctly
testSDL :: IO Bool
testSDL = do
  putStrLn "Testing SDL installation..."
  
  -- Try to initialize SDL
  result <- catch 
    (do
      SDL.initialize [SDL.InitVideo]
      Font.initialize
      
      putStrLn "SDL initialized successfully."
      
      -- Create a small window
      window <- SDL.createWindow "SDL Test" SDL.defaultWindow
        { SDL.windowInitialSize = SDL.V2 320 240 }
      
      putStrLn "Window created successfully."
      
      -- Create a renderer
      renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
      
      putStrLn "Renderer created successfully."
      
      -- Try to load a font
      fontPath <- tryLoadFont [
        "/usr/share/fonts/truetype/dejavu/DejaVuSansMono.ttf", 
        "/usr/share/fonts/TTF/DejaVuSansMono.ttf",
        "/usr/share/fonts/truetype/freefont/FreeMono.ttf",
        "/usr/share/fonts/liberation/LiberationMono-Regular.ttf"
        ] 14
      
      putStrLn "Font loaded successfully."
      
      -- Clean up
      Font.free fontPath
      SDL.destroyRenderer renderer
      SDL.destroyWindow window
      Font.quit
      SDL.quit
      
      putStrLn "SDL is working correctly!"
      return True
    )
    (\(e :: SomeException) -> do
      putStrLn $ "SDL test failed: " ++ show e
      return False
    )
    
  return result

-- | Start the SDL-based tracker interface
startSDLTracker :: Maybe FilePath -> IO ()
startSDLTracker filePath = do
  putStrLn "Starting SDL-based tracker interface..."
  
  -- Wrap everything in an exception handler
  catch
    (do
      -- Check if SDL is available by trying to initialize
      sdlAvailable <- catch 
        (SDL.initialize [SDL.InitVideo] >> return True)
        (\(e :: SomeException) -> do
            putStrLn "Error: Could not initialize SDL!"
            putStrLn $ "Error details: " ++ show e
            putStrLn "Please make sure SDL2 and SDL2_ttf libraries are installed on your system."
            putStrLn "You can run ./install_sdl_deps.sh to install them automatically."
            putStrLn "Press Enter to return to menu..."
            _ <- getLine
            return False
        )
      
      when sdlAvailable $ do
        -- Clean up from our test initialization
        SDL.quit
        
        -- Now initialize properly with wrapped UI initialization
        ui <- catch 
          (initSDL filePath) 
          (\(e :: SomeException) -> do
              putStrLn $ "Error initializing SDL UI: " ++ show e
              putStrLn "Press Enter to return to menu..."
              _ <- getLine
              error $ "SDL initialization error: " ++ show e
          )
            
        -- Event loop with error handling
        _ <- catch 
          (eventLoop ui)
          (\(e :: SomeException) -> do
              putStrLn $ "Error in SDL event loop: " ++ show e
              putStrLn "Press Enter to return to menu..."
              _ <- getLine
              return ()
          )
            
        -- Clean up with error handling
        _ <- catch 
          (cleanupSDL ui)
          (\(e :: SomeException) -> do
              putStrLn $ "Warning: Error cleaning up SDL resources: " ++ show e
              return ()
          )
        return ()
    )
    (\(e :: SomeException) -> do
        putStrLn $ "SDL Tracker Error: " ++ show e
        putStrLn "Press Enter to return to menu..."
        _ <- getLine
        return ()
    )