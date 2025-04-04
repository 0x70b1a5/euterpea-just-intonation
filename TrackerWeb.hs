{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TrackerWeb (startWebTracker) where

import Control.Monad (void, forM_, forM, unless)
import Control.Concurrent (threadDelay)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Maybe (fromMaybe)
import System.FilePath (takeFileName)
import System.Process (system)
import System.Info (os)
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

-- Import project modules
import TrackerTypes
import TrackerParser
import ErrorHandler

-- | Main entry point for the web-based tracker interface
startWebTracker :: Maybe FilePath -> IO ()
startWebTracker filePath = withErrorHandling $ do
  putStrLn "Starting web-based tracker interface..."
  putStrLn "Opening browser window at http://localhost:8023"
  putStrLn "If browser doesn't open automatically, please navigate to http://localhost:8023"
  putStrLn "If you encounter connection issues, try refreshing the browser page"
  
  -- Start the web server
  putStrLn "Starting server... (please wait)"
  
  -- Write a simple index.html that just loads the Threepenny scripts
  let htmlFile = "<!DOCTYPE html>\n<html>\n<head>\n  <meta charset=\"UTF-8\">\n  <title>Just Intonation Tracker</title>\n</head>\n<body>\n  <div id=\"root\">Loading tracker interface...</div>\n</body>\n</html>"
  writeFile "index.html" htmlFile
  
  -- Define the browser opening function to use later
  let url = "http://127.0.0.1:8023"
  let openBrowser = do
        putStrLn "Opening browser..."
        openCmd <- case os of
          "mingw32" -> return $ "start " ++ url -- Windows
          "darwin"  -> return $ "open " ++ url  -- macOS
          _         -> return $ "xdg-open " ++ url ++ " || sensible-browser " ++ url ++ 
                                " || gnome-open " ++ url ++ " || open " ++ url -- Linux/other
        void $ system openCmd
  
  -- Use a flag to track whether the browser has been opened
  browserOpened <- newIORef False
  
  startGUI defaultConfig { 
      jsPort = Just 8023,
      jsStatic = Just "." 
    } $ \window -> do
    
    -- Load tracker file if provided
    tf <- case filePath of
      Just path -> do
        result <- liftIO $ readTrackerFile path
        case result of
          Right loadedTf -> return loadedTf
          Left err -> do
            liftIO $ putStrLn $ "Error loading file: " ++ err
            return defaultTrackerFile
      Nothing -> return defaultTrackerFile
    
    -- Check if browser has been opened and open it if not
    liftIO $ do
      opened <- readIORef browserOpened
      unless opened $ do
        threadDelay 1000000  -- Wait 1 second for server to start
        openBrowser
        writeIORef browserOpened True
    
    -- Set window title
    void $ return window # set UI.title "Just Intonation Tracker"
    
    -- Get the root element
    root <- UI.getElementById window "root"
    root <- case root of
      Just r -> return r
      Nothing -> do
        r <- UI.div
        body <- UI.getBody window
        void $ element body #+ [element r]
        return r
    
    -- Clear the root element
    void $ element root # set UI.children []
    
    -- Set up the basic page structure
    body <- UI.getBody window
    void $ element body # set UI.style [
      ("font-family", "'Courier New', monospace"),
      ("background-color", "#000080"),
      ("color", "#C0C0C0"),
      ("padding", "20px"),
      ("margin", "0")
      ]
    
    -- Create main container
    container <- UI.div # set UI.style [
      ("display", "flex"),
      ("flex-direction", "column"),
      ("max-width", "100%"),
      ("margin", "0 auto")
      ]
    
    -- Create a title
    header <- UI.h1 # set UI.text "Just Intonation Tracker" # set UI.style [
      ("text-align", "center"),
      ("color", "#FFFFFF"),
      ("margin-top", "0")
      ]
    
    -- Create tracker display area
    trackerArea <- UI.div # set UI.style [
      ("background-color", "#000000"),
      ("border", "2px solid #C0C0C0"),
      ("padding", "10px"),
      ("margin-bottom", "20px"),
      ("overflow", "auto")
      ]
    
    -- Create info area
    infoArea <- UI.div # set UI.style [
      ("background-color", "#000044"),
      ("padding", "10px"),
      ("margin-bottom", "10px")
      ]
    
    -- File info
    let fileText = case filePath of
          Just path -> "File: " ++ takeFileName path
          Nothing -> "New Tracker File"
    
    fileInfo <- UI.p # set UI.text fileText
    
    -- Basic tracker info
    baseFreqInfo <- UI.p # set UI.text ("Base Frequency: " ++ show (baseFrequency tf) ++ " Hz")
    baseTempoInfo <- UI.p # set UI.text ("Base Tempo: " ++ show (baseTempo tf) ++ " BPM") 
    rowsPerBeatInfo <- UI.p # set UI.text ("Rows per Beat: " ++ show (rowsPerBeat tf))
    
    void $ element infoArea #+ [element fileInfo, element baseFreqInfo, element baseTempoInfo, element rowsPerBeatInfo]
    
    -- Create table for the tracker data
    table <- UI.table # set UI.id_ "tracker-table"
      # set UI.style [
      ("width", "100%"),
      ("border-collapse", "collapse"),
      ("border", "1px solid #444")
      ]
    
    -- Create table header row
    headerRow <- UI.tr # set UI.style [("background-color", "#222")]
    
    -- Add header cells
    rowNumHeader <- UI.th # set UI.text "Row" # set UI.style headerStyle
    tempoHeader <- UI.th # set UI.text "Tempo" # set UI.style headerStyle
    
    void $ element headerRow #+ [element rowNumHeader, element tempoHeader]
    
    -- Add channel headers
    forM_ [1..(numChannels tf)] $ \ch -> do
      channelHeader <- UI.th # set UI.text ("Ch" ++ show ch) # set UI.style headerStyle
      noteHeader <- UI.th # set UI.text "Note" # set UI.style headerStyle
      instHeader <- UI.th # set UI.text "Inst" # set UI.style headerStyle
      volHeader <- UI.th # set UI.text "Vol" # set UI.style headerStyle
      fxHeader <- UI.th # set UI.text "Fx" # set UI.style headerStyle
      
      void $ element headerRow #+ [element channelHeader, element noteHeader, element instHeader, element volHeader, element fxHeader]
    
    -- Add header row to table
    void $ element table #+ [element headerRow]
    
    -- Create tbody for the rows
    tbody <- UI.mkElement "tbody" # set UI.id_ "tracker-body"
    
    -- Add rows based on tracker data
    rows <- forM [0..(numRows tf - 1)] $ \rowIdx -> do
      let rowData = if rowIdx < length (trackerData tf)
                     then trackerData tf !! rowIdx
                     else emptyTrackerRow (numChannels tf)
      
      -- Create row
      row <- UI.tr # set UI.id_ ("row-" ++ show rowIdx)
                   # set UI.style (if even rowIdx 
                           then [("background-color", "#111")]
                           else [("background-color", "#000")])
      
      -- Row number cell
      rowNumCell <- UI.td # set UI.text (show rowIdx) # set UI.style [
        ("text-align", "right"),
        ("padding", "3px 5px"),
        ("border", "1px solid #333"),
        ("color", "#888"),
        ("font-weight", "bold")
        ]
      
      -- Tempo cell
      let tempoStr = case rowTempo rowData of
                      Just td -> tempoInput td
                      Nothing -> ""
                      
      tempoCell <- UI.td # set UI.style [("border", "1px solid #333")]
      tempoInput <- UI.input # set UI.type_ "text" 
                             # set UI.value tempoStr 
                             # set UI.style [
                               ("width", "100%"),
                               ("background-color", "transparent"),
                               ("border", "none"),
                               ("color", "#FFA500"),
                               ("font-family", "inherit"),
                               ("text-align", "center")
                               ]
      
      void $ element tempoCell #+ [element tempoInput]
      void $ element row #+ [element rowNumCell, element tempoCell]
      
      -- Channel cells
      forM_ (zip [0..] (rowChannels rowData)) $ \(chanIdx, chanData) -> do
        -- Note cell
        let noteStr = case channelNote chanData of
                      Just nd -> noteInput nd
                      Nothing -> ""
        
        noteCell <- UI.td # set UI.style [("border", "1px solid #333")]
        noteInput <- UI.input # set UI.type_ "text" 
                             # set UI.value noteStr 
                             # set UI.style [
                               ("width", "100%"),
                               ("background-color", "transparent"),
                               ("border", "none"),
                               ("color", "#FFFF00"),
                               ("font-family", "inherit"),
                               ("text-align", "center")
                               ]
        
        void $ element noteCell #+ [element noteInput]
        
        -- Instrument cell
        instCell <- UI.td # set UI.style [("border", "1px solid #333")]
        instInput <- UI.input # set UI.type_ "text" 
                             # set UI.value (channelInstrument chanData) 
                             # set UI.style [
                               ("width", "100%"),
                               ("background-color", "transparent"),
                               ("border", "none"),
                               ("color", "#00FFFF"),
                               ("font-family", "inherit"),
                               ("text-align", "center")
                               ]
        
        void $ element instCell #+ [element instInput]
        
        -- Volume cell
        volCell <- UI.td # set UI.style [("border", "1px solid #333")]
        volInput <- UI.input # set UI.type_ "text" 
                           # set UI.value (show (round (channelVolume chanData * 100) :: Int)) 
                           # set UI.style [
                             ("width", "100%"),
                             ("background-color", "transparent"),
                             ("border", "none"),
                             ("color", "#00FF00"),
                             ("font-family", "inherit"),
                             ("text-align", "center")
                             ]
        
        void $ element volCell #+ [element volInput]
        
        -- Effect cell
        let effectStr = case effectCommand (channelEffect chanData) of
                          Just cmd -> [cmd] ++ fromMaybe "" (effectValue (channelEffect chanData))
                          Nothing -> ""
        
        effectCell <- UI.td # set UI.style [("border", "1px solid #333")]
        effectInput <- UI.input # set UI.type_ "text" 
                               # set UI.value effectStr 
                               # set UI.style [
                                 ("width", "100%"),
                                 ("background-color", "transparent"),
                                 ("border", "none"),
                                 ("color", "#FF00FF"),
                                 ("font-family", "inherit"),
                                 ("text-align", "center")
                                 ]
        
        void $ element effectCell #+ [element effectInput]
        
        -- Channel label
        channelCell <- UI.td # set UI.text ("Ch" ++ show (chanIdx + 1)) # set UI.style [
          ("border", "1px solid #333"),
          ("font-weight", "bold"),
          ("color", "#888"),
          ("background-color", "#222")
          ]
        
        -- Add cells to row
        void $ element row #+ [element channelCell, element noteCell, element instCell, element volCell, element effectCell]
      
      return row
    
    -- Add rows to tbody 
    uiRows <- mapM (\r -> return $ element r) rows
    void $ element tbody #+ uiRows
    
    -- Add tbody to table
    void $ element table #+ [element tbody]
    
    -- Add table to tracker area
    void $ element trackerArea #+ [element table]
    
    -- Create controls area
    controlsArea <- UI.div # set UI.style [
      ("display", "flex"),
      ("justify-content", "center"),
      ("gap", "10px"),
      ("margin-top", "20px")
      ]
    
    -- Create buttons
    playButton <- UI.button # set UI.text "Play" # set UI.style buttonStyle
    stopButton <- UI.button # set UI.text "Stop" # set UI.style buttonStyle
    saveButton <- UI.button # set UI.text "Save" # set UI.style buttonStyle
    exportButton <- UI.button # set UI.text "Export WAV" # set UI.style buttonStyle
    
    -- Add buttons to controls area
    void $ element controlsArea #+ [element playButton, element stopButton, element saveButton, element exportButton]
    
    -- Message area for status messages
    messageArea <- UI.div # set UI.id_ "message-area" # set UI.style [
      ("background-color", "#FFFF00"),
      ("color", "#000000"),
      ("padding", "10px"),
      ("margin-top", "20px"),
      ("text-align", "center"),
      ("font-weight", "bold"),
      ("border", "1px solid #000000"),
      ("display", "none")
      ]
    
    -- Assemble the page
    void $ element container #+ [element header, element infoArea, element trackerArea, element controlsArea, element messageArea]
    void $ element root #+ [element container]
    
    -- Add CSS to head
    void $ UI.runFunction $ UI.ffi "document.head.appendChild(document.createElement('style')).innerHTML = %1" css
    
    -- Function to show message
    let showMessage msg = do
          void $ element messageArea # set UI.text msg
          void $ element messageArea # set UI.style [("display", "block")]
          void $ UI.runFunction $ UI.ffi "setTimeout(function() { $('#message-area').fadeOut(); }, 3000);"
    
    -- Add event handlers for buttons
    on UI.click saveButton $ \_ -> 
      case filePath of
        Just path -> do
          -- Get the current tracker data and save it
          -- (In a real implementation, would need to collect data from inputs)
          liftIO $ writeTrackerFile path tf
          showMessage $ "Saved to file: " ++ takeFileName path
        Nothing ->
          showMessage "No file specified. Start the tracker with a file path to enable saving."
    
    -- Play button handler
    on UI.click playButton $ \_ -> do
      -- Set up the audio context and initialize audio
      void $ UI.runFunction $ UI.ffi "initAudio()"
      
      -- Get the row where to start playback (for now, start at the beginning)
      void $ UI.runFunction $ UI.ffi "startPlayback()"
      showMessage "Playback started"
      
    -- Stop button handler
    on UI.click stopButton $ \_ -> do
      void $ UI.runFunction $ UI.ffi "stopPlayback()"
      showMessage "Playback stopped"
    
    -- Export button handler
    on UI.click exportButton $ \_ -> do
      showMessage "Export to WAV is only available in the terminal version."

-- Style for table headers
headerStyle :: [(String, String)]
headerStyle = [
  ("padding", "5px"),
  ("border", "1px solid #444"),
  ("background-color", "#333"),
  ("color", "#FFF"),
  ("text-align", "center")
  ]

-- Style for buttons  
buttonStyle :: [(String, String)]
buttonStyle = [
  ("padding", "8px 15px"),
  ("background-color", "#C0C0C0"),
  ("color", "#000000"),
  ("border", "2px outset #FFFFFF"),
  ("font-weight", "bold"),
  ("cursor", "pointer"),
  ("font-family", "inherit")
  ]

-- CSS for the page
css :: String
css = "body { font-family: 'Courier New', monospace; background-color: #000080; color: #C0C0C0; padding: 20px; margin: 0; } \
      \table { width: 100%; border-collapse: collapse; } \
      \th, td { border: 1px solid #444; padding: 4px; } \
      \input { width: 100%; background-color: transparent; border: none; color: inherit; font-family: inherit; text-align: center; } \
      \input:focus { background-color: #333; outline: none; } \
      \button:hover { background-color: #DDDDDD; } \
      \button:active { border-style: inset; } \
      \tr.playing-row { background-color: #004400 !important; } \
      \tr.playing-row td { color: #FFFFFF; } \
      \tr.playing-row input { color: inherit; }"