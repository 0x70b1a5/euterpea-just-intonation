{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}

module TrackerMain where

import System.IO
import qualified System.Directory as Dir
import Control.Monad (when, unless, forM_)
import Data.Maybe (isJust, fromJust, fromMaybe)
import Data.Ratio
import Data.List (intercalate)
import Euterpea
import System.Console.ANSI
import System.Exit (exitSuccess, ExitCode(ExitSuccess))
import Control.Exception (catch, SomeException, throwIO)
#if !defined(mingw32_HOST_OS) && !defined(__MINGW32__)
import System.Posix.Signals (installHandler, Handler(Catch), sigINT)
#endif
import System.Process (system)
import System.IO.Temp (withSystemTempFile)
-- Use JustIntonationCore instead of Main
import JustIntonationCore
import TrackerTypes
import TrackerParser
import TrackerToMusic

-- | Display tracker file information
displayTrackerInfo :: TrackerFile -> IO ()
displayTrackerInfo tf = do
  putStrLn "Tracker File Information:"
  putStrLn "========================="
  putStrLn $ "Base Frequency: " ++ show (baseFrequency tf) ++ " Hz"
  putStrLn $ "Base Tempo: " ++ show (baseTempo tf) ++ " BPM"
  putStrLn $ "Rows per Beat: " ++ show (rowsPerBeat tf)
  putStrLn $ "Number of Rows: " ++ show (numRows tf)
  putStrLn $ "Number of Channels: " ++ show (numChannels tf)
  putStrLn $ "Number of Populated Rows: " ++ show (length $ filter hasContent $ trackerData tf)
  putStrLn "========================="
  where
    hasContent row = 
      isJust (rowTempo row) || 
      any (\c -> isJust (channelNote c)) (rowChannels row)

-- | Create a simple example tracker file
createExampleTrackerFile :: FilePath -> IO ()
createExampleTrackerFile filePath = do
  -- Create a basic file structure
  let tf = initTrackerFile 440.0 120.0 4 16 4
  
  -- Add some example content
  let exampleTf = tf { trackerData = createExampleData 16 4 }
  
  -- Write to disk
  writeTrackerFile filePath exampleTf
  putStrLn $ "Created example tracker file: " ++ filePath
  putStrLn "This file contains a simple major scale and chord progression"

-- | Create example data for a tracker file
createExampleData :: Int -> Int -> [TrackerRow]
createExampleData numRows numChannels = 
  let 
    -- Create empty rows
    emptyRows = replicate numRows (emptyTrackerRow numChannels)
    
    -- Define a scale pattern
    scale = [
        (0, "440"),    -- A
        (2, "495"),    -- B (9:8 ratio)
        (4, "550"),    -- C# (5:4 ratio)
        (6, "586.67"), -- D (4:3 ratio)
        (8, "660"),    -- E (3:2 ratio)
        (10, "733.33"),-- F# (5/3 ratio)
        (12, "825"),   -- G# (15/8 ratio)
        (14, "880")    -- A octave (2:1 ratio)
      ]
    
    -- Add notes to the first channel
    withNotes = foldr addNote emptyRows scale
    
    -- Add chord on row 15 (across multiple channels)
    withChord = addChord 15 withNotes
    
    -- Add tempo changes
    withTempo = addTempo withChord
  in
    withTempo
  where
    -- Add a note to a specific row in channel 0
    addNote (rowIdx, freq) rows = 
      take rowIdx rows ++ 
      [updateChannelNote rowIdx 0 freq (rows !! rowIdx)] ++ 
      drop (rowIdx + 1) rows
    
    -- Update the note in a specific channel
    updateChannelNote rowIdx chanIdx freq row =
      let 
        channels = rowChannels row
        updatedChannel = (channels !! chanIdx) {
          channelNote = Just $ NoteData {
            noteInput = freq,
            noteFrequency = Just (read freq :: Double),
            noteRatio = Nothing
          }
        }
        newChannels = take chanIdx channels ++ [updatedChannel] ++ drop (chanIdx + 1) channels
      in
        row { rowChannels = newChannels }
    
    -- Add a chord (multiple channels on same row)
    addChord rowIdx rows =
      let 
        row = rows !! rowIdx
        
        -- Define chord notes (A major chord)
        notes = [
            ("440", 0),   -- A (root)
            ("550", 1),   -- C# (major third)
            ("660", 2)    -- E (perfect fifth)
          ]
        
        -- Update each channel
        newRow = foldr 
                  (\(freq, chan) r -> updateChannelNote rowIdx chan freq r) 
                  row 
                  notes
      in
        take rowIdx rows ++ [newRow] ++ drop (rowIdx + 1) rows
    
    -- Add tempo changes
    addTempo rows =
      let 
        -- Add tempo marking at start
        row0 = (rows !! 0) {
          rowTempo = Just $ TempoData {
            tempoInput = "120",
            tempoValue = Just 120.0
          }
        }
        
        -- Add tempo marking in middle
        row8 = (rows !! 8) {
          rowTempo = Just $ TempoData {
            tempoInput = "140",
            tempoValue = Just 140.0
          }
        }
      in
        [row0] ++ 
        take 7 (drop 1 rows) ++ 
        [row8] ++ 
        drop 9 rows

-- | Type to represent cursor position in the tracker grid
data CursorPos = CursorPos {
  cursorRow :: Int,      -- ^ Current row
  cursorCol :: Int,      -- ^ Current column
  cursorField :: Field   -- ^ Current field type (Tempo, Note, Instrument, etc.)
} deriving (Show, Eq)

-- | Fields in the tracker
data Field = TempoField | NoteField | InstField | VolField | FxField
  deriving (Show, Eq)

-- | Get the column index for a field in a specific channel
getFieldCol :: Field -> Int -> Int
getFieldCol TempoField _ = 1  -- Tempo column is always 1
getFieldCol NoteField chanIdx = 2 + chanIdx * 4  -- First column of each channel block
getFieldCol InstField chanIdx = 3 + chanIdx * 4  -- Second column of each channel block
getFieldCol VolField chanIdx = 4 + chanIdx * 4   -- Third column of each channel block
getFieldCol FxField chanIdx = 5 + chanIdx * 4    -- Fourth column of each channel block

-- | Get the field and channel from a column index
getFieldAndChannel :: Int -> Int -> (Field, Int)
getFieldAndChannel cols col
  | col == 1 = (TempoField, 0)  -- Tempo column
  | col > 1 = 
      let 
        -- Calculate channel index (0-based)
        chanWidth = 4
        relCol = col - 2  -- Adjust for row number and tempo columns
        chanIdx = relCol `div` chanWidth
        fieldPos = relCol `mod` chanWidth
        
        -- Determine field type based on position within channel
        field = case fieldPos of
          0 -> NoteField
          1 -> InstField
          2 -> VolField
          3 -> FxField
          _ -> error "Invalid field position"
      in
        if chanIdx < cols
          then (field, chanIdx)
          else (TempoField, 0)  -- Default if out of range
  | otherwise = (TempoField, 0)  -- Default for invalid columns

-- | State for the terminal-based tracker
data TrackerState = TrackerState {
  trackerFile :: TrackerFile,  -- ^ Current tracker file
  cursorPos :: CursorPos,      -- ^ Current cursor position
  selectionStart :: Maybe CursorPos, -- ^ Start of selection (if any)
  selectionEnd :: Maybe CursorPos,   -- ^ End of selection (if any)
  clipboard :: [ClipboardItem],      -- ^ Clipboard contents
  editBuffer :: String,        -- ^ Current edit buffer
  isEditing :: Bool,           -- ^ Whether currently editing a cell
  statusMessage :: String,     -- ^ Current status message
  capturedFreq :: Double,      -- ^ Captured frequency value
  capturedTempo :: Double,     -- ^ Captured tempo value
  filePath :: Maybe FilePath,  -- ^ Current file path
  sigintHandler :: Maybe Handler, -- ^ Original SIGINT handler to restore
  bottomBarExpanded :: Bool    -- ^ Whether bottom help bar is expanded
}

-- | Item in the clipboard
data ClipboardItem = ClipboardItem {
  cbRow :: Int,        -- ^ Relative row position
  cbCol :: Int,        -- ^ Relative column position
  cbField :: Field,    -- ^ Field type
  cbContent :: String  -- ^ Content
} deriving (Show, Eq)

-- | Initialize the tracker state
initTrackerState :: TrackerFile -> Maybe FilePath -> TrackerState
initTrackerState tf path = TrackerState {
  trackerFile = tf,
  cursorPos = CursorPos 0 1 TempoField,  -- Start at first row, tempo column
  selectionStart = Nothing,
  selectionEnd = Nothing,
  clipboard = [],
  editBuffer = "",
  isEditing = False,
  statusMessage = "Ready. Press ? for help.",
  capturedFreq = baseFrequency tf,
  capturedTempo = baseTempo tf,
  filePath = path,
  sigintHandler = Nothing,
  bottomBarExpanded = False  -- Start with collapsed bottom bar
}

-- | Draw the tracker grid in the terminal
drawTrackerGrid :: TrackerState -> IO ()
drawTrackerGrid state = do
  -- Clear screen and move cursor to top-left
  clearScreen
  setCursorPosition 0 0
  
  -- Get tracker file and dimensions
  let tf = trackerFile state
      rows = numRows tf
      cols = numChannels tf
      curPos = cursorPos state
  
  -- Draw header and status bar
  setSGR [SetColor Foreground Vivid White, SetColor Background Vivid Blue]
  putStrLn $ " Just Intonation Tracker " ++ replicate 50 ' ' ++ " "
  setSGR [Reset]
  
  -- Display captured values and basic info
  setSGR [SetColor Foreground Vivid Yellow]
  putStrLn $ " Base Freq: " ++ show (baseFrequency tf) ++ " Hz | " ++
             "Base Tempo: " ++ show (baseTempo tf) ++ " BPM | " ++
             "Rows/Beat: " ++ show (rowsPerBeat tf) ++ " | " ++
             "Captured Freq: " ++ show (capturedFreq state) ++ " Hz | " ++
             "Captured Tempo: " ++ show (capturedTempo state) ++ " BPM"
  setSGR [Reset]
  
  -- Draw column headers
  putStr " Row | Tempo "
  forM_ [0..(cols-1)] $ \c -> do
    let chanLabel = "Ch" ++ show (c+1)
    putStr $ " | " ++ chanLabel ++ " Note  Inst Vol Fx"
  putStrLn ""
  
  -- Draw separator line
  putStrLn $ "-" ++ replicate (7 + 8 + (cols * 20)) '-'
  
  -- Draw each row
  forM_ [0..(rows-1)] $ \r -> do
    let rowData = (trackerData tf) !! r
    let rowStr = formatRow rowData r cols (curPos, state)
    putStrLn rowStr
  
  -- Draw status bar at the bottom
  setCursorPosition (rows + 5) 0
  setSGR [SetColor Foreground Vivid White, SetColor Background Vivid Blue]
  putStrLn $ " " ++ statusMessage state ++ replicate (70 - length (statusMessage state)) ' ' ++ " "
  
  -- Draw keyboard shortcuts at the bottom
  if bottomBarExpanded state
    then do
      -- Draw expanded keyboard shortcuts
      setCursorPosition (rows + 6) 0
      setSGR [SetColor Foreground Vivid Cyan, SetColor Background Dull Black]
      putStrLn $ " [↑,↓,←,→]: Navigate | [Enter]: Edit | [Esc]: Cancel | [S]: Save | [E]: Export | [Space]: Play "
      setCursorPosition (rows + 7) 0
      putStrLn $ " [Q]: Quit | [?]: Help | [A]: Add Row | [D]: Delete Row | [Shift+Arrows]: Select | [C/Ctrl+C]: Copy | [V/Ctrl+V]: Paste "
      setCursorPosition (rows + 8) 0
      putStrLn $ " [T,N,I,V,F]: Jump to Field Types (Tempo, Note, Instrument, Volume, Effects) | [C] (no selection): Capture "
      setCursorPosition (rows + 9) 0
      putStrLn $ " [/]: Collapse help bar "
    else do
      -- Draw collapsed keyboard shortcuts
      setCursorPosition (rows + 6) 0
      setSGR [SetColor Foreground Vivid Cyan, SetColor Background Dull Black]
      putStrLn $ " [/]: Show help | [Space]: Play "
  
  setSGR [Reset]
  
  -- Position cursor at edit location if editing
  if isEditing state
    then do
      let row = cursorRow curPos + 3  -- Adjust for header rows
      let col = getFieldColPosition curPos cols
      setCursorPosition row col
    else do
      -- Otherwise position cursor at the current cell
      let row = cursorRow curPos + 3  -- Adjust for header rows
      let col = getFieldColPosition curPos cols
      setCursorPosition row col

-- | Get the actual column position for the cursor
getFieldColPosition :: CursorPos -> Int -> Int
getFieldColPosition curPos _ =
  case cursorField curPos of
    TempoField -> 9  -- Position within tempo column (after "Row | ")
    NoteField -> 13 + (cursorCol curPos * 20)  -- Adjust for each channel's position
    InstField -> 20 + (cursorCol curPos * 20)
    VolField -> 25 + (cursorCol curPos * 20)
    FxField -> 29 + (cursorCol curPos * 20)

-- | Format a row for display
formatRow :: TrackerRow -> Int -> Int -> (CursorPos, TrackerState) -> String
formatRow row rowIdx cols (curPos, state) =
  let
    -- Format row number
    rowNumStr = " " ++ (if rowIdx < 10 then "0" else "") ++ show rowIdx ++ " "
    
    -- Format tempo column
    tempoStr = " | " ++ formatTempoCell row rowIdx 0 TempoField state
    
    -- Format each channel
    chanStrs = [formatChannelCells row chanIdx rowIdx state | chanIdx <- [0..(cols-1)]]
    
    -- Combine all parts
    fullRow = rowNumStr ++ tempoStr ++ concat chanStrs
  in
    -- Highlight the current row if the cursor is on it
    if rowIdx == cursorRow curPos
      then setSGRCode [SetColor Background Dull Blue] ++ fullRow ++ setSGRCode [Reset]
      else fullRow

-- | Format the tempo cell
formatTempoCell :: TrackerRow -> Int -> Int -> Field -> TrackerState -> String
formatTempoCell row rowIdx chanIdx field state =
  let tempoCellContent = case rowTempo row of
        Just td -> case tempoValue td of
          Just val -> tempoInput td
          Nothing -> ""
        Nothing -> ""
      padding = replicate (6 - length tempoCellContent) ' '
      curPos = cursorPos state
      
      -- Check if this cell is the cursor position
      isCursorHere = rowIdx == cursorRow curPos && 
                     0 == cursorCol curPos && 
                     field == cursorField curPos
                     
      -- Check if this cell is in the selection
      isSelected = isInSelection rowIdx 0 field state
  in
    if isCursorHere
      then setSGRCode [SetColor Foreground Vivid Yellow, SetColor Background Dull Magenta] ++ 
           tempoCellContent ++ padding ++ 
           setSGRCode [Reset, SetColor Background Dull Blue]
    else if isSelected
      then setSGRCode [SetColor Foreground Vivid Yellow, SetColor Background Dull Cyan] ++ 
           tempoCellContent ++ padding ++ 
           setSGRCode [Reset]
      else setSGRCode [SetColor Foreground Vivid Yellow] ++ 
           tempoCellContent ++ padding ++ 
           setSGRCode [Reset]

-- | Format cells for a channel
formatChannelCells :: TrackerRow -> Int -> Int -> TrackerState -> String
formatChannelCells row chanIdx rowIdx state =
  let
    -- Get channel data
    chanData = if chanIdx < length (rowChannels row) 
               then rowChannels row !! chanIdx 
               else emptyChannelData
    
    -- Format note cell
    noteStr = case channelNote chanData of
      Just nd -> fromMaybe "" (noteInput nd <$ noteFrequency nd)
      Nothing -> ""
    
    curPos = cursorPos state
    
    -- Determine if this is the current cell for highlighting
    isNoteHighlighted = rowIdx == cursorRow curPos && 
                       chanIdx == cursorCol curPos && 
                       cursorField curPos == NoteField
                       
    isNoteSelected = isInSelection rowIdx chanIdx NoteField state
    
    -- Format instrument cell
    instStr = channelInstrument chanData
    isInstHighlighted = rowIdx == cursorRow curPos && 
                       chanIdx == cursorCol curPos && 
                       cursorField curPos == InstField
                       
    isInstSelected = isInSelection rowIdx chanIdx InstField state
    
    -- Format volume cell
    volStr = show (round (channelVolume chanData * 100) :: Int)
    isVolHighlighted = rowIdx == cursorRow curPos && 
                      chanIdx == cursorCol curPos && 
                      cursorField curPos == VolField
                      
    isVolSelected = isInSelection rowIdx chanIdx VolField state
    
    -- Format effect cell
    fxStr = case effectCommand (channelEffect chanData) of
      Just cmd -> [cmd] ++ fromMaybe "" (effectValue (channelEffect chanData))
      Nothing -> ""
    isFxHighlighted = rowIdx == cursorRow curPos && 
                     chanIdx == cursorCol curPos && 
                     cursorField curPos == FxField
                     
    isFxSelected = isInSelection rowIdx chanIdx FxField state
  in
    " | " ++ 
    formatCellWithSelection noteStr 8 isNoteHighlighted isNoteSelected Vivid White ++ " " ++
    formatCellWithSelection instStr 4 isInstHighlighted isInstSelected Vivid Cyan ++ " " ++
    formatCellWithSelection volStr 3 isVolHighlighted isVolSelected Vivid Green ++ " " ++
    formatCellWithSelection fxStr 2 isFxHighlighted isFxSelected Vivid Magenta

-- | Format a cell with selection highlighting
formatCellWithSelection :: String -> Int -> Bool -> Bool -> ColorIntensity -> Color -> String
formatCellWithSelection content width isHighlighted isSelected intensity color =
  let
    padding = replicate (width - length content) ' '
    paddedContent = content ++ padding
  in
    if isHighlighted
      then setSGRCode [SetColor Foreground intensity color, SetColor Background Dull Magenta] ++ 
           paddedContent ++ 
           setSGRCode [Reset, SetColor Background Dull Blue]
    else if isSelected
      then setSGRCode [SetColor Foreground intensity color, SetColor Background Dull Cyan] ++ 
           paddedContent ++ 
           setSGRCode [Reset]
      else setSGRCode [SetColor Foreground intensity color] ++ 
           paddedContent ++ 
           setSGRCode [Reset]

-- | Check if a cell is in the current selection
isInSelection :: Int -> Int -> Field -> TrackerState -> Bool
isInSelection rowIdx chanIdx field state =
  case (selectionStart state, selectionEnd state) of
    (Just startPos, Just endPos) ->
      let
        minRow = min (cursorRow startPos) (cursorRow endPos)
        maxRow = max (cursorRow startPos) (cursorRow endPos)
        
        -- Convert field and channel to a column index for comparison
        colIndex = case field of
          TempoField -> 0
          NoteField -> 1 + (chanIdx * 4)
          InstField -> 2 + (chanIdx * 4)
          VolField -> 3 + (chanIdx * 4)
          FxField -> 4 + (chanIdx * 4)
          
        -- Get column indices for selection bounds
        startColIndex = getColumnIndex (cursorField startPos) (cursorCol startPos)
        endColIndex = getColumnIndex (cursorField endPos) (cursorCol endPos)
        
        minCol = min startColIndex endColIndex
        maxCol = max startColIndex endColIndex
      in
        rowIdx >= minRow && rowIdx <= maxRow && colIndex >= minCol && colIndex <= maxCol
    _ -> False

-- | Convert field and channel to a column index
getColumnIndex :: Field -> Int -> Int
getColumnIndex TempoField _ = 0
getColumnIndex NoteField chanIdx = 1 + (chanIdx * 4)
getColumnIndex InstField chanIdx = 2 + (chanIdx * 4)
getColumnIndex VolField chanIdx = 3 + (chanIdx * 4)
getColumnIndex FxField chanIdx = 4 + (chanIdx * 4)

-- | Format a cell with appropriate highlighting (legacy function for compatibility)
formatCell :: String -> Int -> Bool -> ColorIntensity -> Color -> String
formatCell content width isHighlighted intensity color =
  formatCellWithSelection content width isHighlighted False intensity color

-- | Handle user input for the terminal tracker
handleTrackerInput :: TrackerState -> IO ()
handleTrackerInput state = do
  c <- getChar
  
  -- Process the input character
  if isEditing state
    then handleEditModeInput state c
    else handleNavigationModeInput state c

-- | Handle input when in edit mode
handleEditModeInput :: TrackerState -> Char -> IO ()
handleEditModeInput state c = 
  case c of
    '\ESC' -> -- Cancel edit
      trackerLoop $ state { 
        isEditing = False, 
        editBuffer = "", 
        statusMessage = "Edit cancelled" 
      }
    
    '\n' -> -- Commit edit
      commitEdit state
    
    '\DEL' -> -- Backspace
      trackerLoop $ state {
        editBuffer = if not (null (editBuffer state))
                    then init (editBuffer state)
                    else ""
      }
    
    _ -> -- Add character to buffer
      if isValidChar c (cursorField (cursorPos state))
        then trackerLoop $ state { editBuffer = editBuffer state ++ [c] }
        else trackerLoop state

-- | Check if a character is valid for a given field
isValidChar :: Char -> Field -> Bool
isValidChar c field =
  case field of
    TempoField -> isDigit c || c == '.' || c == ':' 
    NoteField -> isDigit c || c == '.' || c == ':'
    InstField -> c `elem` ['s', 'i', 'n', 'q', 'r', 'a', 'w', 't']
    VolField -> isDigit c
    FxField -> True  -- Allow any character for effects
  where
    isDigit c' = c' >= '0' && c' <= '9'

-- | Commit the current edit buffer
commitEdit :: TrackerState -> IO ()
commitEdit state = do
  let curPos = cursorPos state
      field = cursorField curPos
      row = cursorRow curPos
      chan = cursorCol curPos
      tf = trackerFile state
      rowData = (trackerData tf) !! row
      buffer = editBuffer state
  
  -- Update the appropriate field
  newTf <- case field of
    TempoField -> return $ updateTempoField tf row buffer state
    NoteField -> return $ updateNoteField tf row chan buffer state
    InstField -> return $ updateInstField tf row chan buffer
    VolField -> return $ updateVolField tf row chan buffer
    FxField -> return $ updateFxField tf row chan buffer
  
  -- Update state with new data and exit edit mode
  trackerLoop $ state {
    trackerFile = newTf,
    isEditing = False,
    editBuffer = "",
    statusMessage = "Edit committed"
  }

-- | Update tempo field in the tracker file
updateTempoField :: TrackerFile -> Int -> String -> TrackerState -> TrackerFile
updateTempoField tf rowIdx buffer state =
  let 
    rows = trackerData tf
    rowData = rows !! rowIdx
    
    -- Parse tempo input
    parsedTempo = if null buffer
                then Nothing
                else parseTempo buffer (baseTempo tf)
    
    -- Create new tempo data
    newTempo = Just $ TempoData {
      tempoInput = buffer,
      tempoValue = parsedTempo
    }
    
    -- Update row
    newRow = rowData { rowTempo = newTempo }
    
    -- Update rows in tracker file
    newRows = take rowIdx rows ++ [newRow] ++ drop (rowIdx + 1) rows
    
    -- Capture tempo if valid
    newCapturedTempo = case parsedTempo of
                        Just t -> t
                        Nothing -> capturedTempo state
  in
    tf { trackerData = newRows }

-- | Update note field in the tracker file
updateNoteField :: TrackerFile -> Int -> Int -> String -> TrackerState -> TrackerFile
updateNoteField tf rowIdx chanIdx buffer state =
  let 
    rows = trackerData tf
    rowData = rows !! rowIdx
    chanData = (rowChannels rowData) !! chanIdx
    
    -- Parse note input
    parsedNote = if null buffer
                then (Nothing, Nothing)
                else case parseNoteInput buffer (baseFrequency tf) of
                      Just (freq, ratio) -> (Just freq, ratio)
                      Nothing -> (Nothing, Nothing)
    
    -- Create new note data
    newNote = if null buffer 
             then Nothing
             else Just $ NoteData {
               noteInput = buffer,
               noteFrequency = fst parsedNote,
               noteRatio = snd parsedNote
             }
    
    -- Update channel
    newChan = chanData { channelNote = newNote }
    
    -- Update channels in row
    newChans = take chanIdx (rowChannels rowData) ++ [newChan] ++ drop (chanIdx + 1) (rowChannels rowData)
    newRow = rowData { rowChannels = newChans }
    
    -- Update rows in tracker file
    newRows = take rowIdx rows ++ [newRow] ++ drop (rowIdx + 1) rows
    
    -- Capture frequency if valid
    newCapturedFreq = case fst parsedNote of
                      Just f -> f
                      Nothing -> capturedFreq state
  in
    tf { trackerData = newRows }

-- | Update instrument field in the tracker file
updateInstField :: TrackerFile -> Int -> Int -> String -> TrackerFile
updateInstField tf rowIdx chanIdx buffer =
  let 
    rows = trackerData tf
    rowData = rows !! rowIdx
    chanData = (rowChannels rowData) !! chanIdx
    
    -- Validate instrument
    validInst = if null buffer 
               then "sin"  -- Default to sine
               else case stringToInstrument buffer of
                     Just _ -> buffer
                     Nothing -> channelInstrument chanData
    
    -- Update channel
    newChan = chanData { channelInstrument = validInst }
    
    -- Update channels in row
    newChans = take chanIdx (rowChannels rowData) ++ [newChan] ++ drop (chanIdx + 1) (rowChannels rowData)
    newRow = rowData { rowChannels = newChans }
    
    -- Update rows in tracker file
    newRows = take rowIdx rows ++ [newRow] ++ drop (rowIdx + 1) rows
  in
    tf { trackerData = newRows }

-- | Update volume field in the tracker file
updateVolField :: TrackerFile -> Int -> Int -> String -> TrackerFile
updateVolField tf rowIdx chanIdx buffer =
  let 
    rows = trackerData tf
    rowData = rows !! rowIdx
    chanData = (rowChannels rowData) !! chanIdx
    
    -- Parse volume (0-100)
    parsedVol = if null buffer 
               then 100  -- Default volume
               else case reads buffer of
                     [(val, "")] -> min 100 (max 0 val)
                     _ -> round (channelVolume chanData * 100)
    
    -- Update channel with normalized volume (0.0-1.0)
    newChan = chanData { channelVolume = fromIntegral parsedVol / 100.0 }
    
    -- Update channels in row
    newChans = take chanIdx (rowChannels rowData) ++ [newChan] ++ drop (chanIdx + 1) (rowChannels rowData)
    newRow = rowData { rowChannels = newChans }
    
    -- Update rows in tracker file
    newRows = take rowIdx rows ++ [newRow] ++ drop (rowIdx + 1) rows
  in
    tf { trackerData = newRows }

-- | Update effect field in the tracker file
updateFxField :: TrackerFile -> Int -> Int -> String -> TrackerFile
updateFxField tf rowIdx chanIdx buffer =
  let 
    rows = trackerData tf
    rowData = rows !! rowIdx
    chanData = (rowChannels rowData) !! chanIdx
    
    -- Parse effect (first char is command, rest is value)
    (effCmd, effVal) = if null buffer 
                      then (Nothing, Nothing)
                      else (Just (head buffer), Just (drop 1 buffer))
    
    -- Create new effect data
    newEffect = EffectData {
      effectCommand = effCmd,
      effectValue = effVal,
      effectInput = buffer
    }
    
    -- Update channel
    newChan = chanData { channelEffect = newEffect }
    
    -- Update channels in row
    newChans = take chanIdx (rowChannels rowData) ++ [newChan] ++ drop (chanIdx + 1) (rowChannels rowData)
    newRow = rowData { rowChannels = newChans }
    
    -- Update rows in tracker file
    newRows = take rowIdx rows ++ [newRow] ++ drop (rowIdx + 1) rows
  in
    tf { trackerData = newRows }

-- | Handle input when in navigation mode
handleNavigationModeInput :: TrackerState -> Char -> IO ()
handleNavigationModeInput state c =
  case c of
    '\n' -> -- Start editing
      startEditing state
    
    'q' -> -- Quit
      exitTracker state
    
    'h' -> -- Help
      showHelp state
    
    '?' -> -- Help (alternative)
      showHelp state
    
    '/' -> -- Toggle bottom help bar
      trackerLoop $ state { 
        bottomBarExpanded = not (bottomBarExpanded state),
        statusMessage = if bottomBarExpanded state 
                       then "Help bar collapsed" 
                       else "Help bar expanded"
      }
    
    ' ' -> -- Spacebar - Render and play current tracker state
      renderAndPlay state
    
    's' -> -- Save
      saveTrackerFile state
    
    'e' -> -- Export to WAV
      exportToWav state
    
    'c' -> -- Handle dual-use of 'c' key
      if isJust (selectionStart state) && isJust (selectionEnd state)
        then copySelection state    -- If there's a selection, use 'c' for copy
        else captureValue state     -- Otherwise use 'c' for capture
      
    'v' -> -- Alt shortcut for paste
      pasteSelection state
      
    'a' -> -- Add row at current position
      addRow state
    
    'd' -> -- Delete current row
      deleteRow state
      
    -- Original copy/paste operations (still supported)
    -- Handle Ctrl+C
    '\ETX' -> -- Ctrl+C (ETX is ASCII 3)
      copySelection state
      
    -- Handle Ctrl+V
    '\SYN' -> -- Ctrl+V (SYN is ASCII 22)
      pasteSelection state
    
    -- Arrow keys navigation (special handling for escape sequences)
    '\ESC' -> do
      -- Check for arrow key sequence
      c1 <- getChar
      if c1 == '['
        then do
          c2 <- getChar
          -- Check for shifted arrow keys (they send a 1;2 sequence after the arrow)
          if c2 == '1'
            then do
              -- Skip the ; and 2
              _ <- getChar  -- Skip ;
              _ <- getChar  -- Skip 2
              c4 <- getChar
              case c4 of
                'A' -> moveWithSelection state (-1) 0 True  -- Shift+Up
                'B' -> moveWithSelection state 1 0 True     -- Shift+Down
                'C' -> moveWithSelection state 0 1 True     -- Shift+Right
                'D' -> moveWithSelection state 0 (-1) True  -- Shift+Left
                _ -> trackerLoop state
            else
              case c2 of
                'A' -> moveWithSelection state (-1) 0 False  -- Up
                'B' -> moveWithSelection state 1 0 False     -- Down
                'C' -> moveWithSelection state 0 1 False     -- Right
                'D' -> moveWithSelection state 0 (-1) False  -- Left
                _ -> trackerLoop state
        else trackerLoop state
    
    -- Regular key navigation
    'k' -> moveWithSelection state (-1) 0 False  -- Up
    'j' -> moveWithSelection state 1 0 False     -- Down
    'l' -> moveWithSelection state 0 1 False     -- Right
    'h' -> moveWithSelection state 0 (-1) False  -- Left
    
    -- Jump to specific field types (across channels)
    't' -> jumpToField state TempoField -- Tempo
    'n' -> jumpToField state NoteField  -- Note
    'i' -> jumpToField state InstField  -- Instrument
    'v' -> jumpToField state VolField   -- Volume
    'f' -> jumpToField state FxField    -- Effect
    
    _ -> trackerLoop state

-- | Start editing the current cell
startEditing :: TrackerState -> IO ()
startEditing state = do
  let curPos = cursorPos state
      field = cursorField curPos
      row = cursorRow curPos
      chan = cursorCol curPos
      tf = trackerFile state
      rowData = (trackerData tf) !! row
  
  -- Get the current content to pre-fill the edit buffer
  let initialContent = case field of
        TempoField -> 
          case rowTempo rowData of
            Just td -> tempoInput td
            Nothing -> ""
        
        NoteField ->
          case channelNote ((rowChannels rowData) !! chan) of
            Just nd -> noteInput nd
            Nothing -> ""
        
        InstField -> 
          channelInstrument ((rowChannels rowData) !! chan)
        
        VolField ->
          let vol = channelVolume ((rowChannels rowData) !! chan)
          in show (round (vol * 100) :: Int)
        
        FxField ->
          let 
            effect = channelEffect ((rowChannels rowData) !! chan)
            cmd = maybe "" (: []) (effectCommand effect)
            val = fromMaybe "" (effectValue effect)
          in
            cmd ++ val
  
  -- Enter edit mode with the current content
  trackerLoop $ state {
    isEditing = True,
    editBuffer = initialContent,
    statusMessage = "Editing: " ++ show field
  }

-- | Move the cursor by row and column offsets with optional selection
moveWithSelection :: TrackerState -> Int -> Int -> Bool -> IO ()
moveWithSelection state rowDelta colDelta isShiftPressed = do
  let curPos = cursorPos state
      tf = trackerFile state
      rows = numRows tf
      cols = numChannels tf
      
      -- Calculate new row position (with wrapping)
      newRow = (cursorRow curPos + rowDelta) `mod` rows
      
      -- Current field and channel
      field = cursorField curPos
      chan = cursorCol curPos
      
      -- Calculate new column/field position based on field type
      (newField, newCol) = 
        if colDelta == 0 
        then (field, chan)  -- No column change
        else case field of
          TempoField ->
            if colDelta > 0 
            then (NoteField, 0)  -- Move right from tempo to first channel
            else (FxField, cols - 1)  -- Move left from tempo wraps to last effect field
          
          NoteField ->
            if colDelta > 0 
            then (InstField, chan)  -- Move to instrument in same channel
            else if chan > 0
                 then (FxField, chan - 1)  -- Move to previous channel's effect
                 else (TempoField, 0)  -- Move to tempo field
          
          InstField ->
            if colDelta > 0 
            then (VolField, chan)  -- Move to volume in same channel
            else (NoteField, chan)  -- Move to note in same channel
          
          VolField ->
            if colDelta > 0 
            then (FxField, chan)  -- Move to effect in same channel
            else (InstField, chan)  -- Move to instrument in same channel
          
          FxField ->
            if colDelta > 0 
            then if chan < cols - 1
                 then (NoteField, chan + 1)  -- Move to next channel's note
                 else (TempoField, 0)  -- Wrap to tempo field
            else (VolField, chan)  -- Move to volume in same channel
      
      -- Create the new cursor position
      newCurPos = CursorPos newRow newCol newField
      
      -- Handle selection based on shift key
      (newSelStart, newSelEnd) = 
        if isShiftPressed
          then
            -- If shift is pressed and no selection exists, start a new selection
            case selectionStart state of
              Nothing -> (Just curPos, Just newCurPos)
              Just startPos -> (Just startPos, Just newCurPos)
          else
            -- If shift is not pressed, clear any selection
            (Nothing, Nothing)
      
      -- Create status message
      statusMsg = 
        if isShiftPressed
          then "Selection active: from Row " ++ show (cursorRow (fromJust newSelStart)) ++ " to Row " ++ show newRow
          else "Position: Row " ++ show newRow ++ ", " ++ show newField ++ 
               (if newField /= TempoField then " (Ch" ++ show (newCol + 1) ++ ")" else "")
  
  -- Update state with new cursor position and selection
  trackerLoop $ state {
    cursorPos = newCurPos,
    selectionStart = newSelStart,
    selectionEnd = newSelEnd,
    statusMessage = statusMsg
  }

-- | Copy the current selection to clipboard
copySelection :: TrackerState -> IO ()
copySelection state = 
  case (selectionStart state, selectionEnd state) of
    (Just startPos, Just endPos) -> do
      -- Get row and column ranges
      let minRow = min (cursorRow startPos) (cursorRow endPos)
          maxRow = max (cursorRow startPos) (cursorRow endPos)
          startColIndex = getColumnIndex (cursorField startPos) (cursorCol startPos)
          endColIndex = getColumnIndex (cursorField endPos) (cursorCol endPos)
          minCol = min startColIndex endColIndex
          maxCol = max startColIndex endColIndex
          tf = trackerFile state
          
          -- Extract cells from the selection
          clipboardItems = 
            [ ClipboardItem 
                { cbRow = r - minRow  -- Store relative positions
                , cbCol = c - minCol
                , cbField = getFieldFromColIndex c
                , cbContent = getCellContent tf r c
                }
            | r <- [minRow..maxRow]
            , c <- [minCol..maxCol]
            ]
      
      -- Update state with new clipboard
      trackerLoop $ state {
        clipboard = clipboardItems,
        statusMessage = "Copied " ++ show (length clipboardItems) ++ " cells"
      }
    _ -> 
      -- No selection, nothing to copy
      trackerLoop $ state {
        statusMessage = "Nothing selected to copy"
      }

-- | Paste from clipboard at current cursor position
pasteSelection :: TrackerState -> IO ()
pasteSelection state =
  if null (clipboard state)
    then 
      -- Empty clipboard
      trackerLoop $ state { statusMessage = "Clipboard is empty" }
    else do
      -- Get the current cursor position as the paste target
      let curPos = cursorPos state
          baseRow = cursorRow curPos
          baseCol = getColumnIndex (cursorField curPos) (cursorCol curPos)
          tf = trackerFile state
          rows = trackerData tf
          numChans = numChannels tf
          
          -- Ensure we don't paste out of bounds
          validItems = filter (isValidPastePosition baseRow baseCol numChans (length rows)) (clipboard state)
          
          -- Update each cell from clipboard
          updatedTf = foldl (pasteCell baseRow baseCol) tf validItems
      
      -- Update state with pasted content
      trackerLoop $ state {
        trackerFile = updatedTf,
        statusMessage = if null validItems
                       then "No valid cells to paste"
                       else "Pasted " ++ show (length validItems) ++ " cells"
      }

-- | Check if paste position is valid
isValidPastePosition :: Int -> Int -> Int -> Int -> ClipboardItem -> Bool
isValidPastePosition baseRow baseCol numChans maxRows item =
  let 
    targetRow = baseRow + cbRow item
    targetColIndex = baseCol + cbCol item
    targetField = cbField item
    targetChan = case targetField of
                  TempoField -> 0
                  _ -> (targetColIndex - 1) `div` 4
  in
    -- Check if position is within bounds
    targetRow >= 0 && 
    targetRow < maxRows && 
    (targetField == TempoField || targetChan < numChans)

-- | Paste a single cell from clipboard
pasteCell :: Int -> Int -> TrackerFile -> ClipboardItem -> TrackerFile
pasteCell baseRow baseCol tf item =
  let
    -- Calculate target position
    targetRow = baseRow + cbRow item
    targetColIndex = baseCol + cbCol item
    targetField = cbField item
    
    -- Extract channel and field from column index
    targetChan = case targetField of
      TempoField -> 0
      _ -> (targetColIndex - 1) `div` 4
      
    -- Get content to paste
    content = cbContent item
    
    -- Get tracker data
    rows = trackerData tf
    maxRows = length rows
    numChans = numChannels tf
  in
    -- Only paste if within bounds (double check)
    if targetRow >= 0 && targetRow < maxRows && (targetField == TempoField || targetChan < numChans)
      then 
        -- Update target cell based on field type
        case targetField of
          TempoField -> 
            let
              row = rows !! targetRow
              newTempo = Just $ TempoData { 
                tempoInput = content, 
                tempoValue = parseTempo content (baseTempo tf) 
              }
              newRow = row { rowTempo = newTempo }
              newRows = take targetRow rows ++ [newRow] ++ drop (targetRow + 1) rows
            in
              tf { trackerData = newRows }
              
          NoteField ->
            let
              row = rows !! targetRow
              chanData = if targetChan < length (rowChannels row) then rowChannels row !! targetChan else emptyChannelData
              parsed = parseNoteInput content (baseFrequency tf)
              (freq, ratio) = case parsed of
                  Just (f, r) -> (Just f, r)
                  Nothing -> (Nothing, Nothing)
              newNote = if null content then Nothing else Just $ NoteData { noteInput = content, noteFrequency = freq, noteRatio = ratio }
              newChan = chanData { channelNote = newNote }
              newChans = take targetChan (rowChannels row) ++ [newChan] ++ drop (targetChan + 1) (rowChannels row)
              newRow = row { rowChannels = newChans }
              newRows = take targetRow rows ++ [newRow] ++ drop (targetRow + 1) rows
            in
              tf { trackerData = newRows }
              
          InstField ->
            let
              row = rows !! targetRow
              chanData = if targetChan < length (rowChannels row) then rowChannels row !! targetChan else emptyChannelData
              newChan = chanData { channelInstrument = if null content then "sin" else content }
              newChans = take targetChan (rowChannels row) ++ [newChan] ++ drop (targetChan + 1) (rowChannels row)
              newRow = row { rowChannels = newChans }
              newRows = take targetRow rows ++ [newRow] ++ drop (targetRow + 1) rows
            in
              tf { trackerData = newRows }
              
          VolField ->
            let
              row = rows !! targetRow
              chanData = if targetChan < length (rowChannels row) then rowChannels row !! targetChan else emptyChannelData
              volValue = if null content then 1.0 else 
                          case reads content of
                            [(val, "")] -> min 1.0 (max 0.0 (fromIntegral val / 100.0))
                            _ -> channelVolume chanData
              newChan = chanData { channelVolume = volValue }
              newChans = take targetChan (rowChannels row) ++ [newChan] ++ drop (targetChan + 1) (rowChannels row)
              newRow = row { rowChannels = newChans }
              newRows = take targetRow rows ++ [newRow] ++ drop (targetRow + 1) rows
            in
              tf { trackerData = newRows }
              
          FxField ->
            let
              row = rows !! targetRow
              chanData = if targetChan < length (rowChannels row) then rowChannels row !! targetChan else emptyChannelData
              (cmd, val) = if null content then (Nothing, Nothing) else (Just (head content), Just (drop 1 content))
              newEffect = EffectData { effectCommand = cmd, effectValue = val, effectInput = content }
              newChan = chanData { channelEffect = newEffect }
              newChans = take targetChan (rowChannels row) ++ [newChan] ++ drop (targetChan + 1) (rowChannels row)
              newRow = row { rowChannels = newChans }
              newRows = take targetRow rows ++ [newRow] ++ drop (targetRow + 1) rows
            in
              tf { trackerData = newRows }
      else
        -- Out of bounds, don't change
        tf

-- | Get the field type from a column index
getFieldFromColIndex :: Int -> Field
getFieldFromColIndex 0 = TempoField
getFieldFromColIndex col =
  let
    fieldIdx = (col - 1) `mod` 4
  in
    case fieldIdx of
      0 -> NoteField
      1 -> InstField
      2 -> VolField
      3 -> FxField
      _ -> NoteField -- Default case, should not happen

-- | Get content of a cell by row and column index
getCellContent :: TrackerFile -> Int -> Int -> String
getCellContent tf row colIndex =
  let
    rows = trackerData tf
    rowData = if row < length rows then rows !! row else emptyTrackerRow 0
  in
    -- Get content based on field type
    case getFieldFromColIndex colIndex of
      TempoField ->
        case rowTempo rowData of
          Just tempo -> tempoInput tempo
          Nothing -> ""
          
      NoteField ->
        let 
          chanIdx = (colIndex - 1) `div` 4
          chanData = if chanIdx < length (rowChannels rowData) 
                     then rowChannels rowData !! chanIdx 
                     else emptyChannelData
        in
          case channelNote chanData of
            Just note -> noteInput note
            Nothing -> ""
            
      InstField ->
        let 
          chanIdx = (colIndex - 1) `div` 4
          chanData = if chanIdx < length (rowChannels rowData) 
                     then rowChannels rowData !! chanIdx 
                     else emptyChannelData
        in
          channelInstrument chanData
          
      VolField ->
        let 
          chanIdx = (colIndex - 1) `div` 4
          chanData = if chanIdx < length (rowChannels rowData) 
                     then rowChannels rowData !! chanIdx 
                     else emptyChannelData
        in
          show (round (channelVolume chanData * 100) :: Int)
          
      FxField ->
        let 
          chanIdx = (colIndex - 1) `div` 4
          chanData = if chanIdx < length (rowChannels rowData) 
                     then rowChannels rowData !! chanIdx 
                     else emptyChannelData
          effect = channelEffect chanData
        in
          case effectCommand effect of
            Just cmd -> [cmd] ++ fromMaybe "" (effectValue effect)
            Nothing -> ""

-- | Jump to a specific field type
jumpToField :: TrackerState -> Field -> IO ()
jumpToField state field = do
  let curPos = cursorPos state
      tf = trackerFile state
      
      -- Keep the same row and adjust the column/field
      (newField, newCol) = case field of
        TempoField -> (field, 0)  -- Tempo is always in column 0
        _ -> (field, cursorCol curPos)  -- Keep the same channel for other fields
  
  -- Update state with new cursor position
  trackerLoop $ state {
    cursorPos = CursorPos (cursorRow curPos) newCol newField,
    statusMessage = "Jumped to: " ++ show newField
  }

-- | Save the current tracker file
saveTrackerFile :: TrackerState -> IO ()
saveTrackerFile state = 
  case filePath state of
    Just path -> do
      -- Save to existing path
      writeTrackerFile path (trackerFile state)
      trackerLoop $ state { statusMessage = "Saved to: " ++ path }
    
    Nothing -> do
      -- Ask for a file path
      clearScreen
      setCursorPosition 0 0
      putStrLn "Enter file path to save: "
      newPath <- getLine
      
      -- Save to the new path
      writeTrackerFile newPath (trackerFile state)
      trackerLoop $ state { 
        statusMessage = "Saved to: " ++ newPath,
        filePath = Just newPath
      }

-- | Export the current tracker file to WAV
exportToWav :: TrackerState -> IO ()
exportToWav state = do
  -- Ask for output file and duration
  clearScreen
  setCursorPosition 0 0
  putStrLn "Enter output WAV file path: "
  outputPath <- getLine
  putStrLn "Enter duration in seconds: "
  durationStr <- getLine
  let duration = read durationStr :: Double
  
  -- Export to WAV
  renderTrackerFile (trackerFile state) outputPath duration
  
  -- Return to tracker
  trackerLoop $ state { statusMessage = "Exported to: " ++ outputPath }

-- | Render and play the current tracker state
renderAndPlay :: TrackerState -> IO ()
renderAndPlay state = do
  -- Use a temporary file for the WAV output
  withSystemTempFile "tracker_preview.wav" $ \tempFile tempHandle -> do
    -- Close the handle as we'll use the file path
    hClose tempHandle
    
    -- Set status while rendering
    clearScreen
    setCursorPosition 0 0
    putStrLn "Rendering audio preview..."
    
    -- Default duration of 5 seconds
    let duration = 5.0
    
    -- Render to temporary WAV file
    renderTrackerFile (trackerFile state) tempFile duration
    
    -- Play the file using system audio player
    -- Check if we're on macOS (afplay) or Linux (aplay)
    putStrLn "Playing audio preview..."
    result <- system $ "afplay " ++ tempFile ++ " || aplay " ++ tempFile
    
    -- Return to tracker
    let successMsg = "Played preview successfully"
        errorMsg = "Error playing preview"
    
    -- Return to tracker with appropriate status message
    trackerLoop $ state { 
      statusMessage = case result of
                        ExitSuccess -> successMsg
                        _ -> errorMsg
    }

-- | Capture the current frequency or tempo value
captureValue :: TrackerState -> IO ()
captureValue state = do
  let curPos = cursorPos state
      field = cursorField curPos
      row = cursorRow curPos
      chan = cursorCol curPos
      tf = trackerFile state
      rowData = (trackerData tf) !! row
  
  -- Capture value based on field type
  case field of
    TempoField ->
      case rowTempo rowData >>= tempoValue of
        Just tempo ->
          trackerLoop $ state {
            capturedTempo = tempo,
            statusMessage = "Captured tempo: " ++ show tempo ++ " BPM"
          }
        Nothing ->
          trackerLoop $ state { statusMessage = "No tempo value to capture" }
    
    NoteField ->
      case channelNote ((rowChannels rowData) !! chan) >>= noteFrequency of
        Just freq ->
          trackerLoop $ state {
            capturedFreq = freq,
            statusMessage = "Captured frequency: " ++ show freq ++ " Hz"
          }
        Nothing ->
          trackerLoop $ state { statusMessage = "No frequency value to capture" }
    
    _ -> trackerLoop $ state { 
      statusMessage = "Can only capture from tempo or note fields" 
    }
    
-- | Add a new row at the current cursor position
addRow :: TrackerState -> IO ()
addRow state = do
  let curPos = cursorPos state
      rowIdx = cursorRow curPos
      tf = trackerFile state
      rows = trackerData tf
      channels = numChannels tf
      
      -- Create a new empty row
      newRow = emptyTrackerRow channels
      
      -- Insert the new row at the current position
      newRows = take rowIdx rows ++ [newRow] ++ drop rowIdx rows
      
      -- Update the tracker file with new rows
      newTf = tf { 
        trackerData = newRows,
        numRows = numRows tf + 1
      }
  
  -- Update state with new tracker file and status
  trackerLoop $ state {
    trackerFile = newTf,
    statusMessage = "Row added at position " ++ show rowIdx
  }

-- | Delete the current row
deleteRow :: TrackerState -> IO ()
deleteRow state = do
  let curPos = cursorPos state
      rowIdx = cursorRow curPos
      tf = trackerFile state
      rows = trackerData tf
      currentRows = numRows tf
      
  -- Don't delete the last row
  if currentRows <= 1
    then trackerLoop $ state { statusMessage = "Cannot delete the last row" }
    else do
      -- Remove the current row
      let newRows = take rowIdx rows ++ drop (rowIdx + 1) rows
          
          -- Update the tracker file with new rows
          newTf = tf { 
            trackerData = newRows,
            numRows = currentRows - 1
          }
          
          -- Adjust cursor position if needed
          newRowIdx = if rowIdx >= currentRows - 1 
                      then rowIdx - 1  -- Move cursor up if deleting last row
                      else rowIdx      -- Keep cursor at same position
          newCurPos = (cursorPos state) { cursorRow = newRowIdx }
      
      -- Update state with new tracker file and status
      trackerLoop $ state {
        trackerFile = newTf,
        cursorPos = newCurPos,
        statusMessage = "Row deleted at position " ++ show rowIdx
      }

-- | Show help screen
showHelp :: TrackerState -> IO ()
showHelp state = do
  clearScreen
  setCursorPosition 0 0
  putStrLn "Just Intonation Tracker - Help"
  putStrLn "============================"
  putStrLn "Navigation:"
  putStrLn "  Arrow keys / hjkl - Move cursor"
  putStrLn "  t - Jump to tempo field"
  putStrLn "  n - Jump to note field"
  putStrLn "  i - Jump to instrument field"
  putStrLn "  v - Jump to volume field"
  putStrLn "  f - Jump to effect field"
  putStrLn ""
  putStrLn "Editing:"
  putStrLn "  Enter - Start/finish editing"
  putStrLn "  Esc - Cancel edit"
  putStrLn "  Backspace - Delete character"
  putStrLn ""
  putStrLn "Actions:"
  putStrLn "  s - Save tracker file"
  putStrLn "  e - Export to WAV"
  putStrLn "  c - Capture frequency/tempo value"
  putStrLn "  a - Add row at current position"
  putStrLn "  d - Delete current row"
  putStrLn "  q - Quit"
  putStrLn "  ? / h - Show this help"
  
  putStrLn ""
  putStrLn "Selection and Clipboard:"
  putStrLn "  Shift+Arrow keys - Select multiple cells"
  putStrLn "  Ctrl+C or c (with selection) - Copy selected cells"
  putStrLn "  Ctrl+V or v - Paste copied cells at cursor position"
  putStrLn "  c (without selection) - Capture frequency/tempo value"
  putStrLn "  / - Toggle help bar expansion"
  putStrLn "  Spacebar - Play audio preview of current tracker content"
  putStrLn ""
  putStrLn "Input formats:"
  putStrLn "  Notes: Direct frequency (e.g. '440') or ratio (e.g. '3:2')"
  putStrLn "  Tempo: Direct BPM (e.g. '120') or ratio (e.g. '3:2')"
  putStrLn "  Instruments: 'sin' (sine), 'sqr' (square), 'saw' (sawtooth), 'tri' (triangle)"
  putStrLn "  Volume: 0-100"
  putStrLn "  Effects: First char is command (e.g. 'A' for arpeggio), rest is value"
  putStrLn ""
  putStrLn "Press any key to return to tracker..."
  
  -- Wait for key press
  _ <- getChar
  trackerLoop state

-- | Exit the tracker
exitTracker :: TrackerState -> IO ()
exitTracker state = do
  -- Ask for confirmation if there are unsaved changes
  clearScreen
  setCursorPosition 0 0
  putStrLn "Exit tracker? (y/n)"
  
  c <- getChar
  if c == 'y' || c == 'Y'
    then do
#if !defined(mingw32_HOST_OS) && !defined(__MINGW32__)
      -- Restore the original SIGINT handler before returning to menu
      case sigintHandler state of
        Just handler -> installHandler sigINT handler Nothing >> return ()
        Nothing -> return ()
#endif
      trackerMenu  -- Return to main menu
    else trackerLoop state  -- Continue tracking

-- | Main tracker loop
trackerLoop :: TrackerState -> IO ()
trackerLoop state = do
  -- Draw the current state of the tracker
  drawTrackerGrid state
  
  -- Handle user input
  handleTrackerInput state

-- | Create and open a new terminal tracker with the given file
openTerminalTracker :: TrackerFile -> Maybe FilePath -> IO ()
openTerminalTracker tf path = do
  -- Set up terminal for interactive input
  hSetBuffering stdin NoBuffering  -- Read one char at a time
  hSetEcho stdin False             -- Don't echo input
  
#if !defined(mingw32_HOST_OS) && !defined(__MINGW32__)
  -- Install a custom SIGINT handler for Ctrl+C to be used for copying
  -- Store the old handler to restore it later
  oldHandler <- installHandler sigINT (Catch $ do
    -- Instead of terminating, we'll use a special character to trigger the copy operation
    -- This allows Ctrl+C to work for copying without terminating the program
    putChar '\ETX'
    return ()) Nothing
    
  -- Initialize the tracker state with the original handler saved
  let initialState = (initTrackerState tf path) { sigintHandler = Just oldHandler }
#else
  -- On Windows, we don't need the SIGINT handler
  let initialState = initTrackerState tf path
#endif
  
  -- Start the tracker loop
  (trackerLoop initialState `catch` (\(e :: SomeException) -> do
    -- Reset terminal on error
    hSetBuffering stdin LineBuffering
    hSetEcho stdin True
    setSGR [Reset]
    clearScreen
    setCursorPosition 0 0
    putStrLn $ "Error: " ++ show e
    putStrLn "Press Enter to continue..."
    _ <- getLine
#if !defined(mingw32_HOST_OS) && !defined(__MINGW32__)
    -- Restore the original SIGINT handler before returning to menu
    case sigintHandler initialState of
      Just handler -> installHandler sigINT handler Nothing >> return ()
      Nothing -> return ()
#endif
    trackerMenu))

-- | Add tracker functionality to the main menu
trackerMenu :: IO ()
trackerMenu = do
  -- Reset terminal settings
  hSetBuffering stdin LineBuffering
  hSetEcho stdin True
  setSGR [Reset]
  clearScreen
  setCursorPosition 0 0
  
  putStrLn "========================================"
  putStrLn "Just Intonation Tracker"
  putStrLn "========================================"
  putStrLn "Choose an option:"
  putStrLn "1. Open terminal tracker with new file"
  putStrLn "2. Open terminal tracker with existing file"
  putStrLn "3. Load and render tracker file"
  putStrLn "4. Display tracker file information"
  putStrLn "5. Create example tracker file"
  putStrLn "6. Export tracker file to CSV (for editing)"
  putStrLn "7. Load and render test files"
  putStrLn "8. Return to main menu"
  putStrLn "Enter your choice (1-8): "
  
  choice <- getLine
  case choice of
    "1" -> do
      putStrLn "Enter output file path: "
      filePath <- getLine
      putStrLn "Enter base frequency (Hz): "
      baseFreqStr <- getLine
      let baseFreq = read baseFreqStr :: Double
      putStrLn "Enter base tempo (BPM): "
      tempoStr <- getLine
      let tempo = read tempoStr :: Double
      putStrLn "Enter rows per beat: "
      rowsPerBeatStr <- getLine
      let rowsPerBeat = read rowsPerBeatStr :: Int
      putStrLn "Enter number of rows: "
      numRowsStr <- getLine
      let numRows = read numRowsStr :: Int
      putStrLn "Enter number of channels: "
      numChannelsStr <- getLine
      let numChannels = read numChannelsStr :: Int
      
      -- Create the file
      let tf = initTrackerFile baseFreq tempo rowsPerBeat numRows numChannels
      writeTrackerFile filePath tf
      
      -- Open in terminal tracker
      openTerminalTracker tf (Just filePath)
    
    "2" -> do
      putStrLn "Enter tracker file path: "
      filePath <- getLine
      fileExists <- Dir.doesFileExist filePath
      if not fileExists
        then do
          putStrLn $ "Error: File not found: " ++ filePath
          trackerMenu
        else do
          result <- readTrackerFile filePath
          case result of
            Left err -> do
              putStrLn $ "Error: " ++ err
              trackerMenu
            Right tf -> openTerminalTracker tf (Just filePath)
    
    "3" -> do
      putStrLn "Enter tracker file path: "
      filePath <- getLine
      fileExists <- Dir.doesFileExist filePath
      if not fileExists
        then do
          putStrLn $ "Error: File not found: " ++ filePath
          trackerMenu
        else do
          putStrLn "Enter output WAV file path: "
          outputPath <- getLine
          putStrLn "Enter duration in seconds: "
          durationStr <- getLine
          let duration = read durationStr :: Double
          loadAndRenderTrackerFile filePath outputPath duration
          trackerMenu
    
    "4" -> do
      putStrLn "Enter tracker file path: "
      filePath <- getLine
      result <- readTrackerFile filePath
      case result of
        Left err -> putStrLn $ "Error: " ++ err
        Right tf -> displayTrackerInfo tf
      trackerMenu
    
    "5" -> do
      putStrLn "Enter output file path: "
      filePath <- getLine
      createExampleTrackerFile filePath
      trackerMenu
    
    "6" -> do
      putStrLn "Enter tracker file path: "
      filePath <- getLine
      putStrLn "Enter output CSV path: "
      csvPath <- getLine
      result <- readTrackerFile filePath
      case result of
        Left err -> putStrLn $ "Error: " ++ err
        Right tf -> exportTrackerCsv tf csvPath
      trackerMenu
    
    "7" -> do
      putStrLn "Loading and rendering test files..."
      putStrLn "1. Basic example tracker (example_tracker.json)"
      putStrLn "2. Complex example tracker (complex_example_tracker.json)"
      putStrLn "3. Instrument test tracker (instrument_test_tracker.json)"
      putStrLn "4. User example tracker (user_example_tracker.json)"
      putStrLn "Enter your choice (1-4): "
      
      testChoice <- getLine
      case testChoice of
        "1" -> loadAndRenderTrackerFile "example_tracker.json" "example_tracker.wav" 8.0
        "2" -> loadAndRenderTrackerFile "complex_example_tracker.json" "complex_example_tracker.wav" 10.0
        "3" -> loadAndRenderTrackerFile "instrument_test_tracker.json" "instrument_test_tracker.wav" 8.0
        "4" -> loadAndRenderTrackerFile "user_example_tracker.json" "user_example_tracker.wav" 8.0
        _ -> putStrLn "Invalid choice."
      
      trackerMenu
      
    "8" -> return ()
    
    _ -> do
      putStrLn "Invalid choice. Please try again."
      trackerMenu