module TrackerMain where

import System.IO
import System.Directory (doesFileExist)
import Control.Monad (when)
import Data.Maybe (isJust, fromJust)
import Data.Ratio
import Euterpea
import Main hiding (main)
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
        (10, "733.33"),-- F# (5:3 ratio)
        (12, "825"),   -- G# (15:8 ratio)
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

-- | Add tracker functionality to the main menu
trackerMenu :: IO ()
trackerMenu = do
  putStrLn "========================================"
  putStrLn "Just Intonation Tracker"
  putStrLn "========================================"
  putStrLn "Choose an option:"
  putStrLn "1. Load and render tracker file"
  putStrLn "2. Display tracker file information"
  putStrLn "3. Create example tracker file"
  putStrLn "4. Create empty tracker file"
  putStrLn "5. Export tracker file to CSV (for editing)"
  putStrLn "6. Load and render test files"
  putStrLn "7. Return to main menu"
  putStrLn "Enter your choice (1-7): "
  
  choice <- getLine
  case choice of
    "1" -> do
      putStrLn "Enter tracker file path: "
      filePath <- getLine
      fileExists <- doesFileExist filePath
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
    
    "2" -> do
      putStrLn "Enter tracker file path: "
      filePath <- getLine
      result <- readTrackerFile filePath
      case result of
        Left err -> putStrLn $ "Error: " ++ err
        Right tf -> displayTrackerInfo tf
      trackerMenu
    
    "3" -> do
      putStrLn "Enter output file path: "
      filePath <- getLine
      createExampleTrackerFile filePath
      trackerMenu
    
    "4" -> do
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
      createEmptyTrackerFile filePath baseFreq tempo rowsPerBeat numRows numChannels
      trackerMenu
    
    "5" -> do
      putStrLn "Enter tracker file path: "
      filePath <- getLine
      putStrLn "Enter output CSV path: "
      csvPath <- getLine
      result <- readTrackerFile filePath
      case result of
        Left err -> putStrLn $ "Error: " ++ err
        Right tf -> exportTrackerCsv tf csvPath
      trackerMenu
    
    "6" -> do
      putStrLn "Loading and rendering test files..."
      putStrLn "1. Basic example tracker (example_tracker.json)"
      putStrLn "2. Complex example tracker (complex_example_tracker.json)"
      putStrLn "3. Instrument test tracker (instrument_test_tracker.json)"
      putStrLn "Enter your choice (1-3): "
      
      testChoice <- getLine
      case testChoice of
        "1" -> loadAndRenderTrackerFile "example_tracker.json" "example_tracker.wav" 8.0
        "2" -> loadAndRenderTrackerFile "complex_example_tracker.json" "complex_example_tracker.wav" 10.0
        "3" -> loadAndRenderTrackerFile "instrument_test_tracker.json" "instrument_test_tracker.wav" 8.0
        _ -> putStrLn "Invalid choice."
      
      trackerMenu
      
    "7" -> return ()
    
    _ -> do
      putStrLn "Invalid choice. Please try again."
      trackerMenu