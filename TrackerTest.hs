module TrackerTest where

import Data.Ratio
import Data.Maybe (fromMaybe, isJust)
import Euterpea
import Main hiding (main)
import TrackerTypes
import TrackerParser
import TrackerToMusic

-- | Test harness for tracker functionality
runTrackerTests :: IO ()
runTrackerTests = do
  putStrLn "Running Tracker Backend Tests"
  putStrLn "============================="
  
  -- Test 1: Basic note parsing
  testNoteParsingBasic
  
  -- Test 2: Ratio parsing
  testRatioParsing
  
  -- Test 3: Tempo parsing
  testTempoParsing
  
  -- Test 4: Instrument handling
  testInstrumentHandling
  
  -- Test 5: Volume handling
  testVolumeHandling
  
  -- Test 6: Effect handling
  testEffectHandling
  
  -- Test 7: Complete tracker file loading and conversion
  testTrackerConversion "test_scale.json"
  testTrackerConversion "test_chord.json"
  testTrackerConversion "test_arpeggio.json"
  
  putStrLn "============================="
  putStrLn "All tests completed"

-- | Test basic note parsing functionality
testNoteParsingBasic :: IO ()
testNoteParsingBasic = do
  putStrLn "\nTest 1: Basic Note Parsing"
  
  let baseFreq = 440.0
      -- Test direct frequency inputs
      test1 = parseNoteInput "440" baseFreq
      test2 = parseNoteInput "880" baseFreq
      test3 = parseNoteInput "0" baseFreq -- Invalid
      test4 = parseNoteInput "" baseFreq -- Empty
  
  putStrLn $ "Input: \"440\" -> " ++ formatNoteResult test1
  putStrLn $ "Input: \"880\" -> " ++ formatNoteResult test2
  putStrLn $ "Input: \"0\" -> " ++ formatNoteResult test3
  putStrLn $ "Input: \"\" -> " ++ formatNoteResult test4
  
  where
    formatNoteResult Nothing = "Invalid"
    formatNoteResult (Just (freq, _)) = "Valid: " ++ show freq ++ " Hz"

-- | Test ratio parsing functionality
testRatioParsing :: IO ()
testRatioParsing = do
  putStrLn "\nTest 2: Ratio Parsing"
  
  let baseFreq = 440.0
      -- Test ratio inputs
      test1 = parseNoteInput "3:2" baseFreq -- Perfect fifth
      test2 = parseNoteInput "5:4" baseFreq -- Major third
      test3 = parseNoteInput "2:1" baseFreq -- Octave
      test4 = parseNoteInput "0:1" baseFreq -- Invalid
      test5 = parseNoteInput "bad:ratio" baseFreq -- Invalid
  
  putStrLn $ "Input: \"3:2\" -> " ++ formatRatioResult test1
  putStrLn $ "Input: \"5:4\" -> " ++ formatRatioResult test2
  putStrLn $ "Input: \"2:1\" -> " ++ formatRatioResult test3
  putStrLn $ "Input: \"0:1\" -> " ++ formatRatioResult test4
  putStrLn $ "Input: \"bad:ratio\" -> " ++ formatRatioResult test5
  
  where
    formatRatioResult Nothing = "Invalid"
    formatRatioResult (Just (freq, Just ratio)) = 
      "Valid: " ++ show freq ++ " Hz (ratio " ++ show ratio ++ ")"
    formatRatioResult (Just (freq, Nothing)) = 
      "Valid: " ++ show freq ++ " Hz (no ratio)"

-- | Test tempo parsing functionality
testTempoParsing :: IO ()
testTempoParsing = do
  putStrLn "\nTest 3: Tempo Parsing"
  
  let baseTempo = 120.0
      -- Test tempo inputs
      test1 = parseTempo "120" baseTempo
      test2 = parseTempo "240" baseTempo
      test3 = parseTempo "3:2" baseTempo
      test4 = parseTempo "25" baseTempo -- Invalid (<30)
      test5 = parseTempo "350" baseTempo -- Invalid (>300)
      test6 = parseTempo "bad" baseTempo -- Invalid format
  
  putStrLn $ "Input: \"120\" -> " ++ formatTempoResult test1
  putStrLn $ "Input: \"240\" -> " ++ formatTempoResult test2
  putStrLn $ "Input: \"3:2\" -> " ++ formatTempoResult test3
  putStrLn $ "Input: \"25\" -> " ++ formatTempoResult test4
  putStrLn $ "Input: \"350\" -> " ++ formatTempoResult test5
  putStrLn $ "Input: \"bad\" -> " ++ formatTempoResult test6
  
  where
    formatTempoResult Nothing = "Invalid"
    formatTempoResult (Just bpm) = "Valid: " ++ show bpm ++ " BPM"

-- | Test instrument handling
testInstrumentHandling :: IO ()
testInstrumentHandling = do
  putStrLn "\nTest 4: Instrument Handling"
  
  let test1 = stringToInstrument "sin"
      test2 = stringToInstrument "sqr"
      test3 = stringToInstrument "saw"
      test4 = stringToInstrument "tri"
      test5 = stringToInstrument "invalid"
  
  putStrLn $ "Input: \"sin\" -> " ++ formatInstrumentResult test1
  putStrLn $ "Input: \"sqr\" -> " ++ formatInstrumentResult test2
  putStrLn $ "Input: \"saw\" -> " ++ formatInstrumentResult test3
  putStrLn $ "Input: \"tri\" -> " ++ formatInstrumentResult test4
  putStrLn $ "Input: \"invalid\" -> " ++ formatInstrumentResult test5
  
  where
    formatInstrumentResult Nothing = "Invalid"
    formatInstrumentResult (Just inst) = "Valid: " ++ show inst

-- | Test volume handling
testVolumeHandling :: IO ()
testVolumeHandling = do
  putStrLn "\nTest 5: Volume Handling"
  
  -- Test volume inputs (simulated from TrackerParser)
  let normalizeVolume :: String -> Maybe Double
      normalizeVolume input = 
        case reads input :: [(Int, String)] of
          [(val, "")] | val >= 0 && val <= 100 -> Just (fromIntegral val / 100.0)
          _ -> Nothing
      
      test1 = normalizeVolume "100"
      test2 = normalizeVolume "50"
      test3 = normalizeVolume "0"
      test4 = normalizeVolume "101" -- Invalid (>100)
      test5 = normalizeVolume "-1" -- Invalid (<0)
      test6 = normalizeVolume "bad" -- Invalid format
  
  putStrLn $ "Input: \"100\" -> " ++ formatVolumeResult test1
  putStrLn $ "Input: \"50\" -> " ++ formatVolumeResult test2
  putStrLn $ "Input: \"0\" -> " ++ formatVolumeResult test3
  putStrLn $ "Input: \"101\" -> " ++ formatVolumeResult test4
  putStrLn $ "Input: \"-1\" -> " ++ formatVolumeResult test5
  putStrLn $ "Input: \"bad\" -> " ++ formatVolumeResult test6
  
  where
    formatVolumeResult Nothing = "Invalid"
    formatVolumeResult (Just vol) = "Valid: " ++ show vol

-- | Test effect handling
testEffectHandling :: IO ()
testEffectHandling = do
  putStrLn "\nTest 6: Effect Handling"
  
  -- Test basic effect parsing (simplified for testing)
  let parseEffect :: String -> Maybe (Char, String)
      parseEffect "" = Nothing
      parseEffect (cmd:val) = Just (cmd, val)
      
      test1 = parseEffect "A"
      test2 = parseEffect "T5:4"
      test3 = parseEffect "V50"
      test4 = parseEffect ""
  
  putStrLn $ "Input: \"A\" -> " ++ formatEffectResult test1
  putStrLn $ "Input: \"T5:4\" -> " ++ formatEffectResult test2
  putStrLn $ "Input: \"V50\" -> " ++ formatEffectResult test3
  putStrLn $ "Input: \"\" -> " ++ formatEffectResult test4
  
  where
    formatEffectResult Nothing = "No effect"
    formatEffectResult (Just (cmd, val)) = 
      "Effect: " ++ [cmd] ++ " with value " ++ val

-- | Test complete tracker file loading and conversion
testTrackerConversion :: FilePath -> IO ()
testTrackerConversion filePath = do
  putStrLn $ "\nTest 7: Tracker Conversion - " ++ filePath
  
  -- Create the test tracker file
  createTestTrackerFile filePath
  
  -- Load and test parsing
  result <- readTrackerFile filePath
  case result of
    Left err -> putStrLn $ "Error: " ++ err
    Right tf -> do
      putStrLn "File parsed successfully"
      putStrLn $ "  Base Frequency: " ++ show (baseFrequency tf) ++ " Hz"
      putStrLn $ "  Base Tempo: " ++ show (baseTempo tf) ++ " BPM"
      putStrLn $ "  Rows per Beat: " ++ show (rowsPerBeat tf)
      putStrLn $ "  Number of Rows: " ++ show (numRows tf)
      putStrLn $ "  Number of Channels: " ++ show (numChannels tf)
      
      -- Convert to music and render
      let music = trackerToMusic tf
      let outputFile = "test_" ++ filePath ++ ".wav"
      putStrLn $ "Converting to music and rendering to " ++ outputFile
      writeJustWav outputFile 4.0 music
      putStrLn "Conversion successful"

-- | Create test tracker files for different musical patterns
createTestTrackerFile :: FilePath -> IO ()
createTestTrackerFile "test_scale.json" = do
  -- Create a scale pattern (ascending frequencies)
  let baseFreq = 440.0
      tf = initTrackerFile baseFreq 120.0 4 8 1
      
      -- Create a scale: A4, B4, C#5, D5, E5, F#5, G#5, A5
      frequencies = [
          "440",    -- A4
          "495",    -- B4 (9:8 ratio)
          "550",    -- C#5 (5:4 ratio)
          "586.67", -- D5 (4:3 ratio)
          "660",    -- E5 (3:2 ratio)
          "733.33", -- F#5 (5:3 ratio)
          "825",    -- G#5 (15:8 ratio)
          "880"     -- A5 (2:1 ratio)
        ]
      
      -- Create rows with notes
      rows = zipWith createScaleRow [0..] frequencies
      
      -- Update tracker file with rows
      updatedTf = tf { trackerData = rows }
  
  -- Write the file
  writeTrackerFile "test_scale.json" updatedTf
  
  where
    createScaleRow :: Int -> String -> TrackerRow
    createScaleRow rowIndex freq =
      let emptyRow = emptyTrackerRow 1
          -- Create note data
          note = NoteData {
            noteInput = freq,
            noteFrequency = Just (read freq :: Double),
            noteRatio = Nothing
          }
          -- Update first channel with the note
          firstChannel = (head (rowChannels emptyRow)) {
            channelNote = Just note,
            channelInstrument = "sin"
          }
      in emptyRow {
        rowTempo = if rowIndex == 0 
                     then Just (TempoData "120" (Just 120.0))
                     else Nothing,
        rowChannels = [firstChannel]
      }

createTestTrackerFile "test_chord.json" = do
  -- Create a chord pattern (A major chord: A, C#, E)
  let baseFreq = 440.0
      tf = initTrackerFile baseFreq 120.0 4 1 3
      
      -- A major chord frequencies
      frequencies = [
          "440",  -- A4
          "550",  -- C#5 (5:4 ratio)
          "660"   -- E5 (3:2 ratio)
        ]
      
      -- Create a row with the chord
      row = createChordRow frequencies
      
      -- Update tracker file with the row
      updatedTf = tf { trackerData = [row] }
  
  -- Write the file
  writeTrackerFile "test_chord.json" updatedTf
  
  where
    createChordRow :: [String] -> TrackerRow
    createChordRow freqs =
      let emptyRow = emptyTrackerRow 3
          -- Create channels with notes
          channels = zipWith createChannel freqs ["sin", "sqr", "tri"]
      in emptyRow {
        rowTempo = Just (TempoData "120" (Just 120.0)),
        rowChannels = channels
      }
    
    createChannel :: String -> String -> ChannelData
    createChannel freq inst =
      let note = NoteData {
            noteInput = freq,
            noteFrequency = Just (read freq :: Double),
            noteRatio = Nothing
          }
      in ChannelData {
        channelNote = Just note,
        channelInstrument = inst,
        channelVolume = 1.0,
        channelEffect = emptyEffectData
      }

createTestTrackerFile "test_arpeggio.json" = do
  -- Create an arpeggio pattern (A4, C#5, E5, A5, E5, C#5, A4)
  let baseFreq = 440.0
      tf = initTrackerFile baseFreq 120.0 4 7 1
      
      -- Major arpeggio frequencies
      frequencies = [
          "440",    -- A4
          "550",    -- C#5 (5:4 ratio)
          "660",    -- E5 (3:2 ratio)
          "880",    -- A5 (2:1 ratio)
          "660",    -- E5 (3:2 ratio)
          "550",    -- C#5 (5:4 ratio)
          "440"     -- A4
        ]
      
      -- Create rows with notes
      rows = zipWith createArpeggioRow [0..] frequencies
      
      -- Update tracker file with rows
      updatedTf = tf { trackerData = rows }
  
  -- Write the file
  writeTrackerFile "test_arpeggio.json" updatedTf
  
  where
    createArpeggioRow :: Int -> String -> TrackerRow
    createArpeggioRow rowIndex freq =
      let emptyRow = emptyTrackerRow 1
          -- Create note data
          note = NoteData {
            noteInput = freq,
            noteFrequency = Just (read freq :: Double),
            noteRatio = Nothing
          }
          -- Update first channel with the note
          firstChannel = (head (rowChannels emptyRow)) {
            channelNote = Just note,
            channelInstrument = "sqr"
          }
      in emptyRow {
        rowTempo = if rowIndex == 0 
                     then Just (TempoData "120" (Just 120.0))
                     else Nothing,
        rowChannels = [firstChannel]
      }

-- | Helper function to create a test tracker file with more complex patterns
createTestTrackerFile _ = error "Unknown test file template"

-- | Helper to parse a ratio from string (format: "n:m")
parseRatio :: String -> Maybe (Ratio Integer)
parseRatio str = 
  case break (==':') str of
    (numStr, ':':denomStr) -> 
      case (readMaybe numStr, readMaybe denomStr) of
        (Just n, Just d) | d > 0 -> Just (n % d)
        _ -> Nothing
    _ -> Nothing
  where 
    readMaybe :: Read a => String -> Maybe a
    readMaybe s = case reads s of
      [(val, "")] -> Just val
      _ -> Nothing