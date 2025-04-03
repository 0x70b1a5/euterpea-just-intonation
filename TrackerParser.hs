{-# LANGUAGE OverloadedStrings #-}

module TrackerParser where

import Control.Monad (forM)
import Data.Aeson
import Data.Aeson.Types
import Data.Ratio
import Data.Maybe (fromMaybe, catMaybes)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import System.IO
import TrackerTypes

-- | Parse ratio from string (format: "n:m")
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

-- | Parse note input (either direct frequency or ratio)
parseNoteInput :: String -> Double -> Maybe (Double, Maybe (Ratio Integer))
parseNoteInput input baseFreq
  | null input = Nothing
  -- Try direct frequency input
  | all (\c -> isDigit c || c == '.') input = 
      let freq = read input :: Double
      in if freq > 0 then Just (freq, Nothing) else Nothing
  -- Try ratio input (n:m format)
  | otherwise = case parseRatio input of
      Just r -> Just (baseFreq * (fromRational $ toRational r), Just r)
      Nothing -> Nothing
  where
    isDigit c = c >= '0' && c <= '9'

-- | Parse tempo input (either direct BPM or ratio)
parseTempo :: String -> Double -> Maybe Double
parseTempo input baseTempo
  | null input = Nothing
  -- Try direct BPM input
  | all (\c -> isDigit c || c == '.') input = 
      let bpm = read input :: Double
      in if bpm >= 30 && bpm <= 300 then Just bpm else Nothing
  -- Try ratio input (n:m format)
  | otherwise = case parseRatio input of
      Just r -> 
        let calculated = baseTempo * (fromRational $ toRational r)
        in if calculated >= 30 && calculated <= 300 
           then Just calculated 
           else Nothing
      Nothing -> Nothing
  where
    isDigit c = c >= '0' && c <= '9'

-- | JSON instances for TrackerFile
instance FromJSON TrackerFile where
  parseJSON = withObject "TrackerFile" $ \v -> TrackerFile
    <$> v .: "baseFrequency"
    <*> v .: "baseTempo"
    <*> v .: "rowsPerBeat"
    <*> v .: "numRows"
    <*> v .: "numChannels"
    <*> v .: "data"

instance ToJSON TrackerFile where
  toJSON tf = object
    [ "baseFrequency" .= baseFrequency tf
    , "baseTempo" .= baseTempo tf
    , "rowsPerBeat" .= rowsPerBeat tf
    , "numRows" .= numRows tf
    , "numChannels" .= numChannels tf
    , "data" .= trackerData tf
    ]

-- | JSON instances for TrackerRow
instance FromJSON TrackerRow where
  parseJSON = withObject "TrackerRow" $ \v -> TrackerRow
    <$> v .:? "tempo"
    <*> v .: "channels"

instance ToJSON TrackerRow where
  toJSON row = object
    [ "tempo" .= rowTempo row
    , "channels" .= rowChannels row
    ]

-- | JSON instances for TempoData
instance FromJSON TempoData where
  parseJSON = withObject "TempoData" $ \v -> TempoData
    <$> v .: "inputString"
    <*> v .:? "value"

instance ToJSON TempoData where
  toJSON td = object
    [ "inputString" .= tempoInput td
    , "value" .= tempoValue td
    ]

-- | JSON instances for ChannelData
instance FromJSON ChannelData where
  parseJSON = withObject "ChannelData" $ \v -> ChannelData
    <$> v .:? "note"
    <*> v .:? "instrument" .!= "sin"
    <*> v .:? "volume" .!= 1.0
    <*> v .:? "effect" .!= emptyEffectData

instance ToJSON ChannelData where
  toJSON cd = object
    [ "note" .= channelNote cd
    , "instrument" .= channelInstrument cd
    , "volume" .= channelVolume cd
    , "effect" .= channelEffect cd
    ]

-- | JSON instances for NoteData
instance FromJSON NoteData where
  parseJSON = withObject "NoteData" $ \v -> NoteData
    <$> v .: "inputString"
    <*> v .:? "frequency"
    <*> pure Nothing  -- We'll calculate the ratio when processing

instance ToJSON NoteData where
  toJSON nd = object
    [ "inputString" .= noteInput nd
    , "frequency" .= noteFrequency nd
    , "ratio" .= (show <$> noteRatio nd)
    ]

-- | JSON instances for EffectData
instance FromJSON EffectData where
  parseJSON = withObject "EffectData" $ \v -> EffectData
    <$> (fmap T.head <$> (v .:? "command"))
    <*> v .:? "value"
    <*> v .:? "inputString" .!= ""

instance ToJSON EffectData where
  toJSON ed = object
    [ "command" .= (T.singleton <$> effectCommand ed)
    , "value" .= effectValue ed
    , "inputString" .= effectInput ed
    ]

-- | Process a parsed tracker file to calculate all derived values
processTrackerFile :: TrackerFile -> TrackerFile
processTrackerFile tf = 
  tf { trackerData = map (processRow (baseFrequency tf) (baseTempo tf)) (trackerData tf) }

-- | Process a single row
processRow :: Double -> Double -> TrackerRow -> TrackerRow
processRow baseFreq baseBpm row = 
  let 
    -- Process tempo if present
    processedTempo = case rowTempo row of
      Just t -> Just $ processTempo baseBpm t
      Nothing -> Nothing
    
    -- Process all channels
    processedChannels = map (processChannel baseFreq) (rowChannels row)
  in
    row { rowTempo = processedTempo, rowChannels = processedChannels }

-- | Process tempo data
processTempo :: Double -> TempoData -> TempoData
processTempo baseBpm tempo = 
  let calculatedValue = parseTempo (tempoInput tempo) baseBpm
  in tempo { tempoValue = calculatedValue }

-- | Process channel data
processChannel :: Double -> ChannelData -> ChannelData
processChannel baseFreq channel = 
  let 
    -- Process note if present
    processedNote = case channelNote channel of
      Just n -> Just $ processNote baseFreq n
      Nothing -> Nothing
  in
    channel { channelNote = processedNote }

-- | Process note data
processNote :: Double -> NoteData -> NoteData
processNote baseFreq note = 
  case parseNoteInput (noteInput note) baseFreq of
    Just (freq, ratio) -> note { noteFrequency = Just freq, noteRatio = ratio }
    Nothing -> note

-- | Read and parse a tracker file
readTrackerFile :: FilePath -> IO (Either String TrackerFile)
readTrackerFile filePath = do
  fileExists <- doesFileExist filePath
  if not fileExists
    then return (Left $ "File not found: " ++ filePath)
    else do
      contents <- BL.readFile filePath
      case eitherDecode contents of
        Left err -> return (Left $ "JSON parsing error: " ++ err)
        Right tf -> return (Right $ processTrackerFile tf)

-- | Write a tracker file to disk
writeTrackerFile :: FilePath -> TrackerFile -> IO ()
writeTrackerFile filePath tf = do
  BL.writeFile filePath (encode tf)
  putStrLn $ "Tracker file written to: " ++ filePath

-- | Check if file exists
doesFileExist :: FilePath -> IO Bool
doesFileExist filePath = do
  result <- try (openFile filePath ReadMode) :: IO (Either IOError Handle)
  case result of
    Left _ -> return False
    Right handle -> do
      hClose handle
      return True

-- | Create a CSV export of a tracker file (for easier viewing/editing)
exportTrackerCsv :: TrackerFile -> FilePath -> IO ()
exportTrackerCsv tf filePath = do
  let rows = trackerData tf
  let header = "Row,Tempo," ++ concatMap (\c -> "Ch" ++ show c ++ " Note,Ch" ++ show c ++ " Inst,Ch" ++ show c ++ " Vol,Ch" ++ show c ++ " Fx,") [1..numChannels tf]
  let csvRows = map (rowToCsv (numChannels tf)) (zip [0..] rows)
  writeFile filePath (header ++ "\n" ++ unlines csvRows)
  putStrLn $ "CSV file written to: " ++ filePath

-- | Convert a row to CSV format
rowToCsv :: Int -> (Int, TrackerRow) -> String
rowToCsv numChans (rowNum, row) = 
  let 
    -- Row number and tempo
    tempoStr = case rowTempo row of
      Just t -> fromMaybe "" (tempoInput t <$ tempoValue t)
      Nothing -> ""
    
    -- Channel data
    chanData = rowChannels row
    padded = take numChans (chanData ++ repeat emptyChannelData)
    
    -- Convert each channel to CSV cells
    chanCells = concatMap channelToCsv padded
  in
    show rowNum ++ "," ++ tempoStr ++ "," ++ chanCells

-- | Convert channel data to CSV cells
channelToCsv :: ChannelData -> String
channelToCsv channel = 
  let 
    noteStr = case channelNote channel of
      Just n -> fromMaybe "" (noteInput n <$ noteFrequency n)
      Nothing -> ""
    
    instStr = channelInstrument channel
    volStr = show (channelVolume channel * 100)
    
    fxStr = case effectCommand (channelEffect channel) of
      Just cmd -> [cmd] ++ fromMaybe "" (effectValue (channelEffect channel))
      Nothing -> ""
  in
    noteStr ++ "," ++ instStr ++ "," ++ volStr ++ "," ++ fxStr ++ ","

-- | Create a new empty tracker file
createEmptyTrackerFile :: FilePath -> Double -> Double -> Int -> Int -> Int -> IO ()
createEmptyTrackerFile filePath baseFreq baseBpm rowsPerB numRows numChans = do
  let tf = initTrackerFile baseFreq baseBpm rowsPerB numRows numChans
  writeTrackerFile filePath tf
  putStrLn $ "Created empty tracker file: " ++ filePath