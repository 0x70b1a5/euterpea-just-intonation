module TrackerTypes where

import Data.Ratio

-- | Main tracker file structure
data TrackerFile = TrackerFile
  { baseFrequency :: Double          -- ^ Base frequency in Hz (typically 440Hz)
  , baseTempo :: Double              -- ^ Base tempo in BPM (typically 120)
  , rowsPerBeat :: Int               -- ^ Number of rows per beat
  , numRows :: Int                   -- ^ Total number of rows
  , numChannels :: Int               -- ^ Number of channels/voices
  , trackerData :: [TrackerRow]      -- ^ Actual tracker pattern data
  } deriving (Show)

-- | Individual row in the tracker
data TrackerRow = TrackerRow
  { rowTempo :: Maybe TempoData      -- ^ Tempo for this row (if specified)
  , rowChannels :: [ChannelData]     -- ^ Channel data for this row
  } deriving (Show)

-- | Tempo information
data TempoData = TempoData
  { tempoInput :: String            -- ^ Raw tempo input string
  , tempoValue :: Maybe Double      -- ^ Calculated tempo value in BPM
  } deriving (Show)

-- | Channel data (represents one voice at one step)
data ChannelData = ChannelData
  { channelNote :: Maybe NoteData   -- ^ Note information (if any)
  , channelInstrument :: String     -- ^ Instrument type (default: "sin")
  , channelVolume :: Double         -- ^ Volume (0.0-1.0)
  , channelEffect :: EffectData     -- ^ Effect information
  } deriving (Show)

-- | Note information
data NoteData = NoteData
  { noteInput :: String              -- ^ Raw note input string
  , noteFrequency :: Maybe Double    -- ^ Calculated frequency in Hz
  , noteRatio :: Maybe (Ratio Integer) -- ^ Ratio from base (if applicable)
  } deriving (Show)

-- | Effect information
data EffectData = EffectData
  { effectCommand :: Maybe Char      -- ^ Effect command (single character)
  , effectValue :: Maybe String      -- ^ Effect parameter value
  , effectInput :: String            -- ^ Raw effect input string
  } deriving (Show)

-- | Instrument types supported by the tracker
data InstrumentType = 
    Sine      -- ^ Sine wave (pure tone)
  | Square    -- ^ Square wave 
  | Sawtooth  -- ^ Sawtooth wave
  | Triangle  -- ^ Triangle wave
  deriving (Show, Eq)

-- | Convert string to instrument type
stringToInstrument :: String -> Maybe InstrumentType
stringToInstrument "sin" = Just Sine
stringToInstrument "sqr" = Just Square
stringToInstrument "saw" = Just Sawtooth
stringToInstrument "tri" = Just Triangle
stringToInstrument _ = Nothing

-- | Convert instrument type to string
instrumentToString :: InstrumentType -> String
instrumentToString Sine = "sin"
instrumentToString Square = "sqr"
instrumentToString Sawtooth = "saw"
instrumentToString Triangle = "tri"

-- | Default values for a new tracker file
defaultTrackerFile :: TrackerFile
defaultTrackerFile = 
  let emptyRows = replicate 32 (emptyTrackerRow 4)
  in TrackerFile
    { baseFrequency = 440.0
    , baseTempo = 120.0
    , rowsPerBeat = 4
    , numRows = 32
    , numChannels = 4
    , trackerData = emptyRows
    }

-- | Create an empty tracker row
emptyTrackerRow :: Int -> TrackerRow
emptyTrackerRow channels = TrackerRow
  { rowTempo = Nothing
  , rowChannels = replicate channels emptyChannelData
  }

-- | Create empty channel data
emptyChannelData :: ChannelData
emptyChannelData = ChannelData
  { channelNote = Nothing
  , channelInstrument = "sin"
  , channelVolume = 1.0
  , channelEffect = emptyEffectData
  }

-- | Create empty effect data
emptyEffectData :: EffectData
emptyEffectData = EffectData
  { effectCommand = Nothing
  , effectValue = Nothing
  , effectInput = ""
  }

-- | Initialize a new tracker file with empty rows
initTrackerFile :: Double -> Double -> Int -> Int -> Int -> TrackerFile
initTrackerFile baseFreq baseBpm rowsPerB numR numC = 
  let emptyRows = replicate numR (emptyTrackerRow numC)
  in TrackerFile
     { baseFrequency = baseFreq
     , baseTempo = baseBpm
     , rowsPerBeat = rowsPerB
     , numRows = numR
     , numChannels = numC
     , trackerData = emptyRows
     }