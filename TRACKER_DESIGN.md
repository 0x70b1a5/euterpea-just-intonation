# Just Intonation Tracker Design

This document outlines the design for a DOS-style tracker interface for creating Just Intonation music, integrated with our existing Euterpea-based system.

## Overview

We'll create a tracker file format (JSON-based) and parser that can convert tracker data into Euterpea's `Music (Double, Rational)` format for rendering to WAV files. The tracker interface seen in the HTML example will be used as a reference for the file format.

## File Format

We'll use a JSON-based format for the tracker files, which will contain:

### Top-level metadata
```json
{
  "baseFrequency": 440.0,
  "baseTempo": 120.0,
  "rowsPerBeat": 4,
  "numRows": 32,
  "numChannels": 4,
  "data": [
    // Array of rows
  ]
}
```

### Row data
Each row will contain:
```json
{
  "tempo": {
    "inputString": "120", 
    "value": 120.0
  },
  "channels": [
    {
      "note": {
        "inputString": "440", 
        "frequency": 440.0
      },
      "instrument": "sin",
      "volume": 0.8,
      "effect": {
        "command": null,
        "value": null,
        "inputString": ""
      }
    },
    // More channels...
  ]
}
```

## File Parsing and Conversion

1. Parse the JSON tracker file
2. Convert tracker data to Euterpea's `Music (Double, Rational)` format
3. Render to WAV using our existing `writeJustWav` function

## New Modules

### `TrackerTypes.hs`
- Define the data structures for the tracker file format
- Include types for Rows, Channels, Notes, etc.

### `TrackerParser.hs`
- Handle JSON parsing and validation
- Provide functions to read tracker files

### `TrackerToMusic.hs`
- Convert tracker data to Euterpea's `Music (Double, Rational)`
- Handle timing, duration, and parallel voices

### `TrackerMain.hs`
- Add tracker functionality to the main application
- Provide commands for loading/rendering tracker files

## Haskell Data Types

### Main Tracker Types
```haskell
-- Top-level tracker file
data TrackerFile = TrackerFile
  { baseFrequency :: Double
  , baseTempo :: Double
  , rowsPerBeat :: Int
  , numRows :: Int 
  , numChannels :: Int
  , trackerData :: [TrackerRow]
  }

-- Individual row
data TrackerRow = TrackerRow
  { rowTempo :: Maybe TempoData
  , rowChannels :: [ChannelData]
  }

-- Tempo information
data TempoData = TempoData
  { tempoInput :: String
  , tempoValue :: Maybe Double
  }

-- Channel (each column group)
data ChannelData = ChannelData
  { channelNote :: Maybe NoteData
  , channelInstrument :: String
  , channelVolume :: Double
  , channelEffect :: EffectData
  }

-- Note information
data NoteData = NoteData
  { noteInput :: String
  , noteFrequency :: Maybe Double
  }

-- Effect information
data EffectData = EffectData
  { effectCommand :: Maybe Char
  , effectValue :: Maybe String
  , effectInput :: String
  }
```

## Main Functions

### Loading and Parsing
```haskell
-- Parse tracker file from JSON
parseTrackerFile :: FilePath -> IO (Either String TrackerFile)

-- Load tracker file and convert to Music
loadTrackerFile :: FilePath -> IO (Either String (Music (Double, Rational)))
```

### Converting to Euterpea
```haskell
-- Convert tracker file to Music
trackerToMusic :: TrackerFile -> Music (Double, Rational)

-- Get duration for a row based on tempo
getRowDuration :: Double -> Int -> Rational
```

### Rendering
```haskell
-- Load and render tracker file to WAV
renderTrackerFile :: FilePath -> FilePath -> IO ()
```

## CLI Integration

We'll add new options to our CLI interface:
```
6. Load tracker file and render to WAV
7. Display tracker file information
```

## Future Improvements

1. Add a simple TUI (Text User Interface) for editing tracker files
2. Support more instruments using different waveforms
3. Add more effect commands (arpeggio, vibrato, etc.)
4. Allow importing/exporting to other tracker formats
5. Develop a standalone GUI tracker application

## Implementation Plan

1. Create the basic data types and parsers
2. Implement conversion to Euterpea's music format
3. Add CLI commands for loading and rendering tracker files
4. Test with example tracker files
5. Add more advanced features (effects, instruments)