# Just Intonation Tracker Design

This document outlines the design and implementation of our DOS-style tracker interface for creating Just Intonation music, integrated with our existing Euterpea-based system.

## Overview

We've created a terminal-based tracker interface that allows users to create and edit music using just intonation ratios. The application includes a full-featured editing environment with a grid-based display, keyboard shortcuts, and real-time feedback.

## Terminal Tracker Features

### User Interface
- Terminal-based color interface with cursor navigation
- Row and column headers for easy orientation
- Status bar for displaying messages and feedback
- Keyboard shortcut bar for quick reference
- Visual highlighting of the current cell and row

### Editing Capabilities
- Direct frequency input (e.g., "440" for A4)
- Ratio-based input (e.g., "3:2" for a perfect fifth)
- Support for different instrument types (sine, square, sawtooth, triangle)
- Volume control (0-100%)
- Effect commands (arpeggio, transpose)
- Adding and deleting rows
- Saving and loading tracker files
- Exporting to WAV for playback

### Navigation
- Arrow key navigation
- Vim-style hjkl movement
- Keyboard shortcuts for jumping to different field types (tempo, note, instrument, etc.)
- Row-based organization with multiple channels per row

### Special Functions
- Frequency and tempo capture for ratio referencing
- Help screen with detailed instructions
- File management (save, load, export)

## File Format

We use a JSON-based format for the tracker files, which contains:

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
Each row contains:
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

## Implementation Structure

### Modules
- **TrackerTypes.hs**: Data structures for the tracker file format
- **TrackerParser.hs**: JSON parsing and validation
- **TrackerToMusic.hs**: Conversion to Euterpea's Music format
- **TrackerMain.hs**: Terminal interface and main functionality

### Main Data Types
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
  , noteRatio :: Maybe (Ratio Integer)
  }

-- Effect information
data EffectData = EffectData
  { effectCommand :: Maybe Char
  , effectValue :: Maybe String
  , effectInput :: String
  }

-- Terminal UI state
data TrackerState = TrackerState {
  trackerFile :: TrackerFile,
  cursorPos :: CursorPos,
  editBuffer :: String,
  isEditing :: Bool,
  statusMessage :: String,
  capturedFreq :: Double,
  capturedTempo :: Double,
  filePath :: Maybe FilePath
}
```

### Core Functions

#### File Operations
```haskell
-- Parse tracker file from JSON
parseTrackerFile :: FilePath -> IO (Either String TrackerFile)

-- Load tracker file and convert to Music
loadTrackerFile :: FilePath -> IO (Either String (Music (Double, Rational)))

-- Save tracker file to disk
writeTrackerFile :: FilePath -> TrackerFile -> IO ()
```

#### Music Conversion
```haskell
-- Convert tracker file to Music
trackerToMusic :: TrackerFile -> Music (Double, Rational)

-- Render tracker file to WAV
renderTrackerFile :: TrackerFile -> FilePath -> Double -> IO ()
```

#### Terminal UI
```haskell
-- Initialize the tracker state
initTrackerState :: TrackerFile -> Maybe FilePath -> TrackerState

-- Main tracker loop
trackerLoop :: TrackerState -> IO ()

-- Draw the tracker grid
drawTrackerGrid :: TrackerState -> IO ()

-- Handle user input
handleTrackerInput :: TrackerState -> IO ()
```

## Keyboard Shortcuts

```
Navigation:
  Arrow keys / hjkl - Move cursor
  t - Jump to tempo field
  n - Jump to note field
  i - Jump to instrument field
  v - Jump to volume field
  f - Jump to effect field

Editing:
  Enter - Start/finish editing
  Esc - Cancel edit
  Backspace - Delete character

Selection and Clipboard:
  Shift+Arrow keys - Select multiple cells
  Ctrl+C - Copy selected cells
  Ctrl+V - Paste copied cells at cursor position

Actions:
  s - Save tracker file
  e - Export to WAV
  c - Capture frequency/tempo value
  a - Add row at current position
  d - Delete current row
  q - Quit
  ? / h - Show help
```

## Input Formats

- **Notes**: Direct frequency (e.g., "440") or ratio (e.g., "3:2")
- **Tempo**: Direct BPM (e.g., "120") or ratio (e.g., "3:2")
- **Instruments**: 'sin' (sine), 'sqr' (square), 'saw' (sawtooth), 'tri' (triangle)
- **Volume**: 0-100
- **Effects**: First character is command (e.g., 'A' for arpeggio), rest is value

## Menu Integration

From the main application menu, users can:
1. Select "Open Tracker Interface" (option 5)
2. Choose from several tracker operations:
   - Open terminal tracker with new file
   - Open terminal tracker with existing file
   - Load and render tracker file
   - Display tracker file information
   - Create example tracker file
   - Export tracker file to CSV
   - Load and render example files

## Future Improvements

1. Add more effect types (vibrato, delay, etc.)
2. Support for longer patterns with pattern repeat markers
3. Multiple pattern support
4. Pattern arrangement editing
5. Implement MIDI import/export
6. Add in-app playback with position tracking
7. Additional waveform types and filter options