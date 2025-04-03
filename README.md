# Just Intonation Music with Euterpea

This project extends Euterpea with Just Intonation capabilities, allowing you to compose and render music with pure frequency ratios instead of equal temperament. It now includes a tracker-style interface for creating music similar to classic DOS tracker programs.

## Getting Started

### Prerequisites

- GHC (Glasgow Haskell Compiler)
- Cabal (Haskell package manager)
- SDL2 and SDL2_ttf libraries (for the SDL-based tracker interface)

### Installation

1. Clone this repository
2. Install SDL dependencies:
   ```
   ./install_sdl_deps.sh
   ```
3. Run `cabal build` to compile the project

## Just Intonation Basics

Just Intonation uses pure mathematical ratios to define musical intervals, unlike equal temperament which divides the octave into 12 equal parts. This results in harmonically pure sounds that can be particularly striking and resonant.

### Available Intervals

This extension provides the following intervals:

- `unison`: 1/1 (same pitch)
- `majorThird`: 5/4 
- `minorThird`: 6/5
- `perfectFourth`: 4/3
- `perfectFifth`: 3/2
- `majorSixth`: 5/3
- `minorSixth`: 8/5
- `octave`: 2/1

## How to Compose Music

### Basic Notes

To create a note with Just Intonation:

```haskell
-- Create a Just Intonation pitch at 440Hz (A4) using the unison ratio (1/1)
a4 = Prim (Note qn (j 440 unison))

-- Create a note at a perfect fifth above A4 (E5)
e5 = Prim (Note qn (j 440 perfectFifth))

-- Create a C# using the major third ratio
cSharp = Prim (Note qn (j 440 majorThird))

-- Create a note with a different duration (half note)
longA4 = Prim (Note hn (j 440 unison))
```

### Note Durations

Standard note durations:
- `wn`: whole note
- `hn`: half note
- `qn`: quarter note
- `en`: eighth note
- `sn`: sixteenth note
- `tn`: thirty-second note

Dotted versions (1.5× the duration):
- `dwn`: dotted whole note
- `dhn`: dotted half note
- `dqn`: dotted quarter note
- etc.

### Composing Melodies

You can compose sequential melodies using the `:+:` operator or the `line` function:

```haskell
-- A simple melody with four notes in sequence
myMelody = 
  Prim (Note qn (j 440 unison)) :+: 
  Prim (Note qn (j 440 majorThird)) :+:
  Prim (Note qn (j 440 perfectFifth)) :+:
  Prim (Note qn (j 440 octave))

-- The same melody using the line function
myMelody2 = line [
  Prim (Note qn (j 440 unison)),
  Prim (Note qn (j 440 majorThird)),
  Prim (Note qn (j 440 perfectFifth)), 
  Prim (Note qn (j 440 octave))
]
```

### Creating Chords

You can create chords using the `:=:` operator or the `chord` function:

```haskell
-- A major chord (root, major third, perfect fifth)
majorChord = 
  Prim (Note hn (j 440 unison)) :=: 
  Prim (Note hn (j 440 majorThird)) :=:
  Prim (Note hn (j 440 perfectFifth))

-- The same chord using the chord function
majorChord2 = chord [
  Prim (Note hn (j 440 unison)),
  Prim (Note hn (j 440 majorThird)),
  Prim (Note hn (j 440 perfectFifth))
]
```

### Complex Music

You can combine sequential and parallel composition to create more complex music:

```haskell
-- A melody followed by a chord
melodyThenChord = 
  line [
    Prim (Note qn (j 440 unison)),
    Prim (Note qn (j 440 majorThird)),
    Prim (Note qn (j 440 perfectFifth))
  ] :+: 
  chord [
    Prim (Note hn (j 440 unison)),
    Prim (Note hn (j 440 majorThird)),
    Prim (Note hn (j 440 perfectFifth))
  ]
```

## Generating Audio Files

To generate a WAV file from your composition:

```haskell
main :: IO ()
main = do
  putStrLn "Writing music to WAV file..."
  writeJustWav "mymusic.wav" 4.0 myMelody
```

The `writeJustWav` function takes:
1. A file path for the output WAV file
2. The duration in seconds
3. The music to render

## Example Compositions

### Simple Scale

```haskell
justScale :: Music (Double, Rational)
justScale =
  line
    [ Prim (Note qn (j 440 unison)),       -- A4
      Prim (Note qn (j 440 (9 % 8))),      -- B4 (9/8 ratio)
      Prim (Note qn (j 440 (5 % 4))),      -- C#5
      Prim (Note qn (j 440 (4 % 3))),      -- D5
      Prim (Note qn (j 440 (3 % 2))),      -- E5
      Prim (Note qn (j 440 (5 % 3))),      -- F#5
      Prim (Note qn (j 440 (15 % 8))),     -- G#5 
      Prim (Note qn (j 440 (2 % 1)))       -- A5 (octave)
    ]
```

### Major Chord Arpeggio

```haskell
majorArpeggio :: Music (Double, Rational)
majorArpeggio =
  line
    [ Prim (Note qn (j 440 unison)),      -- Root
      Prim (Note qn (j 440 majorThird)),  -- Major third
      Prim (Note qn (j 440 perfectFifth)), -- Perfect fifth
      Prim (Note qn (j 440 octave)),      -- Octave
      Prim (Note qn (j 440 perfectFifth)), -- Back down to fifth
      Prim (Note qn (j 440 majorThird)),  -- Back down to third
      Prim (Note qn (j 440 unison))       -- Back to root
    ]
```

### Tips for Non-Technical Users

1. Start by copying and modifying the example melodies
2. Experiment with different ratios to create new intervals
3. Try changing note durations to create rhythmic variations
4. Combine sequential (:+:) and parallel (:=:) composition
5. Use the `line` and `chord` functions for cleaner code

## Creating Custom Intervals

You can create custom intervals by defining new ratios:

```haskell
-- Define a septimal minor third (7/6 ratio)
septimalMinorThird :: Ratio Integer
septimalMinorThird = 7 % 6

-- Define a harmonic seventh (7/4 ratio)
harmonicSeventh :: Ratio Integer
harmonicSeventh = 7 % 4

-- Use custom intervals in a melody
customMelody = line [
  Prim (Note qn (j 440 unison)),
  Prim (Note qn (j 440 septimalMinorThird)),
  Prim (Note qn (j 440 harmonicSeventh))
]
```

## Running Your Compositions

To run your composition:

1. Save your music in the `Main.hs` file or import it from another module
2. Make sure your `main` function calls `writeJustWav` with your composition
3. Run `cabal run` from the project directory
4. Play the resulting WAV file with any media player

## Using the Tracker Interface

The Just Intonation Tracker provides two interface options:
1. A terminal-based interface similar to classic DOS trackers
2. An SDL-based graphical interface for a more modern experience

Both allow you to create music using a grid-based system, where:
- Rows represent time steps
- Columns represent different voices/instruments
- Each cell can contain a note frequency or ratio

To use the tracker:

1. Run the application with `cabal run`
2. Select option 5 for the terminal tracker or option 6 for the SDL tracker
3. For more options, access the Tracker Menu by choosing option 5, which includes:
   - Open terminal tracker with new file
   - Open terminal tracker with existing file
   - Open SDL tracker with new file
   - Open SDL tracker with existing file 
   - Load and render an existing tracker file
   - Display information about a tracker file
   - Create an example tracker file
   - Export a tracker file to CSV for editing in a spreadsheet

### Tracker Controls

Both the terminal and SDL trackers support similar keyboard shortcuts:

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

### Entering Notes and Values

You can enter different types of values in the tracker:

- **Notes**: Direct frequency (e.g., "440") or ratio (e.g., "3:2")
- **Tempo**: Direct BPM (e.g., "120") or ratio (e.g., "3:2")
- **Instruments**: 'sin' (sine), 'sqr' (square), 'saw' (sawtooth), 'tri' (triangle)
- **Volume**: 0-100
- **Effects**: First character is command (e.g., 'A' for arpeggio), rest is value

The "Capture" function (c key) allows you to store a current frequency or tempo, which serves as the reference point for future ratio inputs, making it easy to maintain harmonic relationships.

### Tracker File Format

Tracker files use a JSON format with the following structure:

```json
{
  "baseFrequency": 440.0,
  "baseTempo": 120.0,
  "rowsPerBeat": 4,
  "numRows": 32,
  "numChannels": 4,
  "data": [
    {
      "tempo": {"inputString": "120", "value": 120.0},
      "channels": [
        {
          "note": {"inputString": "440", "frequency": 440.0},
          "instrument": "sin",
          "volume": 1.0,
          "effect": {"command": null, "value": null, "inputString": ""}
        },
        // More channels...
      ]
    },
    // More rows...
  ]
}
```

### Working with Example Files

You can load and render example tracker files from the tracker menu:
1. From the main menu, select option 5 "Open Tracker interface"
2. From the tracker menu, select option 7 "Load and render test files"
3. Choose from the available test files:
   - Basic example tracker (simple melody and chord)
   - Complex example tracker (various instruments, effects, and tempo changes)
   - Instrument test tracker (comparison of different instrument types)
   - User example tracker (your own saved compositions)

The tracker system currently supports:
- Four instrument types: sine (sin), square (sqr), sawtooth (saw), and triangle (tri)
- Volume control (0-100%)
- Tempo changes (direct BPM or ratios)
- Basic effects (arpeggio, transpose)
- Adding and deleting rows
- Saving and loading tracker files

Enjoy creating music with pure ratios and have fun exploring the world of Just Intonation!