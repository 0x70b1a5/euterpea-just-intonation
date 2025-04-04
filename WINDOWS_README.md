# Just Intonation Music Generator - Web Interface

This application allows you to create music using Just Intonation principles. 
Instead of equal temperament, Just Intonation uses pure frequency ratios which 
can create more harmonic sounds.

## Quick Start - Web Version
If you just want to use the web interface (recommended), you can:

1. Double-click on `just-intonation-tracker-web.exe` to run the program directly
2. A browser window will automatically open with the tracker interface
3. If no browser opens, manually navigate to http://localhost:8023

## Full Application

1. Double-click on `JustIntonationMusic.exe` to run the complete program
2. A command prompt window will open with several options
3. Type a number (1-7) and press Enter to select an option:
   - Option 1: Create a simple melody using Just Intonation
   - Option 2: Create a major arpeggio (A-C#-E-A-E-C#-A)
   - Option 3: Create a minor arpeggio (A-C-E-A-E-C-A)
   - Option 4: Create a Just Intonation scale
   - Option 5: Open Terminal Tracker Interface
   - Option 6: Open Web Tracker Interface (recommended)
   - Option 7: Exit the program
4. When a WAV file is created, it will be saved in the same folder as the program
5. Press Enter when prompted to return to the main menu

## Using the Web Tracker Interface

The Web Tracker Interface is the recommended way to create and edit tracker patterns:

1. When the interface loads, you'll see a grid-based tracker
2. If the browser doesn't open automatically, manually navigate to http://localhost:8023
3. In the web interface, you can:
   - Enter note frequencies directly (e.g., 440) or as ratios (e.g., 3:2)
   - Set instrument types:
     - `sin` - Sine wave (smooth sound)
     - `sqr` - Square wave (rich in harmonics, good for bass)
     - `tri` - Triangle wave (softer than square)
     - `saw` - Sawtooth wave (bright and buzzy)
   - Adjust volume (0-100)
   - Add effects:
     - `V5;2` - Vibrato with rate 5Hz and 2 semitones depth
     - Other effects will be documented as they are added
4. Click Play to hear your composition in the browser
5. Click Save to save your work (if you opened with a file)
6. If you're using the full application, return to the command prompt window and press Enter to go back to the main menu

## Playing the Generated WAV Files

After generating a WAV file, you can play it using any media player:
- Windows Media Player
- VLC
- Any other audio player that supports WAV files

Simply double-click on the WAV file or open it using your preferred media player.

## Understanding Just Intonation

Just Intonation uses frequency ratios based on the harmonic series:
- Unison (1:1) - Same note
- Minor Third (6:5) - Sounds slightly different from equal temperament
- Major Third (5:4) - Purer sound than equal temperament
- Perfect Fourth (4:3) - Very consonant interval
- Perfect Fifth (3:2) - Very pure and resonant
- Major Sixth (5:3) - Noticeably different from equal temperament
- Octave (2:1) - Same note one octave higher

The numbers (like 3:2) represent the frequency ratios. For example, 
if A = 440 Hz, then E (a perfect fifth above) would be 440 × 3/2 = 660 Hz.

## Advanced Usage

If you're interested in more advanced features or creating your own compositions,
talk to your friend who created this program for you. The underlying system is
quite powerful and can be extended to create more complex music using Just Intonation
principles.

## Troubleshooting

If you encounter any issues:

1. Make sure your audio is working (try playing another sound file)
2. Check that the WAV files were created in the same folder as the program
3. Try running the program as administrator if you have permission issues
4. If the web browser doesn't open automatically when using the Web Tracker Interface, manually navigate to http://localhost:8023
5. If you encounter connection issues with the web interface, try refreshing the browser page
6. Contact your friend who provided this program for additional help

## Technical Notes

This program uses:
- Threepenny-GUI library for its web-based interface
- Web Audio API for sound generation in the browser
- Just Intonation principles for music theory

The web interface:
- Opens a local web server on port 8023
- Automatically launches your default web browser
- Runs completely locally (no internet connection required)
- Uses standard web technologies (HTML, CSS, JavaScript)

## Building From Source

If you're technically inclined and want to build the application from source:

1. Install GHC (Glasgow Haskell Compiler) and Cabal
2. Clone the repository
3. Run `cabal build` to build the application
4. For Windows specifically, you can use the included `build-windows-web.sh` script if you have cross-compilation set up

Enjoy making music with Just Intonation!