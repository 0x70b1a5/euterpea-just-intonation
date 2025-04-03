module MainGUI where

import System.Directory (getCurrentDirectory)
import System.IO
import Text.Printf
import System.Exit
import Data.Ratio
import Euterpea
import JustIntonationCore
import qualified Main
import qualified TrackerMain
import qualified TrackerSDL
import ErrorHandler
import Control.Exception (catch, SomeException)

-- Just Intonation constants
-- Helper to create a just pitch from a base frequency and ratio
j :: Double -> Ratio Integer -> JustPitch
j base ratio =
  let exactFreq = base * fromRational ratio
   in (exactFreq, ratio)

-- Common just intervals
perfectFifth :: Ratio Integer
perfectFifth = 3 % 2

majorThird :: Ratio Integer
majorThird = 5 % 4

minorThird :: Ratio Integer
minorThird = 6 % 5

perfectFourth :: Ratio Integer
perfectFourth = 4 % 3

majorSixth :: Ratio Integer
majorSixth = 5 % 3

octave :: Ratio Integer
octave = 2 % 1

unison :: Ratio Integer
unison = 1 % 1

-- | A very simple terminal-based UI for non-technical users
mainGUI :: IO ()
mainGUI = withErrorHandling $ mainGUIWithErrorHandling

-- Implementation with error handling
mainGUIWithErrorHandling :: IO ()
mainGUIWithErrorHandling = do
  clearScreen
  currentDir <- getCurrentDirectory
  
  putStrLn "========================================"
  putStrLn "Just Intonation Music Generator"
  putStrLn "========================================"
  putStrLn $ "Output directory: " ++ currentDir
  putStrLn ""
  putStrLn "Choose an option:"
  putStrLn "1. Generate Default Melody"
  putStrLn "2. Generate Major Arpeggio"
  putStrLn "3. Generate Minor Arpeggio"
  putStrLn "4. Generate Just Scale" 
  putStrLn "5. Open Terminal Tracker Interface"
  putStrLn "6. Open SDL Tracker Interface"
  putStrLn "7. Exit"
  putStrLn "========================================"
  putStrLn "Enter your choice (1-7): "
  
  choice <- getLine
  case choice of
    "1" -> do
      putStrLn "Creating default melody..."
      -- Create a simple melody
      let justMelody = line
            [ Prim (Note qn (j 440 unison)),      -- A4 (440Hz)
              Prim (Note qn (j 440 majorThird)),  -- Major third up (550Hz)
              Prim (Note qn (j 440 perfectFifth)),-- Perfect fifth up (660Hz)
              Prim (Note qn (j 440 octave))       -- Octave up (880Hz)
            ]
      writeJustWav "just_melody.wav" 4.0 justMelody
      putStrLn "Created just_melody.wav in the current directory."
      waitForKeypress
      mainGUIWithErrorHandling
    
    "2" -> do
      putStrLn "Creating major arpeggio..."
      let majorArpeggio = line
            [ Prim (Note qn (j 440 unison)),
              Prim (Note qn (j 440 majorThird)),
              Prim (Note qn (j 440 perfectFifth)),
              Prim (Note qn (j 440 octave)),
              Prim (Note qn (j 440 perfectFifth)),
              Prim (Note qn (j 440 majorThird)),
              Prim (Note qn (j 440 unison))
            ]
      writeJustWav "major_arpeggio.wav" 4.0 majorArpeggio
      putStrLn "Created major_arpeggio.wav in the current directory."
      waitForKeypress
      mainGUIWithErrorHandling
    
    "3" -> do
      putStrLn "Creating minor arpeggio..."
      let minorArpeggio = line
            [ Prim (Note qn (j 440 unison)),
              Prim (Note qn (j 440 minorThird)),
              Prim (Note qn (j 440 perfectFifth)),
              Prim (Note qn (j 440 octave)),
              Prim (Note qn (j 440 perfectFifth)),
              Prim (Note qn (j 440 minorThird)),
              Prim (Note qn (j 440 unison))
            ]
      writeJustWav "minor_arpeggio.wav" 4.0 minorArpeggio
      putStrLn "Created minor_arpeggio.wav in the current directory."
      waitForKeypress
      mainGUIWithErrorHandling
    
    "4" -> do
      putStrLn "Creating just scale..."
      let justScale = line
            [ Prim (Note qn (j 440 unison)),           -- A4 (440Hz)
              Prim (Note qn (j 440 (9 % 8))),          -- B4 (9/8 ratio)
              Prim (Note qn (j 440 majorThird)),       -- C#5 (5/4 ratio)
              Prim (Note qn (j 440 perfectFourth)),    -- D5 (4/3 ratio)
              Prim (Note qn (j 440 perfectFifth)),     -- E5 (3/2 ratio)
              Prim (Note qn (j 440 majorSixth)),       -- F#5 (5/3 ratio)
              Prim (Note qn (j 440 (15 % 8))),         -- G#5 (15/8 ratio)
              Prim (Note qn (j 440 octave))            -- A5 (2/1 ratio)
            ]
      writeJustWav "just_scale.wav" 4.0 justScale
      putStrLn "Created just_scale.wav in the current directory."
      waitForKeypress
      mainGUIWithErrorHandling
    
    "5" -> do
      putStrLn "Opening Terminal Tracker interface..."
      TrackerMain.trackerMenu
      waitForKeypress
      mainGUIWithErrorHandling
    
    "6" -> do
      putStrLn "Opening SDL Tracker interface..."
      TrackerSDL.startSDLTracker Nothing
      waitForKeypress
      mainGUIWithErrorHandling
      
    "7" -> do
      putStrLn "Thank you for using Just Intonation Music Generator!"
      exitSuccess
    
    _ -> do
      putStrLn "Invalid choice. Please try again."
      waitForKeypress
      mainGUIWithErrorHandling

-- | Clear the screen
clearScreen :: IO ()
clearScreen = putStr "\ESC[2J\ESC[H"

-- | Wait for a keypress before continuing
waitForKeypress :: IO ()
waitForKeypress = do
  putStrLn "Press Enter to continue..."
  _ <- getLine
  return ()