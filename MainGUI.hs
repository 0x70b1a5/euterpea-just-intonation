module MainGUI where

import System.Directory (getCurrentDirectory)
import System.IO
import Text.Printf
import System.Exit
import Data.Ratio
import Euterpea
import Main hiding (main)
import qualified TrackerMain

-- | A very simple terminal-based UI for non-technical users
mainGUI :: IO ()
mainGUI = do
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
  putStrLn "5. Open Tracker Interface"
  putStrLn "6. Exit"
  putStrLn "========================================"
  putStrLn "Enter your choice (1-6): "
  
  choice <- getLine
  case choice of
    "1" -> do
      putStrLn "Creating default melody..."
      writeJustWav "just_melody.wav" 4.0 justMelody
      putStrLn "Created just_melody.wav in the current directory."
      waitForKeypress
      mainGUI
    
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
      mainGUI
    
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
      mainGUI
    
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
      mainGUI
    
    "5" -> do
      putStrLn "Opening Tracker interface..."
      TrackerMain.trackerMenu
      waitForKeypress
      mainGUI
      
    "6" -> do
      putStrLn "Thank you for using Just Intonation Music Generator!"
      exitSuccess
    
    _ -> do
      putStrLn "Invalid choice. Please try again."
      waitForKeypress
      mainGUI

-- | Clear the screen
clearScreen :: IO ()
clearScreen = putStr "\ESC[2J\ESC[H"

-- | Wait for a keypress before continuing
waitForKeypress :: IO ()
waitForKeypress = do
  putStrLn "Press Enter to continue..."
  _ <- getLine
  return ()