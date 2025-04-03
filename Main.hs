{-# LANGUAGE Arrows #-}

module Main where

import Data.Ratio
import Debug.Trace
import Euterpea
import System.Process (system)
import Control.Exception (catch, SomeException)
import qualified TrackerMain
import qualified TrackerSDL
-- import qualified TrackerTest

-- Import core just intonation functionality
import JustIntonationCore
import ErrorHandler

-- Helper to create a just pitch from a base frequency and ratio
j :: Double -> Ratio Integer -> JustPitch
j base ratio =
  let exactFreq = base * fromRational ratio
   in trace
        ("Creating pitch: base=" ++ show base ++ "Hz, ratio=" ++ show ratio ++ ", resulting in " ++ show exactFreq ++ "Hz")
        (exactFreq, ratio)

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

minorSixth :: Ratio Integer
minorSixth = 8 % 5

octave :: Ratio Integer
octave = 2 % 1

unison :: Ratio Integer
unison = 1 % 1

-- Example melody using just intonation
justMelody :: Music (Double, Rational)
justMelody =
  line
    [ Prim (Note qn (j 440 unison)), -- A4 (440Hz)
      Prim (Note qn (j 440 majorThird)), -- Major third up (550Hz)
      Prim (Note qn (j 440 perfectFifth)), -- Perfect fifth up (660Hz)
      Prim (Note qn (j 440 octave)) -- Octave up (880Hz)
    ]

-- Function to run an example from Examples.hs
runExample :: IO ()
runExample = do
  -- This tries to import and run the playExample function from Examples.hs
  -- If you have Examples.hs in the same directory, uncomment the line below:
  -- Examples.playExample
  
  -- Otherwise, use the default melody
  putStrLn "Running default example melody..."
  writeJustWav "just.wav" 4.0 justMelody

-- Function to run compositions from MyMusic.hs
runMyMusic :: IO ()
runMyMusic = do
  -- This tries to import and run the renderMyMusic function from MyMusic.hs
  -- If you have MyMusic.hs in the same directory, uncomment the line below:
  -- MyMusic.renderMyMusic
  
  -- Otherwise, use the default melody
  putStrLn "No custom music found, using default melody..."
  writeJustWav "just.wav" 4.0 justMelody

-- Main function with a simple terminal UI
main :: IO ()
main = withErrorHandling $ mainWithErrorHandling

-- Main function with error handling
mainWithErrorHandling :: IO ()
mainWithErrorHandling = do
  putStrLn "========================================"
  putStrLn "Just Intonation Music Generator"
  putStrLn "========================================"
  putStrLn "Choose an option:"
  putStrLn "1. Play default melody"
  putStrLn "2. Play major arpeggio"
  putStrLn "3. Play minor arpeggio"
  putStrLn "4. Play just scale"
  putStrLn "5. Open Terminal Tracker Interface"
  putStrLn "6. Open SDL Tracker Interface"
  putStrLn "7. Exit"
  putStrLn "Enter your choice (1-7): "
  
  choice <- getLine
  case choice of
    "1" -> do
      putStrLn "Creating default melody..."
      writeJustWav "just_melody.wav" 4.0 justMelody
      putStrLn "Done! Check just_melody.wav in the current folder."
      mainWithErrorHandling -- Return to menu
    
    "2" -> do
      putStrLn "Creating major arpeggio..."
      let majorArpeggio = line
            [ Prim (Note qn (j 440 unison)),      -- Root
              Prim (Note qn (j 440 majorThird)),  -- Major third
              Prim (Note qn (j 440 perfectFifth)), -- Perfect fifth
              Prim (Note qn (j 440 octave)),      -- Octave
              Prim (Note qn (j 440 perfectFifth)), -- Back down to fifth
              Prim (Note qn (j 440 majorThird)),  -- Back down to third
              Prim (Note qn (j 440 unison))       -- Back to root
            ]
      writeJustWav "major_arpeggio.wav" 4.0 majorArpeggio
      putStrLn "Done! Check major_arpeggio.wav in the current folder."
      mainWithErrorHandling -- Return to menu
    
    "3" -> do
      putStrLn "Creating minor arpeggio..."
      let minorArpeggio = line
            [ Prim (Note qn (j 440 unison)),      -- Root
              Prim (Note qn (j 440 minorThird)),  -- Minor third
              Prim (Note qn (j 440 perfectFifth)), -- Perfect fifth
              Prim (Note qn (j 440 octave)),      -- Octave
              Prim (Note qn (j 440 perfectFifth)), -- Back down to fifth
              Prim (Note qn (j 440 minorThird)),  -- Back down to third
              Prim (Note qn (j 440 unison))       -- Back to root
            ]
      writeJustWav "minor_arpeggio.wav" 4.0 minorArpeggio
      putStrLn "Done! Check minor_arpeggio.wav in the current folder."
      mainWithErrorHandling -- Return to menu
    
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
      putStrLn "Done! Check just_scale.wav in the current folder."
      mainWithErrorHandling -- Return to menu
    
    "5" -> do
      putStrLn "Opening Terminal Tracker Interface..."
      TrackerMain.trackerMenu
      mainWithErrorHandling -- Return to menu
    
    "6" -> do
      putStrLn "Opening SDL Tracker Interface..."
      TrackerSDL.startSDLTracker Nothing
      mainWithErrorHandling -- Return to menu
    
    "7" -> putStrLn "Goodbye!"
    
    _ -> do
      putStrLn "Invalid choice. Please try again."
      mainWithErrorHandling -- Return to menu
