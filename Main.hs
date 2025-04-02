{-# LANGUAGE Arrows #-}

module Main where

import Data.Ratio
import Debug.Trace
import Euterpea

-- Just Intonation Types
type JustPitch = (Double, Ratio Integer) -- (base frequency in Hz, ratio)

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

-- Create a sine wave instrument for just intonation
sineTable :: Table
sineTable = tableSinesN 4096 [1]

-- Create a signal function for a just pitch
justSF :: JustPitch -> Dur -> AudSF () Double
justSF (freq, _) du =
  let dd = fromRational du
   in trace ("Generating signal: duration=" ++ show dd ++ "s, frequency=" ++ show freq ++ "Hz") $
        proc _ -> do
          -- Generate sine wave at specified frequency
          y <- osc sineTable 0 -< freq
          -- Apply an envelope to avoid clicks at note boundaries
          ex <- envLineSeg [0, 1, 1, 0] [0.01 * dd, 0.94 * dd, 0.05 * dd] -< ()
          returnA -< y * ex * 0.5 -- Scale down the amplitude

-- Convert our just intonation music to a signal function
justToSF :: Music (Double, Rational) -> AudSF () Double
justToSF (Prim (Note du (freq, ratio))) =
  trace ("Converting note to signal: duration=" ++ show du ++ ", frequency=" ++ show freq ++ "Hz") $
    justSF (freq, ratio) du
justToSF (Prim (Rest du)) =
  let dd = fromRational du
   in proc _ -> do
        e <- envLineSeg [0, 0] [dd] -< ()
        returnA -< 0
justToSF (m1 :+: m2) = proc _ -> do
    -- Track the current time
    t <- integral -< 1
    -- Calculate the duration of m1
    let d1 = Euterpea.dur m1
    -- Only play m1 if we're within its duration
    out1 <- if t < fromRational d1 
           then justToSF m1 -< ()
           else returnA -< 0
    -- Only play m2 if we've passed m1's duration
    out2 <- if t >= fromRational d1
           then justToSF m2 -< ()
           else returnA -< 0
    -- Sum the outputs
    returnA -< out1 + out2
justToSF (m1 :=: m2) = proc _ -> do
  y1 <- justToSF m1 -< ()
  y2 <- justToSF m2 -< ()
  returnA -< (y1 + y2) * 0.5 -- Scale down parallel notes
justToSF (Modify _ m) = justToSF m

-- Example melody using just intonation
justMelody :: Music (Double, Rational)
justMelody =
  line
    [ Prim (Note qn (j 440 unison)), -- A4 (440Hz)
      Prim (Note qn (j 440 majorThird)), -- Major third up (550Hz)
      Prim (Note qn (j 440 perfectFifth)), -- Perfect fifth up (660Hz)
      Prim (Note qn (j 440 octave)) -- Octave up (880Hz)
    ]

-- Custom writeWav function that uses our just intonation signal function
writeJustWav :: FilePath -> Double -> Music (Double, Rational) -> IO ()
writeJustWav file dur m = outFile file dur (justToSF m)

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
main = do
  putStrLn "========================================"
  putStrLn "Just Intonation Music Generator"
  putStrLn "========================================"
  putStrLn "Choose an option:"
  putStrLn "1. Play default melody"
  putStrLn "2. Play major arpeggio"
  putStrLn "3. Play minor arpeggio"
  putStrLn "4. Play just scale"
  putStrLn "5. Exit"
  putStrLn "Enter your choice (1-5): "
  
  choice <- getLine
  case choice of
    "1" -> do
      putStrLn "Creating default melody..."
      writeJustWav "just_melody.wav" 4.0 justMelody
      putStrLn "Done! Check just_melody.wav in the current folder."
      main -- Return to menu
    
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
      main -- Return to menu
    
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
      main -- Return to menu
    
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
      main -- Return to menu
    
    "5" -> putStrLn "Goodbye!"
    
    _ -> do
      putStrLn "Invalid choice. Please try again."
      main -- Return to menu
