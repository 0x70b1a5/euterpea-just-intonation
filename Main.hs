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
  MyMusic.renderMyMusic
  
  -- Otherwise, use the default melody
  -- putStrLn "No custom music found, using default melody..."
  -- writeJustWav "just.wav" 4.0 justMelody

-- Main function - choose what to run
main :: IO ()
main = do
  putStrLn "Writing to WAV file with just intonation..."
  
  -- Choose which function to run:
  writeJustWav "just.wav" 4.0 justMelody  -- Default melody
  runExample    -- Run an example from Examples.hs
  runMyMusic    -- Run your music from MyMusic.hs
