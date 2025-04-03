{-# LANGUAGE Arrows #-}

module JustIntonationCore where

import Data.Ratio
import Euterpea

-- Just Intonation Types
type JustPitch = (Double, Ratio Integer) -- (base frequency in Hz, ratio)

-- Custom writeWav function that uses our just intonation signal function
writeJustWav :: FilePath -> Double -> Music (Double, Rational) -> IO ()
writeJustWav file dur m = outFile file dur (justToSF m)

-- Create a sine wave instrument for just intonation
sineTable :: Table
sineTable = tableSinesN 4096 [1]

-- Convert our just intonation music to a signal function
justToSF :: Music (Double, Rational) -> AudSF () Double
justToSF (Prim (Note du (freq, ratio))) =
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

-- Create a signal function for a just pitch
justSF :: JustPitch -> Dur -> AudSF () Double
justSF (freq, _) du =
  let dd = fromRational du
   in proc _ -> do
        -- Generate sine wave at specified frequency
        y <- osc sineTable 0 -< freq
        -- Apply an envelope to avoid clicks at note boundaries
        ex <- envLineSeg [0, 1, 1, 0] [0.01 * dd, 0.94 * dd, 0.05 * dd] -< ()
        returnA -< y * ex * 0.5 -- Scale down the amplitude