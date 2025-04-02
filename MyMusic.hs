{-# LANGUAGE Arrows #-}

module MyMusic where

import Data.Ratio
import Euterpea
import Main hiding (justMelody, main)

-- ==========================================
-- YOUR COMPOSITIONS GO HERE
-- ==========================================

-- A simple melody
myFirstMelody :: Music (Double, Rational)
myFirstMelody =
  line
    [ Prim (Note qn (j 440 unison)),        -- A4
      Prim (Note qn (j 440 majorThird)),    -- C#5
      Prim (Note qn (j 440 perfectFifth)),  -- E5
      Prim (Note qn (j 440 octave))         -- A5
    ]

-- A simple chord
myFirstChord :: Music (Double, Rational)
myFirstChord =
  chord
    [ Prim (Note hn (j 440 unison)),       -- A4
      Prim (Note hn (j 440 majorThird)),   -- C#5
      Prim (Note hn (j 440 perfectFifth))  -- E5
    ]

-- Combination of melody and chord
myCombinedMusic :: Music (Double, Rational)
myCombinedMusic =
  myFirstMelody :+: myFirstChord

-- ==========================================
-- RENDER YOUR MUSIC
-- ==========================================

-- Main function that renders your music to a WAV file
renderMyMusic :: IO ()
renderMyMusic = do
  putStrLn "Rendering your music to WAV file..."
  
  -- Change the line below to render different compositions:
  writeJustWav "my_music.wav" 4.0 myFirstMelody
  
  -- Examples:
  -- writeJustWav "my_music.wav" 4.0 myFirstMelody
  -- writeJustWav "my_chord.wav" 2.0 myFirstChord
  -- writeJustWav "combined.wav" 6.0 myCombinedMusic
  
  putStrLn "Done! Check the current directory for the WAV file."