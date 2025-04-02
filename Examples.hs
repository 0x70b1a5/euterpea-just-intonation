{-# LANGUAGE Arrows #-}

module Examples where

import Data.Ratio
import Euterpea
import Main hiding (justMelody, main)

-- Example 1: Simple scale in Just Intonation
justScale :: Music (Double, Rational)
justScale =
  line
    [ Prim (Note qn (j 440 unison)),           -- A4 (440Hz)
      Prim (Note qn (j 440 (9 % 8))),          -- B4 (9/8 ratio)
      Prim (Note qn (j 440 majorThird)),       -- C#5 (5/4 ratio)
      Prim (Note qn (j 440 perfectFourth)),    -- D5 (4/3 ratio)
      Prim (Note qn (j 440 perfectFifth)),     -- E5 (3/2 ratio)
      Prim (Note qn (j 440 majorSixth)),       -- F#5 (5/3 ratio)
      Prim (Note qn (j 440 (15 % 8))),         -- G#5 (15/8 ratio)
      Prim (Note qn (j 440 octave))            -- A5 (2/1 ratio)
    ]
    
-- Example 2: Major chord arpeggio
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

-- Example 3: Minor chord arpeggio
minorArpeggio :: Music (Double, Rational)
minorArpeggio =
  line
    [ Prim (Note qn (j 440 unison)),      -- Root
      Prim (Note qn (j 440 minorThird)),  -- Minor third
      Prim (Note qn (j 440 perfectFifth)), -- Perfect fifth
      Prim (Note qn (j 440 octave)),      -- Octave
      Prim (Note qn (j 440 perfectFifth)), -- Back down to fifth
      Prim (Note qn (j 440 minorThird)),  -- Back down to third
      Prim (Note qn (j 440 unison))       -- Back to root
    ]

-- Example 4: Simple harmonized melody with chords
harmonizedMelody :: Music (Double, Rational)
harmonizedMelody =
  Prim (Note hn (j 440 unison)) :+:                         -- Melody note A
  (Prim (Note hn (j 440 majorThird)) :=:                   -- Harmony E + C#
   Prim (Note hn (j 440 perfectFifth))) :+:
  Prim (Note hn (j 440 perfectFourth)) :+:                 -- Melody note D
  (Prim (Note hn (j 440 perfectFifth)) :=:                 -- Harmony A + F#
   Prim (Note hn (j 440 majorSixth))) :+:
  Prim (Note wn (j 440 unison))                           -- Final note A

-- Example 5: Custom exotic intervals
-- Define some additional ratios for exotic intervals
septimalMinorThird :: Ratio Integer
septimalMinorThird = 7 % 6  -- Septimal minor third, slightly flatter than 6/5

harmonicSeventh :: Ratio Integer
harmonicSeventh = 7 % 4     -- Harmonic seventh, flatter than equal temperament

undecimalTritone :: Ratio Integer
undecimalTritone = 11 % 8   -- A different kind of tritone (11/8)

-- Exotic scale using these ratios
exoticScale :: Music (Double, Rational)
exoticScale =
  line
    [ Prim (Note qn (j 440 unison)),             -- A
      Prim (Note qn (j 440 septimalMinorThird)), -- Septimal C
      Prim (Note qn (j 440 undecimalTritone)),   -- Undecimal D#
      Prim (Note qn (j 440 perfectFifth)),       -- E
      Prim (Note qn (j 440 harmonicSeventh)),    -- Harmonic G
      Prim (Note qn (j 440 octave))              -- A (octave)
    ]

-- Example 6: Different base frequencies
-- Using a different base frequency (D = 293.66 Hz instead of A = 440 Hz)
dBasedMelody :: Music (Double, Rational)
dBasedMelody =
  line
    [ Prim (Note qn (j 293.66 unison)),        -- D
      Prim (Note qn (j 293.66 majorThird)),    -- F#
      Prim (Note qn (j 293.66 perfectFifth)),  -- A
      Prim (Note qn (j 293.66 octave))         -- D (octave)
    ]

-- Example 7: Rhythm variations
rhythmVariations :: Music (Double, Rational)
rhythmVariations =
  line
    [ Prim (Note qn (j 440 unison)),          -- Quarter note
      Prim (Note en (j 440 majorThird)),      -- Eighth note
      Prim (Note en (j 440 perfectFifth)),    -- Eighth note
      Prim (Note qn (j 440 octave)),          -- Quarter note
      Prim (Note en (j 440 octave)),          -- Eighth note
      Prim (Note en (j 440 perfectFifth)),    -- Eighth note
      Prim (Note en (j 440 majorThird)),      -- Eighth note
      Prim (Note en (j 440 unison))           -- Eighth note
    ]

-- Example 8: Combining multiple techniques (melody, chords, rhythms)
combinedExample :: Music (Double, Rational)
combinedExample =
  -- First phrase: Simple melody
  line [
    Prim (Note qn (j 440 unison)),
    Prim (Note qn (j 440 majorThird)),
    Prim (Note qn (j 440 perfectFifth)),
    Prim (Note qn (j 440 octave))
  ] :+:
  -- Second phrase: Chord
  chord [
    Prim (Note hn (j 440 unison)),
    Prim (Note hn (j 440 majorThird)),
    Prim (Note hn (j 440 perfectFifth))
  ] :+:
  -- Third phrase: Rhythmic variation
  line [
    Prim (Note en (j 440 octave)),
    Prim (Note en (j 440 perfectFifth)),
    Prim (Note en (j 440 majorThird)),
    Prim (Note en (j 440 unison)),
    Prim (Note qn (j 440 perfectFourth)),
    Prim (Note qn (j 440 perfectFifth))
  ] :+:
  -- Final chord
  chord [
    Prim (Note wn (j 440 unison)),
    Prim (Note wn (j 440 perfectFifth)),
    Prim (Note wn (j 440 octave))
  ]

-- Function to render any of the example compositions to WAV
renderExample :: Music (Double, Rational) -> FilePath -> Double -> IO ()
renderExample music filename duration = writeJustWav filename duration music

-- Function to play a specific example (uncomment the one you want to hear)
playExample :: IO ()
playExample = do
  putStrLn "Rendering example music to WAV file..."
  
  -- Choose which example to render by uncommenting ONE of these lines:
  renderExample justScale "scale.wav" 4.0
  -- renderExample majorArpeggio "major_arpeggio.wav" 4.0
  -- renderExample minorArpeggio "minor_arpeggio.wav" 4.0
  -- renderExample harmonizedMelody "harmonized.wav" 6.0
  -- renderExample exoticScale "exotic.wav" 3.0
  -- renderExample dBasedMelody "d_melody.wav" 2.0
  -- renderExample rhythmVariations "rhythm.wav" 3.0
  -- renderExample combinedExample "combined.wav" 8.0
  
  putStrLn "Done! Check the current directory for the WAV file."