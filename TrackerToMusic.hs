{-# LANGUAGE Arrows #-}

module TrackerToMusic where

import Data.Ratio
import Data.Maybe (catMaybes, fromMaybe, isJust)
import Euterpea
-- Use the core functionality
import JustIntonationCore
import TrackerTypes
import TrackerParser (readTrackerFile)

-- | Convert a TrackerFile to Euterpea Music format
trackerToMusic :: TrackerFile -> Music (Double, Rational)
trackerToMusic tf = 
  -- Combine all rows sequentially
  let rows = trackerData tf
      baseFreq = baseFrequency tf
      baseBpm = baseTempo tf
      rowsPerB = fromIntegral (rowsPerBeat tf)
      
      -- Get effective duration for a row
      getRowDur rowIdx = 
        let 
          -- Find the first tempo marking at or before this row
          effectiveTempo = findEffectiveTempo rows rowIdx baseBpm
          -- Calculate how long each beat is in quarter notes
          beatDur = 1.0 / (effectiveTempo / 60.0)
          -- Calculate how long each row is in quarter notes
          rowDur = beatDur / rowsPerB
        in 
          -- Convert to Euterpea rational duration
          toRational rowDur
      
      -- Convert each row to Music
      rowToMusic rowIdx row = 
        let 
          dur = getRowDur rowIdx
          chans = rowChannels row
          -- Convert each channel to Music
          chanMusics = catMaybes $ zipWith (channelToMusic baseFreq dur) [0..] chans
        in
          -- Combine all channels in parallel
          foldr (:=:) (rest dur) chanMusics
  in
    -- Sequence all rows
    foldr (:+:) (rest 0) $ zipWith rowToMusic [0..] rows

-- | Find the effective tempo for a row by looking backward
findEffectiveTempo :: [TrackerRow] -> Int -> Double -> Double
findEffectiveTempo rows rowIdx defaultTempo =
  if rowIdx < 0 || rowIdx >= length rows
  then defaultTempo
  else go rowIdx
  where
    go idx
      | idx < 0 = defaultTempo
      | otherwise = 
          case rowTempo (rows !! idx) >>= tempoValue of
            Just tempo -> tempo
            Nothing -> go (idx - 1)

-- | Convert a channel to Music (if it contains a note)
channelToMusic :: Double -> Rational -> Int -> ChannelData -> Maybe (Music (Double, Rational))
channelToMusic baseFreq dur chanIdx chan = 
  case channelNote chan of
    -- If there's no note, return Nothing
    Nothing -> Nothing
    
    -- If there's a note, create Music for it
    Just noteData -> 
      case noteFrequency noteData of
        -- If frequency is not calculated, skip
        Nothing -> Nothing
        
        -- Otherwise, create the note
        Just freq -> 
          let 
            -- Get the ratio (or calculate it from frequency)
            ratio = case noteRatio noteData of
              Just r -> r
              Nothing -> toRational (freq / baseFreq)
            
            -- Apply volume as an amplitude value
            vol = channelVolume chan
            
            -- Get the instrument
            inst = channelInstrument chan
            
            -- Create a note with the proper instrument
            noteWithInstrument = case stringToInstrument inst of
              Just Sine -> Prim (Note dur (freq, ratio))
              Just Square -> applyInstrument "square" $ Prim (Note dur (freq, ratio))
              Just Sawtooth -> applyInstrument "sawtooth" $ Prim (Note dur (freq, ratio))
              Just Triangle -> applyInstrument "triangle" $ Prim (Note dur (freq, ratio))
              Nothing -> Prim (Note dur (freq, ratio)) -- Default to sine
            
            -- Apply volume adjustment
            -- Note: In a full implementation, we would scale the amplitude 
            -- based on the volume value. For now, we just pass it through.
            noteWithVolume = if vol /= 1.0
                              then applyVolume vol noteWithInstrument
                              else noteWithInstrument
            
            -- Apply effects (if any)
            withEffects = applyEffects (channelEffect chan) noteWithVolume
          in
            Just withEffects

-- | Apply an instrument to a Music value
-- This is a placeholder for actual instrument implementation
-- In a full implementation, this would modify the sound generation
applyInstrument :: String -> Music (Double, Rational) -> Music (Double, Rational)
applyInstrument instName music = 
  -- For now, we'll just return the original music
  -- In a real implementation, this would modify the instrument type
  music
  
-- | Apply volume adjustment to a Music value
-- This is a placeholder for actual volume implementation
-- In a full implementation, this would scale the amplitude
applyVolume :: Double -> Music (Double, Rational) -> Music (Double, Rational)
applyVolume vol music = 
  -- For now, we'll just return the original music
  -- In a real implementation, this would scale the amplitude
  music

-- | Apply effects to a Music value
applyEffects :: EffectData -> Music (Double, Rational) -> Music (Double, Rational)
applyEffects effect music = 
  case effectCommand effect of
    -- No effect
    Nothing -> music
    
    -- Arpeggio effect
    Just 'A' -> arpeggioEffect effect music
    
    -- Transpose effect
    Just 'T' -> transposeEffect effect music
    
    -- Volume effect
    Just 'V' -> volumeEffect effect music
    
    -- No recognized effect
    _ -> music

-- | Implement arpeggio effect (plays note, third, fifth in sequence)
arpeggioEffect :: EffectData -> Music (Double, Rational) -> Music (Double, Rational)
arpeggioEffect effect (Prim (Note dur (freq, ratio))) = 
  -- Calculate new durations
  let subDur = dur / 3
      -- Create the three notes of the arpeggio
      note1 = Prim (Note subDur (freq, ratio))
      -- Major third
      note2 = Prim (Note subDur (freq * 5/4, 5%4 * ratio))
      -- Perfect fifth
      note3 = Prim (Note subDur (freq * 3/2, 3%2 * ratio))
  in
    -- Play in sequence
    note1 :+: note2 :+: note3
arpeggioEffect _ m = m  -- Pass through for non-note music

-- | Implement transpose effect (shifts pitch by ratio)
transposeEffect :: EffectData -> Music (Double, Rational) -> Music (Double, Rational)
transposeEffect effect (Prim (Note dur (freq, ratio))) = 
  case effectValue effect >>= parseRatio of
    Just transRatio -> 
      let newFreq = freq * (fromRational $ toRational transRatio)
          newRatio = ratio * transRatio
      in Prim (Note dur (newFreq, newRatio))
    Nothing -> Prim (Note dur (freq, ratio))
transposeEffect _ m = m  -- Pass through for non-note music

-- | Implement volume effect (scales amplitude)
volumeEffect :: EffectData -> Music (Double, Rational) -> Music (Double, Rational)
volumeEffect _ m = m  -- Placeholder, volume changes would happen at rendering time

-- | Parse ratio from string (format: "n:m")
parseRatio :: String -> Maybe (Ratio Integer)
parseRatio str = 
  case break (==':') str of
    (numStr, ':':denomStr) -> 
      case (readMaybe numStr, readMaybe denomStr) of
        (Just n, Just d) | d > 0 -> Just (n % d)
        _ -> Nothing
    _ -> Nothing
  where 
    readMaybe :: Read a => String -> Maybe a
    readMaybe s = case reads s of
      [(val, "")] -> Just val
      _ -> Nothing

-- | Render a tracker file to WAV
renderTrackerFile :: TrackerFile -> FilePath -> Double -> IO ()
renderTrackerFile tf outputPath duration = do
  let music = trackerToMusic tf
  writeJustWav outputPath duration music
  putStrLn $ "Rendered tracker file to: " ++ outputPath

-- | Load and render a tracker file to WAV
loadAndRenderTrackerFile :: FilePath -> FilePath -> Double -> IO ()
loadAndRenderTrackerFile inputPath outputPath duration = do
  putStrLn $ "Loading tracker file: " ++ inputPath
  result <- readTrackerFile inputPath
  case result of
    Left err -> putStrLn $ "Error: " ++ err
    Right tf -> do
      putStrLn "Converting to music..."
      renderTrackerFile tf outputPath duration
      putStrLn "Done!"

-- | This is just a re-export from TrackerParser
-- We shouldn't define it here to avoid conflicts
-- readTrackerFile :: FilePath -> IO (Either String TrackerFile)