module Hasmt.Pitch (
Pitch(..), moveSemitones, Octave, getPitchNote
) where

import Hasmt.Note

type Octave = Int

-- now we are looking at octaves along with just the pitch class
data Pitch = Pitch Octave PitchClass deriving (Eq, Ord, Show)

instance HavingPitchClass Pitch where
    getPitchClass (Pitch _ pc) = pc

moveSemitones :: Int -> Pitch -> Pitch
moveSemitones i (Pitch oct pc) = Pitch oct' pc'
    where pc' = (pc + i) `mod` 12
          oct' = oct + ((pc + i) `div` 12)

getPitchNote :: Pitch -> Note
getPitchNote = pcToNote . getPitchClass

