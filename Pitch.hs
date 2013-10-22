module Hasmt.Pitch (
Pitch(..), moveSemitones, Octave, getPitchNote, difference
) where

import Hasmt.Note

type Octave = Int

-- now we are looking at octaves along with just the pitch class
data Pitch = Pitch {octave :: Octave, pitchClass :: PitchClass} deriving (Eq, Ord, Show)

instance HavingPitchClass Pitch where
    getPitchClass (Pitch _ pc) = pc

difference :: Pitch -> Pitch -> Int
difference (Pitch oct pc) (Pitch oct' pc') = (oct * 12 + pc) - (oct' * 12 + pc)

moveSemitones :: Int -> Pitch -> Pitch
moveSemitones i (Pitch oct pc) = Pitch oct' pc'
    where pc' = (pc + i) `mod` 12
          oct' = oct + ((pc + i) `div` 12)

getPitchNote :: Pitch -> Note
getPitchNote = pcToNote . getPitchClass

