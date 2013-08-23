module Fretboard where

import Note
import Pitch

type Tuning = [Pitch]

maxFret = 24

-- TODO: Automate this later.. with less jank
standardTuning = [Pitch 0 7, -- Low E
                  Pitch 1 0, -- A
                  Pitch 1 5, -- D
                  Pitch 1 10, -- G
                  Pitch 2 2, -- B
                  Pitch 2 7] -- E

type Fret = Int
type StringNum = Int

strings :: Tuning -> [StringNum]
strings tuning = map fst $ zip [0..] tuning

pitchAtFret :: Tuning -> StringNum -> Fret -> Pitch
pitchAtFret tuning string fret = moveSemitones fret openPitch
    where openPitch = tuning !! string

noteAtFret :: Tuning -> StringNum -> Fret -> Note
noteAtFret tuning stringnum = getPitchNote . (pitchAtFret tuning stringnum)

fretsWithNoteOnString :: Tuning -> StringNum -> Note -> [Fret]
fretsWithNoteOnString tuning stringnum note = filter f [0..maxFret]
    where f fret = noteAtFret tuning stringnum fret `enharmonicEquiv` note

fretsWithPitchOnString :: Tuning -> StringNum -> Pitch -> [Fret]
fretsWithPitchOnString tuning stringnum pitch = filter f [0..maxFret]
    where f fret = pitchAtFret tuning stringnum fret == pitch

fretsWithPitch :: Tuning -> Pitch -> [(StringNum, Fret)]
fretsWithPitch tuning pitch = concat $ map f (strings tuning)
     where f stringNum = map (\x -> (stringNum, x)) $ 
                             fretsWithPitchOnString tuning stringNum pitch

fretsWithNote :: Tuning -> Note -> [(StringNum, Fret)]
fretsWithNote tuning note = concat $ map f (strings tuning)
     where f stringNum = map (\x -> (stringNum, x)) $ 
                             fretsWithNoteOnString tuning stringNum note


