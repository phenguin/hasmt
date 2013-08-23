module Fretboard where

import Util
import Interval
import Note
import Pitch
import Chord


type Tuning = [Pitch]

maxFret = 24

-- TODO: Automate this later.. with less jank
standardTuning = [Pitch 0 7, -- Low E
                  Pitch 1 0, -- A
                  Pitch 1 5, -- D
                  Pitch 1 10, -- G
                  Pitch 2 2, -- B
                  Pitch 2 7] -- E

type FretNum = Int
type StringNum = Int

data Fret = Fret { stringNum :: StringNum, fretNum :: FretNum } deriving (Show, Eq, Ord, Read)
data FretRange = FretRange { lowerBound :: FretNum, upperBound :: FretNum } deriving
                    (Eq, Ord, Show, Read)

strings :: Tuning -> [StringNum]
strings tuning = map fst $ zip [0..] tuning

pitchAtFret :: Tuning -> StringNum -> FretNum -> Pitch
pitchAtFret tuning string fretnum = moveSemitones fretnum openPitch
    where openPitch = tuning !! string

noteAtFret :: Tuning -> StringNum -> FretNum -> Note
noteAtFret tuning stringnum = getPitchNote . (pitchAtFret tuning stringnum)

fretsWithNoteOnString :: Tuning -> StringNum -> Note -> [FretNum]
fretsWithNoteOnString tuning stringnum note = filter f [0..maxFret]
    where f fretnum = noteAtFret tuning stringnum fretnum `enharmonicEquiv` note

fretsWithPitchOnString :: Tuning -> StringNum -> Pitch -> [FretNum]
fretsWithPitchOnString tuning stringnum pitch = filter f [0..maxFret]
    where f fretnum = pitchAtFret tuning stringnum fretnum == pitch

fretsWithPitch :: Tuning -> Pitch -> [Fret]
fretsWithPitch tuning pitch = map g $ concat $ map f (strings tuning)
     where f stringNum = map (\x -> (stringNum, x)) $ 
                             fretsWithPitchOnString tuning stringNum pitch
           g (stringNum, fretNum) = Fret stringNum fretNum

fretsWithNote :: Tuning -> Note -> [Fret]
fretsWithNote tuning note = map g $ concat $ map f (strings tuning)
     where f stringNum = map (\x -> (stringNum, x)) $ 
                             fretsWithNoteOnString tuning stringNum note
           g (stringNum, fretNum) = Fret stringNum fretNum

fretsForChord :: Tuning -> Note -> Chord -> [(Interval, Fret)]
fretsForChord tuning note chord = concatMap f chordNotes
    where chordNotes = spellChordForRoot note chord
          f (interval, note') = map (addFst interval) $ 
                                   fretsWithNote tuning note'

fretsForChordInRange :: Tuning -> Note -> Chord -> FretRange -> [(Interval, Fret)]
fretsForChordInRange tuning note chord fr = filter f $ fretsForChord tuning note chord
    where f (interval, fret) = inFretRange fr fret

inFretRange :: FretRange -> Fret -> Bool
inFretRange (FretRange low high) (Fret _ fretNum) = low <= fretNum && fretNum <= high

-- buildVoicing' :: Interval -> StringNum -> [(Interval, Fret)] -> [Fret]
-- buildVoicing' interval stringNum xs = head $ filterg $ filter f xs
--     where f (interval, (Fret stringNum _)) = interval 


