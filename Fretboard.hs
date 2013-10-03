module Hasmt.Fretboard where

import Hasmt.Util
import Hasmt.Interval
import Hasmt.Note
import Hasmt.Pitch
import Hasmt.Chord

import Data.Function (on)
import Data.List (minimumBy)
import Control.Monad


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
type Voicing = [(Interval, Fret)]

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

isOnString :: StringNum -> Fret -> Bool
isOnString sn (Fret sn' _) = sn == sn'

buildVoicing' :: [(Interval, Fret)] -> StringNum -> [(Interval, Fret)]
buildVoicing' chordFrets sn = filter f chordFrets
    where f (_, fret) = isOnString sn fret

voicingsInRange :: Tuning -> Chord -> Note -> FretRange -> [Voicing]
voicingsInRange tuning chord note fr = mapM (buildVoicing' chordFrets) (strings tuning)
    where chordFrets = fretsForChordInRange tuning note chord fr

-- Actually check pitch later
rootInBass :: Voicing -> Bool
rootInBass v = fst lowest == Unison
    where lowest = minimumBy (compare `on` (stringNum . snd)) v

