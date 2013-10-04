module Hasmt.Interval (
Interval (Unison, Second, Third, Fourth, Fifth, Sixth, Seventh),
semitones, sharp, flat, noteAbove,
intToInterval, applyAccidental
) where

import Hasmt.Note
import Hasmt.Util
import Data.Function (on)

-- TODO: Implement a custom Ord and Eq instance for intervals
data Interval = Unison | Second | Third | 
    Fourth | Fifth | Sixth | 
    Seventh | Flattened Interval | 
    Sharpened Interval 

instance Eq Interval where
    i == i' = semitones i == semitones i'

instance Ord Interval where
    compare = compare `on` semitones

instance Show Interval where
    show Unison = "1"
    show Second = "2"
    show Third = "3"
    show Fourth = "4"
    show Fifth = "5"
    show Sixth = "6"
    show Seventh = "7"
    show (Flattened i) = 'b' : show i
    show (Sharpened i) = '#' : show i

instance EnharmonicEquiv Interval where
    enharmonicEquiv i i' = semitones i == semitones i'

applyAccidental :: Accidental -> Interval -> Interval
applyAccidental Sharp i = sharp i
applyAccidental Flat i = flat i
applyAccidental Natural i = natural i

-- Put note with potentially weird accidentals into more common form
canonicalize :: Note -> Note
canonicalize = pcToNote . getPitchClass

scaleSteps :: Interval -> Int
scaleSteps Unison = 0
scaleSteps Second = 1
scaleSteps Third = 2
scaleSteps Fourth = 3
scaleSteps Fifth = 4
scaleSteps Sixth = 5
scaleSteps Seventh = 6
scaleSteps (Flattened i) = scaleSteps i
scaleSteps (Sharpened i) = scaleSteps i

intToInterval :: Int -> Interval
intToInterval 1 = Unison 
intToInterval 2 = Second 
intToInterval 3 = Third 
intToInterval 4 = Fourth 
intToInterval 5 = Fifth 
intToInterval 6 = Sixth 
intToInterval 7 = Seventh 
intToInterval n = intToInterval (n `mod` 7)

semitones :: Interval -> Int
semitones Unison = 0
semitones Second = 2
semitones Third = 4
semitones Fourth = 5
semitones Fifth = 7
semitones Sixth = 9
semitones Seventh = 11
semitones (Flattened x) = semitones x - 1
semitones (Sharpened x) = semitones x + 1

semitonesToInterval :: Int -> Interval
semitonesToInterval n = case n `mod` 12 of
    0 -> p1 
    1 -> min2 
    2 -> maj2 
    3 -> min3 
    4 -> maj3
    5 -> p4 
    6 -> flat5 
    7 -> p5 
    8 -> min6 
    9 -> maj6 
    10 ->  min7
    11 -> maj7

flat :: Interval -> Interval
flat (Sharpened i) = i
flat i = Flattened i

sharp :: Interval -> Interval
sharp (Flattened i) = i
sharp i = Sharpened i

natural :: Interval -> Interval
natural (Flattened i) = natural i
natural (Sharpened i) = natural i
natural i = i

noteAbove :: Note -> Interval -> Note
noteAbove note interval = pcToNoteWithBase newPitchClass base
    where base = compose (take (scaleSteps interval) $ repeat nextNat) note
          newPitchClass = getPitchClass note + semitones interval
          

-- Aliases
p1 = Unison
octave = Unison
min2 = flat Second
maj2 = Second
min3 = flat Third
maj3 = Third
p4 = Fourth
sharp4 = sharp Fourth
flat5 = flat Fifth
p5 = Fifth
min6 = flat Sixth
maj6 = Sixth
min7 = flat Seventh
maj7 = Seventh
