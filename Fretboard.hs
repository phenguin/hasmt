module Hasmt.Fretboard where

import Hasmt.Util
import Hasmt.Interval
import Hasmt.Note
import Hasmt.Pitch
import Hasmt.Chord

import Data.Function (on)
import Data.List (minimumBy, maximumBy, sortBy, nub, sort, maximum, minimum)
import Control.Monad
import Data.Maybe (catMaybes)
import qualified Data.Set as S hiding (filter, difference)


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
strings = zipWith (curry fst) [0..] 

pitchAtFret :: Tuning -> StringNum -> FretNum -> Pitch
pitchAtFret tuning string fretnum = moveSemitones fretnum openPitch
    where openPitch = tuning !! string

pitchAtFret' tuning (Fret fret strnum) = pitchAtFret tuning fret strnum

noteAtFret :: Tuning -> StringNum -> FretNum -> Note
noteAtFret tuning stringnum = getPitchNote . pitchAtFret tuning stringnum

fretsWithNoteOnString :: Tuning -> StringNum -> Note -> [FretNum]
fretsWithNoteOnString tuning stringnum note = filter f [0..maxFret]
    where f fretnum = noteAtFret tuning stringnum fretnum `enharmonicEquiv` note

fretsWithPitchOnString :: Tuning -> StringNum -> Pitch -> [FretNum]
fretsWithPitchOnString tuning stringnum pitch = filter f [0..maxFret]
    where f fretnum = pitchAtFret tuning stringnum fretnum == pitch

fretsWithPitch :: Tuning -> Pitch -> [Fret]
fretsWithPitch tuning pitch = map g $ concatMap f (strings tuning)
     where f stringNum = map (\x -> (stringNum, x)) $ 
                             fretsWithPitchOnString tuning stringNum pitch
           g (stringNum, fretNum) = Fret stringNum fretNum

fretsWithNote :: Tuning -> Note -> [Fret]
fretsWithNote tuning note = map g $ concatMap f (strings tuning)
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

buildVoicing' :: [(Interval, Fret)] -> StringNum -> [Maybe (Interval, Fret)]
buildVoicing' chordFrets sn = Nothing : (map Just $ filter f chordFrets)
    where f (_, fret) = isOnString sn fret

voicingsInRange :: Tuning -> Chord -> Note -> FretRange -> [Voicing]
voicingsInRange tuning chord@(Chord is) note fr = sortBy (compare `on` difficulty) $
                                                  nub $ 
                                                  filter (hasIntervals is) $
                                                  map (putIntervalInBass Unison . catMaybes) $
                                                  mapM (buildVoicing' chordFrets) (strings tuning)
    where chordFrets = fretsForChordInRange tuning note chord fr

voicingsNearFret :: FretNum -> Tuning -> Chord -> Note -> [Voicing]
voicingsNearFret n tuning chord@(Chord is) note = sortBy (compare `on` sortKey) candidates
    where candidates = voicingsInRange tuning chord note fr
          fr = FretRange low high
          sortKey x = (difficulty x, distanceFromFret n x)
          halfRange = 4
          (low, high) = case n < halfRange of
               True -> (0, 2 * halfRange - n)
               False -> (n - halfRange, n + halfRange)

-- Actually check pitch later
intervalInBass :: Interval -> Voicing -> Bool
intervalInBass interval v = fst lowest == interval
    where lowest = minimumBy (compare `on` (stringNum . snd)) v

putIntervalInBass :: Interval -> Voicing -> Voicing
putIntervalInBass interval [] = []
putIntervalInBass interval vcg = case (fst . head) orderedV == interval of
         True -> orderedV
         False -> putIntervalInBass interval (tail orderedV)
    where orderedV = sortBy (compare `on` (stringNum . snd)) vcg

hasIntervals :: S.Set Interval  -> Voicing -> Bool
hasIntervals is v = is == (S.fromList $ map fst v)

-- Make this a more robust heuristic
difficulty :: Voicing -> Int
difficulty voicing = abs voicingDiameter
    where fretnums = map (fretNum . snd) voicing
          voicingDiameter = maximum fretnums - minimum fretnums

voicingFretRange :: Voicing -> FretRange
voicingFretRange v = FretRange minFretRelaxed maxFretRelaxed
    where voicingFrets = map (fretNum . snd) v
          minFret = minimum voicingFrets
          maxFret = maximum voicingFrets
          minFretRelaxed = max (minFret - 2) 0
          maxFretRelaxed = maxFret + 2

distanceFromFret :: FretNum -> Voicing -> Int
distanceFromFret n voicing = sum $ map (abs . subtract n . fretNum . snd) voicing

closestChordToVoicing :: Tuning -> Voicing -> Note -> Chord -> Voicing
closestChordToVoicing tuning v note chord = minimumBy (compare `on` voiceDistance tuning v) candidates
    where candidates = voicingsInRange tuning chord note (voicingFretRange v)

-- TODO: Implement this better later..
voiceDistance :: Tuning -> Voicing -> Voicing -> Int
voiceDistance tuning v v' = sum $ map distanceMoved vPitches
    where vPitches = map (pitchAtFret' tuning . snd) v
          vPitches' = map (pitchAtFret' tuning . snd) v'
          distanceMoved pitch = minimum $ map (difference pitch) vPitches'


