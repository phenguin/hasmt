module Hasmt.Chord where

import Hasmt.Interval
import Hasmt.Note
import qualified Data.Set as S
import Data.List

newtype Chord = Chord { intervals :: S.Set(Interval) } deriving (Eq)

type Alteration = (Interval, Accidental)

data ChordQuality = Major | Minor | Dominant | Suspended | Augmented | Diminished deriving (Eq, Show)

-- Instances
instance Show Chord where
    show (Chord intervalSet) = "<Chord:(" ++ (unwords $ map show $ S.toList intervalSet) ++ ")>"

-- Construction
chordFromList :: [Interval] -> Chord
chordFromList = Chord . S.fromList

-- Ghetto.. fix this later
extendTo :: ChordQuality -> Int -> Chord -> Chord
extendTo cq n chord@(Chord ints) = Chord $ foldr S.insert ints (extensions cq n)

-- Ghetto.. fix this later
extensions :: ChordQuality -> Int -> [Interval]
extensions Major 7 = [Seventh]
extensions Major 9 = [Seventh, Second]
extensions Major 11 = [Seventh, Second, Fourth]
extensions Major 13 = [Seventh, Second, Fourth, Sixth]

extensions Minor 7 = [flat Seventh]
extensions Minor 9 = [flat Seventh, Second]
extensions Minor 11 = [flat Seventh, Second, Fourth]
extensions Minor 13 = [Seventh, Second, Fourth, Sixth]

extensions Dominant 7 = [flat Seventh]
extensions Dominant 9 = [flat Seventh, Second]
extensions Dominant 11 = [flat Seventh, Second, Fourth]
extensions Dominant 13 = [Seventh, Second, Fourth, Sixth]

extensions Suspended 2 = [Second]
extensions Suspended 4 = [Fourth]
extensions Suspended 9 = [Second]
extensions Suspended 11 = [Fourth]

-- TODO: Check this.. melodic minor modes ughh
extensions Augmented 7 = [Seventh]
extensions Augmented 9 = [flat Seventh, Second]
extensions Augmented 11 = [flat Seventh, Second, Fourth]
extensions Augmented 13 = [Seventh, Second, Fourth, Sixth]

-- TODO: Check this too..
extensions Diminished 7 = [flat Seventh]
extensions Diminished 9 = [flat Seventh, Second]
extensions Diminished 11 = [flat Seventh, Second, Fourth]
extensions Diminished 13 = [flat Seventh, Second, Fourth, Sixth]

extensions _ _ = []

applyAlteration :: Alteration -> Chord -> Chord
applyAlteration (i, a) (Chord intset) = Chord $ S.insert (applyAccidental a i) $ 
                                                S.filter (/= i) $ intset

chordIntervalList :: Chord -> [Interval]
chordIntervalList (Chord iset) = S.toList iset

-- Specfic chord spellings
spellChordForRoot :: Note -> Chord -> [(Interval, Note)]
spellChordForRoot note chord = map f $ S.toList (intervals chord)
    where f interval = (interval, noteAbove note interval)

-- Triads

majorTriad :: Chord
majorTriad = chordFromList [Unison, Third, Fifth]

minorTriad :: Chord
minorTriad = chordFromList [Unison, flat Third, Fifth]

augTriad :: Chord
augTriad = chordFromList [Unison, Third, sharp Fifth]

dimTriad :: Chord
dimTriad = chordFromList [Unison, flat Third, flat Fifth]

-- Basic Seventh Chords

maj7chord :: Chord
maj7chord  = chordFromList [Unison, Third, Fifth, Seventh]

dom7chord :: Chord
dom7chord  = chordFromList [Unison, Third, Fifth, flat Seventh]

min7chord :: Chord
min7chord  = chordFromList [Unison, flat Third, Fifth, flat Seventh]

halfdim7chord :: Chord
halfdim7chord  = chordFromList [Unison, flat Third, flat Fifth, flat Seventh]

fulldim7chord :: Chord
fulldim7chord  = chordFromList [Unison, flat Third, flat Fifth, flat $ flat Seventh]
