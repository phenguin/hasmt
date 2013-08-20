module Chord where

import Interval
import Note
import qualified Data.Set as S
import Data.List

newtype Chord = Chord { intervals :: S.Set(Interval) }

-- Instances
instance Show Chord where
    show (Chord intervalSet) = "<Chord:(" ++ (unwords $ map show $ S.toList intervalSet) ++ ")>"

-- Construction
chordFromList :: [Interval] -> Chord
chordFromList = Chord . S.fromList

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
