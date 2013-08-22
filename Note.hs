module Note (
Note(C,B,D,E,F,G,A),
(~=), HavingPitchClass(..), PitchClass, 
raise, sharpen, lower, flatten, EnharmonicEquiv(..),
pcToNote, pcToNoteWithBase, nextNat, prevNat
) where

import Data.Char (toUpper)
import Control.Monad
import Data.Function (on)

type PitchClass = Int

data Accidental = Flat | Natural | Sharp deriving (Show, Read, Eq, Ord, Enum)

class EnharmonicEquiv a where
    enharmonicEquiv :: a -> a -> Bool

(~=) :: (EnharmonicEquiv a) => a -> a -> Bool
(~=) = enharmonicEquiv

class HavingPitchClass a where
    getPitchClass :: a -> PitchClass

instance HavingPitchClass Note where
    getPitchClass note = case note of
        A -> 0
        B -> 2
        C -> 3
        D -> 5
        E -> 7
        F -> 8
        G -> 10
        Sharpened note -> (getPitchClass note + 1) `mod` 12
        Flattened note -> (getPitchClass note - 1) `mod` 12

-- Canonical.. not bijective due to enharmonic equivalence
pcToNote :: PitchClass -> Note
pcToNote n = case n `mod` 12 of
                  0 -> A
                  1 -> flatten B
                  2 -> B
                  3 -> C
                  4 -> flatten D
                  5 -> D
                  6 -> flatten E
                  7 -> E
                  8 -> F
                  9 -> flatten G
                  10 -> G
                  11 -> flatten A

-- Removes enharmonic ambiguity by specifying a base note
pcToNoteWithBase :: PitchClass -> Note -> Note
pcToNoteWithBase pc base = rebaseNote (pcToNote pc) base

instance EnharmonicEquiv Note where
    enharmonicEquiv n n' = getPitchClass n == getPitchClass n'

data Note = C | D | E | F | G | A | B | Sharpened Note | Flattened Note  deriving (Eq)

stripAccidentals :: Note -> Note
stripAccidentals (Sharpened n) = n
stripAccidentals (Flattened n) = n
stripAccidentals n = n

-- Finds next letter name up from a given note, ignoring and stripping accidentals
nextNat :: Note -> Note
nextNat (Sharpened n) = nextNat n
nextNat (Flattened n) = nextNat n
nextNat A = B
nextNat B = C
nextNat C = D
nextNat D = E
nextNat E = F
nextNat F = G
nextNat G = A

-- Finds prev letter name up from a given note, ignoring and stripping accidentals
prevNat :: Note -> Note
prevNat (Sharpened n) = prevNat n
prevNat (Flattened n) = prevNat n
prevNat B = A
prevNat C = B
prevNat D = C
prevNat E = D
prevNat F = E
prevNat G = F
prevNat A = G

-- Rebase one note to another with appropriate accidentals
-- i.e. rebaseNote D E = Flat (Flat E)
-- TODO: test this.. sketchy fast written logic
rebaseNote :: Note -> Note -> Note
rebaseNote note base' = let base = stripAccidentals base' in
        rebaseNote' base base
    where rebaseNote' note base = let curPC = getPitchClass note 
                                      diff = (curPC - targetPC ) `mod` 12
                                      diff' = (targetPC - curPC) `mod` 12 in
              case diff of
                   0 -> note
                   _ -> if diff' <= diff then
                                         rebaseNote' (sharpen note) base
                                         else
                                         rebaseNote' (flatten note) base
          targetPC = getPitchClass note



instance Ord Note where
    compare = compare `on` getPitchClass

instance Show Note where
    show note = case note of
        C -> "C"
        D -> "D"
        E -> "E"
        F -> "F"
        G -> "G"
        A -> "A"
        B -> "B"
        Sharpened note -> show note ++ "#"
        Flattened note -> show note ++ "b"

instance Read Note where
    readsPrec _ (l:accidentals) = case l `elem` "abcdefgABCDEFG" of
            True -> let ans = foldM acc (note (toUpper l)) accidentals in
                        case ans of
                             Nothing -> []
                             Just note' -> [(note', [])]
            False -> []
       where note 'C' = C
             note 'D' = D
             note 'E' = E
             note 'F' = F
             note 'G' = G
             note 'A' = A
             note 'B' = B
             acc note 'b' = Just $ flatten note
             acc note '#' = Just $ sharpen note
             acc _ _ = Nothing


raise :: Note -> Note
raise (Flattened note) = note
raise note = Sharpened note

sharpen = raise

lower :: Note -> Note
lower (Sharpened note) = note
lower note = Flattened note

flatten = lower

