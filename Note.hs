module Note (
Note(C,B,D,E,F,G,A)
) where

type PitchClass = Int
type Octave = Int

class HavingPitchClass a where
    getPitchClass :: a -> PitchClass

instance HavingPitchClass Note where
    getPitchClass note = case note of
        C -> 0
        D -> 2
        E -> 4
        F -> 5
        G -> 7
        A -> 9
        B -> 11
        Sharped note -> (getPitchClass note + 1) `mod` 12
        Flatted note -> (getPitchClass note - 1) `mod` 12

data Note = C | D | E | F | G | A | B | Sharped Note | Flatted Note  deriving (Eq)

instance Show Note where
    show note = case note of
        C -> "C"
        D -> "D"
        E -> "E"
        F -> "F"
        G -> "G"
        A -> "A"
        B -> "B"
        Sharped note -> show note ++ "#"
        Flatted note -> show note ++ "b"

raise :: Note -> Note
raise (Flatted note) = note
raise note = Sharped note

sharpen = raise

lower :: Note -> Note
lower (Sharped note) = note
lower note = Flatted note

flatten = lower

data Accidental = Flat | Natural | Sharp deriving (Show, Read, Eq, Ord, Enum)

