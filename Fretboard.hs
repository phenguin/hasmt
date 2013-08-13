module Fretboard where

import Note
import Pitch

type Tuning = [Pitch]

-- TODO: Automate this later.. with less jank
standardTuning = [Pitch 0 7, -- Low E
                  Pitch 1 0, -- A
                  Pitch 1 5, -- D
                  Pitch 1 10, -- G
                  Pitch 2 2, -- B
                  Pitch 2 7] -- E

type Fret = Int
type StringNum = Int

noteAtFret tuning stringnum fret = (pcToNote . getPitchClass) $ pitchAtFret tuning stringnum fret

pitchAtFret :: Tuning -> StringNum -> Fret -> Pitch
pitchAtFret tuning string fret = moveSemitones fret openPitch
    where openPitch = tuning !! string
