module Hasmt.ChordParser where

import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Combinator 
import Text.Parsec.Char

import Data.Char (toUpper)
import Control.Monad
import Hasmt.Note
import Hasmt.Chord
import Hasmt.Interval

chord :: Parser (Note, Chord)
chord = do
    cnote <- note
    (c, cq) <- chordQuality
    -- TODO: Add 6 here as a valid extension
    mExtensionInt <- optionMaybe $ 
                    liftM read $ 
                    choice (map (string . show) [13, 11, 9, 7])
    alts <- many alteration
    let extended = case mExtensionInt of
         Nothing -> c
         Just extensionInt -> extendTo cq extensionInt c
        altered = foldr applyAlteration extended alts
    return (cnote, altered)

note :: Parser Note
note = do
    noteChar <- oneOf "ABCDEFGabcdefg"
    maybeAccidental <- optionMaybe $ oneOf "#b"
    case maybeAccidental of
         Nothing -> return $ read [noteChar]
         Just a -> return $ read [noteChar, a]

accidental :: Parser Accidental
accidental = do
    accidentalC <- oneOf "#b"
    case accidentalC of
         '#' -> return Sharp
         'b' -> return Flat
         _ -> error "Parser error"

interval :: Parser Interval
interval = do
    intervalS <- choice $ map (string . show) [13,12 .. 1]
    return $ intToInterval (read intervalS)

alteration :: Parser Alteration
alteration = do
    acc <- accidental
    intval <- interval
    return (intval, acc)

chordQuality :: Parser (Chord, ChordQuality)
chordQuality = do
    q <- choice [ string "maj"
                 , string "M"
                 , string "min"
                 , string "m"
                 , string "-"
                 , string "aug"
                 , string "dim"
                 , string "min"
                 , string "sus"
                 , string ""]
    return $ stringToChordQuality q

stringToChordQuality :: String -> (Chord, ChordQuality)
stringToChordQuality s | s `elem` ["maj", "M", "m"] = (majorTriad, Major)
                       | s `elem` ["min", "-"] = (minorTriad, Minor)
                       | s `elem` ["aug"] = (augTriad, Augmented)
                       | s `elem` ["dim"] = (dimTriad, Diminished)
                       | s `elem` ["sus"] = (chordFromList [Unison, Fifth], Suspended)
                       | s == "" = (majorTriad, Dominant)
                       | otherwise = error "You screwed up"



