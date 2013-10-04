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
import Data.Either
import Data.Maybe

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe (Left _) = Nothing
eitherToMaybe (Right x) = Just x

stringToChord :: String -> Maybe (Note, Chord)
stringToChord s = eitherToMaybe $ parse chord "" s

chord :: Parser (Note, Chord)
chord = do
    cnote <- note
    (c, cq) <- chordQuality
    -- TODO: Add 6 here as a valid extension
    mExtensionInt <- optionMaybe $ 
                    liftM read $ 
                    choice (map (string . show) [13, 11, 9, 7])
    alts <- between (char '(') (char ')') (many alteration) <|>
            many alteration
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
    maybeOne <- optionMaybe $ char '1'
    case maybeOne of
         Just c -> do
             rest <- choice $ map string ["3", "2", "1", "0", ""]
             return $ intToInterval (read (c:rest))
         Nothing -> do
             res <- choice $ map (string . show) [9,8 .. 1]
             return $ intToInterval (read res)

alteration :: Parser Alteration
alteration = do
    acc <- accidental
    intval <- interval
    return (intval, acc)

chordQuality :: Parser (Chord, ChordQuality)
chordQuality = do
    -- TODO: Better way to do this? need lookahead
    maybeM <- optionMaybe $ char 'm'
    case maybeM of
         Just c -> do
             rest <- choice $ map string ["aj", "in", ""]
             return $ stringToChordQuality (c:rest)
         Nothing -> do
             res <- choice $ map string ["M",
                                    "-",
                                    "aug",
                                    "dim",
                                    "sus",
                                    "" ]
             return $ stringToChordQuality res

stringToChordQuality :: String -> (Chord, ChordQuality)
stringToChordQuality s | s `elem` ["maj", "M"] = (majorTriad, Major)
                       | s `elem` ["min", "-", "m"] = (minorTriad, Minor)
                       | s `elem` ["aug"] = (augTriad, Augmented)
                       | s `elem` ["dim"] = (dimTriad, Diminished)
                       | s `elem` ["sus"] = (chordFromList [Unison, Fifth], Suspended)
                       | s == "" = (majorTriad, Dominant)
                       | otherwise = error "You screwed up"



