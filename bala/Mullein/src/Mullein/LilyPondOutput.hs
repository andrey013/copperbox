{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Mullein.LilyPondOutput
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Pretty print LilyPond
--
--------------------------------------------------------------------------------


module Mullein.LilyPondOutput where

import Mullein.Duration
import qualified Mullein.LilyPondSyntax as L
import Mullein.Pitch
import Mullein.ScoreSyntax hiding ( Element )
import Mullein.Utils

import Text.PrettyPrint.Leijen 



class LilyPondElement e where
  outputLy :: e -> Doc

instance LilyPondElement L.Element where
  outputLy (L.Note p od)      = note p <> optDuration od
  outputLy (L.Rest od)        = char 'r' <> optDuration od
  outputLy (L.Spacer od)      = char 's' <> optDuration od
  outputLy (L.Chord _ _)      = text "Chord - TODO"
  outputLy (L.GraceNotes _)   = text "GraceNotes - TODO"



outputPart :: LilyPondElement e => Part e -> Doc
outputPart (Part as)          = vsep $ map outputPhrase as

outputPhrase :: LilyPondElement e => Phrase e -> Doc
outputPhrase (Phrase a)       = outputMotif a
outputPhrase (Repeated a)     = command "repeat" <+> text "volta 2"
                                                 <+> braces (outputMotif a)
outputPhrase (FSRepeat a x y) = command "repeat" <+> text "volta 2"
                                                 <+> braces (outputMotif a)
                                  <$> command "alternative"
                                  <+> braces (braces (outputMotif x)
                                                 <+> braces (outputMotif y))

outputMotif :: LilyPondElement e => Motif e -> Doc
outputMotif (Motif _ bs)      = hsep $ punctuate (text " |") (map outputBar bs)

outputBar :: LilyPondElement e => Bar e -> Doc
outputBar (Bar a)             = outputUnison a
outputBar (Overlay a as)      = overlay $ outputUnison a : map outputUnison as


outputUnison :: LilyPondElement e => Unison e -> Doc
outputUnison (Unison ps tied) = hsep (map outputBracket ps) <>
                                   if tied then char '~' else empty

outputBracket :: LilyPondElement e => Bracket e -> Doc
outputBracket (Singleton e)   = outputLy e
outputBracket (Bracket es)    = lyBeam $ map outputLy es




--------------------------------------------------------------------------------
-- helpers

overlay :: [Doc] -> Doc
overlay = dblangles . vsep . punctuate (text " \\")    


note :: Pitch -> Doc 
note (Pitch l a o) = pitchLabel l a <> ove o where
    ove i | i > 0       = text $ replicate i       '\''
          | i < 0       = text $ replicate (abs i) ','
          | otherwise   = empty



-- lilypond middle c is c' 
-- HNotate middle c is c4
rescale :: Pitch -> Pitch
rescale (Pitch l a o)   = Pitch l a (o-3)

pitchLabel :: PitchLetter -> Accidental -> Doc
pitchLabel l a = char (toLowerLChar l) <> accidental a
  where 
    accidental :: Accidental -> Doc
    accidental Nat            = empty
    accidental Sharp          = text "is"
    accidental Flat           = text "es"
    accidental DoubleSharp    = text "isis"
    accidental DoubleFlat     = text "eses"


optDuration :: Maybe Duration -> Doc
optDuration = maybe empty df where
    df 0   = empty
    df drn = let (n,d,dc) = pdElements $ augDuration drn
             in dots dc $ durn n d

    durn 4 1      = command "longa"
    durn 2 1      = command "breve"
    durn 1 i      = int i
    -- TODO - ideally we shouldn't have 'error' errors here, we should be
    -- using throwError. But that means making a lot of pure code monadic
    -- ... is there another way to do it?
    durn n d      = error $ "lyDuration failed on - " ++ show n ++ "%" ++ show d

    dots :: Int -> (Doc -> Doc)
    dots i | i > 0     = (<> text (replicate i '.'))
           | otherwise = id


lyBeam :: [Doc] -> Doc
lyBeam (x:xs) = x <> char '[' <+> hsep xs <> char ']'
lyBeam []     = empty

command :: String -> Doc
command = (char '\\' <>) . text 

