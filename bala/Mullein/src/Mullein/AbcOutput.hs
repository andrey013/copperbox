{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Mullein.AbcOutput
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Pretty print Abc
--
--------------------------------------------------------------------------------


module Mullein.AbcOutput where

import Mullein.Duration
import qualified Mullein.AbcSyntax as A
import Mullein.Pitch
import Mullein.ScoreSyntax hiding ( Element )

import Data.Ratio
import Text.PrettyPrint.Leijen 



class AbcElement e where
  outputAbc :: e -> Doc

instance AbcElement A.Element where
  outputAbc (A.Note p dm)      = note p dm
  outputAbc (A.Rest dm)        = char 'z' <> multiplier dm
  outputAbc (A.Spacer dm)      = char 'x' <> multiplier dm
  outputAbc (A.Chord _ _)      = text "Chord - TODO"
  outputAbc (A.GraceNotes _)   = text "GraceNotes - TODO"



outputPart :: AbcElement e => Part e -> Doc
outputPart (Part as)          = vsep $ map outputPhrase as

outputPhrase :: AbcElement e => Phrase e -> Doc
outputPhrase (Phrase a)       = outputMotif a
outputPhrase (Repeated a)     = text "|:" <+> outputMotif a <+> text ":|"
outputPhrase (FSRepeat a x y) = text "|:" <+> outputMotif a
                                          <+> text "|[1"  <+> outputMotif x
                                          <+> text ":|[2" <+> outputMotif y
                                          <+> text "|]"

outputMotif :: AbcElement e => Motif e -> Doc
outputMotif (Motif _ bs)      = hsep $ punctuate (text " |") (map outputBar bs)

outputBar :: AbcElement e => Bar e -> Doc
outputBar (Bar a)             = outputUnison a
outputBar (Overlay a as)      = overlay $ outputUnison a : map outputUnison as


outputUnison :: AbcElement e => Unison e -> Doc
outputUnison (Unison ps tied) = hsep (map outputBracket ps) <>
                                   if tied then char '-' else empty

outputBracket :: AbcElement e => Bracket e -> Doc
outputBracket (Singleton e)   = outputAbc e
outputBracket (Bracket es)    = hcat $ map outputAbc es




--------------------------------------------------------------------------------
-- helpers



overlay :: [Doc] -> Doc
overlay = vsep . punctuate (text " & ")    

note :: Pitch -> A.Multiplier -> Doc 
note p m = pitch p <> multiplier m


data PitchChar = UPPER | LOWER
  deriving (Eq,Show)
  
pitch :: Pitch -> Doc
pitch (Pitch l a o) 
    | o > 4     = pitchLabel (PitchLabel l a) LOWER <> octave o 
    | otherwise = pitchLabel (PitchLabel l a) UPPER <> octave o 
  where
    octave :: Int -> Doc
    octave i  | i > 5       = text (replicate (i-5) '\'') 
              | i < 4       = text (replicate (4-i) ',')
              | otherwise   = empty


pitchLabel :: PitchLabel -> PitchChar -> Doc
pitchLabel (PitchLabel l a) pc 
    | pc == LOWER   = accidental a <> (char . toLowerLChar) l
    | otherwise     = accidental a <> (char . toUpperLChar) l
  where     
    accidental :: Accidental -> Doc
    accidental Nat           = empty    
    accidental Sharp         = char '^' 
    accidental Flat          = char '_' 
    accidental DoubleSharp   = text "^^"
    accidental DoubleFlat    = text "__"
    
multiplier :: Duration -> Doc
multiplier dn | dn == 1   = empty
              | otherwise = fn (numerator dn, denominator dn)
  where
    fn (n,1) = integer n
    fn (1,d) = char '/' <> integer d
    fn (n,d) = integer n <> char '/' <> integer d
    

