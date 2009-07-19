{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Mullein.AbcOutput
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Pretty print ABC.
--
--------------------------------------------------------------------------------


module Mullein.AbcOutput where

import Mullein.Core
import Mullein.Duration
import Mullein.Pitch

import Data.OneMany

import Text.PrettyPrint.Leijen hiding ( (<$>) )



class AbcOutput e where
  type AbcDur e :: *
  abcNote  :: e -> AbcDur e -> Doc
  abcPitch :: e -> Doc


class AbcGlyph e where
  abcGlyph :: e -> Doc



instance AbcGlyph (ElementP ScNote) where
  abcGlyph = oElement  

instance AbcOutput Pitch where
  type AbcDur Pitch = Duration
  abcNote p d = printNote p d
  abcPitch = pitch

instance AbcOutput ScNote where
  type AbcDur ScNote = Duration
  abcNote (ScNote p _) d = printNote p d
  abcPitch (ScNote p _) = pitch p




oBarOverlay :: AbcGlyph e => (Bool,[OneMany e]) -> Doc
oBarOverlay (tied,xs) = hsep (map omBeam xs) <> if tied then char '~' else empty


omBeam :: AbcGlyph e => OneMany e -> Doc
omBeam = oneMany abcGlyph (hcat . map abcGlyph) 

oBracket :: (AbcOutput e, AbcDur e ~ Duration) => OneMany (ElementP e) -> Doc
oBracket = oneMany oElement (hcat . map oElement)


oElement :: (AbcOutput e, AbcDur e ~ Duration) => ElementP e -> Doc
oElement (Note dm p)      = abcNote p dm
oElement (Rest dm)        = char 'z' <> multiplier dm
oElement (Spacer dm)      = char 'x' <> multiplier dm
oElement (Chord dm ps)    = brackets $ hcat $ map f ps where
                              f p = abcPitch p <> multiplier dm 
oElement (GraceNotes xs)  = braces $ hcat $ map f xs where
                              f (p,dm) = abcPitch p <> multiplier dm
  

--------------------------------------------------------------------------------
-- helpers




keyField :: Key -> Doc
keyField (Key k m) = field 'K' keyspec where
    keyspec        = pitchLabel k UPPER <> modeSpec m
    -- Don't print \maj\ here
    modeSpec Major = empty
    modeSpec x     = mode x 


meterField :: Meter -> Doc
meterField = field 'M' . f where
    f (TimeSig n d) = integer n <> char '/' <> integer d
    f CommonTime    = text "C"
    f CutTime       = text "C|"


field :: Char -> Doc -> Doc
field ch d = char ch <> colon <> d





overlay :: [Doc] -> Doc
overlay = vsep . punctuate (text " & ")    

printNote :: Pitch -> Duration -> Doc 
printNote p m = pitch p <> multiplier m


data PitchChar = UPPER | LOWER
  deriving (Eq,Show)


-- Mullein - middle c is C5
-- Abc - middle c is 'C' upper case c  
pitch :: Pitch -> Doc
pitch (Pitch l a o) 
    | o > 5     = pitchLabel (PitchLabel l a) LOWER <> octave o 
    | otherwise = pitchLabel (PitchLabel l a) UPPER <> octave o 
  where
    octave :: Int -> Doc
    octave i  | i > 6       = text (replicate (i-6) '\'') 
              | i < 5       = text (replicate (5-i) ',')
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

mode :: Mode -> Doc
mode Major        = text "maj"
mode Minor        = text "min"
mode Lydian       = text "lyd"
mode Ionian       = text "ion" 
mode Mixolydian   = text "mix"
mode Dorian       = text "dor"
mode Aeolian      = text "aeo"
mode Phrygian     = text "phr"
mode Locrian      = text "loc"


multiplier :: Duration -> Doc
multiplier _dn = int 2

{-    
multiplier :: Duration -> Doc
multiplier dn | dn == 1   = empty
              | otherwise = fn (numerator dn, denominator dn)
  where
    fn (n,1) = integer n
    fn (1,d) = char '/' <> integer d
    fn (n,d) = integer n <> char '/' <> integer d

-}
