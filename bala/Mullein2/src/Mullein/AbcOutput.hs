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
import Mullein.Pitch hiding (pitch, octave)

import Data.OneMany

import Text.PrettyPrint.Leijen hiding ( (<$>) )

import Data.Ratio


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




oBarOverlay :: AbcGlyph e => (Tied,[OneMany e]) -> Doc
oBarOverlay (ptied,xs) = hsep (map omBeam xs) <> if ptied then char '~' else empty


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
                              f (GraceNote dm p) = abcPitch p <> multiplier dm
  

--------------------------------------------------------------------------------
-- helpers




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
    | pc == LOWER   = (maybe empty accidental a) <> (char . toLowerLChar) l
    | otherwise     = (maybe empty accidental a) <> (char . toUpperLChar) l
  where     
    accidental :: Accidental -> Doc
    accidental Nat           = char '='    
    accidental Sharp         = char '^' 
    accidental Flat          = char '_' 
    accidental DoubleSharp   = text "^^"
    accidental DoubleFlat    = text "__"


multiplier :: Duration -> Doc
multiplier = df . abc (1%16) where
  df []           = empty
  df [(Unit)]     = empty
  df [(Mult n)]   = integer n
  df [(Div n)]    = char '/' <> integer n
  df [(Frac n d)] = integer n <> char '/' <> integer d
  df _xs          = error $ "multiplier - composite todo..." 


