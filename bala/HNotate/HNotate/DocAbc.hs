--------------------------------------------------------------------------------
-- |
-- Module      :  HNotate.DocAbc
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined
--
-- Output functions for /doc mode/ LilyPond 
--
--------------------------------------------------------------------------------

module HNotate.DocAbc where

import HNotate.Document
import HNotate.Duration
import HNotate.Env
import HNotate.MusicRepDatatypes
import HNotate.NoteListDatatypes
import HNotate.Pitch
import HNotate.TemplateDatatypes


xField :: Int -> HoasExprD -> HoasExprD
xField i expr = HText doc expr where
    doc = field 'X' (int i)


titleField :: String -> HoasExprD -> HoasExprD
titleField name expr = HText doc expr where
    doc = field 'T' (text name)
    
      
meterField :: Int -> Int -> HoasExprD -> HoasExprD
meterField n d expr = HLet update doc expr where
    tsig    = TimeSig n d
    update  = set_current_meter tsig
    doc     = field 'M' (meter tsig)

              
keyField :: PitchLabel -> Mode -> HoasExprD -> HoasExprD
keyField l m expr = HLet update doc expr where
    update  = set_current_key $ Key l m [] 
    doc     = field 'K' (pitchLabel l UPPER <+> mode m)

output :: String -> HoasExprD
output name = HDo directive where
    directive = OutputDirective Nothing name


--------------------------------------------------------------------------------
-- elementary printers

field :: Char -> ODoc -> ODocS
field ch doc = lineS (char ch <> colon <> doc) 


meter :: Meter -> ODoc
meter (TimeSig n d) = int n <> char '/' <> int d
meter CommonTime    = text "C"
meter CutTime       = text "C|"

data PitchChar = UPPER | LOWER
  deriving (Eq,Show)
  
pitch :: Pitch -> ODoc
pitch (Pitch l a o) 
    | o > 4     = pitchLabel (PitchLabel l a) LOWER  <> octave o 
    | otherwise = pitchLabel (PitchLabel l a) UPPER <> octave o 
  where
    octave :: Int -> ODoc
    octave i  | i > 5       = text (replicate (i-5) '\'') 
              | i < 4       = text (replicate (4-i) ',')
              | otherwise   = emptyDoc


pitchLabel :: PitchLabel -> PitchChar -> ODoc
pitchLabel (PitchLabel l a) pc 
    | pc == LOWER   = accidental a <> (char . toLowerLChar) l
    | otherwise     = accidental a <> (char . toUpperLChar) l
  where     
    accidental :: Accidental -> ODoc
    accidental Nat           = emptyDoc    
    accidental Sharp         = char '^' 
    accidental Flat          = char '_' 
    accidental DoubleSharp   = text "^^"
    accidental DoubleFlat    = text "__"


     
mode :: Mode -> ODoc
mode Major        = text "maj" 
mode Minor        = text "min"
mode Lydian       = text "lyd"
mode Ionian       = text "ion" 
mode Mixolydian   = text "mix"
mode Dorian       = text "dor"
mode Aeolian      = text "aeo"
mode Phrygian     = text "phr"
mode Locrian      = text "loc"


