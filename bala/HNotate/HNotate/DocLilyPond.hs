--------------------------------------------------------------------------------
-- |
-- Module      :  HNotate.DocLilyPond
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

module HNotate.DocLilyPond where

import HNotate.Document
import HNotate.Duration
import HNotate.Env
import HNotate.MusicRepDatatypes
import HNotate.NoteListDatatypes
import HNotate.Pitch
import HNotate.TemplateDatatypes


 
relative :: Pitch -> HoasExprD -> HoasExprD
relative p expr = HLet update doc expr where
    update  = set_relative_pitch p 
    doc     = \d -> command "relative" <+> pitch p <+> (bracesLines d)
  
time :: Int -> Int -> HoasExprD -> HoasExprD
time n d expr = HLet update doc expr where
    tsig    = TimeSig n d
    update  = set_current_meter tsig
    doc     = lineS $ command "time" <+> meter tsig


  
definition :: String -> HoasExprD -> HoasExprD
definition s expr = HText doc expr where
    doc     = \d -> text s <+> equals <> indent 1 d 
              
key :: PitchLabel -> Mode -> HoasExprD -> HoasExprD
key l m expr = HLet update doc expr where
    update  = set_current_key (Key l m [])
    doc     = lineS $ command "key" <+> pitchLabel l <+> mode m



relativeOutput :: String -> HoasExprD
relativeOutput name = HDo directive where
    directive = OutputDirective (Just OutputRelative) name

absoluteOutput :: String -> HoasExprD
absoluteOutput name = HDo directive where
    directive = OutputDirective (Just OutputDefault) name



--------------------------------------------------------------------------------
-- elementary printers


meter (TimeSig n d) = int n <> char '/' <> int d
meter CommonTime    = int 4 <> char '/' <> int 4
meter CutTime       = int 2 <> char '/' <> int 2

pitch :: Pitch -> ODoc
pitch (Pitch l a o) = pitchLabel (PitchLabel l a) <> ove o
  where 
    ove o | o == 3  = emptyDoc
          | o <  3  = text $ replicate (3 - o) ','
          | o >  3  = text $ replicate (o - 3) '\''

pitchLabel :: PitchLabel -> ODoc
pitchLabel (PitchLabel l a) = char (toLowerLChar l) <> accidental a
  where 
    accidental :: Accidental -> ODoc
    accidental Nat            = emptyDoc
    accidental Sharp          = text "is"
    accidental Flat           = text "es"
    accidental DoubleSharp    = text "isis"
    accidental DoubleFlat     = text "eses"
     
mode :: Mode -> ODoc
mode Major        = text "major" 
mode Minor        = text "minor"
mode Lydian       = text "lydian"
mode Ionian       = text "ionian" 
mode Mixolydian   = text "mixolydian"
mode Dorian       = text "dorian"
mode Aeolian      = text "aeolian"
mode Phrygian     = text "phrygian"
mode Locrian      = text "locrian"


bracesLines :: ODoc -> ODoc
bracesLines d = lbrace <&\> (indent 2 d) <&\> rbrace

