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

bracesLines :: ODoc -> ODoc
bracesLines = braces' -- enclose (lbrace <> line) (line <> rbrace)

 
relative :: Pitch -> HoasExprD -> HoasExprD
relative p expr =
  HLet (set_relative_pitch p) 
       (\d -> command "relative" <+> pitchOutput p <+> (bracesLines d)) expr
  
meter :: Meter -> HoasExprD -> HoasExprD
meter m expr = 
  HLet (set_current_meter m) 
       (prefixLine $ command "time" <+> meterOutput m) expr

meterOutput (TimeSig n d) = int n <> char '/' <> int d
meterOutput CommonTime    = int 4 <> char '/' <> int 4
meterOutput CutTime       = int 2 <> char '/' <> int 2
  
definition :: String -> HoasExprD -> HoasExprD
definition s e = HText (\d -> text s <+> equals <> indent 1 d) e 
              
key :: PitchLabel -> Mode -> HoasExprD -> HoasExprD
key l m expr = 
  HLet (set_current_key (Key l m [])) 
       (prefixLine $ command "key" <+> pitchLabel l <+> mode m) expr

output :: OutputScheme -> String -> HoasExprD
output scm name = HDo (OutputDirective (Just scm) name)

prefixLine :: ODoc -> (ODoc -> ODoc)
prefixLine d = (d <&\>)

pitchOutput :: Pitch -> ODoc
pitchOutput (Pitch l a o) = pitchLabel (PitchLabel l a) <> ove o
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


