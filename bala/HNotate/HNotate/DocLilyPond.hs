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

lilypond :: [HoasExprD] -> DocuHoas
lilypond ds = Hoas ds

lilypond1 :: HoasExprD -> DocuHoas
lilypond1 d = Hoas [d]
 
version :: DocS
version expr = HText doc expr where
    doc = lineS $ command "version" <+> dblquotes (text "2.10.33")

    
book :: DocS     
book expr = HText doc expr where
    doc d = command "book" <+> (bracesLines d)
    
    
relative :: Pitch -> DocS
relative p expr = HLet update doc expr where
    update  = set_relative_pitch p 
    doc d   = command "relative" <+> pitch (rescale p) <+> (bracesLines d)
  
time :: Int -> Int -> DocS
time n d expr = HLet update doc expr where
    tsig    = TimeSig n d
    update  = set_current_meter tsig
    doc     = lineS $ command "time" <+> meter tsig


  
definition :: String -> DocS
definition s expr = HText doc expr where
    doc d   = text s <+> equals <> indent 1 d 
              
key :: PitchLabel -> Mode -> DocS
key l m expr = HLet update doc expr where
    update  = set_current_key (Key l m [])
    doc     = lineS $ command "key" <+> pitchLabel l <+> mode m


-- output

outputRelative :: String -> HoasExprD
outputRelative name = HDo directive where
    directive = OutputDirective (Just OutputRelative) name

outputAbsolute :: String -> HoasExprD
outputAbsolute name = HDo directive where
    directive = OutputDirective (Just OutputDefault) name



--------------------------------------------------------------------------------
-- elementary printers


meter (TimeSig n d) = int n <> char '/' <> int d
meter CommonTime    = int 4 <> char '/' <> int 4
meter CutTime       = int 2 <> char '/' <> int 2


-- The octave value of a pitch needs to be rescaled /before/ pitch is called

pitch :: Pitch -> ODoc
pitch (Pitch l a o) = pitchLabel (PitchLabel l a) <> ove o
  where 
    ove o | o > 0       = text $ replicate o       '\'' 
          | o < 0       = text $ replicate (abs o) ','
          | otherwise   = emptyDoc

-- lilypond middle c is c' 
-- HNotate middle c is c4

rescale (Pitch l a o)   = Pitch l a (o-3)

pitchLabel :: PitchLabel -> ODoc
pitchLabel (PitchLabel l a) = char (toLowerLChar l) <> accidental a
  where 
    accidental :: Accidental -> ODoc
    accidental Nat            = emptyDoc
    accidental Sharp          = text "is"
    accidental Flat           = text "es"
    accidental DoubleSharp    = text "isis"
    accidental DoubleFlat     = text "eses"


    
duration :: Duration -> ODoc
duration drn
    | drn == no_duration  = emptyDoc
    | otherwise           = let (n,d,dc) = pdElements $ printableDuration drn 
                            in dots dc $ durn n d
  where 
    durn 4 1      = command "longa"  
    durn 2 1      = command "breve" 
    durn 1 i      = int i
    -- TODO - ideally we shouldn't have 'error' errors here, we should be 
    -- using throwError. But that means making a lot of pure code monadic 
    -- ... is there another way to do it? 
    durn n d      = error $ "durationD failed on - " ++ show n ++ "%" ++ show d
    
    dots :: Int -> ODoc -> ODoc
    dots i | i > 0     = (<> text (replicate i '.'))
           | otherwise = id
           
                
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

