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
import HNotate.Pitch
import HNotate.TemplateDatatypes

lilypond :: [HoasExprD] -> DocuHoas
lilypond ds = Hoas ds

lilypond1 :: HoasExprD -> DocuHoas
lilypond1 d = Hoas [d]

noExpr :: HoasExprD
noExpr = HText0 doc where
    doc = nextS $ emptyDoc
 
version :: HoasExprD
version = HText0 doc where
    doc = lineS $ command "version" <+> dblquotes (text "2.10.33")

header :: DocS     
header expr = HText doc expr where
    doc d = command "header" <+> (bracesLines d)

title :: String -> DocS 
title s expr = HText doc expr where
    doc = lineS $ text "title" <+> equals <> dblquotes (text s)     
        
book :: DocS     
book expr = HText doc expr where
    doc d = command "book" <+> (bracesLines d)

score :: DocS     
score expr = HText doc expr where
    doc d = command "score" <+> (bracesLines d)

drummode :: DocS     
drummode expr = HText doc expr where
    doc d = command "drummode" <+> (bracesLines d)

set :: String -> DocS    
set ss expr = HText doc expr where
    doc d = command "set" <+> text ss <+> d
    
    
new :: String -> DocS
new name expr = HText doc expr where
    doc d = command "new" <+> text name <+> d


doubleAngles :: DocS
doubleAngles expr = HText doc expr where
    doc d = dblangles' d

expression :: DocS
expression expr = HText doc expr where
    doc d = braces' d
    
lycommand :: String -> DocS
lycommand name expr = HText doc expr where
    doc d = command name <+> d    
    
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
    
invocation :: String -> HoasExprD   
invocation s = HText0 doc where
    doc = lineS $ command s
    
                  
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

meter :: Meter -> ODoc
meter (TimeSig n d) = int n <> char '/' <> int d
meter CommonTime    = int 4 <> char '/' <> int 4
meter CutTime       = int 2 <> char '/' <> int 2


-- The octave value of a pitch needs to be rescaled /before/ pitch is called

pitch :: Pitch -> ODoc
pitch (Pitch l a o) = pitchLabel (PitchLabel l a) <> ove o
  where 
    ove i | i > 0       = text $ replicate i       '\'' 
          | i < 0       = text $ replicate (abs i) ','
          | otherwise   = emptyDoc

-- lilypond middle c is c' 
-- HNotate middle c is c4
rescale :: Pitch -> Pitch
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
mode Major        = command "major" 
mode Minor        = command "minor"
mode Lydian       = command "lydian"
mode Ionian       = command "ionian" 
mode Mixolydian   = command "mixolydian"
mode Dorian       = command "dorian"
mode Aeolian      = command "aeolian"
mode Phrygian     = command "phrygian"
mode Locrian      = command "locrian"


bracesLines :: ODoc -> ODoc
bracesLines d = lbrace <&\> (indent 2 d) <&\> rbrace

