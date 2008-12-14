{-# OPTIONS -Wall #-}

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
import HNotate.ProcessingBase
import HNotate.TemplateDatatypes

lilypond :: [BuildDocS] -> LilyPondTemplate
lilypond fs = LyTemplate $ buildDocsContents fs

lilypond1 :: BuildDocS -> LilyPondTemplate
lilypond1 f = LyTemplate $ buildDocsContents [f]

 
version :: BuildDocS
version = buildSrcOnlyS (doc <&\>) where
    doc = command "version" <+> dblquotes (text "2.10.33")

header :: BuildDocS     
header = buildSrcOnlyS docS where
    docS = \d -> onNewline $ command "header" <+> (bracesLines d)
    

title :: String -> BuildDocS 
title s = buildSrcOnlyS (doc <&\>) where
    doc = text "title" <+> equals <> dblquotes (text s)     
        
book :: BuildDocS     
book = buildSrcOnlyS docS where
    docS = \d -> onNewline $ command "book" <+> (bracesLines d)

score :: BuildDocS     
score = buildSrcOnlyS docS where
    docS = \d -> command "score" <+> (bracesLines d)

drummode :: BuildDocS     
drummode = buildSrcOnlyS docS where
    docS = \d -> command "drummode" <+> (bracesLines d)

set :: String -> BuildDocS    
set ss = buildSrcOnlyS (doc <+>) where
    doc = command "set" <+> text ss
    
    
new :: String -> BuildDocS
new name = buildSrcOnlyS (doc <+>) where
    doc = command "new" <+> text name


doubleAngles :: BuildDocS
doubleAngles = buildSrcOnlyS dblangles'

expression :: BuildDocS
expression = buildSrcOnlyS braces'
    
lycommand :: String -> BuildDocS
lycommand name = buildSrcOnlyS (doc <+>) where
    doc = command name    
    
relative :: Pitch -> BuildDocS
relative p = buildCombinedS (docS, ohlet upd) where
    docS = \d -> command "relative" <+> pitch (rescale p) <+> (bracesLines d)
    upd = set_relative_pitch p 
    
  
time :: Int -> Int -> BuildDocS
time n d = buildCombinedS ((doc <&\>), ohlet upd) where
    doc = command "time" <+> meter tms
    upd = set_current_meter tms
    tms = TimeSig n d


  
definition :: String -> BuildDocS
definition s = buildSrcOnlyS docS where
    docS = \d -> onNewline $ text s <+> equals <> indent 1 d 
    
invocation :: String -> BuildDocS   
invocation s = buildSrcOnlyS (doc <&\>) where
    doc = command s
    
                  
key :: PitchLabel -> Mode -> BuildDocS
key l m = buildCombinedS ((doc <&\>), ohlet upd) where
    doc = command "key" <+> pitchLabel l <+> mode m
    upd = set_current_key (Key l m [])
    


-- output

outputRelative :: String -> BuildDocS
outputRelative name = buildExprOnlyS (ohdo directive) where
    directive = OutputDirective (Just OutputRelative) name

outputAbsolute :: String -> BuildDocS
outputAbsolute name = buildExprOnlyS (ohdo directive) where
    directive = OutputDirective (Just OutputDefault) name


--------------------------------------------------------------------------------
-- elementary printers

onNewline :: ODoc -> ODoc
onNewline d = text "" <&\> d


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

