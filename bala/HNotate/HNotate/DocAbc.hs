{-# OPTIONS -Wall #-}

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
-- Output functions for /doc mode/ Abc 
--
--------------------------------------------------------------------------------

module HNotate.DocAbc (
  -- * Build Abc output from Document combinators
  abc, abc1,
  -- $documentcombinators
  xField, titleField, composerField, originField, meterField,
  unitNoteLenField, tempoField, transcriberField, notesField,
  keyField,
  outputDefault,
  
  -- $elementaryprinters
  field, meter, pitch, pitchLabel, duration, mode

 ) where


import HNotate.Document
import HNotate.Duration
import HNotate.Env
import HNotate.MusicRepDatatypes
import HNotate.Pitch
import HNotate.TemplateDatatypes

-- $documentcombinators
-- Document building combinators

abc :: [BuildDocS] -> HandBuiltAbc
abc fs = HBAbc $ buildDocsContents fs

abc1 :: BuildDocS -> HandBuiltAbc
abc1 f = HBAbc $ buildDocsContents [f]



-- | @X field@ - reference \/ tune number.
xField :: Int -> BuildDocS
xField i = buildDocOnly (doc <&\>) where
    doc = field 'X' (int i)

titleField :: String -> BuildDocS
titleField name = buildDocOnly (doc <&\>) where
    doc = field 'T' (text name)

-- | @C field@ - composer name.
composerField :: String -> BuildDocS
composerField s = buildDocOnly (doc <&\>) where
    doc = field 'C' (text s)
    
-- | @O field@ - origin.
originField :: String -> BuildDocS
originField s = buildDocOnly (doc <&\>) where
    doc = field 'O' (text s)
      
-- | @M field@ - meter.      
meterField :: Int -> Int -> BuildDocS
meterField n d = buildDocHoas ((doc <&\>), ohlet upd) where    
    doc = field 'M' (meter tms)
    upd = set_current_meter tms
    tms = TimeSig n d

-- | @L field@ - unit note length - set a particular duration as the default
-- note length. If the unit note length is not set the default will be 
-- derived from the current meter (e.g. for 4/4 time the unit note length is
-- an eighth).   
unitNoteLenField :: Duration -> BuildDocS
unitNoteLenField d = buildDocHoas ((doc <&\>), ohlet upd) where
    doc = field 'L' (duration d)
    upd = set_unit_note_length d
    

-- | @Q field@ - tempo - specialized to the duration of a quarter note.
tempoField :: Int -> BuildDocS
tempoField i = buildDocHoas ((doc <&\>), ohlet upd) where
    doc = field 'Q' (text "1/4=" <> int i) 
    upd = set_tempo i
    

-- | @Z field@ - transcriber notes.
transcriberField :: String -> BuildDocS
transcriberField s = buildDocOnly (doc <&\>) where
    doc = field 'Z' (text s)

-- | @N field@ - notes.    
notesField :: String -> BuildDocS
notesField s = buildDocOnly (doc <&\>) where
    doc = field 'N' (text s)


    
-- | @K field@ - key.
keyField :: PitchLabel -> Mode -> BuildDocS
keyField l m = buildDocHoas ((doc <&\>), ohlet upd) where
    doc = field 'K' (pitchLabel l UPPER <+> mode m)
    upd = set_current_key $ Key l m [] 
    


-- | @outputDefault name@ - output the notelist /name/.
outputDefault :: String -> BuildDocS
outputDefault name = buildExprOnly (ohdo directive) where
    directive = OutputDirective Nothing name



--------------------------------------------------------------------------------
-- elementary printers

-- $elementaryprinters
-- Elementary print combinators


field :: Char -> ODoc -> ODoc
field ch doc = char ch <> colon <> doc 


meter :: Meter -> ODoc
meter (TimeSig n d) = int n <> char '/' <> int d
meter CommonTime    = text "C"
meter CutTime       = text "C|"

data PitchChar = UPPER | LOWER
  deriving (Eq,Show)
  
pitch :: Pitch -> ODoc
pitch (Pitch l a o) 
    | o > 4     = pitchLabel (PitchLabel l a) LOWER <> octave o 
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

duration :: Duration -> ODoc
duration dn | dn == no_duration = emptyDoc
            | otherwise        = fn $ ratioElements $ convRational dn
  where
    fn (n,1) = int n
    fn (1,d) = char '/' <> int d
    fn (n,d) = int n <> char '/' <> int d
     
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


