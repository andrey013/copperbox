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

-- | @X field@ - reference \/ tune number.
xField :: Int -> DocS
xField i expr = HText doc expr where
    doc = field 'X' (int i)

titleField :: String -> DocS
titleField name expr = HText doc expr where
    doc = field 'T' (text name)

-- | @C field@ - composer name.
composerField :: String -> DocS
composerField s expr = HText doc expr where
    doc = field 'C' (text s)
    
-- | @O field@ - origin.
originField :: String -> DocS
originField s expr = HText doc expr where
    doc = field 'O' (text s)
      
-- | @M field@ - meter.      
meterField :: Int -> Int -> DocS
meterField n d expr = HLet update doc expr where
    tsig    = TimeSig n d
    update  = set_current_meter tsig
    doc     = field 'M' (meter tsig)

-- | @L field@ - unit note length - set a particular duration as the default
-- note length. If the unit note length is not set the default will be 
-- derived from the current meter (e.g. for 4/4 time the unit note length is
-- an eighth).   
unitNoteLenField :: Duration -> DocS
unitNoteLenField d expr = HLet update doc expr where
    update  = set_unit_note_length d
    doc     = field 'L' (duration d)

-- | @Q field@ - tempo - specialized to the duration of a quarter note.
tempoField :: Int -> DocS
tempoField i expr = HLet update doc expr where
    update  = set_tempo i
    doc     = field 'Q' (text "1/4=" <> int i) 

-- | @Z field@ - transcriber notes.
transcriberField :: String -> DocS
transcriberField s expr = HText doc expr where
    doc = field 'Z' (text s)

-- | @N field@ - notes.    
notesField :: String -> DocS
notesField s expr = HText doc expr where
    doc = field 'N' (text s)
    
-- | @K field@ - key.
keyField :: PitchLabel -> Mode -> DocS
keyField l m expr = HLet update doc expr where
    update  = set_current_key $ Key l m [] 
    doc     = field 'K' (pitchLabel l UPPER <+> mode m)


-- | @outputDefault name@ - output the notelist /name/.
outputDefault :: String -> HoasExprD
outputDefault name = HDo directive where
    directive = OutputDirective Nothing name


--------------------------------------------------------------------------------
-- elementary printers

-- $elementaryprinters
-- Elementary print combinators


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


