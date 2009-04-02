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
-- Note Abc output is /context sensitive/ on the unit note length,
-- This may be directly set through the unit note length field, or
-- indirectly derived through the meter field.  
--------------------------------------------------------------------------------

module HNotate.DocAbc where

import qualified HNotate.AbcForm as Abc
import HNotate.Data ( c_major, c_major'ls, labelSetOf )
import HNotate.DocBase
import HNotate.Duration
import HNotate.Env
import HNotate.MusicRepDatatypes
import HNotate.Pitch
import HNotate.Utils

-- temp
printf :: ((Doc -> AbcEnv -> Doc) -> AbcEnv -> r) -> r
printf p = p (\s _env -> s) abc_env where
  abc_env = AbcEnv { _current_meter       = TimeSig 4 4,
                     _current_key         = c_major,
                     _label_set           = c_major'ls, 
                     _unit_note_length    = quarter,
                     _tempo               = 120  }


-- This to go somewhere else...

set_current_meter             :: Meter -> AbcEnv -> AbcEnv
set_current_meter m env  = env {_current_meter= m}

set_unit_note_length          :: Duration -> AbcEnv -> AbcEnv
set_unit_note_length d  env   = env {_unit_note_length = d}

set_tempo                     :: Int -> AbcEnv -> AbcEnv
set_tempo i env               = env { _tempo = i } 

set_current_key               :: Key -> AbcEnv -> AbcEnv
set_current_key k env         = 
    let lbls = maybe c_major'ls id (labelSetOf k) 
    in env {_current_key = k, _label_set   = lbls}
                     
--------------------------------------------------------------------------------
--
                 
type AbcOutput = DocK AbcEnv Doc

tune :: AbcOutput -> AbcOutput
tune k = local k <> line

-- | @X field@ - reference \/ tune number.
xField :: Int -> AbcOutput
xField i = field 'X' (int i)

-- | @T field@ - tune title.    
titleField :: String -> AbcOutput
titleField name = field 'T' (text name)

-- | @C field@ - composer name.
composerField :: String -> AbcOutput
composerField s = field 'C' (text s)

-- | @O field@ - origin.
originField :: String -> AbcOutput
originField s = field 'O' (text s)

-- | @M field@ - meter.      
meterField :: Int -> Int -> AbcOutput
meterField n d = field 'M' (meter tms) <> update (set_current_meter tms) 
  where tms = TimeSig n d

-- | @L field@ - unit note length - set a particular duration as the default
-- note length. If the unit note length is not set the default will be 
-- derived from the current meter (e.g. for 4/4 time the unit note length is
-- an eighth).   
unitNoteLenField :: Duration -> AbcOutput
unitNoteLenField d =  field 'L' (abcduration d) <> update (set_unit_note_length d)

-- | @Q field@ - tempo - specialized to the duration of a quarter note.
tempoField :: Int -> AbcOutput
tempoField i = field 'Q' (text "1/4=" <> int i) <> update (set_tempo i)
    
-- | @Z field@ - transcriber notes.
transcriberField :: String -> AbcOutput
transcriberField s = field 'Z' (text s)

-- | @N field@ - notes.    
notesField :: String -> AbcOutput
notesField s = field 'N' (text s)

   
-- | @K field@ - key.
-- The key determines how Abc prints a pitch for instance f# is printed
-- as ^f in c major, but just f in g major. 
keyField :: PitchLabel -> Mode -> AbcOutput
keyField l m = doc <> update (set_current_key $ Key l m []) where
    doc = field 'K' (pitchLabel l Abc.UPPER <+> mode m)
    
         
--------------------------------------------------------------------------------
-- elementary printers

-- $elementaryprinters
-- Elementary print combinators


field :: Char -> AbcOutput -> AbcOutput
field ch doc = char ch <> colon <> doc


meter :: Meter -> AbcOutput
meter (TimeSig n d) = int n <> char '/' <> int d
meter CommonTime    = text "C"
meter CutTime       = text "C|"

  
pitch :: Pitch -> AbcOutput
pitch = document . Abc.pitch


pitchLabel :: PitchLabel -> Abc.PitchChar -> AbcOutput
pitchLabel = document `oo` Abc.pitchLabel


-- This might not be correct...
abcduration :: Duration -> AbcOutput
abcduration = document . Abc.multiplier
     
mode :: Mode -> AbcOutput
mode Major        = text "maj" 
mode Minor        = text "min"
mode Lydian       = text "lyd"
mode Ionian       = text "ion" 
mode Mixolydian   = text "mix"
mode Dorian       = text "dor"
mode Aeolian      = text "aeo"
mode Phrygian     = text "phr"
mode Locrian      = text "loc"



    