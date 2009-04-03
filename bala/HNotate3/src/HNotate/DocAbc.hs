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
import HNotate.DocBase
import HNotate.Duration
import HNotate.EvalAbc
import HNotate.MusicRepDatatypes
import HNotate.Pitch
import HNotate.Utils


-- temp
printf :: ((Doc -> AbcEnv -> Doc) -> AbcEnv -> r) -> r
printf p = p (\s _env -> s) abc_env0 



                     
--------------------------------------------------------------------------------
--
                 
type AbcOutput = DocK AbcEnv Doc

tune :: AbcOutput -> AbcOutput
tune k = local k <> line

-- | @X field@ - reference \/ tune number.
tune_number :: Int -> AbcOutput
tune_number i = field 'X' (int i)

-- | @T field@ - tune title.    
title :: String -> AbcOutput
title name = field 'T' (text name)

-- | @C field@ - composer name.
composer :: String -> AbcOutput
composer s = field 'C' (text s)

-- | @O field@ - origin.
origin :: String -> AbcOutput
origin s = field 'O' (text s)

-- | @M field@ - meter.      
meter :: Integer -> Integer -> AbcOutput
meter n d = field 'M' (abcmeter tms) <> update (set_current_meter tms) 
  where tms = TimeSig n d

-- | @L field@ - unit note length - set a particular duration as the default
-- note length. If the unit note length is not set the default will be 
-- derived from the current meter (e.g. for 4/4 time the unit note length is
-- an eighth).   
unit_note_length :: Duration -> AbcOutput
unit_note_length d =  field 'L' (abcDuration d) <> update (set_unit_note_length d)

-- | @Q field@ - tempo - specialized to the duration of a quarter note.
tempo :: Int -> AbcOutput
tempo i = field 'Q' (text "1/4=" <> int i) <> update (set_tempo i)
    
-- | @Z field@ - transcriber notes.
transcriber :: String -> AbcOutput
transcriber s = field 'Z' (text s)

-- | @N field@ - notes.    
notes :: String -> AbcOutput
notes s = field 'N' (text s)

   
-- | @K field@ - key.
-- The key determines how Abc prints a pitch for instance f# is printed
-- as ^f in c major, but just f in g major. 
key :: Key -> AbcOutput
key z = doc <> update (set_current_key z) where
    doc = field 'K' (abcKey z)
    


anacrusis :: Duration -> AbcOutput
anacrusis d = update (set_anacrusis d)


         
--------------------------------------------------------------------------------
-- elementary printers

-- $elementaryprinters
-- Elementary print combinators


field :: Char -> AbcOutput -> AbcOutput
field ch doc = char ch <> colon <> doc


abcmeter :: Meter -> AbcOutput
abcmeter (TimeSig n d) = integer n <> char '/' <> integer d
abcmeter CommonTime    = text "C"
abcmeter CutTime       = text "C|"

  
pitch :: Pitch -> AbcOutput
pitch = document . Abc.pitch


pitchLabel :: PitchLabel -> Abc.PitchChar -> AbcOutput
pitchLabel = document `oo` Abc.pitchLabel


-- This might not be correct...
abcDuration :: Duration -> AbcOutput
abcDuration = document . Abc.multiplier
     
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

abcKey :: Key -> AbcOutput
abcKey (Key l Major _)  = pitchLabel l Abc.UPPER
abcKey (Key l m _)      = pitchLabel l Abc.UPPER <> mode m


    