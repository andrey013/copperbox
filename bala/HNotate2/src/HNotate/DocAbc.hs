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

import HNotate.DocBase
import HNotate.Duration
import HNotate.Env
import HNotate.MusicRepDatatypes





-- temp
printf :: ((Doc -> AbcEnv -> Doc) -> AbcEnv -> r) -> r
printf p = p (\s _env -> s) abc_env where
  abc_env = AbcEnv { _current_meter       = TimeSig 4 4, 
                     _unit_note_length    = quarter,
                     _tempo               = 120  }


-- This to go somewhere else...

set_current_meter             :: Meter -> AbcEnv -> AbcEnv
set_current_meter m env  = env {_current_meter= m}

set_unit_note_length          :: Duration -> AbcEnv -> AbcEnv
set_unit_note_length d  env   = env {_unit_note_length = d}

set_tempo                     :: Int -> AbcEnv -> AbcEnv
set_tempo i env               = env { _tempo = i } 
                 
--------------------------------------------------------------------------------
--
                 
type AbcOutput = DocK AbcEnv Doc

tune :: AbcOutput -> AbcOutput
tune k = local k 

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
unitNoteLenField d =  field 'L' (duration d) <> update (set_unit_note_length d)

-- | @Q field@ - tempo - specialized to the duration of a quarter note.
tempoField :: Int -> AbcOutput
tempoField i = field 'Q' (text "1/4=" <> int i) <> update (set_tempo i)
    
               
--------------------------------------------------------------------------------
-- elementary printers

-- $elementaryprinters
-- Elementary print combinators


field :: Char -> AbcOutput -> AbcOutput
field ch doc = char ch <> colon <> doc <> line


meter :: Meter -> AbcOutput
meter (TimeSig n d) = int n <> char '/' <> int d
meter CommonTime    = text "C"
meter CutTime       = text "C|"

duration :: Duration -> AbcOutput
duration dn | dn == no_duration = empty
            | otherwise         = fn $ ratioElements $ convRational dn
  where
    fn (n,1) = int n
    fn (1,d) = char '/' <> int d
    fn (n,d) = int n <> char '/' <> int d
    