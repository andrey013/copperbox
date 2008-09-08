
--------------------------------------------------------------------------------
-- |
-- Module      :  HNotate.Env
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined
--
-- Datatypes for elements extracted from score templates.
--
--------------------------------------------------------------------------------

module HNotate.Env where

import HNotate.Duration
import HNotate.MusicRepDatatypes
import HNotate.Pitch

import Text.PrettyPrint.Leijen

data Env = Env { 
    _output_format      :: OutputFormat,
    _key                :: Key,
    _meter              :: Meter,
    _measure_length     :: Duration, 
    _unit_note_length   :: Duration, 
    _relative_pitch     :: Pitch,
    _partial_measure    :: (Int,Int),
    _score_comment      :: String -> Doc
  }

current_key :: Env -> Key
current_key = _key

measure_length :: Env -> Duration
measure_length = _measure_length

unit_note_length :: Env -> Duration
unit_note_length = _unit_note_length

relative_pitch :: Env -> Pitch
relative_pitch = _relative_pitch

partial_measure :: Env -> (Int,Int)
partial_measure = _partial_measure

score_comment :: Env -> (String -> Doc)
score_comment = _score_comment

data OutputFormat = Output_Abc | Output_LilyPond | Output_Midi 
  deriving (Eq,Show) 


default_ly_env = Env {
    _output_format          = Output_LilyPond, 
    _key                    = c_major,
    _meter                  = four_four,
    _measure_length         = 4 * quarter,
    _unit_note_length       = quarter,
    _relative_pitch         = middle_c,
    _partial_measure        = (0,0),
    _score_comment          = lyComment
  }
  where 
    lyComment str = enclose (text "%{ ") (text " %}") (string str)             


  
default_abc_env = Env {
    _output_format          = Output_Abc, 
    _key                    = c_major,
    _meter                  = four_four,
    _measure_length         = 4 * quarter,
    _unit_note_length       = eighth,
    _relative_pitch         = middle_c,
    _partial_measure        = (0,0),
    _score_comment          = abcComment
  }
  where
    abcComment str = line <> char '%' <+> string str <> line
    
update_meter :: Meter -> Env -> Env
update_meter m env@(Env {_output_format=Output_Abc})  =   
    if (meterToDouble m > 0.75) 
       then env {_meter = m, _unit_note_length = eighth, 
                 _measure_length = measureLength m}
       else env {_meter = m, _unit_note_length = sixteenth,
                 _measure_length = measureLength m}

update_meter m env                             =  
      env {_meter = m, _measure_length = measureLength m}         


update_current_key k env        = env {_key = k}

update_unit_note_length d  env  = env {_unit_note_length = d}
update_relative_pitch p env     = env {_relative_pitch = p}
update_partial_measure a b env  = env {_partial_measure = (a,b)}

      
measureLength :: Meter -> Duration
measureLength (TimeSig n d)   = duration n d
measureLength CommonTime      = duration 4 4
measureLength CutTime         = duration 2 2

