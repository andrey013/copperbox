
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
    output_format       :: OutputFormat,
    current_key         :: Key,
    current_meter       :: Meter,
    measure_length      :: Duration, 
    unit_note_length    :: Duration, 
    relative_pitch      :: Pitch,
    partial_measure     :: (Int,Int),
    score_comment       :: String -> Doc
  }

 
                      
data OutputFormat = Output_Abc | Output_LilyPond | Output_Midi 
  deriving (Eq,Show) 


default_ly_env = Env {
    output_format           = Output_LilyPond, 
    current_key             = c_major,
    current_meter           = four_four,
    measure_length          = 4 * quarter,
    unit_note_length        = quarter,
    relative_pitch          = middle_c,
    partial_measure         = (0,0),
    score_comment           = lyComment
  }
  where 
    lyComment str = enclose (text "%{ ") (text " %}") (string str)             


  
default_abc_env = Env {
    output_format           = Output_Abc, 
    current_key             = c_major,
    current_meter           = four_four,
    measure_length          = 4 * quarter,
    unit_note_length        = eighth,
    relative_pitch          = middle_c,
    partial_measure         = (0,0),
    score_comment           = abcComment
  }
  where
    abcComment str = line <> char '%' <+> string str <> line
    
updateMeter :: Meter -> Env -> Env
updateMeter m env@(Env {output_format=Output_Abc})  =   
    if (meterToDouble m > 0.75) 
       then env {current_meter=m, unit_note_length=eighth, 
                 measure_length=measureLength m}
       else env {current_meter=m, unit_note_length=sixteenth,
                 measure_length=measureLength m}

updateMeter m env                             =  
      env {current_meter=m, measure_length=measureLength m}         
      
measureLength :: Meter -> Duration
measureLength (TimeSig n d)   = duration n d
measureLength CommonTime      = duration 4 4
measureLength CutTime         = duration 2 2