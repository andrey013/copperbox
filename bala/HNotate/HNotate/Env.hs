{-# LANGUAGE FlexibleInstances #-}

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

module HNotate.Env (
    OutputFormat(..),
    Env,
    
    -- defaults
    default_ly_env,
    default_abc_env,
    
    -- Env query
    output_format,
    current_key,
    current_meter,
    measure_length, 
    unit_note_length, 
    relative_pitch,
    partial_measure,
    cadenza,
    bar_number_check,
    score_comment,
    
    -- Env update
    set_current_key,
    set_current_meter,    
    set_unit_note_length,
    set_relative_pitch,
    set_partial_measure,
    set_cadenza
 ) where

import HNotate.Duration
import HNotate.MusicRepDatatypes
import HNotate.Pitch

import Text.PrettyPrint.Leijen

--------------------------------------------------------------------------------
-- Datatypes

data OutputFormat = Output_Abc | Output_LilyPond | Output_Midi 
  deriving (Eq,Show) 
  
data Env = Env { 
    _output_format      :: OutputFormat,
    _current_key        :: Key,
    _current_meter      :: Meter,
    _measure_length     :: Duration, 
    _unit_note_length   :: Duration, 
    _relative_pitch     :: Pitch,
    _partial_measure    :: (Int,Int),
    _cadenza            :: Bool,
    _bar_number_check   :: Int,
    _score_comment      :: String -> Doc
  }
  deriving Show

instance Show (String -> Doc) where
  show _ = "<<function>>"  


--------------------------------------------------------------------------------
-- Defaults

default_ly_env = Env {
    _output_format          = Output_LilyPond, 
    _current_key            = c_major,
    _current_meter          = four_four,
    _measure_length         = 4 * quarter,
    _unit_note_length       = quarter,
    _relative_pitch         = middle_c,
    _partial_measure        = (0,0),
    _cadenza                = False,
    _bar_number_check       = 4,
    _score_comment          = lyComment
  }
  where 
    lyComment str = enclose (text "%{ ") (text " %}") (string str)             


  
default_abc_env = Env {
    _output_format          = Output_Abc, 
    _current_key            = c_major,
    _current_meter          = four_four,
    _measure_length         = 4 * quarter,
    _unit_note_length       = eighth,
    _relative_pitch         = middle_c,
    _partial_measure        = (0,0),
    _cadenza                = False,
    _bar_number_check       = 4,
    _score_comment          = abcComment
  }
  where
    abcComment str = line <> char '%' <+> string str <> line
    
    
--------------------------------------------------------------------------------
-- Accessor functions

output_format       :: Env -> OutputFormat
output_format       = _output_format

current_key         :: Env -> Key
current_key         = _current_key

current_meter       :: Env -> Meter
current_meter       = _current_meter

measure_length      :: Env -> Duration
measure_length      = _measure_length

unit_note_length    :: Env -> Duration
unit_note_length    = _unit_note_length

relative_pitch      :: Env -> Pitch
relative_pitch      = _relative_pitch

-- Note for a partial measure Abc just prints the barline 'early',
-- LilyPond needs the partial command.
partial_measure     :: Env -> (Int,Int)
partial_measure     = _partial_measure

cadenza             :: Env -> Bool
cadenza             = _cadenza

bar_number_check    :: Env -> Int
bar_number_check    = _bar_number_check

score_comment       :: Env -> (String -> Doc)
score_comment       = _score_comment







--------------------------------------------------------------------------------
-- Update functions - not every field is directly updateable...


set_current_key               :: Key -> Env -> Env
set_current_key k env         = env {_current_key = k}

-- Note there is no recognized 'cadenzaOff' in Abc, 
-- seeing a Meter command is equivalent to cadenza Off     
set_current_meter             :: Meter -> Env -> Env
set_current_meter m env@(Env {_output_format=Output_Abc})  =   
    if (meterToDouble m > 0.75) 
       then env {_current_meter     = m, 
                 _unit_note_length  = eighth, 
                 _measure_length    = measureLength m,
                 _cadenza           = False}
       else env {_current_meter     = m, 
                 _unit_note_length  = sixteenth,
                 _measure_length    = measureLength m,
                 _cadenza           = False}

set_current_meter m env       =  
      env {_current_meter = m, _measure_length = measureLength m}         



set_unit_note_length          :: Duration -> Env -> Env
set_unit_note_length d  env   = env {_unit_note_length = d}

set_relative_pitch            :: Pitch -> Env -> Env
set_relative_pitch p env      = env {_relative_pitch = p}

set_partial_measure           :: Int -> Int -> Env -> Env
set_partial_measure a b env   = env {_partial_measure = (a,b)}

set_cadenza                   :: Bool -> Env -> Env
set_cadenza a env             = env {_cadenza = a}
      
measureLength :: Meter -> Duration
measureLength (TimeSig n d)   = duration n d
measureLength CommonTime      = duration 4 4
measureLength CutTime         = duration 2 2

