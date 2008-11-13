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
    Env,
    Config(..),
    MidiRendering(..),
    
    -- defaults
    default_ly_env,
    default_abc_env,
    default_midi_env,
    
    -- config
    mkLyConfig,
    mkAbcConfig,
    mkMidiConfig,
    
    -- Env query
    output_format,
    current_key,
    current_meter,
    meter_pattern,
    label_set,
    bar_length, 
    unit_note_length, 
    relative_pitch,
    anacrusis,
    unmetered,
    bar_number_check,
    score_comment,
    midi_rendering,
    tempo,
    
    -- computed values
    anacrusis_displacement,
        
    -- Env update
    set_current_key,
    set_current_meter,
    set_meter_pattern,
    set_unit_note_length,
    set_relative_pitch,
    set_anacrusis,
    set_unmetered,
    set_sequential_midi_output,
    set_parallel_midi_output, 
    set_tempo,
    
 ) where

import HNotate.Data
import HNotate.Document
import HNotate.Duration
import HNotate.NoteListDatatypes (System, OutputFormat(..) )
import HNotate.NotateMonad
import HNotate.MusicRepDatatypes
import HNotate.Pitch

import Control.Applicative 
import Control.Monad.Reader
import Data.Maybe (fromMaybe)
import Data.Ratio


--------------------------------------------------------------------------------
-- Datatypes



-- No Show instance use PP instead

 
data Env = Env { 
    _output_format      :: OutputFormat,
    _current_key        :: Key,
    _label_set          :: LabelSet,
    _current_meter      :: Meter,
    _meter_pattern      :: MeterPattern,
    _bar_length         :: Duration, 
    _unit_note_length   :: Duration, 
    _relative_pitch     :: Maybe Pitch,
    _anacrusis          :: Maybe Duration,
    _unmetered          :: Bool,
    _bar_number_check   :: Int,
    _score_comment      :: String -> ODoc,
    _midi_rendering     :: MidiRendering,
    _tempo              :: Int
  }



data Config = Config { 
    _system         :: System,
    _debug_level    :: Int,
    _template_file  :: FilePath,
    _output_file    :: FilePath
    }
  deriving Show

type Delay = Int

data MidiRendering = Midi_Parallel | Midi_Sequential Delay
  deriving (Eq,Show)
  
instance DebugLevel Config where 
    debug_level  = _debug_level



--------------------------------------------------------------------------------
-- Defaults

default_ly_env :: Env
default_ly_env = Env {
    _output_format          = Ly, 
    _current_key            = c_major,
    _label_set              = c_major'ls,
    _current_meter          = four_four,
    _meter_pattern          = four_four_of_eighth,
    _bar_length             = 4 * quarter,
    _unit_note_length       = quarter,
    _relative_pitch         = Nothing,
    _anacrusis              = Nothing,
    _unmetered              = False,
    _bar_number_check       = 4,
    _score_comment          = lyComment,
    _midi_rendering         = Midi_Parallel,
    _tempo                  = 120
  }
  where 
    lyComment str = enclose (text "%{ ") (text " %}") (string str)             


default_abc_env :: Env
default_abc_env = Env {
    _output_format          = Abc, 
    _current_key            = c_major,
    _label_set              = c_major'ls,
    _current_meter          = four_four,
    _meter_pattern          = four_four_of_eighth,
    _bar_length             = 4 * quarter,
    _unit_note_length       = eighth,
    _relative_pitch         = Nothing,
    _anacrusis              = Nothing,
    _unmetered              = True,         -- Abc must start with cadenza on
    _bar_number_check       = 4,
    _score_comment          = abcComment,
    _midi_rendering         = Midi_Parallel,
    _tempo                  = 120
  }
  where
    abcComment str = line <> char '%' <+> string str <> line
    

-- Many of the fields have no bearing on Midi
default_midi_env :: Env
default_midi_env = Env {
    _output_format          = Midi, 
    _current_key            = c_major,
    _label_set              = c_major'ls,
    _current_meter          = four_four,
    _meter_pattern          = four_four_of_eighth,
    _bar_length             = 4 * quarter,
    _unit_note_length       = eighth,
    _relative_pitch         = Nothing,
    _anacrusis              = Nothing,
    _unmetered              = False,
    _bar_number_check       = 0,
    _score_comment          = const (string "<nocomment>"),
    _midi_rendering         = Midi_Parallel,
    _tempo                  = 120
  }
  where
    abcComment str = line <> char '%' <+> string str <> line
    




mkLyConfig :: Int -> System -> FilePath -> FilePath -> Config
mkLyConfig dl sys template outfile = Config { 
    _system         = sys,
    _debug_level    = dl,     
    _template_file  = template,
    _output_file    = outfile
    }

mkAbcConfig :: Int -> System -> FilePath -> FilePath -> Config
mkAbcConfig dl sys template outfile = Config { 
    _system         = sys,
    _debug_level    = dl,      
    _template_file  = template,
    _output_file    = outfile
    }
    
mkMidiConfig :: Int -> System -> FilePath -> Config
mkMidiConfig dl sys outfile = Config { 
    _system         = sys,
    _debug_level    = dl,      
    _template_file  = "",
    _output_file    = outfile
    }
    
         
--------------------------------------------------------------------------------
-- Accessor functions

output_format       :: Env -> OutputFormat
output_format       = _output_format

current_key         :: Env -> Key
current_key         = _current_key

current_meter       :: Env -> Meter
current_meter       = _current_meter

label_set           :: Env -> LabelSet
label_set           = _label_set

meter_pattern       :: Env -> MeterPattern
meter_pattern       = _meter_pattern

bar_length          :: Env -> Duration
bar_length          = _bar_length

unit_note_length    :: Env -> Duration
unit_note_length    = _unit_note_length

relative_pitch      :: Env -> Maybe Pitch
relative_pitch      = _relative_pitch

anacrusis           :: Env -> Maybe Duration
anacrusis           = _anacrusis

unmetered           :: Env -> Bool
unmetered           = _unmetered

bar_number_check    :: Env -> Int
bar_number_check    = _bar_number_check

score_comment       :: Env -> (String -> ODoc)
score_comment       = _score_comment

midi_rendering      :: Env -> MidiRendering
midi_rendering      = _midi_rendering

tempo               :: Env -> Int
tempo               = _tempo


-- LilyPond's \partial command gives the duration of the notes in 
-- the anacrusis (the zeroth bar).
-- For our purposes we need to know the 'start point' in the zeroth bar.  
anacrusis_displacement :: Env -> Duration
anacrusis_displacement env = anaDisp (_anacrusis env) (_bar_length env)
  where
    anaDisp :: Maybe Duration -> Duration -> Duration
    anaDisp Nothing      _    = duration_zero
    anaDisp (Just acsis) blen = blen - acsis



--------------------------------------------------------------------------------
-- Update functions - not every field is directly updateable...

set_current_key               :: Key -> Env -> Env
set_current_key k env         = 
    let lbls = fromMaybe c_major'ls (labelSetOf k) 
    in env {_current_key = k, _label_set   = lbls}

-- Note there is no recognized 'cadenzaOff' in Abc, 
-- seeing a Meter command is equivalent to cadenza Off     
set_current_meter             :: Meter -> Env -> Env
set_current_meter m env@(Env {_output_format=Abc})  =   
    if (meterToDouble m > 0.75) 
       then env {_current_meter     = m, 
                 _unit_note_length  = eighth, 
                 _bar_length        = barLength m,
                 _unmetered         = False}
       else env {_current_meter     = m, 
                 _unit_note_length  = sixteenth,
                 _bar_length        = barLength m,
                 _unmetered         = False}

set_current_meter m env       =  
      env {_current_meter = m, _bar_length = barLength m}         


set_meter_pattern             :: MeterPattern -> Env -> Env
set_meter_pattern mp env      = env {_meter_pattern = mp}     

set_unit_note_length          :: Duration -> Env -> Env
set_unit_note_length d  env   = env {_unit_note_length = d}

set_relative_pitch            :: Pitch -> Env -> Env
set_relative_pitch p env      = env {_relative_pitch = Just p}

set_anacrusis                 :: Duration -> Env -> Env
set_anacrusis d env           = env {_anacrusis = Just d}

set_unmetered                 :: Bool -> Env -> Env
set_unmetered a env           = env {_unmetered = a}

set_sequential_midi_output        :: Int -> Env -> Env
set_sequential_midi_output i env  = env { _midi_rendering = Midi_Sequential i } 

set_parallel_midi_output      :: Env -> Env
set_parallel_midi_output env  = env { _midi_rendering = Midi_Parallel } 

set_tempo                     :: Int -> Env -> Env
set_tempo i env               = env { _tempo = i } 


barLength :: Meter -> Duration
barLength (TimeSig n d)   = convRatio $ n%d
barLength CommonTime      = 4%4
barLength CutTime         = 2%2





--------------------------------------------------------------------------------
--



  


