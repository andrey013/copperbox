{-# LANGUAGE FlexibleInstances          #-}
{-# OPTIONS -Wall #-}
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

    
    -- env
    makeLyEnv,
    makeAbcEnv,
    
    -- config
    makeLyConfig,
    makeAbcConfig,
    
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
    set_tempo,
    
 ) where

import HNotate.Data
import HNotate.Document
import HNotate.Duration
import HNotate.NoteListDatatypes (System, OutputFormat(..) )
import HNotate.NotateMonad
import HNotate.MusicRepDatatypes
import HNotate.Pitch

import Data.Maybe (fromMaybe)
import Data.Ratio


--------------------------------------------------------------------------------
-- Datatypes



-- No Show instance use PP instead

 
data Env = Env { 
    _output_format      :: OutputFormat,
    _debug_level        :: Int,
    _current_key        :: Key,
    _label_set          :: LabelSet,
    _current_meter      :: Meter,
    _meter_pattern      :: Maybe MeterPattern,
    _bar_length         :: Duration, 
    _unit_note_length   :: Duration, 
    _relative_pitch     :: Maybe Pitch,
    _anacrusis          :: Maybe Duration,
    _unmetered          :: Bool,
    _bar_number_check   :: Int,
    _score_comment      :: String -> ODoc,
    _tempo              :: Int
  }



data Config = Config { 
    _system         :: System,   
    _template_file  :: FilePath,
    _output_file    :: FilePath
    }

  
instance DebugLevel Env where 
    debug_level  = _debug_level



--------------------------------------------------------------------------------
-- Defaults

makeLyEnv :: Int -> Env
makeLyEnv dl = Env {
    _output_format          = OutputLy,
    _debug_level            = dl, 
    _current_key            = c_major,
    _label_set              = c_major'ls,
    _current_meter          = four_four,
    _meter_pattern          = Nothing,
    _bar_length             = 4 * quarter,
    _unit_note_length       = quarter,
    _relative_pitch         = Nothing,
    _anacrusis              = Nothing,
    _unmetered              = False,
    _bar_number_check       = 4,
    _score_comment          = lyComment,
    _tempo                  = 120
  }
  where 
    lyComment str = enclose (text "%{ ") (text " %}") (string str)             


makeAbcEnv :: Int -> Env
makeAbcEnv dl = Env {
    _output_format          = OutputAbc, 
    _debug_level            = dl,
    _current_key            = c_major,
    _label_set              = c_major'ls,
    _current_meter          = four_four,
    _meter_pattern          = Nothing,
    _bar_length             = 4 * quarter,
    _unit_note_length       = eighth,
    _relative_pitch         = Nothing,
    _anacrusis              = Nothing,
    _unmetered              = True,         -- Abc must start with cadenza on
    _bar_number_check       = 4,
    _score_comment          = abcComment,
    _tempo                  = 120
  }
  where
    abcComment str = line <> char '%' <+> string str <> line
    


makeLyConfig :: System -> FilePath -> FilePath -> Config
makeLyConfig sys template outfile = Config { 
    _system         = sys,    
    _template_file  = template,
    _output_file    = outfile
    }

makeAbcConfig :: System -> FilePath -> FilePath -> Config
makeAbcConfig sys template outfile = Config { 
    _system         = sys,   
    _template_file  = template,
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
meter_pattern e     = fn $ _meter_pattern e where
  fn (Just mp) = mp
  fn Nothing   = defaultMeterPattern (_current_meter e)

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

tempo               :: Env -> Int
tempo               = _tempo


 
anacrusis_displacement :: Env -> Duration
anacrusis_displacement env = maybe duration_zero id (_anacrusis env)

--------------------------------------------------------------------------------
-- Update functions - not every field is directly updateable...

set_current_key               :: Key -> Env -> Env
set_current_key k env         = 
    let lbls = fromMaybe c_major'ls (labelSetOf k) 
    in env {_current_key = k, _label_set   = lbls}

-- Note there is no recognized 'cadenzaOff' in Abc, 
-- seeing a Meter command is equivalent to cadenza Off     
set_current_meter             :: Meter -> Env -> Env
set_current_meter m env@(Env {_output_format=OutputAbc})  =   
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
set_meter_pattern mp env      = env {_meter_pattern = Just mp}     

set_unit_note_length          :: Duration -> Env -> Env
set_unit_note_length d  env   = env {_unit_note_length = d}

set_relative_pitch            :: Pitch -> Env -> Env
set_relative_pitch p env      = env {_relative_pitch = Just p}

set_anacrusis                 :: Duration -> Env -> Env
set_anacrusis d env           = env {_anacrusis = Just d}

set_unmetered                 :: Bool -> Env -> Env
set_unmetered a env           = env {_unmetered = a}

set_tempo                     :: Int -> Env -> Env
set_tempo i env               = env { _tempo = i } 


barLength :: Meter -> Duration
barLength (TimeSig n d)   = convRatio $ n%d
barLength CommonTime      = 4%4
barLength CutTime         = 2%2





--------------------------------------------------------------------------------
--



  


