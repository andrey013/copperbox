{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  HNotate.EvalAbc
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- ...
--
--------------------------------------------------------------------------------

module HNotate.EvalAbc where

import HNotate.AbcForm
import HNotate.Data
import HNotate.Duration
import HNotate.Metrical
import HNotate.MusicRepDatatypes
import HNotate.NamedElements
import HNotate.NoteList

import Text.PrettyPrint.Leijen

data AbcEnv = AbcEnv {
      _current_meter      :: Meter,
      _current_key        :: Key,
      _label_set          :: LabelSet,      
      _unit_note_length   :: Maybe Duration,
      _tempo              :: Int,
      _anacrusis          :: Duration     
    }
  deriving (Show)

abc_env0 :: AbcEnv
abc_env0 = AbcEnv { _current_meter       = TimeSig 4 4,
                    _current_key         = c_major,
                    _label_set           = default_labelset, 
                    _unit_note_length    = Nothing,
                    _tempo               = 120,
                    _anacrusis           = 0  }
                     
get_anacrusis :: AbcEnv -> Duration             
get_anacrusis = _anacrusis

get_unit_note_length :: AbcEnv -> Duration
get_unit_note_length env = case _unit_note_length env of
    Just d  -> d
    Nothing -> if meterToDouble (_current_meter env) < 0.75 
                 then sixteenth else eighth
-- This to go somewhere else...

set_current_meter             :: Meter -> AbcEnv -> AbcEnv
set_current_meter m env  = env {_current_meter= m}

set_unit_note_length          :: Duration -> AbcEnv -> AbcEnv
set_unit_note_length d  env   = env {_unit_note_length = Just d}

set_tempo                     :: Int -> AbcEnv -> AbcEnv
set_tempo i env               = env { _tempo = i } 

set_current_key               :: Key -> AbcEnv -> AbcEnv
set_current_key k env         = 
    let lbls = maybe default_labelset id (labelSetOf k) 
    in env {_current_key = k, _label_set   = lbls}
    
set_anacrusis                 :: Duration -> AbcEnv -> AbcEnv
set_anacrusis d env           = env {_anacrusis = d}

--------------------------------------------------------------------------------
-- 
   
stdInterp :: NoteList -> AbcEnv -> Doc
stdInterp notes env = 
    outputAbc . abcStaff lset unl $ lineTreeToStaffRep ana bars beams notes
  where
    lset  = maybe lsetFail id  $ labelSetOf (_current_key env)
    unl   = get_unit_note_length env
    ana   = get_anacrusis env
    bars  = repeat . barLength $ _current_meter env
    beams = repeat . mkMeterPattern $ _current_meter env
    
    lsetFail = error $ "stdInterp - label set failed"



    
         