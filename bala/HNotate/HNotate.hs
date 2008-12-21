--------------------------------------------------------------------------------
-- |
-- Module      :  HNotateAlt
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- HNotate - Haskell music notation. 
--
--------------------------------------------------------------------------------

module HNotate (  
    module HNotate.Annotations,
    -- should duration & pitch  export a subset to the 'outside world'?
    module HNotate.Duration,      
    module HNotate.Marks,
    module HNotate.Pitch,            
    module HNotate.OutputMain,
    
    -- restrict the interface to EventTree
      -- * Datatypes
    System, EventList, Event,
    system, systemL, system1,
    
    root, poly,
    noteSgl, note, note',
    restSgl, rest, rest',
    spacerSgl, spacer, spacer',    
    chordGrp, chord, chord',
    chordGrpL, chordL, chordL',
    gracesGrp, graces, graces',
    gracesGrpL, gracesL, gracesL',
    npletGrp, nplet, nplet',
    
    tie, 
    simpleEventlist, 
    ( # ),
    
    {- AddtoEventList(..), ( /@ ), ( /@@ ), -} 
    
    -- Env
    set_current_key,
    set_current_meter,
    set_meter_pattern,
    set_unit_note_length,
    set_relative_pitch,
    set_anacrusis,
    set_unmetered,
    set_tempo,
    
    mkMeter, major, minor
    
 ) where

import HNotate.Annotations
import HNotate.CommonUtils
import HNotate.Duration
import HNotate.Env
import HNotate.Marks
import HNotate.MusicRepDatatypes
import HNotate.NoteListDatatypes
import HNotate.Pitch
import HNotate.OutputMain

mkMeter :: Int -> Int -> Meter
mkMeter n d = TimeSig n d

major :: Mode
major = Major

minor :: Mode
minor = Minor
