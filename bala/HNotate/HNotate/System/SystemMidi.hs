
--------------------------------------------------------------------------------
-- |
-- Module      :  HNotate.Systems.SystemMidi
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- SystemMidi - combining multiple concurrent tracks.
--
--------------------------------------------------------------------------------

module HNotate.System.SystemMidi (
    MidiSystem,
    
    default_midi_system
    
  ) where

import HNotate.Backend.Midi.MidiFragments
import ZMidi


import Data.Foldable as F
import Data.Monoid
import Data.Sequence
import Data.Word
import Prelude hiding (length)



type MidiSystem = PartMidiFile -> MidiFile 

midi_tempo :: Word32
midi_tempo = 500000

-- This should be shared with the backend, I'll have to work 
-- out how to do that (sharing the same reader monad - or something else)
lilypond_ticks'  = 384

  
default_midi_system :: MidiSystem
default_midi_system se = MidiFile header (F.foldl fn (singleton track0) se)
  where
    header = header_fmt1 (1+ length se) (tpb lilypond_ticks')
    track0 = Track $ mempty |> set_tempo midi_tempo |> end_of_track 0
    fn se e = se |> (Track e)
    





    
  