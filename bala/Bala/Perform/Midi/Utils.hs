--------------------------------------------------------------------------------
-- |
-- Module      :  Bala.Perform.Midi.Utils
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Utility functions for Midi.
--
--------------------------------------------------------------------------------

module Bala.Perform.Midi.Utils ( 
  printMidi,
  writeMidi,
  performanceToMidi,
  scoreToMidi
  ) where

import qualified Bala.Format.Midi as MIDI


import Bala.Perform.Base.Class (Perform)
import Bala.Perform.Base.EventTree (Performance)
import Bala.Perform.Midi.MidiBackend (generateMidi, default_midi_env)
import Bala.Perform.Midi.ToMidiScore (midiscore)
import Bala.Perform.Score.Datatypes (ScScore)
import Bala.Perform.Score.ToScore (toScore, default_score_env)

import Bala.Perform.Base.Datatypes

-- | Re-export printMidi
printMidi :: MIDI.MidiFile -> IO ()
printMidi = MIDI.printMidi

-- | Re-export writeMidi
writeMidi :: FilePath -> MIDI.MidiFile -> IO ()
writeMidi = MIDI.writeMidi


performanceToMidi :: (Perform evt) 
                  => Performance evt 
                  -> MIDI.MidiFile
performanceToMidi perf = let sc0   = toScore perf default_score_env
                             msc   = midiscore sc0
                         in generateMidi msc default_midi_env
              
scoreToMidi :: ScScore -> MIDI.MidiFile   
scoreToMidi sc = generateMidi (midiscore sc) default_midi_env

