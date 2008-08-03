--------------------------------------------------------------------------------
-- |
-- Module      :  Bala.Perform.Midi.PerformMidi
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Top level functions to generate Midi.
--
--------------------------------------------------------------------------------

module Bala.Perform.Midi.PerformMidi ( 
  writeMidi,
  performanceToMidi,
  scoreToMidi
  ) where

import qualified Bala.Format.Midi as MIDI

import Bala.Format.Score (ScScore)
import Bala.Perform.Base.Class (Perform, ScoreDuration)
import Bala.Perform.Base.EventTree (Performance)
import Bala.Perform.Midi.Class (PitchMidi, DurationMidi)
import Bala.Perform.Midi.MidiBackend (generateMidi, default_midi_env)
import Bala.Perform.Midi.ToMidiScore (midiscore)
import Bala.Perform.Score.ToScore (toScore, default_score_env)


-- | Re-export writeMidi
writeMidi :: FilePath -> MIDI.MidiFile -> IO ()
writeMidi = MIDI.writeMidi


performanceToMidi :: (Perform evt pch dur, 
                      ScoreDuration dur, 
                      PitchMidi pch, 
                      DurationMidi dur) 
                  => Performance evt 
                  -> MIDI.MidiFile
performanceToMidi perf = let sc0   = toScore perf default_score_env
                             msc   = midiscore sc0
                         in generateMidi msc default_midi_env
              
scoreToMidi :: (PitchMidi pch, DurationMidi dur) 
            => ScScore pch dur 
            -> MIDI.MidiFile   
scoreToMidi sc = generateMidi (midiscore sc) default_midi_env