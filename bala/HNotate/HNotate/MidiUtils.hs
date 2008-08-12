--------------------------------------------------------------------------------
-- |
-- Module      :  HNotate.MidiUtils
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

module HNotate.MidiUtils (
    printMidi,
    writeMidi,
    systemToMidi,
    scoreToMidi
  ) where

import HNotate.Backend.Midi.MidiBackend (generateMidi, default_midi_env)
import HNotate.Backend.Midi.ToMidiScore (midiscore)
import HNotate.Base.Datatypes
import HNotate.Base.Class (Event)
import HNotate.Base.EventTree (System)
import HNotate.Score.Datatypes (ScScore)
import HNotate.Score.ToScore (toScore, default_score_env)
import HNotate.System.SystemMidi

import qualified ZMidi as ZM

-- | Re-export printMidi
printMidi :: ZM.MidiFile -> IO ()
printMidi = ZM.printMidi

-- | Re-export writeMidi
writeMidi :: FilePath -> ZM.MidiFile -> IO ()
writeMidi = ZM.writeMidi


systemToMidi :: (Event evt) => MidiSystem -> System evt -> ZM.MidiFile
systemToMidi sys evts = 
    let sc0   = toScore evts default_score_env
        msc   = midiscore sc0
    in sys $ generateMidi msc default_midi_env

scoreToMidi :: MidiSystem -> ScScore -> ZM.MidiFile
scoreToMidi sys sc = sys $ generateMidi (midiscore sc) default_midi_env

