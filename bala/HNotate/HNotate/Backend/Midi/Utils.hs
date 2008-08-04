--------------------------------------------------------------------------------
-- |
-- Module      :  HNotate.Backend.Midi.Utils
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

module HNotate.Backend.Midi.Utils (
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

import qualified ZMidi as ZM

-- | Re-export printMidi
printMidi :: ZM.MidiFile -> IO ()
printMidi = ZM.printMidi

-- | Re-export writeMidi
writeMidi :: FilePath -> ZM.MidiFile -> IO ()
writeMidi = ZM.writeMidi


systemToMidi :: (Event evt) => System evt -> ZM.MidiFile
systemToMidi perf = 
    let sc0   = toScore perf default_score_env
        msc   = midiscore sc0
    in generateMidi msc default_midi_env

scoreToMidi :: ScScore -> ZM.MidiFile
scoreToMidi sc = generateMidi (midiscore sc) default_midi_env

