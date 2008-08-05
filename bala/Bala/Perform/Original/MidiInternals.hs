--------------------------------------------------------------------------------
-- |
-- Module      :  Bala.Perform.Original.MidiInternals
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Utility functions for generating Abc.
--
--------------------------------------------------------------------------------

module Bala.Perform.Original.MidiInternals (
  printMidi, writeMidi
  ) where

import qualified ZMidi as MIDI  
  
printMidi :: MIDI.MidiFile -> IO ()
printMidi = MIDI.printMidi
    
    
writeMidi :: FilePath -> MIDI.MidiFile -> IO ()
writeMidi = MIDI.writeMidi

  