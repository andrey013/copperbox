--------------------------------------------------------------------------------
-- |
-- Module      :  Bala.Perform.MidiInternals
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

module Bala.Perform.MidiInternals (
  printMidi, writeMidi
  ) where

import qualified Bala.Format.Midi as MIDI  
  
printMidi :: MIDI.MidiFile -> IO ()
printMidi = MIDI.printMidi
    
    
writeMidi :: FilePath -> MIDI.MidiFile -> IO ()
writeMidi = MIDI.writeMidi

  