
--------------------------------------------------------------------------------
-- |
-- Module      :  ZMidi.ZMidi
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- ZMidi - a Midi file library. Top level import.
--
--------------------------------------------------------------------------------

module ZMidi ( 
    module ZMidi.Datatypes,
    module ZMidi.GeneralMidiInstruments,
    module ZMidi.ReadFile,
    -- module ZMidi.SyntaxElements,
    module ZMidi.TextualMidi,
    module ZMidi.WriteFile,
  ) where
  
import ZMidi.Datatypes
import ZMidi.GeneralMidiInstruments
import ZMidi.ReadFile
-- import ZMidi.SyntaxElements
import ZMidi.TextualMidi
import ZMidi.WriteFile

