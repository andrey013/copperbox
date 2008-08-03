
--------------------------------------------------------------------------------
-- |
-- Module      :  Bala.Perform.Midi.Midi
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Import module for Midi.
-- Note - this should be exported again by a module that has instances for
-- the music representation used.
-- e.g. PerformBala.hs exports it for the Bala.Base datatypes.
--------------------------------------------------------------------------------

module Bala.Perform.Midi.Midi 
  ( module Bala.Perform.Midi.MidiBackend
  , module Bala.Perform.Midi.MidiScoreDatatypes
  , module Bala.Perform.Midi.ToMidiScore
  , module Bala.Perform.Midi.Utils
  ) where
  
import Bala.Perform.Midi.MidiBackend
import Bala.Perform.Midi.MidiScoreDatatypes
import Bala.Perform.Midi.ToMidiScore
import Bala.Perform.Midi.Utils

