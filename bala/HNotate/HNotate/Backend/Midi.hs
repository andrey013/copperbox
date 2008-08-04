
--------------------------------------------------------------------------------
-- |
-- Module      :  HNotate.Backend.Midi
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

module HNotate.Backend.Midi (
    module HNotate.Backend.Midi.MidiBackend,
    module HNotate.Backend.Midi.MidiScoreDatatypes,
    module HNotate.Backend.Midi.ToMidiScore,
    module HNotate.Backend.Midi.Utils
  ) where

import HNotate.Backend.Midi.MidiBackend
import HNotate.Backend.Midi.MidiScoreDatatypes
import HNotate.Backend.Midi.ToMidiScore
import HNotate.Backend.Midi.Utils

