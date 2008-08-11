--------------------------------------------------------------------------------
-- |
-- Module      :  HNotate.Backend.Midi.MidiFragments
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Datatypes of the fragments produced during rendering. 
--
--------------------------------------------------------------------------------

module HNotate.Backend.Midi.MidiFragments (
    PartTrack, PartMidiFile
  ) where

import ZMidi

import Data.Sequence

type PartTrack = (Seq Message)
type PartMidiFile = (Seq PartTrack)



