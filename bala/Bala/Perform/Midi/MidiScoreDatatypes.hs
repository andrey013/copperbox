--------------------------------------------------------------------------------
-- |
-- Module      :  Bala.Perform.Midi.MidiScoreDatatypes
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- A lower level variant of the Score representation for emitting Midi.
--
--------------------------------------------------------------------------------

module Bala.Perform.Midi.MidiScoreDatatypes (
  MidiScScore(..),
  MidiScTrack(..),
  MidiScLine,
  MidiScMeasure(..),
  MidiScGlyph(..),
  ) where

import Data.Sequence
import Data.Word

data MidiScScore pch dur = MidiScScore (Seq (MidiScTrack pch dur))

data MidiScTrack pch dur = MidiScTrack {
    scmidi_track_number     :: Int,
--    scmidi_prologue_events  :: Seq (MidiScEvt pch),  
    scmidi_track_events     :: MidiScLine pch dur
  }

type MidiScLine pch dur = Seq (MidiScMeasure pch dur)  

-- | Measures for Midi are different than for LilyPond or Abc.
-- To flatten polyphony we just put one measure after another 
-- (they will both have the same measure number).
--
-- Measure numbers are then used for scaling onset times to get global times, 
-- which are finally resolved as delta times  
data MidiScMeasure pch dur = MidiScMeasure {
    midi_measure_number  :: Int,
    
    midi_measure_voice_number :: Int,
    -- the glyphs (notes, rests, ...) that make up the measure 
    midi_measure_glyphs  :: (Seq (MidiScGlyph pch dur))
  }

-- Chords and graces are rendered to notes in the final output.  
data MidiScGlyph pch dur
    = MidiScNote pch dur
    | MidiScSpacer dur    -- all rests in midi are spacers
    | MidiScChord [MidiScGlyph pch dur] 
    | MidiScGraceNotes [MidiScGlyph pch dur]
                         