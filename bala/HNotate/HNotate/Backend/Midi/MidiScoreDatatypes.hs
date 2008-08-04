--------------------------------------------------------------------------------
-- |
-- Module      :  HNotate.Backend.Midi.MidiScoreDatatypes
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

module HNotate.Backend.Midi.MidiScoreDatatypes (
    MidiScScore(..),
    MidiScTrack(..),
    MidiScLine,
    MidiScMeasure(..),
    MidiScGlyph(..),
  ) where

import HNotate.Base.Datatypes

import Data.Sequence
import Data.Word

data MidiScScore = MidiScScore (Seq MidiScTrack)

data MidiScTrack = MidiScTrack {
    scmidi_track_number     :: Int,
--    scmidi_prologue_events  :: Seq MidiScEvt,
    scmidi_track_events     :: MidiScLine
  }

type MidiScLine = Seq MidiScMeasure

-- | Measures for Midi are different than for LilyPond or Abc.
-- To flatten polyphony we just put one measure after another
-- (they will both have the same measure number).
--
-- Measure numbers are then used for scaling onset times to get global times,
-- which are finally resolved as delta times
data MidiScMeasure = MidiScMeasure {
    midi_measure_number  :: Int,

    midi_measure_voice_number :: Int,
    -- the glyphs (notes, rests, ...) that make up the measure
    midi_measure_glyphs  :: Seq MidiScGlyph
  }

-- Chords and graces are rendered to notes in the final output.
data MidiScGlyph
    = MidiScNote Pitch Duration
    | MidiScSpacer Duration    -- all rests in midi are spacers
    | MidiScChord [Pitch] Duration
    | MidiScGraceNotes [(Pitch,Duration)]
