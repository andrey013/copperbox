--------------------------------------------------------------------------------
-- |
-- Module      :  HNotate.Backend.LilyPond.LyScoreDatatypes
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- A lower level variant of the Score representation for emitting LilyPond.
--
--------------------------------------------------------------------------------

module HNotate.Backend.LilyPond.LyScoreDatatypes (
    LyScScore(..),
    LyScPart(..),
    LyScLine,
    LyScPolyPhrase(..),
    LyScSegment(..),
    LyScMeasure(..),
    LyScGlyph(..),
  ) where

import HNotate.Base.Datatypes

import Data.Sequence

data LyScScore = LyScScore (Seq LyScPart)

data LyScPart = LyScPart {
    lysc_part_number          :: Int,
    lysc_part_body            :: LyScLine
  }

type LyScLine = Seq LyScPolyPhrase


-- | Overlapped segments of a polyphonic phrase.
-- A singleton phrase (with no polyphony) is treated differently, and so is
-- a special case.
data LyScPolyPhrase =
    LyScSingletonPhrase { lysc_single_phrase :: LyScSegment }
  | LyScPolyPhrase { lysc_poly_phrase :: [LyScSegment] }

-- | The series of measures making up a a single voice in a polyphonic phrase.
-- (Actually polyphony can be as short as a note in LilyPond but the shortest
-- we consider rendering to is a measure).
newtype LyScSegment = LyScSegment {
    getLyScSegment :: Seq LyScMeasure
  }


-- | LilyPond automatically inserts bars (unlike Abc), but we still
-- track them so we can typeset in coherent groups.
data LyScMeasure = LyScMeasure {
    lysc_measure_number  :: Int,

    lysc_measure_voice_number :: Int,
    -- the glyphs (notes, rests, ...) that make up the measure
    lysc_measure_glyphs  :: Seq LyScGlyph
  }

-- | A renderable glyph (note, rest, ...) or multiple glyph (chord, ...).
data LyScGlyph = LyScNote Pitch Duration
               | LyScRest Duration
               | LyScSpacer Duration  -- non-printed rest
               | LyScChord (Seq Pitch) Duration
               | LyScGraceNotes (Seq (Pitch,Duration))
