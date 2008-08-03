--------------------------------------------------------------------------------
-- |
-- Module      :  Bala.Perform.Abc.AbcScoreDatatypes
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- A lower level variant of the Score representation for emitting Abc.
--
--------------------------------------------------------------------------------

module Bala.Perform.Abc.AbcScoreDatatypes (
  AbcScTuneBook(..),
  AbcScTune(..),
  AbcScLine,
  AbcScPolyPhrase(..),
  AbcScMeasure(..),
  AbcScGlyph(..),
  ) where

import Bala.Perform.Base.Datatypes
import Data.Sequence

data AbcScTuneBook = AbcScTuneBook (Seq AbcScTune)

data AbcScTune = AbcScTune {
    lysc_tune_number          :: Int,
    lysc_tune_body            :: AbcScLine
  }

type AbcScLine = Seq AbcScPolyPhrase


-- | Overlapped measures of a polyphonic phrase. 
-- A singleton phrase (with no polyphony) is treated differently, and so is
-- a special case.
data AbcScPolyPhrase = 
    AbcScSingletonPhrase { abcsc_single_phrase :: AbcScMeasure }
  | AbcScPolyPhrase { abcsc_poly_phrase :: [AbcScMeasure] }


-- | Measures are very important to Abc.
-- Firstly, we must explicitly add barlines.
-- Secondly, voice overlays (i.e. polyphony) must be coordinated in measures. 
data AbcScMeasure = AbcScMeasure {
    abc_measure_number  :: Int,
    
    abc_measure_voice_number :: Int,
    -- the glyphs (notes, rests, ...) that make up the measure 
    abc_measure_glyphs  :: Seq AbcScGlyph
  }
  
-- | A renderable glyph (note, rest, ...) or multiple glyph (chord, ...).
data AbcScGlyph = AbcScNote Pitch Duration
                | AbcScRest Duration
                | AbcScSpacer Duration  -- non-printed rest
                | AbcScChord [Pitch] Duration 
                | AbcScGraceNotes [(Pitch,Duration)]
                | AbcScBeamedNotes [(Pitch,Duration)]
                       