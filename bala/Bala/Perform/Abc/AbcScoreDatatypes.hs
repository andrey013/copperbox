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

import Data.Sequence

data AbcScTuneBook pch dur = AbcScTuneBook (Seq (AbcScTune pch dur))

data AbcScTune pch dur = AbcScTune {
    lysc_tune_number          :: Int,
    lysc_tune_body            :: AbcScLine pch dur
  }

type AbcScLine pch dur = Seq (AbcScPolyPhrase pch dur)


-- | Overlapped measures of a polyphonic phrase. 
-- A singleton phrase (with no polyphony) is treated differently, and so is
-- a special case.
data AbcScPolyPhrase pch dur  = 
    AbcScSingletonPhrase { lysc_single_phrase :: AbcScMeasure pch dur }
  | AbcScPolyPhrase { lysc_poly_phrase :: [AbcScMeasure pch dur] }


-- | Measures are very important to Abc.
-- Firstly, we must explicitly add barlines.
-- Secondly, voice overlays (i.e. polyphony) must be coordinated in measures. 
data AbcScMeasure pch dur = AbcScMeasure {
    abc_measure_number  :: Int,
    
    abc_measure_voice_number :: Int,
    -- the glyphs (notes, rests, ...) that make up the measure 
    abc_measure_glyphs  :: (Seq (AbcScGlyph pch dur))
  }
  
-- | A renderable glyph (note, rest, ...) or multiple glyph (chord, ...).
data AbcScGlyph pch dur = AbcScNote pch dur
                        | AbcScRest dur
                        | AbcScSpacer dur  -- non-printed rest
                        | AbcScChord [AbcScGlyph pch dur] 
                        | AbcScGraceNotes [AbcScGlyph pch dur]
                        | AbcScBeamedNotes [AbcScGlyph pch dur]
                       