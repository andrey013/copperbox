--------------------------------------------------------------------------------
-- |
-- Module      :  Bala.Perform.LilyPond.LyScoreDatatypes
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

module Bala.Perform.LilyPond.LyScoreDatatypes (
  LyScScore(..),
  LyScPart(..),
  LyScLine,
  LyScPolyPhrase(..),
  LyScSegment(..),
  LyScMeasure(..),
  LyScGlyph(..),
  ) where

import Data.Sequence

data LyScScore pch dur = LyScScore (Seq (LyScPart pch dur))

data LyScPart pch dur = LyScPart {
    lysc_part_number          :: Int,
    lysc_part_body            :: LyScLine pch dur
  }

type LyScLine pch dur = Seq (LyScPolyPhrase pch dur)


-- | A overlapped segments of a polyphonic phrase. 
-- A singleton phrase (with no polyphony) is treated differently, and so is
-- a special case.
data LyScPolyPhrase pch dur  = 
    LyScSingletonPhrase { lysc_single_phrase :: LyScSegment pch dur }
  | LyScPolyPhrase { lysc_poly_phrase :: [LyScSegment pch dur] }

-- | The series of measures making up a a single voice in a polyphonic phrase. 
-- (Actually polyphony can be as short as a note in LilyPond but the shortest
-- we consider rendering to is a measure).  
newtype LyScSegment pch dur = LyScSegment { 
    getLyScSegment :: Seq (LyScMeasure pch dur) 
  }
  
  
-- | LilyPond automatically inserts bars (unlike Abc), but we still 
-- track them so we can typeset in coherent groups.
data LyScMeasure pch dur = LyScMeasure {
    lysc_measure_number  :: Int,
    
    lysc_measure_voice_number :: Int,
    -- the glyphs (notes, rests, ...) that make up the measure 
    lysc_measure_glyphs  :: (Seq (LyScGlyph pch dur))
  }
  
-- | A renderable glyph (note, rest, ...) or multiple glyph (chord, ...).
data LyScGlyph pch dur = LyScNote pch dur
                       | LyScRest dur
                       | LyScSpacer dur  -- non-printed rest
                       | LyScChord [LyScGlyph pch dur] 
                       | LyScGraceNotes [LyScGlyph pch dur]
                      