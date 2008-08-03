
--------------------------------------------------------------------------------
-- |
-- Module      :  Bala.Perform.Score.Datatypes
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Datatypes for Score format
-- 
-- Polyphony is indicated at the start of a measure by pointers to a 
-- dictionary of lines (a sequence of measures). These lines may themselves 
-- have measures that point to further polyphonic lines.
-- 
--------------------------------------------------------------------------------

module Bala.Perform.Score.Datatypes (
  -- * Datatypes
  ScScore(..),
  ScPart(..),
  ScLine,
  ScPolyRefs(..),
  ScMeasure(..),
  ScGlyph(..),
  
  -- * Utils
  lookupPolyRef
  
  ) where

import Bala.Perform.Base.Datatypes
import qualified Data.IntMap as IM
import Data.Monoid
import Data.Sequence





data ScScore = ScScore (Seq ScPart)

data ScPart = ScPart {
    sc_part_number          :: Int,
    sc_part_poly_refs       :: ScPolyRefs,
    -- the primary series of measures 
    sc_part_primary_line    :: ScLine
  }

type ScLine = Seq ScMeasure

newtype ScPolyRefs = ScPolyRefs { 
    getPolyRefs :: IM.IntMap ScLine
  }



-- a Measure could hold a list of indexes to polyphonic measures 
data ScMeasure = ScMeasure {
    sc_measure_number :: Int,
    -- polyphonic elements that start at the same time as this measure 
    sc_measure_poly_refs  :: Seq Int,
    -- the glyphs (notes, rests, ...) that make up the measure 
    sc_measure_glyphs :: Seq ScGlyph
  }



data ScGlyph = ScNote Pitch Duration
             | ScRest Duration
             | ScSpacer Duration  -- non-printed rest
             | ScChord [Pitch] Duration
             | ScGraceNotes [(Pitch,Duration)]
             
                 {-    | ScTaggedGlyph ScTag  -}
{-
-- tag things that aren't processed
newtype ScTag = ScTag Int
-}



           
---


instance Monoid ScPolyRefs where
  mempty = ScPolyRefs mempty
  mappend r r' = ScPolyRefs $ getPolyRefs r `mappend` getPolyRefs r'

      
      
lookupPolyRef :: Int -> ScPolyRefs -> Maybe (Seq ScMeasure)
lookupPolyRef i r = IM.lookup i (getPolyRefs r)    

    