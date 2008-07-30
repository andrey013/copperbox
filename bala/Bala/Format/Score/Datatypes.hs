
--------------------------------------------------------------------------------
-- |
-- Module      :  Bala.Format.Score.Datatypes
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

module Bala.Format.Score.Datatypes (
  -- * Datatypes
  ScScore(..),
  ScPart(..),
  ScLine,
  ScPolyRefs(..),
  ScMeasure(..),
  ScGroupType(..),
  ScGlyph(..),
  ScPitch(..),
  
  -- * Utils
  lookupPolyRef
  
  ) where


import qualified Data.IntMap as IM
import Data.Monoid
import Data.Sequence



type ScLine pch dur = Seq (ScMeasure pch dur)

data ScScore pch dur = ScScore (Seq (ScPart pch dur))

data ScPart pch dur = ScPart {
    sc_part_number          :: Int,
    sc_part_poly_refs       :: ScPolyRefs pch dur,
    -- the primary series of measures 
    sc_part_primary_line    :: ScLine pch dur
  }

newtype ScPolyRefs pch dur = ScPolyRefs { 
    getPolyRefs :: IM.IntMap (ScLine pch dur) 
  }



-- a Measure could hold a list of indexes to polyphonic measures 
data ScMeasure pch dur = ScMeasure {
    sc_measure_number :: Int,
    -- polyphonic elements that start at the same time as this measure 
    sc_measure_poly_refs  :: Seq Int,
    -- the glyphs (notes, rests, ...) that make up the measure 
    sc_measure_glyphs :: (Seq (ScGlyph pch dur))
  }

data ScGroupType = ScBeam | ScChord | ScGraceNotes
  deriving (Eq)

data ScGlyph pch dur = ScNote (ScPitch pch) dur
                     | ScRest dur
                     | ScSpacer dur  -- non-printed rest
                     | ScGroup ScGroupType [ScGlyph pch dur]
                 {-    | ScTaggedGlyph ScTag  -}
{-
-- tag things that aren't processed
newtype ScTag = ScTag Int
-}

data ScPitch pch = ScPitch pch


           
---


instance Monoid (ScPolyRefs pch dur) where
  mempty = ScPolyRefs mempty
  mappend r r' = ScPolyRefs $ getPolyRefs r `mappend` getPolyRefs r'

      
      
lookupPolyRef :: Int -> ScPolyRefs pch dur -> Maybe (Seq (ScMeasure pch dur))
lookupPolyRef i r = IM.lookup i (getPolyRefs r)    

    