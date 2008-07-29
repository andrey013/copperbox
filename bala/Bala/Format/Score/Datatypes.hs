
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
--------------------------------------------------------------------------------

module Bala.Format.Score.Datatypes  where

import Bala.Base

import qualified Data.Map as Map
import Data.Monoid
import Data.Sequence

-- tag things that aren't processed
newtype ScTag = ScTag Integer

data ScScore pch dur = ScScore (Seq (ScPart pch dur))

data ScPart pch dur = ScPart {
    sc_part_number          :: Integer,
    sc_part_poly_refs       :: ScPartRefs pch dur,
    -- the primary series of measures 
    sc_part_primary_line    :: Seq (ScMeasure pch dur)
  }


-- data ScPoly pch dur = ScPolyM (ScMeasure pch dur) | ScPolyRef [Integer]


newtype ScPartRefs pch dur = ScPartRefs { 
    getRefs :: Map.Map Integer (Seq (ScMeasure pch dur)) 
  }

-- a Measure could hold a list of indexes to polyphonic measures 
data ScMeasure pch dur = ScMeasure {
    sc_measure_number :: Integer,
    -- polyphonic elements that start at the same time as this measure 
    sc_measure_poly_refs  :: Seq Integer,
    -- the glyphs (notes, rests, ...) that make up the measure 
    sc_measure_glyphs :: (Seq (ScGlyph pch dur))
  }

data ScGroupType = ScBeam | ScChord | ScGraceNotes
  deriving (Eq)

data ScGlyph pch dur = ScNote (ScPitch pch) dur
                     | ScRest dur
                     | ScSpacer dur  -- non-printed space
                     | ScGroup ScGroupType [ScGlyph pch dur]
--                     | ScTaggedGlyph ScTag


data ScPitch pch = ScPitch pch


                     
                     



           
---


instance Monoid (ScPartRefs pch dur) where
  mempty = ScPartRefs Map.empty
  mappend r r' =  ScPartRefs $ foldr fn (getRefs r) (Map.toAscList $ getRefs r')
    where
      fn (i,x) mp = Map.insert i x mp   
      
      
getPolyRef :: Integer -> ScPartRefs pch dur -> Maybe (Seq (ScMeasure pch dur))
getPolyRef i r = Map.lookup i (getRefs r)    

    