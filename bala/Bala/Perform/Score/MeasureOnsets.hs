{-# LANGUAGE MultiParamTypeClasses #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Bala.Perform.Score.MeasureOnsets
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Build an onset queue out of the measures
--
--------------------------------------------------------------------------------

module Bala.Perform.Score.MeasureOnsets (
  OnsetMeasure(..),
  deriveQueue
  ) where


import Bala.Perform.Base.OnsetQueue
import Bala.Perform.Score.Datatypes

import qualified Data.Foldable as F 
import qualified Data.IntMap as IM
import Data.Monoid
import Data.Sequence


-- Use a different representation of measures to the original Score format.
-- Measures are just a unit in this representation - they don't contain 
-- pointers to simultaneous measures, but the do track wich polyphonic voice 
-- they were originally associated with.
data OnsetMeasure = OnsetMeasure {
    onsetmeasure_number  :: Int,
    
    onsetmeasure_voice_number :: Int,
    -- the glyphs (notes, rests, ...) that make up the measure 
    onsetmeasure_glyphs  :: Seq ScGlyph
  }


instance OnsetEvent OnsetMeasure OnsetMeasure where
  onset m@(OnsetMeasure i _ _) = (i, m)
  
    
deriveQueue :: ScPart -> OnsetQueue OnsetMeasure
deriveQueue p = 
  (buildQueue $ primaryLine p) `mappend` (buildQueue $ linearizedRefs p)
  
primaryLine :: ScPart -> Seq OnsetMeasure
primaryLine = line 0 . sc_part_primary_line 

    
linearizedRefs ::  ScPart -> Seq OnsetMeasure
linearizedRefs = F.foldl fn mempty . extract 
  where
    extract = IM.toAscList . getPolyRefs . sc_part_poly_refs 
    fn acc (v,ln) = acc >< line v ln
    
line :: Int -> ScLine -> Seq OnsetMeasure
line voice = F.foldl fn mempty
  where
    fn acc e = acc |> measure voice e  
      
measure :: Int -> ScMeasure -> OnsetMeasure
measure voice (ScMeasure i _ se) = OnsetMeasure i voice se
 