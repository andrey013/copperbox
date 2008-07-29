
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

data ScPart pch dur = ScPart Integer (ScPartRefs pch dur) (Seq (ScPoly pch dur))


data ScPoly pch dur = ScPolyM (ScMeasure pch dur) | ScPolyRef [Integer]


newtype ScPartRefs pch dur = ScPartRefs { 
                                getRefs :: Map.Map Integer (Seq (ScMeasure pch dur)) }


data ScMeasure pch dur = ScMeasure Integer (Seq (ScGlyph pch dur))

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

    