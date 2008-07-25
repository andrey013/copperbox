
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

data ScScore = ScScore (Seq ScPart)

data ScPart = ScPart Integer ScPartRefs (Seq ScPoly)


data ScPoly = ScPolyM ScMeasure | ScPolyRef Integer

newtype ScPartRefs = ScPartRefs { getRefs :: Map.Map Integer [ScPoly] }


data ScMeasure = ScMeasure Integer (Seq ScGlyph)

data ScGroupType = ScBeam | ScChord | ScGraceNotes
  deriving (Eq)

data ScGlyph = ScNote ScPitch Double
             | ScRest Double
             | ScSpacer Double  -- non-printed space
             | ScGroup ScGroupType [ScGlyph]
--             | ScTaggedGlyph ScTag


data ScPitch = ScPitch PitchName Int
           
---


instance Monoid ScPartRefs where
  mempty = ScPartRefs Map.empty
  mappend r r' =  ScPartRefs $ foldr fn (getRefs r) (Map.toAscList $ getRefs r')
    where
      fn (i,x) mp = Map.insert i x mp   
      
      
    

    