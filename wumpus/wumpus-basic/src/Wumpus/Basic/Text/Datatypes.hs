{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.Text.Datatypes
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Datatypes for handling font metrics.
--
--------------------------------------------------------------------------------

module Wumpus.Basic.Text.Datatypes
  ( 
  
    CodePoint

  , AdvanceVec
  , advanceH

  , CharMetricsTable(..)
  , glyphMaxHeight
  , defaultAdvanceVector
  , advanceVector

  ) where

import Wumpus.Core                              -- package: wumpus-core


import qualified Data.IntMap as IntMap

-- | a Unicode code-point.
--
type CodePoint = Int





type AdvanceVec u = Vec2 u

-- | Take the max width and set the height to zero.
--
-- It is assumed that any deviation from zero in the height
-- component represents that the end vector is in super- or 
-- sub-script mode. As 'advanceHMax' is used in multi-line 
-- concatenation, losing the mode seems acceptable.
--
advanceH :: Num u => AdvanceVec u -> Vec2 u
advanceH (V2 w _)  = V2 w 0




data CharMetricsTable u = forall cu . CharMetricsTable
       { unit_scale_fun     :: PtSize -> cu -> u
       , glyph_max_height   :: cu 
       , default_adv_vec    :: Vec2 cu
       , char_adv_vecs      :: IntMap.IntMap (Vec2 cu)
       }

glyphMaxHeight :: CharMetricsTable u -> PtSize -> u
glyphMaxHeight (CharMetricsTable usf h _ _) sz = (usf sz h)


defaultAdvanceVector :: CharMetricsTable u -> PtSize -> Vec2 u
defaultAdvanceVector (CharMetricsTable usf _ (V2 x y) _) sz = 
    V2 (usf sz x) (usf sz y)

advanceVector :: CharMetricsTable u -> PtSize -> Int -> Maybe (Vec2 u)
advanceVector (CharMetricsTable usf _ _ im) sz ix = 
    fmap fn $ IntMap.lookup ix im 
  where
     fn (V2 x y) = V2 (usf sz x) (usf sz y)

-- Note - the bounding box of a glyph is seemingly _not_ useful 
-- for calculating the bound box of a string. For instance, 
-- /space/ has a zero-width, zero-height bounding box as it has no
-- content, however when drawn it does take up physical space.
-- The size of this space is determined by its advance vector.
--
-- Height can be extracted from the Font bounding box.
--
-- Note the Font bounding box is very wide to use as a default 
-- width...
--


