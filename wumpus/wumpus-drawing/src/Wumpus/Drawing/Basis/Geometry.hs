{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Drawing.Basis.Geometry
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Misc geometric operations.
-- 
--------------------------------------------------------------------------------

module Wumpus.Drawing.Basis.Geometry
  ( 

    midpoint
  , affineComb

  ) where


import Wumpus.Core                              -- package: wumpus-core

import Data.AffineSpace                         -- package: vector-space
import Data.VectorSpace

--------------------------------------------------------------------------------



--------------------------------------------------------------------------------
--


-- | Affine combination...
--
affineComb :: Real u => u -> Point2 u -> Point2 u -> Point2 u
affineComb t p1 p2 = p1 .+^ t *^ (p2 .-. p1)


-- | 'midpoint' : @ start_point * end_point -> Midpoint @
-- 
-- Mid-point on the line formed between the two supplied points.
--
midpoint :: Fractional u => Point2 u -> Point2 u -> Point2 u
midpoint p0 p1 = p0 .+^ v1 ^/ 2 where v1 = p1 .-. p0