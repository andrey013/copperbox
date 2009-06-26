{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# OPTIONS -Wall #-}
{-# OPTIONS -fno-warn-orphans #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Core.Instances
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- MPTC instances
--
--------------------------------------------------------------------------------


module Wumpus.Core.Instances where


import Wumpus.Core.Point
import Wumpus.Core.Matrix
import Wumpus.Core.Vector

import Data.AffineSpace
-- import Data.VectorSpace





instance (AffineSpace a, Num a) => AffineSpace (Point2 a) where
  type Diff (Point2 a) = Vec2 a
  (P2 a b) .-. (P2 x y)   = V2 (a - x)  (b - y)
  (P2 a b) .+^ (V2 vx vy) = P2 (a + vx) (b + vy)

--------------------------------------------------------------------------------
-- Matrix / Vector multiplication (homogeneous coordinates)


instance MatrixMult Matrix3'3 Vec3 where
  (*#) (M3'3 a b c d e f g h i) (V3 m n o) = 
    V3 (a*m+b*n+c*o) (d*m+e*n+f*o) (g*m+h*n+i*o)

-- Note - column vector representation

instance MatrixMult Matrix3'3 Vec2 where
  (M3'3 a b c d e f _ _ _) *# (V2 m n) = V2 (a*m+b*n+c*0) (d*m+e*n+f*0)


instance MatrixMult Matrix3'3 Point2 where
  (M3'3 a b c d e f _ _ _) *# (P2 m n) = P2 (a*m+b*n+c*1) (d*m+e*n+f*1)

