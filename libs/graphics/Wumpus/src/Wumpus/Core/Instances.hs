{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE UndecidableInstances       #-}
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





instance (AffineSpace a, Num (Diff a)) => AffineSpace (Point2 a) where
  type Diff (Point2 a) = Vec2 (Diff a)
  (P2 a b) .-. (P2 x y)   = V2 (a .-. x) (b .-. y)
  (P2 a b) .+^ (V2 vx vy) = P2 (a .+^ vx) (b .+^ vy)


infixr 7 *# 

class VecMult t u where 
  (*#) :: Num a => t a -> u a -> u a

instance VecMult Matrix3'3 Vec3 where
  (*#) (M3'3 a b c d e f g h i) (V3 m n o) = 
    V3 (a*m+b*n+c*o) (d*m+e*n+f*o) (g*m+h*n+i*o)


-- need to take care here vis-a-vis row / column vectors...

instance VecMult Matrix3'3 Vec2 where
  (*#) m (V2 a b) = V2 x y where (V3 x y _) = m *# (V3 a b 0)


instance VecMult Matrix3'3 Point2 where
  (*#) m (P2 a b) = P2 x y where (V3 x y _) = m *# (V3 a b 1)

