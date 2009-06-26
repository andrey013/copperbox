{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
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

--------------------------------------------------------------------------------
-- Matrix / Vector multiplication (homogeneous coordinates)


infixr 7 *# 

class VecMult t u where 
  (*#) :: Num a => t a -> u a -> u a

instance VecMult Matrix3'3 Vec3 where
  (*#) (M3'3 a b c d e f g h i) (V3 m n o) = 
    V3 (a*m+b*n+c*o) (d*m+e*n+f*o) (g*m+h*n+i*o)

-- Note - column vector representation

instance VecMult Matrix3'3 Vec2 where
  (M3'3 a b c d e f _ _ _) *# (V2 m n) = V2 (a*m+b*n+c*0) (d*m+e*n+f*0)


instance VecMult Matrix3'3 Point2 where
  (M3'3 a b c d e f _ _ _) *# (P2 m n) = P2 (a*m+b*n+c*1) (d*m+e*n+f*1)


--------------------------------------------------------------------------------
-- 'Pointwise' transformation


-- pointwise trafo (Polygon xs) = Polygon $ map trafo xs 


class Pointwise sh where
  type Pt sh :: *
  pointwise :: (Pt sh -> Pt sh) -> sh -> sh



instance Pointwise (Point2 a) where
  type Pt (Point2 a) = Point2 a
  pointwise f pt = f pt
 
instance Pointwise (Vec2 a) where
  type Pt (Vec2 a) = Vec2 a
  pointwise f pt = f pt


instance Pointwise (a -> a) where
  type Pt (a->a) = a
  pointwise f pt = \a -> pt (f a)
