{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# OPTIONS -Wall #-}
{-# OPTIONS -fno-warn-orphans #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Wumpus.Instances
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


module Graphics.Wumpus.Instances where


import Graphics.Wumpus.Point
import Graphics.Wumpus.Matrix
import Graphics.Wumpus.Vector

import Data.AffineSpace
import Data.VectorSpace






instance Num a => AdditiveGroup (Point2 a) where
  zeroV = P2 0 0 
  (^+^) = (+)
  negateV = negate


instance (AffineSpace a, Num (Diff a)) => AffineSpace (Point2 a) where
  type Diff (Point2 a) = Vec2 (Diff a)
  (P2 a b) .-. (P2 x y)   = V2 (a .-. x) (b .-. y)
  (P2 a b) .+^ (V2 vx vy) = P2 (a .+^ vx) (b .+^ vy)


class VecMult t u where 
  vecMult :: Num a => t a -> u a -> u a

instance VecMult Matrix3'3 Vec3 where
  vecMult (M3'3 a b c d e f g h i) (V3 m n o) = 
    V3 (a*m+b*n+c*o) (d*m+e*n+f*o) (g*m+h*n+i*o)

instance VecMult Matrix3'3 Point2 where
  vecMult m (P2 a b) = P2 x y where
   (V3 x y _) = m `vecMult` (V3 a b 1)