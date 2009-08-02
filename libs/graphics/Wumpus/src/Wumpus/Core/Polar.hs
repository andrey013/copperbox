{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Core.Polar
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Polar coordinates
--
--------------------------------------------------------------------------------


module Wumpus.Core.Polar
  (
  -- * Polar coordinate type
    Polar
  

  -- * Conversion
  , PolarCoord(..)


  ) where


import Wumpus.Core.Point
import Wumpus.Core.Radian
import Wumpus.Core.Vector

--------------------------------------------------------------------------------

data Polar a = Polar Radian a 
  deriving (Eq)


--------------------------------------------------------------------------------
-- Type class instances

instance Show a => Show (Polar a) where
 showsPrec i (Polar a r) = showsPrec i (a,r)


class PolarCoord t where 
  toPolar :: (Real a, Floating a) => t a -> Polar a
  fromPolar :: Fractional a => Polar a -> t a

instance PolarCoord Vec2 where
  toPolar (V2 x y) = Polar (atan $ toRadian (y/x)) (sqrt (x*x+y*y))
  fromPolar (Polar ang r) = V2 x y where
      x = r * (fromRadian $ cos ang)
      y = r * (fromRadian $ sin ang)


instance PolarCoord Point2 where
  toPolar (P2 x y) = Polar (atan $ toRadian (y/x)) (sqrt (x*x+y*y))
  fromPolar (Polar ang r) = P2 x y where
      x = r * (fromRadian $ cos ang)
      y = r * (fromRadian $ sin ang)
