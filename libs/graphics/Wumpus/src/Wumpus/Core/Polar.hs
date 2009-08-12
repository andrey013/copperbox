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

  , pmult

  ) where

import Wumpus.Core.Fun
import Wumpus.Core.Point
import Wumpus.Core.Radian
import Wumpus.Core.Vector

--------------------------------------------------------------------------------

data Polar a = Polar a Radian
  deriving (Eq)


--------------------------------------------------------------------------------
-- Type class instances

instance Show a => Show (Polar a) where
 showsPrec i (Polar r theta) = showsPrec i (r,theta)





class PolarCoord t where 
  toPolar :: (Real a, Floating a, Approx a) => t a -> Polar a
  fromPolar :: Fractional a => Polar a -> t a


mkTheta :: (Real a, Floating a, Approx a) => a -> a -> Radian
mkTheta x y | x =~= 0 && y >   0 = pi/2
            | x =~= 0 && y <   0 = 3 * pi/2
            | x =~= 0 && y =~= 0 = 0
            | x >   0 && y >~= 0 = toRadian $ atan (y/x)
            | x >   0 && y <   0 = toRadian $ atan (y/x) + (2*pi)
            | x <   0            = toRadian $ atan (y/x) + pi
            | otherwise          = 0 -- erk!

instance PolarCoord Vec2 where
  toPolar (V2 x y) = Polar r (mkTheta x y) where r = sqrt (x*x+y*y)
  fromPolar (Polar r theta) = V2 x y where
      x = r * (fromRadian $ cos theta)
      y = r * (fromRadian $ sin theta)


instance PolarCoord Point2 where
  toPolar (P2 x y) = Polar r (mkTheta x y) where r = sqrt (x*x+y*y)
  fromPolar (Polar r theta) = P2 x y where
      x = r * (fromRadian $ cos theta)
      y = r * (fromRadian $ sin theta)



pmult :: Num a => Polar a -> Polar a -> Polar a  
pmult (Polar r theta) (Polar r' theta') = Polar (r*r') (theta+theta')
