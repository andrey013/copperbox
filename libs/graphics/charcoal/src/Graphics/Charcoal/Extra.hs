{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Charcoal.Extra
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Acknowledgment - code here is taken from 
-- Jerzy Karczmarczuk\'s Clastic.
--
--------------------------------------------------------------------------------


module Graphics.Charcoal.Extra where 

import Graphics.Charcoal.Picture

class Norm2 a where
  norm2 :: a -> Double

instance Norm2 Vector where
  norm2 (V2 x y) = x*x+y*y

instance Norm2 Point where
  norm2 (P2 x y) = x*x+y*y

class VD a where
 (>/) :: a -> Double -> a

instance VD Point where
  (>/) (P2 x y) s = P2 (x/s) (y/s)


step :: (Floating a,Ord a) => a -> a
step x | x<0.0001  = 0
       | x>0.0001  = 1
       | otherwise = 0.5


unitdisk :: Point -> Double 
unitdisk p = step (1 - norm2 p)


nearest :: (Floating b, Ord b) => a -> a -> b -> a 
nearest a b t = if t <= 0.5 then a else b


sscaled :: VD a => Double -> (a -> t) -> a -> t
sscaled s tx p = tx (p >/ s)


polygon :: Int -> Point -> Double
polygon k (P2 x y)
  = let kd = realToFrac (dpi/fromIntegral k)
        kdj= kd * realToFrac (floor (realToFrac (atan2 y x / kd)))
        ks = kdj+kd
        sj = sin kdj
        cj = cos kdj
    in ((x-cj)*(sin ks-sj) - (cos ks-cj)*(y-sj))

dpi :: Double
dpi = 2.0*pi