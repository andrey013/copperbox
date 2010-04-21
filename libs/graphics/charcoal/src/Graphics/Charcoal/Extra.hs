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
import Data.Bits ( (.|.) )


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

vstrip :: Region
vstrip (P2 x y) = abs x <= 0.5

checker :: Region
checker (P2 x y) = even (floor x + floor y)

altRings :: Region
altRings p = even $ floor $ distO p


distO (P2 x y) = sqrt (x*x + y*y)

gasket :: Region
gasket (P2 x y) = floor x .|. floor y == ((floor x)::Int)


regionToPicture :: Greyscale -> Greyscale -> Region -> Picture
regionToPicture exter inter regf = \pt -> if (regf pt) then inter else exter 


unitcircle :: Region
unitcircle (P2 x y) = x*x + y*y <= 1.0

circle :: Double -> Region 
circle r = \(P2 x y) -> x*x + y*y <= r*r


vhalfplane :: Region
vhalfplane (P2 x _) = x >= 0

hhalfplane :: Region
hhalfplane (P2 _ y) = y >= 0


(/\) :: Region -> Region -> Region
(/\) r1 r2 = \pt -> r1 pt && r2 pt

(\/) :: Region -> Region -> Region
(\/) r1 r2 = \pt -> r1 pt || r2 pt

outside :: Region -> Region
outside = (not .)


annulus :: Double -> Double -> Region 
annulus r1 r2 = outside (circle r1) /\ circle r2


