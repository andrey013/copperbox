{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Core.Colour
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- RGB colour interpreted both as HSB and greyscale
--
--------------------------------------------------------------------------------


module Wumpus.Core.Colour where

import Data.VectorSpace

data Colour3 a = C3 !a !a !a
  deriving (Eq,Show)

type DColour3 = Colour3 Double


instance Num a => Num (Colour3 a) where
  (+) (C3 a b c) (C3 x y z) = C3 (a+x) (b+y) (c+z)
  (-) (C3 a b c) (C3 x y z) = C3 (a-x) (b-y) (c-z)
  (*) (C3 a b c) (C3 x y z) = C3 (a*x) (b*y) (c*z)
  abs (C3 a b c)            = C3 (abs a) (abs b) (abs c)
  negate (C3 a b c)         = C3 (negate a) (negate b) (negate c)
  signum (C3 a b c)         = C3 (signum a) (signum b) (signum c)
  fromInteger i             = C3 (fromInteger i) (fromInteger i) (fromInteger i)

instance Fractional a => Fractional (Colour3 a) where
  (/) (C3 a b c) (C3 x y z) = C3 (a/x) (b/y) (c/z)
  recip (C3 a b c)          = C3 (recip a) (recip b) (recip c)
  fromRational a            = C3 (fromRational a) (fromRational a) (fromRational a)
 
instance Num a => AdditiveGroup (Colour3 a) where
  zeroV = C3 0 0 0
  (^+^) = (+)
  negateV = negate

instance (Num a, VectorSpace a) => VectorSpace (Colour3 a) where
  type Scalar (Colour3 a) = Scalar a
  s *^ (C3 a b c) = C3 (s*^a) (s*^b) (s*^c)


max3 :: Double -> Double -> Double -> Double
max3 a b c = max (max a b) c

min3 :: Double -> Double -> Double -> Double
min3 a b c = min (min a b) c

med3 :: Double -> Double -> Double -> Double
med3 a b c = if c <= x then x else if c > y then y else c
  where 
    (x,y)                 = order a b
    order p q | p <= q    = (p,q)
              | otherwise = (q,p)

mkColour :: Num a => a -> a -> a -> Colour3 a
mkColour r g b = C3 r g b

triple :: Num a => (a,a,a) -> Colour3 a
triple (r,g,b) = mkColour r g b


eV :: DColour3
eV = C3 1 1 1

-- Acknowledgment - the conversion functions are derived from
-- the documentation to Dr. Uwe Kern's xcolor LaTeX package

rgb2hsb' :: Double -> Double -> Double -> DColour3
rgb2hsb' r g b = rgb2hsb $ C3 r g b

hsb2rgb' :: Double -> Double -> Double -> DColour3
hsb2rgb' h s b = hsb2rgb $ C3 h s b

rgb2gray' :: Double -> Double -> Double -> Double
rgb2gray' r g b = rgb2gray $ C3 r g b 



rgb2hsb :: DColour3 -> DColour3
rgb2hsb (C3 r g b) = C3 hue sat bri
  where
    x     = max3 r g b
    y     = med3 r g b
    z     = min3 r g b

    bri   = x

    (sat,hue) = if x==z then (0,0) else ((x-z)/x, f $ (x-y)/(x-z))
    
    f n | r >= g && g >= b    = (1/6) * (1-n) 
        | g >= r && r >= b    = (1/6) * (1+n)
        | g >= b && b >= r    = (1/6) * (3-n)
        | b >= g && g >= r    = (1/6) * (3+n)
        | b >= r && r >= g    = (1/6) * (5-n)
        | otherwise           = (1/6) * (5+n)



hsb2rgb :: DColour3 -> DColour3
hsb2rgb (C3 hue sat bri) = bri *^ (eV - (sat *^ fV))
  where
    i     :: Int
    i     = floor $ (6 * hue)
    f     = (6 * hue) - fromIntegral i
    fV    | i == 0    = triple (0,1-f,1)
          | i == 1    = triple (f,0,1)
          | i == 2    = triple (1,0,1-f)
          | i == 3    = triple (1,f,0)
          | i == 4    = triple (1-f,1,0)
          | i == 5    = triple (0,1,f)
          | otherwise = triple (0,1,1)
          
rgb2gray :: DColour3 -> Double
rgb2gray (C3 r g b) = 0.3 * r + 0.59 * g + 0.11 * b 



wumpusBlack :: DColour3
wumpusBlack = C3 0 0 0

wumpusWhite :: DColour3
wumpusWhite = C3 1 1 1

wumpusRed :: DColour3
wumpusRed = C3 1 0 0

wumpusGreen :: DColour3 
wumpusGreen = C3 0 1 0

wumpusBlue :: DColour3
wumpusBlue = C3 0 0 1

