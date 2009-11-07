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


module Wumpus.Core.Colour 
  (
  -- * Colour types
    RGB3(..)
  , DRGB
  , HSB3(..)
  , DHSB

  -- * Operations
  , rgb2hsb
  , hsb2rgb

  , rgb2gray
  , gray2rgb

  , hsb2gray
  , gray2hsb
  
  -- * Predefined colours
  , black
  , white
  , red
  , green
  , blue

  ) where

import Wumpus.Core.Utils


import Data.VectorSpace

data RGB3 a = RGB3 !a !a !a
  deriving (Eq,Show)

type DRGB = RGB3 Double


data HSB3 a = HSB3 !a !a !a 
  deriving (Eq,Show)

type DHSB = HSB3 Double 



instance Num a => Num (RGB3 a) where
  (+) (RGB3 a b c) (RGB3 x y z) = RGB3 (a+x) (b+y) (c+z)
  (-) (RGB3 a b c) (RGB3 x y z) = RGB3 (a-x) (b-y) (c-z)
  (*) (RGB3 a b c) (RGB3 x y z) = RGB3 (a*x) (b*y) (c*z)
  abs (RGB3 a b c)            = RGB3 (abs a) (abs b) (abs c)
  negate (RGB3 a b c)         = RGB3 (negate a) (negate b) (negate c)
  signum (RGB3 a b c)         = RGB3 (signum a) (signum b) (signum c)
  fromInteger i = RGB3 (fromInteger i) (fromInteger i) (fromInteger i)

instance Fractional a => Fractional (RGB3 a) where
  (/) (RGB3 a b c) (RGB3 x y z) = RGB3 (a/x) (b/y) (c/z)
  recip (RGB3 a b c)            = RGB3 (recip a) (recip b) (recip c)
  fromRational a = RGB3 (fromRational a) (fromRational a) (fromRational a)
 
instance Num a => AdditiveGroup (RGB3 a) where
  zeroV = RGB3 0 0 0
  (^+^) = (+)
  negateV = negate

instance (Num a, VectorSpace a) => VectorSpace (RGB3 a) where
  type Scalar (RGB3 a) = Scalar a
  s *^ (RGB3 a b c) = RGB3 (s*^a) (s*^b) (s*^c)



instance Num a => Num (HSB3 a) where
  (+) (HSB3 a b c) (HSB3 x y z) = HSB3 (a+x) (b+y) (c+z)
  (-) (HSB3 a b c) (HSB3 x y z) = HSB3 (a-x) (b-y) (c-z)
  (*) (HSB3 a b c) (HSB3 x y z) = HSB3 (a*x) (b*y) (c*z)
  abs (HSB3 a b c)            = HSB3 (abs a) (abs b) (abs c)
  negate (HSB3 a b c)         = HSB3 (negate a) (negate b) (negate c)
  signum (HSB3 a b c)         = HSB3 (signum a) (signum b) (signum c)
  fromInteger i = HSB3 (fromInteger i) (fromInteger i) (fromInteger i)

instance Fractional a => Fractional (HSB3 a) where
  (/) (HSB3 a b c) (HSB3 x y z) = HSB3 (a/x) (b/y) (c/z)
  recip (HSB3 a b c)            = HSB3 (recip a) (recip b) (recip c)
  fromRational a = HSB3 (fromRational a) (fromRational a) (fromRational a)
 
instance Num a => AdditiveGroup (HSB3 a) where
  zeroV = HSB3 0 0 0
  (^+^) = (+)
  negateV = negate

instance (Num a, VectorSpace a) => VectorSpace (HSB3 a) where
  type Scalar (HSB3 a) = Scalar a
  s *^ (HSB3 a b c) = HSB3 (s*^a) (s*^b) (s*^c)

--------------------------------------------------------------------------------
-- Operations


vE :: DRGB
vE = RGB3 1 1 1

-- Acknowledgment - the conversion functions are derived from
-- the documentation to Dr. Uwe Kern's xcolor LaTeX package



rgb2hsb :: DRGB -> DHSB
rgb2hsb (RGB3 r g b) = HSB3 hue sat bri
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



hsb2rgb :: DHSB -> DRGB
hsb2rgb (HSB3 hue sat bri) = bri *^ (vE - (sat *^ fV))
  where
    i     :: Int
    i     = floor $ (6 * hue)
    f     = (6 * hue) - fromIntegral i
    fV    | i == 0    = RGB3  0     (1-f) 1 
          | i == 1    = RGB3  f     0     1
          | i == 2    = RGB3  1     0     (1-f)
          | i == 3    = RGB3  1     f     0
          | i == 4    = RGB3  (1-f) 1     0
          | i == 5    = RGB3  0     1     f
          | otherwise = RGB3  0     1     1
          
rgb2gray :: DRGB -> Double
rgb2gray (RGB3 r g b) = 0.3 * r + 0.59 * g + 0.11 * b 

gray2rgb :: Double -> DRGB
gray2rgb gray = gray *^ vE

hsb2gray :: DHSB -> Double
hsb2gray (HSB3 _ _ b) = b 

gray2hsb :: Double -> DHSB
gray2hsb gray = HSB3 0 0 gray




--------------------------------------------------------------------------------

-- Some colours

-- There will be name clashes with the X11Colours / SVGColours.


black :: DRGB
black = RGB3 0 0 0

white :: DRGB
white = RGB3 1 1 1

red :: DRGB
red = RGB3 1 0 0

green :: DRGB 
green = RGB3 0 1 0

blue :: DRGB
blue = RGB3 0 0 1
