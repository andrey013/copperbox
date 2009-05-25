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

import Wumpus.Core.Vector

import Data.VectorSpace

type Colour3 = DVec3

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

mkColour :: Double -> Double -> Double -> Colour3
mkColour r g b = V3 r g b

triple :: (Double, Double, Double) -> Colour3
triple (r,g,b) = mkColour r g b


eV :: Colour3
eV = V3 1 1 1

-- Acknowledgment - the conversion functions are derived from
-- the documentation to Dr. Uwe Kern's xcolor LaTeX package

rgb2hsb' :: Double -> Double -> Double -> Colour3
rgb2hsb' r g b = rgb2hsb $ V3 r g b

hsb2rgb' :: Double -> Double -> Double -> Colour3
hsb2rgb' h s b = hsb2rgb $ V3 h s b

rgb2gray' :: Double -> Double -> Double -> Double
rgb2gray' r g b = rgb2gray $ V3 r g b 



rgb2hsb :: Colour3 -> Colour3
rgb2hsb (V3 r g b) = V3 hue sat bri
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



hsb2rgb :: Colour3 -> Colour3
hsb2rgb (V3 hue sat bri) = bri *^ (eV - (sat *^ fV))
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
          
rgb2gray :: Colour3 -> Double
rgb2gray (V3 r g b) = 0.3 * r + 0.59 * g + 0.11 * b 



wumpusBlack :: Colour3
wumpusBlack = triple (0,0,0)

wumpusWhite :: Colour3
wumpusWhite = triple (1,1,1)

wumpusRed :: Colour3
wumpusRed = triple (1,0,0)

wumpusGreen :: Colour3 
wumpusGreen = triple (0,1,0)

wumpusBlue :: Colour3
wumpusBlue = triple (0,0,1)

