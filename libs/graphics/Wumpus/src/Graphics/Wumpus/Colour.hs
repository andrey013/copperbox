{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Wumpus.Colour
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


module Graphics.Wumpus.Colour where

import Data.SG

type Colour3 = Triple Double

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


eV :: Colour3
eV = Triple (1,1,1)

-- Acknowledgment - the conversion functions are derived from
-- the documentation to Dr. Uwe Kern's xcolor LaTeX package

rgb2hsb' :: Double -> Double -> Double -> Colour3
rgb2hsb' r g b = rgb2hsb $ Triple (r,g,b)

hsb2rgb' :: Double -> Double -> Double -> Colour3
hsb2rgb' h s b = hsb2rgb $ Triple (h,s,b)

rgb2gray' :: Double -> Double -> Double -> Double
rgb2gray' r g b = rgb2gray $ Triple (r,g,b) 



rgb2hsb :: Colour3 -> Colour3
rgb2hsb (Triple(r,g,b)) = Triple (hue,sat,bri)
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
hsb2rgb (Triple(hue,sat,bri)) = bri `scaleRel` (eV - (sat `scaleRel` fV))
  where
    i     :: Int
    i     = floor $ (6 * hue)
    f     = (6 * hue) - fromIntegral i
    fV    | i == 0    = Triple (0,1-f,1)
          | i == 1    = Triple (f,0,1)
          | i == 2    = Triple (1,0,1-f)
          | i == 3    = Triple (1,f,0)
          | i == 4    = Triple (1-f,1,0)
          | i == 5    = Triple (0,1,f)
          | otherwise = Triple (0,1,1)
          
rgb2gray :: Colour3 -> Double
rgb2gray (Triple (r,g,b)) = 0.3 * r + 0.59 * g + 0.11 * b 



wumpusBlack :: Colour3
wumpusBlack = Triple (0,0,0)

wumpusWhite :: Colour3
wumpusWhite = Triple (1,1,1)

wumpusRed :: Colour3
wumpusRed = Triple (1,0,0)

wumpusGreen :: Colour3 
wumpusGreen = Triple (0,1,0)

wumpusBlue :: Colour3
wumpusBlue = Triple (0,0,1)

