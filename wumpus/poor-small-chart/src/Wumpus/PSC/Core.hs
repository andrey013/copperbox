{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.PSC.Core
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC 
--
-- Core types, functions...
--
--------------------------------------------------------------------------------

module Wumpus.PSC.Core
  (
  -- * Dantaset
    Dataset
  , univariateVerticals
  , univariateHorizontals
  
  -- * Range (strict pair)
  , Range(..)
  , rangeDist
  , withinRange
  , resize
  , rangeScalingContext

  -- * functions
  , rescale
  , clamp
  , contains
  
  , ffloat
  , minMax
  
  ) where


import Wumpus.Basic.Graphic                     -- package: wumpus-basic

import Numeric


type Dataset ux uy = [(ux,uy)]

-- | Interpret the data as positions in the vertical, evenly
-- spaced in the horixontal between [0..1].
--
univariateVerticals :: Fractional ux => [uy] -> Dataset ux uy
univariateVerticals [] = []
univariateVerticals ys = zip (iterate (+ ux) 0) ys
  where
    ux = 1 / (fromIntegral $ length ys - 1)


-- | Interpret the data as positions in the horizontal, evenly
-- spaced in the vertical between [0..1].
--
univariateHorizontals :: Fractional uy => [ux] -> Dataset ux uy
univariateHorizontals [] = []
univariateHorizontals xs = zip xs (iterate (+ uy) 0)
  where
    uy = 1 / (fromIntegral $ length xs - 1)


--------------------------------------------------------------------------------

-- | 'Range' @ (min, max) @
--
data Range u = Range !u !u
  deriving (Eq,Ord,Show)



-- | 'rangeDist' - max - min.
--
rangeDist :: Num u => Range u -> u
rangeDist (Range u v) = v-u

withinRange :: Ord u => u -> Range u -> Bool
withinRange a (Range u v) = u <= a && a <= v

-- @rescale old_min old_max  new_min new_max  a@
--
resize :: (Num a, Fractional b) => Range a -> Range b -> (a -> b) -> a -> b
resize (Range amin amax) (Range bmin bmax) fn a = 
    bmin + offset * (output_range / input_range)  
  where
    input_range   = fn $ amax - amin
    output_range  = bmax - bmin
    offset        = fn $ a - amin 



rangeScalingContext :: (Num ux, Num uy, Fractional u) 
                    => Range ux -> Range u -> (ux -> u) 
                    -> Range uy -> Range u -> (uy -> u) 
                    -> ScalingContext ux uy u
rangeScalingContext rux rx fX ruy ry fY = 
    ScalingContext (\x -> resize rux rx fX x) (\y -> resize ruy ry fY y)


--------------------------------------------------------------------------------

-- @rescale old_min old_max  new_min new_max  a@
--
rescale :: Fractional a => a -> a -> a -> a -> a -> a
rescale amin amax bmin bmax a = 
    bmin + offset * (output_range / input_range)  
  where
    input_range   = amax - amin
    output_range  = bmax - bmin
    offset        = a - amin 




-- | @clamp min max a@ - clamp a to be with in the bounds 
-- min..max
--
clamp :: Ord a => a -> a -> a -> a
clamp amin amax a = max amin (min amax a)


-- | @contains : min * max * a -> Bool @ 
-- 
-- Is a within in the bounds min..max?
--
contains :: Ord a => a -> a -> a -> Bool
contains amin amax a = a >= amin && a <= amax


-- | As per showFFloat but makes a String...
--
ffloat :: RealFloat u => Int -> u -> String
ffloat prec = ($ "") . showFFloat (Just prec)


minMax :: (Ord u, Ord v) => Dataset u v -> ((u,v), (u,v))
minMax (x:xs) = foldr fn (x,x) xs where
    fn (u,v) ((u0,v0),(u1,v1)) = ((min u u0, min v v0), (max u u1, max v v1))
minMax _                    = error $ "minMax - empty dataset."


