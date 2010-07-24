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
  -- * Types
    Chart
  , Dataset
  
  , PointSize
  , LineWidth
  , Range(..)

  -- * Output
  , writeChartEPS
  , writeChartSVG

  -- * Drawing rectangles
  , rangeDist
  , withinRange
  , RangeProjection
  , projection
  

  -- * functions
  , rescale
  , clamp
  , contains
  
  , ffloat
  , minMax
  
  ) where



import Wumpus.Core                              -- package: wumpus-core

import Numeric



type Chart = DPicture

type Dataset ux uy = [(ux,uy)]



--------------------------------------------------------------------------------


-- | PointSize - synonymously font size.
--
type PointSize = Int


-- | LineWidth - 1 = 1%72 of an inch.
-- 
type LineWidth = Double

-- | 'Range' @ (min, max) @
--
data Range u = u ::: u
  deriving (Eq,Ord,Show)




--------------------------------------------------------------------------------
-- Output

writeChartEPS :: FilePath -> Chart -> IO ()
writeChartEPS = writeEPS_latin1 


writeChartSVG :: FilePath -> Chart -> IO ()
writeChartSVG = writeSVG_latin1 




--------------------------------------------------------------------------------



-- | 'rangeDist' - max - min.
--
rangeDist :: Num u => Range u -> u
rangeDist (u ::: v) = v-u

withinRange :: Ord u => u -> Range u -> Bool
withinRange a (u ::: v) = u <= a && a <= v


-- for Wumpus.Basic.Graphic ?
-- 

type Projection      ua u = ua -> u
type RangeProjection ua u = (Range ua, ua -> u)

projection :: Fractional u
           => Range ua -> Range u -> (ua -> u) -> Projection ua u
projection (ua0 ::: ua1) (u0 ::: u1) fromUA = 
   rescale (fromUA ua0) (fromUA ua1) u0 u1 . fromUA




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


