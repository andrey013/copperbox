{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.PSC.Core
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

module Graphics.PSC.Core
  (
  -- * Types
    Chart
  , DrawingContext
  , ScaleCtx
  , Dataset
  , DrawingRectangle
  , Projection(..)
  , XYProjection
  
  , PointSize
  , LineWidth
  , Range(..)

  -- * Output
  , writeChartEPS
  , writeChartSVG

  -- * functions
  , rectWidth
  , rectHeight

  , rangeDist
  
  , rescale
  , clamp
  , contains
  
  , ffloat
  , minMax
  
  ) where



import Wumpus.Core                      -- package: wumpus-core

import Numeric



type Chart = DPicture

-- | Note - this representation allows for zero, one or more
-- Primitives to be collected together.
--

type DrawingContext u v = (DrawingRectangle, u -> Double, v -> Double)

type ScaleCtx u v a = DrawingContext u v -> a  


type Dataset u v = [(u,v)]

-- | DrawingRectangle = (width,height)
--
type DrawingRectangle = (Double,Double) 


data Projection u = Projection 
      { proj_conv               :: u -> Double
      , proj_trans              :: Double
      , proj_scale              :: Double
      }

type XYProjection u v = (Projection u, Projection v)

--------------------------------------------------------------------------------


-- | PointSize - synonymously font size.
--
type PointSize = Int


-- | LineWidth - 1 = 1%72 of an inch.
-- 
type LineWidth = Double

-- | 'Range' @ min ::: max @
--
data Range u = u ::: u




--------------------------------------------------------------------------------
-- Output

writeChartEPS :: FilePath -> Chart -> IO ()
writeChartEPS = writeEPS_latin1 


writeChartSVG :: FilePath -> Chart -> IO ()
writeChartSVG = writeSVG_latin1 




--------------------------------------------------------------------------------

rectWidth :: DrawingRectangle -> Double
rectWidth (w,_) = w

rectHeight :: DrawingRectangle -> Double
rectHeight (_,h) = h 



-- | 'rangeDist' - max - min.
--
rangeDist :: Num u => Range u -> u
rangeDist (u ::: v) = v-u



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


