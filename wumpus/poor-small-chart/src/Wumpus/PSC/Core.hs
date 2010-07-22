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
  , RangeProjection
  , RectangleLoc
  , withinRectangleLoc
  , projection
  , rectangleScaleCtx
  

  -- * functions
  , rescale
  , clamp
  , contains
  
  , ffloat
  , minMax
  
  ) where



import Wumpus.Core                              -- package: wumpus-core
import Wumpus.Basic.Graphic                     -- package: wumpus-basic
import Wumpus.Basic.Monads.CoordScaleMonad
import Numeric



type Chart u = Picture u

-- | Note - this representation allows for zero, one or more
-- Primitives to be collected together.
--

-- type DrawingContext u v = (DrawingRectangle, u -> Double, v -> Double)

-- type ScaleCtx u v a = DrawingContext u v -> a  


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

writeChartEPS :: (Real u, Floating u, PSUnit u) 
              => FilePath -> Chart u -> IO ()
writeChartEPS = writeEPS_latin1 


writeChartSVG :: (Real u, Floating u, PSUnit u) 
              => FilePath -> Chart u -> IO ()
writeChartSVG = writeSVG_latin1 




--------------------------------------------------------------------------------



-- | 'rangeDist' - max - min.
--
rangeDist :: Num u => Range u -> u
rangeDist (u ::: v) = v-u



-- for Wumpus.Basic.Graphic ?
-- 
type RectangleLoc u = (Rectangle u, Point2 u)


withinRectangleLoc :: (Num u, Ord u) => Point2 u -> RectangleLoc u -> Bool
withinRectangleLoc (P2 x y) (Rectangle w h, P2 ox oy) = 
   ox <= x && x <= (ox+w) && oy <= y && y <= (oy+h)



projection :: Fractional u
           => Range ua -> Range u -> (ua -> u) -> Projection ua u
projection (ua0 ::: ua1) (u0 ::: u1) fromUA = 
   rescale (fromUA ua0) (fromUA ua1) u0 u1 . fromUA

type RangeProjection ua u = (Range ua, ua -> u)


rectangleScaleCtx :: Fractional u 
                  => RangeProjection ux u 
                  -> RangeProjection uy u 
                  -> Rectangle u
                  -> ScaleCtx ux uy u
rectangleScaleCtx (x_range,fx) (y_range,fy) (Rectangle w h) = 
    ScaleCtx x_proj y_proj
  where
    x_proj  = projection x_range (0 ::: w) fx
    y_proj  = projection y_range (0 ::: h) fy




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


