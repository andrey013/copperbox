{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Extra.ScatterPlot
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  experimental
-- Portability :  GHC only
--
-- Dots
-- 
--------------------------------------------------------------------------------

module Wumpus.Extra.ScatterPlot
  ( 
    PlotPoint(..)

  , scatterPlot

  ) where

import Wumpus.Extra.Dots
import Wumpus.Core



class PlotPoint a where
  plotPoint :: a -> Point2 Double 

instance Real a => PlotPoint (Point2 a) where
  plotPoint = fmap realToFrac

scatterPlot :: (PlotPoint a, Ellipse t) 
            => BoundingBox Double -> t -> [a] -> Picture Double
scatterPlot bb attr xs = multi $ map (dotDisk attr) points 
  where
    xs'                       = map plotPoint xs
    ((xmin,xmax),(ymin,ymax)) = range xs'   
    sx                        = boundaryWidth bb  / (xmax-xmin)
    sy                        = boundaryHeight bb / (ymax-ymin)
    points  = map (\(P2 x y) -> P2 (sx*(x-xmin)) (sy*(y-ymin))) xs'



range :: (Num u, Ord u) => [Point2 u] -> ((u,u),(u,u))
range = fn . trace where
  fn (BBox (P2 x0 y0) (P2 x1 y1)) = ((x0,x1), (y0,y1))