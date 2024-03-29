{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Doodle.ScatterPlot
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC with TypeFamilies and more
--
-- Scatter plots...
-- 
--------------------------------------------------------------------------------

module Wumpus.Doodle.ScatterPlot
  ( 
    PlotPoint(..)

  , scatterPlot
  , cropPlot 


  ) where

import Wumpus.Extra.Dots
import Wumpus.Extra.Marks
import Wumpus.Core


class PlotPoint a where
  plotPoint :: a -> Point2 Double 

instance Real a => PlotPoint (Point2 a) where
  plotPoint = fmap realToFrac

scatterPlot :: (PlotPoint a, Mark t) 
             => Double -> Double -> t -> [a] -> Picture Double
scatterPlot sx sy attr xs = multi $ map (dotDisk attr) points
  where
    points          = map (movePt . plotPoint) xs
    movePt (P2 x y) = P2 (x*sx) (y*sy)

cropPlot :: (Num u, Ord u) => Picture u -> Picture u
cropPlot p = translate (-x) (-y) p where
  (P2 x y) = boundaryBottomRight $ boundary p 