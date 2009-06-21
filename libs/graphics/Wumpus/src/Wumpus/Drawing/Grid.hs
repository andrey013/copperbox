{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Drawing.Grid
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Grids
--
--------------------------------------------------------------------------------


module Wumpus.Drawing.Grid where

import Wumpus.Core.Instances
import Wumpus.Core.Line
import Wumpus.Core.Point
import Wumpus.Core.Transformations


-- Two points:
-- 1. Don't really want to be enumerating doubles
-- 2. What is the relation between a grid and a frame? 
--   (then we can dispose of the arbitrary scaling)

grid :: DPoint2 -> DPoint2 -> [DLineSegment2]
grid (P2 x0 y0) (P2 x1 y1) = map (pointwise (scale 10 10)) $ hlines ++ vlines 
  where
    hlines = [ lineTo (P2 x0 y) (P2 x1 y) | y <- [y0..y1]]
    vlines = [ lineTo (P2 x y0) (P2 x y1) | x <- [x0..x1]]
