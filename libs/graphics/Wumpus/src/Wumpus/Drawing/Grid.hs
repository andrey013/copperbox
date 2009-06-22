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

import Wumpus.Core.Frame
import Wumpus.Core.Instances
import Wumpus.Core.Line
import Wumpus.Core.Point
import Wumpus.Core.Transformations
import Wumpus.Core.Vector

import Data.AffineSpace

-- Two points:
-- 1. Don't really want to be enumerating doubles
-- 2. What is the relation between a grid and a frame? 
--   (then we can dispose of the arbitrary scaling)

grid :: DPoint2 -> DPoint2 -> [DLineSegment2]
grid (P2 x0 y0) (P2 x1 y1) = map (pointwise (scale 10 10)) $ hlines ++ vlines 
  where
    hlines = [ lineTo (P2 x0 y) (P2 x1 y) | y <- [y0..y1]]
    vlines = [ lineTo (P2 x y0) (P2 x y1) | x <- [x0..x1]]


grid' :: DPoint2 -> DPoint2 -> (DFrame2 -> [DLineSegment2])
grid' o@(P2 x0 y0) (P2 x1 y1) = \(Frame2 o' xv yv) -> 
  let fV p   = p .+^ xv + yv
      hlines = [ lineTo (fV $ P2 x0 y) (fV $ P2 x1 y) | y <- [y0..y1]]
      vlines = [ lineTo (fV $ P2 x y0) (fV $ P2 x y1) | x <- [x0..x1]]
  in map (pointwise (scale 10 10)) $ hlines ++ vlines 



makeVector :: Point2 Double -> Point2 Double -> Vec2 Double
makeVector = (.-.)
  

-- 1.10

fun1'10 :: DPoint2 -> DFrame2 -> DFrame2 -> DVec2
fun1'10 p (Frame2 o _ _) (Frame2 o' _ _) = v2
  where v2 = (o .-. o') + (o' .-. p)

dummy :: DVec2
dummy = fun1'10 (P2 1 1) (ortho (P2 0 0)) (ortho (P2 0 1))
