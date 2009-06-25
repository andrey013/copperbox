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

import Wumpus.Core.Fun
import Wumpus.Core.Instances
import Wumpus.Core.Line
import Wumpus.Core.Point
import Wumpus.Core.Transformations
import Wumpus.Core.Vector

import Wumpus.Drawing.BasicCF

import Data.AffineSpace

--  What is the relation between a grid and a frame? 


oldGrid :: DPoint2 -> DPoint2 -> [DLineSegment2]
oldGrid (P2 x0 y0) (P2 x1 y1) = map (pointwise (scale 10 10)) $ hlines ++ vlines 
  where
    hlines = [ lineTo (P2 x0 y) (P2 x1 y) | y <- [y0..y1]]
    vlines = [ lineTo (P2 x y0) (P2 x y1) | x <- [x0..x1]]


type Grid = LineBag

-- | simple borderless grid
grid :: Int -> Int -> DPoint2 -> Grid
grid xstep ystep tr = \o -> let vecbound = tr .-. o in
                            hlines vecbound o ++ vlines vecbound o
  where
    hlines (V2 x y) o = zipWith xtrans (steps xstep x) (repeat $ line (vvec y) o)
    vlines (V2 x y) o = zipWith ytrans (steps ystep y) (repeat $ line (hvec x) o)
    xtrans d = translate d 0 
    ytrans d = translate 0 d



