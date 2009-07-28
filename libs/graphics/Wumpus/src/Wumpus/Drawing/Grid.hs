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

import Wumpus.Core.BoundingBox
import Wumpus.Core.Fun
import Wumpus.Core.Line
import Wumpus.Core.Point
import Wumpus.Core.Vector

import Wumpus.Drawing.Basic




grid :: Double -> Double -> Double -> Double -> Picture
grid xstep ystep w h = Picture $ \pt -> 
    fork (mapM_ drawLine, bounds) (gridlines pt)
  where
    xpoints pt@(P2 x0 _) = genPoints (\(P2 x _) -> x <~= (x0+w))
                                     (\(P2 x _) -> P2 (x+xstep) 0)
                                     pt
    ypoints pt@(P2 _ y0) = genPoints (\(P2 _ y) -> y <~= (y0+h))
                                     (\(P2 _ y) -> P2 0 (y+ystep))
                                     pt
    hlines = map (hline w) . ypoints 
    vlines = map (vline h) . xpoints

    gridlines pt = hlines pt ++ vlines pt     


vgrid :: Num a => (Integer,Integer) -> (Integer,Integer) -> [Vec2 a]
vgrid (i,j) (i',j') = [ mkvec a b | b <- [j..j']
                                  , a <- [i..i'] ]
  where
    mkvec a b = V2 (fromInteger a) (fromInteger b)

calendarGrid :: Num a => Int -> Int -> [Vec2 a]
calendarGrid st n = map transp $ take n $ drop st $  vgrid (0,0) (6,5)
  where
    transp (V2 x y) = (V2 x (tot-y))
    tot  = let (a,b) = (fromIntegral n) `divMod` 7 in 
           if (st+b >7) then (fromIntegral $ a+1) else (fromIntegral a)
