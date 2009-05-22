{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.WumpusLib.Arrow
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Draw arrows
--
--------------------------------------------------------------------------------


module Graphics.WumpusLib.Arrow where

import Graphics.Wumpus.Point
import Graphics.Wumpus.Wumpus


arrow :: Monad m => DPoint2 -> DPoint2 -> PsT m ()
arrow p1@(P2 x1 y1) p2@(P2 x2 y2) = saveExecRestore $ do
    setmiterlimit 1 
    newpath
    moveto x1 y1
    lineto x2 y2 
    rotate (360 - r2d theta)
    rlineto (-10) (-4)
    rmoveto   10    4   -- back to origin
    rlineto (-10)   4
    closepath
    stroke
  where
    theta = tan (y2-y1/x2-x1)



d2r :: Double -> Double 
d2r = (*) (pi/180)

r2d :: Double -> Double
r2d = (*) (180/pi)



vee :: Monad m => Double -> Double -> Double -> PsT m ()
vee x y theta = saveExecRestore $ do
   setmiterlimit 1
   newpath
   moveto x y
   translate x y
   rotate theta
   rlineto (-10) (-4)
   rmoveto   10    4   -- back to origin
   rlineto (-10)   4
   closepath
   stroke


triangle :: Monad m => Double -> Double -> Double -> PsT m ()
triangle x y theta = saveExecRestore $ do
   newpath
   moveto x y
   translate x y
   rotate theta
   rlineto (-10) (-4)
   rlineto   0     8  
   closepath
   fill
 