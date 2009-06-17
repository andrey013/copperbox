{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Drawing.Arrow
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


module Wumpus.Drawing.Arrow where

import Wumpus.Core.Fun
import Wumpus.Core.Point
import Wumpus.Core.PostScript


arrow :: DPoint2 -> DPoint2 -> WumpusM ()
arrow p1@(P2 x1 y1) p2@(P2 x2 y2) = saveExecRestore $ do
    ps_setmiterlimit 1 
    ps_newpath
    ps_moveto x1 y1
    ps_lineto x2 y2 
    ps_rotate (360 - r2d theta)
    ps_rlineto (-10) (-4)
    ps_rmoveto   10    4   -- back to origin
    ps_rlineto (-10)   4
    ps_closepath
    ps_stroke
  where
    theta = tan (y2-y1/x2-x1)





vee :: Double -> Double -> Double -> WumpusM ()
vee x y theta = saveExecRestore $ do
   ps_setmiterlimit 1
   ps_newpath
   ps_moveto x y
   ps_translate x y
   ps_rotate theta
   ps_rlineto (-10) (-4)
   ps_rmoveto   10    4   -- back to origin
   ps_rlineto (-10)   4
   ps_closepath
   ps_stroke


triangle :: Double -> Double -> Double -> WumpusM ()
triangle x y theta = saveExecRestore $ do
   ps_newpath
   ps_moveto x y
   ps_translate x y
   ps_rotate theta
   ps_rlineto (-10) (-4)
   ps_rlineto   0     8  
   ps_closepath
   ps_fill
 