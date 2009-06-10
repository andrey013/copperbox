{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Drawing.PSSkeletons
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Some PostScript skeletons (templates) 
--
--------------------------------------------------------------------------------


module Wumpus.Drawing.PSSkeletons where

import Wumpus.Core.Colour
import Wumpus.Core.Instances ()
import Wumpus.Core.Point
import Wumpus.Core.Vector
import Wumpus.Core.Wumpus

import Data.AffineSpace


import Prelude hiding ( concat ) 


type RgbColour = DRGB


strokePathSkel :: WumpusM a -> WumpusM a
strokePathSkel m = saveExecRestore $ do
  newpath
  a <- m
  stroke
  return a

fillPathSkel :: WumpusM a -> WumpusM a
fillPathSkel m = saveExecRestore $ do
  newpath
  a <- m
  fill
  return a

closeStrokePathSkel :: WumpusM a -> WumpusM a
closeStrokePathSkel m = saveExecRestore $ do
  newpath
  a <- m
  closepath
  stroke
  return a

closeFillPathSkel :: WumpusM a -> WumpusM a
closeFillPathSkel m = saveExecRestore $ do
  newpath
  a <- m
  closepath
  fill
  return a

polygon :: [DPoint2] -> WumpusM ()
polygon []          = return ()
polygon (P2 x y:ps) = closeStrokePathSkel $ do 
    moveto x y
    mapM_ lineto' ps 
  where
    lineto' (P2 a b) = lineto a b


squarepath :: (Double,Double) -> (Double,Double) -> WumpusM ()
squarepath (x1,y1) (x2,y2) = do 
  moveto x1 y1
  lineto x1 y2
  lineto x2 y2
  lineto x2 y1
  closepath


movetoPt :: DPoint2 -> WumpusM ()
movetoPt (P2 x y) = moveto x y

linetoPt :: DPoint2 -> WumpusM ()
linetoPt (P2 x y) = lineto x y 



--- Old rubbish...

wedge :: (Double,Double) -> Double -> Double -> Double -> WumpusM ()
wedge (x,y) r ang1 ang2 =  closeStrokePathSkel $ do
  moveto x y
  arc x y r ang1 ang2
 
ellipse :: (Double,Double) -> (Double,Double) -> WumpusM ()
ellipse (x,y) (rh,rv) = saveExecRestore $ do 
  scale 1 (rv/rh)
  newpath
  arc x y rh 0 360
  closepath
  stroke


ellipticarc :: (Double,Double) -> (Double,Double) -> Double -> Double -> WumpusM ()
ellipticarc (x,y) (rh,rv) ang1 ang2 = saveExecRestore $ do 
  scale 1 (rv/rh)
  newpath
  arc x y rh ang1 ang2
  stroke

