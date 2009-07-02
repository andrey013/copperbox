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

import Wumpus.Drawing.PostScript



type RgbColour = DRGB

data PathStyle = Stroke | Fill | Clip
  deriving (Eq,Show) 

-- can only stroke an /open/ path... 
strokeOpenPathSkel :: WumpusM a -> WumpusM a
strokeOpenPathSkel m = saveExecRestore $ do
  ps_newpath
  a <- m
  ps_stroke
  return a



closedPathSkel :: PathStyle -> WumpusM a -> WumpusM a
closedPathSkel pstyle m = saveExecRestore $ do
    ps_newpath
    a <- m
    ps_closepath
    drawCmd
    return a
  where
    drawCmd = case pstyle of 
                Stroke -> ps_stroke
                Fill   -> ps_fill   
                _      -> ps_clip

fillPathSkel :: WumpusM a -> WumpusM a
fillPathSkel = closedPathSkel Fill

strokePathSkel :: WumpusM a -> WumpusM a
strokePathSkel = closedPathSkel Stroke

clipPathSkel :: WumpusM a -> WumpusM a
clipPathSkel = closedPathSkel Clip


-- needs a rethink vis Fill/Stroke/Clip...
polygon :: [DPoint2] -> WumpusM ()
polygon []          = return ()
polygon (P2 x y:ps) = strokePathSkel $ do 
    ps_moveto x y
    mapM_ lineto' ps 
  where
    lineto' (P2 a b) = ps_lineto a b


squarepath :: (Double,Double) -> (Double,Double) -> WumpusM ()
squarepath (x1,y1) (x2,y2) = do 
  ps_moveto x1 y1
  ps_lineto x1 y2
  ps_lineto x2 y2
  ps_lineto x2 y1
  ps_closepath


movetoPt :: DPoint2 -> WumpusM ()
movetoPt (P2 x y) = ps_moveto x y

linetoPt :: DPoint2 -> WumpusM ()
linetoPt (P2 x y) = ps_lineto x y 



--- Old rubbish...

wedge :: (Double,Double) -> Double -> Double -> Double -> WumpusM ()
wedge (x,y) r ang1 ang2 =  strokePathSkel $ do
  ps_moveto x y
  ps_arc x y r ang1 ang2
 
ellipse :: (Double,Double) -> (Double,Double) -> WumpusM ()
ellipse (x,y) (rh,rv) = saveExecRestore $ do 
  ps_scale 1 (rv/rh)
  ps_newpath
  ps_arc x y rh 0 360
  ps_closepath
  ps_stroke


ellipticarc :: (Double,Double) -> (Double,Double) -> Double -> Double -> WumpusM ()
ellipticarc (x,y) (rh,rv) ang1 ang2 = saveExecRestore $ do 
  ps_scale 1 (rv/rh)
  ps_newpath
  ps_arc x y rh ang1 ang2
  ps_stroke

