{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.WumpusLib.Basic
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Basic shapes etc.
--
--------------------------------------------------------------------------------


module Graphics.WumpusLib.Basic where

import Graphics.Wumpus.CTM ( psMatrix )
import Graphics.Wumpus.Instances
import Graphics.Wumpus.Matrix
import Graphics.Wumpus.Point
import Graphics.Wumpus.Vector
import Graphics.Wumpus.Wumpus

import Data.AffineSpace

import Prelude hiding ( concat ) 

strokePathSkel :: Monad m => PsT m a -> PsT m a
strokePathSkel m = saveExecRestore $ do
  newpath
  a <- m
  stroke
  return a

fillPathSkel :: Monad m => PsT m a -> PsT m a
fillPathSkel m = saveExecRestore $ do
  newpath
  a <- m
  fill
  return a

closeStrokePathSkel :: Monad m => PsT m a -> PsT m a
closeStrokePathSkel m = saveExecRestore $ do
  newpath
  a <- m
  closepath
  stroke
  return a

closeFillPathSkel :: Monad m => PsT m a -> PsT m a
closeFillPathSkel m = saveExecRestore $ do
  newpath
  a <- m
  closepath
  fill
  return a



line :: Monad m => (Double,Double) -> (Double,Double) -> PsT m ()
line (x1,y1) (x2,y2) = strokePathSkel $ do 
  moveto x1 y1
  lineto x2 y2

polygon :: Monad m => [(Double,Double)] -> PsT m ()
polygon []         = return ()
polygon ((x,y):ps) = closeStrokePathSkel $ do 
   moveto x y
   mapM_ (uncurry lineto) ps 

squarepath :: Monad m => (Double,Double) -> (Double,Double) -> PsT m ()
squarepath (x1,y1) (x2,y2) = do 
  moveto x1 y1
  lineto x1 y2
  lineto x2 y2
  lineto x2 y1
  closepath

diamond :: Monad m => (Double,Double) -> (Double,Double) -> PsT m ()
diamond (x1,y1) (x2,_y2) = saveExecRestore $ do 
    translate x1 y1
    concat $ psMatrix ((1,(-1)),(1,1)) (negate w, 0)
    newpath 
    squarepath (0,0) (x2,x2) 
    stroke
  where
    w = x2/2

--- diamond again

unitSquare :: DPoint2 -> [DPoint2]
unitSquare p = [p, p .+^ (V2 0 1), p .+^ (V2 1 1), p .+^ (V2 1 0)] 

polygon2 :: Monad m => [DPoint2] -> PsT m ()
polygon2 []            = return ()
polygon2 ((P2 x y):ps) = closeStrokePathSkel $ do 
   moveto x y
   mapM_ (\(P2 a b) -> lineto a b) ps 


diamond2 :: Monad m => (Double,Double) -> (Double,Double) -> PsT m ()
diamond2 (x1,y1) (w,h) = polygon2 $  map (trans1.scale1.rot1) xs where
  xs     = unitSquare $ P2 0 0
  rot1   = vecMult $ rotationMatrix (pi/4)
  scale1 = vecMult $ scalingMatrix w h
  trans1 = vecMult $ translationMatrix x1 y1

--------------------------------------------------------------------------------
-- arcs and ellipses

circle  :: Monad m => (Double,Double) -> Double -> PsT m ()
circle (x,y) r = closeStrokePathSkel $ 
  arc x y r 0 360 
   

disk  :: Monad m => (Double,Double) -> Double -> PsT m ()
disk (x,y) r = closeFillPathSkel $ 
  arc x y r 0 360


wedge :: Monad m => (Double,Double) -> Double -> Double -> Double -> PsT m ()
wedge (x,y) r ang1 ang2 =  closeStrokePathSkel $ do
  moveto x y
  arc x y r ang1 ang2
 
ellipse :: Monad m => (Double,Double) -> (Double,Double) -> PsT m ()
ellipse (x,y) (rh,rv) = saveExecRestore $ do 
  scale 1 (rv/rh)
  newpath
  arc x y rh 0 360
  closepath
  stroke


ellipticarc :: Monad m 
            => (Double,Double) -> (Double,Double) -> Double -> Double -> PsT m ()
ellipticarc (x,y) (rh,rv) ang1 ang2 = saveExecRestore $ do 
  scale 1 (rv/rh)
  newpath
  arc x y rh ang1 ang2
  stroke

