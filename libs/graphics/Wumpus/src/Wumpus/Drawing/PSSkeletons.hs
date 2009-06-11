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



--- More old stuff

{-
   
data Fill = Fill { _fillColour :: RgbColour }
  deriving (Eq,Show)

data Stroke = Stroke { _lineWidth :: Double, _lineColour :: RgbColour }
  deriving (Eq,Show)

data PolygonEnv = PolygonEnv { mbFill :: Maybe Fill, mbStroke :: Maybe Stroke }
  deriving (Eq,Show)

class Env env where
  envId :: env

instance Env Fill where
  envId = Fill wumpusBlack

instance Env Stroke where
  envId = Stroke 1 wumpusBlack

instance Env (Maybe a) where
  envId = Nothing

instance Env PolygonEnv where
  envId = PolygonEnv envId envId

class FillColour env where 
  fillColour :: RgbColour -> env -> env

instance FillColour Fill where
  fillColour c e = e { _fillColour = c }

instance (Env e, FillColour e) => FillColour (Maybe e) where
  fillColour c (Just e) = Just $ fillColour c e
  fillColour c Nothing  = Just $ fillColour c envId
 
instance FillColour PolygonEnv where
  fillColour c (PolygonEnv f s) = PolygonEnv (fillColour c f) s

instance FillColour Polygon where
  fillColour c (Polygon ps e) = Polygon ps (fillColour c e)

class LineWidth env where
  lineWidth :: Double -> env -> env 

instance LineWidth Stroke where
  lineWidth w e = e { _lineWidth = w }

instance (Env e, LineWidth e) => LineWidth (Maybe e) where
  lineWidth w (Just e) = Just $ lineWidth w e
  lineWidth w Nothing  = Just $ lineWidth w envId

instance LineWidth PolygonEnv where
  lineWidth w (PolygonEnv f s) = PolygonEnv f (lineWidth w s) 

class LineColour env where
  lineColour :: RgbColour -> env -> env 

instance LineColour Stroke where
  lineColour c e = e { _lineColour = c }

instance (Env e, LineColour e) => LineColour (Maybe e) where
  lineColour c (Just e) = Just $ lineColour c e
  lineColour c Nothing  = Just $ lineColour c envId

instance LineColour PolygonEnv where
  lineColour c (PolygonEnv f s) = PolygonEnv f (lineColour c s) 

-}
