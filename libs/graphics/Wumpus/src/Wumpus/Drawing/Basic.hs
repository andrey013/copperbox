{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Drawing.Basic
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


module Wumpus.Drawing.Basic where

import Wumpus.Core.Colour
import Wumpus.Core.Instances
import Wumpus.Core.Matrix
import Wumpus.Core.Point
import Wumpus.Core.Vector
import Wumpus.Core.Wumpus

import Data.AffineSpace
import Data.VectorSpace

import Prelude hiding ( concat ) 

type RgbColour = Colour3
type Point = DPoint2

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

type Line = (Point,Point)

line :: (Double,Double) -> (Double,Double) -> Line
line (x1,y1) (x2,y2) = (P2 x1 y1, P2 x2 y2) 

drawLine :: Line -> WumpusM ()
drawLine (P2 x1 y1, P2 x2 y2) = strokePathSkel $ do 
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

unitSquare :: Point -> Polygon
unitSquare p = [p, p .+^ (V2 0 1), p .+^ (V2 1 1), p .+^ (V2 1 0)] 

type Polygon = [Point]


-- drawing a two colour polygon (path one colour, fill another)
-- uses the trick from Bill Casselman MI section 1.8

drawPolygon :: PolygonEnv -> [DPoint2] -> WumpusM ()
drawPolygon _   []            = return ()
drawPolygon env ((P2 x y):ps) =  saveExecRestore $ do 
   case env of 
     PolygonEnv (Just fEnv) (Just sEnv) -> do 
         polygonPath
         gsave
         setRgbColour $ _fillColour fEnv
         fill
         grestore
         setRgbColour $ _lineColour sEnv
         setlinewidth $ _lineWidth sEnv
         stroke
         
     PolygonEnv (Just fEnv) _           -> do 
         polygonPath
         setRgbColour $ _fillColour fEnv
         fill
     PolygonEnv Nothing     (Just sEnv) -> do
         polygonPath
         setRgbColour $ _lineColour sEnv
         setlinewidth $ _lineWidth sEnv
         stroke

     -- default is a stroked path
     _                                  -> polygonPath >> stroke    
       
  where
    polygonPath = do 
      newpath
      moveto x y
      mapM_ (\(P2 a b) -> lineto a b) ps 
      closepath

setRgbColour :: RgbColour -> WumpusM ()
setRgbColour (V3 r g b) = setrgbcolor r g b

whenMb :: Monad m => Maybe a -> (a -> m ()) -> m()
whenMb a sk = maybe (return ()) sk a 
   
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



diamond :: (Double,Double) -> (Double,Double) -> Polygon
diamond (x1,y1) (w,h) = map (trans1.scale1.rot1) xs where
  xs     = unitSquare $ P2 0 0
  rot1   = vecMult $ rotationMatrix (pi/4)
  scale1 = vecMult $ scalingMatrix w h
  trans1 = vecMult $ translationMatrix x1 y1

--------------------------------------------------------------------------------
-- arcs and ellipses

type Radius = Double
type Origin = Point

type Circle = (Origin,Radius)

circle :: (Double,Double) -> Double -> Circle
circle (x,y) r = (P2 x y, r)

drawCircle  :: Circle -> WumpusM ()
drawCircle (P2 x y, r) = closeStrokePathSkel $ 
  arc x y r 0 360 

data Disk = Disk Origin Radius Fill
  deriving (Eq,Show)
   
disk :: (Double,Double) -> Double -> Disk
disk (x,y) r = Disk (P2 x y) r envId

instance FillColour Disk where
  fillColour c (Disk o r env) = Disk o r (fillColour c env)

drawDisk  :: Disk -> WumpusM ()
drawDisk (Disk (P2 x y) r (Fill c)) = closeFillPathSkel $ do
  setRgbColour c
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



-- dots

plusDot :: Point -> WumpusM ()
plusDot (P2 x y) = do
    drawLine (trans1 p1, trans1 p2)
    drawLine (trans1.rot1 $ p1, trans1.rot1 $ p2)
  where 
    p1 = zeroV .+^ (V2 (-2) 0)
    p2 = zeroV .+^ (V2 2 0)  
    
    rot1   = vecMult $ rotationMatrix (pi/2)
    trans1 = vecMult $ translationMatrix x y

