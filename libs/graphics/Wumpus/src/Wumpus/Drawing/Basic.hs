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
import Wumpus.Core.CTM ( psMatrix )
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
         whenMb (mbFillColour fEnv) rgbSetColour
         fill
         grestore
         whenMb (mbLineColour sEnv) rgbSetColour
         whenMb (mbLineWidth sEnv) setlinewidth
         stroke
         
     PolygonEnv (Just fEnv) _           -> do 
         polygonPath
         whenMb (mbFillColour fEnv) rgbSetColour
         fill
     PolygonEnv Nothing     (Just sEnv) -> do
         polygonPath
         whenMb (mbLineColour sEnv) rgbSetColour
         whenMb (mbLineWidth sEnv) setlinewidth
         stroke

     -- default is a stroked path
     _                                  -> polygonPath >> stroke    
       
  where
    polygonPath = do 
      newpath
      moveto x y
      mapM_ (\(P2 a b) -> lineto a b) ps 
      closepath

rgbSetColour :: RgbColour -> WumpusM ()
rgbSetColour (V3 r g b) = setrgbcolor r g b

whenMb :: Monad m => Maybe a -> (a -> m ()) -> m()
whenMb a sk = maybe (return ()) sk a 
   
data Fill = Fill { mbFillColour :: Maybe RgbColour }

data Stroke = Stroke { mbLineWidth :: Maybe Double, mbLineColour :: Maybe RgbColour }

data PolygonEnv = PolygonEnv { mbFill :: Maybe Fill, mbStroke :: Maybe Stroke }

class Env env where
  envId :: env

instance Env Fill where
  envId = Fill Nothing

instance Env Stroke where
  envId = Stroke Nothing Nothing

instance Env (Maybe a) where
  envId = Nothing

instance Env PolygonEnv where
  envId = PolygonEnv envId envId

class FillColour env where 
  fillColour :: RgbColour -> env -> env

instance FillColour Fill where
  fillColour c e = e { mbFillColour = Just c }

instance (Env e, FillColour e) => FillColour (Maybe e) where
  fillColour c (Just e) = Just $ fillColour c e
  fillColour c Nothing  = Just $ fillColour c envId
 
instance FillColour PolygonEnv where
  fillColour c (PolygonEnv f s) = PolygonEnv (fillColour c f) s


class LineWidth env where
  lineWidth :: Double -> env -> env 

instance LineWidth Stroke where
  lineWidth w e = e { mbLineWidth = Just w }

instance (Env e, LineWidth e) => LineWidth (Maybe e) where
  lineWidth w (Just e) = Just $ lineWidth w e
  lineWidth w Nothing  = Just $ lineWidth w envId

instance LineWidth PolygonEnv where
  lineWidth w (PolygonEnv f s) = PolygonEnv f (lineWidth w s) 

class LineColour env where
  lineColour :: RgbColour -> env -> env 

instance LineColour Stroke where
  lineColour c e = e { mbLineColour = Just c }

instance (Env e, LineColour e) => LineColour (Maybe e) where
  lineColour c (Just e) = Just $ lineColour c e
  lineColour c Nothing  = Just $ lineColour c envId

instance LineColour PolygonEnv where
  lineColour c (PolygonEnv f s) = PolygonEnv f (lineColour c s) 



diamond2 :: (Double,Double) -> (Double,Double) -> Polygon
diamond2 (x1,y1) (w,h) = map (trans1.scale1.rot1) xs where
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



-- dots

plusDot :: Monad m => DPoint2 -> PsT m ()
plusDot (P2 x y) = do
    line2 (trans1 p1) (trans1 p2)
    line2 (trans1.rot1 $ p1) (trans1.rot1 $ p2)
  where 
    p1 = zeroV .+^ (V2 (-2) 0)
    p2 = zeroV .+^ (V2 2 0)  
    
    rot1   = vecMult $ rotationMatrix (pi/2)
    trans1 = vecMult $ translationMatrix x y


line2 :: Monad m => DPoint2 -> DPoint2 -> PsT m ()
line2 (P2 a b) (P2 m n) = line (a,b) (m,n)