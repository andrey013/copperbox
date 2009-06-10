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
import Wumpus.Core.Line
import Wumpus.Core.Matrix
import Wumpus.Core.Point
import Wumpus.Core.Transformations
import Wumpus.Core.Vector
import Wumpus.Core.Wumpus hiding ( translate, rotate )

import Wumpus.Drawing.PSSkeletons


import Data.AffineSpace
import Data.VectorSpace

import Prelude hiding ( concat ) 

type Point = DPoint2

type Radius = Double
type Origin = Point



-- really a line segment...
data Line = Line DPoint2 DPoint2 
  deriving (Eq,Show)


line :: (Double,Double) -> (Double,Double) -> Line
line (x1,y1) (x2,y2) = Line (P2 x1 y1) (P2 x2 y2) 

drawLine :: Line -> WumpusM ()
drawLine (Line (P2 x1 y1) (P2 x2 y2)) = strokePathSkel $ do 
    moveto x1 y1
    lineto x2 y2

drawPoint :: DPoint2 -> WumpusM ()
drawPoint = polygon . unitSquare


squarepath :: (Double,Double) -> (Double,Double) -> WumpusM ()
squarepath (x1,y1) (x2,y2) = do 
  moveto x1 y1
  lineto x1 y2
  lineto x2 y2
  lineto x2 y1
  closepath

-- should this generate a Polygon or its path?
-- unitSquare :: Point -> Polygon
unitSquare :: Point -> [Point]
unitSquare p = usqr where 
    usqr = [p, p .+^ (V2 0 1), p .+^ (V2 1 1), p .+^ (V2 1 0)]

data Polygon = Polygon [Point] PolygonEnv
  deriving (Eq,Show)


-- drawing a two colour polygon (path one colour, fill another)
-- uses the trick from Bill Casselman MI section 1.8

drawPolygon :: Polygon -> WumpusM ()
drawPolygon (Polygon []            _  ) = return ()
drawPolygon (Polygon ((P2 x y):ps) env) = saveExecRestore $ do 
    case env of
      PolygonEnv (Just fEnv) (Just sEnv) ->
        do { polygonPath
           ; gsave
           ; setRgbColour $ _fillColour fEnv
           ; fill
           ; grestore
           ; setRgbColour $ _lineColour sEnv
           ; setlinewidth $ _lineWidth sEnv
           ; stroke
           }
         
      PolygonEnv (Just fEnv) _           ->
        do { polygonPath
           ; setRgbColour $ _fillColour fEnv
           ; fill
           }
      PolygonEnv Nothing     (Just sEnv) ->
        do { polygonPath
           ; setRgbColour $ _lineColour sEnv
           ; setlinewidth $ _lineWidth sEnv
           ; stroke
           }
      -- default is a stroked path
      _                                  -> do { polygonPath; stroke }
       
  where
    polygonPath = do 
        newpath
        moveto x y
        mapM_ (\(P2 a b) -> lineto a b) ps 
        closepath

setRgbColour :: RgbColour -> WumpusM ()
setRgbColour (RGB3 r g b) = setrgbcolor r g b

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



diamond :: (Double,Double) -> (Double,Double) -> Polygon
diamond (x1,y1) (w,h) = Polygon xs envId 
  where
    xs     = map (trans1.scale1.rot1) $ unitSquare $ P2 0 0
    rot1   = (*#) $ rotationMatrix (pi/4)
    scale1 = (*#) $ scalingMatrix w h
    trans1 = (*#) $ translationMatrix x1 y1

--------------------------------------------------------------------------------
-- arcs and ellipses

data Circle = Circle Origin Radius Stroke

circle :: (Double,Double) -> Double -> Circle
circle (x,y) r  = Circle (P2 x y) r envId

drawCircle  :: Circle -> WumpusM ()
drawCircle (Circle (P2 x y) r env) = closeStrokePathSkel $ 
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



-- dots

plusDot :: Point -> [DLineSegment]
plusDot (P2 x y) = map (translate x y) [ls1,ls2]
  where
    ls1 = lineTo p1 p2
    ls2 = rotate90 ls1
    p1, p2 ::Point
    p1  = origin .+^ (V2 (-2) 0)
    p2  = origin .+^ (V2 2 0)  


asterisk :: Point -> [DLineSegment]
asterisk (P2 x y) = zipWith fn (replicate 5 ls1) [0..4]
  where
   ls1 = vline origin 2  
   fn ln theta = translate x y $ rotate ((2*theta*pi)/5) ln


drawLineSegment :: DLineSegment -> WumpusM ()
drawLineSegment (LS p p') = closeStrokePathSkel $ do 
    movetoPt p
    linetoPt p'