{-# LANGUAGE TypeFamilies               #-}
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
import Wumpus.Core.Curve
import Wumpus.Core.Line
import Wumpus.Core.Matrix
import Wumpus.Core.Point
import Wumpus.Core.Pointwise
import Wumpus.Core.PostScript
import Wumpus.Core.Transformations
import Wumpus.Core.Vector

import Wumpus.Drawing.PSSkeletons

import Data.AffineSpace


type Radius = Double
type Origin = DPoint2


data Polygon = Polygon [DPoint2]
  deriving (Eq,Show)


instance Pointwise Polygon where
  type Pt Polygon = DPoint2
  pointwise f (Polygon xs) = Polygon $ map f xs



data Circle = Circle Origin Radius
  deriving (Eq,Show)


instance Pointwise Circle where
  type Pt Circle = DPoint2
  pointwise f (Circle o r) = Circle (f o) r


--------------------------------------------------------------------------------



drawLine :: DLineSegment2 -> WumpusM ()
drawLine (LS (P2 x1 y1) (P2 x2 y2)) = strokePathSkel $ do 
    ps_moveto x1 y1
    ps_lineto x2 y2

drawPoint :: DPoint2 -> WumpusM ()
drawPoint = polygon . unitSquare

-- should this generate a Polygon or its path?
-- unitSquare :: Point -> Polygon
unitSquare :: DPoint2 -> [DPoint2]
unitSquare p = usqr where 
    usqr = [p, p .+^ (V2 0 1), p .+^ (V2 1 1), p .+^ (V2 1 0)]

drawPolygon :: Polygon -> WumpusM ()
drawPolygon (Polygon [])            = return ()
drawPolygon (Polygon ((P2 x y):ps)) = saveExecRestore $ do 
    ps_newpath
    ps_moveto x y
    mapM_ (\(P2 a b) -> ps_lineto a b) ps 
    ps_closepath
    ps_stroke


drawLineBag :: [DLineSegment2] -> WumpusM ()
drawLineBag []  = return ()
drawLineBag xs  = strokeOpenPathSkel $ mapM_ step xs
  where
    step (LS (P2 x1 y1) (P2 x2 y2)) = do 
      ps_moveto x1 y1
      ps_lineto x2 y2

setRgbColour :: RgbColour -> WumpusM ()
setRgbColour (RGB3 r g b) = ps_setrgbcolor r g b

whenMb :: Monad m => Maybe a -> (a -> m ()) -> m()
whenMb a sk = maybe (return ()) sk a 


diamond :: (Double,Double) -> DPoint2 -> Polygon
diamond (w,h) (P2 x1 y1) = Polygon xs
  where
    xs     = map (trans1.scale1.rot1) $ unitSquare $ P2 0 0
    rot1   = (*#) $ rotationMatrix (pi/4)
    scale1 = (*#) $ scalingMatrix w h
    trans1 = (*#) $ translationMatrix x1 y1

--------------------------------------------------------------------------------
-- arcs and ellipses

circle :: (Double,Double) -> Double -> Circle
circle (x,y) r  = Circle (P2 x y) r

drawCircle  :: Circle -> WumpusM ()
drawCircle (Circle (P2 x y) r) = strokePathSkel $ 
  ps_arc x y r 0 360 

data Disk = Disk Origin Radius
  deriving (Eq,Show)
   
disk :: (Double,Double) -> Double -> Disk
disk (x,y) r = Disk (P2 x y) r

drawDisk  :: Disk -> WumpusM ()
drawDisk (Disk (P2 x y) r) = fillPathSkel $ do
  ps_arc x y r 0 360


drawCurve :: DCurve -> WumpusM ()
drawCurve (Curve (P2 x0 y0) (P2 x1 y1) (P2 x2 y2) (P2 x3 y3)) = 
  strokeOpenPathSkel $  do 
    ps_moveto x0 y0
    ps_curveto x1 y1 x2 y2 x3 y3


-- also draw control points
drawBezier :: DCurve -> WumpusM ()
drawBezier (Curve (P2 x0 y0) (P2 x1 y1) (P2 x2 y2) (P2 x3 y3)) = 
  strokeOpenPathSkel $  do 
    ps_moveto x1 y1             -- start from point1
    ps_lineto x0 y0
    ps_curveto x1 y1 x2 y2 x3 y3
    ps_lineto x2 y2



--------------------------------------------------------------------------------
-- dots

dotPlus :: DPoint2 -> [DLineSegment2]
dotPlus (P2 x y) = map (translate x y) [ls1,ls2]
  where
    ls1 = hline (P2 (-2) 0) 4
    ls2 = rotate90 ls1
  

dotX :: DPoint2 -> [DLineSegment2]
dotX (P2 x y) = map (translate x y) [ls1,ls2]
  where
    ls1 = rotate30 $ vline (P2 0 (-2)) 4
    ls2 = rotate (5*pi/3) $ ls1


dotAsterisk :: DPoint2 -> [DLineSegment2]
dotAsterisk (P2 x y) = map (translate x y) $ circular (replicate 5 ls1)
  where
   ls1 = vline stdOrigin 2  


dotTriangle :: DPoint2 -> Polygon
dotTriangle = polyDot (P2 0 2) 3

dotSquare :: DPoint2 -> Polygon
dotSquare = polyDot (P2 1.5 1.5) 4

dotPentagon :: DPoint2 -> Polygon
dotPentagon = polyDot (P2 0 1.5) 5 

polyDot :: DPoint2 -> Int -> DPoint2 -> Polygon
polyDot pt1 n (P2 x y) = Polygon xs
  where
    xs = map (translate x y) $ circular $ replicate n pt1 

dotDiamond :: DPoint2 -> Polygon
dotDiamond (P2 x y) = Polygon (map (translate x y) [p1,p2,p3,p4])
  where
   p1 = P2 0 1.5
   p2 = P2 (-1) 0
   p3 = reflectY p1
   p4 = reflectX p2

 

drawLineSegment :: DLineSegment2 -> WumpusM ()
drawLineSegment (LS p p') = strokeOpenPathSkel $  do 
    movetoPt p
    linetoPt p'