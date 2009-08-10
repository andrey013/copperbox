{-# OPTIONS -Wall #-}

module Node2 where

import Wumpus.Core.BoundingBox
import Wumpus.Core.Frame
-- import Wumpus.Core.Instances
import Wumpus.Core.Point
import Wumpus.Core.Pointwise
import Wumpus.Core.Polygon
import Wumpus.Core.Radian
import Wumpus.Core.Transformations
import Wumpus.Core.Vector

import Wumpus.Drawing.Path
import Wumpus.Drawing.Picture

import Data.AffineSpace




demo1 :: IO ()
demo1 = writePicture "node2.ps"  drawing1 where
  drawing1  = displacePicture (V2 100 580) $ 
                  node1 <..> node2  <..> arr1
  node1     = dotDiamond  `at` (0,0)
  node2     = dotTriangle `at` (40,40)
  
  pt1       = extractCoordinate center node1
  pt2       = extractCoordinate center node2
  arr1      = arrow pt1 (pt2 .-. pt1)  




-- from Arrow which currently depends on the old picture type


arrowheadTriangle :: Double -> Radian -> (Radian -> DPoint2 -> DPolygon)
arrowheadTriangle d ang = 
  \theta endpt -> let halfBW = d * fromRadian (tan ang) 
                      tri    = isoscelesTriangle (2*halfBW) d endpt
                  in   pointwise (rotateAbout (theta - pi/2) endpt)
                     $ pointwise (inFrame (ortho $ P2 halfBW d))
                     $ tri


arrow :: DPoint2 -> DVec2 -> Picture
arrow p v = (picPath $ VPath stroke ln) <..> tip
  where
    ln    = newPath p `lineTo` v
    theta = endGradient ln
    tip   = picPolygon stroke $ arrowheadTriangle 10 (pi/10) theta (p .+^ v)
 
