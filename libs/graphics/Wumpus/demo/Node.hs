{-# OPTIONS -Wall #-}

module Node where

import Wumpus.Core.BoundingBox
import Wumpus.Core.Instances ()
import Wumpus.Core.Vector

import Wumpus.Drawing.Arrow
import Wumpus.Drawing.Path
import Wumpus.Drawing.Picture



demo1 :: IO ()
demo1 = writePicture "node1.ps"  drawing1 where
  drawing1  = displacePicture (V2 100 580) $ 
                  node1 <..> node2  <..> arr1
  node1     = dotDiamond  `at` (0,0)
  node2     = dotTriangle `at` (40,40)
  
  pt1       = extractCoordinate center node1
  pt2       = extractCoordinate center node2
  arr1      = picPath stroke $ arrow pt1 pt2 

