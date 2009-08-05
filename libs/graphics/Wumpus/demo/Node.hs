{-# OPTIONS -Wall #-}

module Node where

import Wumpus.Core.BoundingBox
import Wumpus.Core.Frame
import Wumpus.Core.Line
import Wumpus.Core.Point
import Wumpus.Core.Polygon

import Wumpus.Drawing.Arrow
import Wumpus.Drawing.Basic

import MonadLib.Monads

demo1 :: IO ()
demo1 = writePicture "node1.ps"  drawing1 where
  drawing1  = withFont (timesRoman 9) $ displace 100 580 $ 
                  node1 <..> node2 <..> arr1
  node1     = dotDiamond  `at` (0,0)
  node2     = dotTriangle `at` (40,40)

  -- Note the extracted positions are relative to the already displaced
  -- frame, hence the runReader bit.
  -- Clearly this is a hack, how to do this properly needs thinking about.
  arr1      = do pt1 <- extractCoordinate center node1
                 pt2 <- extractCoordinate center node2
                 return $ runReader (ortho zeroPt) (picArrow $ arrow pt1 pt2)





