{-# OPTIONS -Wall #-}

module Arrow1 where

import Wumpus.Core.Frame
import Wumpus.Core.Instances
import Wumpus.Core.Matrix
import Wumpus.Core.Point
import Wumpus.Core.PostScript
import Wumpus.Core.Transformations
import Wumpus.Drawing.Arrow
import Wumpus.Drawing.Basic

import Wumpus.Core.Frame

demo1 :: IO ()
demo1 = writePS "arrow1.ps" $ runWumpus st0 $ drawing1 where
  drawing1 = do { ps_translate 60 380 
                ; arrow (P2 0 0) (P2 40 40) 
                ; arrowhead1
                }
             
arrowhead1 :: WumpusM ()
arrowhead1 = drawPolygon $ arrowheadTriangle 4 (pi/4) 0 (P2 10 10)


rotatePoint = print p1 where
  p1 = rotate45 (P2 10 (0::Double))

m000 :: Matrix3'3 Double
m000 = mT * mR 

m001 :: Matrix3'3 Double
m001 = mT * mR * mT'


m002 :: DPoint2
m002 = m001 *# p1

m003 :: DPoint2
m003 = (mT' * mR * mT) *# p1


p1 :: DPoint2
p1 = P2 10 0

p2 :: DPoint2 
p2 = P2 10 10

mR, mT, mT' :: Matrix3'3 Double
mR  = M3'3  1 0 0   0 (-1) 0   0 0 1
mT  = M3'3  1 0 2   0 1 4   0 0 1
mT' = M3'3  1 0 (-2)   0 1 (-4)   0 0 1
  
-- see utah

mRT :: Matrix3'3 Double
mRT = M3'3 1 0 2   0 (-1) 4   0 0 1

m004 = mRT *# p1

mRTa :: Matrix3'3 Double
mRTa = M3'3 1 0 (-2)   0 (-1) (-4)   0 0 1

m005 = mRTa *# p1


n001 :: Frame2 Double
n001 = ortho (P2 0 0)

n002 = p1 == p1 `pointInFrame` n001


n003 :: Frame2 Double
n003 = ortho (P2 2 4)

n004 = p2 `pointInFrame` n003
