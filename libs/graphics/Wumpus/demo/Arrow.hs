{-# OPTIONS -Wall #-}

module Arrow where

import Wumpus.Core.Curve
import Wumpus.Core.Geometric
import Wumpus.Core.Line
import Wumpus.Core.Point
import Wumpus.Core.Radian

import Wumpus.Drawing.Arrow
import Wumpus.Drawing.Basic
import Wumpus.Drawing.Label
import Wumpus.Drawing.PostScript
import Wumpus.Drawing.X11Colours

import Data.Ratio

-- import Data.AffineSpace

{-
dummy1 = gravesenLength 0.01 $ testCurve (pi/2)
dummy2 :: Double
dummy2 = distance (P2 0 50) (P2 50 (0::Double))
-}

dummy3 = r2d $ interior 0 (negate $ d2r 45)
dummy4 = r2d $ interior 0 (d2r 170)

demo1 :: IO ()
demo1 = writePS "arrow1.ps" $ runWumpus st0 $ drawing1 where
  drawing1 = do { setupFont "Times-Roman" 9
                ; ps_translate 60 480 
                ; drawArrow $ arrow (P2 0 0) (P2 40 0) 
                ; setRgbColour dodgerBlue1 
                ; ps_translate 100 0
                ; arrowhead1 (P2 0 0)  (P2 10 50) (arrowheadTriangle 10 (pi/10)) drawPolygon
                ; arrowhead1 (P2 10 0) (P2 20 50) (arrowheadVee 10 (pi/10)) 
                                                  (mapM_ drawLine)
                ; arrowhead1 (P2 20 0) (P2 30 50) (arrowheadPerp 5) (mapM_ drawLine)
                ; mapM_ drawLine $ arrowCenterMarker $ lineTo (P2 30 0) (P2 40 50)

                -- curved arrows
                ; ps_translate 100 0
                ; curvedArrs veeArrowC
                
                ; ps_translate (-60) 60
                ; curvedArrs perpArrowC
                                
                -- subdividet
                ; ps_translate (-60) 60
                ; let (a,b) = subdividet 0.9 $ testCurve (pi/2)
                ; setRgbColour brown1
                ; drawCurve a
                ; setRgbColour chartreuse1
                ; drawCurve b 
                -- 
                ; ps_translate 0 100
                ; splitCurve $ testCurve (pi/2)
                }

curvedArrs :: (DCurve -> (DCurve,[DLineSegment2])) -> WumpusM ()
curvedArrs proc = 
  do { setRgbColour coral1
     ; drawCurveArr $ proc $ testCurve (pi/2)
     ; ps_translate 10 0
     ; drawCurveArr $ proc $ testCurve (pi/3)
     ; ps_translate 10 0
     ; drawCurveArr $ proc $ testCurve (pi/4)
     ; setRgbColour skyBlue1
--     ; drawLineBag $ dotPlus $ cubic (testCurve (pi/4)) 0.9
--     ; drawLineBag $ dotPlus $ cubic (testCurve (pi/4)) 0.5
     ; ps_translate 50 0
     ; drawCurveArr $ proc $ straightBezier
     }
                             
arrowhead1 :: DPoint2 -> DPoint2 -> (Radian -> DPoint2 -> a) 
                -> (a -> WumpusM()) -> WumpusM ()
arrowhead1 start_pt end_pt arrHead drawFun = 
    do { drawLine $ arrline
       ; drawFun $ arrHead theta end_pt
       }
  where
    theta    :: Radian
    arrline  = lineTo start_pt end_pt
    theta    = langle arrline


drawCurveArr :: (DCurve, [DLineSegment2]) -> WumpusM ()
drawCurveArr (c,xs) = do
  setRgbColour coral1
  drawCurve c
  setRgbColour aquamarine1
  mapM_ drawLine xs
  let et = endTangent c
  setRgbColour darkSlateGray4
  drawLabel $ label (show et) (endPoint c)


testCurve :: Radian -> DCurve
testCurve = bezierArc 50 0


straightBezier :: DCurve
straightBezier = Curve (P2 0 0) (P2 20 20) (P2 40 40) (P2 60 60)

splitCurve :: DCurve -> WumpusM ()
splitCurve crv = do 
    { setRgbColour darkGoldenrod1
    ; drawBezier a
    ; setRgbColour cyan4
    ; drawBezier b  
    }
  where
    cl    = floor $ gravesenLength 0.1 crv
    t     = (cl-10) % cl       -- go back the length of the arrow head
    (a,b) = subdividet t crv
       


