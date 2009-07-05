{-# OPTIONS -Wall #-}

module Arrow where

import Wumpus.Core.Curve
import Wumpus.Core.Line
import Wumpus.Core.Point
import Wumpus.Core.Radian
import Wumpus.Drawing.Arrow
import Wumpus.Drawing.Basic
import Wumpus.Drawing.PostScript
import Wumpus.Drawing.X11Colours



{-
import Data.AffineSpace

dummy1 = gravesenLength 0.01 $ testCurve (pi/2)
dummy2 :: Double
dummy2 = distance (P2 0 50) (P2 50 (0::Double))
-}


demo1 :: IO ()
demo1 = writePS "arrow1.ps" $ runWumpus st0 $ drawing1 where
  drawing1 = do { ps_translate 60 480 
                ; drawArrow $ arrow (P2 0 0) (P2 40 0) 
                ; setRgbColour dodgerBlue1 
                ; ps_translate 100 0
                ; arrowhead1 (P2 0 0)  (P2 10 50) (arrowheadTriangle 10 (pi/10)) drawPolygon
                ; arrowhead1 (P2 10 0) (P2 20 50) (arrowheadVee 10 (pi/10)) 
                                                  (mapM_ drawLine)
                ; arrowhead1 (P2 20 0) (P2 30 50) (arrowheadPerp 5) (mapM_ drawLine)
                ; mapM_ drawLine $ arrowCenterMarker $ lineTo (P2 30 0) (P2 40 50)

                -- curved arrows
                ; setRgbColour coral1
                ; ps_translate 100 0
                ; drawCurveArr $ veeArrowC $ testCurve (pi/2)
                ; ps_translate 10 0
                ; drawCurveArr $ veeArrowC $ testCurve (pi/3)
                ; ps_translate 10 0
                ; drawCurveArr $ veeArrowC $ testCurve (pi/4)
                ; setRgbColour skyBlue1
                ; drawLineBag $ dotPlus $ cubic (testCurve (pi/4)) 0.9
                ; drawLineBag $ dotPlus $ cubic (testCurve (pi/4)) 0.5
                ; ps_translate 40 0
                ; drawCurveArr $ veeArrowC $ straightBezier
                
                -- subdividet
                ; ps_translate (-120) 60
                ; let (a,b) = subdividet 0.9 $ testCurve (pi/2)
                ; setRgbColour brown1
                ; drawCurve a
                ; setRgbColour chartreuse1
                ; drawCurve b 
                }
                             
arrowhead1 :: DPoint2 -> DPoint2 -> (DRadian -> DPoint2 -> a) 
                -> (a -> WumpusM()) -> WumpusM ()
arrowhead1 start_pt end_pt arrHead drawFun = 
    do { drawLine $ arrline
       ; drawFun $ arrHead theta end_pt
       }
  where
    theta    :: DRadian
    arrline  = lineTo start_pt end_pt
    theta    = langle arrline


drawCurveArr :: (DCurve, [DLineSegment2]) -> WumpusM ()
drawCurveArr (c,xs) = do
  drawCurve c
  mapM_ drawLine xs

testCurve :: DRadian -> DCurve
testCurve = bezierArc 50 0


straightBezier :: DCurve
straightBezier = Curve (P2 0 0) (P2 20 20) (P2 40 40) (P2 60 60)