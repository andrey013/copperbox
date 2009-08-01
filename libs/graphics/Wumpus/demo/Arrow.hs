{-# OPTIONS -Wall #-}

module Arrow where

import Wumpus.Core.Curve
import Wumpus.Core.Fun
import Wumpus.Core.Line
import Wumpus.Core.Point
import Wumpus.Core.Radian

import Wumpus.Drawing.Arrow
import Wumpus.Drawing.Basic
import Wumpus.Drawing.Label
import Wumpus.Drawing.X11Colours

import Data.Ratio



demo1 :: IO ()
demo1 = writePicture "arrow1.ps" drawing1 where
  drawing1 = displace 60 380 $ withFont (timesRoman 9) $
               arr1 <..> arrheads1 <..> cm1 
                    <..> c_vees    <..> p_vees
                    <..> subs1     <..> splits1

  arr1      = picArrow $ arrow (P2 0 0) (P2 40 0) 

  arrheads1 = displace 50 0 $ withRgbColour dodgerBlue1 $ cat
                [ arrowhead1 (P2 0 0) 
                             (P2 10 50) 
                             (picPolygon `oo` arrowheadTriangle 10 (pi/10))
                , arrowhead1 (P2 10 0) 
                             (P2 20 50) 
                             (picLines `oo` arrowheadVee 10 (pi/10))
                , arrowhead1 (P2 20 0) 
                             (P2 30 50) 
                             (picLines `oo` arrowheadPerp 5)
                ]

  cm1       = displace 100 0 $ picLines $ arrowCenterMarker $ 
                lineTo (P2 30 0) (P2 40 50)
               

  c_vees    = displace 0 100 $ curvedArrs veeArrowC

  p_vees    = displace 0 160 $ curvedArrs perpArrowC

  subs1     = let (a,b) = subdividet (0.9::Double) $ testCurve (pi/2) in
              displace 0 230 $ 
                       (withRgbColour brown1 $ picCurve a)
                  <..> (withRgbColour chartreuse1 $ picCurve b)

  splits1   = displace 200 230 $
                splitCurve $ testCurve (pi/2)


curvedArrs :: (DCurve -> (DCurve,[DLineSegment2])) -> Picture
curvedArrs proc = withRgbColour coral1 $ 
                     cs <..> (curveArr $ proc straightBezier)
                        <..> dotty
  where
    cs    = hcatSep 10 [ fn (pi/2), fn (pi/3), fn (pi/4) ]
    fn    = curveArr . proc . testCurve  
    dotty = withRgbColour skyBlue1 $ d1 <..> d2
    d1    = (movePt dotPlus) $ cubic (testCurve (pi/4)) 0.9
    d2    = (movePt dotPlus) $ cubic (testCurve (pi/4)) 0.5
    movePt pic = \(P2 x y) -> displace x y pic

                    
arrowhead1 :: DPoint2 -> DPoint2 -> (Radian -> DPoint2 -> Picture) -> Picture
arrowhead1 start_pt end_pt arrHead = 
    (picLine  arrline) <..> (arrHead theta end_pt)
  where
    theta    :: Radian
    arrline  = lineTo start_pt end_pt
    theta    = langle arrline


curveArr :: (DCurve, [DLineSegment2]) -> Picture
curveArr (c,xs) = 
         (withRgbColour coral1 $ picCurve c)
    <..> (withRgbColour aquamarine1 $ picLines xs)
    <..> (withRgbColour darkSlateGray4 $ 
            picLabel (show (endTangent c)) 40 20)

testCurve :: Radian -> DCurve
testCurve = bezierArc 50 0


straightBezier :: DCurve
straightBezier = Curve (P2 0 0) (P2 20 20) (P2 40 40) (P2 60 60)

splitCurve :: DCurve -> Picture
splitCurve crv =  
         (withRgbColour darkGoldenrod1 $ picBezier a)
    <..> (withRgbColour cyan4 $ picBezier b)
  where
    cl    = floor $ gravesenLength 0.1 crv
    t     = (cl-10) % (cl::Integer)       -- go back the length of the arrow head
    (a,b) = subdividet t crv
       

{-
dummy1 = gravesenLength 0.01 $ testCurve (pi/2)
dummy2 :: Double
dummy2 = distance (P2 0 50) (P2 50 (0::Double))


dummy3 = r2d $ interior 0 (negate $ d2r 45)
dummy4 = r2d $ interior 0 (d2r 170)
-}


