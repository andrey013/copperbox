{-# OPTIONS -Wall #-}

module Hyperbola where


import Wumpus.Core.Curve
import Wumpus.Core.Geometric ( adjustvk ) 
import Wumpus.Core.Point
import Wumpus.Core.Pointwise
import Wumpus.Core.Polygon
import Wumpus.Core.Transformations

import Wumpus.Drawing.Basic
import Wumpus.Drawing.Plot
import Wumpus.Drawing.SVGColours




demo1 :: IO ()
demo1 = writePicture "hyperbola1.ps" drawing1 where
  drawing1 = hypers <..> curves1 <..> c_hack1 <..> c_hack2 <..> ajtest

  hypers    = displace 150 380 $ 
                    (cat $ map (picCurve . scalePoints 30) (hyperbola (-2) 2 1))
               <..> (withRgbColour blueViolet $ 
                       cat $ map (picCurve . scalePoints 30) (hyperbola (-2) 2 2))
  
  curves1   = displace 150 480 $ withRgbColour coral $ 
                picCurve bcurve <..> (displace 0 40 $ 
                                            (picCurve c1) 
                                       <..> (withRgbColour skyBlue $ picCurve c2))
  bcurve    = Curve (P2 0 0) (P2 5.0 50) (P2 45 50) (P2 50 0)
  (c1,c2)   = subdivide bcurve
 
  c_hack1   = displace 150 580 $ 
                  curveHack [P2 60 30, P2 0 15, P2 90 90, P2 100 0]

  c_hack2   = displace 350 580 $ 
                  curveHack [P2 10 10, P2 40 10, P2 40 40, P2 10 40]  



curveHack :: [DPoint2] -> Picture
curveHack xs = 
         (withRgbColour darkSeaGreen $ picPolygon $ Polygon xs)
    <..> (withRgbColour blueViolet   $ cat $ map picBezier cs)
  where
    cs = smoothw 0.6 xs


ajtest :: Picture
ajtest =  displace 300 500 $ 
         (cat $ map (\(P2 x y) -> displace x y dotTriangle) [p0,p1,p2])
    <..> (withRgbColour firebrick $ 
             cat $ map (\(P2 x y) -> displace (x+20) (y+20) dotDiamond) [pA,p1,pB])
  where
    p0 = P2 0  40 
    p1 = P2 20 30
    p2 = P2 60 10
    (pA,pB) = adjustvk p0 p1 p2 0.6


scalePoints :: Double -> DCurve -> DCurve
scalePoints d = pointwise (uniformScale d)


cubicPoly :: Num a => a -> a -> a -> a -> a -> a
cubicPoly a0 a1 a2 a3 x = a3*(x*x*x) + a2*(x*x) + a1*x + a0


horner :: Num a => a -> [a] -> a
horner t = foldr1 fn where 
  fn i a = a*t + i



{-

-- n^3 + 3n^2 + n - 7

d0 :: Double
d0 = (2^3) + (3*2^2) + 2 - 7 

d1 :: Double
d1 = cubicPoly (-7) 1 3 1  2
 
d2 :: Double
d2 = horner 2 [-7,1,3,1]

-}




