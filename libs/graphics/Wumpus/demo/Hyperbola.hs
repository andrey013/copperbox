

module Hyperbola where


import Wumpus.Core.Curve
import Wumpus.Core.Geometric ( adjustvk ) 
import Wumpus.Core.Point
import Wumpus.Core.Pointwise
import Wumpus.Core.Polygon
import Wumpus.Core.Transformations

import Wumpus.Drawing.Basic
import Wumpus.Drawing.Plot
import Wumpus.Drawing.PostScript
import Wumpus.Drawing.SVGColours



demo2 = hyperbola (-2) 2 4

demo1 :: IO ()
demo1 = writePS "hyperbola1.ps" $ runWumpus env0 $ drawing1 where
  drawing1 = do { ps_translate 150 380 
                ; let curves1 = hyperbola (-2) 2 1
                ; mapM_ drawCurve $ map (scalePoints 30) curves1
                ; withRgbColour blueViolet $ do
                    let curves2 = hyperbola (-2) 2 2
                    mapM_ drawCurve $ map (scalePoints 30) curves2
                ------
                ; ps_translate 0 100
                ; withRgbColour coral $ do
                    let bcurve = Curve (P2 0 0) (P2 5.0 50) (P2 45 50) (P2 50 0)
                    drawCurve bcurve
                    let (c1,c2) = subdivide bcurve
                    drawCurve $ ctranslate 0 40 c1
                    withRgbColour skyBlue $ drawCurve $ ctranslate 0 40 c2
                ; ps_translate 0 100
--                ; ps_setdash [2,1] 0
                ; curveHack [P2 60 30, P2 0 15, P2 90 90, P2 100 0]
                ; ps_translate 200 (-100)
                ; curveHack [P2 10 10, P2 40 10, P2 40 40, P2 10 40] 
                ; ps_translate 0 (100)
--                ; ps_setdash [] 0
--                ; ajtest
                }


curveHack :: [DPoint2] -> WumpusM ()
curveHack xs = do 
    withRgbColour darkSeaGreen $ drawPolygon $ Polygon xs
    withRgbColour blueViolet   $  mapM_ drawBezier cs
  where
    cs = smoothw 0.6 xs

{-
ajtest :: WumpusM ()
ajtest = do 
    mapM_ (drawPolygon . dotTriangle) [p0,p1,p2]
    withRgbColour firebrick $ 
        mapM_ (drawPolygon . dotDiamond . pointwise (translate 20 20)) [pA,p1,pB]
  where
    p0 = P2 0  40 
    p1 = P2 20 30
    p2 = P2 60 10
    (pA,pB) = adjustvk p0 p1 p2 0.6
-}

scalePoints :: Double -> DCurve -> DCurve
scalePoints d = pointwise (uniformScale d)


-- temp hack
ctranslate :: Double -> Double -> DCurve -> DCurve
ctranslate x y (Curve p0 p1 p2 p3) = 
  Curve (translate x y p0) (translate x y p1) (translate x y p2) (translate x y p3) 

cubicPoly :: Num a => a -> a -> a -> a -> a -> a
cubicPoly a0 a1 a2 a3 x = a3*(x*x*x) + a2*(x*x) + a1*x + a0


horner :: Num a => a -> [a] -> a
horner t = foldr1 fn where 
  fn i a = a*t + i


-- n^3 + 3n^2 + n - 7

d0 = (2^3) + (3*2^2) + 2 - 7 

d1 = cubicPoly (-7) 1 3 1  2
 

d2 = horner 2 [-7,1,3,1]


