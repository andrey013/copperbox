

module Hyperbola where


import Wumpus.Core.Curve
import Wumpus.Core.Point
import qualified Wumpus.Core.Transformations as W
import Wumpus.Core.Wumpus

import Wumpus.Drawing.Basic
import Wumpus.Drawing.SVGColours

f :: Floating a => a -> a
f x = sqrt (1 + x ^^ 2)

f' :: Floating a => a -> a
f' x = x / sqrt (1 + x ^^ 2)


f2 :: Floating a => a -> (a,a)
f2 x = (y,x/y) where y = sqrt (1+x^^2)

hyperbola :: Double -> Double -> Int -> [DCurve]
hyperbola x0 x1 n = cs
  where
    (y0,s) = f2 x0
    h = (x1-x0)/ fromIntegral  n
    cs = step n (x0,y0,s) []
    step 0 _   acc = reverse acc 
    step i alg acc = let (alg',a) = makeCurve alg h in step (i-1) alg' (a:acc)



type Alg = (Double,Double,Double)


makeCurve :: Alg -> Double -> (Alg,DCurve)
makeCurve (x0,y0,s) h = 
    ((x3,y3,s'), Curve (P2 x0 y0) (P2 x1 y1) (P2 x2 y2)  (P2 x3 y3) )
  where
    x1 = x0 + h/3
    y1 = y0 + s * h/3
    x3 = x0 + h
    (y3,s') = f2 x3
    x2 = x3 - h/3
    y2 = y3 - s'*(h/3)


demo2 = hyperbola (-2) 2 4

demo1 :: IO ()
demo1 = writePS "hyperbola1.ps" $ runWumpus st0 $ drawing1 where
  drawing1 = do { translate 150 380 
                ; let curves1 = hyperbola (-2) 2 1
                ; mapM_ drawCurve $ map (scalePoints 30) curves1
                ; setRgbColour blueViolet
                ; let curves2 = hyperbola (-2) 2 2
                ; mapM_ drawCurve $ map (scalePoints 30) curves2
                ------
                ; translate 0 100
                ; setRgbColour coral
                ; let bcurve = Curve (P2 0 0) (P2 5.0 50) (P2 45 50) (P2 50 0)
                ; drawCurve bcurve
                ; let (c1,c2) = subdivide bcurve
                ; drawCurve $ ctranslate 0 40 c1
                ; setRgbColour skyBlue
                ; drawCurve $ ctranslate 0 40 c2
                ; curveHack
                }


curveHack :: WumpusM ()
curveHack = do 
    translate 0 100
    setRgbColour darkSeaGreen
    drawPolygon $ Polygon xs
    setRgbColour blueViolet
    mapM_ drawBezier cs
  where
    xs = [P2 70 30, P2 0 20, P2 90 90, P2 100 0]
    cs = smoothw xs

scalePoints :: Double -> DCurve -> DCurve
scalePoints d = fmap (d*)

-- temp hack
ctranslate :: Double -> Double -> DCurve -> DCurve
ctranslate x y (Curve p0 p1 p2 p3) = 
  Curve (W.translate x y p0) (W.translate x y p1) (W.translate x y p2) (W.translate x y p3) 

cubicPoly :: Num a => a -> a -> a -> a -> a -> a
cubicPoly a0 a1 a2 a3 x = a3*(x*x*x) + a2*(x*x) + a1*x + a0


horner :: Num a => a -> [a] -> a
horner t = foldr1 fn where 
  fn i a = a*t + i


-- n^3 + 3n^2 + n - 7

d0 = (2^3) + (3*2^2) + 2 - 7 

d1 = cubicPoly (-7) 1 3 1  2
 

d2 = horner 2 [-7,1,3,1]


