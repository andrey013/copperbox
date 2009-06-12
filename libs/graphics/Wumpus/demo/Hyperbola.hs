

module Hyperbola where


import Wumpus.Core.Curve
import Wumpus.Core.Point
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
                }

scalePoints :: Double -> DCurve -> DCurve
scalePoints d = fmap (d*)