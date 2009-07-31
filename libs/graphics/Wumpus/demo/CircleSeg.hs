


module CircleSeg where

import Wumpus.Core.Curve
import Wumpus.Core.Fun
import Wumpus.Core.Instances
import Wumpus.Core.Line
import Wumpus.Core.Point
import Wumpus.Core.Pointwise
import Wumpus.Core.Transformations
import Wumpus.Core.Vector

import Wumpus.Drawing.Basic
import Wumpus.Drawing.PostScript
import Wumpus.Drawing.X11Colours

import Data.AffineSpace


demo1 :: IO ()
demo1 = writePS "circleseg1.ps" $ runWumpus env0 $ drawing1 where
  drawing1 = do { ps_translate 40 680 
                ; localRgbColour tomato4 $ do
                      -- drawPolygon $ dotDiamond zeroPt
                      drawLine $ hline 70 zeroPt
                      drawLine $ pointwise (rotateAbout (pi/4) zeroPt) $ hline 70 zeroPt
                ; localRgbColour steelBlue1 $ do 
                      drawBezier $ pointwise (uniformScale 60) $ circleSegment (pi/4)
                ---
                      drawBezier $ pointwise (translate 100 0) $ bezierArc 20 0 (pi/2)
                
                ; ps_translate 0 (-100)
                ; mapM_ dpo $ pointwise (uniformScale 30) $ plotSine 
                ---
                ; ps_translate 0 (-50)
                ; drawBezier $ pointwise (uniformScale 30) $ cwd sin cos 0 1
                ; drawBezier $ pointwise (uniformScale 30) $ cwd sin cos 1 2
                ; drawBezier $ pointwise (uniformScale 30) $ cwd sin cos 2 3
                ; drawBezier $ pointwise (uniformScale 30) $ cwd sin cos 3 4
                ; drawBezier $ pointwise (uniformScale 30) $ cwd sin cos 4 5
                --
                ; ps_translate 0 (-50)
                ; mapM_ (drawCurve . pointwise (uniformScale 10)) 
                                     $ sinePath 20
                ; ps_translate 0 (-50)
                ; mapM_ (drawCurve . pointwise (scale 5 2.5)) 
                                     $ cosPath 50
                --
                ; ps_translate 0 (-50)
                ; drawBezier $ tildeCurve 50 (P2 0  0)
                ; drawCurve  $ tildeCurve 50 (P2 60 0)
                }

dpo :: DVec2 -> WumpusM ()
-- dpo = drawPolygon . dotSquare . (zeroPt .+^)
dpo _  = return ()


plotSine :: [DVec2]
plotSine = [V2 t (sin t) | t <- divisions 50 10 ]


-----------


-- curve-with-derivative
-- plot f when we know the derivative f'

cwd :: Fractional a => (a -> a) -> (a -> a) -> a -> a -> Curve a
cwd f f' c d  = Curve p0 p1 p2 p3 where
  h  = d - c
  p0 = P2 c         (f c)
  p1 = P2 (c+h/3)   ((f c) + (h/3)*(f' c))
  p2 = P2 (d - h/3) ((f d) - (h/3)*(f' d))
  p3 = P2 d         (f d)

sinePath :: (Fractional a, Floating a, Ord a) => Int -> [Curve a]
sinePath n = intermap (cwd sin cos) $ steps 1 (fromIntegral n)
    
cosPath :: (Fractional a, Floating a, Ord a) => Int -> [Curve a]
cosPath n = intermap (cwd cos (negate . sin)) $ steps 1 (fromIntegral n)



{-
cws :: Fractional a => (a -> a) -> a -> a -> Curve a 
cws f x h = Curve p0 p1 p2 p3 where
  p0 = P2 x           (f x)
  p1 = P2 (x+h/3)     ((f x) + (h/3)*(g x))
  p2 = P2 (x+(2*h)/3) ((f d) - (h/3)*(g $ x+h))
  p3 = P2 d           (f $ x+h)
  g  = 
-}



simpsons :: Fractional a => (a -> a) -> a -> a -> a
simpsons f a b = ((b-a)/6) * ((f a) + 4*(f $ 0.5*(a+b)) + (f b))