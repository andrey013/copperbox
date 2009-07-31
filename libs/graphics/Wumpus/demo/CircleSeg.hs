{-# OPTIONS -Wall #-}


module CircleSeg where

import Wumpus.Core.Curve
import Wumpus.Core.Fun
-- import Wumpus.Core.Instances
import Wumpus.Core.Line
import Wumpus.Core.Point
import Wumpus.Core.Pointwise
import Wumpus.Core.Transformations
import Wumpus.Core.Vector

import Wumpus.Drawing.Basic
-- import Wumpus.Drawing.PostScript
import Wumpus.Drawing.X11Colours

-- import Data.AffineSpace


demo1 :: IO ()
demo1 = writePicture "circleseg1.ps" drawing1 where
  drawing1 = displace 40 680 (pic1 <..> pic2 <..> sinePic <..> beziers
                                   <..> sine1 <..> cos1 <..> tb)

  pic1 = withRgbColour tomato4 $ 
                dotDiamond
           <..> (picLine $ hline 70 zeroPt)
           <..> (picLine $ pointwise (rotateAbout (pi/4) zeroPt) $ hline 70 zeroPt)

  pic2 = withRgbColour steelBlue1 $
                (picBezier $ pointwise (uniformScale 60) $ circleSegment (pi/4))
           <..> (picBezier $ pointwise (translate 100 0) $ bezierArc 20 0 (pi/2))


  sinePic = displace 0 (-100) $ dpo $ pointwise (uniformScale 30) $ plotSine
  
  beziers = displace 0 (-150) $ cat $ zipWith fn [0..4] [1..5] where
      fn a b = picBezier $ pointwise (uniformScale 30) $ cwd sin cos a b

  sine1   = displace 0 (-200) $ cat $ 
                map (picCurve . pointwise (uniformScale 10) ) $ sinePath 20

  cos1    = displace 0 (-250) $ cat $ 
                map (picCurve . pointwise (uniformScale 10) ) $ cosPath 20

  tb      = displace 0 (-300) $ picCurve (tildeCurve 50 (P2 0  0)) <++> 
                                picBezier (tildeCurve 50 (P2 60 0))



dpo :: [DVec2] -> Picture
dpo = cat . map (vdisplace `flip` dotSquare)


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


