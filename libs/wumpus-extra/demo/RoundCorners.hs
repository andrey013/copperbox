{-# OPTIONS -Wall #-}
{-# LANGUAGE FlexibleContexts           #-}


module RoundCorners where

import Wumpus.Core
import Wumpus.Extra
import Wumpus.Extra.SVGColours

import Data.AffineSpace
import Data.VectorSpace

import Data.Function ( on) 

main :: IO ()
main = sequence_ [ demo01 ]


test01 :: Radian
test01 = r2d $ vangle (hvec 10) (vvec (10::Double))

pOgin :: DPoint2
pOgin = P2 0 0

p2 :: DPoint2
p2 = P2 100 100

test02 = direction $ p2 .-. pOgin
test03 = direction $ pvec pOgin p2
test04 = direction $ pvec p2 pOgin



test_a01 = r2d $ langle pOgin (P2 100    0)        -- 0
test_a02 = r2d $ langle pOgin (P2 100    100)      -- 45
test_a03 = r2d $ langle pOgin (P2 0      100)      -- 90
test_a04 = r2d $ langle pOgin (P2 (-100) 100)      -- 135
test_a05 = r2d $ langle pOgin (P2 (-100) 0)        -- 180  
test_a06 = r2d $ langle pOgin (P2 (-100) (-100))   -- 225
test_a07 = r2d $ langle pOgin (P2 0      (-100))   -- 270
test_a08 = r2d $ langle pOgin (P2 100    (-100))   -- 315
test_a09 = r2d $ langle pOgin (P2 100    0)        -- 0

testANG = [ test_a01, test_a02, test_a03, test_a04, test_a05 
          , test_a06, test_a07, test_a08, test_a09 ]


roundCorner :: (DPoint2, DPoint2,DPoint2) -> Double -> DPath
roundCorner (p0,p1,p2) d = path p0 [lineTo p11, curveTo cp1 cp2 p12, lineTo p2] 
  where
    p11   = p1 .+^ (avec (langle p1 p0) d)
    p12   = p1 .+^ (avec (langle p1 p2) d)

    theta = circularModulo $ vangle (p0 .-. p11) (p2 .-. p12)

    d'    = rescale (0,pi) (0,d) (realToFrac theta)
    
    cp1   = p11 .+^ (avec (langle p0 p1) d')
    cp2   = p12 .+^ (avec (langle p2 p1) d')



demo01 :: IO ()
demo01 = do 
    writeEPS_latin1 "./out/rounded.eps" pic1 
    writeSVG_latin1 "./out/rounded.svg" pic1 
  where
    pic1 :: Picture Double
    pic1 = frame $ 
             ostroke std_attr $ roundCorner (P2 0 0, P2 0 100, P2 100 100) 25

    std_attr = (black, LineWidth 1.0)
