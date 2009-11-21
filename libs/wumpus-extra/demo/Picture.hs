{-# OPTIONS -Wall #-}

module Picture where

import Wumpus.Core
import Wumpus.Extra
import Wumpus.Extra.SVGColours
import Wumpus.Geometry

main :: IO ()
main = sequence_ [ demo01, demo02, demo03 ]


colouredSquare :: (Fractional u, Ord u) => DRGB -> u -> Picture u
colouredSquare c sz = 
  frame $ fill c $ extractPath $ square sz zeroPt 


demo01 :: IO ()
demo01 = do 
    writeEPS_latin1 "./out/picture01.eps" pic1 
    writeSVG_latin1 "./out/picture01.svg" pic1 
  where
    pic1 :: Picture Double
    pic1 = uniformScale 5 $ stackOnto [d1,d2,d3,d4,d5,d6 1.0,d7]
                          $ colouredSquare cornsilk 100
    d1   = dotX         black   $ P2 10 10
    d2   = dotPlus      black   $ P2 20 20
    d3   = dotDiamond   black   $ P2 40 20 
    d4   = dotDisk      black   $ P2 20 30
    d5   = dotSquare    black   $ P2 30 20
    d6   = squareDot    black   $ P2 30 30 
    d7   = dotCross     black   $ P2 40 30


demo02 :: IO ()
demo02 = do 
    writeEPS_latin1 "./out/picture02.eps" pic1 
    writeSVG_latin1 "./out/picture02.svg" pic1 
  where
    pic1 :: Picture Double
    pic1 = uniformScale 5 $ backgroundFill cornflowerBlue 
                          $ blankPicture (BBox zeroPt (P2 100 100))


demo03 :: IO ()
demo03 = do 
    writeEPS_latin1 "./out/picture03.eps" pic1 
    writeSVG_latin1 "./out/picture03.svg" pic1 
  where
    pic1 :: Picture Double
    pic1 = hsepA HTop  20 col1 [col2]
    col1 = vsepA VLeft 20 p_lines     [p_strip, p_loop, p_trias]
    col2 = vsepA VLeft 20 p_tri_strip [p_tri_fan, p_quads, p_quad_strip]

    pts  = [P2 25 32, P2 40 56, P2 42 36, P2 64 27, P2 27 23, P2 60 10,
            P2 56 30, P2 12 14, P2 28 42 ]

    pts2 = [P2 5 40, P2 7 6, P2 20 38, P2 25 10, P2 41 42, P2 50 6 ]
    
    pts3 = [P2 30 40, P2 55 24, P2 50 42, P2 50 62, P2 34 62 ]
    
    pts4 = [P2 10 10, P2 26 10, P2 20 30, P2 15 28,
            P2 40 10, P2 50 10, P2 50 64, P2 42 60] 

    pts5 = mix [P2 10 40, P2 24 40, P2 34 42, P2 50 38]
               [P2 8  10, P2 26 8,  P2 36 12, P2 50 10]

    bkg = (`over` background cornflowerBlue 70 70)

    p_lines         = bkg $ linesUnconnected () pts
    p_strip         = bkg $ lineStrip () pts
    p_loop          = bkg $ lineLoop () pts
    p_trias         = bkg $ triangles () pts
    p_tri_strip     = bkg $ triangleStrip () pts2
    p_tri_fan       = bkg $ triangleFan () pts3
    p_quads         = bkg $ quads () pts4
    p_quad_strip    = bkg $ quadStrip () pts5

mix :: [a] -> [a] -> [a]
mix (x:xs) (y:ys) = x:y:mix xs ys
mix _      _      = []