{-# OPTIONS -Wall #-}

module Picture where

import Wumpus.Core
import Wumpus.Doodle.Lines
import Wumpus.Extra
import Wumpus.Extra.PictureLanguage
import Wumpus.Extra.SVGColours


main :: IO ()
main = sequence_ [ demo01, demo02, demo03, demo04 ]


colouredSquare :: (Fractional u, Ord u) => DRGB -> u -> Picture u
colouredSquare c sz = 
  frame $ fill c $ extractPath $ square sz zeroPt 


demo01 :: IO ()
demo01 = do 
    writeEPS_latin1 "./out/picture01.eps" pic1 
    writeSVG_latin1 "./out/picture01.svg" pic1 
  where
    pic1 :: Picture Double
    pic1 = uniformScale 5 $ stackOnto [d1,d2,d3,d4,d5,d6]
                          $ colouredSquare cornsilk 100
    d1,d2,d3,d4,d5,d6 :: Picture Double                      
    d1   = dotX         std_attr $ P2 10 10
    d2   = dotPlus      std_attr $ P2 20 20
    d3   = dotDiamond   std_attr $ P2 40 20 
    d4   = dotDisk      std_attr $ P2 20 30
    d5   = dotSquare    std_attr $ P2 30 20
    d6   = dotCross     std_attr $ P2 40 30

    std_attr = (black, 1.0::Double)

demo02 :: IO ()
demo02 = do 
    writeEPS_latin1 "./out/picture02.eps" pic1 
    writeSVG_latin1 "./out/picture02.svg" pic1 
  where
    pic1 :: Picture Double
    pic1 = stackOntoCenter [poly] bkgd
    bkgd = uniformScale 5 $ backgroundFill cornflowerBlue 
                          $ blankPicture (BBox zeroPt (P2 100 100))
    poly = frame $ fillPolygon () $ rotate45 $ rectangle 40 50 (P2 20 20) 

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

    bkg = (`over` backgroundRect cornflowerBlue 70 70)

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


demo04 :: IO ()
demo04 = do 
    writeEPS_latin1 "./out/picture04.eps" pic1 
    writeSVG_latin1 "./out/picture04.svg" pic1 
  where
    pic1 :: Picture Double
    pic1 = uniformScale 1.25 $ 
             vsepA VLeft 20 (scanlikePic ps1) (map scanlikePic [ps2,ps3,ps4])
    ps1  = ixDownLeftRight 8 6 (fmap (*18))
    ps2  = ixLeftRightDown 8 6 (fmap (*18))
    ps3  = ixLeftRightUp   8 6 (fmap (*18))
    ps4  = ixUpLeftRight   8 6 (fmap (*18))

scanlikePic :: [Point2 Double] -> Picture Double
scanlikePic []     = error "scanlikePic - empty"
scanlikePic (x:xs) = multi $ ls : start : rest 
  where
    ls     = frame $ ostroke () $ vertexPath (x:xs)
    start  = dotDisk (red, 1.0::Double) x
    rest   = map (dotDisk (black, 1.0::Double)) xs