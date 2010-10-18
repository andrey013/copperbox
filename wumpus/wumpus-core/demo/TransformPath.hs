{-# OPTIONS -Wall #-}

module TransformPath where

import Wumpus.Core
import Wumpus.Core.Colour

import Data.AffineSpace                         -- package: vector-space

import System.Directory


main :: IO ()
main = do 
    createDirectoryIfMissing True "./out/"
    writeEPS_latin1 "./out/transform_path01.eps" pic1
    writeSVG_latin1 "./out/transform_path01.svg" pic1
    writeEPS_latin1 "./out/transform_path02.eps" pic2
    writeSVG_latin1 "./out/transform_path02.svg" pic2
    writeEPS_latin1 "./out/transform_path03.eps" pic3
    writeSVG_latin1 "./out/transform_path03.svg" pic3
    writeEPS_latin1 "./out/transform_path04.eps" pic4
    writeSVG_latin1 "./out/transform_path04.svg" pic4
    writeEPS_latin1 "./out/transform_path05.eps" pic5
    writeSVG_latin1 "./out/transform_path05.svg" pic5



pic1 :: Picture Double
pic1 = pth `picOver` ch `picOver` xy_frame "no transform"
  where
     pth  = mkBlackPath id pt
     ch   = zcrosshair pt
     pt   = P2 70 10


pic2 :: Picture Double
pic2 = pth `picOver` ch `picOver` xy_frame "rotate 30deg"
  where
     pth  = mkBlackPath (rotate ang) pt
     ch   = rotate ang $ zcrosshair pt
     pt   = P2 70 10
     ang  = 0.25*pi

pic3 :: Picture Double
pic3 = pth `picOver` ch `picOver` xy_frame "rotateAbout (60,0) 30deg"
  where
     pth  = mkBlackPath (rotateAbout ang pto) pt
     ch   = rotateAbout ang pto $ zcrosshair pt
     pt   = P2 70 10
     pto  = P2 60 0
     ang  = 0.25*pi


pic4 :: Picture Double
pic4 = pth `picOver` ch `picOver` xy_frame "scale 1 2"
  where
     pth  = mkBlackPath (scale 1 2) pt
     ch   = scale 1 2 $ zcrosshair pt
     pt   = P2 70 10

pic5 :: Picture Double
pic5 = pth `picOver` ch `picOver` xy_frame "translate -70 -10"
  where
     pth  = mkBlackPath (translate (-70) (-10)) pt
     ch   = translate (-70) (-10) $ zcrosshair pt
     pt   = P2 70 10


mkBlackPath :: (Real u, Floating u, FromPtSize u) 
             => (Primitive u -> Primitive u) 
             -> Point2 u -> Picture u
mkBlackPath trafo bl = 
    frame [ trafo $ ostroke black custom_stroke_attr $ path bl ps]
  where
    ps = [lineTo p1, lineTo p2, lineTo p3]
    p1 = bl .+^ vec 25 12
    p2 = p1 .+^ vec 6 (-12)
    p3 = p2 .+^ vec 25 12



custom_stroke_attr :: StrokeAttr
custom_stroke_attr = default_stroke_attr { line_width = 2 }

zcrosshair :: (Real u, Floating u, FromPtSize u) => Point2 u -> Picture u
zcrosshair = crosshair 56 12 

crosshair :: (Real u, Floating u, FromPtSize u) 
          => u -> u -> Point2 u -> Picture u
crosshair w h bl = frame [ostroke burlywood default_stroke_attr $ path bl ps]
  where
    ps    = [ lineTo tr, lineTo br, lineTo tl, lineTo bl ]
    tl    = bl .+^ vvec h
    tr    = bl .+^ vec  w h
    br    = bl .+^ hvec w
    
burlywood :: RGBi
burlywood = RGBi 222 184 135

xy_frame :: (Real u, Floating u, FromPtSize u) => String -> Picture u
xy_frame ss = 
    frame [ mkline (P2 (-4) 0) (P2 150 0)
          , mkline (P2 0 (-4)) (P2 0 150) 
          , textlabel black wumpus_default_font ss (P2 0 (-20))
          ]

  where
    mkline p1 p2 = ostroke black default_stroke_attr $ path p1 [lineTo p2]