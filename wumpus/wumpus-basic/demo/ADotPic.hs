{-# OPTIONS -Wall #-}

module ADotPic where


import Wumpus.Basic.Anchors
import Wumpus.Basic.Dots
import Wumpus.Basic.Graphic
import Wumpus.Basic.Colour.SVGColours

import Wumpus.Core                      -- package: wumpus-core

import System.Directory

main :: IO ()
main = do 
    createDirectoryIfMissing True "./out/"
    demo01

pt2 :: Point2 Double
pt2 = P2 100 10


demo01 :: IO ()
demo01 = do 
    writeEPS_latin1 "./out/anchor_dots01.eps" pic1
    writeSVG_latin1 "./out/anchor_dots01.svg" pic1


std_attr :: DrawingContext
std_attr = standardContext 24


pic1 :: DPicture
pic1 = liftToPictureU $ execDrawing std_attr $ mf 


mf :: (Floating u, FromPtSize u) => Drawing u ()
mf = do 
    a <- drawi $ dotCircle `ati` zeroPt
    b <- drawi $ modifyImage fn $ dotCircle `ati` (P2 60 60)
    _ <- drawi $ dotCircle `ati` (P2 120 0)
    let p1 = radialAnchor (pi/4)    a
    let p2 = radialAnchor (5* pi/4) b
    xdraw link1 $ straightLineBetween p1 p2
  where
    link1 = xlinkhref "http://www.haskell.org"


fn :: Num u => GraphicF u
fn = annotate (localize (fillColour red) $ filledDisk 2 `at` (P2 60 60)) 


annotate :: Graphic u -> GraphicF u
annotate a = (`oplus` a)
