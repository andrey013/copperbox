{-# OPTIONS -Wall #-}

module Arrowheads where


import Wumpus.Basic.Arrows
import Wumpus.Basic.Chains
import Wumpus.Basic.Colour.SVGColours
import Wumpus.Basic.Graphic
import Wumpus.Basic.Paths hiding ( length )

import Wumpus.Core                              -- package: wumpus-core

import Data.AffineSpace                         -- package: vector-space

import Control.Monad
import System.Directory

main :: IO ()
main = do 
    createDirectoryIfMissing True "./out/"
    demo01

pt2 :: Point2 Double
pt2 = P2 100 10


demo01 :: IO ()
demo01 = do 
    writeEPS_latin1 "./out/arrowheads01.eps" pic1
    writeSVG_latin1 "./out/arrowheads01.svg" pic1

pic1 :: Picture Double
pic1 = liftToPictureU $ execDrawing std_ctx $ tableGraphic $ arrtable

arrtable :: [(Arrowhead Double, Arrowhead Double)]
arrtable = 
    [ (tri90,       tri90)
    , (tri60,       tri60)
    , (tri45,       tri45)
    , (otri90,      otri90)
    , (otri60,      otri60)
    , (otri45,      otri45)
    , (revtri90,    revtri90)
    , (revtri60,    revtri60)
    , (revtri45,    revtri45)
    , (orevtri90,   orevtri90)
    , (orevtri60,   orevtri60)
    , (orevtri45,   orevtri45)
    , (barb90,      barb90)
    , (barb60,      barb60)
    , (barb45,      barb45)
    , (revbarb90,   revbarb90)
    , (revbarb60,   revbarb60)
    , (revbarb45,   revbarb45)
    , (perp,        perp)
    , (bracket,     bracket)
    , (diskTip,     diskTip)
    , (odiskTip,    odiskTip)
    , (squareTip,   squareTip)
    , (osquareTip,  osquareTip)
    , (diamondTip,  diamondTip)
    , (odiamondTip, odiamondTip)
    ]

tableGraphic :: (Real u, Floating u, FromPtSize u) 
             => [(Arrowhead u, Arrowhead u)] -> Drawing u ()
tableGraphic tips = zipWithM_ makeArrowDrawing tips ps
  where
    ps = unchain (coordinateScalingContext 120 24) $ tableDown 20 4


 
std_ctx :: DrawingContext
std_ctx = fillColour peru $ standardContext 18



makeArrowDrawing :: (Real u, Floating u, FromPtSize u) 
                 => (Arrowhead u, Arrowhead u) -> Point2 u -> Drawing u ()
makeArrowDrawing (arrl,arrr) p0 = 
    drawi_ $ strokeConnector (leftrightArrow connLine arrl arrr) p0 p1
  where
    p1 = p0 .+^ hvec 100
  

