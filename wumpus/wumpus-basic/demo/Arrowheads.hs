{-# OPTIONS -Wall #-}

module Arrowheads where


import Wumpus.Basic.Kernel
import Wumpus.Drawing.Arrows
import Wumpus.Drawing.Chains
import Wumpus.Drawing.Colour.SVGColours
import Wumpus.Drawing.Paths hiding ( length )

import Wumpus.Core                              -- package: wumpus-core

import Data.AffineSpace                         -- package: vector-space

import Control.Monad
import System.Directory

main :: IO ()
main = do 
    createDirectoryIfMissing True "./out/"
    let pic1 = runDrawingU std_ctx arrow_drawing
    writeEPS "./out/arrowheads01.eps" pic1
    writeSVG "./out/arrowheads01.svg" pic1

arrow_drawing :: Drawing Double
arrow_drawing = drawTracing $ tableGraphic arrtable

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
    , (curveTip,    curveTip)
    , (revcurveTip, revcurveTip)
    ]

tableGraphic :: (Real u, Floating u, FromPtSize u) 
             => [(Arrowhead u, Arrowhead u)] -> TraceDrawing u ()
tableGraphic tips = zipWithM_ makeArrowDrawing tips ps
  where
    ps = unchain (coordinateScalingContext 120 24) $ tableDown 20 4


 
std_ctx :: DrawingContext
std_ctx = fillColour peru $ standardContext 18



makeArrowDrawing :: (Real u, Floating u, FromPtSize u) 
                 => (Arrowhead u, Arrowhead u) -> Point2 u 
                 -> TraceDrawing u ()
makeArrowDrawing (arrl,arrr) p0 = 
    drawi_ $ apply2R2 (strokeConnector (leftrightArrow connLine arrl arrr)) p0 p1
  where
    p1 = p0 .+^ hvec 100
  

