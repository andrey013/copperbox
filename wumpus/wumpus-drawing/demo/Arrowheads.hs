{-# OPTIONS -Wall #-}

module Arrowheads where


import Wumpus.Drawing.Arrows
import Wumpus.Drawing.Chains
import Wumpus.Drawing.Colour.SVGColours
import Wumpus.Drawing.Paths hiding ( length )

import Wumpus.Basic.Kernel                      -- package: wumpus-basic

import Wumpus.Core                              -- package: wumpus-core

import Data.AffineSpace                         -- package: vector-space

import System.Directory

main :: IO ()
main = do 
    createDirectoryIfMissing True "./out/"
    let pic1 = runCtxPictureU std_ctx arrow_drawing
    writeEPS "./out/arrowheads01.eps" pic1
    writeSVG "./out/arrowheads01.svg" pic1

arrow_drawing :: CtxPicture
arrow_drawing = 
    drawTracing $ localize dotted_line $ tableGraphic arrtable

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

tableGraphic :: [(Arrowhead Double, Arrowhead Double)] -> TraceDrawing Double ()
tableGraphic tips = 
    drawi_ $ chn (map makeArrowDrawing tips) `at` start
  where
    chn   = tableDown 20 (120,24)
    start = P2 0 480

 
std_ctx :: DrawingContext
std_ctx = fill_colour peru $ standardContext 18



makeArrowDrawing :: (Arrowhead Double, Arrowhead Double) -> LocGraphic Double
makeArrowDrawing (arrl,arrr) = 
    promote_li1 $ \p0 -> ignoreAns $
      connect (leftRightArrow arrl arrr connLine) p0 (mkP1 p0)
  where
    mkP1    = (.+^ hvec 100)

