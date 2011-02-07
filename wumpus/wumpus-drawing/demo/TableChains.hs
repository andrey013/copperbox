{-# OPTIONS -Wall #-}

module TableChains where


import Wumpus.Basic.Kernel
import Wumpus.Drawing.Colour.SVGColours
import Wumpus.Drawing.Grids

import Wumpus.Core                              -- package: wumpus-core

import System.Directory

main :: IO ()
main = do 
    createDirectoryIfMissing True "./out/"
    let pic1 = runCtxPictureU std_ctx table_drawing
    writeEPS "./out/table_chains01.eps" pic1
    writeSVG "./out/table_chains01.svg" pic1

std_ctx :: DrawingContext
std_ctx = fillColour peru $ standardContext 18

table_drawing :: CtxPicture Double
table_drawing = drawTracing $ do 
--    tableGraphic
    draw $ connect (interiorGrid 10) (P2 (-20) (-20)) (P2 150 80)
    draw $ grid (3,2) 20 `at` (P2 300 60)

{-
tableGraphic :: (Real u, Floating u, FromPtSize u) 
             => TraceDrawing u ()
tableGraphic = do 
    draw $ filledDisk 3  `at` dstart
    draw $ filledDisk 3  `at` rstart
    zipchainWith (textline . show) [1..20::Int] downs
    zipchainWith (textline . show) [1..20::Int] rights
  where
    downs   = tableDown  4 (36,24) dstart
    rights  = tableRight 5 (36,24) rstart

    dstart  = P2 0   200   -- note grows down...
    rstart  = P2 240 200   -- ditto
-}
 





