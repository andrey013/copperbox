{-# OPTIONS -Wall #-}

module TableChains where


import Wumpus.Drawing.Colour.SVGColours
import Wumpus.Drawing.Extras.Grids

import Wumpus.Basic.Kernel                      -- package: wumpus-basic
import Wumpus.Core                              -- package: wumpus-core

import System.Directory

main :: IO ()
main = do 
    createDirectoryIfMissing True "./out/"
    let pic1 = runCtxPictureU std_ctx table_drawing
    writeEPS "./out/table_chains01.eps" pic1
    writeSVG "./out/table_chains01.svg" pic1

std_ctx :: DrawingContext
std_ctx = fill_colour peru $ standardContext 18

table_drawing :: CtxPicture
table_drawing = udrawTracing (0::Double) $ do 
    draw $ grid (grid_minor_colour cornflower_blue) (0,0) (4,2)
