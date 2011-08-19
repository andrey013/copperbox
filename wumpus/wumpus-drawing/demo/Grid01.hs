{-# OPTIONS -Wall #-}


module Grid01 where


import Wumpus.Drawing.Colour.SVGColours
import Wumpus.Drawing.Dots.SimpleDots
import Wumpus.Drawing.Extras.Axes
import Wumpus.Drawing.Extras.Grids
import Wumpus.Drawing.Text.StandardFontDefs

import Wumpus.Basic.Kernel                      -- package: wumpus-basic
import Wumpus.Basic.System.FontLoader

import Wumpus.Core                              -- package: wumpus-core

import System.Directory



main :: IO ()
main = simpleFontLoader main1 >> return ()

main1 :: FontLoader -> IO ()
main1 loader = do
    createDirectoryIfMissing True "./out/"    
    base_metrics <- loader [ Left helvetica, Left times_roman ]
    printLoadErrors base_metrics
    let pic1 = runCtxPictureU (makeCtx base_metrics) grid_pic
    writeEPS "./out/grid01.eps" pic1
    writeSVG "./out/grid01.svg" pic1


makeCtx :: FontLoadResult -> DrawingContext
makeCtx = set_font helvetica . metricsContext 18


grid_pic :: CtxPicture
grid_pic = udrawTracing (0::Double) $ do 
    node (0,0) $ grid (grid_major_colour red) 4 3
    node (1,1) $ orthontAxes (1,3) (1,2)
    node (0,6) $ smallDisk
    node (0,6) $ grid (grid_major_colour red) 4 3
    node (0,6) $ horizontalLabels NN [0,1,2,3,4]
    node (0,6) $ verticalLabels EE [0,1,2,3]
    return ()
