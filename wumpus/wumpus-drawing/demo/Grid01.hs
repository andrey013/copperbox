{-# OPTIONS -Wall #-}


module Grid01 where


import Wumpus.Drawing.Colour.SVGColours
import Wumpus.Drawing.Dots.Marks
import Wumpus.Drawing.Extras.Grids
import Wumpus.Drawing.Text.DirectionZero
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
    base_metrics <- loader [ helvetica, times_roman ]
    printLoadErrors base_metrics
    let pic1 = runCtxPictureU (makeCtx base_metrics) grid_pic
    writeEPS "./out/grid01.eps" pic1
    writeSVG "./out/grid01.svg" pic1


makeCtx :: FontLoadResult -> DrawingContext
makeCtx = set_font helvetica . metricsContext 18


grid_pic :: CtxPicture
grid_pic = udrawTracing (0::Double) $ do 
    draw $ evalGrid $ localizeGrid (grid_major_colour red)
                    $ grid ((-1),(-1)) (3,2)
    return ()
