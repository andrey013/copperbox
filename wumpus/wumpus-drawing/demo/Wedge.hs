{-# OPTIONS -Wall #-}


module Wedge where

import Wumpus.Drawing.Basis.DrawingPrimitives
import Wumpus.Drawing.Colour.SVGColours
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
    writeEPS "./out/wedge.eps" pic1
    writeSVG "./out/wedge.svg" pic1


makeCtx :: FontLoadResult -> DrawingContext
makeCtx = set_font helvetica . metricsContext 18


grid_pic :: CtxPicture
grid_pic = udrawTracing (0::Double) $ do 
    radius <- fmap ((3*) . vector_x) $ snapmove (1,0)
    node (0,0) $ grid (dotted_major_grid) 4 3
    node (0,0) $ arc radius (0.25*pi) `incline` 0

    node (5,0) $ grid (dotted_major_grid) 4 3
    node (5,0) $ wedge DRAW_FILL_STROKE radius (0.25*pi) `incline` 0
    return ()
