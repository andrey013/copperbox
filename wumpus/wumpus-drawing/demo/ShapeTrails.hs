{-# OPTIONS -Wall #-}

module ShapeTrails where

import Wumpus.Drawing.Basis.DrawingPrimitives
import Wumpus.Drawing.Basis.ShapeTrails
import Wumpus.Drawing.Colour.SVGColours
import Wumpus.Drawing.Dots.SimpleDots
import Wumpus.Drawing.Paths
import Wumpus.Drawing.Text.DirectionZero
import Wumpus.Drawing.Text.StandardFontDefs

import Wumpus.Basic.Kernel                      -- package: wumpus-basic
import Wumpus.Basic.System.FontLoader


import Wumpus.Core                              -- package: wumpus-core


import Data.Monoid
import System.Directory

main :: IO ()
main = simpleFontLoader main1 >> return ()

main1 :: FontLoader -> IO ()
main1 loader = do
    createDirectoryIfMissing True "./out/"    
    base_metrics <- loader [ Left helvetica ]
    printLoadErrors base_metrics
    let pic1 = runCtxPictureU (makeCtx base_metrics) shape_pic
    writeEPS "./out/shape_trails.eps" pic1
    writeSVG "./out/shape_trails.svg" pic1 


makeCtx :: FontLoadResult -> DrawingContext
makeCtx = set_font helvetica . metricsContext 11

shape_pic :: CtxPicture 
shape_pic = drawTracing $ tableGraphic shape_table


-- Note - for shapes inlcine is probably not the best API...
-- rectangle merits (w * h) etc.

type ShapeElt = (String, Radian -> AnaTrail Double)

shape_table :: [ShapeElt]
shape_table = 
    [ ("rcircle_trail",                 rcircle_trail 20)
    , ("rrectangle_trail",              rrectangle_trail 40 20)
    , ("rdiamond_trail",                rdiamond_trail 20 40)
    , ("risosceles_triangle_trail",     risosceles_triangle_trail 20 40)
    , ("rsemicircle_trail",             rsemicircle_trail 20)
    , ("rellipse_trail",                rellipse_trail 30 20)
    , ("rsemiellipse_trail",            rsemiellipse_trail 30 20)
    ]


tableGraphic :: [ShapeElt] -> TraceDrawing Double ()
tableGraphic ss = 
    drawl start $ ignoreAns $ runTableColumnwise 6 (200,80)
                $ mapM (chain1 .  makeShapeGraphic1) ss
  where
    start = P2 0 520 

makeShapeGraphic1 :: ShapeElt -> DLocGraphic 
makeShapeGraphic1 (ss,trailf) = dsk <> pth <> lbl
  where
    dsk = moveStart (V2 28 6) $ smallDisk
    pth = moveStart (V2 28 6) $ renderAnaTrail CSTROKE $ trailf ang15
    lbl = moveStart (V2 56 12) $ ignoreAns $ textline WW ss 
    
    
