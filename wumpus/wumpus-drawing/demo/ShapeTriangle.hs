{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS -Wall #-}


module ShapeTriangle where


import FontLoaderUtils

import Wumpus.Drawing.Colour.SVGColours
import Wumpus.Drawing.Dots.Marks
import Wumpus.Drawing.Shapes
import Wumpus.Drawing.Shapes.Triangle
import Wumpus.Drawing.Text.LRText
import Wumpus.Drawing.Text.SafeFonts

import Wumpus.Basic.Kernel hiding ( northwards, southwards, eastwards, westwards )
import Wumpus.Basic.System.FontLoader.Afm
import Wumpus.Basic.System.FontLoader.GhostScript


import Wumpus.Core                              -- package: wumpus-core

import System.Directory


main :: IO ()
main = do 
    (mb_gs, mb_afm) <- processCmdLine default_font_loader_help
    createDirectoryIfMissing True "./out/"
    maybe gs_failk  makeGSPicture  $ mb_gs
    maybe afm_failk makeAfmPicture $ mb_afm
  where
    gs_failk  = putStrLn "No GhostScript font path supplied..."
    afm_failk = putStrLn "No AFM v4.1 font path supplied..."


makeGSPicture :: FilePath -> IO ()
makeGSPicture font_dir = do
    putStrLn "Using GhostScript metrics..."
    (gs_metrics, msgs) <- loadGSMetrics font_dir ["Courier"]
    mapM_ putStrLn msgs
    let pic1 = runCtxPictureU (makeCtx gs_metrics) shape_pic
    writeEPS "./out/shape_triangle01.eps" pic1
    writeSVG "./out/shape_triangle01.svg" pic1

makeAfmPicture :: FilePath -> IO ()
makeAfmPicture font_dir = do
    putStrLn "Using AFM 4.1 metrics..."
    (afm_metrics, msgs) <- loadAfmMetrics font_dir ["Courier"]
    mapM_ putStrLn msgs
    let pic2 = runCtxPictureU (makeCtx afm_metrics) shape_pic
    writeEPS "./out/shape_triangle02.eps" pic2
    writeSVG "./out/shape_triangle02.svg" pic2




makeCtx :: GlyphMetrics -> DrawingContext
makeCtx = fontFace courier . metricsContext 16


shape_pic :: DCtxPicture
shape_pic = drawTracing $ do
    a1  <- localize shapeSty $ drawi $ 
              (strokedShape $ isoscelesTriangle 300 200) `at` (P2 200 150)    
    draw $ labelPoint northwards "center"   `at` center a1
    draw $ labelPoint northwards "north"    `at` north a1
    draw $ labelPoint southwards "south"    `at` south a1
    draw $ labelPoint eastwards  "east"     `at` east a1
    draw $ labelPoint westwards  "west"     `at` west a1
    return ()    


shapeSty :: DrawingContextF
shapeSty = strokeColour light_slate_gray . ultrathick

labelPoint :: (Real u, Floating u, FromPtSize u) 
           => ParaPointDisplace u -> String -> LocGraphic u
labelPoint fn ss = markX `oplus` msg
  where
    msg = ignoreAns $ moveStartPoint (fn 16) $ textAlignCenter ss

    -- note this move does not work well in the horizontal...


-- For Wumpus-Basic...

type ParaPointDisplace u = u -> PointDisplace u

northwards :: Num u => u -> PointDisplace u
northwards d = displaceV d


southwards :: Num u => u -> PointDisplace u
southwards d = displaceV (negate d)

eastwards :: Num u => u -> PointDisplace u
eastwards d = displaceH d

westwards :: Num u => u -> PointDisplace u
westwards d = displaceH (negate d)


-- Note - it would be nice to have an API like PPrint for 
-- constructing escaped text. 