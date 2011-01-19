{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS -Wall #-}


module ShapeParallelogram where


import FontLoaderUtils

import Wumpus.Drawing.Colour.SVGColours
import Wumpus.Drawing.Dots.Marks
import Wumpus.Drawing.Shapes
import Wumpus.Drawing.Shapes.Parallelogram
import Wumpus.Drawing.Text.LRText
import Wumpus.Drawing.Text.SafeFonts

import Wumpus.Basic.Kernel
import Wumpus.Basic.System.FontLoader.Afm
import Wumpus.Basic.System.FontLoader.GhostScript


import Wumpus.Core                              -- package: wumpus-core

import System.Directory


main :: IO ()
main = do 
    (mb_gs, mb_afm) <- processCmdLine default_font_loader_help
    createDirectoryIfMissing True "./out/shapes/"
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
    writeEPS "./out/shapes/parallelogram01.eps" pic1
    writeSVG "./out/shapes/parallelogram01.svg" pic1

makeAfmPicture :: FilePath -> IO ()
makeAfmPicture font_dir = do
    putStrLn "Using AFM 4.1 metrics..."
    (afm_metrics, msgs) <- loadAfmMetrics font_dir ["Courier"]
    mapM_ putStrLn msgs
    let pic2 = runCtxPictureU (makeCtx afm_metrics) shape_pic
    writeEPS "./out/shapes/parallelogram02.eps" pic2
    writeSVG "./out/shapes/parallelogram02.svg" pic2




makeCtx :: GlyphMetrics -> DrawingContext
makeCtx = fontFace courier . metricsContext 16


shape_pic :: DCtxPicture
shape_pic = drawTracing $ do
    a1  <- localize shapeSty $ drawi $ 
              (strokedShape $ zparallelogram 300 150) `at` (P2 200 150)
    draw $ label NORTH        "(center)"      `at` center a1
{-
    draw $ label NORTH        "(north)"       `at` north a1
    draw $ label SOUTH        "(south)"       `at` south a1
    draw $ label EAST         "(east)"        `at` east a1
    draw $ label WEST         "(west)"        `at` west a1
    draw $ label NORTH_EAST   "(northeast)"   `at` northeast a1
    draw $ label NORTH_WEST   "(northwest)"   `at` northwest a1
    draw $ label SOUTH_EAST   "(southeast)"   `at` southeast a1
    draw $ label SOUTH_WEST   "(southwest)"   `at` southwest a1
    draw $ label EAST         "(10 deg)"      `at` radialAnchor deg10 a1
-}
    return ()    
  where
    deg10 = d2r (10::Double)
    



shapeSty :: DrawingContextF
shapeSty = strokeColour light_steel_blue . ultrathick

labelPoint :: (Real u, Floating u, FromPtSize u) 
           => (u -> PointDisplace u) -> String -> LocGraphic u
labelPoint fn ss = markX `oplus` msg
  where
    msg = ignoreAns $ moveStartPoint (fn 16) $ textAlignCenter ss


labelWest :: (Real u, Floating u, FromPtSize u) 
           => String -> LocGraphic u
labelWest ss = markX `oplus` msg
  where
    msg = ignoreAns $ moveStartPoint (westwards 10) $ 
              multiAlignCenter EE ss `rot` 0

labelEast :: (Real u, Floating u, FromPtSize u) 
           => String -> LocGraphic u
labelEast ss = markX `oplus` msg
  where
    msg = ignoreAns $ moveStartPoint (eastwards 10) $ 
              multiAlignCenter WW ss `rot` 0

              

label :: (Real u, Floating u, FromPtSize u) 
           => Cardinal -> String -> LocGraphic u
label cpos ss = markX `oplus` msg
  where
    (rpos,fn)     = go cpos
    msg           = ignoreAns $ moveStartPoint (fn 10) $ 
                       multiAlignCenter rpos ss `rot` 0

    go NORTH      = (SS, northwards)
    go NORTH_EAST = (SW, northeastwards)
    go EAST       = (WW, eastwards) 
    go SOUTH_EAST = (NW, southeastwards)
    go SOUTH      = (NN, southwards)
    go SOUTH_WEST = (NE, southwestwards)
    go WEST       = (EE, westwards)
    go NORTH_WEST = (SE, northwestwards)
  

-- Note - it would be nice to have an API like PPrint for 
-- constructing escaped text. 