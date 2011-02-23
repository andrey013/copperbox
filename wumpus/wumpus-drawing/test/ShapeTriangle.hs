{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS -Wall #-}


module ShapeTriangle where


import FontLoaderUtils

import Wumpus.Drawing.Colour.SVGColours
import Wumpus.Drawing.Dots.Marks
import Wumpus.Drawing.Shapes
import Wumpus.Drawing.Text.RotTextLR
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
    base_metrics <- loadGSFontMetrics font_dir ["Courier"]
    printLoadErrors base_metrics
    let pic1 = runCtxPictureU (makeCtx base_metrics) shape_pic
    writeEPS "./out/shapes/triangle01.eps" pic1
    writeSVG "./out/shapes/triangle01.svg" pic1

makeAfmPicture :: FilePath -> IO ()
makeAfmPicture font_dir = do
    putStrLn "Using AFM 4.1 metrics..."
    base_metrics <- loadAfmFontMetrics font_dir ["Courier"]
    printLoadErrors base_metrics
    let pic2 = runCtxPictureU (makeCtx base_metrics) shape_pic
    writeEPS "./out/shapes/triangle02.eps" pic2
    writeSVG "./out/shapes/triangle02.svg" pic2




makeCtx :: FontLoadResult -> DrawingContext
makeCtx = set_font courier . metricsContext 16


shape_pic :: DCtxPicture
shape_pic = drawTracing $ do
    a1  <- localize shapeSty $ drawi $ 
              (strokedShape $ triangle 300 200) `at` (P2 200 150)    
    draw $ label NORTH        "(center)"      `at` center a1
    draw $ label NORTH        "(north)"       `at` radialP  90 a1
    draw $ label SOUTH        "(south)"       `at` radialP 270 a1
    draw $ label EAST         "(east)"        `at` radialP   0 a1
    draw $ label WEST         "(west)"        `at` radialP 180 a1
    draw $ label NORTH_EAST   "(northeast)"   `at` radialP  45 a1
    draw $ label NORTH_WEST   "(northwest)"   `at` radialP 135 a1
    draw $ label SOUTH_EAST   "(southeast)"   `at` radialP 315 a1
    draw $ label SOUTH_WEST   "(southwest)"   `at` radialP 225 a1
    draw $ label EAST         "(10 deg)"      `at` radialAnchor deg10 a1
    return ()    
  where
    deg10 = d2r (10::Double)

radialP :: (RadialAnchor a, DUnit a ~ u) => Double -> a -> Point2 u
radialP deg a = radialAnchor (d2r deg) a

shapeSty :: DrawingContextF
shapeSty = stroke_colour light_steel_blue . line_ultra_thick

labelPoint :: (Real u, Floating u, PtSize u) 
           => (u -> PointDisplace u) -> String -> LocGraphic u
labelPoint fn ss = markX `oplus` msg
  where
    msg = ignoreAns $ moveStart (fn 16) $ textAlignCenter ss


labelWest :: (Real u, Floating u, PtSize u) 
           => String -> LocGraphic u
labelWest ss = markX `oplus` msg
  where
    msg = ignoreAns $ moveStart (westwards 10) $ 
              apply2R3 (multiAlignCenter ss) EE 0

labelEast :: (Real u, Floating u, PtSize u) 
           => String -> LocGraphic u
labelEast ss = markX `oplus` msg
  where
    msg = ignoreAns $ moveStart (eastwards 10) $ 
              apply2R3 (multiAlignCenter ss) WW 0

              

label :: (Real u, Floating u, PtSize u) 
           => Cardinal -> String -> LocGraphic u
label cpos ss = markX `oplus` msg
  where
    (rpos,fn)     = go cpos
    msg           = ignoreAns $ moveStart (fn 10) $ 
                       apply2R3 (multiAlignCenter ss) rpos 0

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