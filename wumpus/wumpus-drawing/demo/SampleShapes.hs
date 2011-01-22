{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS -Wall #-}


module SampleShapes where


import FontLoaderUtils

import Wumpus.Drawing.Colour.SVGColours
import Wumpus.Drawing.Dots.Marks
import Wumpus.Drawing.Shapes
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
    maybe gs_failk  (makeGSPicture shape_list)  $ mb_gs
    maybe afm_failk (makeAfmPicture shape_list) $ mb_afm
  where
    gs_failk  = putStrLn "No GhostScript font path supplied..."
    afm_failk = putStrLn "No AFM v4.1 font path supplied..."


makeGSPicture :: [(String, DCtxPicture)] -> FilePath -> IO ()
makeGSPicture shapes font_dir = do
    putStrLn "Using GhostScript metrics..."
    (gs_metrics, msgs) <- loadGSMetrics font_dir ["Courier"]
    mapM_ putStrLn msgs
    mapM_ (out1 gs_metrics) shapes 
  where
    out1 gs_metrics (name, shape_pic) = do 
       let pic1 = runCtxPictureU (makeCtx gs_metrics) shape_pic
       writeEPS ("./out/shapes/" ++ name ++ "01.eps") pic1
       writeSVG ("./out/shapes/" ++ name ++ "01.svg") pic1


makeAfmPicture :: [(String, DCtxPicture)] -> FilePath -> IO ()
makeAfmPicture shapes font_dir = do
    putStrLn "Using AFM 4.1 metrics..."
    (afm_metrics, msgs) <- loadAfmMetrics font_dir ["Courier"]
    mapM_ putStrLn msgs
    mapM_ (out1 afm_metrics) shapes 
  where
    out1 afm_metrics (name, shape_pic) = do 
        let pic1 = runCtxPictureU (makeCtx afm_metrics) shape_pic
        writeEPS ("./out/shapes/" ++ name ++ "02.eps") pic1
        writeSVG ("./out/shapes/" ++ name ++ "02.svg") pic1



shape_list :: [(String, DCtxPicture)]
shape_list = 
    [ ( "circle",         shapePic $ circle 150)
    , ( "diamond",        shapePic $ diamond 150 100)
    , ( "ellipse",        shapePic $ ellipse 150 100)
    , ( "parallelogram",  shapePic $ zparallelogram 150 100)
    , ( "rectangle",      shapePic $ rectangle 300 200)
    , ( "semicircle",     shapePic_cnsew $ semicircle 150) 
    , ( "semiellipse",    shapePic_cns $ semiellipse 100 150) 
    , ( "trapezium",      shapePic $ ztrapezium 300 150)
    , ( "triangle",       shapePic $ isoscelesTriangle 300 200)
    ]

makeCtx :: GlyphMetrics -> DrawingContext
makeCtx = fontFace courier . metricsContext 16




shapePic :: ( CenterAnchor a, CardinalAnchor a, CardinalAnchor2 a
            , RadialAnchor a
            , DUnit a ~ Double) 
             => DLocShape a -> DCtxPicture
shapePic sh = drawTracing $ do
    a1  <- localize shapeSty $ drawi $ (strokedShape $ sh) `at` (P2 200 150)    
    draw $ label NORTH        "(center)"      `at` center a1
    draw $ label NORTH        "(north)"       `at` north a1
    draw $ label SOUTH        "(south)"       `at` south a1
    draw $ label EAST         "(east)"        `at` east a1
    draw $ label WEST         "(west)"        `at` west a1
    draw $ label NORTH_EAST   "(northeast)"   `at` northeast a1
    draw $ label NORTH_WEST   "(northwest)"   `at` northwest a1
    draw $ label SOUTH_EAST   "(southeast)"   `at` southeast a1
    draw $ label SOUTH_WEST   "(southwest)"   `at` southwest a1
    draw $ label EAST         "(10 deg)"      `at` radialAnchor deg10 a1
    return ()    
  where
    deg10 = d2r (10::Double)



-- Just center and Cardinal1 anchors - this is for semicircle
-- which doesn\'t have a set of anchors properly defined.
--
shapePic_cnsew :: (CenterAnchor a, CardinalAnchor a, DUnit a ~ Double) 
               => DLocShape a -> DCtxPicture
shapePic_cnsew sh = drawTracing $ do
    a1  <- localize shapeSty $ drawi $ (strokedShape sh) `at` (P2 200 150)
    draw $ label NORTH        "(center)"      `at` center a1
    draw $ label NORTH        "(north)"       `at` north a1
    draw $ label SOUTH        "(south)"       `at` south a1
    draw $ label EAST         "(east)"        `at` east a1
    draw $ label WEST         "(west)"        `at` west a1
    return ()


-- Just center, north and south anchors - this is for semiellipse
-- which doesn\'t have a set of anchors properly defined.
--
shapePic_cns :: (CenterAnchor a, CardinalAnchor a, DUnit a ~ Double) 
             => DLocShape a -> DCtxPicture
shapePic_cns sh = drawTracing $ do
    a1  <- localize shapeSty $ drawi $ (strokedShape sh) `at` (P2 200 150)
    draw $ label NORTH        "(center)"      `at` center a1
    draw $ label NORTH        "(north)"       `at` north a1
    draw $ label SOUTH        "(south)"       `at` south a1
    return ()




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