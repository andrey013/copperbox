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

import Control.Monad
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


makeGSPicture :: ShapeList -> FilePath -> IO ()
makeGSPicture shapes font_dir = do
    putStrLn "Using GhostScript metrics..."
    (gs_metrics, msgs) <- loadGSMetrics font_dir ["Courier"]
    mapM_ putStrLn msgs
    mapM_ (out1 gs_metrics) shapes 
  where
    out1 gs_metrics (name, shape_pic) = do 
       let pic1 = runCtxPictureU (makeCtx gs_metrics) $ shape_pic name
       writeEPS ("./out/shapes/" ++ name ++ "01.eps") pic1
       writeSVG ("./out/shapes/" ++ name ++ "01.svg") pic1


makeAfmPicture :: ShapeList -> FilePath -> IO ()
makeAfmPicture shapes font_dir = do
    putStrLn "Using AFM 4.1 metrics..."
    (afm_metrics, msgs) <- loadAfmMetrics font_dir ["Courier"]
    mapM_ putStrLn msgs
    mapM_ (out1 afm_metrics) shapes 
  where
    out1 afm_metrics (name, shape_pic) = do 
        let pic1 = runCtxPictureU (makeCtx afm_metrics) $ shape_pic name
        writeEPS ("./out/shapes/" ++ name ++ "02.eps") pic1
        writeSVG ("./out/shapes/" ++ name ++ "02.svg") pic1

type ShapeList = [(String, (String -> DCtxPicture))]


shape_list :: ShapeList
shape_list = 
    [ ( "circle",             shapePic voidExtra $ circle 150)
    , ( "diamond",            shapePic voidExtra $ diamond 150 100)
    , ( "ellipse",            shapePic voidExtra $ ellipse 150 100)
    , ( "invsemicircle",      shapePic voidExtra $ invsemicircle 150)
    , ( "invsemiellipse",     shapePic voidExtra $ invsemiellipse 100 150)
    , ( "invtriangle"
      , shapePic (apexAnchor >=> topCorners) $ invtriangle 300 150)
    , ( "parallelogram",      shapePic voidExtra $ zparallelogram 250 200)
    , ( "rectangle",          shapePic voidExtra $ rectangle 300 175)
    , ( "semicircle"
      , shapePic (apexAnchor >=> baseCorners) $ semicircle 150) 
    , ( "semiellipse"
      , shapePic (apexAnchor >=> baseCorners) $ semiellipse 100 150) 
    , ( "trapezium",          shapePic voidExtra $ ztrapezium 300 150)
    , ( "triangle"
      , shapePic (apexAnchor >=> baseCorners) $ triangle 300 150 )
    ]

makeCtx :: GlyphMetrics -> DrawingContext
makeCtx = fontFace courier . metricsContext 16

rotate05 :: Rotate a => a -> a
rotate05 = rotate (d2r (5::Double))

-- Extra elaboration...

voidExtra :: a -> TraceDrawing u ()
voidExtra _ = return ()

apexAnchor :: ( Real u, Floating u, FromPtSize u
            , ApexAnchor a
            , u ~ DUnit a )
            => a -> TraceDrawing u a
apexAnchor a = do
    draw $ label EAST   "(apex)"    `at` apex  a
    return a

baseCorners :: ( Real u, Floating u, FromPtSize u
               , BottomCornerAnchor a
               , u ~ DUnit a )
            => a -> TraceDrawing u a
baseCorners a = do
    draw $ label SOUTH_WEST   "(bottom left)"    `at` bottomLeftCorner  a
    draw $ label SOUTH_EAST   "(bottom right)"   `at` bottomRightCorner a
    return a

topCorners :: ( Real u, Floating u, FromPtSize u
              , TopCornerAnchor a
              , u ~ DUnit a )
           => a -> TraceDrawing u a
topCorners a = do
    draw $ label NORTH_WEST   "(top left)"    `at` topLeftCorner  a
    draw $ label NORTH_EAST   "(top right)"   `at` topRightCorner a
    return a





shapePic :: ( CenterAnchor a, CardinalAnchor a, CardinalAnchor2 a
            , RadialAnchor a
            , Scale a, Rotate a
            , DUnit a ~ Double) 
         => (a -> DTraceDrawing b) -> DShape a -> String -> DCtxPicture
shapePic mf sh name = drawTracing $ do
    a1  <- localize shapeSty $ drawi $ 
              uniformScale 2 $ rotate05 $ shape `at` (P2 100 0)
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
    draw $ label NORTH_WEST   "(110 deg)"     `at` radialAnchor deg110 a1
    draw $ label WEST         "(190 deg)"     `at` radialAnchor deg190 a1
    draw $ label NORTH        "(250 deg)"     `at` radialAnchor deg250 a1
    _ <- mf a1
    return ()    
  where
    shape   = strokedShape $ setDecoration textF sh
    textF   = ignoreAns (multiAlignCenter CENTER name)

    deg10   = d2r (10::Double)
    deg110  = d2r (110::Double)
    deg190  = d2r (190::Double)
    deg250  = d2r (250::Double)






shapeSty :: DrawingContextF
shapeSty = strokeColour light_steel_blue . ultrathick

label :: (Real u, Floating u, FromPtSize u) 
           => Cardinal -> String -> LocGraphic u
label cpos ss = markX `oplus` msg
  where
    (rpos,fn)     = go cpos
    msg           = ignoreAns $ moveStart (fn 10) $ 
                       multiAlignCenter rpos ss `rot` 0

    go NORTH      = (SS, northwards)
    go NORTH_EAST = (SW, northeastwards)
    go EAST       = (WW, eastwards) 
    go SOUTH_EAST = (NW, southeastwards)
    go SOUTH      = (NN, southwards)
    go SOUTH_WEST = (NE, southwestwards)
    go WEST       = (EE, westwards)
    go NORTH_WEST = (SE, northwestwards)
  

