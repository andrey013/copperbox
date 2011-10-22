{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# OPTIONS -Wall #-}


module SampleShapes where


import Wumpus.Drawing.Colour.SVGColours
import Wumpus.Drawing.Dots.SimpleDots
import Wumpus.Drawing.Shapes
import Wumpus.Drawing.Text.DirectionZero
import Wumpus.Drawing.Text.StandardFontDefs

import Wumpus.Basic.Kernel                      -- package: wumpus-basic
import Wumpus.Basic.System.FontLoader

import Wumpus.Core                              -- package: wumpus-core

import Control.Monad
import Data.Monoid
import System.Directory

main :: IO ()
main = simpleFontLoader main1 >> return ()

main1 :: FontLoader -> IO ()
main1 loader = do
    createDirectoryIfMissing True "./out/shapes/" 
    base_metrics <- loader [ Left courier ]
    printLoadErrors base_metrics
    let ctx = makeCtx base_metrics
    mapM_ (out1 ctx) shape_list
  where
    out1 ctx (name, shape_pic) = do 
       let pic1 = runCtxPictureU ctx $ shape_pic name
       writeEPS ("./out/shapes/" ++ name ++ "01.eps") pic1
       writeSVG ("./out/shapes/" ++ name ++ "01.svg") pic1


type ShapeList = [(String, (String -> CtxPicture))]


shape_list :: ShapeList
shape_list = 
    [ ( "circle"
      , shapePic voidExtra $ circle 150)
    , ( "diamond"
      ,  shapePic (apexAnchor >=> midPoints 4) $ diamond 150 100)
    , ( "ellipse"
      , shapePic voidExtra $ ellipse 150 100)
    , ( "invsemicircle"
      , shapePic (apexAnchor >=> topCorners) $ invsemicircle 150)
    , ( "invsemiellipse"
      , shapePic (apexAnchor >=> topCorners) $ invsemiellipse 100 150)
    , ( "invtriangle"
      , shapePic (apexAnchor >=> topCorners >=> midPoints 3) $ 
          invtriangle 300 150)
    , ( "parallelogram"
      , shapePic (topCorners >=> bottomCorners >=> midPoints 4) $ 
          zparallelogram 250 200)
    , ( "rectangle"
      , shapePic (topCorners >=> bottomCorners >=> midPoints 4) $ 
          rectangle 300 175)
    , ( "semicircle"
      , shapePic (apexAnchor >=> bottomCorners) $ semicircle 150) 
    , ( "semiellipse"
      , shapePic (apexAnchor >=> bottomCorners) $ semiellipse 100 150) 
    , ( "trapezium"
      ,  shapePic (bottomCorners >=> topCorners >=> midPoints 4) $ 
          trapezium 300 200 150)
    , ( "triangle"
      , shapePic (apexAnchor >=> bottomCorners >=> midPoints 3) $ 
          triangle 300 150 )
    ]

makeCtx :: FontLoadResult -> DrawingContext
makeCtx = set_font courier . metricsContext 16

rotate05 :: Rotate a => Image u a -> Image u a
rotate05 = rotate (d2r (5::Double))

-- Extra elaboration...

voidExtra :: a -> TraceDrawing u ()
voidExtra _ = return ()



apexAnchor :: ( Real u, Floating u, InterpretUnit u
              , ApexAnchor a, u ~ DUnit a)
            => a -> TraceDrawing u a
apexAnchor a = do
    draw $ label EAST   "(apex)"    `at` apex  a
    return a

bottomCorners :: ( Real u, Floating u, InterpretUnit u
                 , BottomCornerAnchor a, u ~ DUnit a
                 )
            => a -> TraceDrawing u a
bottomCorners a = do
    draw $ label SOUTH_WEST   "(bottom left)"    `at` bottomLeftCorner  a
    draw $ label SOUTH_EAST   "(bottom right)"   `at` bottomRightCorner a
    return a

topCorners :: ( Real u, Floating u, InterpretUnit u
              , TopCornerAnchor a, u ~ DUnit a
              )
           => a -> TraceDrawing u a
topCorners a = do
    draw $ label NORTH_WEST   "(top left)"    `at` topLeftCorner  a
    draw $ label NORTH_EAST   "(top right)"   `at` topRightCorner a
    return a


midPoints :: ( Real u, Floating u, InterpretUnit u
             , SideMidpointAnchor a, u ~ DUnit a
             )
          => Int -> a -> TraceDrawing u a
midPoints n a = mapM_ mf [1..n] >> return a
  where
    mf i = let msg = "(side midpt " ++ show i ++ ")"
           in draw $ label EAST  msg    `at` sideMidpoint i  a




shapePic :: ( Functor t
            , CenterAnchor (t Double)
            , CardinalAnchor (t Double)
            , CardinalAnchor2 (t Double)
            , RadialAnchor (t Double)
            , Scale (t Double)
            , Rotate (t Double)
            , Double ~ DUnit (t Double)
            ) 
         => (t Double -> DTraceDrawing a) -> DShape t -> String -> CtxPicture
shapePic mf sh name = udrawTracing (0::Double) $ do
    a1  <- localize shapeSty $ drawi $ 
              uniformScale 2 $ {- rotate05 $ -} shape `at` (P2 100 0)
    draw $ label NORTH        "(center)"      `at` center a1
    draw $ label NORTH        "(north)"       `at` north a1
    draw $ label SOUTH        "(south)"       `at` south a1
    draw $ label EAST         "(east)"        `at` east a1
    draw $ label WEST         "(west)"        `at` west a1
    draw $ label NORTH_EAST   "(northeast)"   `at` northeast a1
    draw $ label NORTH_WEST   "(northwest)"   `at` northwest a1
    draw $ label SOUTH_EAST   "(southeast)"   `at` southeast a1
    draw $ label SOUTH_WEST   "(southwest)"   `at` southwest a1
    draw $ label EAST         "(10 deg)"      `at` radialAnchor (d2r 10) a1
    draw $ label NORTH_WEST   "(110 deg)"     `at` radialAnchor (d2r 110) a1
    draw $ label WEST         "(190 deg)"     `at` radialAnchor (d2r 190) a1
    draw $ label NORTH        "(250 deg)"     `at` radialAnchor (d2r 250) a1
    draw $ label WEST         "(200 deg)"     `at` radialAnchor (d2r 200) a1

    draw $ label WEST         "(0 deg)"     `at` radialAnchor (d2r 0) a1
    draw $ label WEST         "(359 deg)"     `at` radialAnchor (d2r 359) a1
    _ <- mf a1
    return ()    
  where
    shape   = strokedShape $ setDecoration textF sh
    textF   = promoteLocTheta $ \pt _ -> 
                ignoreAns (multilineText VALIGN_CENTER CENTER name) `at` pt





shapeSty :: DrawingContextF
shapeSty = stroke_colour light_steel_blue . line_ultra_thick

label :: (Real u, Floating u, InterpretUnit u) 
      => Cardinal -> String -> LocGraphic u
label cpos ss = dotX `mappend` msg
  where
    (rpos,fn)     = go cpos
    msg           = ignoreAns $ moveStart (fn 10) $ 
                       multilineText VALIGN_CENTER rpos ss 

    go NORTH      = (SS, go_north)
    go NORTH_EAST = (SW, go_north_east)
    go EAST       = (WW, go_east) 
    go SOUTH_EAST = (NW, go_south_east)
    go SOUTH      = (NN, go_south)
    go SOUTH_WEST = (NE, go_south_west)
    go WEST       = (EE, go_west)
    go NORTH_WEST = (SE, go_north_west)
  

