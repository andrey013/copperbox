{-# LANGUAGE FlexibleContexts           #-}
{-# OPTIONS -Wall #-}


module SampleShapes where


import Wumpus.Drawing.Colour.SVGColours
import Wumpus.Drawing.Dots.Marks
import Wumpus.Drawing.Shapes
import Wumpus.Drawing.Text.DirectionZero
import Wumpus.Drawing.Text.StandardFontDefs

import Wumpus.Basic.Kernel                      -- package: wumpus-basic
import Wumpus.Basic.System.FontLoader

import Wumpus.Core                              -- package: wumpus-core

import Control.Monad
import System.Directory

main :: IO ()
main = simpleFontLoader main1 >> return ()

main1 :: FontLoader -> IO ()
main1 loader = do
    createDirectoryIfMissing True "./out/" 
    base_metrics <- loader [courier]
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
          ztrapezium 300 150)
    , ( "triangle"
      , shapePic (apexAnchor >=> bottomCorners >=> midPoints 3) $ 
          triangle 300 150 )
    ]

makeCtx :: FontLoadResult -> DrawingContext
makeCtx = set_font courier . metricsContext 16

rotate05 :: (CtxRotate t u, InterpretUnit u) => Image t u -> Image t u
rotate05 = rotate (d2r (5::Double))

-- Extra elaboration...

voidExtra :: a -> TraceDrawing u ()
voidExtra _ = return ()

-- What to call this one?
--
infixr 1 `aat`

aat :: LocImage t u -> Anchor u -> Image t u
aat img a = a >>= \pt -> img `at` pt



apexAnchor :: ( Real u, Floating u, InterpretUnit u
              , ApexAnchor t u)
            => t u -> TraceDrawing u (t u)
apexAnchor a = do
    draw $ label EAST   "(apex)"    `aat` apex  a
    return a

bottomCorners :: ( Real u, Floating u, InterpretUnit u
                 , BottomCornerAnchor t u
                 )
            => t u -> TraceDrawing u (t u)
bottomCorners a = do
    draw $ label SOUTH_WEST   "(bottom left)"    `aat` bottomLeftCorner  a
    draw $ label SOUTH_EAST   "(bottom right)"   `aat` bottomRightCorner a
    return a

topCorners :: ( Real u, Floating u, InterpretUnit u
              , TopCornerAnchor t u
              )
           => (t u) -> TraceDrawing u (t u)
topCorners a = do
    draw $ label NORTH_WEST   "(top left)"    `aat` topLeftCorner  a
    draw $ label NORTH_EAST   "(top right)"   `aat` topRightCorner a
    return a


midPoints :: ( Real u, Floating u, InterpretUnit u
             , SideMidpointAnchor t u
             )
          => Int -> t u -> TraceDrawing u (t u)
midPoints n a = mapM_ mf [1..n] >> return a
  where
    mf i = let msg = "(side midpt " ++ show i ++ ")"
           in draw $ label EAST  msg    `aat` sideMidpoint i  a




shapePic :: ( Functor t
            , CenterAnchor t Double
            , CardinalAnchor t Double
            , CardinalAnchor2 t Double
            , RadialAnchor t Double
            , CtxScale t Double
            , CtxRotate t Double
            ) 
         => (t Double -> DTraceDrawing a) -> DShape t -> String -> CtxPicture
shapePic mf sh name = udrawTracing (0::Double) $ do
    a1  <- localize shapeSty $ drawi $ 
              uniformScale 2 $ rotate05 $ shape `at` (P2 100 0)
    draw $ label NORTH        "(center)"      `aat` center a1
    draw $ label NORTH        "(north)"       `aat` north a1
    draw $ label SOUTH        "(south)"       `aat` south a1
    draw $ label EAST         "(east)"        `aat` east a1
    draw $ label WEST         "(west)"        `aat` west a1
    draw $ label NORTH_EAST   "(northeast)"   `aat` northeast a1
    draw $ label NORTH_WEST   "(northwest)"   `aat` northwest a1
    draw $ label SOUTH_EAST   "(southeast)"   `aat` southeast a1
    draw $ label SOUTH_WEST   "(southwest)"   `aat` southwest a1
    draw $ label EAST         "(10 deg)"      `aat` radialAnchor deg10 a1
    draw $ label NORTH_WEST   "(110 deg)"     `aat` radialAnchor deg110 a1
    draw $ label WEST         "(190 deg)"     `aat` radialAnchor deg190 a1
    draw $ label NORTH        "(250 deg)"     `aat` radialAnchor deg250 a1
    _ <- mf a1
    return ()    
  where
    shape   = strokedShape $ setDecoration textF sh
    textF   = lift1R2 $ ignoreAns (startPosRot (multiAlignCenter name) CENTER 0)

    deg10   = d2r (10::Double)
    deg110  = d2r (110::Double)
    deg190  = d2r (190::Double)
    deg250  = d2r (250::Double)






shapeSty :: DrawingContextF
shapeSty = stroke_colour light_steel_blue . line_ultra_thick

label :: (Real u, Floating u, InterpretUnit u) 
      => Cardinal -> String -> LocGraphic u
label cpos ss = markX `oplus` msg
  where
    (rpos,fn)     = go cpos
    msg           = ignoreAns $ moveStart (fn 10) $ 
                       startPosRot (multiAlignCenter ss) rpos 0

    go NORTH      = (SS, northwards)
    go NORTH_EAST = (SW, northeastwards)
    go EAST       = (WW, eastwards) 
    go SOUTH_EAST = (NW, southeastwards)
    go SOUTH      = (NN, southwards)
    go SOUTH_WEST = (NE, southwestwards)
    go WEST       = (EE, westwards)
    go NORTH_WEST = (SE, northwestwards)
  

