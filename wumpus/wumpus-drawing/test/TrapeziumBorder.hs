{-# OPTIONS -Wall #-}

module TrapeziumBorder where

import Wumpus.Drawing.Arrows
import Wumpus.Drawing.Colour.SVGColours
import Wumpus.Drawing.Dots.Marks
import Wumpus.Drawing.Geometry.Quadrant
import Wumpus.Drawing.Paths
import Wumpus.Drawing.Text.SafeFonts

import Wumpus.Basic.Kernel                      -- package: wumpus-basic
import Wumpus.Core                              -- package: wumpus-core

import System.Directory



main :: IO ()
main = do 
    createDirectoryIfMissing True "./out/" 
    let pic1 = runCtxPictureU dctx $ drawing1
    writeEPS "./out/trapezium_border01.eps" pic1
    writeSVG "./out/trapezium_border01.svg" pic1



dctx :: DrawingContext
dctx = fontFace courier_bold $ standardContext 24

drawing1 :: CtxPicture Double
drawing1 = drawTracing $ trace1

trace1 :: TraceDrawing Double ()
trace1 = do
    axisPlot (-20,300) (-20,200)
    trapeziumPlot    
    intersectionCross (0.5*pi)
    intersectionPlot (d2r (60::Double)) dark_blue
    intersectionPlot (d2r (45::Double)) olive_drab
    intersectionPlot (d2r (30::Double)) cornflower_blue
    intersectionPlot (d2r (25::Double)) light_salmon
    intersectionPlot (d2r (15::Double)) sienna
    intersectionPlot (d2r ( 5::Double)) thistle
    intersectionCross 0

axisPlot :: (Real u, Floating u, FromPtSize u) 
         => (u,u) -> (u,u) -> TraceDrawing u ()
axisPlot (x0,x1) (y0,y1) = do
    drawi_ $ connect (rightArrow tri45 connLine) (P2 x0 0) (P2 x1 0)
    drawi_ $ connect (rightArrow tri45 connLine) (P2 0 y0) (P2 0 y1)
  

trapezium_height :: Num u => u
trapezium_height = 100

trapezium_top_width :: Num u => u
trapezium_top_width = 150

trapezium_bw :: Num u => u
trapezium_bw = 100

top_rang :: Radian
top_rang = atan (trapezium_height / side)
  where
    side = trapezium_top_width - trapezium_bw

trapeziumPlot :: Num u => TraceDrawing u ()
trapeziumPlot = localize (thick . strokeColour red) $ 
    draw $ closedStroke $ vertexPath [ ll, lr, ur, ul ]
  where
    ll = zeroPt 
    lr = displaceVec (hvec trapezium_bw) ll
    ul = displaceVec (vvec trapezium_height) ll
    ur = displaceVec (hvec trapezium_top_width) ul


intersectionPlot :: (Real u, Floating u, FromPtSize u) 
                 => Radian -> RGBi -> TraceDrawing u ()
intersectionPlot ang rgb = localize (strokeColour rgb) $ do 
    drawi_ $ connect (rightArrow tri45 connLine) p0 p1
    drawi_ $ markCross `at` p3
  where
    p0 = zeroPt 
    p1 = displaceVec (avec ang 200) p0
    v1 = rightTrapezoidQI trapezium_top_width trapezium_height top_rang ang
    p3 = displaceVec v1 zeroPt 

intersectionCross :: (Real u, Floating u, FromPtSize u) 
                 => Radian -> TraceDrawing u ()
intersectionCross ang =  
    drawi_ $ markCross `at` displaceVec v1 zeroPt 
  where
    v1 = rightTrapezoidQI trapezium_top_width trapezium_height top_rang ang
