{-# OPTIONS -Wall #-}

module ColourCharts where

import ColourChartUtils

import Wumpus.Basic.Chains
import Wumpus.Basic.Graphic

import Wumpus.Core                              -- package: wumpus-core

import Control.Monad
import System.Directory


main :: IO ()
main = do 
    createDirectoryIfMissing True "./out/"
    --
    let svg_pic = runDrawingU draw_ctx svg 
    writeEPS "./out/SVGcolours.eps" svg_pic
    writeSVG "./out/SVGcolours.svg" svg_pic
    --
    let x11_p = runDrawingU draw_ctx x11_portrait
    writeEPS "./out/X11colours.eps" $ uniformScale 0.75 x11_p
    let x11_l = runDrawingU draw_ctx x11_landscape
    writeSVG "./out/X11colours.svg" x11_l

draw_ctx :: DrawingContext
draw_ctx = (standardContext 9)

svg :: Drawing Double
svg = makeDrawing 52 all_svg_colours

x11_landscape :: Drawing Double
x11_landscape = makeDrawing 52 all_x11_colours

x11_portrait :: Drawing Double
x11_portrait = makeDrawing 72 all_x11_colours     

makeDrawing :: Int -> [(String,RGBi)] -> DDrawing
makeDrawing row_count xs = drawTracing $ tableGraphic row_count xs

tableGraphic :: Int -> [(String,RGBi)] -> TraceDrawing Double ()
tableGraphic row_count xs = 
    zipWithM_ (\(name,rgb) pt -> colourSample name rgb pt) xs ps
  where
    ps = unchain (coordinateScalingContext 152 11) $ tableDown row_count 10 


colourSample :: String -> RGBi -> DPoint2 -> TraceDrawing Double ()
colourSample name rgb pt = localize (fillColour rgb) $ do 
    draw $ borderedRectangle 15 10 `at` pt
    draw $ textline name `at` displace 20 2 pt 
        

