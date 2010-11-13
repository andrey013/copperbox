{-# OPTIONS -Wall #-}

module ColourCharts where

import ColourDefns

import Wumpus.Basic.Chains
import Wumpus.Basic.Graphic

import Wumpus.Core                              -- package: wumpus-core

import Control.Monad
import System.Directory


main :: IO ()
main = do 
    createDirectoryIfMissing True "./out/"
    writeEPS "./out/SVGcolours.eps" svg
    writeSVG "./out/SVGcolours.svg" svg
    writeEPS "./out/X11colours.eps" $ uniformScale 0.75 x11_portrait
    writeSVG "./out/X11colours.svg" x11_landscape

svg :: Picture Double
svg = makePicture 52 all_svg_colours

x11_landscape :: Picture Double
x11_landscape = makePicture 52 all_x11_colours

x11_portrait :: Picture Double
x11_portrait = makePicture 72 all_x11_colours     

makePicture :: Int -> [(String,RGBi)] -> DPicture 
makePicture row_count xs = 
    liftToPictureU $ execTraceDrawing (standardContext 9) $ 
        tableGraphic row_count xs

tableGraphic :: Int -> [(String,RGBi)] -> TraceDrawing Double ()
tableGraphic row_count xs = 
    zipWithM_ (\(name,rgb) pt -> colourSample name rgb pt) xs ps
  where
    ps = unchain (coordinateScalingContext 152 11) $ tableDown row_count 10 


colourSample :: String -> RGBi -> DPoint2 -> TraceDrawing Double ()
colourSample name rgb pt = localize (fillColour rgb) $ do 
    draw $ borderedRectangle 15 10 `at` pt
    draw $ textline name `at` displace 20 2 pt 
        

