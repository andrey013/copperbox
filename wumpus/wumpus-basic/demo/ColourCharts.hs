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
    writeEPS_latin1 "./out/SVGcolours.eps" svg
    writeSVG_latin1 "./out/SVGcolours.svg" svg
    writeEPS_latin1 "./out/X11colours.eps" $ uniformScale 0.75 x11_portrait
    writeSVG_latin1 "./out/X11colours.svg" x11_landscape

svg :: Picture Double
svg = makePicture 52 all_svg_colours

x11_landscape :: Picture Double
x11_landscape = makePicture 52 all_x11_colours

x11_portrait :: Picture Double
x11_portrait = makePicture 72 all_x11_colours     

makePicture :: Int -> [(String,RGBi)] -> DPicture 
makePicture row_count xs = 
    liftToPictureU $ execDrawing (standardContext 9) $ 
        tableGraphic row_count xs

tableGraphic :: Int -> [(String,RGBi)] -> Drawing Double ()
tableGraphic row_count xs = 
    zipWithM_ (\(name,rgb) pt -> colourSample name rgb pt) xs ps
  where
    ps = unchain (coordinateScalingContext 152 11) $ tableDown row_count 10 


colourSample :: String -> RGBi -> DPoint2 -> Drawing Double ()
colourSample name rgb pt = localCtx (fillColour rgb) $ do 
    draw $ borderedRectangle 15 10 `at` pt
    draw $ textline name `at` displace 20 2 pt 
        

