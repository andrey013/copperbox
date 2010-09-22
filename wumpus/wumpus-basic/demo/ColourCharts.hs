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
svg = makePicture 60 all_svg_colours

x11_landscape :: Picture Double
x11_landscape = makePicture 60 all_x11_colours

x11_portrait :: Picture Double
x11_portrait = makePicture 72 all_x11_colours     

makePicture :: Int -> [(String,RGBi)] -> DPicture 
makePicture row_count xs = 
    liftToPictureU $ execDrawing (standardContext 10) $ 
        tableGraphic row_count xs

tableGraphic :: Int -> [(String,RGBi)] -> Drawing Double ()
tableGraphic row_count xs = 
    zipWithM_ (\(name,rgb) pt -> colourSample name rgb pt) xs ps
  where
    ps = unchain $ tableDown row_count 10 160 13

-- Note - cannot use node twice as it increments the point supply.
--
colourSample :: String -> RGBi -> DPoint2 -> Drawing Double ()
colourSample name rgb pt = localCtx (secondaryColour rgb) $ do 
    draw $ borderedRectangle 15 10 `at` pt
    draw $ textline name `at` displace 20 2 pt 
        


{-

downLeftRight :: (Monad m, Num u, Ord u) 
              => Int -> u -> ChainT u m a -> m a
downLeftRight row_count width ma = runChainT fn start_pt ma
  where
    y_top       = 12 * fromIntegral row_count
    start_pt    = P2 0 y_top
    
    fn (P2 x y) | y < 0     = P2 (x+width) y_top
                | otherwise = P2 x (y - 12)   

-}