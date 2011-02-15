{-# OPTIONS -Wall #-}

module ColourCharts where

import ColourChartUtils

import Wumpus.Drawing.Chains

import Wumpus.Basic.Kernel                      -- package: wumpus-basic

import Wumpus.Core                              -- package: wumpus-core

import System.Directory


main :: IO ()
main = do 
    createDirectoryIfMissing True "./out/"
    --
    let svg_pic = runCtxPictureU draw_ctx svg 
    writeEPS "./out/SVGcolours.eps" svg_pic
    writeSVG "./out/SVGcolours.svg" svg_pic
    --
    let x11_p = runCtxPictureU draw_ctx x11_portrait
    writeEPS "./out/X11colours.eps" $ uniformScale 0.75 x11_p
    let x11_l = runCtxPictureU draw_ctx x11_landscape
    writeSVG "./out/X11colours.svg" x11_l

draw_ctx :: DrawingContext
draw_ctx = (standardContext 9)

svg :: CtxPicture Double
svg = makeDrawing 52 all_svg_colours

x11_landscape :: CtxPicture Double
x11_landscape = makeDrawing 52 all_x11_colours

x11_portrait :: CtxPicture Double
x11_portrait = makeDrawing 72 all_x11_colours     

makeDrawing :: Int -> [(String,RGBi)] -> DCtxPicture
makeDrawing row_count xs = drawTracing $ tableGraphic row_count xs

tableGraphic :: Int -> [(String,RGBi)] -> TraceDrawing Double ()
tableGraphic row_count xs = 
    draw $ unchainZip emptyLocGraphic gs chn `at` pt
  where
    chn  = tableDown row_count (152,11)
    pt   = displaceV (fromIntegral $ 11 * row_count) zeroPt 
    gs   = map (uncurry colourSample) xs
   

colourSample :: String -> RGBi -> LocGraphic Double
colourSample name rgb = localize (fill_colour rgb) $ 
    promoteR1 $ \pt ->  
      oplus (borderedRectangle 15 10 `at` pt)
            (textline name `at` displace 20 2 pt)
        

