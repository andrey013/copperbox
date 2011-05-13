{-# OPTIONS -Wall #-}

module ColourCharts where

import ColourChartUtils

import Wumpus.Drawing.Basis.DrawingPrimitives

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
    writeEPS "./out/X11colours.eps" $ scale 0.75 0.75 x11_p
    let x11_l = runCtxPictureU draw_ctx x11_landscape
    writeSVG "./out/X11colours.svg" x11_l

draw_ctx :: DrawingContext
draw_ctx = (standardContext 9)

svg :: CtxPicture
svg = makeDrawing 52 all_svg_colours

x11_landscape :: CtxPicture
x11_landscape = makeDrawing 52 all_x11_colours

x11_portrait :: CtxPicture
x11_portrait = makeDrawing 72 all_x11_colours     

makeDrawing :: Int -> [(String,RGBi)] -> CtxPicture
makeDrawing row_count xs = drawTracing $ tableGraphic row_count xs

tableGraphic :: Int -> [(String,RGBi)] -> TraceDrawing Double ()
tableGraphic row_count xs = draw $ (chain_ chn gs) `at` pt
  where
    chn  = tableDown row_count (152,11)
    pt   = dispV (fromIntegral $ 11 * row_count) zeroPt 
    gs   = map (uncurry colourSample) xs
   

colourSample :: String -> RGBi -> LocGraphic Double
colourSample name rgb = localize (fill_colour rgb) $ 
    promoteR1 $ \pt ->  
      oplus (blRectangle FILL_STROKE 15 10 `at` pt)
            (dcTextlabel name `at` displace 20 2 pt)
        

