{-# OPTIONS -Wall #-}


module IterDraw where

import Wumpus.Drawing.Colour.SVGColours
import Wumpus.Drawing.Text.SafeFonts

import Wumpus.Basic.Kernel                      -- package: wumpus-basic

import Wumpus.Core                              -- package: wumpus-core

import Control.Applicative
import System.Directory


main :: IO ()
main = do 
    createDirectoryIfMissing True "./out/"
    let pic1 = runCtxPictureU dctx iter_drawing
    writeEPS "./out/iter_draw01.eps" pic1
    writeSVG "./out/iter_draw01.svg" pic1

dctx :: DrawingContext
dctx = set_font courier_bold $ standardContext 24

iter_drawing :: DCtxPicture
iter_drawing = drawTracing $ do 
    drawi_ $ (redA `advcat` greenB `advcat` blueC) `at` zeroPt

bldisplace :: Num u => PointDisplace u
bldisplace = displace (-4) (-4)

hspace :: Num u => (Vec2 u)
hspace = hvec 28

redA :: Fractional u => AdvGraphic u
redA = intoAdvGraphic (pure hspace) (background `oplus` textline "A")
  where
    background = localize (fill_colour tomato) 
                          (moveStart bldisplace $ filledRectangle 24 24)

greenB :: Fractional u => AdvGraphic u
greenB = intoAdvGraphic (pure hspace) (background `oplus` textline "B")
  where
    background = localize (fill_colour yellow_green) 
                          (moveStart bldisplace $ filledRectangle 24 24)

blueC :: Fractional u => AdvGraphic u
blueC = intoAdvGraphic (pure hspace) (background `oplus` textline "C")
  where
    background = localize (fill_colour light_sky_blue) 
                          (moveStart bldisplace $ filledRectangle 24 24)

