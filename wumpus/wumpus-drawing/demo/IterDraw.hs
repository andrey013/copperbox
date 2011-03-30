{-# OPTIONS -Wall #-}


module IterDraw where

import Wumpus.Drawing.Colour.SVGColours
import Wumpus.Drawing.Text.StandardFontDefs

import Wumpus.Basic.Kernel                      -- package: wumpus-basic

import Wumpus.Core                              -- package: wumpus-core

import Control.Applicative
import System.Directory


main :: IO ()
main = do 
    createDirectoryIfMissing True "./out/"
    let pic1 = runCtxPictureU dctx iter_drawing
    writeEPS "./out/iter_draw.eps" pic1
    writeSVG "./out/iter_draw.svg" pic1

dctx :: DrawingContext
dctx = set_font courier_bold $ standardContext 24

iter_drawing :: CtxPicture
iter_drawing = drawTracing $ do 
    draw $ (redA `catAdv` greenB `catAdv` blueC) `at` zeroPt

bldisplace :: Num u => PointDisplace u
bldisplace = displace (-4) (-4)

hspace :: Num u => (Vec2 u)
hspace = hvec 28


redA :: AdvGraphic Double
redA = intoAdvGraphic (pure hspace) (background `oplus` plainTextLine "A")
  where
    background = localize (fill_colour tomato) 
                          (moveStart bldisplace $ filledRectangle 24 24)

greenB :: AdvGraphic Double
greenB = intoAdvGraphic (pure hspace) (background `oplus` plainTextLine "B")
  where
    background = localize (fill_colour yellow_green) 
                          (moveStart bldisplace $ filledRectangle 24 24)

blueC :: AdvGraphic Double
blueC = intoAdvGraphic (pure hspace) (background `oplus` plainTextLine "C")
  where
    background = localize (fill_colour light_sky_blue) 
                          (moveStart bldisplace $ filledRectangle 24 24)

