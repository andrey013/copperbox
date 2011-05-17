{-# OPTIONS -Wall #-}


module IterDraw where

import Wumpus.Drawing.Colour.SVGColours
import Wumpus.Drawing.Text.StandardFontDefs

import Wumpus.Basic.Kernel                      -- package: wumpus-basic

import Wumpus.Core                              -- package: wumpus-core

import Control.Applicative
import Data.Monoid
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
    draw $ (runAdvObject $ redA `advance` greenB `advance` blueC) `at` zeroPt

bldisplace :: Num u => Vec2 u
bldisplace = vec (-4) (-4)

hdist :: Num u => (Vec2 u)
hdist = hvec 28


redA :: AdvObject Double
redA = makeAdvObject (pure hdist) (background `mappend` dcTextlabel "A")
  where
    background = localize (fill_colour tomato) 
                          (moveStart bldisplace $ dcRectangle FILL 24 24)

greenB :: AdvObject Double
greenB = makeAdvObject (pure hdist) (background `mappend` dcTextlabel "B")
  where
    background = localize (fill_colour yellow_green) 
                          (moveStart bldisplace $ dcRectangle FILL 24 24)

blueC :: AdvObject Double
blueC = makeAdvObject (pure hdist) (background `mappend` dcTextlabel "C")
  where
    background = localize (fill_colour light_sky_blue) 
                          (moveStart bldisplace $ dcRectangle FILL 24 24)

