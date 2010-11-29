{-# OPTIONS -Wall #-}


module IterDraw where

import Wumpus.Basic.Colour.SVGColours
import Wumpus.Basic.Graphic
import Wumpus.Basic.SafeFonts

import Wumpus.Core                      -- package: wumpus-core

import System.Directory


main :: IO ()
main = do 
    createDirectoryIfMissing True "./out/"
    let pic1 = runDrawingU dctx iter_drawing
    writeEPS "./out/iter_draw01.eps" pic1
    writeSVG "./out/iter_draw01.svg" pic1

dctx :: DrawingContext
dctx = fontFace courier_bold $ standardContext 24

iter_drawing :: DDrawing
iter_drawing = drawTracing $ do 
    draw $ extr (redA `catv` greenB `catv` blueC) `at` zeroPt

extr :: AdvGraphic u -> LocGraphic u
extr = postpro1 snd
 
infixr 6 `catv`

catv :: AdvGraphic u -> AdvGraphic u -> AdvGraphic u
catv = accumulate1 oplus

bldisplace :: Num u => PointDisplace u
bldisplace = displace (-4) (-4)

hspace :: Num u => PointDisplace u
hspace = hdisplace 28

redA :: Fractional u => AdvGraphic u
redA = makeAdvGraphic hspace (background `oplus` textline "A")
  where
    background = localize (fillColour tomato) 
                          (prepro1 bldisplace $ filledRectangle 24 24)

greenB :: Fractional u => AdvGraphic u
greenB = makeAdvGraphic hspace (background `oplus` textline "B")
  where
    background = localize (fillColour yellow_green) 
                          (prepro1 bldisplace $ filledRectangle 24 24)

blueC :: Fractional u => AdvGraphic u
blueC = makeAdvGraphic hspace (background `oplus` textline "C")
  where
    background = localize (fillColour light_sky_blue) 
                          (prepro1 bldisplace $ filledRectangle 24 24)

