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
    demo01


demo01 :: IO ()
demo01 = do 
    writeEPS_latin1 "./out/iter_draw01.eps" pic1
    writeSVG_latin1 "./out/iter_draw01.svg" pic1

dctx :: DrawingContext
dctx = fontface courier_bold $ standardContext 24

pic1 :: DPicture
pic1 = liftToPictureU $ execTraceDrawing dctx $ do 
    draw $ extr (redA `catv` greenB `catv` blueC) `at` zeroPt

extr :: IterGraphic u -> LocGraphic u
extr = postpro1 snd
 
infixr 6 `catv`

catv :: IterGraphic u -> IterGraphic u -> IterGraphic u
catv = accumulate1 oplus

bldisplace :: Num u => PointDisplace u
bldisplace = displace (-4) (-4)

hspace :: Num u => PointDisplace u
hspace = hdisplace 28

redA :: Fractional u => IterGraphic u
redA = makeIterGraphic hspace (background `oplus` textline "A")
  where
    background = localize (fillColour tomato) 
                          (prepro1 bldisplace $ filledRectangle 24 24)

greenB :: Fractional u => IterGraphic u
greenB = makeIterGraphic hspace (background `oplus` textline "B")
  where
    background = localize (fillColour yellow_green) 
                          (prepro1 bldisplace $ filledRectangle 24 24)

blueC :: Fractional u => IterGraphic u
blueC = makeIterGraphic hspace (background `oplus` textline "C")
  where
    background = localize (fillColour light_sky_blue) 
                          (prepro1 bldisplace $ filledRectangle 24 24)

