{-# OPTIONS -Wall #-}


module IterDraw where

import Wumpus.Basic.Kernel hiding ( hspace )
import Wumpus.Drawing.Colour.SVGColours
import Wumpus.Drawing.Text.SafeFonts

import Wumpus.Core                      -- package: wumpus-core

import Control.Applicative
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
    draw $ extr (redA `chain1` greenB `chain1` blueC) `at` zeroPt

extr :: AdvGraphic u -> LocGraphic u
extr = fmap $ \(_,b) -> (uNil, b)
 


bldisplace :: Num u => PointDisplace u
bldisplace = displace (-4) (-4)

hspace :: Num u => PointDisplace u
hspace = displaceH 28

redA :: Fractional u => AdvGraphic u
redA = makeAdvGraphic (pure hspace) (background `oplus` textline "A")
  where
    background = localize (fillColour tomato) 
                          (moveStartPoint bldisplace $ filledRectangle 24 24)

greenB :: Fractional u => AdvGraphic u
greenB = makeAdvGraphic (pure hspace) (background `oplus` textline "B")
  where
    background = localize (fillColour yellow_green) 
                          (moveStartPoint bldisplace $ filledRectangle 24 24)

blueC :: Fractional u => AdvGraphic u
blueC = makeAdvGraphic (pure hspace) (background `oplus` textline "C")
  where
    background = localize (fillColour light_sky_blue) 
                          (moveStartPoint bldisplace $ filledRectangle 24 24)

