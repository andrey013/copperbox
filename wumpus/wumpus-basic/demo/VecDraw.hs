{-# LANGUAGE FlexibleContexts           #-}
{-# OPTIONS -Wall #-}

module VecDraw where

import Wumpus.Basic.Colour.SVGColours
import Wumpus.Basic.Graphic
import Wumpus.Basic.SafeFonts

import Wumpus.Core                      -- package: wumpus-core

import Control.Applicative
import System.Directory


main :: IO ()
main = do 
    createDirectoryIfMissing True "./out/"
    demo01


demo01 :: IO ()
demo01 = do 
    writeEPS_latin1 "./out/vec_draw01.eps" pic1
    writeSVG_latin1 "./out/vec_draw01.svg" pic1

dctx :: DrawingContext
dctx = fontface courier_bold $ standardContext 24

pic1 :: DPicture
pic1 = liftToPictureU $ execDrawing dctx $ do 
    draw $ dv (redA `catv` greenB `catv` blueC) `at` zeroPt

dv :: VecGraphic u -> LocGraphic u
dv g = liftA snd . g
 
infixr 6 `catv`

catv :: VecGraphic u -> VecGraphic u -> VecGraphic u
catv f g = \pt -> f pt >>= \(p1,a) ->
                  g p1 >>= \(p2,b) ->  
                  return (p2, a `oplus` b)

redA :: Fractional u => VecGraphic u
redA = intoVecGraphic (hdisplace 24) (background `oplus` textline "A")
  where
    background = localize (fillColour tomato) . filledRectangle 24 24 . displace (-4) (-4)

greenB :: Fractional u => VecGraphic u
greenB = intoVecGraphic (hdisplace 24) (background `oplus` textline "B")
  where
    background = localize (fillColour yellow_green) 
                   . filledRectangle 24 24 . displace (-4) (-4)

blueC :: Fractional u => VecGraphic u
blueC = intoVecGraphic (hdisplace 24) (background `oplus` textline "C")
  where
    background = localize (fillColour light_sky_blue) 
                   . filledRectangle 24 24 . displace (-4) (-4)

