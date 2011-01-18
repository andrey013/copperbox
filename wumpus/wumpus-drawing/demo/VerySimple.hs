{-# LANGUAGE FlexibleContexts           #-}
{-# OPTIONS -Wall #-}

module VerySimple where

import Wumpus.Basic.Kernel
import Wumpus.Drawing.Text.SafeFonts

import Wumpus.Core                      -- package: wumpus-core

import System.Directory



main :: IO ()
main = do 
    createDirectoryIfMissing True "./out/" 
    let pic1 = runCtxPictureU dctx $ drawing1
    writeEPS "./out/very_simple01.eps" pic1
    writeSVG "./out/very_simple01.svg" pic1



dctx :: DrawingContext
dctx = fontFace courier_bold $ standardContext 24

drawing1 :: CtxPicture Double
drawing1 = drawTracing $ trace1

trace1 :: TraceDrawing Double ()
trace1 = do
    draw $ ( rotate (0.5*pi) $ textline "hello world") `at` zeroPt
    draw $ rect1
    draw $ rotateAbout (0.25*pi) (P2 140 20) $ rect1
    draw $ bbrectangle bbox1
    draw $ bbrectangle $ centerOrthoBBox (0.25 * pi) bbox1 
  where
    bbox1 = BBox (P2 100 10) (P2 180 30)
    rect1 = strokedRectangle 80 20 `at` P2 100 10
    
   


