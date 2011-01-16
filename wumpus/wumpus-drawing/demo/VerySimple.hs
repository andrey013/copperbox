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
   draw $ bbrectangle bbox1
   draw $ bbrectangle $ centerOrthoBBox (0.25 * pi) bbox1 
  where
    bbox1 = BBox (P2 100 0) (P2 180 30)

   


-- TODO expose this from Wumpus-Basic
bbrectangle :: Fractional u => BoundingBox u -> Graphic u
bbrectangle (BBox p1@(P2 llx lly) p2@(P2 urx ury))
    | llx == urx && lly == ury = emptyLocGraphic `at` p1
    | otherwise                = 
        localize drawing_props $ rect1 `oplus` cross
  where
    drawing_props = capRound . dashPattern (Dash 0 [(1,2)])
    rect1         = strokedRectangle (urx-llx) (ury-lly) `at` p1
    cross         = straightLineBetween p1 p2 
                      `oplus` straightLineBetween (P2 llx ury) (P2 urx lly)

