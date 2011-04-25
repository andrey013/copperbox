{-# OPTIONS -Wall #-}


module LabelledCircle where

import Wumpus.Drawing.Colour.SVGColours
import Wumpus.Drawing.Shapes
import Wumpus.Drawing.Text.Base.Label
import Wumpus.Drawing.Text.DirectionZero
import Wumpus.Drawing.Text.StandardFontDefs

import Wumpus.Basic.Kernel                      -- package: wumpus-basic
import Wumpus.Basic.System.FontLoader


import Wumpus.Core                              -- package: wumpus-core

import System.Directory

main :: IO ()
main = simpleFontLoader main1 >> return ()

main1 :: FontLoader -> IO ()
main1 loader = do
    createDirectoryIfMissing True "./out/" 
    base_metrics <- loader [ Left helvetica ]
    printLoadErrors base_metrics
    let pic1 = runCtxPictureU (makeCtx base_metrics) $ circle_pic
    writeEPS "./out/labelled_circle.eps" pic1
    writeSVG "./out/labelled_circle.svg" pic1


makeCtx :: FontLoadResult -> DrawingContext
makeCtx = set_font helvetica . metricsContext 16



circle_pic :: CtxPicture
circle_pic = drawTracing $ circle_drawing


circle_drawing :: TraceDrawing Double ()
circle_drawing = do
    a <- drawi $ localize (strokeFill maroon moccasin) $
          (f1 $ f2 $ f3 $ f4 $ borderedShape $ circle 100) `at` P2 200 200
    draw $ dcDisk FILL 3 `at` southeast a
    return ()
  where
    f1 = label_above (textline "label-above")
    f2 = label_below (textline "label-below")
    f3 = label_left_of  (textline "label-left")
    f4 = label_right_of (textline "label-right")

strokeFill :: RGBi -> RGBi -> DrawingContextF
strokeFill s f = stroke_colour s . fill_colour f

