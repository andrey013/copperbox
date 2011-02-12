{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS -Wall #-}


module LabelledCircle where


import FontLoaderUtils

import Wumpus.Drawing.Colour.SVGColours
import Wumpus.Drawing.Shapes
import Wumpus.Drawing.Text.Label
import Wumpus.Drawing.Text.RotTextLR
import Wumpus.Drawing.Text.SafeFonts

import Wumpus.Basic.Kernel                      -- package: wumpus-basic
import Wumpus.Basic.System.FontLoader.Afm
import Wumpus.Basic.System.FontLoader.GhostScript


import Wumpus.Core                              -- package: wumpus-core

import System.Directory


main :: IO ()
main = do 
    (mb_gs, mb_afm) <- processCmdLine default_font_loader_help
    createDirectoryIfMissing True "./out/shapes/"
    maybe gs_failk  makeGSPicture   $ mb_gs
    maybe afm_failk makeAfmPicture  $ mb_afm
  where
    gs_failk  = putStrLn "No GhostScript font path supplied..."
    afm_failk = putStrLn "No AFM v4.1 font path supplied..."


makeGSPicture :: FilePath -> IO ()
makeGSPicture font_dir = do
    putStrLn "Using GhostScript metrics..."
    base_metrics <- loadGSFontMetrics font_dir ["Helvetica"]
    printLoadErrors base_metrics
    let pic1 = runCtxPictureU (makeCtx base_metrics) $ circle_pic
    writeEPS "./out/labelled_circle01.eps" pic1
    writeSVG "./out/labelled_circle01.svg" pic1


makeAfmPicture :: FilePath -> IO ()
makeAfmPicture font_dir = do
    putStrLn "Using AFM 4.1 metrics..."
    base_metrics <- loadAfmFontMetrics font_dir ["Helvetica"]
    printLoadErrors base_metrics
    let pic1 = runCtxPictureU (makeCtx base_metrics) $ circle_pic
    writeEPS ("./out/labelled_circle02.eps") pic1
    writeSVG ("./out/labelled_circle02.svg") pic1


makeCtx :: FontLoadResult -> DrawingContext
makeCtx = fontFace helvetica . metricsContext 16



circle_pic :: CtxPicture Double
circle_pic = drawTracing $ circle_drawing


circle_drawing :: TraceDrawing Double ()
circle_drawing = do
    a <- drawi $ localize (strokeFill maroon moccasin) $
          (f1 $ f2 $ f3 $ f4 $ borderedShape $ circle 100) `at` P2 200 200
    draw $ filledDisk 3 `at` southeast a
    return ()
  where
    f1 = label_above (singleLine "label-above")
    f2 = label_below (singleLine "label-below")
    f3 = label_left_of  (singleLine "label-left")
    f4 = label_right_of (singleLine "label-right")

strokeFill :: RGBi -> RGBi -> DrawingContextF
strokeFill s f = strokeColour s . fillColour f


-- Note - maybe LRText should use margins afterall...
