{-# OPTIONS -Wall #-}


module NewText where

import Wumpus.Drawing.Colour.SVGColours
import Wumpus.Drawing.Dots.Marks
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
    base_metrics <- loader [helvetica]
    printLoadErrors base_metrics
    let pic1 = runCtxPictureU (makeCtx base_metrics) drawing01
    writeEPS "./out/new_text.eps" pic1
    writeSVG "./out/new_text.svg" pic1

makeCtx :: FontLoadResult -> DrawingContext
makeCtx = set_font helvetica . metricsContext 18



drawing01 :: CtxPicture
drawing01 = drawTracing $ localize (fill_colour red) $ mf 


-- Note - Baseline positions not meaningful for multiline text

mf :: TraceDrawing Double ()
mf = localize text_margin_loose  $ do
    draw $ (fn $ leftAlign body `startAddr` SS) `at` zeroPt
    draw $ redPlus `at` zeroPt

    draw $ (fn $ centerAlign body `startAddr` SS) `at` P2 0 150
    draw $ redPlus `at` P2 0 150

    draw $ (fn $ rightAlign body `startAddr` SS) `at` P2 0 300
    draw $ redPlus `at` P2 0 300
  where
    fn    = illustrateBoundedLocGraphic


redPlus :: (Fractional u, InterpretUnit u) => LocGraphic u
redPlus = localize (stroke_colour red) markPlus


body :: (Ord u, InterpretUnit u) => [Doc u]
body = [ string "Further work"
       , (textSize 48 $ string "on")
           <+> (fontColour red $ string "multiline")
           <+> string "text"
       , ( rfill 50 $ string "and") <> string "other things."
       ] 
