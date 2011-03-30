{-# OPTIONS -Wall #-}


module SingleChar where


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
    writeEPS "./out/single_char.eps" pic1
    writeSVG "./out/single_char.svg" pic1


makeCtx :: FontLoadResult -> DrawingContext
makeCtx = set_font helvetica . metricsContext 18



drawing01 :: CtxPicture
drawing01 = drawTracing $ localize (fill_colour red) $ mf 


-- Note - Baseline positions not meaningful for multiline text

mf :: TraceDrawing Double ()
mf = localize (text_margin 6.0 6.0)  $ do
    draw $ (fn $ posChar 'S' `startAddr` SS) `at` zeroPt
    draw $ redPlus `at` zeroPt

    draw $ (fn $ posChar 'N' `startAddr` NN) `at` P2 40 0
    draw $ redPlus `at` P2 40 0

    draw $ (fn $ posChar 'E' `startAddr` EE) `at` P2 80 0
    draw $ redPlus `at` P2 80 0

    draw $ (fn $ posChar 'W' `startAddr` WW) `at` P2 120 0
    draw $ redPlus `at` P2 120 0

    draw $ (fn $ posChar 'C' `startAddr` CENTER) `at` P2 160 0
    draw $ redPlus `at` P2 160 0

    draw $ (fn $ posChar 'X' `startAddr` NE) `at` P2 200 0
    draw $ redPlus `at` P2 200 0

  where
    fn    = illustrateBoundedLocGraphic


redPlus :: (Fractional u, InterpretUnit u) => LocGraphic u
redPlus = localize (stroke_colour red) markPlus

