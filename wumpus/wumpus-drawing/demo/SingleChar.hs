{-# OPTIONS -Wall #-}


module SingleChar where


import Wumpus.Drawing.Colour.SVGColours
import Wumpus.Drawing.Dots.SimpleDots
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
    draw $ (fn SS $ posChar 'S') `at` zeroPt
    draw $ redPlus `at` zeroPt

    draw $ (fn NN $ posChar 'N') `at` P2 40 0
    draw $ redPlus `at` P2 40 0

    draw $ (fn EE $ posChar 'E') `at` P2 80 0
    draw $ redPlus `at` P2 80 0

    draw $ (fn WW $ posChar 'W') `at` P2 120 0
    draw $ redPlus `at` P2 120 0

    draw $ (fn CENTER $ posChar 'C') `at` P2 160 0
    draw $ redPlus `at` P2 160 0

    draw $ (fn NE $ posChar 'X') `at` P2 200 0
    draw $ redPlus `at` P2 200 0

  where
    fn addr obj = illustrateBoundedLocGraphic (runPosObjectR2 obj `startAddr` addr)


redPlus :: (Fractional u, InterpretUnit u) => LocGraphic u
redPlus = localize (stroke_colour red) dotPlus

