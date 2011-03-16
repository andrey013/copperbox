{-# OPTIONS -Wall #-}


module SingleChar where

import FontLoaderUtils

import Wumpus.Drawing.Colour.SVGColours
import Wumpus.Drawing.Dots.Marks
import Wumpus.Drawing.Text.PosChar
import Wumpus.Drawing.Text.SafeFonts

import Wumpus.Basic.Kernel                      -- package: wumpus-basic
import Wumpus.Basic.System.FontLoader.Afm
import Wumpus.Basic.System.FontLoader.GhostScript

import Wumpus.Core                              -- package: wumpus-core

import System.Directory


main :: IO ()
main = do 
    (mb_gs, mb_afm) <- processCmdLine default_font_loader_help
    createDirectoryIfMissing True "./out/"
    maybe gs_failk  makeGSPicture  $ mb_gs
    maybe afm_failk makeAfmPicture $ mb_afm
  where
    gs_failk  = putStrLn "No GhostScript font path supplied..."
    afm_failk = putStrLn "No AFM v4.1 font path supplied..."


makeGSPicture :: FilePath -> IO ()
makeGSPicture font_dir = do
    putStrLn "Using GhostScript metrics..."
    base_metrics <- loadGSFontMetrics font_dir ["Helvetica"]
    printLoadErrors base_metrics
    let pic1 = runCtxPictureU (makeCtx base_metrics) drawing01
    writeEPS "./out/single_char01.eps" pic1
    writeSVG "./out/single_char01.svg" pic1

makeAfmPicture :: FilePath -> IO ()
makeAfmPicture font_dir = do
    putStrLn "Using AFM 4.1 metrics..."
    base_metrics <- loadAfmFontMetrics font_dir ["Helvetica"]
    printLoadErrors base_metrics
    let pic2 = runCtxPictureU (makeCtx base_metrics) drawing01
    writeEPS "./out/single_char02.eps" pic2
    writeSVG "./out/single_char02.svg" pic2



makeCtx :: FontLoadResult -> DrawingContext
makeCtx = set_font helvetica . metricsContext 18



drawing01 :: CtxPicture
drawing01 = drawTracing $ localize (fill_colour red) $ mf 


-- Note - Baseline positions not meaningful for multiline text

mf :: TraceDrawing Double ()
mf = localize (text_margin 6.0 6.0)  $ do
    drawi_ $ (fn $ posChar 'S' `startPos` SS) `at` zeroPt
    draw   $ redPlus `at` zeroPt

    drawi_ $ (fn $ posChar 'N' `startPos` NN) `at` P2 40 0
    draw   $ redPlus `at` P2 40 0

    drawi_ $ (fn $ posChar 'E' `startPos` EE) `at` P2 80 0
    draw   $ redPlus `at` P2 80 0

    drawi_ $ (fn $ posChar 'W' `startPos` WW) `at` P2 120 0
    draw   $ redPlus `at` P2 120 0

    drawi_ $ (fn $ posChar 'C' `startPos` CENTER) `at` P2 160 0
    draw   $ redPlus `at` P2 160 0

    drawi_ $ (fn $ posChar 'X' `startPos` NE) `at` P2 200 0
    draw   $ redPlus `at` P2 200 0

  where
    fn    = illustrateBoundedLocGraphic


redPlus :: (Fractional u, InterpretUnit u) => LocGraphic u
redPlus = local_ctx (stroke_colour red) markPlus

