{-# OPTIONS -Wall #-}


module NewText where

import FontLoaderUtils

import Wumpus.Drawing.Colour.SVGColours
import Wumpus.Drawing.Dots.Marks
import Wumpus.Drawing.Text.DocTextLR
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
    writeEPS "./out/new_text01.eps" pic1
    writeSVG "./out/new_text01.svg" pic1

makeAfmPicture :: FilePath -> IO ()
makeAfmPicture font_dir = do
    putStrLn "Using AFM 4.1 metrics..."
    base_metrics <- loadAfmFontMetrics font_dir ["Helvetica"]
    printLoadErrors base_metrics
    let pic2 = runCtxPictureU (makeCtx base_metrics) drawing01
    writeEPS "./out/new_text02.eps" pic2
    writeSVG "./out/new_text02.svg" pic2



makeCtx :: FontLoadResult -> DrawingContext
makeCtx = set_font helvetica . metricsContext 18



drawing01 :: CtxPicture
drawing01 = drawTracing UDouble $ localize (fill_colour red) $ mf 


-- Note - Baseline positions not meaningful for multiline text

mf :: TraceDrawing Double ()
mf = localize (text_margin 6.0 6.0)  $ do
    drawi_ $ (fn $ leftAlign body `startPos` SS) `at` zeroPt
    draw   $ redPlus `at` zeroPt

    drawi_ $ (fn $ centerAlign body `startPos` SS) `at` P2 0 150
    draw   $ redPlus `at` P2 0 150

    drawi_ $ (fn $ rightAlign body `startPos` SS) `at` P2 0 300
    draw   $ redPlus `at` P2 0 300
  where
    fn    = illustrateBoundedLocGraphic


redPlus :: (Fractional u, PtSize u) => LocGraphic u
redPlus = localize (stroke_colour red) markPlus


body :: (Ord u, PtSize u) => [DocText u]
body = [ string "Further work"
       , (textSize 36 $ string "on")
           <+> (fontColour blue $ string "multiline")
           <+> string "text"
       , (rfill 50 $ string "and") <> string "other things."
       ] 
