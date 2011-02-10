{-# OPTIONS -Wall #-}


module NewText where

import FontLoaderUtils

import Wumpus.Drawing.Colour.SVGColours
import Wumpus.Drawing.Dots.Marks
import Wumpus.Drawing.Text.CatText
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
    (gs_metrics, msgs) <- loadGSMetrics font_dir ["Helvetica"]
    mapM_ putStrLn msgs
    let pic1 = runCtxPictureU (makeCtx gs_metrics) drawing01
    writeEPS "./out/new_text01.eps" pic1
    writeSVG "./out/new_text01.svg" pic1

makeAfmPicture :: FilePath -> IO ()
makeAfmPicture font_dir = do
    putStrLn "Using AFM 4.1 metrics..."
    (afm_metrics, msgs) <- loadAfmMetrics font_dir ["Helvetica"]
    mapM_ putStrLn msgs
    let pic2 = runCtxPictureU (makeCtx afm_metrics) drawing01
    writeEPS "./out/new_text02.eps" pic2
    writeSVG "./out/new_text02.svg" pic2



makeCtx :: GlyphMetrics -> DrawingContext
makeCtx = fontFace helvetica . metricsContext 18



drawing01 :: DCtxPicture
drawing01 = drawTracing $ localize (fillColour red) $ mf 


-- Note - Baseline positions not meaningful for multiline text

mf :: (Real u, Floating u, Ord u, FromPtSize u) => TraceDrawing u ()
mf = localize (textMargin 6.0 6.0)  $ do
    drawi_ $ (fn $ leftAlign body `startPos` SS) `at` zeroPt
    draw   $ redPlus `at` zeroPt

    drawi_ $ (fn $ centerAlign body `startPos` SS) `at` P2 150 0
    draw   $ redPlus `at` P2 150 0

    drawi_ $ (fn $ rightAlign body `startPos` SS) `at` P2 300 0
    draw   $ redPlus `at` P2 300 0
  where
    fn    = illustrateBoundedLocGraphic


redPlus :: (Fractional u, FromPtSize u) => LocGraphic u
redPlus = localize (strokeColour red) markPlus


body :: FromPtSize u => [CatText u]
body = [ string "Further work"
       , string "on" <+> (fontColour blue $ string "multiline")
       , string "text."
       ] 
