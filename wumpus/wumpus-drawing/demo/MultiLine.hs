{-# OPTIONS -Wall #-}


module MultiLine where

import FontLoaderUtils

import Wumpus.Drawing.Colour.SVGColours
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
    writeEPS "./out/multi_line01.eps" pic1
    writeSVG "./out/multi_line01.svg" pic1

makeAfmPicture :: FilePath -> IO ()
makeAfmPicture font_dir = do
    putStrLn "Using AFM 4.1 metrics..."
    base_metrics <- loadAfmFontMetrics font_dir ["Helvetica"]
    printLoadErrors base_metrics
    let pic2 = runCtxPictureU (makeCtx base_metrics) drawing01
    writeEPS "./out/multi_line02.eps" pic2
    writeSVG "./out/multi_line02.svg" pic2



makeCtx :: FontLoadResult -> DrawingContext
makeCtx = set_font helvetica . metricsContext 9



drawing01 :: DCtxPicture
drawing01 = drawTracing $ localize (fill_colour red) $ mf 


-- Note - Baseline positions not meaningful for multiline text

mf :: (Real u, Floating u, Ord u, FromPtSize u) => TraceDrawing u ()
mf = do
    draw $ testDrawL NN `at` (P2   0 520)
    draw $ testDrawL SS `at` (P2  75 520)
    draw $ testDrawL EE `at` (P2 150 520)
    draw $ testDrawL WW `at` (P2 225 520)
    draw $ testDrawL NE `at` (P2   0 460)
    draw $ testDrawL SE `at` (P2  75 460)
    draw $ testDrawL SW `at` (P2 150 460)
    draw $ testDrawL NW `at` (P2 225 460)
    draw $ testDrawL CENTER    `at` (P2   0 400)
    --
    draw $ testDrawC NN `at` (P2   0 320)
    draw $ testDrawC SS `at` (P2  75 320)
    draw $ testDrawC EE `at` (P2 150 320)
    draw $ testDrawC WW `at` (P2 225 320)
    draw $ testDrawC NE `at` (P2   0 260)
    draw $ testDrawC SE `at` (P2  75 260)
    draw $ testDrawC SW `at` (P2 150 260)
    draw $ testDrawC NW `at` (P2 225 260)
    draw $ testDrawC CENTER    `at` (P2   0 200)
    --
    draw $ testDrawR NN `at` (P2   0 120)
    draw $ testDrawR SS `at` (P2  75 120)
    draw $ testDrawR EE `at` (P2 150 120)
    draw $ testDrawR WW `at` (P2 225 120)
    draw $ testDrawR NE `at` (P2   0 60)
    draw $ testDrawR SE `at` (P2  75 60)
    draw $ testDrawR SW `at` (P2 150 60)
    draw $ testDrawR NW `at` (P2 225 60)
    draw $ testDrawR CENTER    `at` (P2   0 0)
    

testDrawL :: (Real u, Floating u, Ord u, FromPtSize u) 
          => RectPosition -> LocGraphic u
testDrawL rpos = filledDisk 2 `oplus` (ignoreAns txt)
  where
    txt = illustrateBoundedLocGraphic $ 
            apply2R3 (multiAlignLeft sample_text) rpos 0

testDrawC :: (Real u, Floating u, Ord u, FromPtSize u) 
          => RectPosition -> LocGraphic u
testDrawC rpos = filledDisk 2 `oplus` (ignoreAns txt)
  where
    txt = illustrateBoundedLocGraphic $ 
            apply2R3 (multiAlignCenter sample_text)  rpos 0


testDrawR :: (Real u, Floating u, Ord u, FromPtSize u) 
          => RectPosition -> LocGraphic u
testDrawR rpos = filledDisk 2 `oplus` (ignoreAns txt)
  where
    txt = illustrateBoundedLocGraphic $ 
            apply2R3 (multiAlignRight sample_text) rpos 0

sample_text :: String
sample_text = "Is\nthis\nokay&question;"

