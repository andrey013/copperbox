{-# OPTIONS -Wall #-}


module SingleLine where

import FontLoaderUtils

import Wumpus.Drawing.Colour.SVGColours
import Wumpus.Drawing.Text.LRText
import Wumpus.Drawing.Text.SafeFonts

import Wumpus.Basic.Kernel              -- package: wumpus-basic
import Wumpus.Basic.System.FontLoader.Afm
import Wumpus.Basic.System.FontLoader.GhostScript

import Wumpus.Core                      -- package: wumpus-core

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
    writeEPS "./out/single_line01.eps" pic1
    writeSVG "./out/single_line01.svg" pic1

makeAfmPicture :: FilePath -> IO ()
makeAfmPicture font_dir = do
    putStrLn "Using AFM 4.1 metrics..."
    (afm_metrics, msgs) <- loadAfmMetrics font_dir ["Helvetica"]
    mapM_ putStrLn msgs
    let pic2 = runCtxPictureU (makeCtx afm_metrics) drawing01
    writeEPS "./out/single_line02.eps" pic2
    writeSVG "./out/single_line02.svg" pic2



makeCtx :: GlyphMetrics -> DrawingContext
makeCtx = fontFace helvetica . metricsContext 12



drawing01 :: DCtxPicture
drawing01 = drawTracing $ localize (fillColour red) $ mf 


mf :: (Floating u, FromPtSize u) => TraceDrawing u ()
mf = do
    draw $ testDraw NN `at` (P2   0 200)
    draw $ testDraw SS `at` (P2  75 200)
    draw $ testDraw EE `at` (P2 150 200)
    draw $ testDraw WW `at` (P2 225 200)
    draw $ testDraw NE `at` (P2   0 100)
    draw $ testDraw SE `at` (P2  75 100)
    draw $ testDraw SW `at` (P2 150 100)
    draw $ testDraw NW `at` (P2 225 100)
    draw $ testDraw CENTER    `at` (P2   0 0)
    draw $ testDraw BL_LEFT   `at` (P2  75 0)
    draw $ testDraw BL_CENTER `at` (P2 150 0)
    draw $ testDraw BL_RIGHT  `at` (P2 225 0)
    

testDraw :: (Floating u, FromPtSize u) => RectPosition -> LocGraphic u
testDraw rpos = filledDisk 2 `oplus` ans
  where
    ans = singleLine rpos "Qwerty" `rot`  (pi * 0.25)
