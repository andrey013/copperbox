        {-# OPTIONS -Wall #-}


module MultiLine where

import Wumpus.Drawing.Colour.SVGColours
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
    writeEPS "./out/multi_line.eps" pic1
    writeSVG "./out/multi_line.svg" pic1

makeCtx :: FontLoadResult -> DrawingContext
makeCtx = set_font helvetica . metricsContext 9



drawing01 :: CtxPicture
drawing01 = drawTracing $ localize (fill_colour red) $ mf 


-- Note - Baseline positions not meaningful for multiline text

mf :: TraceDrawing Double ()
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
    

testDrawL :: (Real u, Floating u, Ord u, InterpretUnit u) 
          => RectAddress -> LocGraphic u
testDrawL rpos = filledDisk 2 `oplus` (ignoreAns txt)
  where
    txt = illustrateBoundedLocGraphic $ 
            startAddr (multiAlignLeft sample_text) rpos

testDrawC :: (Real u, Floating u, Ord u, InterpretUnit u) 
          => RectAddress -> LocGraphic u
testDrawC rpos = filledDisk 2 `oplus` (ignoreAns txt)
  where
    txt = illustrateBoundedLocGraphic $ 
            startAddr (multiAlignCenter sample_text)  rpos


testDrawR :: (Real u, Floating u, Ord u, InterpretUnit u) 
          => RectAddress -> LocGraphic u
testDrawR rpos = filledDisk 2 `oplus` (ignoreAns txt)
  where
    txt = illustrateBoundedLocGraphic $ 
            startAddr (multiAlignRight sample_text) rpos

sample_text :: String
sample_text = "Is\nthis\nokay&question;"

