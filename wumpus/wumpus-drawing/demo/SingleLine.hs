{-# OPTIONS -Wall #-}


module SingleLine where

import Wumpus.Drawing.Colour.SVGColours
import Wumpus.Drawing.Text.DirectionZero
import Wumpus.Drawing.Text.StandardFontDefs

import Wumpus.Basic.Kernel                      -- package: wumpus-basic
import Wumpus.Basic.System.FontLoader

import Wumpus.Core                              -- package: wumpus-core

import Data.Monoid
import System.Directory


main :: IO ()
main = simpleFontLoader main1 >> return ()

main1 :: FontLoader -> IO ()
main1 loader = do
    createDirectoryIfMissing True "./out/" 
    base_metrics <- loader [ Left helvetica ]
    printLoadErrors base_metrics
    let pic1 = runCtxPictureU (makeCtx base_metrics) drawing01
    writeEPS "./out/single_line.eps" pic1
    writeSVG "./out/single_line.svg" pic1



makeCtx :: FontLoadResult -> DrawingContext
makeCtx = set_font helvetica . metricsContext 12



drawing01 :: CtxPicture
drawing01 = drawTracing $ localize (fill_colour red) $ mf 


mf :: TraceDrawing Double ()
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
    

testDraw :: RectAddress -> LocGraphic Double
testDraw rpos = dcDisk FILL 2 `mappend` (ignoreAns ans)
  where
    ans = textline "Qwerty" rpos



