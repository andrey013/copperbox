{-# OPTIONS -Wall #-}


module NumberText where

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
    base_metrics <- loader [ Right times_roman_family ]
    printLoadErrors base_metrics
    let pic1 = runCtxPictureU (makeCtx base_metrics) drawing01
    writeEPS "./out/number_text.eps" pic1
    writeSVG "./out/number_text.svg" pic1


makeCtx :: FontLoadResult -> DrawingContext
makeCtx = set_font times_roman . metricsContext 24



drawing01 :: CtxPicture
drawing01 = drawTracing $ localize (fill_colour red) $ mf 


-- Note - Baseline positions not meaningful for multiline text

mf :: TraceDrawing Double ()
mf = do
    drawl zeroPt $ render times_roman_family doc CENTER
    return ()
  where
    doc = leftAlign [ float  (0.12112 :: Double)
                    , string "0.12112"
                    , string "12113111411115111116"
                    , string "00000000000000000000" 
                    , integer 12113111411115111116
                    , integer 10000000000000000000
                    ]

dZero :: Double
dZero = 0

