{-# OPTIONS -Wall #-}


module NumberText where

import FontLoaderUtils

import Wumpus.Drawing.Colour.SVGColours
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
    base_metrics <- loadGSFontMetrics font_dir ["Times-Roman"]
    printLoadErrors base_metrics
    let pic1 = runCtxPictureU (makeCtx base_metrics) drawing01
    writeEPS "./out/number_text01.eps" pic1
    writeSVG "./out/number_text01.svg" pic1

makeAfmPicture :: FilePath -> IO ()
makeAfmPicture font_dir = do
    putStrLn "Using AFM 4.1 metrics..."
    base_metrics <- loadAfmFontMetrics font_dir ["Times-Roman"]
    printLoadErrors base_metrics
    let pic2 = runCtxPictureU (makeCtx base_metrics) drawing01
    writeEPS "./out/number_text02.eps" pic2
    writeSVG "./out/number_text02.svg" pic2



makeCtx :: FontLoadResult -> DrawingContext
makeCtx = set_font times_roman . metricsContext 24



drawing01 :: DCtxPicture
drawing01 = drawTracing $ localize (fill_colour red) $ mf 


-- Note - Baseline positions not meaningful for multiline text

mf :: (Real u, Floating u, Ord u, PtSize u) => TraceDrawing u ()
mf = do
    drawli_ zeroPt $ rectStart CENTER $ 
                       leftAlign [ string "0.12112"
                                 , string "12113111411115111116"
                                 , string "00000000000000000000" 
                                 , integer 12113111411115111116
                                 , integer 10000000000000000000
                                 ]

rectStart :: Floating  u => RectPosition -> PosImage u a -> LocImage u a
rectStart = flip startPos