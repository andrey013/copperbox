{-# OPTIONS -Wall #-}

module Connectors where


import Wumpus.Drawing.Arrows
import Wumpus.Drawing.Chains
import Wumpus.Drawing.Colour.SVGColours
import Wumpus.Drawing.Connectors.BoxConnectors
import Wumpus.Drawing.Connectors.ConnectorPaths
import Wumpus.Drawing.Paths hiding ( length )
import Wumpus.Drawing.Text.RotTextLR
import Wumpus.Drawing.Text.SafeFonts

import Wumpus.Basic.Kernel                      -- package: wumpus-basic
import Wumpus.Basic.System.FontLoader.Afm
import Wumpus.Basic.System.FontLoader.GhostScript

import Wumpus.Core                              -- package: wumpus-core

import FontLoaderUtils


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
    let pic1 = runCtxPictureU (makeCtx base_metrics) conn_pic
    writeEPS "./out/connectors01.eps" pic1
    writeSVG "./out/connectors01.svg" pic1 

makeAfmPicture :: FilePath -> IO ()
makeAfmPicture font_dir = do 
    putStrLn "Using AFM 4.1 metrics..."
    base_metrics <- loadAfmFontMetrics font_dir ["Helvetica"]
    printLoadErrors base_metrics
    let pic1 = runCtxPictureU (makeCtx base_metrics) conn_pic
    writeEPS "./out/connectors02.eps" pic1
    writeSVG "./out/connectors02.svg" pic1 

makeCtx :: FontLoadResult -> DrawingContext
makeCtx = set_font helvetica . metricsContext 11

conn_pic :: CtxPicture 
conn_pic = drawTracing $ localize (dest_arm_len (0.75::Em))
                       $ tableGraphic conntable

conntable :: [(String, PathQuery Double)]
conntable = 
    [ ("connline",      connline)
    , ("connarc",       connarc)
    , ("connhdiagh",    connhdiagh)
    , ("connvdiagv",    connvdiagv)
    , ("conndiagh",     conndiagh)
    , ("conndiagv",     conndiagv)
    , ("connhdiag",     connhdiag)
    , ("connvdiag",     connvdiag)
    , ("connabar",      connabar)
    , ("connbbar",      connbbar)
    , ("connaright",    connaright)
    , ("connbright",    connbright)
    , ("connhrr",       connhrr)
    , ("connrrh",       connrrh)
    , ("connvrr",       connvrr)
    , ("connrrv",       connrrv)
    , ("connaloop",     connaloop)
    , ("connbloop",     connbloop)
    , ("connhbezier",   connhbezier)
    , ("connvbezier",   connvbezier)
    ]

tableGraphic :: [(String, PathQuery Double)] -> TraceDrawing Double ()
tableGraphic conns = 
    draw $ chn (map makeConnDrawing conns) `at` start
  where
    chn   = tableDown 8 (180,64) 
    start = P2 0 520 

 
std_ctx :: DrawingContext
std_ctx = fill_colour peru $ standardContext 18



makeConnDrawing :: (String, PathQuery Double) -> DLocGraphic 
makeConnDrawing (ss,conn) = 
    promoteR1 $ \p0 -> fn p0 (displace 60 40 p0) 
  where
    fn p0 p1   = disk p0 `oplus` disk p1 `oplus` dcon p0 p1 `oplus` lbl p1

    disk pt    = localize (fill_colour red) $ filledDisk 2 `at` pt
    dcon p0 p1 = ignoreAns $ connect (uniformArrow curveTip conn) p0 p1

    lbl  pt    = ignoreAns $ atStartPos (textbox ss) (displaceH 10 pt) WW


