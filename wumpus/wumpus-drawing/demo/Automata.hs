{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS -Wall #-}



module Automata where

import Wumpus.Drawing.Arrows
import Wumpus.Drawing.Colour.SVGColours
import Wumpus.Drawing.Extras.Grids
import Wumpus.Drawing.Paths
import Wumpus.Drawing.Shapes
import Wumpus.Drawing.Text.SafeFonts
import Wumpus.Drawing.Text.LRText

import FontLoaderUtils

import Wumpus.Basic.Kernel                      -- package: wumpus-basic
import Wumpus.Basic.System.FontLoader.Afm
import Wumpus.Basic.System.FontLoader.GhostScript

import Wumpus.Core                              -- package: wumpus-core

import Data.AffineSpace                         -- package: vector-space

import Control.Applicative
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
    (base_metrics, msgs) <- loadGSMetrics font_dir automata_fonts 
    mapM_ putStrLn msgs
    let pic1 = runCtxPictureU (makeCtx base_metrics) automata
    writeEPS "./out/automata01.eps" pic1
    writeSVG "./out/automata01.svg" pic1 

makeAfmPicture :: FilePath -> IO ()
makeAfmPicture font_dir = do 
    putStrLn "Using AFM 4.1 metrics..."
    (base_metrics, msgs) <- loadAfmMetrics font_dir automata_fonts
    mapM_ putStrLn msgs
    let pic1 = runCtxPictureU (makeCtx base_metrics) automata
    writeEPS "./out/automata02.eps" pic1
    writeSVG "./out/automata02.svg" pic1 

automata_fonts :: [FontName]
automata_fonts = map ps_font_name [ times_roman, times_italic ]

makeCtx :: GlyphMetrics -> DrawingContext
makeCtx = snapGrid 60.0 60.0 . fontFace helvetica . metricsContext 14


automata :: DCtxPicture
automata = drawTracing $ do
    draw $ grid (3,2) slate_gray `at` zeroPt
    q0     <- nodei (0,0)   $ state
    q1     <- cxdrawi  (above_right_of q0)      $ state
    q2     <- cxdrawi  (below_right_of q0)      $ state
    q3     <- cxdrawi  (below_right_of q1)      $ stopstate

    s0     <- query (left_of q0)
    
    drawrc q0 q1 $ straightconn
    drawrc q1 q3 $ straightconn
    drawrc q0 q2 $ straightconn
    drawrc q2 q3 $ straightconn
    drawc  s0 (west q0) $ straightconn

    return ()


state :: ( Real u, Floating u, FromPtSize u) 
      => LocImage u (Circle u)
state = strokedShape $ circle 20


stopstate :: ( Real u, Floating u, FromPtSize u) 
          => LocImage u (Circle u)
stopstate = dblStrokedShape $ circle 20


straightconn :: (Real u, Floating u, FromPtSize u) 
             => ConnectorGraphic u
straightconn = ignoreAns $ rightArrow tri45 connLine













