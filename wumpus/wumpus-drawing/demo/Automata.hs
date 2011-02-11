{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS -Wall #-}



module Automata where

import Wumpus.Drawing.Arrows
import Wumpus.Drawing.Extras.Loop
import Wumpus.Drawing.Paths
import Wumpus.Drawing.Shapes
import Wumpus.Drawing.Text.SafeFonts
import Wumpus.Drawing.Text.Label
import Wumpus.Drawing.Text.RotTextLR

import FontLoaderUtils

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
makeCtx = 
    snapGrid (60.0::Double) (60.0::Double) 
        . fontFace times_roman . metricsContext 14


automata :: DCtxPicture
automata = drawTracing $ do
    q0     <- nodei (8,0)   $ state "q0"
    q1     <- cxdrawi  (above_right_of q0)      $ state "q1"
    q2     <- cxdrawi  (below_right_of q0)      $ state "q2"
    q3     <- cxdrawi  (below_right_of q1)      $ stopstate "q3"

    s0     <- query (left_of q0)
    
    drawrci_ q0 q1 $ label_midway_of SE (singleLine "0") $ straightconn
    drawci_  (center q1) (north q1) $ label_midway_of SS (singleLine "0") arrloop
    drawrci_ q1 q3 $ label_midway_of SW (singleLine "1") $ straightconn
    drawrci_ q0 q2 $ label_midway_of NE (singleLine "1") $ straightconn 
    drawrci_ q2 q3 $ label_midway_of NW (singleLine "0") $ straightconn
    drawci_  (center q2) (south q2) $ label_midway_of NN (singleLine "1") arrloop
    drawci_  s0 (west q0) $ label_atstart_of EE (singleLine "start") straightconn

    return ()



state :: ( Real u, Floating u, FromPtSize u) 
      => String -> LocImage u (Circle u)
state ss = 
    localize (fontFace times_italic) $ 
        label_center_of (singleLine ss) $ strokedShape $ circle 20


stopstate :: ( Real u, Floating u, FromPtSize u) 
          => String -> LocImage u (Circle u)
stopstate ss = 
    localize (fontFace times_italic) $ 
        label_center_of lbl $ dblStrokedShape $ circle 20
  where
    lbl = singleLine ss 
  


straightconn :: (Real u, Floating u, FromPtSize u) 
             => ConnectorImage u (Path u)
straightconn = rightArrow tri45 connLine

-- Note - there is a problem with @rightArrow@ as @loop@
-- manufactures the start and end points...
--
arrloop :: (Real u, Floating u, FromPtSize u) 
             => ConnectorImage u (Path u)
arrloop = rightArrow barb45 loop







