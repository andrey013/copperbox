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
    base_metrics <- loadGSFontMetrics font_dir automata_fonts 
    printLoadErrors base_metrics
    let pic1 = runCtxPictureU (makeCtx base_metrics) automata
    writeEPS "./out/automata01.eps" pic1
    writeSVG "./out/automata01.svg" pic1 

makeAfmPicture :: FilePath -> IO ()
makeAfmPicture font_dir = do 
    putStrLn "Using AFM 4.1 metrics..."
    base_metrics <- loadAfmFontMetrics font_dir automata_fonts
    printLoadErrors base_metrics
    let pic1 = runCtxPictureU (makeCtx base_metrics) automata
    writeEPS "./out/automata02.eps" pic1
    writeSVG "./out/automata02.svg" pic1 

automata_fonts :: [FontName]
automata_fonts = map ps_font_name [ times_roman, times_italic ]

makeCtx :: FontLoadResult -> DrawingContext
makeCtx = 
    snap_grid_factors 60.0 60.0 . set_font times_roman . metricsContext 14


automata :: CtxPicture
automata = drawTracing  $ do
    q0     <- nodei (8,0)   $ state "q0"
    q1     <- drawi $ above_right_of q0 `op` state "q1"
    q2     <- drawi $ below_right_of q0 `op` state "q2"
    q3     <- drawi $ below_right_of q1 `op` stopstate "q3"

    let s0 = left_of q0

    drawi_ $ label_midway_of SE (textbox "0") $ straightconn q0 q1
    drawi_ $ label_midway_of SS (textbox "0") $ arrloop (center q1) (north q1) 
    drawi_ $ label_midway_of SW (textbox "1") $ straightconn q1 q3
    drawi_ $ label_midway_of NE (textbox "1") $ straightconn q0 q2
    drawi_ $ label_midway_of NW (textbox "0") $ straightconn q2 q3
    drawi_ $ label_midway_of NN (textbox "1") $ arrloop (center q2) (south q2) 
    drawi_ $ label_atstart_of EE (textbox "start")$ astraightconn s0 (west q0) 

    return ()

infixr 1 `op`

-- Note - need name for this monadic version of @at@.
--
op :: Anchor u -> LocImage t u -> Image t u
op ancr img = ancr >>= \pt -> img `at` pt


state :: String -> DLocImage Circle
state ss = 
    localize (set_font times_italic) $ 
        label_center_of (textbox ss) $ strokedShape $ circle 20


stopstate :: String -> DLocImage Circle 
stopstate ss = 
    localize (set_font times_italic) $ 
        label_center_of lbl $ dblStrokedShape $ circle 20
  where
    lbl = textbox ss 
  


straightconn :: ( Real u, Floating u, InterpretUnit u
                , CenterAnchor t1 u, RadialAnchor  t1 u
                , CenterAnchor t2 u, RadialAnchor  t2 u
                )
             => t1 u -> t2 u -> Image Path u
straightconn a b =
    radialConnectorPoints a b >>= \(p0,p1) -> 
    connect (rightArrow tri45 connLine) p0 p1


astraightconn :: ( Real u, Floating u, InterpretUnit u)
              => Anchor u -> Anchor u -> Image Path u
astraightconn a b =
    a >>= \p0 -> b >>= \p1 -> connect (rightArrow tri45 connLine) p0 p1


-- Note - there is a problem with @rightArrow@ as @loop@
-- manufactures the start and end points...
--
arrloop :: ( Real u, Floating u, InterpretUnit u, LengthTolerance u) 
        => Anchor u -> Anchor u -> Image Path u
arrloop a b =
    a >>= \p0 -> b >>= \p1 -> connect (rightArrow barb45 loop) p0 p1







