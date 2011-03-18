{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS -Wall #-}

-- Acknowledgment - the petri net is taken from Claus Reinke\'s
-- paper /Haskell-Coloured Petri Nets/.


module PetriNet where

import Wumpus.Drawing.Arrows
import Wumpus.Drawing.Colour.SVGColours
import Wumpus.Drawing.Paths
import Wumpus.Drawing.Shapes
import Wumpus.Drawing.Text.RotTextLR
import Wumpus.Drawing.Text.SafeFonts

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
    base_metrics <- loadGSFontMetrics font_dir ["Helvetica", "Helvetica-Bold"]
    printLoadErrors base_metrics
    let pic1 = runCtxPictureU (makeCtx base_metrics) petri_net
    writeEPS "./out/petri_net01.eps" pic1
    writeSVG "./out/petri_net01.svg" pic1 

makeAfmPicture :: FilePath -> IO ()
makeAfmPicture font_dir = do 
    putStrLn "Using AFM 4.1 metrics..."
    base_metrics <- loadAfmFontMetrics font_dir ["Helvetica", "Helvetica-Bold"]
    printLoadErrors base_metrics
    let pic1 = runCtxPictureU (makeCtx base_metrics) petri_net
    writeEPS "./out/petri_net02.eps" pic1
    writeSVG "./out/petri_net02.svg" pic1 


makeCtx :: FontLoadResult -> DrawingContext
makeCtx = set_font helvetica . metricsContext 14


petri_net :: CtxPicture
petri_net = udrawTracing (0::Double) $ do
    pw     <- drawli (P2 0 140)   $ place
    tu1    <- drawli (P2 70 140)  $ transition
    rtw    <- drawli (P2 140 140) $ place
    tu2    <- drawli (P2 210 140) $ transition
    w      <- drawli (P2 280 140) $ place
    tu3    <- drawli (P2 350 140) $ transition
    res    <- drawli (P2 280 70)  $ place
    pr     <- drawli (P2 0 0)     $ place
    tl1    <- drawli (P2 70 0)    $ transition
    rtr    <- drawli (P2 140 0)   $ place
    tl2    <- drawli (P2 210 0)   $ transition
    r      <- drawli (P2 280 0)   $ place
    tl3    <- drawli (P2 350 0)   $ transition
{-
    drawc (east pw)  (west tu1)   $ straightconn
    drawc (east tu1) (west rtw)   $ straightconn
    drawc (east rtw) (west tu2)   $ straightconn
    drawc (east tu2) (west w)     $ straightconn
    drawc (east w)   (west tu3)   $ straightconn
    drawc (north tu3) (north pw)  $ connectorC 32 
    drawc (east pr)  (west tl1)   $ straightconn
    drawc (east tl1) (west rtr)   $ straightconn
    drawc (east rtr) (west tl2)   $ straightconn
    drawc (east tl2) (west r)     $ straightconn
    drawc (east r)   (west tl3)   $ straightconn
    drawc (south tl3) (south pr)  $ connectorC (-32)
    drawc (southwest res) (northeast tl2) $ straightconn
    drawc (northwest tl3) (southeast res) $ straightconn
    drawc (southwest tu3) (northeast res) $ connectorD 6
    drawc (southwest tu3) (northeast res) $ connectorD (-6)
    drawc (northwest res) (southeast tu2) $ connectorD 6
    drawc (northwest res) (southeast tu2) $ connectorD (-6)
    draw $ lblParensParens `at` (P2 (-36) 150)
    draw $ lblParensParens `at` (P2 300 60)
    draw $ lblParensParensParens `at` (P2 (-52) (-14))
    draw $ lblBold "processing_w"   `at` (projectAnchor south 12 pw)
    draw $ lblBold "ready_to_write" `at` (projectAnchor south 12 rtw)
    draw $ lblBold "writing"        `at` (projectAnchor south 12 w)
    draw $ lblBold' "resource"      `at` (P2 300 72)
    draw $ lblBold "processing_r"   `at` (projectAnchor north 12 pr)
    draw $ lblBold "ready_to_read"  `at` (projectAnchor north 12 rtr)
    draw $ lblBold "reading"        `at` (projectAnchor north 12 r)
-}
    return ()

greenFill :: LocImage t u -> LocImage t u
greenFill = localize (fill_colour lime_green)


place :: DLocImage Circle
place = greenFill $ borderedShape $ circle 14


transition :: DLocImage Rectangle 
transition = greenFill $ borderedShape $ rectangle 32 22




straightconn :: ConnectorGraphic Double
straightconn = ignoreAns $ rightArrow tri45 connLine


connectorC :: Double -> ConnectorGraphic Double
connectorC v = ignoreAns $ rightArrow tri45 (connRightVHV v)

connectorD :: Double -> ConnectorGraphic Double
connectorD u = ignoreAns $ rightArrow tri45 (connIsosceles u)


lblParensParens :: DLocGraphic
lblParensParens = localize (set_font helvetica) $ textline "(),()"

lblParensParensParens :: DLocGraphic
lblParensParensParens = localize (set_font helvetica) $ textline "(),(),()"


lblBold' :: String -> DLocGraphic
lblBold' ss = localize (set_font helvetica_bold) $ textline ss


lblBold :: String -> DLocGraphic
lblBold ss = localize (set_font helvetica_bold) $ 
                ignoreAns $ textAlignCenter ss

