{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS -Wall #-}

-- Acknowledgment - the petri net is taken from Claus Reinke\'s
-- paper /Haskell-Coloured Petri Nets/.


module PetriNet where

import Wumpus.Drawing.Arrows
import Wumpus.Drawing.Colour.SVGColours
import Wumpus.Drawing.Paths
import Wumpus.Drawing.Shapes
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
    base_metrics <- loader [helvetica, helvetica_bold]
    printLoadErrors base_metrics
    let pic1 = runCtxPictureU (makeCtx base_metrics) petri_net
    writeEPS "./out/petri_net.eps" pic1
    writeSVG "./out/petri_net.svg" pic1 

makeCtx :: FontLoadResult -> DrawingContext
makeCtx = set_font helvetica . metricsContext 14


petri_net :: CtxPicture
petri_net = udrawTracing (0::Double) $ do
    pw     <- drawi $ place         `at` (P2 0 140)
    tu1    <- drawi $ transition    `at` (P2 70 140)  
    rtw    <- drawi $ place         `at` (P2 140 140)
    tu2    <- drawi $ transition    `at` (P2 210 140)
    w      <- drawi $ place         `at` (P2 280 140)
    tu3    <- drawi $ transition    `at` (P2 350 140)
    res    <- drawi $ place         `at` (P2 280 70)
    pr     <- drawi $ place         `at` (P2 0 0)
    tl1    <- drawi $ transition    `at` (P2 70 0)
    rtr    <- drawi $ place         `at` (P2 140 0)
    tl2    <- drawi $ transition    `at` (P2 210 0)
    r      <- drawi $ place         `at` (P2 280 0)
    tl3    <- drawi $ transition    `at` (P2 350 0)

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
    drawl (projectAnchor south 12 pw)     $ lblBold "processing_w"
    drawl (projectAnchor south 12 rtw)    $ lblBold "ready_to_write"
    drawl (projectAnchor south 12 w)      $ lblBold "writing"
    draw $ lblBold' "resource"      `at` (P2 300 72)
    drawl (projectAnchor north 12 pr)     $ lblBold "processing_r"
    drawl (projectAnchor north 12 rtr)    $ lblBold "ready_to_read"
    drawl (projectAnchor north 12 r)      $ lblBold "reading"

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

