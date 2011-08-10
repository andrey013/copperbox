{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS -Wall #-}

-- Acknowledgment - the petri net is taken from Claus Reinke\'s
-- paper /Haskell-Coloured Petri Nets/.


module PetriNet where

import Wumpus.Drawing.Colour.SVGColours
import Wumpus.Drawing.Connectors
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
    base_metrics <- loader [ Right helvetica_family ]
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
    drawc (north tu3) (north pw)  $ connectorC 
    drawc (east pr)  (west tl1)   $ straightconn
    drawc (east tl1) (west rtr)   $ straightconn
    drawc (east rtr) (west tl2)   $ straightconn
    drawc (east tl2) (west r)     $ straightconn
    drawc (east r)   (west tl3)   $ straightconn
    drawc (south tl3) (south pr)  $ connectorC'
    drawc (southwest res) (northeast tl2) $ straightconn
    drawc (northwest tl3) (southeast res) $ straightconn
    drawc (southwest tu3) (northeast res) $ connectorD
    drawc (southwest tu3) (northeast res) $ connectorD'
    drawc (northwest res) (southeast tu2) $ connectorD
    drawc (northwest res) (southeast tu2) $ connectorD'
    draw $ lblParensParens `at` (P2 (-36) 150)
    draw $ lblParensParens `at` (P2 300 60)
    draw $ lblParensParensParens `at` (P2 (-52) (-14))
    drawl (projectAnchor south 12 pw)     $ lblBold "processing_w"
    drawl (projectAnchor south 12 rtw)    $ lblBold "ready_to_write"
    drawl (projectAnchor south 12 w)      $ lblBold "writing"
    draw $ lblBold "resource"      `at` (P2 300 72)
    drawl (projectAnchor north 12 pr)     $ lblBold "processing_r"
    drawl (projectAnchor north 12 rtr)    $ lblBold "ready_to_read"
    drawl (projectAnchor north 12 r)      $ lblBold "reading"

    return ()

greenFill :: LocImage u a -> LocImage u a
greenFill = localize (fill_colour lime_green)


place :: DLocImage (Circle Double)
place = greenFill $ borderedShape $ circle 14


transition :: DLocImage (Rectangle Double)
transition = greenFill $ borderedShape $ rectangle 32 22




straightconn :: ConnectorGraphic Double
straightconn = ignoreAns $ rightArrow tri45 connline


connectorC :: ConnectorGraphic Double
connectorC = 
    ignoreAns $ localize (uniform_arm_len  (30::Double)) 
              $ rightArrow tri45 connbbar

connectorC' :: ConnectorGraphic Double
connectorC' = 
    ignoreAns $ localize (uniform_arm_len  (30::Double)) 
                      $ rightArrow tri45 connabar

connectorD :: ConnectorGraphic Double
connectorD = ignoreAns $ rightArrow tri45 connarc

connectorD' :: ConnectorGraphic Double
connectorD' = 
    ignoreAns $ localize (conn_arc_angle $ negate $ pi / 12) 
                      $ rightArrow tri45 connarc


lblParensParens :: DLocGraphic
lblParensParens = 
    ignoreAns $ localize (set_font helvetica) $ textline "(),()" CENTER

lblParensParensParens :: DLocGraphic
lblParensParensParens = 
    ignoreAns $ localize (set_font helvetica) $ textline "(),(),()" CENTER



lblBold :: String -> DLocGraphic
lblBold ss = 
    ignoreAns $ localize (set_font helvetica_bold) $ textline ss CENTER

