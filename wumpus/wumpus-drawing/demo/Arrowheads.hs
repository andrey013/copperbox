{-# OPTIONS -Wall #-}

module Arrowheads where

import Wumpus.Drawing.Colour.SVGColours
import Wumpus.Drawing.Connectors
import qualified Wumpus.Drawing.Connectors.ConnectorPaths as C
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
    let pic1 = runCtxPictureU (makeCtx base_metrics) arrow_drawing
    writeEPS "./out/arrowheads.eps" pic1
    writeSVG "./out/arrowheads.svg" pic1


makeCtx :: FontLoadResult -> DrawingContext
makeCtx = set_font helvetica . metricsContext 14



arrow_drawing :: CtxPicture
arrow_drawing = 
    drawTracing $ localize dotted_line $ tableGraphic arrtable

arrtable :: [(String, ArrowTip)]
arrtable = 
    [ ("tri90",                 tri90)
    , ("tri60",                 tri60)
    , ("tri45",                 tri45)  
    , ("otri90",                otri90)
    , ("otri60",                otri60)
    , ("otri45",                otri45)
    , ("revtri90",              revtri90)
    , ("revtri60",              revtri60)
    , ("revtri45",              revtri45)
    , ("orevtri90",             orevtri90)
    , ("orevtri60",             orevtri60)
    , ("orevtri45",             orevtri45)
    , ("barb90",                barb90)
    , ("barb60",                barb60)
    , ("barb45",                barb45)
    , ("revbarb90",             revbarb90)
    , ("revbarb60",             revbarb60)
    , ("revbarb45",             revbarb45)
    , ("perp",                  perp)
    , ("bracket",               bracket)
    , ("diskTip",               diskTip)
    , ("odiskTip",              odiskTip)
    , ("squareTip",             squareTip)
    , ("osquareTip",            osquareTip)
    , ("diamondTip",            diamondTip)
    , ("odiamondTip",           odiamondTip)
    , ("diamondWideTip",        diamondWideTip)
    , ("odiamondWideTip",       odiamondWideTip)
    , ("curveTip",              curveTip)
    , ("revcurveTip",           revcurveTip)    
    ]



tableGraphic :: [(String, ArrowTip)] -> TraceDrawing Double ()
tableGraphic tips = 
    drawl start $ runChain_ chn_alg $ mapM makeArrowDrawing tips
  where
    chn_alg = tableDown 18 (180,24)
    start   = P2 0 480

 
std_ctx :: DrawingContext
std_ctx = fill_colour peru $ standardContext 18



-- Note - /null/ chain action needs a better type synonym name.
--
makeArrowDrawing :: (String, ArrowTip) -> Chain Double (UNil Double)
makeArrowDrawing (name, utip) = onChain (aconn `mappend` lbl)
  where
    aconn = ignoreAns $ promoteLoc $ \pt ->
              connect pt (displace (hvec 60) pt) (uniformArrow utip connline)

    lbl   = ignoreAns $ promoteLoc $ \pt -> 
              textline WW name `at` (displace (hvec 66) pt)


-- Cf. Parsec\'s Token module...

connline :: (Real u, Floating u, InterpretUnit u) => ConnectorPathQuery u
connline = C.connline default_connector_props
