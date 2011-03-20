{-# OPTIONS -Wall #-}

module Connectors where


import Wumpus.Drawing.Arrows
import Wumpus.Drawing.Chains
import Wumpus.Drawing.Colour.SVGColours
import Wumpus.Drawing.Connectors.ConnectorPaths
import Wumpus.Drawing.Paths hiding ( length )

import Wumpus.Basic.Kernel                      -- package: wumpus-basic

import Wumpus.Core                              -- package: wumpus-core

import System.Directory

main :: IO ()
main = do 
    createDirectoryIfMissing True "./out/"
    let pic1 = runCtxPictureU std_ctx conn_pic
    writeEPS "./out/connectors01.eps" pic1
    writeSVG "./out/connectors01.svg" pic1



conn_pic :: CtxPicture 
conn_pic = drawTracing $ tableGraphic $ conntable

conntable :: [PathQuery Double]
conntable = 
    [ connline
    , connarc
    , connhdiagh
    , connvdiagv
    , conndiagh
    , conndiagv
    , connhdiag
    , connvdiag

    -- OLD 
    , connLine
    , connRightVH
    , connRightHV
    , connRightVHV 15
    , connRightHVH 15
    , connIsosceles 25
    , connIsosceles (-25)
    , connIsosceles2 15
    , connIsosceles2 (-15)
    , connLightningBolt 15
    , connLightningBolt (-15)
    , connIsoscelesCurve 25
    , connIsoscelesCurve (-25)
    , connSquareCurve
    , connUSquareCurve
    , connTrapezoidCurve 40 0.5
    , connTrapezoidCurve (-40) 0.5
    , connZSquareCurve   
    , connUZSquareCurve   
    ]

tableGraphic :: [PathQuery Double] -> TraceDrawing Double ()
tableGraphic conns = 
    draw $ chn (map makeConnDrawing conns) `at` start
  where
    chn   = tableDown 8 (120,64) 
    start = P2 0 520 

 
std_ctx :: DrawingContext
std_ctx = fill_colour peru $ standardContext 18



makeConnDrawing :: PathQuery Double -> DLocGraphic 
makeConnDrawing conn = 
    promoteR1 $ \p0 -> ignoreAns $ 
        connect (uniformArrow curveTip conn) p0 (mkP1 p0)
  where
    mkP1 = displace 80 60
  

