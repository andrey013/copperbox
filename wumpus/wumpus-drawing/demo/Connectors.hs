{-# OPTIONS -Wall #-}

module Connectors where


import Wumpus.Basic.Kernel
import Wumpus.Drawing.Arrows
import Wumpus.Drawing.Chains
import Wumpus.Drawing.Colour.SVGColours
import Wumpus.Drawing.Paths hiding ( length )

import Wumpus.Core                              -- package: wumpus-core

import System.Directory

main :: IO ()
main = do 
    createDirectoryIfMissing True "./out/"
    let pic1 = runCtxPictureU std_ctx conn_pic
    writeEPS "./out/connectors01.eps" pic1
    writeSVG "./out/connectors01.svg" pic1



conn_pic :: CtxPicture Double
conn_pic = drawTracing $ tableGraphic $ conntable

conntable :: [ConnectorPath Double]
conntable = 
    [ connLine
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

tableGraphic :: (Real u, Floating u, FromPtSize u) 
             => [ConnectorPath u] -> TraceDrawing u ()
tableGraphic conns = zipchainWith makeConnDrawing conns ps
  where
    ps = tableDown 10 (120,52) (displaceV 520 zeroPt)


 
std_ctx :: DrawingContext
std_ctx = fillColour peru $ standardContext 18



makeConnDrawing :: (Real u, Floating u, FromPtSize u) 
                 => ConnectorPath u -> LocGraphic u
makeConnDrawing conn = 
    promoteR1 $ \p0 -> fmap (replaceL uNil) $ 
        connect (uniformArrow curveTip conn) p0 (mkP1 p0)
  where
    mkP1 = displace 100 40
  

