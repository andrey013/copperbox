{-# OPTIONS -Wall #-}

module Connectors where


import Wumpus.Basic.Arrows
import Wumpus.Basic.Chains
import Wumpus.Basic.Colour.SVGColours
import Wumpus.Basic.Graphic
import Wumpus.Basic.Paths hiding ( length )

import Wumpus.Core                              -- package: wumpus-core

import Data.AffineSpace                         -- package: vector-space

import Control.Monad
import System.Directory

main :: IO ()
main = do 
    createDirectoryIfMissing True "./out/"
    demo01

pt2 :: Point2 Double
pt2 = P2 100 10


demo01 :: IO ()
demo01 = do 
    writeEPS "./out/connectors01.eps" pic1
    writeSVG "./out/connectors01.svg" pic1

pic1 :: Picture Double
pic1 = liftToPictureU $ execTraceDrawing std_ctx $ tableGraphic $ conntable

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
tableGraphic conns = zipWithM_ makeConnDrawing conns ps
  where
    ps = unchain (coordinateScalingContext 120 52) $ tableDown 10 6


 
std_ctx :: DrawingContext
std_ctx = fillColour peru $ standardContext 18



makeConnDrawing :: (Real u, Floating u, FromPtSize u) 
                 => ConnectorPath u -> Point2 u -> TraceDrawing u ()
makeConnDrawing conn p0 = 
    drawi_ $ situ2 (strokeConnector (dblArrow conn curveTip)) p0 p1
  where
    p1 = p0 .+^ vec 100 40
  

