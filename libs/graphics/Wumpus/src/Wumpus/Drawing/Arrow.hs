{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Drawing.Arrow
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Draw arrows
--
--------------------------------------------------------------------------------


module Wumpus.Drawing.Arrow where

import Wumpus.Core.Instances
import Wumpus.Core.Line
import Wumpus.Core.Point
import Wumpus.Core.PostScript
import Wumpus.Core.Transformations
import Wumpus.Core.Vector

import Wumpus.Drawing.Basic

import Data.AffineSpace




arrowheadTriangle :: Double -> Double -> (Double -> DPoint2 -> Polygon)
arrowheadTriangle d ang = 
  \theta endpoint -> let p0 = endpoint .+^ (hvec (-d))
                         pg = Polygon [ rotateAbout (pi-ang) endpoint p0, 
                                        endpoint, 
                                        rotateAbout (pi+ang) endpoint p0]
                     in pointwise (rotateAbout theta endpoint) pg


arrowheadVee :: Double -> Double -> (Double -> DPoint2 -> [DLineSegment2])
arrowheadVee d ang = 
  \theta endpoint -> let p0  = endpoint .+^ (hvec (-d))
                         p01 = rotateAbout (pi-ang) endpoint p0
                         p02 = rotateAbout (pi+ang) endpoint p0
                     in map (pointwise (rotateAbout theta endpoint))
                            [ lineTo p01 endpoint, lineTo endpoint p02]





-- TODO - tip should be more general, e.g. list of lines, or arcs
data Arrow a = Arrow (LineSegment2 a) Polygon
  deriving (Eq,Show)

type DArrow = Arrow Double

arrow :: DPoint2 -> DPoint2 -> DArrow
arrow p p' = Arrow ln tip where
  ln    = lineTo p p'
  theta = pi + (atan $ gradient ln) 
  tip   = arrowheadTriangle 10 (pi/10) theta p'


drawArrow :: DArrow -> WumpusM () 
drawArrow (Arrow ln tip) = drawLine ln >> drawPolygon tip
 

