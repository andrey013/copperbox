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

import Wumpus.Core.Line
import Wumpus.Core.Point
import Wumpus.Core.Pointwise
import Wumpus.Core.PostScript
import Wumpus.Core.Transformations
import Wumpus.Core.Vector

import Wumpus.Drawing.Basic

import Data.AffineSpace




arrowheadTriangle :: Double -> Double -> (Double -> DPoint2 -> Polygon)
arrowheadTriangle d ang = 
  \theta endpt -> let p0 = endpt .+^ (hvec (-d))
                      pg = Polygon [ rotateAbout (pi-ang) endpt p0, 
                                     endpt, 
                                     rotateAbout (pi+ang) endpt p0]
                  in pointwise (rotateAbout theta endpt) pg


arrowheadVee :: Double -> Double -> (Double -> DPoint2 -> [DLineSegment2])
arrowheadVee d ang = 
  \theta endpt -> let p0  = endpt .+^ (hvec (-d))
                      p01 = rotateAbout (pi-ang) endpt p0
                      p02 = rotateAbout (pi+ang) endpt p0
                  in map (pointwise (rotateAbout theta endpt))
                            [ lineTo p01 endpt, lineTo endpt p02]



arrowheadPerp :: Double -> (Double -> DPoint2 -> [DLineSegment2])
arrowheadPerp d = 
  \theta endpt -> let p0 = endpt .+^ (hvec (-d))
                      p1 = endpt .+^ (hvec d)
                  in [pointwise (rotateAbout (theta+pi/2) endpt) (lineTo p0 p1)]


arrowCenterMarker :: DLineSegment2 -> [DLineSegment2]
arrowCenterMarker ls = [ls,cm] where
  p     = lineCenter ls 
  theta = langle ls
  p0    = p .+^ vec2 (theta + pi/2) 2
  p1    = p .+^ vec2 (theta - pi/2) 2
  cm    = lineTo p0 p1

-- TODO - tip should be more general, e.g. list of lines, or arcs
data Arrow a = Arrow (LineSegment Point2 a) Polygon
  deriving (Eq,Show)

type DArrow = Arrow Double

arrow :: DPoint2 -> DPoint2 -> DArrow
arrow p p' = Arrow ln tip where
  ln    = lineTo p p'
  theta = pi + (langle ln) 
  tip   = arrowheadTriangle 10 (pi/10) theta p'


drawArrow :: DArrow -> WumpusM () 
drawArrow (Arrow ln tip) = drawLine ln >> drawPolygon tip
 

