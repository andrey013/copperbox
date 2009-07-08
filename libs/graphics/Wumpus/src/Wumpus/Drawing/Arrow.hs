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

import Wumpus.Core.Curve
import Wumpus.Core.Frame
import Wumpus.Core.Geometric
import Wumpus.Core.Line
import Wumpus.Core.Point
import Wumpus.Core.Pointwise
import Wumpus.Core.Polygon
import Wumpus.Core.Radian
import Wumpus.Core.Transformations
import Wumpus.Core.Vector

import Wumpus.Drawing.Basic
import Wumpus.Drawing.PostScript

import Data.AffineSpace

import Data.Ratio

veeArrow :: DLineSegment2 -> [DLineSegment2]
veeArrow ln = ln:vs where
  vs = arrowheadVee 10 (pi/10) (langle ln) (endPoint ln) 

-- This illustrates Wumpus's biggest current problem - what is the 
-- /union/ of a curve and an arrowhead (made up of line segments)?

-- Also has the problem that the arrow head is tangent to the end,
-- it should probably follow the secant of its /height/ instead.


veeArrowC :: DCurve -> (DCurve,[DLineSegment2])
veeArrowC crv = (crv,vs) where
  vs = arrowheadVee 10 (pi/10) (pi + d1 + halfdiff d1 d2) (endPoint crv) 
  d1 = endTangent crv
  cl = floor $ gravesenLength 0.1 crv
  t  = (cl-10) % cl       -- go back the length of the arrow head
  d2 = if t>0 then let (a,_) = subdividet t crv in endTangent a
              else d1

  halfdiff a b = (interior a b) / 2



perpArrowC :: DCurve -> (DCurve,[DLineSegment2])
perpArrowC crv = (crv,vs) where
  vs = arrowheadPerp 5 (endTangent crv) (endPoint crv)


arrowheadTriangle :: Double -> DRadian -> (DRadian -> DPoint2 -> DPolygon)
arrowheadTriangle d ang = 
  \theta endpt -> let halfBW = d * fromRadian (tan ang) 
                      tri    = isoscelesTriangle (2*halfBW) d endpt
                  in   pointwise (rotateAbout (theta - pi/2) endpt)
                     $ pointwise (inFrame `flip` (ortho $ P2 halfBW d))
                     $ tri


arrowheadVee :: Double -> DRadian
             -> (DRadian -> DPoint2 -> [DLineSegment2])
arrowheadVee d ang = 
  \theta endpt -> let p0  = endpt .+^ (hvec (-d))
                      p01 = rotateAbout (pi-ang) endpt p0
                      p02 = rotateAbout (pi+ang) endpt p0
                  in map (pointwise (rotateAbout (theta - pi) endpt))
                            [ lineTo p01 endpt, lineTo endpt p02]



arrowheadPerp :: Double -> (DRadian -> DPoint2 -> [DLineSegment2])
arrowheadPerp d = 
  \theta endpt -> let p0 = endpt .+^ (hvec (-d))
                      p1 = endpt .+^ (hvec d)
                  in [pointwise (rotateAbout (theta+pi/2) endpt) (lineTo p0 p1)]


arrowCenterMarker :: DLineSegment2 -> [DLineSegment2]
arrowCenterMarker ls = [ls,cm] where
  p     = lineCenter ls 
  theta = langle ls
  p0    = p .+^ avec2 (theta + pi/2) 2
  p1    = p .+^ avec2 (theta - pi/2) 2
  cm    = lineTo p0 p1

-- TODO - tip should be more general, e.g. list of lines, or arcs
data Arrow a = Arrow (LineSegment Point2 a) (Polygon a)
  deriving (Eq,Show)

type DArrow = Arrow Double

arrow :: DPoint2 -> DPoint2 -> DArrow
arrow p p' = Arrow ln tip where
  ln    = lineTo p p'
  theta = {- pi/2 + -} (langle ln) 
  tip   = arrowheadTriangle 10 (pi/10) theta p'


drawArrow :: DArrow -> WumpusM () 
drawArrow (Arrow ln tip) = drawLine ln >> drawPolygon tip
 

