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


module Wumpus.Drawing.Arrow 
  ( 
    veeArrow
  , veeArrowC
  , perpArrowC
  , arrowCenterMarker

  -- * Arrowhead construction
  , arrowheadTriangle
  , arrowheadVee
  , arrowheadPerp
  
  , arrow

  ) where

import Wumpus.Core.BoundingBox
import Wumpus.Core.Curve
import Wumpus.Core.Frame
import Wumpus.Core.Geometric
import Wumpus.Core.Line
import Wumpus.Core.Point
import Wumpus.Core.Pointwise
import Wumpus.Core.Polygon
import Wumpus.Core.Radian
import Wumpus.Core.Transform
import Wumpus.Core.Vector

import Wumpus.Drawing.Path
-- import Wumpus.Drawing.PostScript

import Data.AffineSpace

import Data.Ratio

veeArrow :: DLineSegment2 -> [DLineSegment2]
veeArrow ln = ln:vs where
  vs = arrowheadVee 10 (pi/10) (langle ln) (endPoint ln) 

-- This illustrates the biggest problem that Wumpus has at present 
-- - what is the  /union/ of a curve and an arrowhead (made up of 
-- line segments)?


veeArrowC :: DCurve -> (DCurve,[DLineSegment2])
veeArrowC crv = (crv,vs) where
  cl :: Integer
  vs = arrowheadVee 10 (pi/10) (pi + (vangle $ bisector v1 v2)) (endPoint crv) 
  v1 = endTangentVector crv
  cl = floor $ gravesenLength 0.1 crv
  t  = (cl-10) % cl       -- go back the length of the arrow head
  v2 = if t>0 then endTangentVector $ fst $ subdividet t crv else v1
  



perpArrowC :: DCurve -> (DCurve,[DLineSegment2])
perpArrowC crv = (crv,vs) where
  vs = arrowheadPerp 5 (endTangent crv) (endPoint crv)


arrowCenterMarker :: DLineSegment2 -> [DLineSegment2]
arrowCenterMarker ls = [ls,cm] where
  p     = lineCenter ls 
  theta = langle ls
  p0    = p .+^ avec2 (theta + pi/2) 2
  p1    = p .+^ avec2 (theta - pi/2) 2
  cm    = LS p0 p1


--------------------------------------------------------------------------------
-- Arrowhead construction

arrowheadTriangle :: Double -> Radian -> (Radian -> DPoint2 -> DPolygon)
arrowheadTriangle d ang = 
  \theta endpt@(P2 x y) -> let halfBW = d * fromRadian (tan ang) 
                               tri    = translate x y (isoscelesTriangle (2*halfBW) d)
                           in   pointwise (rotateAbout (theta - pi/2) endpt)
                                $ pointwise (inFrame (ortho $ P2 halfBW d))
                                $ tri


arrowheadVee :: Double -> Radian
             -> (Radian -> DPoint2 -> [DLineSegment2])
arrowheadVee d ang = 
  \theta endpt -> let p0  = endpt .+^ (hvec (-d))
                      p01 = rotateAbout (pi-ang) endpt p0
                      p02 = rotateAbout (pi+ang) endpt p0
                  in map (pointwise (rotateAbout (theta - pi) endpt))
                            [LS p01 endpt, LS endpt p02]



arrowheadPerp :: Double -> (Radian -> DPoint2 -> [DLineSegment2])
arrowheadPerp d = 
  \theta endpt -> let p0 = endpt .+^ (hvec (-d))
                      p1 = endpt .+^ (hvec d)
                  in [pointwise (rotateAbout (theta+pi/2) endpt) (LS p0 p1)]




arrow :: DPoint2 -> DPoint2 -> Path DPoint2
arrow p p' = segmentPath (ln:tip) where
  ln    = LS p p'
  theta = {- pi/2 + -} (langle ln) 
  tip   = arrowheadVee 10 (pi/10) theta p'


--------------------------------------------------------------------------------
-- OLD - arrow type 

{-


-- TODO - tip should be more general, e.g. list of lines, or arcs
data Arrow a = Arrow (LineSegment Point2 a) (Polygon a)
  deriving (Eq,Show)

type DArrow = Arrow Double

arrow :: DPoint2 -> DPoint2 -> DArrow
arrow p p' = Arrow ln tip where
  ln    = lineTo p p'
  theta = {- pi/2 + -} (langle ln) 
  tip   = arrowheadTriangle 10 (pi/10) theta p'


picArrow :: DArrow -> Picture 
picArrow (Arrow ln poly) = withFrame $ \frm -> 
  let ln'  = pointwise (coord frm) ln
      arr' = Arrow ln' (pointwise (coord frm) poly)
  in (drawArrow arr', bounds ln')

drawArrow :: DArrow -> WumpusM () 
drawArrow (Arrow ln tip) = drawLine ln >> fillPolygon tip

-} 

