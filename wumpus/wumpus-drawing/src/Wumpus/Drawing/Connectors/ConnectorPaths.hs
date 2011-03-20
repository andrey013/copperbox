{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Drawing.Connectors.ConnectorPaths
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Primitive connectors
--
--------------------------------------------------------------------------------

module Wumpus.Drawing.Connectors.ConnectorPaths
  ( 

    Connector
  , connline
  , connarc
  , connhdiagh
  , connvdiagv
  
  , conndiagh
  , conndiagv

  , connhdiag
  , connvdiag

  ) where

import Wumpus.Drawing.Paths

import Wumpus.Basic.Geometry.Quadrant           -- package: wumpus-basic
import Wumpus.Basic.Kernel

import Wumpus.Core                              -- package: wumpus-core

import Data.AffineSpace                         -- package: vector-space




-- | The type of Connectors - a query from start and end point to 
-- a Path.
--
type Connector u = ConnectorQuery u (Path u)

-- | Straight line connector.
--
connline :: Floating u => Connector u
connline = promoteR2 $ \p0 p1 -> return $ line p0 p1



-- | Form an arc connector.
-- 
-- If the conn_arc_angle in the Drawing context is positive the arc
-- will be formed /above/ the straight line joining the points. 
-- If the angle is negative it will be drawn below. 
-- 
-- The notion of /above/ is respective to the line direction, of 
-- course.
-- 
--
connarc :: (Real u, Floating u, Ord u, LengthTolerance u) 
        => Connector u
connarc = promoteR2 $ \p0 p1 -> 
    connectorArcAngle >>= \arc_ang ->
    let v1      = pvec p0 p1
        hlen    = 0.5 * vlength v1
        ang     = vdirection v1
        cp0     = p0 .+^ avec (ang + arc_ang) hlen
        cp1     = p1 .+^ avec (pi + ang - arc_ang) hlen
    in return $ curve p0 cp0 cp1 p1




-- | Horizontal-diagonal-horizontal connector.
--
-- Horizontal /arms/ are drawn from the start and end points, a
-- diagonal segment joins the arms. 
-- 
connhdiagh :: (Real u, Floating u, InterpretUnit u)
          => Connector u
connhdiagh = promoteR2 $ \p0 p1 -> 
   connectorSrcArm >>= \src_arm ->
   connectorDstArm >>= \dst_arm ->
   case quadrant $ vdirection $ pvec p0 p1 of
     QUAD_NE -> right p0 p1 src_arm dst_arm
     QUAD_SE -> right p0 p1 src_arm dst_arm
     _       -> left  p0 p1 src_arm dst_arm
  where
    right p0 p1 h0 h1 = return $ vertexPath [ p0, p0 .+^ hvec h0
                                            , p1 .-^ hvec h1, p1 ]

    left  p0 p1 h0 h1 = return $ vertexPath [ p0, p0 .-^ hvec h0 
                                            , p1 .+^ hvec h1, p1 ]



-- | Vertical-diagonal-vertical connector.
--
-- Vertical /arms/ are drawn from the start and end points, a
-- diagonal segment joins the arms. 
-- 
connvdiagv :: (Real u, Floating u, InterpretUnit u)
          => Connector u
connvdiagv = promoteR2 $ \p0 p1 -> 
   connectorSrcArm >>= \src_arm ->
   connectorDstArm >>= \dst_arm ->
   case quadrant $ vdirection $ pvec p0 p1 of
     QUAD_NE -> up   p0 p1 src_arm dst_arm
     QUAD_NW -> up   p0 p1 src_arm dst_arm
     _       -> down p0 p1 src_arm dst_arm
  where
    up   p0 p1 v0 v1 = return $ vertexPath [ p0, p0 .+^ vvec v0
                                           , p1 .-^ vvec v1, p1 ]

    down p0 p1 v0 v1 = return $ vertexPath [ p0, p0 .-^ vvec v0 
                                           , p1 .+^ vvec v1, p1 ]



-- | Diagonal-horizontal connector.
--
-- Restricted variant of 'hconndiag' - a diagonal segment is drawn 
-- from the start point joining a horizontal arm drawn from the 
-- end point
-- 
conndiagh :: (Real u, Floating u, InterpretUnit u)
          => Connector u
conndiagh = promoteR2 $ \p0 p1 -> 
   connectorDstArm >>= \dst_arm ->
   case quadrant $ vdirection $ pvec p0 p1 of
     QUAD_NE -> right p0 p1 dst_arm
     QUAD_SE -> right p0 p1 dst_arm
     _       -> left  p0 p1 dst_arm
  where
    right p0 p1 h1 = return $ vertexPath [ p0, p1 .-^ hvec h1, p1 ]

    left  p0 p1 h1 = return $ vertexPath [ p0, p1 .+^ hvec h1, p1 ]


-- | Diagonal-vertical connector.
--
-- Restricted variant of 'vconndiag' - a diagonal segment is drawn 
-- from the start point joining a vertical arm drawn from the end 
-- point.
-- 
conndiagv :: (Real u, Floating u, InterpretUnit u)
          => Connector u
conndiagv = promoteR2 $ \p0 p1 -> 
   connectorDstArm >>= \dst_arm ->
   case quadrant $ vdirection $ pvec p0 p1 of
     QUAD_NE -> up    p0 p1 dst_arm
     QUAD_NW -> up    p0 p1 dst_arm
     _       -> down  p0 p1 dst_arm
  where
    up   p0 p1 v1 = return $ vertexPath [ p0, p1 .-^ vvec v1, p1 ]

    down p0 p1 v1 = return $ vertexPath [ p0, p1 .+^ vvec v1, p1 ]



-- | Horizontal-diagonal connector.
--
-- Restricted variant of 'hconndiag' - a horizontal arm is drawn
-- from the start point joining a diagonal segment drawn from the 
-- end point.
-- 
connhdiag :: (Real u, Floating u, InterpretUnit u)
          => Connector u
connhdiag = promoteR2 $ \p0 p1 -> 
   connectorSrcArm >>= \src_arm ->
   case quadrant $ vdirection $ pvec p0 p1 of
     QUAD_NE -> right p0 p1 src_arm
     QUAD_SE -> right p0 p1 src_arm
     _       -> left  p0 p1 src_arm
  where
    right p0 p1 h1 = return $ vertexPath [ p0, p0 .+^ hvec h1, p1 ]

    left  p0 p1 h1 = return $ vertexPath [ p0, p0 .-^ hvec h1, p1 ]


-- | Vertical-diagonal connector.
--
-- Restricted variant of 'vconndiag' - a horizontal arm is drawn
-- from the start point joining a vertical segment drawn from the 
-- end point.
-- 
connvdiag :: (Real u, Floating u, InterpretUnit u)
          => Connector u
connvdiag = promoteR2 $ \p0 p1 -> 
   connectorSrcArm >>= \src_arm ->
   case quadrant $ vdirection $ pvec p0 p1 of
     QUAD_NE -> up    p0 p1 src_arm
     QUAD_NW -> up    p0 p1 src_arm
     _       -> down  p0 p1 src_arm
  where
    up   p0 p1 v1 = return $ vertexPath [ p0, p0 .+^ vvec v1, p1 ]

    down p0 p1 v1 = return $ vertexPath [ p0, p0 .-^ vvec v1, p1 ]
