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

  , connabar
  , connbbar

  , connaright
  , connbright

  , connhrr
  , connrrh
  , connvrr
  , connrrv

  , connaloop
  , connbloop

  , connhbezier
  , connvbezier

  ) where

import Wumpus.Drawing.Connectors.Base
import Wumpus.Drawing.Paths

import Wumpus.Basic.Geometry.Quadrant           -- package: wumpus-basic
import Wumpus.Basic.Kernel hiding ( promoteR2 )

import Wumpus.Core                              -- package: wumpus-core

import Data.AffineSpace                         -- package: vector-space




-- | The type of Connectors - a query from start and end point to 
-- a Path.
--
type Connector u = ConnectorQuery u (Path u)



-- | Straight line connector.
--
connline :: (Real u, Floating u, InterpretUnit u) => Connector u
connline = promoteConn $ \p0 p1 -> return $ line p0 p1



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
connarc :: (Real u, Floating u, Ord u, InterpretUnit u, LengthTolerance u) 
        => Connector u
connarc = promoteConn $ \p0 p1 -> 
    connectorArcAngle >>= \arc_ang ->
    let v1      = pvec p0 p1
        hlen    = 0.5 * vlength v1
        ang     = vdirection v1
        cp0     = p0 .+^ avec (ang + arc_ang) hlen
        cp1     = p1 .+^ avec (pi + ang - arc_ang) hlen
    in return $ curve p0 cp0 cp1 p1




-- | Horizontal-diagonal-horizontal connector.
--
-- >      --@
-- >     /
-- >  o--
-- 
-- Horizontal /arms/ are drawn from the start and end points, a
-- diagonal segment joins the arms. 
-- 
connhdiagh :: (Real u, Floating u, InterpretUnit u)
          => Connector u
connhdiagh = promoteConn $ \p0 p1 -> 
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
-- >  @
-- >  |
-- >   \
-- >    |
-- >    o
--
-- Vertical /arms/ are drawn from the start and end points, a
-- diagonal segment joins the arms. 
-- 
connvdiagv :: (Real u, Floating u, InterpretUnit u)
          => Connector u
connvdiagv = promoteConn $ \p0 p1 -> 
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
-- >    --@
-- >   /
-- >  o
-- 
-- Restricted variant of 'hconndiag' - a diagonal segment is drawn 
-- from the start point joining a horizontal arm drawn from the 
-- end point
-- 
conndiagh :: (Real u, Floating u, InterpretUnit u)
          => Connector u
conndiagh = promoteConn $ \p0 p1 -> 
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
-- >    @
-- >    |
-- >   /
-- >  o
--
-- Restricted variant of 'vconndiag' - a diagonal segment is drawn 
-- from the start point joining a vertical arm drawn from the end 
-- point.
-- 
conndiagv :: (Real u, Floating u, InterpretUnit u)
          => Connector u
conndiagv = promoteConn $ \p0 p1 -> 
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
-- >      @
-- >     /
-- >  o--
--
-- Restricted variant of 'hconndiag' - a horizontal arm is drawn
-- from the start point joining a diagonal segment drawn from the 
-- end point.
-- 
connhdiag :: (Real u, Floating u, InterpretUnit u)
          => Connector u
connhdiag = promoteConn $ \p0 p1 -> 
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
-- >    @
-- >   /
-- >  |
-- >  o
--
-- Restricted variant of 'vconndiag' - a horizontal arm is drawn
-- from the start point joining a vertical segment drawn from the 
-- end point.
-- 
connvdiag :: (Real u, Floating u, InterpretUnit u)
          => Connector u
connvdiag = promoteConn $ \p0 p1 -> 
    connectorSrcArm >>= \src_arm ->
    case quadrant $ vdirection $ pvec p0 p1 of
      QUAD_NE -> up    p0 p1 src_arm
      QUAD_NW -> up    p0 p1 src_arm
      _       -> down  p0 p1 src_arm
  where
    up   p0 p1 v1 = return $ vertexPath [ p0, p0 .+^ vvec v1, p1 ]

    down p0 p1 v1 = return $ vertexPath [ p0, p0 .-^ vvec v1, p1 ]


-- DESIGN NOTE - should the concept of /above/ and /below/ use 
-- quadrants?
--


-- | Bar connector.
--
-- >  ,----, 
-- >  |    |
-- >  o    @  
--
-- The bar is drawn /above/ the points.
--
connabar :: (Real u, Floating u, InterpretUnit u) => Connector u
connabar = promoteConn $ \p0 p1 ->
    connectorSrcArm >>= \src_arm ->
    connectorDstArm >>= \dst_arm ->
    let ang = vdirection $ pvec p0 p1
    in 
    return $ vertexPath [ p0, thetaNorthwards src_arm ang p0
                        , thetaNorthwards dst_arm ang p1, p1 ]


-- | Bar connector.
-- 
-- >  o    @ 
-- >  |    |
-- >  '----'  
--
-- The bar is drawn /below/ the points.
--
connbbar :: (Real u, Floating u, InterpretUnit u) => Connector u
connbbar = promoteConn $ \p0 p1 ->
    connectorSrcArm >>= \src_arm ->
    connectorDstArm >>= \dst_arm ->
    let ang = vdirection $ pvec p0 p1
    in return $ vertexPath [ p0, thetaSouthwards src_arm ang p0
                           , thetaSouthwards dst_arm ang p1, p1 ]



-- | Right angle connector.
-- 
-- >  ,----@ 
-- >  | 
-- >  o   
--
-- The bar is drawn /above/ the points.
--
connaright :: (Real u, Floating u, InterpretUnit u) => Connector u
connaright = promoteConn $ \ p0@(P2 x0 _) p1@(P2 _ y1) ->
    let mid = P2 x0 y1 in return $ vertexPath [p0, mid, p1]


-- | Right angle connector.
-- 
-- >       @ 
-- >       |
-- >  o----'  
--
-- The bar is drawn /below/ the points.
--
connbright :: (Real u, Floating u, InterpretUnit u) => Connector u
connbright = promoteConn $ \ p0@(P2 _ y0) p1@(P2 x1 _) ->
    let mid = P2 x1 y0 in return $ vertexPath [p0, mid, p1]



-- Helper 

-- | Derive the direction aka. sign of an arm.
--
directional :: (Num u, Ord u) => u -> u -> u -> u
directional src dst arm = if  src < dst then arm else negate arm
                    


-- | Connector with two horizontal segements and a joining 
-- vertical segment.
--
-- >       ,--@
-- >       |
-- >  o----'  
--
-- The length of the first horizontal segment is the source arm 
-- length. The length of the final segment is the remaing 
-- horizontal distance. 
--
connhrr :: (Real u, Floating u, InterpretUnit u) 
        => Connector u
connhrr = promoteConn $ \ p0@(P2 x0 y0) p1@(P2 x1 y1) ->
    fmap (directional x0 x1) connectorSrcArm >>= \ src_arm -> 
    let a0 = p0 .+^ hvec src_arm
        a1 = a0 .+^ vvec (y1 - y0)
    in return $ vertexPath [p0, a0, a1, p1]


-- | Connector with two horizontal segements and a joining 
-- vertical segment.
--
-- >     ,----@
-- >     |
-- >  o--'  
--
-- The length of the final horizontal segment is the distination 
-- arm length. The length of the initial segment is the remaining
-- horizontal distance. 
--
connrrh :: (Real u, Floating u, InterpretUnit u) 
        => Connector u
connrrh = promoteConn $ \ p0@(P2 x0 y0) p1@(P2 x1 y1) ->
    fmap (directional x0 x1) connectorDstArm >>= \ dst_arm -> 
    let a1 = p1 .-^ hvec dst_arm
        a0 = a1 .-^ vvec (y1 - y0)
    in return $ vertexPath [p0, a0, a1, p1]


-- | Connector with two right angles...
--
-- >       @
-- >       |
-- >  ,----'
-- >  |
-- >  o  
--
connvrr :: (Real u, Floating u, InterpretUnit u) 
        => Connector u
connvrr = promoteConn $ \ p0@(P2 x0 y0) p1@(P2 x1 y1) ->
    fmap (directional y0 y1) connectorSrcArm >>= \ src_arm -> 
    let a0 = p0 .+^ vvec src_arm
        a1 = a0 .+^ hvec (x1 - x0)
    in return $ vertexPath [p0, a0, a1, p1]


-- | Connector with two right angles...
--
-- >       @
-- >       |
-- >  ,----'
-- >  |
-- >  o  
--
connrrv :: (Real u, Floating u, InterpretUnit u) 
               => Connector u
connrrv = promoteConn $ \ p0@(P2 x0 y0) p1@(P2 x1 y1) ->
    fmap (directional y0 y1) connectorDstArm >>= \ dst_arm -> 
    let a1 = p1 .-^ vvec dst_arm
        a0 = a1 .-^ hvec (x1 - x0)
    in return $ vertexPath [p0, a0, a1, p1]




-- | Loop connector.
--
-- >  ,---------, 
-- >  |         |
-- >  '-o    @--'
--
-- The loop is drawn /above/ the points.
--
connaloop :: (Real u, Floating u, InterpretUnit u) => Connector u
connaloop = loopbody id

-- | Loop connector.
--
-- >  ,-o    @--, 
-- >  |         |
-- >  '---------'
--
-- The loop is drawn /above/ the points.
--
connbloop :: (Real u, Floating u, InterpretUnit u) => Connector u
connbloop = loopbody negate

-- | Looping just differs on a negate...
--
loopbody :: (Real u, Floating u, InterpretUnit u) 
         => (u -> u) -> Connector u
loopbody fn = promoteConn $ \p0 p1 ->
    connectorSrcArm   >>= \src_arm ->
    connectorDstArm   >>= \dst_arm ->
    connectorLoopSize >>= \loop_len ->
    let ang = vdirection $ pvec p0 p1 
        a0  = displaceParallel (negate src_arm) ang p0
        a1  = displacePerpendicular (fn loop_len) ang a0
        z0  = displaceParallel dst_arm ang p1
        z1  = displacePerpendicular (fn loop_len) ang z0
    in return $ vertexPath [ p0, a0, a1, z1, z0, p1 ]

-- | Bezier curve connector - the control points are positioned 
-- horizontally respective to the source and dest.
--
-- >  *--@ 
-- >    .  
-- >   . 
-- >  o--*  
--
-- Note - the source and dest arm lengths are doubled, generally 
-- this produces nicer curves.
--
connhbezier :: (Real u, Floating u, InterpretUnit u, LengthTolerance u)
            => Connector u
connhbezier = promoteConn $ \p0 p1 -> 
    fmap (2*) connectorSrcArm   >>= \src_arm ->
    fmap (2*) connectorDstArm   >>= \dst_arm ->
    case quadrant $ vdirection $ pvec p0 p1 of
      QUAD_NE -> right p0 p1 src_arm dst_arm
      QUAD_SE -> right p0 p1 src_arm dst_arm
      _       -> left  p0 p1 src_arm dst_arm
  where
    right p0 p1 h0 h1 = return $ curve p0 (p0 .+^ hvec h0) (p1 .-^ hvec h1) p1

    left  p0 p1 h0 h1 = return $ curve p0 (p0 .-^ hvec h0) (p1 .+^ hvec h1) p1


-- | Bezier curve connector - the control points are positioned 
-- vertically respective to the source and dest.
--
-- >        @ 
-- >       .|  
-- >  *  .  *
-- >  |.
-- >  o
--
-- Note - the source and dest arm lengths are doubled, generally 
-- this produces nicer curves.
--
connvbezier :: (Real u, Floating u, InterpretUnit u, LengthTolerance u)
            => Connector u
connvbezier = promoteConn $ \p0 p1 -> 
    fmap (2*) connectorSrcArm   >>= \src_arm ->
    fmap (2*) connectorDstArm   >>= \dst_arm ->
    case quadrant $ vdirection $ pvec p0 p1 of
      QUAD_NE -> up   p0 p1 src_arm dst_arm
      QUAD_NW -> up   p0 p1 src_arm dst_arm
      _       -> down p0 p1 src_arm dst_arm
  where
    up   p0 p1 v0 v1 = return $ curve p0 (p0 .+^ vvec v0) (p1 .-^ vvec v1) p1

    down p0 p1 v0 v1 = return $ curve p0 (p0 .-^ vvec v0) (p1 .+^ vvec v1) p1



