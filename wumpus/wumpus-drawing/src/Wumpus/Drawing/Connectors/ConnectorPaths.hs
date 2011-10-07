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

    connline
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
import Wumpus.Drawing.Connectors.ConnectorProps
import Wumpus.Drawing.Paths

import Wumpus.Basic.Kernel                      -- package: wumpus-basic

import Wumpus.Core                              -- package: wumpus-core

import Data.AffineSpace                         -- package: vector-space




inlineSrc :: (Real u, Floating u, InterpretUnit u) 
          => SpacingProjection u
inlineSrc props p0 p1 = 
    connectorSrcSpace props >>= \sep -> 
    let ang = vdirection $ pvec p0 p1
    in return $ p0 .+^ avec ang sep

inlineDst :: (Real u, Floating u, InterpretUnit u) 
          => SpacingProjection u
inlineDst props p0 p1 = 
    connectorDstSpace props >>= \sep -> 
    let ang = vdirection $ pvec p0 p1
    in return $ p1 .-^ avec ang sep


-- | Like 'inlineSrc' but /expands/ rather than /contracts/.
-- 
-- Use for loops.
--
extlineSrc :: (Real u, Floating u, InterpretUnit u) 
          => SpacingProjection u
extlineSrc props p0 p1 = 
    connectorSrcSpace props >>= \sep -> 
    let ang = vdirection $ pvec p0 p1
    in return $ p0 .-^ avec ang sep


-- | Like 'inlineDst' but /expands/ rather than /contracts/.
-- 
-- Use for loops.
--
extlineDst :: (Real u, Floating u, InterpretUnit u) 
          => SpacingProjection u
extlineDst props p0 p1 = 
    connectorDstSpace props >>= \sep -> 
    let ang = vdirection $ pvec p0 p1
    in return $ p1 .+^ avec ang sep

-- | Horizontal \"orthonormal\" version of 'inlineSrc'.
--
horizontalSrc :: (Real u, Floating u, InterpretUnit u) 
              => SpacingProjection u
horizontalSrc props p0 p1 = 
    connectorSrcSpace props >>= \sep ->
    case horizontalDirection $ vdirection $ pvec p0 p1 of
      RIGHTWARDS -> return $ p0 .+^ go_right sep
      _          -> return $ p0 .+^ go_left sep        



-- | Horizontal \"orthonormal\" version of 'inlineDst'.
--
horizontalDst :: (Real u, Floating u, InterpretUnit u) 
              => SpacingProjection u
horizontalDst props p0 p1 = 
    connectorDstSpace props >>= \sep ->
    case horizontalDirection $ vdirection $ pvec p0 p1 of
      RIGHTWARDS -> return $ p1 .+^ go_left sep
      _          -> return $ p1 .+^ go_right sep



-- | Vertical \"orthonormal\" version of 'inlineSrc'.
--
verticalSrc :: (Real u, Floating u, InterpretUnit u) 
            => SpacingProjection u
verticalSrc props p0 p1 =
    connectorSrcSpace props >>= \sep ->
    case verticalDirection $ vdirection $ pvec p0 p1 of
      UPWARDS -> return $ p0 .+^ go_up sep       
      _       -> return $ p0 .+^ go_down sep


-- | Vertical \"orthonormal\" version of 'inlineDst'.
--
verticalDst :: (Real u, Floating u, InterpretUnit u) 
            => SpacingProjection u
verticalDst props p0 p1 =
    connectorDstSpace props >>= \sep ->
    case verticalDirection $ vdirection $ pvec p0 p1 of
      UPWARDS -> return $ p1 .+^ go_down sep       
      _       -> return $ p1 .+^ go_up sep


abovePerpSrc :: (Real u, Floating u, InterpretUnit u) 
         => SpacingProjection u
abovePerpSrc props p0 p1 = 
    connectorSrcSpace props >>= \sep -> 
    let ang = vdirection $ pvec p0 p1
    in return $ p0 .+^ avec (ang + half_pi) sep

abovePerpDst :: (Real u, Floating u, InterpretUnit u) 
         => SpacingProjection u
abovePerpDst props p0 p1 =
    connectorDstSpace props >>= \sep -> 
    let ang = vdirection $ pvec p0 p1
    in return $ p1 .+^ avec (ang + half_pi) sep


belowPerpSrc :: (Real u, Floating u, InterpretUnit u) 
         => SpacingProjection u
belowPerpSrc props p0 p1 =
    connectorSrcSpace props >>= \sep -> 
    let ang = vdirection $ pvec p0 p1
    in return $ p0 .+^ avec (ang - half_pi) sep

belowPerpDst :: (Real u, Floating u, InterpretUnit u) 
         => SpacingProjection u
belowPerpDst props p0 p1 =
    connectorDstSpace props >>= \sep -> 
    let ang = vdirection $ pvec p0 p1
    in return $ p1 .+^ avec (ang - half_pi) sep



-- | Promote a function from source and dest points to a connector 
-- function accounting for the separator values in the 
-- DrawingContext.
--
-- TO BECOME OBSOLETE.
-- 
buildProjConn :: (Real u, Floating u, InterpretUnit u) 
              => SpacingProjection u -> SpacingProjection u
              -> (Point2 u -> Point2 u -> Query u (AbsPath u))
              -> ConnectorPathQuery u
buildProjConn _ _ fn = qpromoteConn $ \p0 p1 -> fn p0 p1







-- | Straight line connector.
--
connline :: (Real u, Floating u, InterpretUnit u) 
         => ConnectorProps -> ConnectorPathQuery u
connline _ = qpromoteConn $ \p0 p1 -> return $ line1 p0 p1




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
connarc :: (Real u, Floating u, Ord u, InterpretUnit u, Tolerance u) 
        => ConnectorProps -> ConnectorPathQuery u
connarc props = qpromoteConn $ \p0 p1 -> 
            let arc_ang = conn_arc_ang props 
                v1      = pvec p0 p1
                hlen    = 0.5 * vlength v1
                ang     = vdirection v1
                cp0     = p0 .+^ avec (ang + arc_ang) hlen
                cp1     = p1 .+^ avec (pi + ang - arc_ang) hlen
            in return $ curve1 p0 cp0 cp1 p1




-- | Horizontal-diagonal-horizontal connector.
--
-- >      --@
-- >     /
-- >  o--
-- 
-- Horizontal /arms/ are drawn from the start and end points, a
-- diagonal segment joins the arms. 
-- 
connhdiagh :: (Real u, Floating u, Tolerance u, InterpretUnit u)
           => ConnectorProps -> ConnectorPathQuery u
connhdiagh props = buildProjConn horizontalSrc horizontalDst $ \p0 p1 -> 
    connectorArms props >>= \(src_arm, dst_arm) ->
    case horizontalDirection $ vdirection $ pvec p0 p1 of
      RIGHTWARDS -> right p0 p1 src_arm dst_arm
      _          -> left  p0 p1 src_arm dst_arm
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
connvdiagv :: (Real u, Floating u, Tolerance u, InterpretUnit u)
           => ConnectorProps -> ConnectorPathQuery u
connvdiagv props = buildProjConn verticalSrc verticalDst $ \p0 p1 -> 
    connectorArms props >>= \(src_arm, dst_arm) ->
    case verticalDirection $ vdirection $ pvec p0 p1 of
      UPWARDS -> up   p0 p1 src_arm dst_arm
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
conndiagh :: (Real u, Floating u, Tolerance u, InterpretUnit u)
          => ConnectorProps -> ConnectorPathQuery u
conndiagh props = buildProjConn inlineSrc horizontalDst $ \p0 p1 -> 
    connectorArms props >>= \(_,dst_arm) ->
    case horizontalDirection $ vdirection $ pvec p0 p1 of
      RIGHTWARDS -> right p0 p1 dst_arm
      _          -> left  p0 p1 dst_arm
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
conndiagv :: (Real u, Floating u, Tolerance u, InterpretUnit u)
          => ConnectorProps -> ConnectorPathQuery u
conndiagv props = buildProjConn inlineSrc verticalDst $ \p0 p1 -> 
    connectorArms props >>= \(_,dst_arm) ->
    case verticalDirection $ vdirection $ pvec p0 p1 of
      UPWARDS -> up    p0 p1 dst_arm
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
connhdiag :: (Real u, Floating u, Tolerance u, InterpretUnit u)
          => ConnectorProps -> ConnectorPathQuery u
connhdiag props = buildProjConn horizontalSrc inlineDst $ \p0 p1 -> 
    connectorArms props  >>= \(src_arm,_) ->
    case horizontalDirection $ vdirection $ pvec p0 p1 of
      RIGHTWARDS -> right p0 p1 src_arm
      _          -> left  p0 p1 src_arm
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
connvdiag :: (Real u, Floating u, Tolerance u, InterpretUnit u)
          => ConnectorProps -> ConnectorPathQuery u
connvdiag props = buildProjConn verticalSrc inlineDst $ \p0 p1 -> 
    connectorArms props >>= \(src_arm,_) ->
    case verticalDirection $ vdirection $ pvec p0 p1 of
      UPWARDS -> up    p0 p1 src_arm
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
connabar :: (Real u, Floating u, Tolerance u, InterpretUnit u) 
         => ConnectorProps -> ConnectorPathQuery u
connabar props = buildProjConn abovePerpSrc abovePerpDst $ \p0 p1 ->
    connectorArms props >>= \(src_arm,dst_arm) ->
    let ang = vdirection $ pvec p0 p1
    in return $ vertexPath [ p0, dispDirectionTheta UP src_arm ang p0
                           , dispDirectionTheta UP dst_arm ang p1, p1 ]


-- | Bar connector.
-- 
-- >  o    @ 
-- >  |    |
-- >  '----'  
--
-- The bar is drawn /below/ the points.
--
connbbar :: (Real u, Floating u, Tolerance u, InterpretUnit u) 
         => ConnectorProps -> ConnectorPathQuery u
connbbar props = buildProjConn belowPerpSrc belowPerpDst $ \p0 p1 ->
    connectorArms props >>= \(src_arm, dst_arm) ->
    let ang = vdirection $ pvec p0 p1
    in return $ vertexPath [ p0, dispDirectionTheta DOWN src_arm ang p0
                           , dispDirectionTheta DOWN dst_arm ang p1, p1 ]



-- | Right angle connector.
-- 
-- >  ,----@ 
-- >  | 
-- >  o   
--
-- The bar is drawn /above/ the points.
--
connaright :: (Real u, Floating u, Tolerance u, InterpretUnit u) 
           => ConnectorProps -> ConnectorPathQuery u
connaright _ = 
    buildProjConn verticalSrc horizontalDst $ \ p0@(P2 x0 _) p1@(P2 _ y1) ->
      let mid = P2 x0 y1 in return $ vertexPath [p0, mid, p1]


-- | Right angle connector.
-- 
-- >       @ 
-- >       |
-- >  o----'  
--
-- The bar is drawn /below/ the points.
--
connbright :: (Real u, Floating u, Tolerance u, InterpretUnit u) 
           => ConnectorProps -> ConnectorPathQuery u
connbright _ = 
    buildProjConn horizontalSrc verticalDst $ \ p0@(P2 _ y0) p1@(P2 x1 _) ->
    let mid = P2 x1 y0 in return $ vertexPath [p0, mid, p1]



-- Helper 

-- | Derive the direction aka. sign of an arm.
--
directional :: (Num u, Ord u) => u -> u -> u -> u
directional src dst arm = if src < dst then arm else negate arm
                    


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
connhrr :: (Real u, Floating u, Tolerance u, InterpretUnit u) 
        => ConnectorProps -> ConnectorPathQuery u
connhrr props = 
    buildProjConn horizontalSrc horizontalDst $ \ p0@(P2 x0 y0) p1@(P2 x1 y1) ->
    connectorArms props >>= \(src_arm,_) -> 
    let a0 = p0 .+^ hvec (directional x0 x1 src_arm)
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
connrrh :: (Real u, Floating u, Tolerance u, InterpretUnit u) 
        => ConnectorProps -> ConnectorPathQuery u
connrrh props = 
    buildProjConn horizontalSrc horizontalDst $ \p0@(P2 x0 y0) p1@(P2 x1 y1) ->
      connectorArms props >>= \(_,dst_arm) -> 
      let a1 = p1 .-^ hvec (directional x0 x1 dst_arm)
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
connvrr :: (Real u, Floating u, Tolerance u, InterpretUnit u) 
        => ConnectorProps -> ConnectorPathQuery u
connvrr props = 
    buildProjConn verticalSrc verticalDst $ \p0@(P2 x0 y0) p1@(P2 x1 y1) ->
      connectorArms props >>= \(src_arm,_) -> 
      let a0 = p0 .+^ vvec (directional y0 y1 src_arm)
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
connrrv :: (Real u, Floating u, Tolerance u, InterpretUnit u) 
        => ConnectorProps -> ConnectorPathQuery u
connrrv props = 
    buildProjConn verticalSrc verticalDst $ \ p0@(P2 x0 y0) p1@(P2 x1 y1) ->
      connectorArms props >>= \(_,dst_arm) -> 
      let a1 = p1 .-^ vvec (directional y0 y1 dst_arm)
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
connaloop :: (Real u, Floating u, Tolerance u, InterpretUnit u) 
          => ConnectorProps -> ConnectorPathQuery u
connaloop = loopbody id

-- | Loop connector.
--
-- >  ,-o    @--, 
-- >  |         |
-- >  '---------'
--
-- The loop is drawn /above/ the points.
--
connbloop :: (Real u, Floating u, Tolerance u, InterpretUnit u) 
          => ConnectorProps -> ConnectorPathQuery u
connbloop = loopbody negate

-- | Looping just differs on a negate...
--
loopbody :: (Real u, Floating u, Tolerance u, InterpretUnit u)
         => (u -> u) -> ConnectorProps -> ConnectorPathQuery u
loopbody fn props = buildProjConn extlineSrc extlineDst $ \p0 p1 ->
    connectorArms props  >>= \(src_arm, dst_arm) ->
    connectorLoopSize props >>= \loop_len ->
    let ang = vdirection $ pvec p0 p1 
        a0  = dispParallel (negate src_arm) ang p0
        a1  = dispPerpendicular (fn loop_len) ang a0
        z0  = dispParallel dst_arm ang p1
        z1  = dispPerpendicular (fn loop_len) ang z0
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
-- Warning - currently bezier connectors do not draw properly
-- with source or destination spacers.
--
connhbezier :: (Real u, Floating u, InterpretUnit u, Tolerance u)
            => ConnectorProps -> ConnectorPathQuery u
connhbezier props = qpromoteConn $ \p0 p1 -> 
    fmap (\(a,b) -> (2*a,2*b)) (connectorArms props) >>= \(src_arm,dst_arm) ->
    case horizontalDirection $ vdirection $ pvec p0 p1 of
      RIGHTWARDS -> right p0 p1 src_arm dst_arm
      _          -> left  p0 p1 src_arm dst_arm
  where
    right p0 p1 h0 h1 = return $ curve1 p0 (p0 .+^ hvec h0) (p1 .-^ hvec h1) p1

    left  p0 p1 h0 h1 = return $ curve1 p0 (p0 .-^ hvec h0) (p1 .+^ hvec h1) p1


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
-- Warning - currently bezier connectors do not draw properly
-- with source or destination spacers.
--
connvbezier :: (Real u, Floating u, InterpretUnit u, Tolerance u)
            => ConnectorProps -> ConnectorPathQuery u
connvbezier props = qpromoteConn $ \p0 p1 -> 
    fmap (\(a,b) -> (2*a,2*b)) (connectorArms props) >>= \(src_arm,dst_arm) ->
      case verticalDirection $ vdirection $ pvec p0 p1 of
        UPWARDS -> up   p0 p1 src_arm dst_arm
        _       -> down p0 p1 src_arm dst_arm
  where
    up   p0 p1 v0 v1 = return $ curve1 p0 (p0 .+^ vvec v0) (p1 .-^ vvec v1) p1
    down p0 p1 v0 v1 = return $ curve1 p0 (p0 .-^ vvec v0) (p1 .+^ vvec v1) p1



