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
  
  , connaflam
  , connbflam

  , connaorthohbar
  , connborthohbar

  , connaorthovbar
  , connborthovbar

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

import Wumpus.Drawing.Basis.InclineTrails
import Wumpus.Drawing.Connectors.Base
import Wumpus.Drawing.Connectors.ConnectorProps
import Wumpus.Drawing.Paths

import Wumpus.Basic.Kernel                      -- package: wumpus-basic

import Wumpus.Core                              -- package: wumpus-core

import Data.AffineSpace                         -- package: vector-space

import Control.Applicative




-- | Build as Path as a CatTrail between two points.
--
catConnector :: (Floating u, Ord u, InterpretUnit u, Tolerance u) 
             => (Point2 u -> Point2 u -> Query u (CatTrail u)) 
             -> ConnectorPathQuery u
catConnector mf = qpromoteConn $ \p0 p1 -> catTrailPath p0 <$> mf p0 p1 



-- | Straight line connector.
--
connline :: (Real u, Floating u, InterpretUnit u, Tolerance u) 
         => ConnectorProps -> ConnectorPathQuery u
connline _ = catConnector $ \p0 p1 -> pure $ catline $ pvec p0 p1




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
connarc props = catConnector $ \p0 p1 -> 
            let arc_ang = conn_arc_ang props 
                v1      = pvec p0 p1
                h       = (0.5 * vlength v1) * (fromRadian $ tan arc_ang)
            in return $ vtriCurve CW h v1 




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
connhdiagh props = catConnector $ \p0 p1 -> 
    connectorLegs props >>= \(src_leg, dst_leg) ->
    return $ trail_hdiagh src_leg dst_leg $ pvec p0 p1


-- probably want trail_hdiagh as a library function

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
connvdiagv props = catConnector $ \p0 p1 -> 
    connectorLegs props >>= \(src_leg, dst_leg) ->
    return $ trail_vdiagv src_leg dst_leg $ pvec p0 p1


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
conndiagh props = catConnector $ \p0 p1 -> 
    connectorLegs props >>= \(_, dst_leg) ->
    return $ trail_diagh dst_leg $ pvec p0 p1


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
conndiagv props = catConnector $ \p0 p1 -> 
    connectorLegs props >>= \(_, dst_leg) ->
    return $ trail_diagv dst_leg $ pvec p0 p1



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
connhdiag props = catConnector $ \p0 p1 -> 
    connectorLegs props >>= \(src_leg, _) ->
    return $ trail_hdiag src_leg $ pvec p0 p1


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
connvdiag props = catConnector $ \p0 p1 -> 
    connectorLegs props >>= \(src_leg, _) ->
    return $ trail_vdiag src_leg $ pvec p0 p1



-- DESIGN NOTE - should the concept of /above/ and /below/ use 
-- ClockDirection instead?
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
connabar props = catConnector $ \p0 p1 ->
    connectorLegs props >>= \(src_leg,dst_leg) ->
    return $ trail_perp_bar2 CW src_leg dst_leg $ pvec p0 p1


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
connbbar props = catConnector $ \p0 p1 ->
    connectorLegs props >>= \(src_leg,dst_leg) ->
    return $ trail_perp_bar2 CCW src_leg dst_leg $ pvec p0 p1


-- | /Flam/ connector.
--
-- >    ,- '
-- >  ,-   | 
-- >  |    |
-- >  o    @  
--
-- The bar is drawn /above/ the points.
--
connaflam :: (Real u, Floating u, Tolerance u, InterpretUnit u) 
         => ConnectorProps -> ConnectorPathQuery u
connaflam props = catConnector $ \p0 p1 ->
    connectorLegs props >>= \(src_leg,dst_leg) ->
    return $ trail_vflam CW src_leg dst_leg $ pvec p0 p1

-- | /Flam/ connector - bleow.
--
connbflam :: (Real u, Floating u, Tolerance u, InterpretUnit u) 
         => ConnectorProps -> ConnectorPathQuery u
connbflam props = catConnector $ \p0 p1 ->
    connectorLegs props >>= \(src_leg,dst_leg) ->
    return $ trail_vflam CCW src_leg dst_leg $ pvec p0 p1




-- | Bar connector - always orthonormal .
--
-- >  
-- >  ,----, 
-- >  |    |
-- >  o    @  
--
-- The bar is drawn /above/ the points.
--
connaorthohbar :: (Real u, Floating u, Tolerance u, InterpretUnit u) 
               => ConnectorProps -> ConnectorPathQuery u
connaorthohbar props = catConnector $ \p0 p1 ->
    connectorLoopSize props >>= \looph ->
    return $ trail_ortho_hbar CW looph $ pvec p0 p1

-- | Bar connector orthonormal - below.
--
connborthohbar :: (Real u, Floating u, Tolerance u, InterpretUnit u) 
               => ConnectorProps -> ConnectorPathQuery u
connborthohbar props = catConnector $ \p0 p1 ->
    connectorLoopSize props >>= \looph ->
    return $ trail_ortho_hbar CCW looph $ pvec p0 p1




-- | Bar connector - always orthonormal.
--
-- >  
-- >  ,--- o 
-- >  |   
-- >  '--- @  
-- > 
--
-- The bar is drawn /left/ of the points.
--
connaorthovbar :: (Real u, Floating u, Tolerance u, InterpretUnit u) 
               => ConnectorProps -> ConnectorPathQuery u
connaorthovbar props = catConnector $ \p0 p1 ->
    connectorLoopSize props >>= \looph ->
    return $ trail_ortho_vbar CW looph $ pvec p0 p1

-- | Bar connector orthonormal - right of the points.
--
connborthovbar :: (Real u, Floating u, Tolerance u, InterpretUnit u) 
               => ConnectorProps -> ConnectorPathQuery u
connborthovbar props = catConnector $ \p0 p1 ->
    connectorLoopSize props >>= \looph ->
    return $ trail_ortho_vbar CCW looph $ pvec p0 p1



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
connaright _ = catConnector $ \p0 p1 ->
    return $ trail_vright $ pvec p0 p1


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
connbright _ = catConnector $ \p0 p1 -> 
    return $ trail_hright $ pvec p0 p1




-- | Connector with two horizontal segments and a joining 
-- vertical segment.
--
-- >       ,--@
-- >       |
-- >  o----'  
--
-- The length of the first horizontal segment is the source arm 
-- length. The length of the final segment is the remaining 
-- horizontal distance. 
--
connhrr :: (Real u, Floating u, Tolerance u, InterpretUnit u) 
        => ConnectorProps -> ConnectorPathQuery u
connhrr props = catConnector $ \p0 p1 ->
    connectorLegs props >>= \(src_leg,_) ->
    return $ trail_hrr src_leg $ pvec p0 p1


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
connrrh props = catConnector $ \p0 p1 ->
    connectorLegs props >>= \(_,dst_leg) ->
    return $ trail_rrh dst_leg $ pvec p0 p1


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
connvrr props = catConnector $ \p0 p1 ->
    connectorLegs props >>= \(src_leg,_) ->
    return $ trail_vrr src_leg $ pvec p0 p1


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
connrrv props = catConnector $ \p0 p1 ->
    connectorLegs props >>= \(_,dst_leg) ->
    return $ trail_rrv dst_leg $ pvec p0 p1



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
connaloop props = catConnector $ \p0 p1 -> 
    connectorLegs props     >>= \(src_leg,dst_leg) ->
    connectorLoopSize props >>= \looph             ->
    return $ trail_rect_loop CW src_leg dst_leg looph $ pvec p0 p1
  

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
connbloop props = catConnector $ \p0 p1 -> 
    connectorLegs props     >>= \(src_leg,dst_leg) ->
    connectorLoopSize props >>= \looph             ->
    return $ trail_rect_loop CCW src_leg dst_leg looph $ pvec p0 p1



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



