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

    conn_line
  , conn_arc
  , conn_hdiagh
  , conn_vdiagv
  
  , conn_diagh
  , conn_diagv

  , conn_hdiag
  , conn_vdiag

  , conna_bar
  , connb_bar
  
  , conna_flam
  , connb_flam

  , conna_orthohbar
  , connb_orthohbar

  , conna_orthovbar
  , connb_orthovbar

  , conna_right
  , connb_right

  , conn_hrr
  , conn_rrh
  , conn_vrr
  , conn_rrv

  , conna_loop
  , connb_loop

  , conn_hbezier
  , conn_vbezier

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
             => (ConnectorProps -> Point2 u -> Point2 u -> Query u (CatTrail u)) 
             -> ConnectorPathSpec u
catConnector mf = ConnectorPathSpec $ \props -> 
    qpromoteConn $ \p0 p1 -> catTrailPath p0 <$> mf props p0 p1 


-- | Straight line connector.
--
conn_line :: (Real u, Floating u, InterpretUnit u, Tolerance u) 
          => ConnectorPathSpec u
conn_line = catConnector $ \_ p0 p1 -> pure $ catline $ pvec p0 p1




-- | Form an arc connector.
-- 
-- If the conn_arc_angle in the Drawing context is positive the arc
-- will be formed /above/ the straight line joining the points. 
-- If the angle is negative it will be drawn below. 
-- 
-- The notion of /above/ is respective to the line direction, of 
-- course.
-- 
-- TODO - above and below versions...
--
conn_arc :: (Real u, Floating u, Ord u, InterpretUnit u, Tolerance u) 
        => ConnectorPathSpec u
conn_arc = catConnector $ \props p0 p1 -> 
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
conn_hdiagh :: (Real u, Floating u, Tolerance u, InterpretUnit u)
           => ConnectorPathSpec u
conn_hdiagh = catConnector $ \props p0 p1 -> 
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
conn_vdiagv :: (Real u, Floating u, Tolerance u, InterpretUnit u)
            => ConnectorPathSpec u
conn_vdiagv = catConnector $ \props p0 p1 -> 
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
conn_diagh :: (Real u, Floating u, Tolerance u, InterpretUnit u)
           => ConnectorPathSpec u
conn_diagh = catConnector $ \props p0 p1 -> 
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
conn_diagv :: (Real u, Floating u, Tolerance u, InterpretUnit u)
           => ConnectorPathSpec u
conn_diagv = catConnector $ \props p0 p1 -> 
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
conn_hdiag :: (Real u, Floating u, Tolerance u, InterpretUnit u)
           => ConnectorPathSpec u
conn_hdiag = catConnector $ \props p0 p1 -> 
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
conn_vdiag :: (Real u, Floating u, Tolerance u, InterpretUnit u)
           => ConnectorPathSpec u
conn_vdiag = catConnector $ \props p0 p1 -> 
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
conna_bar :: (Real u, Floating u, Tolerance u, InterpretUnit u) 
          => ConnectorPathSpec u
conna_bar = catConnector $ \props p0 p1 ->
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
connb_bar :: (Real u, Floating u, Tolerance u, InterpretUnit u) 
          => ConnectorPathSpec u
connb_bar = catConnector $ \props p0 p1 ->
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
conna_flam :: (Real u, Floating u, Tolerance u, InterpretUnit u) 
           => ConnectorPathSpec u
conna_flam = catConnector $ \props p0 p1 ->
    connectorLegs props >>= \(src_leg,dst_leg) ->
    return $ trail_vflam CW src_leg dst_leg $ pvec p0 p1

-- | /Flam/ connector - bleow.
--
connb_flam :: (Real u, Floating u, Tolerance u, InterpretUnit u) 
         => ConnectorPathSpec u
connb_flam = catConnector $ \props p0 p1 ->
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
conna_orthohbar :: (Real u, Floating u, Tolerance u, InterpretUnit u) 
                => ConnectorPathSpec u
conna_orthohbar = catConnector $ \props p0 p1 ->
    connectorLoopSize props >>= \looph ->
    return $ trail_ortho_hbar CW looph $ pvec p0 p1

-- | Bar connector orthonormal - below.
--
connb_orthohbar :: (Real u, Floating u, Tolerance u, InterpretUnit u) 
                => ConnectorPathSpec u
connb_orthohbar = catConnector $ \props p0 p1 ->
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
conna_orthovbar :: (Real u, Floating u, Tolerance u, InterpretUnit u) 
                => ConnectorPathSpec u
conna_orthovbar = catConnector $ \props p0 p1 ->
    connectorLoopSize props >>= \looph ->
    return $ trail_ortho_vbar CW looph $ pvec p0 p1

-- | Bar connector orthonormal - right of the points.
--
connb_orthovbar :: (Real u, Floating u, Tolerance u, InterpretUnit u) 
                => ConnectorPathSpec u
connb_orthovbar = catConnector $ \props p0 p1 ->
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
conna_right :: (Real u, Floating u, Tolerance u, InterpretUnit u) 
            => ConnectorPathSpec u
conna_right = catConnector $ \_ p0 p1 ->
    return $ trail_vright $ pvec p0 p1


-- | Right angle connector.
-- 
-- >       @ 
-- >       |
-- >  o----'  
--
-- The bar is drawn /below/ the points.
--
connb_right :: (Real u, Floating u, Tolerance u, InterpretUnit u) 
            => ConnectorPathSpec u
connb_right = catConnector $ \_ p0 p1 -> 
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
conn_hrr :: (Real u, Floating u, Tolerance u, InterpretUnit u) 
         => ConnectorPathSpec u
conn_hrr = catConnector $ \props p0 p1 ->
    connectorLegs props >>= \(src_leg,_) ->
    return $ trail_hrr src_leg $ pvec p0 p1


-- | Connector with two horizontal segements and a joining 
-- vertical segment.
--
-- >     ,----@
-- >     |
-- >  o--'  
--
-- The length of the final horizontal segment is the destination 
-- arm length. The length of the initial segment is the remaining
-- horizontal distance. 
--
conn_rrh :: (Real u, Floating u, Tolerance u, InterpretUnit u) 
         => ConnectorPathSpec u
conn_rrh = catConnector $ \props p0 p1 ->
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
-- The length of the first vertical segment is the source arm 
-- length. The length of the final segment is the remaining 
-- vertical distance. 
--
conn_vrr :: (Real u, Floating u, Tolerance u, InterpretUnit u) 
         => ConnectorPathSpec u
conn_vrr = catConnector $ \props p0 p1 ->
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
-- The length of the final vertical segment is the destination 
-- arm length. The length of the initial segment is the remaining
-- vertical distance. 
--
conn_rrv :: (Real u, Floating u, Tolerance u, InterpretUnit u) 
         => ConnectorPathSpec u
conn_rrv = catConnector $ \props p0 p1 ->
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
conna_loop :: (Real u, Floating u, Tolerance u, InterpretUnit u) 
           => ConnectorPathSpec u
conna_loop = catConnector $ \props p0 p1 -> 
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
connb_loop :: (Real u, Floating u, Tolerance u, InterpretUnit u) 
           => ConnectorPathSpec u
connb_loop = catConnector $ \props p0 p1 -> 
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
conn_hbezier :: (Real u, Floating u, InterpretUnit u, Tolerance u)
             => ConnectorPathSpec u
conn_hbezier = ConnectorPathSpec $ \props -> 
    qpromoteConn $ \p0 p1 -> 
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
conn_vbezier :: (Real u, Floating u, InterpretUnit u, Tolerance u)
             => ConnectorPathSpec u
conn_vbezier = ConnectorPathSpec $ \props -> 
    qpromoteConn $ \p0 p1 -> 
    fmap (\(a,b) -> (2*a,2*b)) (connectorArms props) >>= \(src_arm,dst_arm) ->
      case verticalDirection $ vdirection $ pvec p0 p1 of
        UPWARDS -> up   p0 p1 src_arm dst_arm
        _       -> down p0 p1 src_arm dst_arm
  where
    up   p0 p1 v0 v1 = return $ curve1 p0 (p0 .+^ vvec v0) (p1 .-^ vvec v1) p1
    down p0 p1 v0 v1 = return $ curve1 p0 (p0 .-^ vvec v0) (p1 .+^ vvec v1) p1



