{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.Paths.Connectors
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Library of connector paths...
--
-- \*\* WARNING \*\* this module is an experiment, and may 
-- change significantly or even be dropped from future revisions.
-- 
--------------------------------------------------------------------------------

module Wumpus.Basic.Paths.Connectors 
  ( 

    ConnectorPath
  , DConnectorPath

  , connLine

  , connRightVH
  , connRightHV
  , connRightVHV
  , connRightHVH

  , connIsosceles
  , connIsosceles2
  , connLightningBolt

  , curveconn

  ) where

import Wumpus.Basic.Paths.Base

import Wumpus.Core                              -- package: wumpus-core

import Data.AffineSpace                         -- package: vector-space

import Prelude hiding ( length )



type ConnectorPath u = Point2 u -> Point2 u -> Path u

type DConnectorPath = ConnectorPath Double

--------------------------------------------------------------------------------

-- | Connect with a straight line.
--
connLine :: Floating u => ConnectorPath u
connLine = line

-- | Right-angled connector - go vertical, then go horizontal.
--
connRightVH :: Floating u => ConnectorPath u
connRightVH p1@(P2 x1 _) p2@(P2 _ y2) = 
    let mid = P2 x1 y2 in traceLinePoints [p1, mid, p2]

-- | Right-angled connector - go horizontal, then go vertical.
--
connRightHV :: Floating u => ConnectorPath u
connRightHV p1@(P2 _ y1) p2@(P2 x2 _) = 
    let mid = P2 x2 y1 in traceLinePoints [p1, mid, p2]

-- | Right-angled connector - go vertical for the supplied 
-- distance, go horizontal, go vertical again for the 
-- remaining distance.
-- 
connRightVHV :: Floating u => u -> ConnectorPath u
connRightVHV v p1@(P2 x1 _) p2@(P2 x2 _) = traceLinePoints [p1, a1, a2, p2]
  where
    a1 = p1 .+^ vvec v
    a2 = a1 .+^ hvec (x2 - x1)


-- | Right-angled connector - go horizontal for the supplied 
-- distance, go verical, go horizontal again for the 
-- remaining distance.
-- 
connRightHVH :: Floating u => u -> ConnectorPath u
connRightHVH h p1@(P2 _ y1) p2@(P2 _ y2) = traceLinePoints [p1,a1,a2,p2]
  where
    a1 = p1 .+^ hvec h
    a2 = a1 .+^ vvec (y2 - y1)


-- | /Triangular/ joint.
-- 
-- @u@ is the altitude of the triangle.
--
connIsosceles :: (Real u, Floating u) => u -> ConnectorPath u 
connIsosceles u p1@(P2 x1 y1) p2@(P2 x2 y2) = 
    traceLinePoints [p1, mid_pt .+^ avec perp_ang u, p2]
  where
    mid_pt    = P2 (x1 + 0.5*(x2-x1)) (y1 + 0.5*(y2-y1))
    perp_ang  = (pi*0.5) + direction (pvec p1 p2) 



-- | Double /triangular/ joint.
-- 
-- @u@ is the altitude of the triangle.
--
connIsosceles2 :: (Real u, Floating u) => u -> ConnectorPath u 
connIsosceles2 u p1@(P2 x1 y1) p2@(P2 x2 y2) = 
    traceLinePoints $ 
      [ p1, mid1 .+^ avec perp_ang u, mid2 .-^ avec perp_ang u, p2 ]
  where
    mid1      = P2 (x1 + 0.33*(x2-x1)) (y1 + 0.33*(y2-y1))
    mid2      = P2 (x1 + 0.66*(x2-x1)) (y1 + 0.66*(y2-y1))
    perp_ang  = (pi*0.5) + direction (pvec p1 p2) 


-- | /Triangular/ joint.
-- 
-- @u@ is the altitude of the triangle.
--
connLightningBolt :: (Real u, Floating u) => u -> ConnectorPath u 
connLightningBolt u p1@(P2 x1 y1) p2@(P2 x2 y2) = 
    traceLinePoints $ 
      [ p1, mid_pt .+^ avec perp_ang u, mid_pt .-^ avec perp_ang u, p2]
  where
    mid_pt    = P2 (x1 + 0.5*(x2-x1)) (y1 + 0.5*(y2-y1))
    perp_ang  = (pi*0.5) + direction (pvec p1 p2) 


--------------------------------------------------------------------------------



-- OLD ...
--
curveconn :: (Floating u, Ord u) => Radian -> Radian -> ConnectorPath u
curveconn r1 r2 p1 p2 = curveByAngles p1 r1 r2 p2
