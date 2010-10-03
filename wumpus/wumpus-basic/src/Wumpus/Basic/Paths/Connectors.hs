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

    Connector
  , connect
  , vhconn
  , hvconn
  , vhvconn
  , hvhconn
  , curveconn
  , joint



  ) where

import Wumpus.Basic.Paths.Base

import Wumpus.Core                              -- package: wumpus-core

import Data.AffineSpace                         -- package: vector-space

import Prelude hiding ( length )

type Connector u = Point2 u -> Point2 u -> Path u

connect :: Floating u => Connector u
connect = line

vhconn :: Floating u => Connector u
vhconn p1@(P2 x1 _) p2@(P2 _ y2) = 
    let mid = P2 x1 y2 in tracePoints [p1, mid, p2]

hvconn :: Floating u => Connector u
hvconn p1@(P2 _ y1) p2@(P2 x2 _) = 
    let mid = P2 x2 y1 in tracePoints [p1, mid, p2]

vhvconn :: Floating u => u -> Connector u
vhvconn v p1@(P2 x1 _) p2@(P2 x2 _) = tracePoints [p1, a1, a2, p2]
  where
    a1 = p1 .+^ vvec v
    a2 = a1 .+^ hvec (x2 - x1)


hvhconn :: Floating u => u -> Connector u
hvhconn h p1@(P2 _ y1) p2@(P2 _ y2) = tracePoints [p1,a1,a2,p2]
  where
    a1 = p1 .+^ hvec h
    a2 = a1 .+^ vvec (y2 - y1)

curveconn :: (Floating u, Ord u) => Radian -> Radian -> Connector u
curveconn r1 r2 p1 p2 = curveByAngles p1 r1 r2 p2


joint :: (Real u, Floating u) => u -> Connector u 
joint u p1@(P2 x1 y1) p2@(P2 x2 y2) = 
    tracePoints [p1, mid_pt .+^ avec perp_ang u, p2]
  where
    mid_pt    = P2 (x1 + 0.5*(x2-x1)) (y1 + 0.5*(y2-y1))
    perp_ang  = (pi*0.5) + direction (pvec p2 p1) 




--------------------------------------------------------------------------------


