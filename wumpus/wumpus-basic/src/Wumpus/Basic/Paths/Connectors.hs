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
-- Extended path type - more amenable for complex drawings than
-- the type in Wumpus-Core.
--
-- \*\* WARNING \*\* this module is an experiment, and may 
-- change significantly or even be dropped from future revisions.
-- 
--------------------------------------------------------------------------------

module Wumpus.Basic.Paths.Connectors 
  ( 

    ConnPath
  , connectS
  , vhconn
  , hvconn
  , arbv
  , arbh
  , curveconn
  , joint
  , pathGraphic
  , fillPath

  , shorten
  , midpoint

  ) where

import Wumpus.Basic.Graphic
import Wumpus.Basic.Paths.Base
import Wumpus.Basic.Paths.Construction

import Wumpus.Core                              -- package: wumpus-core

import Data.AffineSpace                         -- package: vector-space

import Prelude hiding ( length )

type ConnPath u = Point2 u -> Point2 u -> Path u

connectS :: Floating u => ConnPath u
connectS = \p1 p2 -> execPath p1 $ lineto p2

vhconn :: Floating u => ConnPath u
vhconn p1 p2 = execPath p1 $ verticalHorizontal p2

hvconn :: Floating u => ConnPath u
hvconn p1 p2 = execPath p1 $ horizontalVertical p2

arbv :: Floating u => u -> ConnPath u
arbv v p1@(P2 x1 y1) (P2 x2 y2) = execPath p1 $ vline v >> hline dx >> vline dy
  where
    dx = x2 - x1
    dy = y2 - (y1+v)


arbh :: Floating u => u -> ConnPath u
arbh h p1@(P2 x1 y1) (P2 x2 y2) = execPath p1 $ hline h >> vline dy >> hline dx
  where
    dx = x2 - (x1+h)
    dy = y2 - y1

curveconn :: (Floating u, Ord u) => Radian -> Radian -> ConnPath u
curveconn r1 r2 p1 p2 = execPath p1 $ curveto r1 r2 p2


joint :: (Real u, Floating u) => u -> ConnPath u 
joint u p1@(P2 x1 y1) p2@(P2 x2 y2) = 
    execPath p1 $ lineto (mid_pt .+^ avec perp_ang u) >> lineto p2
  where
    mid_pt    = P2 (x1 + 0.5*(x2-x1)) (y1 + 0.5*(y2-y1))
    perp_ang  = (pi*0.5) + direction (pvec p2 p1) 

-- This one might be more useful...
-- 
-- ... No - can\'t a add tips to this one.
--
pathGraphic :: Num u => ConnPath u -> ConnGraphic u
pathGraphic bpath = \p1 p2 -> openStroke $ toPrimPathU $ bpath p1 p2


-- Mind out for name clash...

-- | Closes and fills a path
--
fillPath :: Num u => Path u -> Graphic u
fillPath = filledPath . toPrimPathU



shorten  :: (Real u, Floating u, Ord u) => u -> Path u -> Path u
shorten u p = shortenL u $ shortenR u p

--------------------------------------------------------------------------------

-- This should return direction as well...
--
midpoint :: (Real u, Floating u) => Path u -> Maybe (Point2 u)
midpoint pa = let u = length pa in
  if u == 0 then Nothing else tipR $ shortenR (u/2) pa

