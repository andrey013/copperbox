{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.Kernel.Base.Anchors
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Anchor points on shapes, bounding boxes, etc.
--
-- Anchors are addressable positions, an examplary use is taking
-- anchors on node shapes to get the start and end points for 
-- connectors in a network (graph) diagram.
-- 
--------------------------------------------------------------------------------

module Wumpus.Basic.Kernel.Base.Anchors
  ( 

  -- * Anchors
    CenterAnchor(..)
  , CardinalAnchor(..)
  , CardinalAnchor2(..)
  , RadialAnchor(..)

  -- * Extended anchor points
  , northwards
  , southwards
  , eastwards
  , westwards
  , northeastwards
  , southeastwards
  , southwestwards
  , northwestwards

  , radialConnectorPoints

  ) where

import Wumpus.Core                      -- package: wumpus-core

import Data.AffineSpace                 -- package: vector-space


-- | Center of an object.
--
class CenterAnchor t where
  center :: DUnit t ~ u => t -> Point2 u

-- | Cardinal (compass) positions on an object. 
-- 
-- Note - in TikZ cardinal anchors are not necessarily at the
-- equivalent radial position, for instance reactangle north-east
-- is the top-right corner whether or not this is incident at 
-- 45deg.
--
-- Wumpus generally follows the TikZ convention.
--
class CardinalAnchor t where
  north :: DUnit t ~ u => t -> Point2 u
  south :: DUnit t ~ u => t -> Point2 u
  east  :: DUnit t ~ u => t -> Point2 u
  west  :: DUnit t ~ u => t -> Point2 u


-- | Secondary group of cardinal (compass) positions on an object. 
-- 
-- It seems possible that for some objects defining the primary
-- compass points (north, south,...) will be straight-forward 
-- whereas defining the secondary compass points may be 
-- problemmatic, hence the compass points are split into two 
-- classes.
--
class CardinalAnchor2 t where
  northeast :: DUnit t ~ u => t -> Point2 u
  southeast :: DUnit t ~ u => t -> Point2 u
  southwest :: DUnit t ~ u => t -> Point2 u
  northwest :: DUnit t ~ u => t -> Point2 u


-- | Anchor on a border that can be addressed by an angle.
--
-- The angle is counter-clockwise from the right-horizontal, i.e.
-- 0 is /east/.
--
class RadialAnchor t where
  radialAnchor :: DUnit t ~ u => Radian -> t -> Point2 u




extendPtDist :: (Real u, Floating u) => u -> Point2 u -> Point2 u -> Point2 u
extendPtDist d p1 p2 = let v   = pvec p1 p2
                           ang = direction v
                           len = vlength v
                       in p1 .+^ avec ang (len+d)


-- | 'northwards' : @ dist * object -> Point @
-- 
-- Project the anchor along a line from the center that goes 
-- through the north anchor. 
--
-- If the distance is zero the answer with be the north anchor.
--
-- If the distance is negative the answer within the object before 
-- the north anchor.
--
-- If the distance is positive the anchor outside the object.
--
northwards :: ( Real u, Floating u, CenterAnchor t, CardinalAnchor t
              , u ~ DUnit t ) 
           => u -> t -> Point2 u
northwards u a = extendPtDist u (center a) (north a)


-- | 'southwards' : @ dist * object -> Point @
-- 
-- Variant of the function 'northwards', but projecting the line 
-- southwards from the center of the object.
--
southwards :: ( Real u, Floating u, CenterAnchor t, CardinalAnchor t
              , u ~ DUnit t ) 
           => u -> t -> Point2 u
southwards u a = extendPtDist u (center a) (south a)


-- | 'eastwards' : @ dist * object -> Point @
-- 
-- Variant of the function 'northwards', but projecting the line 
-- eastwards from the center of the object.
--
eastwards :: ( Real u, Floating u, CenterAnchor t, CardinalAnchor t
             , u ~ DUnit t ) 
          => u -> t -> Point2 u
eastwards u a = extendPtDist u (center a) (east a)


-- | 'westwards' : @ dist * object -> Point @
-- 
-- Variant of the function 'northwards', but projecting the line 
-- westwards from the center of the object.
--
westwards :: ( Real u, Floating u, CenterAnchor t, CardinalAnchor t
             , u ~ DUnit t ) 
          => u -> t -> Point2 u
westwards u a = extendPtDist u (center a) (west a)


-- | 'northeastwards' : @ dist * object -> Point @
-- 
-- Variant of the function 'northwards', but projecting the line 
-- northeastwards from the center of the object.
--
northeastwards :: ( Real u, Floating u, CenterAnchor t, CardinalAnchor2 t
                  , u ~ DUnit t ) 
               => u -> t -> Point2 u
northeastwards u a = extendPtDist u (center a) (northeast a)


-- | 'southeastwards' : @ dist * object -> Point @
-- 
-- Variant of the function 'northwards', but projecting the line 
-- southeastwards from the center of the object.
--
southeastwards :: ( Real u, Floating u, CenterAnchor t, CardinalAnchor2 t
                  , u ~ DUnit t ) 
               => u -> t -> Point2 u
southeastwards u a = extendPtDist u (center a) (southeast a)


-- | 'southwestwards' : @ dist * object -> Point @
-- 
-- Variant of the function 'northwards', but projecting the line 
-- southwestwards from the center of the object.
--
southwestwards :: ( Real u, Floating u, CenterAnchor t, CardinalAnchor2 t
                  , u ~ DUnit t ) 
               => u -> t -> Point2 u
southwestwards u a = extendPtDist u (center a) (southwest a)


-- | 'northwestwards' : @ dist * object -> Point @
-- 
-- Variant of the function 'northwards', but projecting the line 
-- northwestwards from the center of the object.
--
northwestwards :: ( Real u, Floating u, CenterAnchor t, CardinalAnchor2 t
                  , u ~ DUnit t ) 
               => u -> t -> Point2 u
northwestwards u a = extendPtDist u (center a) (northwest a)


--------------------------------------------------------------------------------

-- | 'radialConnectorPoints' : @ object_a * object_b -> (Point_a, Point_b) @
--
-- Find the radial connectors points for objects @a@ and @b@ along
-- the line joining their centers.
--
radialConnectorPoints :: ( Real u, Floating u
                         , CenterAnchor t1, RadialAnchor t1
                         , CenterAnchor t2, RadialAnchor t2
                         , u ~ DUnit t1, DUnit t1 ~ DUnit t2 ) 
                      => t1 -> t2 -> (Point2 u, Point2 u) 
radialConnectorPoints a b = (radialAnchor theta a, radialAnchor (theta+pi) b)
  where
    theta = direction $ pvec (center a) (center b)
    

--------------------------------------------------------------------------------
-- Instances 

instance Fractional u => CenterAnchor (BoundingBox u) where
  center (BBox (P2 xl ylo) (P2 xr yhi)) = P2 x y 
     where
       x = xl+0.5*(xr-xl)
       y = ylo+0.5*(yhi-ylo)
       

instance Fractional u => CardinalAnchor (BoundingBox u) where
  north (BBox (P2 xl _  ) (P2 xr yhi)) = P2 (xl+0.5*(xr-xl)) yhi
  south (BBox (P2 xl ylo) (P2 xr _  )) = P2 (xl+0.5*(xr-xl)) ylo
  east  (BBox (P2 _  ylo) (P2 xr yhi)) = P2 xr (ylo+0.5*(yhi-ylo))
  west  (BBox (P2 xl ylo) (P2 _  yhi)) = P2 xl (ylo+0.5*(yhi-ylo))


instance Fractional u => CardinalAnchor2 (BoundingBox u) where
  northeast (BBox _ ur)                 = ur
  southeast (BBox (P2 _ ylo) (P2 xr _)) = P2 xr ylo
  southwest (BBox ll _)                 = ll
  northwest (BBox (P2 xl _) (P2 _ yhi)) = P2 xl yhi 

