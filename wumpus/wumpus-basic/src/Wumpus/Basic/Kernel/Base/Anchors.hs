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
-- Anchor points on shapes.
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

class CenterAnchor t where
  center :: DUnit t ~ u => t -> Point2 u

-- | Note - in TikZ cardinal anchors are not necessarily at the
-- equivalent radial position, for instance reactangle north-east
-- is the top-right corner whether or not this is incident at 
-- 45deg.
--
class CardinalAnchor t where
  north :: DUnit t ~ u => t -> Point2 u
  south :: DUnit t ~ u => t -> Point2 u
  east  :: DUnit t ~ u => t -> Point2 u
  west  :: DUnit t ~ u => t -> Point2 u

class CardinalAnchor2 t where
  northeast :: DUnit t ~ u => t -> Point2 u
  southeast :: DUnit t ~ u => t -> Point2 u
  southwest :: DUnit t ~ u => t -> Point2 u
  northwest :: DUnit t ~ u => t -> Point2 u


-- | Anchor on a border that can be identified with and angle.
--
class RadialAnchor t where
  radialAnchor :: DUnit t ~ u => Radian -> t -> Point2 u


extendPtDist :: (Real u, Floating u) => u -> Point2 u -> Point2 u -> Point2 u
extendPtDist d p1 p2 = let v   = pvec p1 p2
                           ang = direction v
                           len = vlength v
                       in p1 .+^ avec ang (len+d)



northwards :: ( Real u, Floating u, CenterAnchor t, CardinalAnchor t
              , u ~ DUnit t ) 
           => u -> t -> Point2 u
northwards u a = extendPtDist u (center a) (north a)


southwards :: ( Real u, Floating u, CenterAnchor t, CardinalAnchor t
              , u ~ DUnit t ) 
           => u -> t -> Point2 u
southwards u a = extendPtDist u (center a) (south a)

eastwards :: ( Real u, Floating u, CenterAnchor t, CardinalAnchor t
             , u ~ DUnit t ) 
          => u -> t -> Point2 u
eastwards u a = extendPtDist u (center a) (east a)

westwards :: ( Real u, Floating u, CenterAnchor t, CardinalAnchor t
             , u ~ DUnit t ) 
          => u -> t -> Point2 u
westwards u a = extendPtDist u (center a) (west a)


northeastwards :: ( Real u, Floating u, CenterAnchor t, CardinalAnchor2 t
                  , u ~ DUnit t ) 
               => u -> t -> Point2 u
northeastwards u a = extendPtDist u (center a) (northeast a)


southeastwards :: ( Real u, Floating u, CenterAnchor t, CardinalAnchor2 t
                  , u ~ DUnit t ) 
               => u -> t -> Point2 u
southeastwards u a = extendPtDist u (center a) (southeast a)

southwestwards :: ( Real u, Floating u, CenterAnchor t, CardinalAnchor2 t
                  , u ~ DUnit t ) 
               => u -> t -> Point2 u
southwestwards u a = extendPtDist u (center a) (southwest a)

northwestwards :: ( Real u, Floating u, CenterAnchor t, CardinalAnchor2 t
                  , u ~ DUnit t ) 
               => u -> t -> Point2 u
northwestwards u a = extendPtDist u (center a) (northwest a)


--------------------------------------------------------------------------------

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

