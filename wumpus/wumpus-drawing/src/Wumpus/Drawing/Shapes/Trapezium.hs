{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Drawing.Shapes.Trapezium
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Trapezium.
--
-- Note cardinal anchors correspond directly to the compass 
-- positions.
-- 
--------------------------------------------------------------------------------

module Wumpus.Drawing.Shapes.Trapezium
  ( 

    Trapezium
  , DTrapezium
  , trapezium
  , ztrapezium


  ) where

import Wumpus.Drawing.Geometry.Intersection
import Wumpus.Drawing.Geometry.Paths
import Wumpus.Drawing.Paths
import Wumpus.Drawing.Shapes.Base

import Wumpus.Basic.Kernel                      -- package: wumpus-basic

import Wumpus.Core                              -- package: wumpus-core


import Control.Applicative




--------------------------------------------------------------------------------
-- Diamond

-- | A trapezium.
--
data Trapezium u = Trapezium 
      { tz_ctm          :: ShapeCTM u
      , tz_base_width   :: !u
      , tz_height       :: !u
      , tz_base_l_ang   :: Radian
      , tz_base_r_ang   :: Radian
      }


type DTrapezium = Trapezium Double

type instance DUnit (Trapezium u) = u


instance (Real u, Floating u) => CenterAnchor (Trapezium u) where
  center = ctmCenter . tz_ctm



instance (Real u, Floating u) => CardinalAnchor (Trapezium u) where
  north = tzRadialAnchor (0.5*pi)
  south = \tz -> let hh = 0.5 * tz_height tz
                 in projectPoint (P2 0 (-hh)) (tz_ctm tz)
  east  = tzRadialAnchor 0
  west  = tzRadialAnchor pi




instance (Real u, Floating u) => CardinalAnchor2 (Trapezium u) where
  northeast = tzRadialAnchor (0.25*pi)
  southeast = tzRadialAnchor (1.75*pi)
  southwest = tzRadialAnchor (1.25*pi)
  northwest = tzRadialAnchor (0.75*pi)



instance (Real u, Floating u) => RadialAnchor (Trapezium u) where
   radialAnchor = tzRadialAnchor


tzRadialAnchor :: (Real u, Floating u) 
                  => Radian -> Trapezium u -> Point2 u
tzRadialAnchor theta (Trapezium { tz_ctm        = ctm
                                , tz_base_width = bw
                                , tz_height     = h
                                , tz_base_l_ang = lang
                                , tz_base_r_ang = rang }) =
    maybe ctr id $ findIntersect ctr theta $ polygonLines ps
  where 
    ps  = tzPoints bw h lang rang ctm 
    ctr = ctmCenter ctm
    
    




-- | 'trapezium'  : @ base_width * height * bottom_left_ang * 
--     bottom_right_ang -> Trapezium @
--
--
trapezium :: (Real u, Floating u, FromPtSize u) 
                  => u -> u -> Radian -> Radian -> LocShape u (Trapezium u)
trapezium bw h lang rang = 
    intoLocShape (mkTrapezium bw h lang rang) (mkTrapeziumPath bw h lang rang)


-- | 'ztrapezium'  : @ base_width * height -> Trapezium @
--
--
ztrapezium :: (Real u, Floating u, FromPtSize u) 
           => u -> u -> LocShape u (Trapezium u)
ztrapezium bw h = trapezium bw h ang ang
  where
    ang = d2r (60::Double)

{-
-- Note - we can draw a trapezium with this code - **but** it has 
-- a very bad notion of center...

parallelogram :: (Real u, Floating u, FromPtSize u) 
              => u -> u -> Radian -> LocShape u (Trapezium u)
parallelogram bw h lang = trapezium bw h lang (pi - lang) 

zparallelogram :: (Real u, Floating u, FromPtSize u) 
              => u -> u -> LocShape u (Trapezium u)
zparallelogram bw h = trapezium bw h ((1/3) * pi) ((2/3) * pi)
-}

--------------------------------------------------------------------------------


mkTrapezium :: (Real u, Fractional u) 
            => u -> u -> Radian -> Radian -> LocCF u (Trapezium u)
mkTrapezium bw h lang rang = promoteR1 $ \ctr -> 
    pure $ Trapezium { tz_ctm           = makeShapeCTM ctr
                     , tz_base_width    = bw
                     , tz_height        = h
                     , tz_base_l_ang    = lang
                     , tz_base_r_ang    = rang
                     }


mkTrapeziumPath :: (Real u, Floating u, FromPtSize u) 
                => u -> u -> Radian -> Radian -> LocCF u (Path u)
mkTrapeziumPath bw h lang rang = promoteR1 $ \ctr -> 
    roundCornerShapePath $ tzPath bw h lang rang ctr


tzPath :: (Real u, Floating u) 
       => u -> u -> Radian -> Radian -> LocCoordPath u
tzPath bw h lang rang (P2 x y) = [ bl, br, tr, tl ]
  where
    half_base = 0.5 * bw
    hh        = 0.5 * h
    br        = P2 (x + half_base ) (y - hh)
    bl        = P2 (x - half_base ) (y - hh)
    tr        = displaceVec (rightSideVec h rang) br
    tl        = displaceVec (leftSideVec h lang) bl


tzPoints :: (Real u, Floating u) 
               => u -> u -> Radian -> Radian -> ShapeCTM u -> [Point2 u]
tzPoints bw h lang rang ctm = map (projectPoint `flip` ctm) [ bl, br, tr, tl ]
  where
    half_base = 0.5 * bw
    hh        = 0.5 * h
    bl        = P2 (-half_base) (-hh)
    br        = P2 half_base    (-hh)
    tr        = displaceVec (rightSideVec h rang) br
    tl        = displaceVec (leftSideVec h lang) bl



-- | Calculate the vector that produces the upper-left point given
-- the lower-left point.
--
-- Note - expects ang value 0 < ang < 180, though does not check...
-- 
leftSideVec :: Floating u => u -> Radian -> Vec2 u
leftSideVec h ang | ang <  0.5*pi = less_ninety
                  | ang == 0.5*pi = vvec h
                  | otherwise     = grtr_ninety
  where
    less_ninety = let dist = h / (fromRadian $ sin ang) in avec ang dist
    grtr_ninety = let theta = ang - (0.5*pi) 
                      dist  = h / (fromRadian $ cos theta) 
                  in avec ang dist




-- | Calculate the vector that produces the upper-left point given
-- the lower-left point.
--
-- Note - expects ang value 0 < ang < 180, though does not check...
-- 
rightSideVec :: Floating u => u -> Radian -> Vec2 u
rightSideVec h ang | ang <  0.5*pi = less_ninety
                   | ang == 0.5*pi = vvec h
                   | otherwise     = grtr_ninety
  where
    less_ninety = let dist = h / (fromRadian $ sin ang) in avec (pi-ang) dist
    grtr_ninety = let theta = ang - (0.5*pi) 
                      dist  = h / (fromRadian $ cos theta) 
                  in avec (pi-ang) dist

