{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Drawing.Shapes.Semicircle
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Semicircle. 
-- 
--------------------------------------------------------------------------------

module Wumpus.Drawing.Shapes.Semicircle
  ( 

    Semicircle
  , DSemicircle
  , semicircle

  ) where

import Wumpus.Drawing.Paths.Absolute
import Wumpus.Drawing.Shapes.Base

import Wumpus.Basic.Geometry.Base               -- package: wumpus-basic
import Wumpus.Basic.Geometry.Intersection
import Wumpus.Basic.Kernel

import Wumpus.Core                              -- package: wumpus-core

import Data.AffineSpace                         -- package: vector-space


import Control.Applicative



--------------------------------------------------------------------------------
-- Datatype

data Semicircle u = Semicircle 
      { sc_ctm          :: ShapeCTM u
      , sc_radius       :: !u 
      , sc_syn_props    :: SyntheticProps u
      }

type instance DUnit (Semicircle u) = u

-- | rect_width is the width of the (greater) enclosing rectangle.
data SyntheticProps u = SyntheticProps
      { sc_ctr_minor  :: u
      , sc_ctr_major  :: u
      }

type instance DUnit (SyntheticProps u) = u

  
type DSemicircle = Semicircle Double


instance Functor Semicircle where
  fmap f (Semicircle ctm r props) = Semicircle (fmap f ctm) (f r) (fmap f props)

instance Functor SyntheticProps where
  fmap f (SyntheticProps cmin cmaj) = SyntheticProps (f cmin) (f cmaj)


--------------------------------------------------------------------------------
-- Affine trans

mapCTM :: (ShapeCTM u -> ShapeCTM u) -> Semicircle u -> Semicircle u
mapCTM f = (\s i -> s { sc_ctm = f i }) <*> sc_ctm


instance (Real u, Floating u) => Rotate (Semicircle u) where
  rotate ang            = mapCTM (rotate ang)
              
instance (Real u, Floating u) => RotateAbout (Semicircle u) where
  rotateAbout ang pt    = mapCTM (rotateAbout ang pt)

instance Fractional u => Scale (Semicircle u) where
  scale sx sy           = mapCTM (scale sx sy)

instance InterpretUnit u => Translate (Semicircle u) where
  translate dx dy       = mapCTM (translate dx dy)


--------------------------------------------------------------------------------
-- Anchors

-- | 'runDisplaceCenter' : @ ( radius
--                           * height_minor 
--                           * height_major -> Vec ) * semicircle -> Point @
--
runDisplaceCenter :: (Real u, Floating u) 
                  => (u -> u -> u -> Vec2 u) -> Semicircle u -> Anchor u
runDisplaceCenter fn (Semicircle { sc_ctm       = ctm
                                 , sc_radius    = radius
                                 , sc_syn_props = syn    }) = 
    projectFromCtr (fn radius (sc_ctr_minor syn) (sc_ctr_major syn)) ctm


instance (Real u, Floating u) => 
    CenterAnchor (Semicircle u) where
  center = runDisplaceCenter $ \_ _ _ -> V2 0 0

instance (Real u, Floating u) => 
    ApexAnchor (Semicircle u) where
  apex = runDisplaceCenter $ \_ _    cmaj -> V2 0  cmaj

instance (Real u, Floating u) => 
    BottomCornerAnchor (Semicircle u) where
  bottomLeftCorner  = runDisplaceCenter $ \r hminor _  -> V2 (-r) (-hminor)
  bottomRightCorner = runDisplaceCenter $ \r hminor _  -> V2  r   (-hminor)

instance (Real u, Floating u) => 
    CardinalAnchor (Semicircle u) where
  north = apex
  south = runDisplaceCenter $ \_ cmin _    -> V2 0  (-cmin)
  east  = runDisplaceCenter $ \r cmin _    -> let x = pyth r cmin in V2 x 0
  west  = runDisplaceCenter $ \r cmin _    -> let x = pyth r cmin in V2 (-x) 0

-- | Use Pythagoras formula for working out the /east/ and /west/
-- distances. A right-triangle is formed below the centroid, 
-- radius is the hypotenuese, hminor is the other side.
--
pyth :: Floating u => u -> u -> u
pyth hyp s1 = sqrt $ pow2 hyp - pow2 s1
  where
    pow2 = (^ (2::Int))


instance (Real u, Floating u, Tolerance u) => 
    CardinalAnchor2 (Semicircle u) where
  northeast = radialAnchor (0.25*pi)
  southeast = radialAnchor (1.75*pi)
  southwest = radialAnchor (1.25*pi)
  northwest = radialAnchor (0.75*pi)




instance (Real u, Floating u, Tolerance u) => 
    RadialAnchor (Semicircle u) where
  radialAnchor theta = runDisplaceCenter (scRadialVec theta)

-- helpers

scRadialVec :: (Real u, Floating u, Ord u, Tolerance u)
            => Radian -> u -> u -> u -> Vec2 u
scRadialVec theta radius hminor _ = go theta
  where
    (lang,rang)                     = baselineRange radius hminor
    (bctr, br, _, bl)               = constructionPoints radius hminor
    plane                           = makePlane zeroPt theta
    base_line                       = LineSegment bl br
    left_curve                      = mkCurve radius half_pi bctr
    right_curve                     = mkCurve radius 0 bctr
    post                            = maybe (V2 0 0) (\(P2 x y) -> V2 x y)
    go a | lang <= a && a <= rang   = post $ interLinesegLine base_line plane 
         | half_pi <= a && a < lang = post $ interCurveLine left_curve plane
         | otherwise                = post $ interCurveLine right_curve plane


mkCurve :: Floating u => u -> Radian -> Point2 u -> BezierCurve u
mkCurve radius theta ctr = BezierCurve p0 p1 p2 p3
  where
    (BezierCurve p0 p1 p2 p3) = bezierMinorArc half_pi radius theta ctr



-- | 'constructionPoints' : @ radius * hminor -> 
--     (base_ctr, base_right, apex, base_left) @
--
-- Assumes centroid is (0,0).
--
constructionPoints :: Num u 
                   => u -> u -> (Point2 u, Point2 u, Point2 u, Point2 u)
constructionPoints radius hminor = (bctr, br, apx, bl)
  where
    bctr  = P2 0 (-hminor)
    br    = bctr .+^ hvec radius
    apx   = bctr .+^ vvec radius
    bl    = bctr .-^ hvec radius




-- | 'baselineRange' : @ radius * hminor -> (left_base_ang, right_base_ang) @
--
-- Find the angle range where a ray from the centroid will cross
-- the baseline rather than cut the curve.
--
baselineRange :: (Real u, Floating u) => u -> u -> (Radian, Radian)
baselineRange radius hminor = (lang, rang)
  where
    ang   = toRadian $ atan (radius / hminor)
    lang  = (1.5*pi) - ang
    rang  = (1.5*pi) + ang

--------------------------------------------------------------------------------
-- Construction

-- | 'semicircle'  : @ radius -> Shape @
--
semicircle :: (Real u, Floating u, InterpretUnit u, Tolerance u) 
           => u -> Shape Semicircle u
semicircle radius = 
    let props = synthesizeProps radius
    in makeShape (mkSemicircle radius props) 
                 (mkSemicirclePath radius (sc_ctr_minor props))
          

synthesizeProps :: Floating u => u -> SyntheticProps u
synthesizeProps radius = 
    SyntheticProps { sc_ctr_minor  = cminor
                   , sc_ctr_major  = cmajor
                   }
  where
    cminor = (4 * radius) / (3 * pi)
    cmajor = radius - cminor


mkSemicircle :: InterpretUnit u
             => u -> SyntheticProps u -> LocThetaQuery u (Semicircle u)
mkSemicircle radius props = promoteR2 $ \ctr theta -> 
    pure $ Semicircle { sc_ctm = makeShapeCTM ctr theta
                      , sc_radius = radius
                      , sc_syn_props = props 
                      }



-- TODO - need to check other shapes to see if the are deriving 
-- the center properly...
--
mkSemicirclePath :: (Real u, Floating u, InterpretUnit u, Tolerance u) 
                 => u -> u -> LocThetaQuery u (AbsPath u)
mkSemicirclePath radius cminor = promoteR2 $ \pt theta ->
    let ctr = displacePerpendicular (-cminor) theta pt
    in pure $ curvePath $ bezierArcPoints pi radius theta ctr 

