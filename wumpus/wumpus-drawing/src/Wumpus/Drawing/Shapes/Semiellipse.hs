{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Drawing.Shapes.Semiellipse
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Semiellipse.
-- 
--------------------------------------------------------------------------------

module Wumpus.Drawing.Shapes.Semiellipse
  ( 

    Semiellipse
  , DSemiellipse
  , semiellipse

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

data Semiellipse u = Semiellipse 
      { se_ctm          :: ShapeCTM u
      , se_rx           :: !u 
      , se_ry           :: !u
      , se_syn_props    :: SyntheticProps u
      }

type instance DUnit (Semiellipse u) = u

-- | rect_width is the width of the (greater) enclosing rectangle.
data SyntheticProps u = SyntheticProps
      { se_ry_minor  :: u
      , se_ry_major  :: u
      }

type instance DUnit (SyntheticProps u) = u
  
type DSemiellipse = Semiellipse Double

instance Functor Semiellipse where
  fmap f (Semiellipse ctm rx ry props) = 
    Semiellipse (fmap f ctm) (f rx) (f ry) (fmap f props)

instance Functor SyntheticProps where
  fmap f (SyntheticProps rymin rymaj) = SyntheticProps (f rymin) (f rymaj)

--------------------------------------------------------------------------------
-- Affine trans

mapCTM :: (ShapeCTM u -> ShapeCTM u) -> Semiellipse u -> Semiellipse u
mapCTM f = (\s i -> s { se_ctm = f i }) <*> se_ctm


instance (Real u, Floating u) => Rotate (Semiellipse u) where
  rotate ang            = mapCTM (rotate ang)
              
instance (Real u, Floating u) => RotateAbout (Semiellipse u) where
  rotateAbout ang pt    = mapCTM (rotateAbout ang pt)

instance Fractional u => Scale (Semiellipse u) where
  scale sx sy           = mapCTM (scale sx sy)

instance InterpretUnit u => Translate (Semiellipse u) where
  translate dx dy       = mapCTM (translate dx dy)


--------------------------------------------------------------------------------
-- Anchors


-- | 'runDisplaceCenter' : @ ( rx
--                           * ry 
--                           * ry_minor 
--                           * ry_major -> Vec ) * semiellipse -> Point @
--
runDisplaceCenter :: (Real u, Floating u) 
                  => (u -> u -> u -> u -> Vec2 u) -> Semiellipse u -> Anchor u
runDisplaceCenter fn (Semiellipse { se_ctm       = ctm
                                  , se_rx        = rx
                                  , se_ry        = ry
                                  , se_syn_props = syn    }) = 
    projectFromCtr (fn rx ry (se_ry_minor syn) (se_ry_major syn)) ctm




instance (Real u, Floating u) => 
    CenterAnchor (Semiellipse u) where
  center = runDisplaceCenter $ \_ _ _ _ -> V2 0 0

instance (Real u, Floating u, LengthTolerance u) => 
    ApexAnchor (Semiellipse u) where
  apex = runDisplaceCenter $ \_ _ _ ry_major -> V2 0 ry_major

instance (Real u, Floating u) => 
    BottomCornerAnchor (Semiellipse u) where
  bottomLeftCorner  = runDisplaceCenter $ \rx _ ry_minor _  -> V2 (-rx) (-ry_minor)
  bottomRightCorner = runDisplaceCenter $ \rx _ ry_minor _  -> V2  rx   (-ry_minor)


instance (Real u, Floating u, LengthTolerance u) => 
    CardinalAnchor (Semiellipse u) where
  north = apex
  south = runDisplaceCenter $ \_ _ ry_minor _ -> V2 0 (-ry_minor)
  east  = radialAnchor 0
  west  = radialAnchor pi

instance (Real u, Floating u, LengthTolerance u) => 
    CardinalAnchor2 (Semiellipse u) where
  northeast = radialAnchor (0.25*pi)
  southeast = radialAnchor (1.75*pi)
  southwest = radialAnchor (1.25*pi)
  northwest = radialAnchor (0.75*pi)



instance (Real u, Floating u, LengthTolerance u) => 
    RadialAnchor (Semiellipse u) where
  radialAnchor theta = runDisplaceCenter (seRadialVec theta)


seRadialVec :: (Real u, Floating u, Ord u, LengthTolerance u)
            => Radian -> u -> u -> u -> u -> Vec2 u
seRadialVec theta rx ry hminor _ = go theta
  where
    (lang,rang)                     = baselineRange rx hminor
    (bctr, br, _, bl)               = constructionPoints rx ry hminor
    plane                           = makePlane zeroPt theta
    base_line                       = LineSegment bl br
    (right_curve,left_curve)        = bezierSemiellipse rx ry bctr
    post                            = maybe (V2 0 0) (\(P2 x y) -> V2 x y)
    go a | lang <= a && a <= rang   = post $ interLinesegLine base_line plane 
         | half_pi <= a && a < lang = post $ interCurveLine left_curve plane
         | otherwise                = post $ interCurveLine right_curve plane



-- | 'constructionPoints' : @ rx * ry * hminor -> 
--     (base_ctr, base_right, apex, base_left) @
--
-- Assumes centroid is (0,0).
--
constructionPoints :: Num u 
                   => u -> u -> u -> (Point2 u, Point2 u, Point2 u, Point2 u)
constructionPoints rx ry hminor = (bctr, br, apx, bl)
  where
    bctr  = P2 0 (-hminor)
    br    = bctr .+^ hvec rx
    apx   = bctr .+^ vvec ry
    bl    = bctr .+^ hvec (-rx)




-- | 'baselineRange' : @ radius * hminor -> (left_base_ang, right_base_ang) @
--
-- Find the angle range where a ray from the centroid will cross
-- the baseline rather than cut the curve.
--
baselineRange :: (Real u, Floating u) => u -> u -> (Radian, Radian)
baselineRange rx hminor = (lang, rang)
  where
    ang   = toRadian $ atan (rx / hminor)
    lang  = (1.5*pi) - ang
    rang  = (1.5*pi) + ang



--------------------------------------------------------------------------------
-- Construction


-- | 'semiellipse'  : @ x_radius * y_radius -> Shape @
--
semiellipse :: (Real u, Floating u, InterpretUnit u, LengthTolerance u) 
            => u -> u -> Shape Semiellipse u
semiellipse rx ry = 
    let props = synthesizeProps ry
    in makeShape (mkSemiellipse rx ry props) 
                 (mkSemiellipsePath rx ry (se_ry_minor props))
          


synthesizeProps :: Floating u => u -> SyntheticProps u
synthesizeProps ry = 
    SyntheticProps { se_ry_minor  = ry_minor
                   , se_ry_major  = ry_major
                   }
  where
    ry_minor = (4 * ry) / (3 * pi)
    ry_major = ry - ry_minor


mkSemiellipse :: u -> u -> SyntheticProps u -> LocThetaQuery u (Semiellipse u)
mkSemiellipse rx ry props = promoteR2 $ \ctr theta -> 
    pure $ Semiellipse { se_ctm = makeShapeCTM ctr theta
                       , se_rx = rx
                       , se_ry = ry
                       , se_syn_props = props 
                       }


mkSemiellipsePath :: (Real u, Floating u, LengthTolerance u) 
                  => u -> u -> u -> LocThetaQuery u (AbsPath u)
mkSemiellipsePath rx ry cminor = promoteR2 $ \pt theta ->
    let ctr = displacePerpendicular (-cminor) theta pt
        xs  = bezierSemiellipsePoints rx ry ctr
    in return $ curvePath $ map (rotateAbout theta ctr) xs 


bezierSemiellipsePoints :: Floating u
                        => u -> u -> Point2 u -> [Point2 u]
bezierSemiellipsePoints rx ry pt = [ p0, c1,c2,p3, c4,c5,p6 ]
  where 
    (BezierCurve p0 c1 c2 p3, BezierCurve _ c4 c5 p6) = bezierSemiellipse rx ry pt


-- For Geometry?

-- | Generate the bezier arcs for quadrants I and II. 
--
-- Drawing is expected to proceed CCW.
-- 
-- Note - Point is the (full) ellipse center.
--
bezierSemiellipse :: Floating u
                  => u -> u -> Point2 u -> (BezierCurve u, BezierCurve u)
bezierSemiellipse rx ry (P2 x y) = 
    (BezierCurve p00 c01 c02 p03, BezierCurve p03 c04 c05 p06)
  where
    lrx = rx * kappa
    lry = ry * kappa
    p00 = P2 (x + rx) y
    c01 = p00 .+^ vvec lry
    c02 = p03 .+^ hvec lrx

    p03 = P2 x (y + ry) 
    c04 = p03 .+^ hvec (-lrx)
    c05 = p06 .+^ vvec lry

    p06 = P2 (x - rx) y



kappa :: Floating u => u
kappa = 4 * ((sqrt 2 - 1) / 3)
