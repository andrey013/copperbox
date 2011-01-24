{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleContexts           #-}
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
-- Semiellipse - note some Anchor instances are TODO. 
-- Simple shapes - rectangle, circle diamond, ellipse.
-- 
--------------------------------------------------------------------------------

module Wumpus.Drawing.Shapes.Semiellipse
  ( 

    Semiellipse
  , DSemiellipse
  , semiellipse

  ) where

import Wumpus.Drawing.Paths
import Wumpus.Drawing.Shapes.Base

import Wumpus.Basic.Kernel                      -- package: wumpus-basic

import Wumpus.Core                              -- package: wumpus-core

import Data.AffineSpace                         -- package: vector-space

import Control.Applicative



--------------------------------------------------------------------------------
-- Semi-ellipse

data Semiellipse u = Semiellipse 
      { se_ctm          :: ShapeCTM u
      , se_rx           :: !u 
      , se_ry           :: !u
      , se_syn_props    :: SyntheticProps u
      }



-- | rect_width is the width of the (greater) enclosing rectangle.
data SyntheticProps u = SyntheticProps
      { se_ry_minor  :: u
      , se_ry_major  :: u
      }
  
type DSemiellipse = Semiellipse Double

type instance DUnit (Semiellipse u) = u


--------------------------------------------------------------------------------
-- Affine trans

mapCTM :: (ShapeCTM u -> ShapeCTM u) -> Semiellipse u -> Semiellipse u
mapCTM f = (\s i -> s { se_ctm = f i }) <*> se_ctm

instance Num u => Scale (Semiellipse u) where
  scale sx sy = mapCTM (scale sx sy)


instance Rotate (Semiellipse u) where
  rotate ang = mapCTM (rotate ang)
                  

instance (Real u, Floating u) => RotateAbout (Semiellipse u) where
  rotateAbout ang pt = mapCTM (rotateAbout ang pt)


instance Num u => Translate (Semiellipse u) where
  translate dx dy = mapCTM (translate dx dy)


--------------------------------------------------------------------------------
-- Anchors



runSemiellipse :: (u -> u -> u -> u -> ShapeCTM u -> a) -> Semiellipse u -> a
runSemiellipse fn (Semiellipse { se_ctm       = ctm
                               , se_rx        = rx
                               , se_ry        = ry
                               , se_syn_props = syn    }) = 
    fn rx ry (se_ry_minor syn) (se_ry_major syn) ctm




instance (Real u, Floating u) => CenterAnchor (Semiellipse u) where
  center = ctmCenter . se_ctm



instance (Real u, Floating u) => CardinalAnchor (Semiellipse u) where
  north = northAnchor
  south = southAnchor
  east _ = error $ "semiellipse - no east anchor."
  west _ = error $ "semiellipse - no west anchor."


northAnchor :: (Real u, Floating u) => Semiellipse u -> Point2 u
northAnchor = runSemiellipse $ \_ _ _ rymajor ->
    projectPoint $ P2 0 rymajor


southAnchor :: (Real u, Floating u) => Semiellipse u -> Point2 u
southAnchor = runSemiellipse $ \_ _ ryminor _ ->
    projectPoint $ P2 0 (-ryminor)


{-
pyth :: Floating u => u -> u -> u
pyth hyp s1 = sqrt $ pow2 hyp - pow2 s1
  where
    pow2 = (^ (2::Int))
-}


-- TODO - Radial and Cardinal2 instances


--------------------------------------------------------------------------------
-- Construction


-- | 'semiellipse'  : @ x_radius * y_radius -> Shape @
--
semiellipse :: (Real u, Floating u) 
            => u -> u -> Shape u (Semiellipse u)
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


mkSemiellipse :: Num u 
              => u -> u -> SyntheticProps u -> LocCF u (Semiellipse u)
mkSemiellipse rx ry props = promoteR1 $ \ctr -> 
    pure $ Semiellipse { se_ctm = makeShapeCTM ctr
                       , se_rx = rx
                       , se_ry = ry
                       , se_syn_props = props 
                       }


mkSemiellipsePath :: (Floating u, Ord u) 
                  => u -> u -> u -> LocCF u (Path u)
mkSemiellipsePath rx ry cminor = promoteR1 $ \(P2 x y) ->
    pure $ traceCurvePoints $ bezierSemiEllipse rx ry (P2 x (y - cminor))



-- For Geometry?

-- Note - Point is the (full) ellipse center.
--
bezierSemiEllipse :: (Fractional u, Floating u) 
              => u -> u -> Point2 u -> [Point2 u]
bezierSemiEllipse rx ry (P2 x y) = 
    [ p00,c01,c02, p03,c04,c05, p06 ]
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
