{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Drawing.Shapes.Ellipse
-- Copyright   :  (c) Stephen Tetley 2010-2011
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Ellipse shape.
-- 
--------------------------------------------------------------------------------

module Wumpus.Drawing.Shapes.Ellipse
  ( 


  -- * Ellipse
    Ellipse
  , DEllipse
  , ellipse


  ) where

import Wumpus.Drawing.Paths
import Wumpus.Drawing.Shapes.Base

import Wumpus.Basic.Kernel                      -- package: wumpus-basic

import Wumpus.Core                              -- package: wumpus-core


import Control.Applicative





--------------------------------------------------------------------------------
-- Ellipse


data Ellipse u = Ellipse
      { ell_ctm     :: ShapeCTM u 
      , ell_rx      :: !u
      , ell_ry      :: !u
      }

type DEllipse = Ellipse Double


--------------------------------------------------------------------------------
-- Affine trans

mapEllipseCTM :: (ShapeCTM u -> ShapeCTM u) -> Ellipse u -> Ellipse u
mapEllipseCTM f = (\s i -> s { ell_ctm = f i }) <*> ell_ctm

instance PtSize u => Scale (Ellipse u) where
  scale sx sy = mapEllipseCTM (scale sx sy)


instance Rotate (Ellipse u) where
  rotate ang = mapEllipseCTM (rotate ang)
                  

instance (Real u, Floating u, PtSize u) => RotateAbout (Ellipse u) where
  rotateAbout ang pt = mapEllipseCTM (rotateAbout ang pt)


instance PtSize u => Translate (Ellipse u) where
  translate dx dy = mapEllipseCTM (translate dx dy)


--------------------------------------------------------------------------------
-- Anchors

runDisplaceCenter :: (Real u, Floating u, PtSize u) 
                  => (u -> u -> Vec2 u) -> Ellipse u -> Point2 u
runDisplaceCenter fn (Ellipse { ell_ctm = ctm
                       , ell_rx  = rx
                       , ell_ry  = ry }) = 
    displaceCenter (fn rx ry) ctm


-- | x_radius is the unit length.
--
scaleEll :: (Scale t, Fractional u, PtSize u) => u -> u -> t -> t
scaleEll rx ry = scale 1 (psDouble $ ry/rx) 


instance (Real u, Floating u, PtSize u) => CenterAnchor (Ellipse u) u where
  center = runDisplaceCenter $ \_ _ -> V2 0 0


instance (Real u, Floating u, PtSize u) => RadialAnchor (Ellipse u) u where
  radialAnchor theta = runDisplaceCenter $ \rx ry -> 
                         scaleEll rx ry $ avec theta rx


instance (Real u, Floating u, PtSize u) => CardinalAnchor (Ellipse u) u where
  north = radialAnchor (0.5*pi)
  south = radialAnchor (1.5*pi)
  east  = radialAnchor  0
  west  = radialAnchor  pi


instance (Real u, Floating u, PtSize u) => CardinalAnchor2 (Ellipse u) u where
  northeast = radialAnchor (0.25*pi)
  southeast = radialAnchor (1.75*pi)
  southwest = radialAnchor (1.25*pi)
  northwest = radialAnchor (0.75*pi)


--------------------------------------------------------------------------------
-- Construction

-- | 'ellipse'  : @ x_radii * y_radii -> shape @
--
ellipse :: (Real u, Floating u, PtSize u, Ord u) 
        => u -> u -> Shape Ellipse u
ellipse rx ry = makeShape (mkEllipse rx ry) (mkEllipsePath rx ry)


mkEllipse :: Num u => u -> u -> LocThetaCF u (Ellipse u)
mkEllipse rx ry = promoteR2 $ \ctr theta -> 
    pure $ Ellipse { ell_ctm = makeShapeCTM ctr theta
                   , ell_rx  = rx
                   , ell_ry  = ry 
                   }


mkEllipsePath :: (Real u, Floating u, PtSize u, Ord u) 
              => u -> u -> LocThetaCF u (Path u)
mkEllipsePath rx ry = promoteR2 $ \pt theta -> 
    pure $ traceCurvePoints $ map (rotateAbout theta pt) 
                            $ bezierEllipse rx ry pt

