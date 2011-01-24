{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleContexts           #-}
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

import Data.AffineSpace                         -- package: vector-space 

import Control.Applicative





--------------------------------------------------------------------------------
-- Ellipse


data Ellipse u = Ellipse
      { ell_ctm     :: ShapeCTM u 
      , ell_rx      :: !u
      , ell_ry      :: !u
      }

type DEllipse = Ellipse Double

type instance DUnit (Ellipse u) = u

--------------------------------------------------------------------------------
-- Affine trans

mapEllipseCTM :: (ShapeCTM u -> ShapeCTM u) -> Ellipse u -> Ellipse u
mapEllipseCTM f = (\s i -> s { ell_ctm = f i }) <*> ell_ctm

instance Num u => Scale (Ellipse u) where
  scale sx sy = mapEllipseCTM (scale sx sy)


instance Rotate (Ellipse u) where
  rotate ang = mapEllipseCTM (rotate ang)
                  

instance (Real u, Floating u) => RotateAbout (Ellipse u) where
  rotateAbout ang pt = mapEllipseCTM (rotateAbout ang pt)


instance Num u => Translate (Ellipse u) where
  translate dx dy = mapEllipseCTM (translate dx dy)


--------------------------------------------------------------------------------
-- Anchors

runEllipse :: (u -> u -> ShapeCTM u  -> a) -> Ellipse u -> a
runEllipse fn (Ellipse { ell_ctm = ctm, ell_rx = rx, ell_ry = ry }) = 
    fn rx ry ctm


-- | x_radius is the unit length.
--
scaleEll :: (Scale t, Fractional u, u ~ DUnit t) => u -> u -> t -> t
scaleEll rx ry = scale 1 (ry/rx) 


instance (Real u, Floating u) => CenterAnchor (Ellipse u) where
  center = runEllipse $ \_ _ -> ctmCenter


instance (Real u, Floating u) => RadialAnchor (Ellipse u) where
  radialAnchor theta = runEllipse $ \rx ry -> 
    projectPoint $ scaleEll rx ry $ zeroPt .+^ avec theta rx


instance (Real u, Floating u) => CardinalAnchor (Ellipse u) where
  north = radialAnchor (0.5*pi)
  south = radialAnchor (1.5*pi)
  east  = radialAnchor  0
  west  = radialAnchor  pi


instance (Real u, Floating u) => CardinalAnchor2 (Ellipse u) where
  northeast = radialAnchor (0.25*pi)
  southeast = radialAnchor (1.75*pi)
  southwest = radialAnchor (1.25*pi)
  northwest = radialAnchor (0.75*pi)


--------------------------------------------------------------------------------
-- Construction

-- | 'ellipse'  : @ x_radii * y_radii -> shape @
--
ellipse :: (Real u, Floating u, Ord u) 
        => u -> u -> Shape u (Ellipse u)
ellipse rx ry = makeShape (mkEllipse rx ry) (mkEllipsePath rx ry)


mkEllipse :: Num u => u -> u -> LocThetaCF u (Ellipse u)
mkEllipse rx ry = promoteR2 $ \ctr theta -> 
    pure $ Ellipse { ell_ctm = makeShapeCTM ctr theta
                   , ell_rx  = rx
                   , ell_ry  = ry 
                   }


mkEllipsePath :: (Real u, Floating u, Ord u) 
              => u -> u -> LocThetaCF u (Path u)
mkEllipsePath rx ry = promoteR2 $ \pt theta -> 
    pure $ traceCurvePoints $ map (rotateAbout theta pt) 
                            $ bezierEllipse rx ry pt

