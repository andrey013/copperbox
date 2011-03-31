{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
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

type instance DUnit (Ellipse u) = u

type DEllipse = Ellipse Double


instance Functor Ellipse where
  fmap f (Ellipse ctm rx ry) = Ellipse (fmap f ctm) (f rx) (f ry)


--------------------------------------------------------------------------------
-- Affine trans

mapCTM :: (ShapeCTM u -> ShapeCTM u) -> Ellipse u -> Ellipse u
mapCTM f = (\s i -> s { ell_ctm = f i }) <*> ell_ctm


instance (Real u, Floating u) => Rotate (Ellipse u) where
  rotate ang            = mapCTM (rotate ang)
              
instance (Real u, Floating u) => RotateAbout (Ellipse u) where
  rotateAbout ang pt    = mapCTM (rotateAbout ang pt)

instance Fractional u => Scale (Ellipse u) where
  scale sx sy           = mapCTM (scale sx sy)

instance InterpretUnit u => Translate (Ellipse u) where
  translate dx dy       = mapCTM (translate dx dy)


--------------------------------------------------------------------------------
-- Anchors

-- Note - this is monadic for Ellipse...

runDisplaceCenter :: (Real u, Floating u)
                  => (u -> u -> Vec2 u) -> Ellipse u -> Anchor u
runDisplaceCenter fn (Ellipse { ell_ctm = ctm
                              , ell_rx  = rx
                              , ell_ry  = ry }) = 
    projectFromCtr (fn rx ry) ctm


-- NOTE - the Affine instances provided by Wumpus-Core for Point 
-- and Vector may be contradictory for Wumpus-Basic.


-- | x_radius is the unit length.
--
scaleEll :: (Real u, Fractional u)
         => u -> u -> Vec2 u -> Vec2 u
scaleEll rx ry v = let rat = realToFrac (ry/rx) in scale 1 rat v


instance (Real u, Floating u) => CenterAnchor Ellipse u where
  center = runDisplaceCenter $ \_ _ -> V2 0 0


instance (Real u, Floating u) => RadialAnchor Ellipse u where
  radialAnchor theta = runDisplaceCenter $ \rx ry -> 
                         scaleEll rx ry $ avec theta rx


instance (Real u, Floating u) => CardinalAnchor Ellipse u where
  north = radialAnchor (0.5*pi)
  south = radialAnchor (1.5*pi)
  east  = radialAnchor  0
  west  = radialAnchor  pi


instance (Real u, Floating u) => CardinalAnchor2 Ellipse u where
  northeast = radialAnchor (0.25*pi)
  southeast = radialAnchor (1.75*pi)
  southwest = radialAnchor (1.25*pi)
  northwest = radialAnchor (0.75*pi)


--------------------------------------------------------------------------------
-- Construction

-- | 'ellipse'  : @ x_radii * y_radii -> shape @
--
ellipse :: (Real u, Floating u, Ord u, InterpretUnit u, LengthTolerance u) 
        => u -> u -> Shape Ellipse u
ellipse rx ry = makeShape (mkEllipse rx ry) (mkEllipsePath rx ry)


mkEllipse :: InterpretUnit u => u -> u -> LocThetaQuery u (Ellipse u)
mkEllipse rx ry = promoteR2 $ \ctr theta -> 
    pure $ Ellipse { ell_ctm = makeShapeCTM ctr theta
                   , ell_rx  = rx
                   , ell_ry  = ry 
                   }


mkEllipsePath :: (Real u, Floating u, InterpretUnit u, LengthTolerance u) 
              => u -> u -> LocThetaQuery u (Path u)
mkEllipsePath rx ry = promoteR2 $ \pt theta -> 
    let xs = map (rotateAbout theta pt) $ bezierEllipse rx ry pt
    in return $ curvePath xs


