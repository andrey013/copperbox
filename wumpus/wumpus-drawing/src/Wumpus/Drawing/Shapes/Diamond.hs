{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Drawing.Shapes.Diamond
-- Copyright   :  (c) Stephen Tetley 2010-2011
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Diamond (rhombus).
-- 
--------------------------------------------------------------------------------

module Wumpus.Drawing.Shapes.Diamond
  ( 

    Diamond
  , DDiamond
  , diamond


  ) where

import Wumpus.Drawing.Geometry.Quadrant
import Wumpus.Drawing.Geometry.Paths
import Wumpus.Drawing.Paths
import Wumpus.Drawing.Shapes.Base

import Wumpus.Basic.Kernel                      -- package: wumpus-basic

import Wumpus.Core                              -- package: wumpus-core

import Data.AffineSpace                         -- package: vector-space 
import Data.VectorSpace

import Control.Applicative




--------------------------------------------------------------------------------
-- Diamond


data Diamond u = Diamond 
      { dia_ctm   :: ShapeCTM u
      , dia_hw    :: !u
      , dia_hh    :: !u
      }

type DDiamond = Diamond Double

type instance DUnit (Diamond u) = u


--------------------------------------------------------------------------------
-- Affine trans

mapCTM :: (ShapeCTM u -> ShapeCTM u) -> Diamond u -> Diamond u
mapCTM f = (\s i -> s { dia_ctm = f i }) <*> dia_ctm

instance Num u => Scale (Diamond u) where
  scale sx sy = mapCTM (scale sx sy)


instance Rotate (Diamond u) where
  rotate ang = mapCTM (rotate ang)
                  

instance (Real u, Floating u) => RotateAbout (Diamond u) where
  rotateAbout ang pt = mapCTM (rotateAbout ang pt)


instance Num u => Translate (Diamond u) where
  translate dx dy = mapCTM (translate dx dy)



--------------------------------------------------------------------------------
-- Anchors

runDiamond :: (u -> u -> ShapeCTM u  -> a) -> Diamond u -> a
runDiamond fn (Diamond { dia_ctm = ctm, dia_hw = hw, dia_hh = hh }) = 
   fn hw hh ctm


instance (Real u, Floating u) => CenterAnchor (Diamond u) where
  center = runDiamond (\_ _ -> ctmCenter)

instance (Real u, Floating u) => CardinalAnchor (Diamond u) where
  north = runDiamond $ \_  hh -> projectPoint $ P2 0 hh
  south = runDiamond $ \_  hh -> projectPoint $ P2 0 (-hh)
  east  = runDiamond $ \hw _  -> projectPoint $ P2 hw 0
  west  = runDiamond $ \hw _  -> projectPoint $ P2 (-hw) 0

instance (Real u, Floating u, Fractional u) => CardinalAnchor2 (Diamond u) where
  northeast x = midpoint (north x) (east x)
  southeast x = midpoint (south x) (east x)
  southwest x = midpoint (south x) (west x)
  northwest x = midpoint (north x) (west x)



instance (Real u, Floating u) => RadialAnchor (Diamond u) where
    radialAnchor ang = runDiamond $ \hw hh -> 
      projectPoint $ displaceVec (diamondRadialVector hw hh ang) zeroPt



midpoint :: Fractional u => Point2 u -> Point2 u -> Point2 u
midpoint p1 p2 = let v = 0.5 *^ pvec p1 p2 in p1 .+^ v


--------------------------------------------------------------------------------
-- Construction

-- | 'diamond'  : @ half_width * half_height -> shape @
--
-- Note - args might change to tull_width and full_height...
--
diamond :: (Real u, Floating u, FromPtSize u) 
        => u -> u -> Shape u (Diamond u)
diamond hw hh = makeShape (mkDiamond hw hh) (mkDiamondPath hw hh)


mkDiamond :: Num u => u -> u -> LocThetaCF u (Diamond u)
mkDiamond hw hh = promoteR2 $ \ctr theta -> 
    pure $ Diamond { dia_ctm = makeShapeCTM ctr theta
                   , dia_hw  = hw
                   , dia_hh = hh 
                   }


mkDiamondPath :: (Real u, Floating u, FromPtSize u) 
              => u -> u -> LocThetaCF u (Path u)
mkDiamondPath hw hh = promoteR2 $ \ctr theta -> 
    roundCornerShapePath $ map (rotateAbout theta ctr) 
                         $ diamondCoordPath hw hh ctr
