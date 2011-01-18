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

import Wumpus.Drawing.Geometry.Intersection
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


mapDiamondCTM :: (ShapeCTM u -> ShapeCTM u) -> Diamond u -> Diamond u
mapDiamondCTM f = (\s i -> s { dia_ctm = f i }) <*> dia_ctm

instance Num u => Scale (Diamond u) where
  scale sx sy = mapDiamondCTM (scale sx sy)


instance Rotate (Diamond u) where
  rotate ang = mapDiamondCTM (rotate ang)
                  

instance (Real u, Floating u) => RotateAbout (Diamond u) where
  rotateAbout ang pt = mapDiamondCTM (rotateAbout ang pt)


instance Num u => Translate (Diamond u) where
  translate dx dy = mapDiamondCTM (translate dx dy)



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
   radialAnchor = diamondIntersect


-- Utils.Intersection needs improving...


diamondIntersect :: (Real u, Floating u) 
                 => Radian -> Diamond u -> Point2 u
diamondIntersect theta (Diamond { dia_ctm = ctm, dia_hw = hw, dia_hh = hh }) = 
    let ps  = diamondPoints hw hh ctm 
        ctr = ctmCenter ctm
    in maybe ctr id $ findIntersect ctr theta $ polygonLines ps
    


midpoint :: Fractional u => Point2 u -> Point2 u -> Point2 u
midpoint p1 p2 = let v = 0.5 *^ pvec p1 p2 in p1 .+^ v



-- | 'diamond'  : @ half_width * half_height -> shape @
--
-- Note - args might change to tull_width and full_height...
--
diamond :: (Real u, Floating u, FromPtSize u) 
        => u -> u -> LocShape u (Diamond u)
diamond hw hh = intoLocShape (mkDiamond hw hh) (mkDiamondPath hw hh)


mkDiamond :: Num u => u -> u -> LocCF u (Diamond u)
mkDiamond hw hh = promoteR1 $ \ctr -> 
    pure $ Diamond { dia_ctm = makeShapeCTM ctr, dia_hw = hw, dia_hh = hh }


mkDiamondPath :: (Real u, Floating u, FromPtSize u) 
              => u -> u -> LocCF u (Path u)
mkDiamondPath hw hh = promoteR1 $ \ctr -> 
    roundCornerShapePath $ diamondCoordPath hw hh ctr


diamondPoints :: (Real u, Floating u) => u -> u -> ShapeCTM u -> [Point2 u]
diamondPoints hw hh ctm = map (projectPoint `flip` ctm) [ s, e, n, w ]
  where
    s = P2   0  (-hh)
    e = P2   hw    0
    n = P2   0    hh
    w = P2 (-hw)   0 


