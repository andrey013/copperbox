{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
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

import Wumpus.Drawing.Paths
import Wumpus.Drawing.Shapes.Base

import Wumpus.Basic.Geometry.Base               -- package: wumpus-basic
import Wumpus.Basic.Geometry.Quadrant
import Wumpus.Basic.Geometry.Paths
import Wumpus.Basic.Kernel      

import Wumpus.Core                              -- package: wumpus-core


import Control.Applicative




--------------------------------------------------------------------------------
-- Diamond


data Diamond u = Diamond 
      { dia_ctm   :: ShapeCTM u
      , dia_hw    :: !u
      , dia_hh    :: !u
      }

type DDiamond = Diamond Double


instance Functor Diamond where
  fmap f (Diamond ctm hw hh) = Diamond (fmap f ctm) (f hw) (f hh)

--------------------------------------------------------------------------------
-- Affine trans

mapCTM :: (ShapeCTM u -> ShapeCTM u) -> Diamond u -> Diamond u
mapCTM f = (\s i -> s { dia_ctm = f i }) <*> dia_ctm



instance CtxRotate Diamond u where
  ctxRotate sz ang = mapCTM (ctxRotate sz ang)
              
instance InterpretUnit u => CtxRotateAbout Diamond u where
  ctxRotateAbout sz ang pt = mapCTM (ctxRotateAbout sz ang pt)

instance InterpretUnit u => CtxScale Diamond u where
  ctxScale sz sx sy = mapCTM (ctxScale sz sx sy)

instance InterpretUnit u => CtxTranslate Diamond u where
  ctxTranslate sz dx dy = mapCTM (ctxTranslate sz dx dy)


--------------------------------------------------------------------------------
-- Anchors

-- | 'runDisplaceCenter' : @ ( half_width 
--                           * half_height -> Vec ) * diamond -> Point @
--
runDisplaceCenter :: InterpretUnit u
                  => (u -> u -> Vec2 u) -> Diamond u -> Anchor u
runDisplaceCenter fn (Diamond { dia_ctm = ctm
                              , dia_hw = hw
                              , dia_hh = hh }) = 
   projectFromCtr (fn hw hh) ctm


instance InterpretUnit u => CenterAnchor Diamond u where
  center = runDisplaceCenter $ \_ _ -> V2 0 0

instance InterpretUnit u => ApexAnchor Diamond u where
  apex = runDisplaceCenter $ \_  hh -> V2 0 hh

instance (Fractional u, InterpretUnit u) => 
    SideMidpointAnchor Diamond u where
  sideMidpoint n a = step (n `mod` 4) 
    where
      step 1 = midpoint <$> north a <*> west a
      step 2 = midpoint <$> west a  <*> south a
      step 3 = midpoint <$> south a <*> east a
      step _ = midpoint <$> east a  <*> north a


instance InterpretUnit u => CardinalAnchor Diamond u where
  north = apex
  south = runDisplaceCenter $ \_  hh -> V2 0 (-hh)
  east  = runDisplaceCenter $ \hw _  -> V2 hw 0
  west  = runDisplaceCenter $ \hw _  -> V2 (-hw) 0

instance (Fractional u, InterpretUnit u) => CardinalAnchor2 Diamond u where
  northeast x = midpoint <$> north x <*> east x
  southeast x = midpoint <$> south x <*> east x
  southwest x = midpoint <$> south x <*> west x
  northwest x = midpoint <$> north x <*> west x



instance (Real u, Floating u, InterpretUnit u) => 
      RadialAnchor Diamond u where
  radialAnchor ang = runDisplaceCenter $ \hw hh -> 
                     diamondRadialVector hw hh ang




--------------------------------------------------------------------------------
-- Construction

-- | 'diamond'  : @ half_width * half_height -> shape @
--
-- Note - args might change to tull_width and full_height...
--
diamond :: (Real u, Floating u, InterpretUnit u, LengthTolerance u)
        => u -> u -> Shape Diamond u
diamond hw hh = makeShape (mkDiamond hw hh) (mkDiamondPath hw hh)


mkDiamond :: InterpretUnit u => u -> u -> LocThetaQuery u (Diamond u)
mkDiamond hw hh = promoteR2 $ \ctr theta -> 
    pure $ Diamond { dia_ctm = makeShapeCTM ctr theta
                   , dia_hw  = hw
                   , dia_hh  = hh 
                   }


mkDiamondPath :: (Real u, Floating u, InterpretUnit u, LengthTolerance u)
              => u -> u -> LocThetaQuery u (Path u)
mkDiamondPath hw hh = promoteR2 $ \ctr theta ->
    pointSize >>= \sz -> 
    let ps = map (ctxRotateAbout sz theta ctr) $ diamondCoordPath hw hh ctr
    in roundCornerShapePath ps




