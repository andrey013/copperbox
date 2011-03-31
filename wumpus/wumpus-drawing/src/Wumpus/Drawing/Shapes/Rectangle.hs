{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Drawing.Shapes.Rectangle
-- Copyright   :  (c) Stephen Tetley 2010-2011
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Rectangle shape.
--
-- Note - CardinalAnchor2 (northeast etc.) point to their radial 
-- positions (this is a change since earlier versions).
-- 
--------------------------------------------------------------------------------

module Wumpus.Drawing.Shapes.Rectangle
  ( 

    Rectangle
  , DRectangle
  , rectangle

  ) where

import Wumpus.Drawing.Paths
import Wumpus.Drawing.Shapes.Base

import Wumpus.Basic.Geometry.Paths              -- package: wumpus-basic
import Wumpus.Basic.Geometry.Quadrant
import Wumpus.Basic.Kernel

import Wumpus.Core                              -- package: wumpus-core


import Control.Applicative



-- Data type

data Rectangle u = Rectangle 
      { rect_ctm    :: ShapeCTM u
      , rect_hw     :: !u
      , rect_hh     :: !u 
      }
  deriving (Eq,Ord,Show)

type instance DUnit (Rectangle u) = u

type DRectangle = Rectangle Double


instance Functor Rectangle where
  fmap f (Rectangle ctm hw hh) = Rectangle (fmap f ctm) (f hw) (f hh)

--------------------------------------------------------------------------------
-- Affine trans

mapCTM :: (ShapeCTM u -> ShapeCTM u) -> Rectangle u -> Rectangle u
mapCTM f = (\s i -> s { rect_ctm = f i }) <*> rect_ctm



instance (Real u, Floating u) => Rotate (Rectangle u) where
  rotate ang            = mapCTM (rotate ang)
              
instance (Real u, Floating u) => RotateAbout (Rectangle u) where
  rotateAbout ang pt    = mapCTM (rotateAbout ang pt)

instance Fractional u => Scale (Rectangle u) where
  scale sx sy           = mapCTM (scale sx sy)

instance InterpretUnit u => Translate (Rectangle u) where
  translate dx dy       = mapCTM (translate dx dy)

--------------------------------------------------------------------------------
-- Anchors



-- | 'runDisplaceCenter' : @ ( half_width
--                           * half_height -> Vec ) * rectangle -> Point @
--
runDisplaceCenter :: (Real u, Floating u) 
                  => (u -> u -> Vec2 u) -> Rectangle u -> Anchor u
runDisplaceCenter fn (Rectangle { rect_ctm = ctm
                                , rect_hw  = hw
                                , rect_hh  = hh }) = 
   projectFromCtr (fn hw hh) ctm


instance (Real u, Floating u) => 
    CenterAnchor Rectangle u where
  center = runDisplaceCenter $ \_ _ -> V2 0 0

instance (Real u, Floating u) => 
    TopCornerAnchor Rectangle u where
  topLeftCorner  = runDisplaceCenter $ \hw hh -> V2 (-hw) hh
  topRightCorner = runDisplaceCenter $ \hw hh -> V2   hw  hh

instance (Real u, Floating u) => 
    BottomCornerAnchor Rectangle u where
  bottomLeftCorner  = runDisplaceCenter $ \hw hh -> V2 (-hw) (-hh)
  bottomRightCorner = runDisplaceCenter $ \hw hh -> V2   hw  (-hh)

instance (Real u, Floating u) => 
    SideMidpointAnchor Rectangle u where
  sideMidpoint n a = step (n `mod` 4) 
    where
      step 1 = north a
      step 2 = west a
      step 3 = south a
      step _ = east a


instance (Real u, Floating u) => 
    CardinalAnchor Rectangle u where
  north = runDisplaceCenter $ \_  hh -> V2 0 hh
  south = runDisplaceCenter $ \_  hh -> V2 0 (-hh)
  east  = runDisplaceCenter $ \hw _  -> V2 hw 0
  west  = runDisplaceCenter $ \hw _  -> V2 (-hw) 0

instance (Real u, Floating u) => 
    CardinalAnchor2 Rectangle u where
  northeast = radialAnchor (0.25*pi)
  southeast = radialAnchor (1.75*pi)
  southwest = radialAnchor (1.25*pi)
  northwest = radialAnchor (0.75*pi)


instance (Real u, Floating u) => 
    RadialAnchor Rectangle u where
  radialAnchor theta = runDisplaceCenter $ \hw hh -> 
                          rectRadialVector hw hh theta



--------------------------------------------------------------------------------
-- Construction


-- | 'rectangle'  : @ width * height -> shape @
--
rectangle :: (Real u, Floating u, InterpretUnit u, LengthTolerance u) 
          => u -> u -> Shape Rectangle u
rectangle w h = 
    makeShape (mkRectangle (0.5*w) (0.5*h)) (mkRectPath  (0.5*w) (0.5*h))


mkRectangle :: InterpretUnit u => u -> u -> LocThetaQuery u (Rectangle u)
mkRectangle hw hh = promoteR2 $ \ctr theta -> 
    pure $ Rectangle { rect_ctm    = makeShapeCTM ctr theta
                     , rect_hw     = hw
                     , rect_hh     = hh
                     }


mkRectPath :: (Real u, Floating u, InterpretUnit u, LengthTolerance u) 
           => u -> u -> LocThetaQuery u (Path u)
mkRectPath hw hh = promoteR2 $ \ctr theta -> 
    let btm_left = displace (-hw) (-hh) ctr
        xs       = rectangleCoordPath (2*hw) (2*hh) btm_left
    in roundCornerShapePath $ map (rotateAbout theta ctr) xs
    


