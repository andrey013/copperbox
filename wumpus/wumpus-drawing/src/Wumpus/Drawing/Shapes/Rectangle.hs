{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleContexts           #-}
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
--------------------------------------------------------------------------------

module Wumpus.Drawing.Shapes.Rectangle
  ( 

    Rectangle
  , DRectangle
  , rectangle

  ) where

import Wumpus.Drawing.Geometry.Paths
import Wumpus.Drawing.Geometry.Quadrant
import Wumpus.Drawing.Paths
import Wumpus.Drawing.Shapes.Base

import Wumpus.Basic.Kernel                      -- package: wumpus-basic

import Wumpus.Core                              -- package: wumpus-core


import Control.Applicative



-- Data type

data Rectangle u = Rectangle 
      { rect_ctm    :: ShapeCTM u
      , rect_hw     :: !u
      , rect_hh     :: !u 
      }
  deriving (Eq,Ord,Show)

type DRectangle = Rectangle Double


type instance DUnit (Rectangle u) = u


--------------------------------------------------------------------------------
-- Affine trans

mapCTM :: (ShapeCTM u -> ShapeCTM u) -> Rectangle u -> Rectangle u
mapCTM f = (\s i -> s { rect_ctm = f i }) <*> rect_ctm

instance Num u => Scale (Rectangle u) where
  scale sx sy = mapCTM (scale sx sy)


instance Rotate (Rectangle u) where
  rotate ang = mapCTM (rotate ang)
                  

instance (Real u, Floating u) => RotateAbout (Rectangle u) where
  rotateAbout ang pt = mapCTM (rotateAbout ang pt)


instance Num u => Translate (Rectangle u) where
  translate dx dy = mapCTM (translate dx dy)

--------------------------------------------------------------------------------
-- Anchors



runDisplaceCenter :: (Real u, Floating u) 
                  => (u -> u -> Vec2 u) -> Rectangle u -> Point2 u
runDisplaceCenter fn (Rectangle { rect_ctm = ctm
                                , rect_hw  = hw
                                , rect_hh  = hh }) = 
   displaceCenter (fn hw hh) ctm


instance (Real u, Floating u) => CenterAnchor (Rectangle u) where
  center = runDisplaceCenter $ \_ _ -> V2 0 0

instance (Real u, Floating u) => CardinalAnchor (Rectangle u) where
  north = runDisplaceCenter $ \_  hh -> V2 0 hh
  south = runDisplaceCenter $ \_  hh -> V2 0 (-hh)
  east  = runDisplaceCenter $ \hw _  -> V2 hw 0
  west  = runDisplaceCenter $ \hw _  -> V2 (-hw) 0

instance (Real u, Floating u) => CardinalAnchor2 (Rectangle u) where
  northeast = runDisplaceCenter $ \hw hh -> V2 hw hh
  southeast = runDisplaceCenter $ \hw hh -> V2 hw (-hh)
  southwest = runDisplaceCenter $ \hw hh -> V2 (-hw) (-hh)
  northwest = runDisplaceCenter $ \hw hh -> V2 (-hw) hh


instance (Real u, Floating u) => RadialAnchor (Rectangle u) where
  radialAnchor theta = runDisplaceCenter $ \hw hh -> 
                          rectRadialVector hw hh theta



--------------------------------------------------------------------------------
-- Construction


-- | 'rectangle'  : @ width * height -> shape @
--
rectangle :: (Real u, Floating u, FromPtSize u) 
          => u -> u -> Shape u (Rectangle u)
rectangle w h = 
    makeShape (mkRectangle (0.5*w) (0.5*h))
              (mkRectPath  (0.5*w) (0.5*h))


mkRectangle :: Num u => u -> u -> LocThetaCF u (Rectangle u)
mkRectangle hw hh = promoteR2 $ \ctr theta -> 
    pure $ Rectangle { rect_ctm    = makeShapeCTM ctr theta
                     , rect_hw     = hw
                     , rect_hh     = hh
                     }


mkRectPath :: (Real u, Floating u, FromPtSize u) 
           => u -> u -> LocThetaCF u (Path u)
mkRectPath hw hh = promoteR2 $ \ctr theta -> 
    let btm_left = displace (-hw) (-hh) ctr
    in roundCornerShapePath $ map (rotateAbout theta ctr)
                            $ rectangleCoordPath (2*hw) (2*hh) btm_left
    


