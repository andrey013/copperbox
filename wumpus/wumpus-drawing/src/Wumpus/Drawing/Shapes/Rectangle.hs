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

import Wumpus.Drawing.Geometry.Intersection
import Wumpus.Drawing.Geometry.Paths
import Wumpus.Drawing.Paths
import Wumpus.Drawing.Shapes.Base

import Wumpus.Basic.Kernel                      -- package: wumpus-basic

import Wumpus.Core                              -- package: wumpus-core


import Control.Applicative



--------------------------------------------------------------------------------
-- Rectangle

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


runRectangle :: (u -> u -> ShapeCTM u -> a) -> Rectangle u -> a
runRectangle fn (Rectangle { rect_ctm = ctm, rect_hw = hw, rect_hh = hh }) = 
   fn hw hh ctm

instance (Real u, Floating u) => CenterAnchor (Rectangle u) where
  center = ctmCenter . rect_ctm

instance (Real u, Floating u) => CardinalAnchor (Rectangle u) where
  north = runRectangle $ \_  hh -> projectPoint $ P2 0 hh
  south = runRectangle $ \_  hh -> projectPoint $ P2 0 (-hh)
  east  = runRectangle $ \hw _  -> projectPoint $ P2 hw 0
  west  = runRectangle $ \hw _  -> projectPoint $ P2 (-hw) 0

instance (Real u, Floating u) => CardinalAnchor2 (Rectangle u) where
  northeast = runRectangle $ \hw hh -> projectPoint $ P2 hw hh
  southeast = runRectangle $ \hw hh -> projectPoint $ P2 hw (-hh)
  southwest = runRectangle $ \hw hh -> projectPoint $ P2 (-hw) (-hh)
  northwest = runRectangle $ \hw hh -> projectPoint $ P2 (-hw) hh


instance (Real u, Floating u) => RadialAnchor (Rectangle u) where
  radialAnchor theta = runRectangle $ \hw hh -> 
    projectPoint $ rectangleIntersect hw hh theta

-- Note - the answer needs projecting with the CTM...
--
rectangleIntersect :: (Real u, Floating u) 
                   => u -> u -> Radian -> Point2 u
rectangleIntersect hw hh theta = 
    maybe zeroPt id $ findIntersect zeroPt theta $ rectangleLines zeroPt hw hh 


--------------------------------------------------------------------------------
-- Constructors

-- | 'rectangle'  : @ width * height -> shape @
--
rectangle :: (Real u, Floating u, FromPtSize u) 
          => u -> u -> LocShape u (Rectangle u)
rectangle w h = 
    intoLocShape (mkRectangle (0.5*w) (0.5*h))
                 (mkRectPath  (0.5*w) (0.5*h))


mkRectangle :: Num u => u -> u -> LocCF u (Rectangle u)
mkRectangle hw hh = promoteR1 $ \ctr -> 
    pure $ Rectangle { rect_ctm    = makeShapeCTM ctr
                     , rect_hw     = hw
                     , rect_hh     = hh
                     }


mkRectPath :: (Real u, Floating u, FromPtSize u) 
           => u -> u -> LocCF u (Path u)
mkRectPath hw hh = promoteR1 $ \ctr -> 
    let btm_left = displace (-hw) (-hh) ctr
    in roundCornerShapePath $ rectangleCoordPath (2*hw) (2*hh) btm_left
    


