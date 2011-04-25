{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Drawing.Shapes.Trapezium
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Isoceles Trapezium.
-- 
--------------------------------------------------------------------------------

module Wumpus.Drawing.Shapes.Trapezium
  ( 

    Trapezium
  , DTrapezium
  , trapezium


  ) where

import Wumpus.Drawing.Paths.Absolute
import Wumpus.Drawing.Shapes.Base

import Wumpus.Basic.Geometry                    -- package: wumpus-basic
import Wumpus.Basic.Kernel

import Wumpus.Core                              -- package: wumpus-core



import Control.Applicative




--------------------------------------------------------------------------------
-- Trapezium

-- | A trapezium.
--
data Trapezium u = Trapezium 
      { tz_ctm          :: ShapeCTM u
      , tz_base_width   :: !u
      , tz_top_width   :: !u
      , tz_height       :: !u
      }

type instance DUnit (Trapezium u) = u

type DTrapezium = Trapezium Double

instance Functor Trapezium where
  fmap f (Trapezium ctm bw tw h) = Trapezium (fmap f ctm) (f bw) (f tw) (f h)

--------------------------------------------------------------------------------
-- Affine trans

mapCTM :: (ShapeCTM u -> ShapeCTM u) -> Trapezium u -> Trapezium u
mapCTM f = (\s i -> s { tz_ctm = f i }) <*> tz_ctm


instance (Real u, Floating u) => Rotate (Trapezium u) where
  rotate ang            = mapCTM (rotate ang)
              
instance (Real u, Floating u) => RotateAbout (Trapezium u) where
  rotateAbout ang pt    = mapCTM (rotateAbout ang pt)

instance Fractional u => Scale (Trapezium u) where
  scale sx sy           = mapCTM (scale sx sy)

instance InterpretUnit u => Translate (Trapezium u) where
  translate dx dy       = mapCTM (translate dx dy)


--------------------------------------------------------------------------------
-- Anchors



-- | 'runDisplaceCenter' : @ ( half_base_width 
--                           * half_top_width
--                           * half_height -> Vec ) * trapezium -> Point @
--
runDisplaceCenter :: (Real u, Floating u)
                  => (u -> u -> u -> Vec2 u) 
                  -> Trapezium u -> Anchor u
runDisplaceCenter fn (Trapezium { tz_ctm          = ctm
                                , tz_base_width   = bw
                                , tz_top_width    = tw
                                , tz_height       = h   }) =
    projectFromCtr (fn (0.5 * bw) (0.5 * tw) (0.5 * h)) ctm

instance (Real u, Floating u) => 
    CenterAnchor (Trapezium u) where
  center = runDisplaceCenter $ \_ _ _ -> V2 0 0



instance (Real u, Floating u) => 
    BottomCornerAnchor (Trapezium u) where
  bottomLeftCorner  = runDisplaceCenter $ \hbw _ hh -> V2 (-hbw) (-hh)
  bottomRightCorner = runDisplaceCenter $ \hbw _ hh -> V2  hbw   (-hh)





instance (Real u, Floating u) => 
    TopCornerAnchor (Trapezium u) where
  topLeftCorner  = runDisplaceCenter $ \_ htw hh -> V2 (-htw) hh
  topRightCorner = runDisplaceCenter $ \_ htw hh -> V2   htw  hh


instance (Real u, Floating u, Tolerance u) => 
    SideMidpointAnchor (Trapezium u) where
  sideMidpoint n a = step (n `mod` 4) 
    where
      step 1 = north a
      step 2 = west a
      step 3 = south a
      step _ = east a



instance (Real u, Floating u, Tolerance u) => 
    CardinalAnchor (Trapezium u) where
  north = runDisplaceCenter $ \_ _ hh -> V2 0 hh
  south = runDisplaceCenter $ \_ _ hh -> V2 0 (-hh)
  east  = tzRadialAnchor 0
  west  = tzRadialAnchor pi


instance (Real u, Floating u, Tolerance u) => 
    CardinalAnchor2 (Trapezium u) where
  northeast = tzRadialAnchor (0.25*pi)
  southeast = tzRadialAnchor (1.75*pi)
  southwest = tzRadialAnchor (1.25*pi)
  northwest = tzRadialAnchor (0.75*pi)



instance (Real u, Floating u, Tolerance u) => 
    RadialAnchor (Trapezium u) where
  radialAnchor = tzRadialAnchor

-- 
tzRadialAnchor :: (Real u, Floating u, Tolerance u) 
               => Radian -> Trapezium u -> Anchor u
tzRadialAnchor theta (Trapezium { tz_ctm        = ctm
                                , tz_base_width = bw
                                , tz_top_width  = tw
                                , tz_height     = h  }) =
    post $ findIntersect zeroPt theta $ polygonLineSegments ps
  where 
    ps   = runVertices4 zeroPt $ isoscelesTrapeziumVertices bw tw h
    post = \ans -> case ans of 
                    Nothing       -> projectFromCtr (V2 0 0) ctm
                    Just (P2 x y) -> projectFromCtr (V2 x y) ctm
    
    
--------------------------------------------------------------------------------
-- Construction


-- | 'trapezium'  : @ base_width * height * bottom_left_ang * 
--     bottom_right_ang -> Shape @
--
--
trapezium :: (Real u, Floating u, InterpretUnit u, Tolerance u) 
          => u -> u -> u -> Shape Trapezium u
trapezium bw tw h = 
    makeShape (mkTrapezium bw tw h) (mkTrapeziumPath 0 bw tw h)




--------------------------------------------------------------------------------


mkTrapezium :: (Real u, Fractional u, InterpretUnit u) 
            => u -> u -> u -> LocThetaQuery u (Trapezium u)
mkTrapezium bw tw h = promoteR2 $ \ctr theta -> 
    pure $ Trapezium { tz_ctm           = makeShapeCTM ctr theta
                     , tz_base_width    = bw
                     , tz_top_width     = tw
                     , tz_height        = h
                     }


mkTrapeziumPath :: (Real u, Floating u, InterpretUnit u, Tolerance u) 
                => u -> u -> u -> u -> LocThetaQuery u (AbsPath u)
mkTrapeziumPath rnd bw tw h = promoteR2 $ \ctr theta -> 
    let xs = runVertices4 ctr $ isoscelesTrapeziumVertices bw tw h
    in roundCornerShapePath rnd $ map (rotateAbout theta ctr) xs


