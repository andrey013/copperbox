{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Drawing.Shapes.Parallelogram
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Parallelogram.
--
-- 
--------------------------------------------------------------------------------

module Wumpus.Drawing.Shapes.Parallelogram
  ( 

    Parallelogram
  , DParallelogram
  , parallelogram
  , zparallelogram


  ) where

import Wumpus.Drawing.Paths
import Wumpus.Drawing.Shapes.Base

import Wumpus.Basic.Geometry.Base               -- package: wumpus-basic
import Wumpus.Basic.Geometry.Intersection
import Wumpus.Basic.Geometry.Paths
import Wumpus.Basic.Kernel

import Wumpus.Core                              -- package: wumpus-core


import Control.Applicative




--------------------------------------------------------------------------------
-- Parallelogram

-- | A Paralleogram.
--
data Parallelogram u = Parallelogram 
      { pll_ctm         :: ShapeCTM u
      , pll_base_width  :: !u
      , pll_height      :: !u
      , pll_base_l_ang  :: Radian
      , pll_syn_props   :: SyntheticProps u
      }

type instance DUnit (Parallelogram u) = u

-- | Note (center) is a line dropped from the center of the
-- paralleogram.
-- 
-- > base_minor is the (center) to left corner.
-- > base_major is the (center) to right corner.
--
data SyntheticProps u = SyntheticProps
      { pll_base_minor  :: u
      , pll_base_major  :: u
      }

type instance DUnit (SyntheticProps u) = u


type DParallelogram = Parallelogram Double


instance Functor Parallelogram where
  fmap f (Parallelogram ctm bw h lang props) = 
      Parallelogram (fmap f ctm) (f bw) (f h) lang (fmap f props)

instance Functor SyntheticProps where
  fmap f (SyntheticProps bmin bmaj) = SyntheticProps (f bmin) (f bmaj)



--------------------------------------------------------------------------------
-- Affine trans

mapCTM :: (ShapeCTM u -> ShapeCTM u) -> Parallelogram u -> Parallelogram u
mapCTM f = (\s i -> s { pll_ctm = f i }) <*> pll_ctm


instance (Real u, Floating u) => Rotate (Parallelogram u) where
  rotate ang            = mapCTM (rotate ang)
              
instance (Real u, Floating u) => RotateAbout (Parallelogram u) where
  rotateAbout ang pt    = mapCTM (rotateAbout ang pt)

instance Fractional u => Scale (Parallelogram u) where
  scale sx sy           = mapCTM (scale sx sy)

instance InterpretUnit u => Translate (Parallelogram u) where
  translate dx dy       = mapCTM (translate dx dy)

--------------------------------------------------------------------------------
-- Anchors

-- | 'runDisplaceCenter' : @ ( half_base_width
--                           * half_height 
--                           * base_minor
--                           * base_major -> Vec ) * parallelogram -> Point @
--
runDisplaceCenter :: (Real u, Floating u)
                  => (u -> u -> u -> u -> Vec2 u) -> Parallelogram u -> Anchor u
runDisplaceCenter fn (Parallelogram { pll_ctm        = ctm
                                    , pll_base_width = bw
                                    , pll_height     = h  
                                    , pll_syn_props  = syn }) = 
    projectFromCtr (fn (0.5 * bw) (0.5 * h) 
                       (pll_base_minor syn) (pll_base_major syn)) ctm



instance (Real u, Floating u) => 
    CenterAnchor Parallelogram u where
  center = runDisplaceCenter $ \_ _ _ _ -> V2 0 0

-- top anchors swap the base minor and major...
--
instance (Real u, Floating u) => 
    TopCornerAnchor Parallelogram u where
  topLeftCorner  = runDisplaceCenter $ \_ hh _ bmaj -> V2 (-bmaj) hh
  topRightCorner = runDisplaceCenter $ \_ hh bmin _ -> V2 bmin    hh

instance (Real u, Floating u) => 
    BottomCornerAnchor Parallelogram u where
  bottomLeftCorner  = runDisplaceCenter $ \_ hh bmin _ -> V2 (-bmin) (-hh)
  bottomRightCorner = runDisplaceCenter $ \_ hh _ bmaj -> V2 bmaj    (-hh)

instance (Real u, Floating u) => 
    SideMidpointAnchor Parallelogram u where
  sideMidpoint n a = step (n `mod` 4) 
    where
      step 1 = midpoint (topRightCorner a)    (topLeftCorner a)
      step 2 = midpoint (topLeftCorner a)     (bottomLeftCorner a)
      step 3 = midpoint (bottomLeftCorner a)  (bottomRightCorner a)
      step _ = midpoint (bottomRightCorner a) (topRightCorner a)



instance (Real u, Floating u) => 
    CardinalAnchor Parallelogram u where
  north = runDisplaceCenter $ \_  hh _ _ -> V2 0 hh
  south = runDisplaceCenter $ \_  hh _ _ -> V2 0 (-hh)
  east  = runDisplaceCenter $ \hw _  _ _ -> V2 hw 0
  west  = runDisplaceCenter $ \hw _  _ _ -> V2 (-hw) 0


instance (Real u, Floating u, InterpretUnit u, LengthTolerance u) => 
    CardinalAnchor2 Parallelogram u where
  northeast = pllRadialAnchor (0.25*pi)
  southeast = pllRadialAnchor (1.75*pi)
  southwest = pllRadialAnchor (1.25*pi)
  northwest = pllRadialAnchor (0.75*pi)



instance (Real u, Floating u, InterpretUnit u, LengthTolerance u) => 
     RadialAnchor Parallelogram u where
   radialAnchor = pllRadialAnchor


-- TODO - update this to a quadrant function...
--
pllRadialAnchor :: (Real u, Floating u, InterpretUnit u, LengthTolerance u) 
                => Radian -> Parallelogram u -> Anchor u
pllRadialAnchor theta (Parallelogram { pll_ctm       = ctm
                                     , pll_height    = h
                                     , pll_syn_props = syn }) =
    post $ findIntersect zeroPt theta $ polygonLineSegments ps
  where 
    ps   = pllPoints (pll_base_minor syn) (pll_base_major syn) h
    post = \ans -> case ans of 
                    Nothing       -> projectFromCtr (V2 0 0) ctm
                    Just (P2 x y) -> projectFromCtr (V2 x y) ctm
    

--------------------------------------------------------------------------------
-- Construction


-- | 'parallelogram'  : @ width * height * bottom_left_ang -> Parallelogram @
--
--
parallelogram :: (Real u, Floating u, InterpretUnit u, LengthTolerance u) 
              => u -> u -> Radian -> Shape Parallelogram u
parallelogram bw h lang =
    let props = synthesizeProps bw h lang 
    in makeShape (mkParallelogram bw h lang props) 
                 (mkParallelogramPath (pll_base_minor props) 
                                      (pll_base_major props) h)


-- | 'zparallelogram'  : @ base_width * height -> Parallelogram @
--
--
zparallelogram :: (Real u, Floating u, InterpretUnit u, LengthTolerance u) 
              => u -> u -> Shape Parallelogram u
zparallelogram bw h = parallelogram bw h ang
  where
    ang = d2r (60::Double)


--------------------------------------------------------------------------------


mkParallelogram :: (Real u, Fractional u, InterpretUnit u, LengthTolerance u) 
                => u -> u -> Radian -> SyntheticProps u 
                -> LocThetaQuery u (Parallelogram u)
mkParallelogram bw h lang props = promoteR2 $ \ctr theta -> 
    pure $ Parallelogram { pll_ctm          = makeShapeCTM ctr theta
                         , pll_base_width   = bw
                         , pll_height       = h
                         , pll_base_l_ang   = lang
                         , pll_syn_props    = props
                         }

-- Note - expects ang value 0 < ang < 180, though does not check...
-- 
synthesizeProps :: Fractional u => u -> u -> Radian -> SyntheticProps u
synthesizeProps bw h lang 
    | lang == 0.5*pi = let hw = 0.5 * bw in SyntheticProps hw hw
    | lang >  0.5*pi = less_ninety
    | otherwise      = grtr_ninety
  where
    less_ninety = let extw            = h / (fromRadian $ tan lang)
                      half_rect_width = 0.5 * (bw + extw)
                  in SyntheticProps half_rect_width (half_rect_width - extw)

    grtr_ninety = let extw            = h / (fromRadian $ tan (pi-lang))
                      half_rect_width = 0.5 * (bw + extw)
                  in SyntheticProps (half_rect_width - extw) half_rect_width

                       



mkParallelogramPath :: (Real u, Floating u, InterpretUnit u, LengthTolerance u) 
                    => u -> u -> u -> LocThetaQuery u (AbsPath u)
mkParallelogramPath bw_minor bw_major h = promoteR2 $ \ctr theta -> 
    let xs = pllPath bw_minor bw_major h ctr
    in roundCornerShapePath $ map (rotateAbout theta ctr) xs 
                         


pllPath :: (Real u, Floating u) 
       => u -> u -> u -> LocCoordPath u
pllPath bw_minor bw_major h (P2 x y) = [ bl, br, tr, tl ]
  where
    hh = 0.5 * h
    bl = P2 (x - bw_minor) (y - hh)
    br = P2 (x + bw_major) (y - hh)
    tl = P2 (x - bw_major) (y + hh)     -- topleft subtracts major
    tr = P2 (x + bw_minor) (y + hh)     -- topright adds minor


pllPoints :: (Real u, Floating u) 
          => u -> u -> u -> [Point2 u]
pllPoints bw_minor bw_major h = [ bl, br, tr, tl ]
  where
    hh = 0.5 * h     
    bl = P2 (-bw_minor) (-hh) 
    br = P2   bw_major  (-hh)
    tl = P2 (-bw_major) hh
    tr = P2   bw_minor  hh




