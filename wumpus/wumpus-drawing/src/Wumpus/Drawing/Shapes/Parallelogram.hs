{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleContexts           #-}
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

import Wumpus.Drawing.Geometry.Intersection
import Wumpus.Drawing.Geometry.Paths
import Wumpus.Drawing.Paths
import Wumpus.Drawing.Shapes.Base

import Wumpus.Basic.Kernel                      -- package: wumpus-basic

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


-- | rect_width is the width of the (greater) enclosing rectangle.
data SyntheticProps u = SyntheticProps
      { pll_base_minor  :: u
      , pll_base_major  :: u
      }


type DParallelogram = Parallelogram Double

type instance DUnit (Parallelogram u) = u




--------------------------------------------------------------------------------
-- Affine trans

mapCTM :: (ShapeCTM u -> ShapeCTM u) -> Parallelogram u -> Parallelogram u
mapCTM f = (\s i -> s { pll_ctm = f i }) <*> pll_ctm

instance Num u => Scale (Parallelogram u) where
  scale sx sy = mapCTM (scale sx sy)


instance Rotate (Parallelogram u) where
  rotate ang = mapCTM (rotate ang)
                  

instance (Real u, Floating u) => RotateAbout (Parallelogram u) where
  rotateAbout ang pt = mapCTM (rotateAbout ang pt)


instance Num u => Translate (Parallelogram u) where
  translate dx dy = mapCTM (translate dx dy)

--------------------------------------------------------------------------------
-- Anchors


-- 'runParallelogram' : @ half_base_width * half_height *ctm -> Ans @
-- 
runParallelogram :: Fractional u 
                 => (u -> u -> ShapeCTM u  -> a) 
                 -> Parallelogram u -> a
runParallelogram fn (Parallelogram { pll_ctm        = ctm
                                   , pll_base_width = bw
                                   , pll_height     = h   }) =  
    fn (0.5 * bw) (0.5 * h) ctm



instance (Real u, Floating u) => CenterAnchor (Parallelogram u) where
  center = ctmCenter . pll_ctm




instance (Real u, Floating u) => CardinalAnchor (Parallelogram u) where
  north = runParallelogram $ \_  hh -> projectPoint $ P2 0 hh
  south = runParallelogram $ \_  hh -> projectPoint $ P2 0 (-hh)
  east  = runParallelogram $ \hw _  -> projectPoint $ P2 hw 0
  west  = runParallelogram $ \hw _  -> projectPoint $ P2 (-hw) 0


instance (Real u, Floating u) => CardinalAnchor2 (Parallelogram u) where
  northeast = pllRadialAnchor (0.25*pi)
  southeast = pllRadialAnchor (1.75*pi)
  southwest = pllRadialAnchor (1.25*pi)
  northwest = pllRadialAnchor (0.75*pi)



instance (Real u, Floating u) => RadialAnchor (Parallelogram u) where
   radialAnchor = pllRadialAnchor


pllRadialAnchor :: (Real u, Floating u) 
                  => Radian -> Parallelogram u -> Point2 u
pllRadialAnchor theta (Parallelogram { pll_ctm       = ctm
                                     , pll_height    = h
                                     , pll_syn_props = syn }) =
    maybe ctr id $ findIntersect ctr theta $ polygonLines ps
  where 
    ps  = pllPoints (pll_base_minor syn) (pll_base_major syn) h ctm
    ctr = ctmCenter ctm
    
    

--------------------------------------------------------------------------------
-- Construction


-- | 'parallelogram'  : @ width * height * bottom_left_ang -> Parallelogram @
--
--
parallelogram :: (Real u, Floating u, FromPtSize u) 
              => u -> u -> Radian -> Shape u (Parallelogram u)
parallelogram bw h lang =
    let props = synthesizeProps bw h lang 
    in makeShape (mkParallelogram bw h lang props) 
                 (mkParallelogramPath (pll_base_minor props) 
                                      (pll_base_major props) h)


-- | 'zparallelogram'  : @ base_width * height -> Parallelogram @
--
--
zparallelogram :: (Real u, Floating u, FromPtSize u) 
              => u -> u -> Shape u (Parallelogram u)
zparallelogram bw h = parallelogram bw h ang
  where
    ang = d2r (60::Double)


--------------------------------------------------------------------------------


mkParallelogram :: (Real u, Fractional u) 
                => u -> u -> Radian -> SyntheticProps u 
                -> LocThetaCF u (Parallelogram u)
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

                       



mkParallelogramPath :: (Real u, Floating u, FromPtSize u) 
                    => u -> u -> u -> LocThetaCF u (Path u)
mkParallelogramPath bw_minor bw_major h = promoteR2 $ \ctr theta -> 
    roundCornerShapePath $ map (rotateAbout theta ctr) 
                         $ pllPath bw_minor bw_major h ctr


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
               => u -> u -> u -> ShapeCTM u -> [Point2 u]
pllPoints bw_minor bw_major h ctm = map (projectPoint `flip` ctm) [ bl, br, tr, tl ]
  where
    hh = 0.5 * h     
    bl = P2 (-bw_minor) (-hh) 
    br = P2   bw_major  (-hh)
    tl = P2 (-bw_major) hh
    tr = P2   bw_minor  hh




