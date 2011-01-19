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



instance (Real u, Floating u) => CenterAnchor (Parallelogram u) where
  center = ctmCenter . pll_ctm


{-
runDiamond :: (u -> u -> ShapeCTM u  -> a) -> Diamond u -> a
runDiamond fn (Diamond { dia_ctm = ctm, dia_hw = hw, dia_hh = hh }) = 
   fn hw hh ctm

instance (Real u, Floating u) => CardinalAnchor (Parallelogram u) where
  north = tzRadialAnchor (0.5*pi)
  south = \tz -> let hh = 0.5 * tz_height tz
                 in projectPoint (P2 0 (-hh)) (tz_ctm tz)
  east  = tzRadialAnchor 0
  west  = tzRadialAnchor pi




instance (Real u, Floating u) => CardinalAnchor2 (Parallelogram u) where
  northeast = tzRadialAnchor (0.25*pi)
  southeast = tzRadialAnchor (1.75*pi)
  southwest = tzRadialAnchor (1.25*pi)
  northwest = tzRadialAnchor (0.75*pi)



instance (Real u, Floating u) => RadialAnchor (Parallelogram u) where
   radialAnchor = tzRadialAnchor


tzRadialAnchor :: (Real u, Floating u) 
                  => Radian -> Parallelogram u -> Point2 u
tzRadialAnchor theta (Parallelogram { tz_ctm        = ctm
                                , tz_base_width = bw
                                , tz_height     = h
                                , tz_base_l_ang = lang
                                , tz_base_r_ang = rang }) =
    maybe ctr id $ findIntersect ctr theta $ polygonLines ps
  where 
    ps  = tzPoints bw h lang rang ctm 
    ctr = ctmCenter ctm
    
-}    

--------------------------------------------------------------------------------
-- Constructors


-- | 'parallelogram'  : @ width * height * bottom_left_ang -> Parallelogram @
--
--
parallelogram :: (Real u, Floating u, FromPtSize u) 
                  => u -> u -> Radian -> LocShape u (Parallelogram u)
parallelogram bw h lang =
    intoLocShape (mkParallelogram bw h lang) (mkParallelogramPath bw h lang)


-- | 'zparallelogram'  : @ base_width * height -> Parallelogram @
--
--
zparallelogram :: (Real u, Floating u, FromPtSize u) 
              => u -> u -> LocShape u (Parallelogram u)
zparallelogram bw h = parallelogram bw h ang
  where
    ang = d2r (60::Double)


--------------------------------------------------------------------------------


mkParallelogram :: (Real u, Fractional u) 
            => u -> u -> Radian -> LocCF u (Parallelogram u)
mkParallelogram bw h lang = promoteR1 $ \ctr -> 
    pure $ Parallelogram { pll_ctm          = makeShapeCTM ctr
                         , pll_base_width   = bw
                         , pll_height       = h
                         , pll_base_l_ang   = lang
                         }

mkParallelogramPath :: (Real u, Floating u, FromPtSize u) 
                    => u -> u -> Radian -> LocCF u (Path u)
mkParallelogramPath bw h lang = promoteR1 $ \ctr -> 
    roundCornerShapePath $ pllPath bw h lang ctr


pllPath :: (Real u, Floating u) 
       => u -> u -> Radian -> LocCoordPath u
pllPath _ h lang pt = [ bl, br, tr, tl ]
  where
    tr_vec    = urVec h lang
    tl_vec    = ulVec h lang
    bl        = displaceVec (vreverse tr_vec) pt
    br        = displaceVec (vreverse tl_vec) pt
    tr        = displaceVec tr_vec pt
    tl        = displaceVec tl_vec pt


pllPoints :: (Real u, Floating u) 
               => u -> u -> Radian -> ShapeCTM u -> [Point2 u]
pllPoints _ h lang ctm = map (projectPoint `flip` ctm) [ bl, br, tr, tl ]
  where
    tr_vec    = urVec h lang
    tl_vec    = ulVec h lang
    bl        = displaceVec (vreverse tr_vec) zeroPt
    br        = displaceVec (vreverse tl_vec) zeroPt
    tr        = displaceVec tr_vec zeroPt
    tl        = displaceVec tl_vec zeroPt





urVec :: Floating u => u -> Radian -> Vec2 u
urVec h lang = avec half_ang dist
  where
    hh          = 0.5 * h
    half_ang    = 0.5 * lang
    dist        = hh / (fromRadian $ sin half_ang)

ulVec :: Floating u => u -> Radian -> Vec2 u
ulVec h lang = avec (theta + 0.5*pi) dist
  where
    hh          = 0.5 * h
    rang        = pi - lang
    theta       = (0.5*pi) - (0.5*rang)    
    dist        = hh / (fromRadian $ cos theta)

