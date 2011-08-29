{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Drawing.Paths.PathPrimitives
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Primitive multi-line paths for drawing shapes - rectangles, 
-- diamonds, etc.
--
-- All the primitives build HPaths.
-- 
--------------------------------------------------------------------------------

module Wumpus.Drawing.Paths.PathPrimitives
  ( 

    PathScheme(..)

  , drawPathScheme
  , drawPathScheme_

  , pathSchemeSegmentInits

  , pathIterateLocus

  , rectanglePathScm
  , diamondPathScm
  , polygonPathScm

  ) where

import Wumpus.Drawing.Paths.Base
import Wumpus.Drawing.Paths.HPath

import Wumpus.Basic.Kernel                      -- package: wumpus-basic

import Wumpus.Core                              -- package: wumpus-core

import Data.AffineSpace                         -- package: vector-space
import Data.VectorSpace 

import Data.List ( unfoldr )
import Data.Monoid

data PathScheme u = PathScheme 
      { pscm_from_center :: Vec2 u
      , pscm_path        :: HPath u 
      }


drawPathScheme :: (Floating u, Ord u, InterpretUnit u, Tolerance u)
               => PathMode -> PathScheme u -> LocImage u (AbsPath u)
drawPathScheme mode (PathScheme v1 hp) = promoteLoc $ \pt -> 
    let absp = runHPath hp (pt .+^ v1)
    in drawPath mode absp 

drawPathScheme_ :: (Floating u, Ord u, InterpretUnit u, Tolerance u)
                => PathMode -> PathScheme u -> LocGraphic u
drawPathScheme_ mode scm = ignoreAns $ drawPathScheme mode scm 


-- | Note - this collects only start-points of the segment.
--
pathSchemeSegmentInits :: (Floating u, Ord u, InterpretUnit u, Tolerance u)
                       => PathScheme u -> LocQuery u [Point2 u]
pathSchemeSegmentInits (PathScheme v1 hp) = qpromoteLoc $ \pt -> 
    let absp = runHPath hp (pt .+^ v1) in return $ step (pathViewL absp)
  where
    step EmptyPathL                   = []
    step (LineSeg _ sp _ :<< se)      = sp : step (pathViewL se)
    step (CurveSeg _ sp _ _ _ :<< se) = sp : step (pathViewL se)


-- | Create a PathAlg from the vector list - each vector in the 
-- input list iterates to the start point rather then the 
-- cumulative tip.
--
-- When the PathAlg is run, the supplied point is the /locus/ of 
-- the path and it does not form part of the path proper.
-- 
-- Like 'pathStartIsLocus', this constructor is typically used to 
-- make /shape paths/. Some shapes are easier to express as 
-- iterated displacements of the center rather than 
-- /turtle drawing/. 
-- 
pathIterateLocus :: Num u => [Vec2 u] -> PathScheme u
pathIterateLocus []      = PathScheme zeroVec mempty
pathIterateLocus (v0:xs) = PathScheme v0 (step v0 xs)
  where
    step v1 []      = line (v0 ^-^ v1)
    step v1 (v2:vs) = line (v2 ^-^ v1) `mappend` step v2 vs



-- | Implicit start point is /center/, the generated moves are 
-- counter-clockwise so the move-list is
--
-- > [ bl_to_br, br_to_tr, tr_to_tl, tl_to_br ]
--
rectanglePathScm :: Fractional u => u -> u -> PathScheme u
rectanglePathScm w h = 
    PathScheme { pscm_from_center = ctr_to_bl 
               , pscm_path        = mconcat spec
               }
  where
    ctr_to_bl = vec (negate $ 0.5*w) (negate $ 0.5*h)
    spec      = [ line_right w, line_up h, line_left w, line_down h ]


-- | 'diamondPathScm' : @ half_width * half_height -> PathAlg @
--
diamondPathScm :: Num u => u -> u -> PathScheme u
diamondPathScm hw hh = pathIterateLocus [ vs,ve,vn,vw ]
  where
    vs = vvec (-hh)
    ve = hvec hw
    vn = vvec hh
    vw = hvec (-hw)



-- | 'polygonPathScm' : @ num_points * radius -> PathAlg @ 
--
polygonPathScm :: Floating u => Int -> u -> PathScheme u
polygonPathScm n radius = pathIterateLocus $ unfoldr phi (0,top)
  where
    top                     = 0.5*pi
    theta                   = (2*pi) / fromIntegral n
    
    phi (i,ang) | i < n     = Just (avec ang radius, (i+1,ang+theta))
                | otherwise = Nothing
