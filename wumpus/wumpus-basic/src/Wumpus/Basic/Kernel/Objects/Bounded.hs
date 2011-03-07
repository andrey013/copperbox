{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.Kernel.Objects.Bounded
-- Copyright   :  (c) Stephen Tetley 2010-2011
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  highly unstable
-- Portability :  GHC 
--
-- Bounded versions of Graphic and LocGraphic.
--
-- Bounded meaning they are actually Images that return the 
-- bounding box of the Graphic.
--
--------------------------------------------------------------------------------

module Wumpus.Basic.Kernel.Objects.Bounded
  (


  -- * Bounded graphic / loc graphic
    BoundedGraphic
  , DBoundedGraphic
  , BoundedLocGraphic
  , DBoundedLocGraphic
  , BoundedLocThetaGraphic
  , DBoundedLocThetaGraphic

  , emptyBoundedLocGraphicAU
  , emptyBoundedLocGraphicRU
  , emptyBoundedLocThetaGraphicAU
  , emptyBoundedLocThetaGraphicRU

  , centerOrthoBBoxAU
  , centerOrthoBBoxRU

  , illustrateBoundedAU
  , illustrateBoundedRU
  , illustrateBoundedLocAU
  , illustrateBoundedLocRU
  , illustrateBoundedLocThetaAU
  , illustrateBoundedLocThetaRU

  , bbrectangleAU
  , bbrectangleRU

  ) where

import Wumpus.Basic.Kernel.Base.BaseDefs
import Wumpus.Basic.Kernel.Base.ContextFun
import Wumpus.Basic.Kernel.Base.DrawingContext
import Wumpus.Basic.Kernel.Base.QueryDC
import Wumpus.Basic.Kernel.Base.UpdateDC
import Wumpus.Basic.Kernel.Objects.BaseObjects
import Wumpus.Basic.Kernel.Objects.DrawingPrimitives
import Wumpus.Basic.Kernel.Objects.Graphic

import Wumpus.Core                              -- package: wumpus-core

import Control.Applicative

--------------------------------------------------------------------------------

-- | Graphic with a bounding box.
-- 
type BoundedGraphic u      = Image BoundingBox u

type DBoundedGraphic       = BoundedGraphic Double




-- | LocGraphic with a bounding box.
--
type BoundedLocGraphic u      = LocImage BoundingBox u

type DBoundedLocGraphic       = BoundedLocGraphic Double


-- | LocThetaGraphic with a bounding box.
--
-- Note the size of bounding box for the \"same\" shape will vary 
-- according to the rotation. A bounding box is always 
-- orthonormal (?) to the x- and y-axes.
--
type BoundedLocThetaGraphic u   = LocThetaImage BoundingBox u

type DBoundedLocThetaGraphic    = BoundedLocThetaGraphic Double






-- | 'centerOrthoBBoxAU' : @ theta * bbox -> BBox @
-- 
-- Rotate a bounding box by @theta@ about its center. Take the 
-- new bounding box.
--
-- Remember that bounding boxes are always orthonormal rectangles,
-- so the dimensions as well as the positions may change under 
-- rotation. 
--

centerOrthoBBoxAU :: (Real u, Floating u, PsDouble u) 
                  => Radian -> BoundingBox u -> BoundingBox u
centerOrthoBBoxAU theta bb = 
    traceBoundary $ map (rotateAbout theta ctr) ps
  where 
    ps  = boundaryCornerList bb
    ctr = boundaryCenter bb

-- | Relative unit version of 'centerOrthoBBoxAU'.
-- 
-- Note this function is monadic whereas 'centerOrthoBBoxAU' is 
-- pure.
--     
centerOrthoBBoxRU :: (Real u, Floating u, CtxSize u, DrawingCtxM m) 
                => Radian -> BoundingBox u -> m (BoundingBox u)
centerOrthoBBoxRU theta bb = 
    dsizeF bb >>= \bb1 -> usizeF $ centerOrthoBBoxAU theta bb1




-- | 'emptyBoundedLocGraphicAU' : @ BoundedLocGraphic @
--
-- Build an empty 'BoundedLocGraphic'.
-- 
-- The 'emptyBoundedLocGraphic' is treated as a /null primitive/ 
-- by @Wumpus-Core@ and is not drawn, although it does generate
-- the minimum bounding box with both the bottom-left and 
-- upper-right corners at the implicit start point.
--
emptyBoundedLocGraphicAU :: PsDouble u => BoundedLocGraphic u
emptyBoundedLocGraphicAU = intoLocImage fn emptyLocGraphicAU
  where
    fn = promoteR1 $ \pt -> pure (BBox pt pt)


-- | Relative unit version of 'emptyBoundedLocGraphicAU'.
--
emptyBoundedLocGraphicRU :: CtxSize u => BoundedLocGraphic u
emptyBoundedLocGraphicRU = intoLocImage fn emptyLocGraphicRU
  where
    fn = promoteR1 $ \pt -> pure (BBox pt pt)


-- | 'emptyBoundedLocThetaGraphicAU' : @ BoundedLocThetaGraphic @
--
-- Build an empty 'BoundedLocThetaGraphic'.
-- 
-- The 'emptyBoundedLocThetaGraphic' is treated as a /null primitive/ 
-- by @Wumpus-Core@ and is not drawn, although it does generate
-- the minimum bounding box with both the bottom-left and 
-- upper-right corners at the implicit start point (the implicit 
-- inclination can be ignored).
--
emptyBoundedLocThetaGraphicAU :: PsDouble u => BoundedLocThetaGraphic u
emptyBoundedLocThetaGraphicAU = lift1R2 emptyBoundedLocGraphicAU

-- | Relative unit version of 'emptyBoundedLocThetaGraphicAU'.
--
emptyBoundedLocThetaGraphicRU :: CtxSize u => BoundedLocThetaGraphic u
emptyBoundedLocThetaGraphicRU = lift1R2 emptyBoundedLocGraphicRU

--------------------------------------------------------------------------------
-- 

-- | Draw a BoundedGraphic, illustrating the bounding box.
--
illustrateBoundedAU :: PsDouble u
                           => BoundedGraphic u -> BoundedGraphic u
illustrateBoundedAU mf = annotate mf bbrectangleAU

-- | Relative unit version of 'emptyBoundedLocThetaAU'.
--
illustrateBoundedRU :: CtxSize u
                           => BoundedGraphic u -> BoundedGraphic u
illustrateBoundedRU mf = annotate mf bbrectangleRU


-- | Draw a BoundedLocGraphic, illustrating the bounding box.
--
illustrateBoundedLocAU :: PsDouble u
                       => BoundedLocGraphic u -> BoundedLocGraphic u
illustrateBoundedLocAU mf = 
    promoteR1 $ \pt -> illustrateBoundedAU $ apply1R1 mf pt


-- | Relative unit version of 'illustrateBoundedLocAU'.
--
illustrateBoundedLocRU :: CtxSize u
                       => BoundedLocGraphic u -> BoundedLocGraphic u
illustrateBoundedLocRU mf = 
    promoteR1 $ \pt -> illustrateBoundedRU $ apply1R1 mf pt


-- | Draw a BoundedLocThetaGraphic, illustrating the bounding box.
--
illustrateBoundedLocThetaAU :: PsDouble u
                            => BoundedLocThetaGraphic u 
                            -> BoundedLocThetaGraphic u
illustrateBoundedLocThetaAU mf = 
    promoteR2 $ \pt theta -> illustrateBoundedAU $ apply2R2 mf pt theta

-- | Relative unit version of 'illustrateBoundedLocThetaAU'.
--
illustrateBoundedLocThetaRU :: (Fractional u, CtxSize u)
                            => BoundedLocThetaGraphic u 
                            -> BoundedLocThetaGraphic u
illustrateBoundedLocThetaRU mf = 
    promoteR2 $ \pt theta -> illustrateBoundedRU $ apply2R2 mf pt theta


-- 

bbrectangleAU :: PsDouble u => BoundingBox u -> Graphic u
bbrectangleAU (BBox p1@(P2 llx lly) p2@(P2 urx ury))
    | llx == urx && lly == ury = emptyLocGraphicAU `at` p1
    | otherwise                = 
        localize drawing_props $ rect1 `oplus` cross
  where
    drawing_props = cap_round . dotted_line
    rect1         = strokedRectangleAU (urx-llx) (ury-lly) `at` p1
    cross         = straightLineAU p1 p2 
                      `oplus` straightLineAU (P2 llx ury) (P2 urx lly)


bbrectangleRU :: CtxSize u => BoundingBox u -> Graphic u
bbrectangleRU (BBox p1@(P2 llx lly) p2@(P2 urx ury))
    | llx == urx && lly == ury = emptyLocGraphicRU `at` p1
    | otherwise                = 
        localize drawing_props $ rect1 `oplus` cross
  where
    drawing_props = cap_round . dotted_line
    rect1         = strokedRectangleRU (urx-llx) (ury-lly) `at` p1
    cross         = straightLineRU p1 p2 
                      `oplus` straightLineRU (P2 llx ury) (P2 urx lly)

