{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.Kernel.Objects.Bounded
-- Copyright   :  (c) Stephen Tetley 2010
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

  , emptyBoundedLocGraphic
  , emptyBoundedLocThetaGraphic

  , centerOrthoBBox
  , illustrateBoundedGraphic
  , illustrateBoundedLocGraphic
  , illustrateBoundedLocThetaGraphic

  , bbrectangle

  ) where

import Wumpus.Basic.Kernel.Base.BaseDefs
import Wumpus.Basic.Kernel.Base.ContextFun
import Wumpus.Basic.Kernel.Base.DrawingContext
import Wumpus.Basic.Kernel.Base.UpdateDC
import Wumpus.Basic.Kernel.Objects.BaseObjects
import Wumpus.Basic.Kernel.Objects.DrawingPrimitives
import Wumpus.Basic.Kernel.Objects.Graphic

import Wumpus.Core                              -- package: wumpus-core

import Control.Applicative

--------------------------------------------------------------------------------

-- | Graphic with a bounding box.
-- 
type BoundedGraphic u      = Image u (BoundingBox u)

type DBoundedGraphic       = BoundedGraphic Double




-- | LocGraphic with a bounding box.
--
type BoundedLocGraphic u      = LocImage u (BoundingBox u)

type DBoundedLocGraphic       = BoundedLocGraphic Double


-- | LocThetaGraphic with a bounding box.
--
-- Note the size of bounding box for the \"same\" shape will vary 
-- according to the rotation. A bounding box is always 
-- orthonormal (?) to the x- and y-axes.
--
type BoundedLocThetaGraphic u   = LocThetaImage u (BoundingBox u)

type DBoundedLocThetaGraphic    = BoundedLocThetaGraphic Double




-- | 'centerOrthoBBox' : @ theta * bbox -> BBox @
-- 
-- Rotate a bounding box by @theta@ about its center. Take the 
-- new bounding box.
--
-- Remember that bounding boxes are always orthonormal rectangles,
-- so the dimensions as well as the positions may change under 
-- rotation. 
--
centerOrthoBBox :: (Real u, Floating u, PtSize u) 
                => Radian -> BoundingBox u -> BoundingBox u
centerOrthoBBox theta bb = 
    traceBoundary $ map (rotateAbout theta ctr) ps
  where
    ps  = boundaryCornerList bb
    ctr = boundaryCenter bb




-- | 'emptyBoundedLocGraphic' : @ BoundedLocGraphic @
--
-- Build an empty 'BoundedLocGraphic'.
-- 
-- The 'emptyBoundedLocGraphic' is treated as a /null primitive/ 
-- by @Wumpus-Core@ and is not drawn, although it does generate
-- the minimum bounding box with both the bottom-left and 
-- upper-right corners at the implicit start point.
--
emptyBoundedLocGraphic :: Num u => BoundedLocGraphic u
emptyBoundedLocGraphic = intoLocImage fn emptyLocGraphic
  where
    fn = promoteR1 $ \pt -> pure (BBox pt pt)


-- | 'emptyBoundedLocThetaGraphic' : @ BoundedLocThetaGraphic @
--
-- Build an empty 'BoundedLocThetaGraphic'.
-- 
-- The 'emptyBoundedLocThetaGraphic' is treated as a /null primitive/ 
-- by @Wumpus-Core@ and is not drawn, although it does generate
-- the minimum bounding box with both the bottom-left and 
-- upper-right corners at the implicit start point (the implicit 
-- inclination can be ignored).
--
emptyBoundedLocThetaGraphic :: Num u => BoundedLocThetaGraphic u
emptyBoundedLocThetaGraphic = lift1R2 emptyBoundedLocGraphic

--------------------------------------------------------------------------------
-- 

-- This is a common pattern so needs a name...

illustrateBoundedGraphic :: Fractional u => BoundedGraphic u -> BoundedGraphic u
illustrateBoundedGraphic mf = 
    mf >>= \(bb,g1) -> bbrectangle bb >>= \(_,g0) -> return (bb, g0 `oplus` g1)


illustrateBoundedLocGraphic :: Fractional u 
                            => BoundedLocGraphic u -> BoundedLocGraphic u
illustrateBoundedLocGraphic mf = 
    promoteR1 $ \pt -> illustrateBoundedGraphic $ apply1R1 mf pt


illustrateBoundedLocThetaGraphic :: Fractional u 
    => BoundedLocThetaGraphic u -> BoundedLocThetaGraphic u
illustrateBoundedLocThetaGraphic mf = 
    promoteR2 $ \pt theta-> illustrateBoundedGraphic $ apply2R2 mf pt theta


-- 
bbrectangle :: Fractional u => BoundingBox u -> Graphic u
bbrectangle (BBox p1@(P2 llx lly) p2@(P2 urx ury))
    | llx == urx && lly == ury = emptyLocGraphic `at` p1
    | otherwise                = 
        localize drawing_props $ rect1 `oplus` cross
  where
    drawing_props = cap_round . dotted_line
    rect1         = strokedRectangle (urx-llx) (ury-lly) `at` p1
    cross         = straightLineGraphic p1 p2 
                      `oplus` straightLineGraphic (P2 llx ury) (P2 urx lly)

