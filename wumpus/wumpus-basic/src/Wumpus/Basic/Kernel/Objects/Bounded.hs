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


  , emptyBoundedLocGraphic
  , emptyBoundedLocThetaGraphic

  , centerOrthoBBox

  , illustrateBounded
  , illustrateBoundedLoc
  , illustrateBoundedLocTheta

  , bbrectangle

  ) where

import Wumpus.Basic.Kernel.Base.BaseDefs
import Wumpus.Basic.Kernel.Base.QueryDC
import Wumpus.Basic.Kernel.Base.UpdateDC
import Wumpus.Basic.Kernel.Objects.DrawingPrimitives
import Wumpus.Basic.Kernel.Objects.ImageBasis
import Wumpus.Basic.Kernel.Objects.Image
import Wumpus.Basic.Kernel.Objects.LocImage
import Wumpus.Basic.Kernel.Objects.LocThetaImage
import Wumpus.Basic.Kernel.Objects.Query

import Wumpus.Core                              -- package: wumpus-core


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







-- | 'centerOrthoBBox' : @ theta * bbox -> BBox @
-- 
-- Rotate a bounding box by @theta@ about its center. Take the 
-- new bounding box.
--
-- Remember that bounding boxes are always orthonormal rectangles,
-- so the dimensions as well as the positions may change under 
-- rotation. 
--

centerOrthoBBox :: (Fractional u, InterpretUnit u)
                => Radian -> BoundingBox u -> Query (BoundingBox u)
centerOrthoBBox theta bb = 
    makeQuery point_size 
              (\sz -> let dbb = uconvertExt sz bb
                          ps  = boundaryCornerList dbb
                          ctr = boundaryCenter dbb
                          ans = traceBoundary $ map (rotateAbout theta ctr) ps
                      in uconvertExt sz ans)



-- | 'emptyBoundedLocGraphic' : @ BoundedLocGraphic @
--
-- Build an empty 'BoundedLocGraphic'.
-- 
-- The 'emptyBoundedLocGraphic' is treated as a /null primitive/ 
-- by @Wumpus-Core@ and is not drawn, although it does generate
-- the minimum bounding box with both the bottom-left and 
-- upper-right corners at the implicit start point.
--
emptyBoundedLocGraphic :: InterpretUnit u => BoundedLocGraphic u
emptyBoundedLocGraphic = intoLocImage fn emptyLocGraphic
  where
    fn = makeQuery id (\_ pt -> BBox pt pt)




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
emptyBoundedLocThetaGraphic :: InterpretUnit u => BoundedLocThetaGraphic u
emptyBoundedLocThetaGraphic = intoLocThetaImage fn emptyLocThetaGraphic
  where
    fn = makeQuery id (\_ pt _ -> BBox pt pt)

--------------------------------------------------------------------------------
-- 


-- | Draw a BoundedGraphic, illustrating the bounding box.
--
illustrateBounded :: InterpretUnit u
                  => BoundedGraphic u -> BoundedGraphic u
illustrateBounded gf = annotate gf bbrectangle



-- | Draw a BoundedLocGraphic, illustrating the bounding box.
--
illustrateBoundedLoc :: InterpretUnit u
                     => BoundedLocGraphic u -> BoundedLocGraphic u
illustrateBoundedLoc gf = annotate gf fn
  where
    fn bb = lift_li1 (bbrectangle bb)






-- | Draw a BoundedLocThetaGraphic, illustrating the bounding box.
--
illustrateBoundedLocTheta :: InterpretUnit u
                          => BoundedLocThetaGraphic u 
                          -> BoundedLocThetaGraphic u
illustrateBoundedLocTheta gf = annotate gf fn
  where
    fn bb = lift_lti2 (bbrectangle bb)




bbrectangle :: InterpretUnit u => BoundingBox u -> Graphic u
bbrectangle (BBox p1@(P2 llx lly) p2@(P2 urx ury))
    | llx == urx && lly == ury = emptyLocGraphic `at` p1
    | otherwise                = 
        localize drawing_props $ rect1 `oplus` cross
  where
    drawing_props = cap_round . dotted_line
    rect1         = strokedRectangle (urx-llx) (ury-lly) `at` p1
    cross         = straightLine p1 p2 
                      `oplus` straightLine (P2 llx ury) (P2 urx lly)

