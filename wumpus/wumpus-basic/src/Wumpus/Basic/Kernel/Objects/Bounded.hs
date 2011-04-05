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
-- These abojects are type synonyms.
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
import Wumpus.Basic.Kernel.Objects.Basis
import Wumpus.Basic.Kernel.Objects.DrawingPrimitives
import Wumpus.Basic.Kernel.Objects.Image
import Wumpus.Basic.Kernel.Objects.LocImage
import Wumpus.Basic.Kernel.Objects.LocThetaImage

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

centerOrthoBBox :: (Real u, Floating u, Ord u)
                => Radian -> BoundingBox u -> BoundingBox u
centerOrthoBBox theta bb = traceBoundary $ map (rotateAbout theta ctr) ps
  where
    ctr = boundaryCenter bb
    ps  = boundaryCornerList bb


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
    fn = promoteR1 $ \pt -> return $ BBox pt pt




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
    fn = promoteR2 $ \pt _ -> return $ BBox pt pt


--
-- NOTE - CONCATENATION
--
-- Because there is no tangible relation between the start point 
-- and answer BoundingBox, LocBoundedGraphic supports exactly the
-- same concatenation as LocImage.
--
-- PosImage and AdvGraphic are the objects that support more 
-- sophisticated concatenation.
--
 


--------------------------------------------------------------------------------
-- 


-- | Draw a BoundedGraphic, illustrating the bounding box.
--
illustrateBoundedGraphic :: InterpretUnit u
                         => BoundedGraphic u -> BoundedGraphic u
illustrateBoundedGraphic gf = elaborate gf bbrectangle



-- | Draw a BoundedLocGraphic, illustrating the bounding box.
--
illustrateBoundedLocGraphic :: InterpretUnit u
                            => BoundedLocGraphic u -> BoundedLocGraphic u
illustrateBoundedLocGraphic gf = elaborate gf fn
  where
    fn bb = lift0R1 (bbrectangle bb)






-- | Draw a BoundedLocThetaGraphic, illustrating the bounding box.
--
illustrateBoundedLocThetaGraphic :: InterpretUnit u
                                 => BoundedLocThetaGraphic u 
                                 -> BoundedLocThetaGraphic u
illustrateBoundedLocThetaGraphic gf = elaborate gf fn
  where
    fn bb = lift0R2 (bbrectangle bb)




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

