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

  , illustrateBoundedGraphic
  , illustrateBoundedLocGraphic


  ) where

import Wumpus.Basic.Kernel.Base.BaseDefs
import Wumpus.Basic.Kernel.Base.ContextFun
import Wumpus.Basic.Kernel.Base.DrawingContext
import Wumpus.Basic.Kernel.Base.UpdateDC
import Wumpus.Basic.Kernel.Objects.BaseObjects
import Wumpus.Basic.Kernel.Objects.Graphic

import Wumpus.Core                              -- package: wumpus-core
import Wumpus.Core.Colour ( blue )

--------------------------------------------------------------------------------

-- | Graphic with a bounding box.
-- 
type BoundedGraphic u      = Image u (BoundingBox u)

type DBoundedGraphic       = BoundedGraphic Double




-- | LocGraphic with a bounding box.
--
type BoundedLocGraphic u      = LocImage u (BoundingBox u)

type DBoundedLocGraphic       = BoundedLocGraphic Double



--------------------------------------------------------------------------------
-- 

illustrateBoundedGraphic :: Fractional u => BoundedGraphic u -> BoundedGraphic u
illustrateBoundedGraphic mf = 
    CF $ \ctx -> let (bb,g1) = unCF mf ctx
                     (_, g0) = unCF (bbrectangle bb) ctx
                 in (bb, g0 `oplus` g1)  -- bb first


illustrateBoundedLocGraphic :: Fractional u 
                            => BoundedLocGraphic u -> BoundedLocGraphic u
illustrateBoundedLocGraphic mf = 
    CF1 $ \ctx pt -> unCF (illustrateBoundedGraphic $ mf `at` pt) ctx


bbrectangle :: Fractional u => BoundingBox u -> Graphic u
bbrectangle (BBox p1@(P2 llx lly) p2@(P2 urx ury)) = 
    localize drawing_props $ rect1 `oplus` cross
  where
    drawing_props = strokeColour blue . capRound . dashPattern (Dash 0 [(1,2)])
    rect1         = strokedRectangle (urx-llx) (ury-lly) `at` p1
    cross         = straightLineBetween p1 p2 
                      `oplus` straightLineBetween (P2 llx ury) (P2 urx lly)
