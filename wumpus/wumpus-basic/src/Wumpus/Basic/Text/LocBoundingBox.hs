{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.Text.LocBoundingBox
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Bounding Box as a functional from some start point.
--
--------------------------------------------------------------------------------

module Wumpus.Basic.Text.LocBoundingBox
  ( 
  
    LocBoundingBox
  , runLocBoundingBox

  , locBoundingBox
  , oLocBoundingBox

  , locBoundaryUnion
  , shiftUnion

  ) where


import Wumpus.Core                              -- package: wumpus-core

import Data.AffineSpace

--------------------------------------------------------------------------------

newtype LocBoundingBox u = LocBoundingBox 
          { getLocBoundingBox :: Point2 u -> BoundingBox u }

runLocBoundingBox :: Point2 u -> LocBoundingBox u -> BoundingBox u
runLocBoundingBox pt bb = getLocBoundingBox bb pt


locBoundingBox :: Num u => u -> u -> u -> u -> LocBoundingBox u
locBoundingBox llx lly urx ury = LocBoundingBox $ \(P2 x y) ->
    let ll = P2 (x+llx) (y+lly)
        ur = P2 (x+urx) (y+ury)
    in BBox ll ur 

oLocBoundingBox :: Num u => u -> u -> LocBoundingBox u
oLocBoundingBox w h = LocBoundingBox $ \pt -> BBox pt (pt .+^ vec w h)


locBoundaryUnion  :: Ord u 
                  => LocBoundingBox u -> LocBoundingBox u -> LocBoundingBox u
locBoundaryUnion a b = LocBoundingBox $ \pt -> 
    getLocBoundingBox a pt `boundaryUnion`  getLocBoundingBox b pt

-- Note - this shifts the start point, /concatenation/ of the
-- advance vectors in TextPaths shifts the end point.
--
shiftUnion  :: (Num u, Ord u)
            => LocBoundingBox u -> Vec2 u -> LocBoundingBox u 
            -> LocBoundingBox u
shiftUnion a v b = LocBoundingBox $ \pt -> 
    getLocBoundingBox a pt `boundaryUnion`  getLocBoundingBox b (pt .+^ v)