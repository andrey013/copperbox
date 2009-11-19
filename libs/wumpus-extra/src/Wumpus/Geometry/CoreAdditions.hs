{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Geometry.CoreAdditions
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Additions due to be added to Wumpus.Core
--
--------------------------------------------------------------------------------


module Wumpus.Geometry.CoreAdditions
  (
  -- * Version
    wumpus_core_version

  -- * Pending additions to Wumpus.Core
  , path
  , lineTo 
  , curveTo

  ) where

import Wumpus.Core
import Wumpus.Core.PictureInternal

wumpus_core_version :: (Int,Int,Int)
wumpus_core_version = (0,9,6)



-- Pending addition to wumpus-core

-- Name change...
type PathSegment u = PathSeg u

path :: Point2 u -> [PathSegment u] -> Path u
path = Path 

lineTo :: Point2 u -> PathSegment u
lineTo = PLine

curveTo :: Point2 u -> Point2 u -> Point2 u -> PathSegment u
curveTo = PCurve
