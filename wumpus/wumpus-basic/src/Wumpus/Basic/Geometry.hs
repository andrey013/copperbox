{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.Kernel
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  highly unstable
-- Portability :  GHC 
--
-- Import shim for @Wumpus.Basic.Geometry@ modules.
--
--
--------------------------------------------------------------------------------

module Wumpus.Basic.Geometry
  (
    module Wumpus.Basic.Geometry.Base
  , module Wumpus.Basic.Geometry.Illustrate
  , module Wumpus.Basic.Geometry.Intersection
  , module Wumpus.Basic.Geometry.Paths
  , module Wumpus.Basic.Geometry.Quadrant
  , module Wumpus.Basic.Geometry.Vertices

  ) where


import Wumpus.Basic.Geometry.Base
import Wumpus.Basic.Geometry.Illustrate
import Wumpus.Basic.Geometry.Intersection
import Wumpus.Basic.Geometry.Paths
import Wumpus.Basic.Geometry.Quadrant
import Wumpus.Basic.Geometry.Vertices
