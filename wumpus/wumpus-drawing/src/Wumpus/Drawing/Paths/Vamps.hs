{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Drawing.Paths.Vamps
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Library of vamps (currently small).
--
-- 
--------------------------------------------------------------------------------

module Wumpus.Drawing.Paths.Vamps
  ( 

    squareWE

  ) where

import Wumpus.Drawing.Paths.Base.PathBuilder
import Wumpus.Drawing.Paths.Base.RelPath

import Wumpus.Basic.Kernel

import Wumpus.Core                              -- package: wumpus-core



-- TODO - library of useful / illustrative vamps (circle, square etc.)




squareWE :: (Fractional u, Floating u) => u -> Vamp u
squareWE diam = makeVamp (hvec diam) rpath (SUBPATH_CLOSED STROKE)
  where
    hdiam = 0.5 * diam
    rpath = vertexPath [ vvec hdiam, hvec diam, vvec (-diam), hvec (-diam) ]


-- Drawing a cirle is probably best done with 90deg arcs.
