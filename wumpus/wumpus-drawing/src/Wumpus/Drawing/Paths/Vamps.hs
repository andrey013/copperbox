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

-- import Wumpus.Basic.Geometry                    -- package: wumpus-basic
import Wumpus.Basic.Kernel

import Wumpus.Core                              -- package: wumpus-core



-- TODO - library of useful / illustrative vamps (circle, square etc.)




squareWE :: Fractional u => u -> Vamp u
squareWE diam = makeVamp (hvec diam) rpath (SUBPATH_CLOSED STROKE)
  where
    hdiam = 0.5 * diam
    rpath = vertexPath [ vvec hdiam, hvec diam, vvec (-diam), hvec (-diam) ]

{-
Vamp { vamp_move_span   = v0
                     , vamp_move_start  = half_v0 ^+^ v1
                     , vamp_dc_update   = id
                     , vamp_deco_path   = path
                     , vamp_path_end    = PATH_CLOSED
                     }
  where
    radius      = 0.5 * vlength v0
    half_v0     = 0.5 *^ v0
    (v1,path)   = fromPathAlgCurves $ circlePathAlg radius

    -- Note - circlePathAlg starts from east

-}