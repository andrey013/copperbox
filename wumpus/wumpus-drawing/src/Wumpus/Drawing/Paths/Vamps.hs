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
-- Shim import module for the Absolute Path modules.
--
-- 
--------------------------------------------------------------------------------

module Wumpus.Drawing.Paths.Vamps
  ( 

    -- * Re-exported types
    Vamp(..)
  , PathEnd(..)

  , circleVamp

  ) where

import Wumpus.Drawing.Paths.Base.BuildCommon
import Wumpus.Drawing.Paths.Base.RelPath

import Wumpus.Basic.Geometry                    -- package: wumpus-basic

import Wumpus.Core                              -- package: wumpus-core

import Data.VectorSpace                         -- package: vector-space


-- TODO - library of useful / illustrative vamps (circle, square etc.)




circleVamp :: (Fractional u, Floating u) => Vec2 u -> Vamp u
circleVamp v0 = Vamp { vamp_move_span   = v0
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