{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenVG
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- A Haskell binding for the OpenVG vector and raster graphics API.
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenVG (
  module Graphics.Rendering.OpenVG.VG,
  module Graphics.Rendering.OpenVG.VGU,
) where

import Graphics.Rendering.OpenVG.VG
import Graphics.Rendering.OpenVG.VGU
