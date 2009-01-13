{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.FreeType
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Binding to some of the FreeType2 library.
--
--------------------------------------------------------------------------------

module Graphics.Rendering.FreeType (
  module Graphics.Rendering.FreeType.BaseInterface,
  module Graphics.Rendering.FreeType.FixedPrecision,
  module Graphics.Rendering.FreeType.GlyphManagement,
  module Graphics.Rendering.FreeType.Outline
) where

import Graphics.Rendering.FreeType.BaseInterface
import Graphics.Rendering.FreeType.FixedPrecision
import Graphics.Rendering.FreeType.GlyphManagement
import Graphics.Rendering.FreeType.Outline

