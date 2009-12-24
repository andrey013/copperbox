{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenVG.Raw.VG.ShivaExtensions
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  unstable
-- Portability :  GHC
--
-- Shiva extensions to replace EGL - see the README in the shiva-vg
-- archive.
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenVG.Raw.VG.ShivaExtensions (
  -- * Functions
  vgCreateContextSH,
  vgResizeSurfaceSH,
  vgDestroyContextSH

) where

import Graphics.Rendering.OpenVG.Raw.VG.Core101

