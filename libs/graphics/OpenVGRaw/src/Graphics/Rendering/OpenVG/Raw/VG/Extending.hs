{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenVG.Raw.VG.Extending
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  unstable
-- Portability :  GHC
--
-- This module corresponds to section 14 (Extending the API) 
-- of the OpenVG 1.0.1 specs.
--
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenVG.Raw.VG.Extending (
  -- * Tokens
  vg_VENDOR,
  vg_RENDERER,
  vg_VERSION,
  vg_EXTENSIONS,
 
  -- * Functions
  vgGetString
) where

import Graphics.Rendering.OpenVG.Raw.VG.Core101
