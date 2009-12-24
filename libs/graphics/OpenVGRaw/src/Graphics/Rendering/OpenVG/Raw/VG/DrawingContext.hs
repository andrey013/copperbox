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
-- This module corresponds to sections 4.1 (Errors) and 4.3 
-- (Forcing Drawing to Complete) of the OpenVG 1.0.1 specs.
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenVG.Raw.VG.DrawingContext (
  -- * Tokens
  vg_NO_ERROR, 
  vg_BAD_HANDLE_ERROR, 
  vg_ILLEGAL_ARGUMENT_ERROR,
  vg_OUT_OF_MEMORY_ERROR, 
  vg_PATH_CAPABILITY_ERROR,
  vg_UNSUPPORTED_IMAGE_FORMAT_ERROR, 
  vg_UNSUPPORTED_PATH_FORMAT_ERROR,
  vg_IMAGE_IN_USE_ERROR,
  vg_NO_CONTEXT_ERROR,
  
  -- * Functions
  vgGetError,
  vgFlush, 
  vgFinish


) where

import Graphics.Rendering.OpenVG.Raw.VG.Core101


