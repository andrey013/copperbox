{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenVG.Raw.VGU.VGU
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  unstable
-- Portability :  GHC
--
-- This module corresponds to section 16 (The VGU Utility Library) 
-- of the OpenVG 1.0.1 specs.
--
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenVG.Raw.VGU.VGU (
  -- * Functions
  vguLine,
  vguPolygon,
  vguRect,
  vguRoundRect,
  vguEllipse,
  vguArc,
  
  -- * Tokens
  vgu_NO_ERROR,
  vgu_BAD_HANDLE_ERROR,
  vgu_ILLEGAL_ARGUMENT_ERROR,
  vgu_OUT_OF_MEMORY_ERROR,
  vgu_PATH_CAPABILITY_ERROR,
  vgu_BAD_WARP_ERROR,

  vgu_ARC_OPEN,
  vgu_ARC_CHORD,
  vgu_ARC_PIE

) where


import Graphics.Rendering.OpenVG.Raw.VGU.CInternals


