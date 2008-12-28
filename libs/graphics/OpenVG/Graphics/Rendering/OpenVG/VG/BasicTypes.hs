{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenVG.VG.BasicTypes
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- This module corresponds to section 3.2 (Primitive Data Types) 
-- of the OpenVG 1.1 specs.
--
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenVG.VG.BasicTypes (
  -- * Primitive Types
  VGbyte, VGubyte, VGshort, VGint, VGuint, VGbitfield, 
  VGboolean, VGfloat,
  VGenum,
) where

import Graphics.Rendering.OpenVG.Config