{-# LANGUAGE CPP                        #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenVG.Config
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- This purely internal module defines the platform-specific stuff which has
-- been figured out by configure.
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenVG.Config (
  -- VG types
  VGbyte, VGubyte, VGshort, VGint, VGuint, VGbitfield, 
  VGboolean, VGfloat,
  VGenum
  
) where

import Data.Int
import Data.Word

--------------------------------------------------------------------------------

#include "HsOpenVGConfig.h"

--------------------------------------------------------------------------------


-- | 8-bit two\'s complement signed integer.
type VGbyte = Int8

-- | 8-bit unsigned signed integer.
type VGubyte = Word8

-- | 16-bit two\'s complement signed integer.
type VGshort = Int16

-- | 32-bit two\'s complement signed integer.
type VGint = Int32

-- | 32-bit unsigned signed integer.
type VGuint = Word32

-- | 32-bit unsigned signed integer - for parameters that may contain
-- single-bit values.
type VGbitfield = Word32

-- | An enumeration { VG_FALSE(0), VG_TRUE(1) }, any non-zero value
-- is interpreted as True.
-- (As per Haskell OpenGL binding, implemented as Word8) 
type VGboolean = Word8

-- | A 32-bit IEEE 754 floating-point value.
type VGfloat = Float

-- | Enumerated integer types
type VGenum = Word32
