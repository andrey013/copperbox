{-# LANGUAGE CPP                        #-}
{-# LANGUAGE ForeignFunctionInterface   #-}
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
-- Types defined in <vg/openvg.h>.
--
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenVG.VG.BasicTypes (
  -- * Primitive Types
  VGbyte, VGubyte, VGshort, VGint, VGuint, VGbitfield, 
  VGboolean, VGfloat,
  VGenum,
  
  vg_FALSE, vg_TRUE,
  
  VGHandle,
  vg_INVALID_HANDLE,
  
  VGImage',
  
) where

#include <vg/openvg.h>

import Data.Word ( Word32 )
import Foreign.C.Types ( CFloat, CChar, CUChar, CShort, CInt, CUInt )

type VGfloat    = CFloat
type VGbyte     = CChar
type VGubyte    = CUChar
type VGshort    = CShort
type VGint      = CInt
type VGuint     = CUInt
type VGbitfield = CUInt

type VGenum     = Word32   {- Hmmm?? -}



-- | The type of data that can be displayed.
type VGboolean = CInt

#{enum VGboolean,
  , vg_FALSE    = VG_FALSE
  , vg_TRUE     = VG_TRUE
  }
  
type VGHandle = #type VGHandle

vg_INVALID_HANDLE :: VGHandle 
vg_INVALID_HANDLE = #const VG_INVALID_HANDLE

type VGImage' = VGHandle


