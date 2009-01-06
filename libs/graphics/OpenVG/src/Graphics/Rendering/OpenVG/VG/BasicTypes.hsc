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
-- This module corresponds to sections 3.2 (Primitive Data Types) 
-- and 3.6 (Handle-based Data Types) of the OpenVG 1.0.1 specs.
-- 
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenVG.VG.BasicTypes (
  -- * Primitive Types
  VGbyte, VGubyte, VGshort, VGint, VGuint, VGbitfield, 
  VGboolean, VGfloat,
  VGenum,
  
  vg_FALSE, vg_TRUE, marshalBool,
  
  -- * Handle-based Types
  VGHandle,
  vg_INVALID_HANDLE,
  
  VGPath, VGImage, VGPaint
  
) where

#include <vg/openvg.h>

import Data.Word ( Word32 )
import Graphics.Rendering.OpenGL.GL.BasicTypes

type VGfloat    = GLfloat
type VGbyte     = GLbyte
type VGubyte    = GLubyte
type VGshort    = GLshort
type VGint      = GLint
type VGuint     = GLuint
type VGbitfield = GLbitfield

type VGenum     = GLenum 



-- | The type of data that can be displayed.
type VGboolean = GLint

#{enum VGboolean,
  , vg_FALSE    = VG_FALSE
  , vg_TRUE     = VG_TRUE
  }

marshalBool :: Bool -> VGboolean
marshalBool x = case x of
  True -> vg_TRUE
  False -> vg_FALSE

  
type VGHandle = #type VGHandle

vg_INVALID_HANDLE :: VGHandle 
vg_INVALID_HANDLE = #const VG_INVALID_HANDLE

type VGPath  = VGHandle
type VGImage = VGHandle
type VGPaint = VGHandle

