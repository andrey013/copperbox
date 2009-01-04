{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenVG.VG.Errors
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- This module corresponds to section 4.1 (Errors) 
-- of the OpenVG 1.0.1 specs.
--
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenVG.VG.Errors where

{-
import Graphics.Rendering.OpenGL.GL.StateVar (
   GettableStateVar, makeGettableStateVar )
-}

--------------------------------------------------------------------------------

-- | VG errors.

data VG_ErrorCode =
     VG_NoError
   | VG_BadHandleError
   | VG_IllegalArgumentError
   | VG_OutOfMemoryError
   | VG_PathCapabilityError
   | VG_UnsupportedImageFormatError
   | VG_UnsupportedPathFormatError
   | VG_ImageInUseError
   | VG_NoContextError
   deriving ( Eq, Ord, Show )
   
