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
-- This module corresponds to section 4 (DrawingContext) 
-- of the OpenVG 1.1 specs.
--
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenVG.VG.Errors where

import Graphics.Rendering.OpenGL.GL.StateVar (
   GettableStateVar, makeGettableStateVar )

--------------------------------------------------------------------------------

-- | VG errors.

data VG_ErrorCode =
     VG_NoError                         -- 0
   | VG_BadHandleError                  -- 0x1000
   | VG_IllegalArgumentError            -- 0x1001
   | VG_OutOfMemoryError                -- 0x1002
   | VG_PathCapabilityError             -- 0x1003
   | VG_UnsupportedImageFormatError     -- 0x1004
   | VG_UnsupportedPathFormatError      -- 0x1005
   | VG_ImageInUseError                 -- 0x1006
   | VG_NoContextError
   deriving ( Eq, Ord, Show )
   
