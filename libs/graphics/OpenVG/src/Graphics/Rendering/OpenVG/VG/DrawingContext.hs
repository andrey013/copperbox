{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenVG.VG.ShivaExtensions
-- Copyright   :  (c) Stephen Tetley 2008, 2009
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

module Graphics.Rendering.OpenVG.VG.DrawingContext (
  -- * Errors
  VG_ErrorCode(..),
  getError,
  
  -- * Forcing drawing to complete
  flush, 
  finish


) where


import Graphics.Rendering.OpenVG.Raw.VG.Core101 ( VGenum )
import Graphics.Rendering.OpenVG.Raw.VG.DrawingContext

import Control.Monad ( liftM )

--------------------------------------------------------------------------------
-- Errors


data VG_ErrorCode =
     VG_NoError
   | VG_BadHandle
   | VG_IllegalArgument
   | VG_OutOfMemory
   | VG_PathCapability
   | VG_UnsupportedImageFormat
   | VG_UnsupportedPathFormat
   | VG_ImageInUse
   | VG_NoContextError
   deriving ( Eq, Ord, Show )

-- | 
getError :: IO VG_ErrorCode
getError = liftM unmarshalErrorCode vgGetError 



--------------------------------------------------------------------------------
--  Forcing drawing to complete

-- | 'flush' ensures all the outstanding drawing requests on the 
-- current context are completed. The call may return before the 
-- actual drawing takes place.
--
-- 'flush' corresponds to the OpenVG call @vgFlush@.
-- 
flush :: IO ()
flush = vgFlush

-- | 'finish' forces all the outstanding drawing requests on the 
-- current context are performed. The call returns when drawing is
-- completed.
--
-- 'finish' corresponds to the OpenVG call @vgFinish@.
--
finish :: IO ()
finish = vgFinish


-------------------------------------------------------------------------------- 

unmarshalErrorCode :: VGenum -> VG_ErrorCode
unmarshalErrorCode x 
    | x == vg_NO_ERROR                       = VG_NoError
    | x == vg_BAD_HANDLE_ERROR               = VG_BadHandle
    | x == vg_ILLEGAL_ARGUMENT_ERROR         = VG_IllegalArgument
    | x == vg_OUT_OF_MEMORY_ERROR            = VG_OutOfMemory
    | x == vg_PATH_CAPABILITY_ERROR          = VG_PathCapability
    | x == vg_UNSUPPORTED_IMAGE_FORMAT_ERROR = VG_UnsupportedImageFormat
    | x == vg_UNSUPPORTED_PATH_FORMAT_ERROR  = VG_UnsupportedPathFormat 
    | x == vg_IMAGE_IN_USE_ERROR             = VG_ImageInUse
    | x == vg_NO_CONTEXT_ERROR               = VG_NoContextError
    | otherwise = error ("unmarshalErrorCode: illegal value " ++ show x)
