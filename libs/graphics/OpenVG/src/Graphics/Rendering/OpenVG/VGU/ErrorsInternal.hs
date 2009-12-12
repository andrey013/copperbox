{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenVG.VG.Errors
-- Copyright   :  (c) Stephen Tetley 2008, 2009
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- VGU error codes.
--
-- Portions of code have been copied from 
-- 
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenVG.VGU.ErrorsInternal (
   VGU_ErrorCode(..),
   withErrorCode,
   unmarshalErrorCode
) where

import Graphics.Rendering.OpenVG.VG.BasicTypes ( VGenum )
import Graphics.Rendering.OpenVG.VGU.CInternals

import Control.Monad ( liftM )

--------------------------------------------------------------------------------
-- | VGU errors.
       
data VGU_ErrorCode = 
     VGU_NoError
   | VGU_BadHandle
   | VGU_IllegalArgument
   | VGU_OutOfMemory
   | VGU_PathCapability
   | VGU_BadWarp
   deriving ( Eq, Ord, Show )


withErrorCode :: IO VGenum -> IO VGU_ErrorCode
withErrorCode f = liftM unmarshalErrorCode f 


unmarshalErrorCode :: VGenum -> VGU_ErrorCode
unmarshalErrorCode x
    | x == vgu_NO_ERROR               = VGU_NoError
    | x == vgu_BAD_HANDLE_ERROR       = VGU_BadHandle
    | x == vgu_ILLEGAL_ARGUMENT_ERROR = VGU_IllegalArgument
    | x == vgu_OUT_OF_MEMORY_ERROR    = VGU_OutOfMemory
    | x == vgu_PATH_CAPABILITY_ERROR  = VGU_PathCapability
    | x == vgu_BAD_WARP_ERROR         = VGU_BadWarp
    | otherwise = error ("unmarshalErrorCode: illegal value " ++ show x)
