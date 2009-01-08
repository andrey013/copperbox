{-# LANGUAGE CPP                        #-}
{-# LANGUAGE ForeignFunctionInterface   #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenVG.VGU.CInternals
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Internal module declaring all the datatypes and foreign declarations 
-- defined in <vg/openvgu.h>.
--
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenVG.VGU.CInternals where

#include <vg/vgu.h>

import Graphics.Rendering.OpenVG.VG.BasicTypes

import Foreign.Ptr ( Ptr )

type VGUErrorCode' = VGenum

-- | Enumerations

#{enum VGenum,
  , vgu_NO_ERROR                        = VGU_NO_ERROR
  , vgu_BAD_HANDLE_ERROR                = VGU_BAD_HANDLE_ERROR
  , vgu_ILLEGAL_ARGUMENT_ERROR          = VGU_ILLEGAL_ARGUMENT_ERROR
  , vgu_OUT_OF_MEMORY_ERROR             = VGU_OUT_OF_MEMORY_ERROR
  , vgu_PATH_CAPABILITY_ERROR           = VGU_PATH_CAPABILITY_ERROR
  , vgu_BAD_WARP_ERROR                  = VGU_BAD_WARP_ERROR
  }

#{enum VGenum,
  , vgu_ARC_OPEN                        = VGU_ARC_OPEN
  , vgu_ARC_CHORD                       = VGU_ARC_CHORD
  , vgu_ARC_PIE                         = VGU_ARC_PIE
} 
  

foreign import ccall unsafe "vg/openvg.h vguLine" 
    vguLine :: VGPath 
            -> VGfloat -> VGfloat -> VGfloat -> VGfloat 
            -> IO VGUErrorCode'

foreign import ccall unsafe "vg/openvg.h vguPolygon"
    vguPolygon :: VGPath 
               -> Ptr VGfloat -> VGint -> VGboolean 
               -> IO VGUErrorCode' 

foreign import ccall unsafe "vg/openvg.h vguRect"
    vguRect :: VGPath 
            -> VGfloat -> VGfloat -> VGfloat -> VGfloat 
            -> IO VGUErrorCode'     

foreign import ccall unsafe "vg/openvg.h vguRoundRect"
    vguRoundRect :: VGPath 
                 -> VGfloat -> VGfloat -> VGfloat -> VGfloat
                 -> VGfloat -> VGfloat 
                 -> IO VGUErrorCode'  

foreign import ccall unsafe "vg/openvg.h vguEllipse"
    vguEllipse :: VGPath 
               -> VGfloat -> VGfloat -> VGfloat -> VGfloat 
               -> IO VGUErrorCode'  

foreign import ccall unsafe "vg/openvg.h vguArc"
    vguArc :: VGPath 
           -> VGfloat -> VGfloat -> VGfloat -> VGfloat
           -> VGfloat -> VGfloat
           -> VGenum 
           -> IO VGUErrorCode'  
                 
{-

-- not implemented by shiva-vg

foreign import ccall unsafe "vg/openvg.h vguComputeWarpQuadToSquare"
    vguComputeWarpQuadToSquare :: VGfloat -> VGfloat 
                               -> VGfloat -> VGfloat
                               -> VGfloat -> VGfloat
                               -> VGfloat -> VGfloat
                               -> Ptr VGfloat
                               -> IO VGUErrorCode'  


foreign import ccall unsafe "vg/openvg.h vguComputeWarpSquareToQuad"
    vguComputeWarpSquareToQuad :: VGfloat -> VGfloat 
                               -> VGfloat -> VGfloat
                               -> VGfloat -> VGfloat
                               -> VGfloat -> VGfloat
                               -> Ptr VGfloat
                               -> IO VGUErrorCode'  

foreign import ccall unsafe "vg/openvg.h vguComputeWarpQuadToQuad"
    vguComputeWarpQuadToQuad :: VGfloat -> VGfloat 
                             -> VGfloat -> VGfloat
                             -> VGfloat -> VGfloat
                             -> VGfloat -> VGfloat
                             -> VGfloat -> VGfloat 
                             -> VGfloat -> VGfloat
                             -> VGfloat -> VGfloat
                             -> VGfloat -> VGfloat
                             -> Ptr VGfloat
                             -> IO VGUErrorCode'  
-}  
