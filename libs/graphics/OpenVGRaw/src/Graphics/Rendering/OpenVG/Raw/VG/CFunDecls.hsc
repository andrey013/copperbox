{-# LANGUAGE CPP                        #-}
{-# LANGUAGE ForeignFunctionInterface   #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenVG.Raw.VG.CFunDecls
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Internal module declaring all the foreign declarations of the 
-- functions defined in <vg/openvg.h>.
--
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenVG.Raw.VG.CFunDecls where

#include <vg/openvg.h>

import Graphics.Rendering.OpenVG.Raw.VG.BasicTypes

import Foreign.Ptr ( Ptr )
import Foreign.C.String ( CString )


--------------------------------------------------------------------------------
--
-- Section 4 - Drawing Context


foreign import ccall unsafe "vg/openvg.h vgGetError"
    vgGetError :: IO VGenum



foreign import ccall unsafe "vg/openvg.h vgFlush"
    vgFlush :: IO ()

foreign import ccall unsafe "vg/openvg.h vgFinish"
    vgFinish ::  IO ()

--------------------------------------------------------------------------------
--
-- Section 5 - Setting API Parameters

foreign import ccall unsafe "vg/openvg.h vgSeti"
    vgSeti :: VGenum -> VGint -> IO ()

foreign import ccall unsafe "vg/openvg.h vgSetf"
    vgSetf :: VGenum -> VGfloat -> IO ()

foreign import ccall unsafe "vg/openvg.h vgSetfv"
    vgSetfv :: VGenum -> VGint -> Ptr VGfloat -> IO ()

foreign import ccall unsafe "vg/openvg.h vgSetiv"
    vgSetiv :: VGenum -> VGint -> Ptr VGint -> IO ()

foreign import ccall unsafe "vg/openvg.h vgGetf"
    vgGetf :: VGenum -> IO VGfloat

foreign import ccall unsafe "vg/openvg.h vgGeti"
    vgGeti :: VGenum -> IO VGint

foreign import ccall unsafe "vg/openvg.h vgGetVectorSize"
    vgGetVectorSize :: VGenum -> IO VGint

foreign import ccall unsafe "vg/openvg.h vgGetfv"
    vgGetfv :: VGenum -> VGint -> IO (Ptr VGfloat)

foreign import ccall unsafe "vg/openvg.h vgGetiv"
    vgGetiv :: VGenum -> VGint -> IO (Ptr VGint)

foreign import ccall unsafe "vg/openvg.h vgSetParameterf"
    vgSetParameterf :: VGHandle -> VGenum -> VGfloat -> IO ()

foreign import ccall unsafe "vg/openvg.h vgSetParameteri"
    vgSetParameteri :: VGHandle -> VGenum -> VGint -> IO ()

foreign import ccall unsafe "vg/openvg.h vgSetParameterfv"
    vgSetParameterfv :: VGHandle -> VGenum -> VGint -> Ptr VGfloat -> IO ()

foreign import ccall unsafe "vg/openvg.h vgSetParameteriv"
    vgSetParameteriv :: VGHandle -> VGenum -> VGint -> Ptr VGint -> IO ()

foreign import ccall unsafe "vg/openvg.h vgGetParameterf"
    vgGetParameterf :: VGHandle -> VGenum -> IO VGfloat

foreign import ccall unsafe "vg/openvg.h vgGetParameteri"
    vgGetParameteri :: VGHandle -> VGenum -> IO VGint

foreign import ccall unsafe "vg/openvg.h vgGetParameterVectorSize"
    vgGetParameterVectorSize :: VGHandle -> VGenum -> IO VGint

foreign import ccall unsafe "vg/openvg.h vgGetParameterfv"
    vgGetParameterfv :: VGHandle -> VGenum -> VGint -> IO (Ptr VGfloat)

foreign import ccall unsafe "vg/openvg.h vgGetParameteriv"
    vgGetParameteriv :: VGHandle -> VGenum -> VGint -> IO (Ptr VGint)

--------------------------------------------------------------------------------
-- Section 6 - Rendering Quality

foreign import ccall unsafe "vg/openvg.h vgLoadIdentity"
    vgLoadIdentity :: IO ()

foreign import ccall unsafe "vg/openvg.h vgLoadMatrix"
    vgLoadMatrix :: Ptr VGfloat -> IO ()

foreign import ccall unsafe "vg/openvg.h vgGetMatrix"
    vgGetMatrix :: Ptr VGfloat -> IO ()

foreign import ccall unsafe "vg/openvg.h vgMultMatrix"
    vgMultMatrix :: Ptr VGfloat -> IO ()

foreign import ccall unsafe "vg/openvg.h vgTranslate"
    vgTranslate :: VGfloat -> VGfloat -> IO ()

foreign import ccall unsafe "vg/openvg.h vgScale"
    vgScale :: VGfloat -> VGfloat -> IO ()

foreign import ccall unsafe "vg/openvg.h vgShear"
    vgShear :: VGfloat -> VGfloat -> IO ()

foreign import ccall unsafe "vg/openvg.h vgRotate"
    vgRotate :: VGfloat -> IO ()

--------------------------------------------------------------------------------
-- Section 7 - Scissoring

-- Stubbed in shivaVG 0.2.1 (not implemented)
foreign import ccall unsafe "vg/openvg.h vgMask"
    vgMask :: VGImage -> VGenum -> VGint -> VGint -> VGint -> VGint -> IO ()


foreign import ccall unsafe "vg/openvg.h vgClear"
    vgClear :: VGint -> VGint -> VGint -> VGint -> IO ()


--------------------------------------------------------------------------------
-- Section 8 - Paths

foreign import ccall unsafe "vg/openvg.h vgCreatePath"
    vgCreatePath :: VGint -> VGenum -> VGfloat -> VGfloat -> VGint -> VGint
                 -> VGbitfield
                 -> IO VGPath

foreign import ccall unsafe "vg/openvg.h vgClearPath"
    vgClearPath :: VGPath -> VGbitfield -> IO ()

foreign import ccall unsafe "vg/openvg.h vgDestroyPath"
    vgDestroyPath :: VGPath -> IO ()

foreign import ccall unsafe "vg/openvg.h vgRemovePathCapabilities"
    vgRemovePathCapabilities :: VGPath -> VGbitfield -> IO ()

foreign import ccall unsafe "vg/openvg.h vgGetPathCapabilities"
    vgGetPathCapabilities :: VGPath -> IO VGbitfield

foreign import ccall unsafe "vg/openvg.h vgAppendPath"
    vgAppendPath :: VGPath -> VGPath -> IO ()

foreign import ccall unsafe "vg/openvg.h vgAppendPathData"
    vgAppendPathData :: VGPath -> VGint -> Ptr VGubyte -> Ptr a -> IO ()

foreign import ccall unsafe "vg/openvg.h vgModifyPathCoords"
    vgModifyPathCoords :: VGPath -> VGint -> VGint -> Ptr a -> IO ()

foreign import ccall unsafe "vg/openvg.h vgTransformPath"
    vgTransformPath :: VGPath -> VGPath -> IO ()

foreign import ccall unsafe "vg/openvg.h vgInterpolatePath"
    vgInterpolatePath :: VGPath -> VGPath -> VGPath -> VGfloat -> IO VGboolean

-- Stubbed in shivaVG 0.2.1 (not implemented)
foreign import ccall unsafe "vg/openvg.h vgPathLength"
    vgPathLength :: VGPath -> VGint -> VGint -> IO VGfloat



-- Stubbed in shivaVG 0.2.1 (not implemented)
foreign import ccall unsafe "vg/openvg.h vgPointAlongPath"
    vgPointAlongPath :: VGPath -> VGint -> VGint -> VGfloat
                     -> Ptr VGfloat -> Ptr VGfloat
                     -> Ptr VGfloat -> Ptr VGfloat
                     -> IO ()


foreign import ccall unsafe "vg/openvg.h vgPathBounds"
    vgPathBounds :: VGPath 
                 -> Ptr VGfloat -> Ptr VGfloat 
                 -> Ptr VGfloat -> Ptr VGfloat
                 -> IO ()


foreign import ccall unsafe "vg/openvg.h vgPathTransformedBounds"
    vgPathTransformedBounds :: VGPath
                            -> Ptr VGfloat -> Ptr VGfloat
                            -> Ptr VGfloat -> Ptr VGfloat
                            -> IO ()

foreign import ccall unsafe "vg/openvg.h vgDrawPath"
    vgDrawPath :: VGPath -> VGbitfield -> IO ()


--------------------------------------------------------------------------------
-- Section 9 - Paint

foreign import ccall unsafe "vg/openvg.h vgCreatePaint"
    vgCreatePaint :: IO VGPaint

foreign import ccall unsafe "vg/openvg.h vgDestroyPaint"
    vgDestroyPaint :: VGPaint -> IO ()

foreign import ccall unsafe "vg/openvg.h vgSetPaint"
    vgSetPaint :: VGPaint -> VGbitfield -> IO ()


-- vgGetPaint - NOT IMPLEMENTED BY SHIVA-VG
-- foreign import ccall unsafe "vg/openvg.h vgGetPaint"
--     vgGetPaint :: VGenum -> IO VGPaint



-- vgSetColor - NOT IMPLEMENTED BY SHIVA-VG
-- foreign import ccall unsafe "vg/openvg.h vgSetColor"
--     vgSetColor :: VGPaint -> VGuint -> IO ()



-- vgGetColor - NOT IMPLEMENTED BY SHIVA-VG
-- foreign import ccall unsafe "vg/openvg.h vgGetColor"
--     vgGetColor :: VGPaint -> IO VGuint


foreign import ccall unsafe "vg/openvg.h vgPaintPattern"
    vgPaintPattern :: VGPaint -> VGImage -> IO ()

--------------------------------------------------------------------------------
-- Section 10 - Images


foreign import ccall unsafe "vg/openvg.h vgCreateImage"
    vgCreateImage :: VGenum -> VGint -> VGint -> VGbitfield -> IO VGImage

foreign import ccall unsafe "vg/openvg.h vgDestroyImage"
    vgDestroyImage :: VGImage -> IO ()

foreign import ccall unsafe "vg/openvg.h vgClearImage"
    vgClearImage :: VGImage -> VGint -> VGint -> VGint -> VGint -> IO ()

foreign import ccall unsafe "vg/openvg.h vgImageSubData"
    vgImageSubData :: VGImage -> Ptr a -> VGint -> VGenum
                   -> VGint -> VGint -> VGint -> VGint
                   -> IO ()


foreign import ccall unsafe "vg/openvg.h vgGetImageSubData"
    vgGetImageSubData :: VGImage -> Ptr a -> VGint -> VGenum
                      -> VGint -> VGint -> VGint -> VGint
                      -> IO ()

-- Stubbed in shivaVG 0.2.1 (not implemented)
foreign import ccall unsafe "vg/openvg.h vgChildImage"
    vgChildImage :: VGImage -> VGint -> VGint -> VGint -> VGint -> IO VGImage


-- Stubbed in shivaVG 0.2.1 (not implemented)
foreign import ccall unsafe "vg/openvg.h vgGetParent"
    vgGetParent :: VGImage -> IO VGImage



foreign import ccall unsafe "vg/openvg.h vgCopyImage"
    vgCopyImage :: VGImage -> VGint -> VGint
                -> VGImage -> VGint -> VGint
                -> VGint   -> VGint
                -> VGboolean
                -> IO ()


foreign import ccall unsafe "vg/openvg.h vgDrawImage"
    vgDrawImage :: VGImage -> IO ()

foreign import ccall unsafe "vg/openvg.h vgSetPixels"
    vgSetPixels :: VGint -> VGint -> VGImage
                -> VGint -> VGint -> VGint -> VGint
                -> IO ()

foreign import ccall unsafe "vg/openvg.h vgWritePixels"
    vgWritePixels :: Ptr a -> VGint -> VGenum
                  -> VGint -> VGint -> VGint -> VGint
                  -> IO ()


foreign import ccall unsafe "vg/openvg.h vgGetPixels"
    vgGetPixels :: VGImage -> VGint -> VGint
                -> VGint -> VGint -> VGint -> VGint
                -> IO ()

foreign import ccall unsafe "vg/openvg.h vgReadPixels"
    vgReadPixels :: Ptr a -> VGint -> VGenum 
                 -> VGint -> VGint -> VGint -> VGint
                 -> IO ()


foreign import ccall unsafe "vg/openvg.h vgCopyPixels"
    vgCopyPixels :: VGint -> VGint
                 -> VGint -> VGint -> VGint -> VGint
                 -> IO ()

--------------------------------------------------------------------------------
-- Section 11 - Image Filters


-- Stubbed in shivaVG 0.2.1 (not implemented)
foreign import ccall unsafe "vg/openvg.h vgColorMatrix"
    vgColorMatrix :: VGImage -> VGImage -> Ptr VGfloat -> IO ()


-- Stubbed in shivaVG 0.2.1 (not implemented)
foreign import ccall unsafe "vg/openvg.h vgConvolve"
    vgConvolve :: VGImage -> VGImage
               -> VGint -> VGint -> VGint -> VGint
               -> Ptr VGshort 
               -> VGfloat -> VGfloat
               -> VGenum
               -> IO ()

-- Stubbed in shivaVG 0.2.1 (not implemented)
foreign import ccall unsafe "vg/openvg.h vgSeparableConvolve"
    vgSeparableConvolve :: VGImage -> VGImage
                        -> VGint -> VGint -> VGint -> VGint
                        -> Ptr VGshort -> Ptr VGshort
                        -> VGfloat -> VGfloat
                        -> VGenum
                        -> IO ()


-- Stubbed in shivaVG 0.2.1 (not implemented)
foreign import ccall unsafe "vg/openvg.h vgGaussianBlur"
    vgGaussianBlur :: VGImage -> VGImage -> VGfloat
                   -> VGfloat -> VGenum
                   -> IO ()


-- Stubbed in shivaVG 0.2.1 (not implemented)
foreign import ccall unsafe "vg/openvg.h vgLookup"
    vgLookup :: VGImage -> VGImage 
             -> Ptr VGubyte -> Ptr VGubyte 
             -> Ptr VGubyte -> Ptr VGubyte
             -> VGboolean -> VGboolean
             -> IO ()


-- Stubbed in shivaVG 0.2.1 (not implemented)
foreign import ccall unsafe "vg/openvg.h vgLookupSingle"
    vgLookupSingle :: VGImage -> VGImage
                   -> Ptr VGuint
                   -> VGenum
                   -> VGboolean -> VGboolean
                   -> IO ()

--------------------------------------------------------------------------------
-- Section 13 - Querying Hardware

foreign import ccall unsafe "vg/openvg.h vgHardwareQuery"
    vgHardwareQuery :: VGenum -> VGint -> IO VGenum

foreign import ccall unsafe "vg/openvg.h vgGetString"
    vgGetString :: VGenum -> IO CString

--------------------------------------------------------------------------------
-- Shiva-VG Extensions - see the README in the shiva archive.

foreign import ccall unsafe "vg/openvg.h vgCreateContextSH"
    vgCreateContextSH :: VGint -> VGint -> IO VGboolean

foreign import ccall unsafe "vg/openvg.h vgResizeSurfaceSH"
    vgResizeSurfaceSH :: VGint -> VGint -> IO ()


foreign import ccall unsafe "vg/openvg.h vgDestroyContextSH"
    vgDestroyContextSH :: IO ()









