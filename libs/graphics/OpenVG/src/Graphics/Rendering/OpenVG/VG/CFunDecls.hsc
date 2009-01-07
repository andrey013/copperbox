{-# LANGUAGE CPP                        #-}
{-# LANGUAGE ForeignFunctionInterface   #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenVG.VG.CFunDecls
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Internal module declaring all the foreign declarations of the functions
-- defined in <vg/openvg.h>.
--
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenVG.VG.CFunDecls where

#include <vg/openvg.h>

import Graphics.Rendering.OpenVG.VG.BasicTypes

import Foreign.Ptr ( Ptr )
import Foreign.C.String ( CString )

-- 'suffix indicates a marshalled enum type.
type VGHardwareQueryResult' = VGenum
type VGHardwareQueryType'   = VGenum
type VGImageChannel'        = VGenum
type VGImageFormat'         = VGenum
type VGMaskOperation'       = VGenum
type VGPaintMode'           = VGenum
type VGPathDatatype'        = VGenum
type VGStringID'            = VGenum
type VGTilingMode'          = VGenum

foreign import ccall unsafe "vg/openvg.h vgFlush" 
    vgFlush :: IO ()
    
foreign import ccall unsafe "vg/openvg.h vgFinish" 
    vgFinish ::  IO ()
    
        
-- getters and setters
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

    
-- | Matrix Manipulation
-- set the current matrix to the identity matrix
foreign import ccall unsafe "vg/openvg.h vgLoadIdentity"  
    vgLoadIdentity :: IO ()
    
foreign import ccall unsafe "vg/openvg.h vgLoadMatrix"  
    vgLoadMatrix :: Ptr VGfloat -> IO ()

foreign import ccall unsafe "vg/openvg.h vgGetMatrix"  
    vgGetMatrix :: IO (Ptr VGfloat)

-- multiple the current matrix by the given matrix    
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

-- | Masking and Clearing
{-
-- TODO vgMask seems to be missing in the dll... ?
foreign import ccall unsafe "vg/openvg.h vgMask"  
    vgMask :: VGImage -> VGMaskOperation' -> 
                  VGint -> VGint -> VGint -> VGint -> IO ()
-}
    
foreign import ccall unsafe "vg/openvg.h vgClear"  
    vgClear :: VGint -> VGint -> VGint -> VGint -> IO ()



-- | Paths
foreign import ccall unsafe "vg/openvg.h vgCreatePath"  
    vgCreatePath :: VGint 
                 -> VGPathDatatype' 
                 -> VGfloat 
                 -> VGfloat
                 -> VGint 
                 -> VGint 
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

-- append the data in /Ptr a/ to the path handle /VGPath/.    
foreign import ccall unsafe "vg/openvg.h vgAppendPathData"  
    vgAppendPathData :: VGPath -> VGint -> Ptr VGubyte -> Ptr a -> IO ()


foreign import ccall unsafe "vg/openvg.h vgModifyPathCoords"  
    vgModifyPathCoords :: VGPath -> VGint -> VGint -> Ptr a -> IO ()
    

foreign import ccall unsafe "vg/openvg.h vgTransformPath"  
    vgTransformPath :: VGPath -> VGPath -> IO ()
    
    
foreign import ccall unsafe "vg/openvg.h vgInterpolatePath"  
    vgInterpolatePath :: VGPath 
                      -> VGPath 
                      -> VGPath 
                      -> VGfloat 
                      -> IO VGboolean
{-
-- TODO vgPathLength seems to be missing in the dll... ?
foreign import ccall unsafe "vg/openvg.h vgPathLength"  
    vgPathLength :: VGPath -> VGint -> VGint -> IO VGfloat   
-}

{-
-- TODO vgPointAlongPath seems to be missing in the dll... ?                                
foreign import ccall unsafe "vg/openvg.h vgPointAlongPath"  
    vgPointAlongPath :: VGPath 
                     -> VGint 
                     -> VGint 
                     -> VGfloat
                     -> Ptr VGfloat
                     -> Ptr VGfloat
                     -> Ptr VGfloat
                     -> Ptr VGfloat
                     -> IO ()  
-}

foreign import ccall unsafe "vg/openvg.h vgPathBounds"  
    vgPathBounds :: VGPath 
                 -> Ptr VGfloat 
                 -> Ptr VGfloat 
                 -> Ptr VGfloat
                 -> Ptr VGfloat
                 -> IO () 
                                                   

foreign import ccall unsafe "vg/openvg.h vgPathTransformedBounds"  
    vgPathTransformedBounds :: VGPath 
                            -> Ptr VGfloat 
                            -> Ptr VGfloat 
                            -> Ptr VGfloat
                            -> Ptr VGfloat
                            -> IO () 

foreign import ccall unsafe "vg/openvg.h vgDrawPath"  
    vgDrawPath :: VGPath -> VGbitfield -> IO ()



-- | Paint 
foreign import ccall unsafe "vg/openvg.h vgCreatePaint"  
    vgCreatePaint :: IO VGPaint    

foreign import ccall unsafe "vg/openvg.h vgDestroyPaint"  
    vgDestroyPaint :: VGPaint -> IO ()
    
foreign import ccall unsafe "vg/openvg.h vgSetPaint"  
    vgSetPaint :: VGPaint -> VGbitfield -> IO ()

{-
-- TODO vgGetPaint seems to be missing in the dll... ?
foreign import ccall unsafe "vg/openvg.h vgGetPaint"  
    vgGetPaint :: VGPaintMode' -> IO VGPaint
-}

{-
-- TODO vgSetColor seems to be missing in the dll... ?
foreign import ccall unsafe "vg/openvg.h vgSetColor"  
    vgSetColor :: VGPaint -> VGuint -> IO ()
-}

{-
-- TODO vgGetColor seems to be missing in the dll... ?
foreign import ccall unsafe "vg/openvg.h vgGetColor"  
    vgGetColor :: VGPaint -> IO VGuint
-}

foreign import ccall unsafe "vg/openvg.h vgPaintPattern"  
    vgPaintPattern :: VGPaint -> VGImage -> IO ()


-- | Images

foreign import ccall unsafe "vg/openvg.h vgCreateImage"  
    vgCreateImage :: VGImageFormat' 
                  -> VGint
                  -> VGint
                  -> VGbitfield
                  -> IO VGImage
                                  
                                  
foreign import ccall unsafe "vg/openvg.h vgDestroyImage"  
    vgDestroyImage :: VGImage -> IO () 
                  
foreign import ccall unsafe "vg/openvg.h vgClearImage"  
    vgClearImage :: VGImage -> VGint -> VGint -> VGint -> VGint -> IO () 
                              
foreign import ccall unsafe "vg/openvg.h vgImageSubData"  
    vgImageSubData :: VGImage 
                   -> Ptr a
                   -> VGint
                   -> VGImageFormat' 
                   -> VGint 
                   -> VGint 
                   -> VGint
                   -> VGint 
                   -> IO ()
                                    

foreign import ccall unsafe "vg/openvg.h vgGetImageSubData"  
    vgGetImageSubData :: VGImage 
                      -> Ptr a
                      -> VGint
                      -> VGImageFormat' 
                      -> VGint 
                      -> VGint 
                      -> VGint
                      -> VGint 
                      -> IO ()

{-
-- TODO vgChildImage seems to be missing in the dll... ?
foreign import ccall unsafe "vg/openvg.h vgChildImage"  
    vgChildImage :: VGImage -> VGint -> VGint -> VGint -> VGint -> IO VGImage
-}

{-
-- TODO vgGetParent seems to be missing in the dll... ?                        
foreign import ccall unsafe "vg/openvg.h vgGetParent"  
    vgGetParent :: VGImage -> IO VGImage
-}


foreign import ccall unsafe "vg/openvg.h vgCopyImage"  
    vgCopyImage :: VGImage -> VGint -> VGint 
                -> VGImage -> VGint -> VGint 
                -> VGint   -> VGint 
                -> VGboolean
                -> IO ()
    
                             
foreign import ccall unsafe "vg/openvg.h vgDrawImage"  
    vgDrawImage :: VGImage -> IO ()

foreign import ccall unsafe "vg/openvg.h vgSetPixels"  
    vgSetPixels :: VGint 
                -> VGint 
                -> VGImage 
                -> VGint 
                -> VGint 
                -> VGint 
                -> VGint 
                -> IO ()

foreign import ccall unsafe "vg/openvg.h vgWritePixels"  
    vgWritePixels :: Ptr a
                  -> VGint 
                  -> VGImageFormat'  
                  -> VGint 
                  -> VGint 
                  -> VGint 
                  -> VGint 
                  -> IO ()
                

foreign import ccall unsafe "vg/openvg.h vgGetPixels"  
    vgGetPixels :: VGImage
                -> VGint 
                -> VGint  
                -> VGint 
                -> VGint 
                -> VGint 
                -> VGint 
                -> IO ()
                  
foreign import ccall unsafe "vg/openvg.h vgReadPixels"  
    vgReadPixels :: Ptr a 
                 -> VGint
                 -> VGImageFormat' 
                 -> VGint  
                 -> VGint 
                 -> VGint 
                 -> VGint 
                 -> IO ()
                

foreign import ccall unsafe "vg/openvg.h vgCopyPixels"  
    vgCopyPixels :: VGint
                 -> VGint
                 -> VGint  
                 -> VGint 
                 -> VGint 
                 -> VGint 
                 -> IO ()
                              

-- | Image Filters

{-
-- TODO vgColorMatrix seems to be missing in the dll... ? 
foreign import ccall unsafe "vg/openvg.h vgColorMatrix"  
    vgColorMatrix :: VGImage -> VGImage -> Ptr VGfloat -> IO ()
-}     

{-
-- TODO vgConvolve seems to be missing in the dll... ? 
foreign import ccall unsafe "vg/openvg.h vgConvolve"  
    vgConvolve :: VGImage 
               -> VGImage
               -> VGint
               -> VGint
               -> VGint
               -> VGint 
               -> Ptr VGshort
               -> VGfloat
               -> VGfloat
               -> VGTilingMode'
               -> IO ()                            
-}

{-
-- TODO vgSeparableConvolve seems to be missing in the dll... ? 
foreign import ccall unsafe "vg/openvg.h vgSeparableConvolve"  
    vgSeparableConvolve :: VGImage 
                        -> VGImage
                        -> VGint
                        -> VGint
                        -> VGint
                        -> VGint 
                        -> Ptr VGshort
                        -> Ptr VGshort
                        -> VGfloat
                        -> VGfloat
                        -> VGTilingMode'
                        -> IO () 
-}

{-
-- TODO vgGaussianBlur seems to be missing in the dll... ?            
foreign import ccall unsafe "vg/openvg.h vgGaussianBlur"  
    vgGaussianBlur :: VGImage 
                   -> VGImage
                   -> VGfloat
                   -> VGfloat
                   -> VGTilingMode'
                   -> IO () 
-}

{-
-- TODO vgLookup seems to be missing in the dll... ? 
foreign import ccall unsafe "vg/openvg.h vgLookup"  
    vgLookup :: VGImage 
             -> VGImage
             -> Ptr VGubyte
             -> Ptr VGubyte
             -> Ptr VGubyte
             -> Ptr VGubyte
             -> VGboolean
             -> VGboolean
             -> IO () 
-}

{-
-- TODO vgLookupSingle seems to be missing in the dll... ?
foreign import ccall unsafe "vg/openvg.h vgLookupSingle"  
    vgLookupSingle :: VGImage 
                   -> VGImage
                   -> Ptr VGuint
                   -> VGImageChannel'
                   -> VGboolean
                   -> VGboolean
                   -> IO () 
-}


-- | Hardware Queries
{-
-- TODO vgHardwareQuery seems to be missing in the dll... ?
foreign import ccall unsafe "vg/openvg.h vgHardwareQuery"  
    vgHardwareQuery :: VGHardwareQueryType' 
                    -> VGint 
                    -> IO VGHardwareQueryResult'
-}

-- | Renderer and Extension Information
foreign import ccall unsafe "vg/openvg.h vgGetString"  
    vgGetString :: VGStringID' -> IO CString 

-- | Shiva-VG Extensions
-- See the README in the shiva archive.
foreign import ccall unsafe "vg/openvg.h vgCreateContextSH"  
    vgCreateContextSH :: VGint -> VGint -> IO VGboolean
    
foreign import ccall unsafe "vg/openvg.h vgResizeSurfaceSH"  
    vgResizeSurfaceSH :: VGint -> VGint -> IO ()
    

foreign import ccall unsafe "vg/openvg.h vgDestroyContextSH"  
    vgDestroyContextSH :: IO ()
    








