{-# LANGUAGE CPP                        #-}
{-# LANGUAGE ForeignFunctionInterface   #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.FreeType.Internals.COutline
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Internal module corresponding to FT_OUTLINE_H.
--
--
--------------------------------------------------------------------------------

module Graphics.Rendering.FreeType.Internals.COutline where

#include <ft2build.h>
#include FT_FREETYPE_H
#include FT_IMAGE_H
#include FT_OUTLINE_H


import Graphics.Rendering.FreeType.Internals.CBasicDataTypes
import Graphics.Rendering.FreeType.Internals.CImage

import Foreign.C.String ( CString )
import Foreign.C.Types ( CInt )
import Foreign.Ptr ( Ptr, FunPtr ) 

--------------------------------------------------------------------------------

-- | @FToutline@ corresponds to the FreeType type @FT_Outline@.

data FToutline = FToutline {
      _n_contours     :: FTshort,
      _n_points       :: FTshort,
      _points         :: Ptr FTvector,
      _tags           :: CString,
      _contours       :: FTshort,
      _outline_flags  :: FTint
    }


foreign import ccall unsafe "freetype/freetype.h FT_Outline_New" 
    ft_Outline_New :: FTlibrary 
                   -> FTuint 
                   -> FTint 
                   -> Ptr FToutline 
                   -> IO FTerror

foreign import ccall unsafe "freetype/freetype.h FT_Outline_Done" 
    ft_Outline_Done :: FTlibrary 
                    -> Ptr FToutline 
                    -> IO FTerror


foreign import ccall unsafe "freetype/freetype.h FT_Outline_Copy" 
    ft_Outline_Copy :: Ptr FToutline 
                    -> Ptr FToutline 
                    -> IO FTerror


foreign import ccall unsafe "freetype/freetype.h FT_Outline_Translate" 
    ft_Outline_Translate :: Ptr FToutline 
                         -> FTpos
                         -> FTpos
                         -> IO ()
                         

foreign import ccall unsafe "freetype/freetype.h FT_Outline_Transform" 
    ft_Outline_Transform :: Ptr FToutline 
                         -> Ptr FTmatrix 
                         -> IO ()


foreign import ccall unsafe "freetype/freetype.h FT_Outline_Embolden" 
    ft_Outline_Embolden :: Ptr FToutline -> FTpos -> IO FTerror


foreign import ccall unsafe "freetype/freetype.h FT_Outline_Reverse" 
   ft_Outline_Reverse :: Ptr FToutline -> IO ()


foreign import ccall unsafe "freetype/freetype.h FT_Outline_Check" 
   ft_Outline_Check :: Ptr FToutline -> IO FTerror


foreign import ccall unsafe "freetype/freetype.h FT_Outline_Get_BBox" 
   ft_Outline_Get_BBox :: Ptr FToutline -> Ptr FTbbox -> IO FTerror
   


type FToutlineflags_    = CInt

#{enum FToutlineflags_ ,
  , ft_OUTLINE_NONE             = FT_OUTLINE_NONE
  , ft_OUTLINE_OWNER            = FT_OUTLINE_OWNER
  , ft_OUTLINE_EVEN_ODD_FILL    = FT_OUTLINE_EVEN_ODD_FILL
  , ft_OUTLINE_REVERSE_FILL     = FT_OUTLINE_REVERSE_FILL
  , ft_OUTLINE_IGNORE_DROPOUTS  = FT_OUTLINE_IGNORE_DROPOUTS
  , ft_OUTLINE_SMART_DROPOUTS   = FT_OUTLINE_SMART_DROPOUTS
  , ft_OUTLINE_INCLUDE_STUBS    = FT_OUTLINE_INCLUDE_STUBS

  , ft_OUTLINE_HIGH_PRECISION   = FT_OUTLINE_HIGH_PRECISION
  , ft_OUTLINE_SINGLE_PASS      = FT_OUTLINE_SINGLE_PASS 
  }


type FT_Outline_MoveToFunc = Ptr FTvector -> VoidPtr -> IO FTint

foreign import ccall "wrapper"
    mkOutline_MoveToFunc :: FT_Outline_MoveToFunc 
                         -> IO (FTcallback FT_Outline_MoveToFunc)
   

type FT_Outline_LineToFunc = Ptr FTvector -> VoidPtr -> IO FTint

type FT_Outline_ConicToFunc =   Ptr FTvector 
                             -> Ptr FTvector 
                             -> VoidPtr 
                             -> IO FTint

type FT_Outline_CubicToFunc =   Ptr FTvector 
                             -> Ptr FTvector
                             -> Ptr FTvector 
                             -> VoidPtr 
                             -> IO FTint
                             
data FToutlinefuncs = FToutlinefuncs {
      _move_to   :: FunPtr FT_Outline_MoveToFunc,
      _line_to   :: FunPtr FT_Outline_LineToFunc,
      _conic_to  :: FunPtr FT_Outline_ConicToFunc,
      _cubic_to  :: FunPtr FT_Outline_CubicToFunc,
      _shift     :: FTint,
      _delta     :: FTpos
    }


foreign import ccall unsafe "freetype/freetype.h FT_Outline_Decompose" 
   ft_Outline_Decompose :: Ptr FToutline 
                        -> Ptr FToutlinefuncs 
                        -> VoidPtr 
                        -> IO FTerror
   


foreign import ccall unsafe "freetype/freetype.h FT_Outline_Get_CBox" 
   ft_Outline_Get_CBox :: Ptr FToutline -> Ptr FTbbox -> IO ()
   

foreign import ccall unsafe "freetype/freetype.h FT_Outline_Get_Bitmap" 
   ft_Outline_Get_Bitmap :: FTlibrary 
                         -> Ptr FToutline 
                         -> Ptr FTbitmap
                         -> IO FTerror




foreign import ccall unsafe "freetype/freetype.h FT_Outline_Render" 
   ft_Outline_Render :: FTlibrary 
                     -> Ptr FToutline 
                     -> Ptr FTrasterparams
                     -> IO FTerror




type FTorientation_    = CInt



#{enum FTorientation_ ,
  , ft_ORIENTATION_TRUETYPE     = FT_ORIENTATION_TRUETYPE
  , ft_ORIENTATION_POSTSCRIPT   = FT_ORIENTATION_POSTSCRIPT
  , ft_ORIENTATION_FILL_RIGHT   = FT_ORIENTATION_FILL_RIGHT
  , ft_ORIENTATION_FILL_LEFT    = FT_ORIENTATION_FILL_LEFT
  , ft_ORIENTATION_NONE         = FT_ORIENTATION_NONE
  }


foreign import ccall unsafe "freetype/freetype.h FT_Outline_Get_Orientation" 
   ft_Outline_Get_Orientation :: Ptr FToutline 
                              -> IO FTorientation_

-- end of file
