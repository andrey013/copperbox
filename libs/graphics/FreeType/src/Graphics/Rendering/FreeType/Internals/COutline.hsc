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

data FT_struct_outline = FT_struct_outline {
      _n_contours     :: FT_short,
      _n_points       :: FT_short,
      _points         :: Ptr FT_struct_vector,
      _tags           :: CString,
      _contours       :: FT_short,
      _outline_flags  :: FT_int
    }


foreign import ccall unsafe "freetype/freetype.h FT_Outline_New" 
    ft_outline_new :: FT_library_ptr 
                   -> FT_uint 
                   -> FT_int 
                   -> Ptr FT_struct_outline 
                   -> IO FT_error

foreign import ccall unsafe "freetype/freetype.h FT_Outline_Done" 
    ft_outline_done :: FT_library_ptr 
                    -> Ptr FT_struct_outline 
                    -> IO FT_error


foreign import ccall unsafe "freetype/freetype.h FT_Outline_Copy" 
    ft_outline_copy :: Ptr FT_struct_outline 
                    -> Ptr FT_struct_outline 
                    -> IO FT_error


foreign import ccall unsafe "freetype/freetype.h FT_Outline_Translate" 
    ft_outline_translate :: Ptr FT_struct_outline 
                         -> FT_pos
                         -> FT_pos
                         -> IO ()
                         

foreign import ccall unsafe "freetype/freetype.h FT_Outline_Transform" 
    ft_outline_transform :: Ptr FT_struct_outline 
                         -> Ptr FT_struct_matrix 
                         -> IO ()


foreign import ccall unsafe "freetype/freetype.h FT_Outline_Embolden" 
    ft_outline_embolden   :: Ptr FT_struct_outline 
                          -> FT_pos 
                          -> IO FT_error


foreign import ccall unsafe "freetype/freetype.h FT_Outline_Reverse" 
   ft_outline_reverse     :: Ptr FT_struct_outline 
                          -> IO ()


foreign import ccall unsafe "freetype/freetype.h FT_Outline_Check" 
   ft_outline_check       :: Ptr FT_struct_outline 
                          -> IO FT_error


foreign import ccall unsafe "freetype/freetype.h FT_Outline_Get_BBox" 
   ft_outline_get_bbox    :: Ptr FT_struct_outline 
                          -> Ptr FT_struct_bbox 
                          -> IO FT_error
   


type FT_enum_outlineflags    = CInt

#{enum FT_enum_outlineflags ,
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


type FT_Outline_MoveToFunc = Ptr FT_struct_vector -> VoidPtr -> IO FT_int

foreign import ccall "wrapper"
    mkOutline_MoveToFunc :: FT_Outline_MoveToFunc 
                         -> IO (FT_callback FT_Outline_MoveToFunc)
   

type FT_Outline_LineToFunc = Ptr FT_struct_vector -> VoidPtr -> IO FT_int

type FT_Outline_ConicToFunc =   Ptr FT_struct_vector 
                             -> Ptr FT_struct_vector 
                             -> VoidPtr 
                             -> IO FT_int

type FT_Outline_CubicToFunc =   Ptr FT_struct_vector 
                             -> Ptr FT_struct_vector
                             -> Ptr FT_struct_vector 
                             -> VoidPtr 
                             -> IO FT_int
                             
data FT_struct_outlinefuncs = FT_struct_outlinefuncs {
      _move_to   :: FunPtr FT_Outline_MoveToFunc,
      _line_to   :: FunPtr FT_Outline_LineToFunc,
      _conic_to  :: FunPtr FT_Outline_ConicToFunc,
      _cubic_to  :: FunPtr FT_Outline_CubicToFunc,
      _shift     :: FT_int,
      _delta     :: FT_pos
    }


foreign import ccall unsafe "freetype/freetype.h FT_Outline_Decompose" 
   ft_outline_decompose :: Ptr FT_struct_outline 
                        -> Ptr FT_struct_outlinefuncs 
                        -> VoidPtr 
                        -> IO FT_error
   


foreign import ccall unsafe "freetype/freetype.h FT_Outline_Get_CBox" 
   ft_outline_get_cbox  :: Ptr FT_struct_outline 
                        -> Ptr FT_struct_bbox 
                        -> IO ()
   

foreign import ccall unsafe "freetype/freetype.h FT_Outline_Get_Bitmap" 
   ft_outline_get_bitmap  :: FT_library_ptr 
                          -> Ptr FT_struct_outline 
                          -> Ptr FT_struct_bitmap
                          -> IO FT_error




foreign import ccall unsafe "freetype/freetype.h FT_Outline_Render" 
   ft_outline_render    :: FT_library_ptr 
                        -> Ptr FT_struct_outline 
                        -> Ptr FT_struct_rasterparams
                        -> IO FT_error




type FT_enum_orientation    = CInt



#{enum FT_enum_orientation ,
  , ft_ORIENTATION_TRUETYPE     = FT_ORIENTATION_TRUETYPE
  , ft_ORIENTATION_POSTSCRIPT   = FT_ORIENTATION_POSTSCRIPT
  , ft_ORIENTATION_FILL_RIGHT   = FT_ORIENTATION_FILL_RIGHT
  , ft_ORIENTATION_FILL_LEFT    = FT_ORIENTATION_FILL_LEFT
  , ft_ORIENTATION_NONE         = FT_ORIENTATION_NONE
  }


foreign import ccall unsafe "freetype/freetype.h FT_Outline_Get_Orientation" 
   ft_outline_get_orientation :: Ptr FT_struct_outline 
                              -> IO FT_enum_orientation

-- end of file
