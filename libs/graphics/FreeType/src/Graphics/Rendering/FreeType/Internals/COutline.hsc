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

import Graphics.Rendering.FreeType.Internals.CBaseTypes
import Graphics.Rendering.FreeType.Internals.CBasicDataTypes
import Graphics.Rendering.FreeType.Internals.CImage

import Foreign.C.Types (  CInt, CShort )
import Foreign.Ptr ( Ptr, FunPtr ) 
import Foreign.Storable

--------------------------------------------------------------------------------


type FT_outline_moveto_func   = Ptr FT_struct_vector 
                              -> VoidPtr 
                              -> IO FT_int

foreign import ccall "wrapper"
    mk_outline_moveto_func   :: FT_outline_moveto_func 
                            -> IO (FT_callback FT_outline_moveto_func)
   

type FT_outline_lineto_func   = Ptr FT_struct_vector 
                              -> VoidPtr 
                              -> IO FT_int

type FT_outline_conicto_func  = Ptr FT_struct_vector 
                              -> Ptr FT_struct_vector 
                              -> VoidPtr 
                              -> IO FT_int

type FT_outline_cubicto_func  = Ptr FT_struct_vector 
                              -> Ptr FT_struct_vector
                              -> Ptr FT_struct_vector 
                              -> VoidPtr 
                              -> IO FT_int


                             
data FT_struct_outlinefuncs = FT_struct_outlinefuncs {
      _move_to   :: FunPtr FT_outline_moveto_func,
      _line_to   :: FunPtr FT_outline_lineto_func,
      _conic_to  :: FunPtr FT_outline_conicto_func,
      _cubic_to  :: FunPtr FT_outline_cubicto_func,
      _shift     :: FT_int,
      _delta     :: FT_pos
    }

type FT_enum_orientation    = CInt



#{enum FT_enum_orientation ,
  , ft_ORIENTATION_TRUETYPE     = FT_ORIENTATION_TRUETYPE
  , ft_ORIENTATION_POSTSCRIPT   = FT_ORIENTATION_POSTSCRIPT
  , ft_ORIENTATION_FILL_RIGHT   = FT_ORIENTATION_FILL_RIGHT
  , ft_ORIENTATION_FILL_LEFT    = FT_ORIENTATION_FILL_LEFT
  , ft_ORIENTATION_NONE         = FT_ORIENTATION_NONE
  }
  
--------------------------------------------------------------------------------
-- wrappers

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
                          -> Ptr Bitmap
                          -> IO FT_error
                          

foreign import ccall unsafe "freetype/freetype.h FT_Outline_Render" 
   ft_outline_render    :: FT_library_ptr 
                        -> Ptr FT_struct_outline 
                        -> Ptr FT_struct_rasterparams
                        -> IO FT_error



foreign import ccall unsafe "freetype/freetype.h FT_Outline_Get_Orientation" 
   ft_outline_get_orientation :: Ptr FT_struct_outline 
                              -> IO FT_enum_orientation


peekOutline_n_contours :: FT_outline_ptr -> IO CShort
peekOutline_n_contours ptr = do 
      i <- #{peek FT_Outline, n_contours} ptr
      return i
      
      
-- end of file
