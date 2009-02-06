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
import Graphics.Rendering.FreeType.Utils ( Marshal(..), Unmarshal(..) )

import Foreign.C.Types (  CInt, CShort, CChar )

import Foreign.Ptr ( Ptr, FunPtr ) 
import Foreign.Storable

--------------------------------------------------------------------------------


-- FT_Outline is a struct not a handle, it is quite reasonable for
-- a C program to declare an FT_Outline (from ftgrid.c in the FreeType
-- examples):
--
-- @FT_Outline  transformed;@
--
-- Then pass it to FT_Outline_New...
--
-- FT_Outline_New( handle->library,
--                 outline->n_points,
--                 outline->n_contours,
--                 &transformed );
-- 
-- Before disposing of it...
--
-- FT_Outline_Done( handle->library, &transformed );
--  

data FT_struct_outline = FT_struct_outline {
      _n_contours     :: CShort,
      _n_points       :: CShort,
      _points         :: Ptr FT_struct_vector,
      _tags           :: Ptr CChar,
      _contours       :: Ptr CShort,
      _outline_flags  :: CInt
    }

type FT_outline = FT_struct_outline

instance Storable FT_struct_outline where
  sizeOf    _ = #{size FT_Outline}
  alignment _ = alignment (undefined :: CShort)
  
  peek ptr = do 
      a <- #{peek FT_Outline, n_contours} ptr
      b <- #{peek FT_Outline, n_points}   ptr
      c <- #{peek FT_Outline, points}     ptr
      d <- #{peek FT_Outline, tags}       ptr
      e <- #{peek FT_Outline, contours}   ptr
      f <- #{peek FT_Outline, flags}      ptr
      return $ FT_struct_outline a b c d e f
  
  poke ptr (FT_struct_outline a b c d e f) = do
        #{poke FT_Outline, n_contours}  ptr a
        #{poke FT_Outline, n_points}    ptr b
        #{poke FT_Outline, points}      ptr c
        #{poke FT_Outline, tags}        ptr d
        #{poke FT_Outline, contours}    ptr e
        #{poke FT_Outline, flags}       ptr f

type FT_outline_ptr = Ptr FT_struct_outline


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



--------------------------------------------------------------------------------
type FT_outline_moveto_func   = Ptr FT_struct_vector 
                              -> VoidPtr 
                              -> IO FT_int
   

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

data Orientation = 
      TrueType
    | PostScript
    | FillRight
    | FillLeft
    | ONone
    deriving ( Eq, Ord, Show )
    
instance Marshal Orientation where
  marshal x = case x of
      TrueType    -> ft_ORIENTATION_TRUETYPE
      PostScript  -> ft_ORIENTATION_POSTSCRIPT
      FillRight   -> ft_ORIENTATION_FILL_RIGHT
      FillLeft    -> ft_ORIENTATION_FILL_LEFT
      ONone       -> ft_ORIENTATION_NONE

      
      
instance Unmarshal Orientation where
  unmarshal x
      | x == ft_ORIENTATION_TRUETYPE    = TrueType 
      | x == ft_ORIENTATION_POSTSCRIPT  = PostScript 
      | x == ft_ORIENTATION_FILL_RIGHT  = FillRight 
      | x == ft_ORIENTATION_FILL_LEFT   = FillLeft 
      | x == ft_ORIENTATION_NONE        = ONone 
      | otherwise = error ("unmarshal: PixelMode - illegal value " ++ show x)                  
      
      
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
