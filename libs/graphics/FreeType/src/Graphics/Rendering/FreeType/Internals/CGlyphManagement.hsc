{-# LANGUAGE CPP                        #-}
{-# LANGUAGE ForeignFunctionInterface   #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.FreeType.Internals.CGlyphManagement
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Internal module corresponding to FT_GLYPH_H.
--
--
--------------------------------------------------------------------------------

module Graphics.Rendering.FreeType.Internals.CGlyphManagement where

#include <ft2build.h>
#include FT_FREETYPE_H
#include FT_GLYPH_H 

import Graphics.Rendering.FreeType.Internals.CBaseTypes
import Graphics.Rendering.FreeType.Internals.CBasicDataTypes



-- import Foreign.C.Types
import Foreign.Ptr ( Ptr ) 
import Foreign.Storable ( Storable(..) )



--------------------------------------------------------------------------------
data FT_GLYPH_CLASS_



data FT_glyphrec = FT_glyphrec {
      _library  :: FT_library_ptr,
      _clazz    :: Ptr FT_GLYPH_CLASS_,
      _format   :: FT_enum_glyphformat,
      _advance  :: Vector
    }

instance Storable FT_glyphrec where
  sizeOf    _ = #{size FT_GlyphRec}
  alignment _ = alignment (undefined :: FT_library_ptr)
  
  peek ptr = do 
      l <- #{peek FT_GlyphRec, library} ptr
      c <- #{peek FT_GlyphRec, clazz}   ptr
      f <- #{peek FT_GlyphRec, format}  ptr
      a <- #{peek FT_GlyphRec, advance} ptr
      return $ FT_glyphrec l c f a
  
  poke ptr (FT_glyphrec l c f a) = do
        #{poke FT_GlyphRec, library} ptr l
        #{poke FT_GlyphRec, clazz}   ptr c
        #{poke FT_GlyphRec, format}  ptr f
        #{poke FT_GlyphRec, advance} ptr a

-- Glyph is exported from the library

newtype Glyph = Glyph (Ptr FT_glyphrec) deriving Storable

-- FT_glyph is used internally 
type FT_glyph_ptr = Ptr FT_glyph_slot_ptr

--------------------------------------------------------------------------------



foreign import ccall unsafe "freetype/freetype.h FT_Get_Glyph" 
    ft_get_glyph          :: FT_glyph_slot_ptr 
                          -> Ptr FT_glyph_ptr 
                          -> IO FT_error


foreign import ccall unsafe "freetype/freetype.h FT_Glyph_Copy" 
    ft_glyph_copy         :: FT_glyph_ptr
                          -> Ptr FT_glyph_ptr 
                          -> IO FT_error


foreign import ccall unsafe "freetype/freetype.h FT_Glyph_Get_CBox" 
    ft_glyph_get_cbox     :: FT_glyph_ptr 
                          -> FT_uint 
                          -> Ptr FT_struct_bbox
                          -> IO ()


  
  
  
-- end of file
