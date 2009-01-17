{-# LANGUAGE CPP                        #-}
{-# LANGUAGE ForeignFunctionInterface   #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.FreeType.Internals.CBaseFuns
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- FFI wrappers to FreeType'\s /BaseInterface/ functions. 
--
--------------------------------------------------------------------------------

module Graphics.Rendering.FreeType.Internals.CBaseFuns where

#include <ft2build.h>
#include FT_FREETYPE_H

import Graphics.Rendering.FreeType.Internals.CBaseTypes
import Graphics.Rendering.FreeType.Internals.CBasicDataTypes

import Foreign.C.String ( CString, peekCString )
import Foreign.Marshal.Array ( peekArray )
import Foreign.Ptr ( Ptr ) 
import Foreign.Storable ( peekByteOff )



--------------------------------------------------------------------------------
  
-- | @FT_Init_FreeType@.
foreign import ccall unsafe "freetype/freetype.h FT_Init_FreeType" 
    ft_init_freetype    :: Ptr FT_library_ptr 
                        -> IO FT_error

-- | @FT_Done_FreeType@.
foreign import ccall unsafe "freetype/freetype.h FT_Done_FreeType" 
    ft_done_freetype    :: FT_library_ptr 
                        -> IO FT_error


--------------------------------------------------------------------------------
  
      
foreign import ccall unsafe "freetype/freetype.h FT_New_Face" 
    ft_new_face         :: FT_library_ptr 
                        -> CString 
                        -> FT_long 
                        -> Ptr FT_face_ptr 
                        -> IO FT_error


foreign import ccall unsafe "freetype/freetype.h  FT_New_Memory_Face" 
    ft_new_memory_face  :: FT_library_ptr 
                        -> Ptr FT_byte
                        -> FT_long 
                        -> FT_long
                        -> Ptr FT_face_ptr 
                        -> IO FT_error


foreign import ccall unsafe "freetype/freetype.h FT_Open_Face" 
    ft_open_face        :: FT_library_ptr 
                        -> Ptr FT_struct_openargs
                        -> FT_long 
                        -> FT_face_ptr 
                        -> IO FT_error


foreign import ccall unsafe "freetype/freetype.h FT_Attach_File" 
    ft_attach_file      :: FT_face_ptr 
                        -> CString 
                        -> IO FT_error
     

foreign import ccall unsafe "freetype/freetype.h FT_Attach_Stream" 
    ft_attach_stream    :: FT_face_ptr
                        -> Ptr FT_struct_openargs 
                        -> IO FT_error


foreign import ccall unsafe "freetype/freetype.h FT_Done_Face" 
    ft_done_face        :: FT_face_ptr 
                        -> IO FT_error


foreign import ccall unsafe "freetype/freetype.h FT_Select_Size" 
    ft_select_size      :: FT_face_ptr 
                        -> FT_int 
                        -> IO FT_error


foreign import ccall unsafe "freetype/freetype.h FT_Request_Size" 
    ft_request_size     :: FT_face_ptr 
                        -> FT_sizerequest_ptr 
                        -> IO FT_error


foreign import ccall unsafe "freetype/freetype.h FT_Set_Char_Size" 
    ft_set_char_size    :: FT_face_ptr 
                        -> FT_f26dot6
                        -> FT_f26dot6
                        -> FT_uint
                        -> FT_uint
                        -> IO FT_error 


foreign import ccall unsafe "freetype/freetype.h FT_Set_Pixel_Sizes" 
    ft_set_pixel_sizes  :: FT_face_ptr 
                        -> FT_uint 
                        -> FT_uint 
                        -> IO FT_error 



foreign import ccall unsafe "freetype/freetype.h FT_Load_Glyph" 
    ft_load_glyph       :: FT_face_ptr 
                        -> FT_uint 
                        -> FT_int32 
                        -> IO FT_error 


foreign import ccall unsafe "freetype/freetype.h FT_Load_Char" 
    ft_load_char        :: FT_face_ptr 
                        -> FT_uint 
                        -> FT_int32 
                        -> IO FT_error 




foreign import ccall unsafe "freetype/freetype.h FT_Set_Transform" 
    ft_set_transform    :: FT_face_ptr 
                        -> Ptr FT_struct_matrix 
                        -> Ptr FT_struct_vector 
                        -> IO () 
    
    
foreign import ccall unsafe "freetype/freetype.h FT_Render_Glyph" 
    ft_render_glyph     :: FT_glyph_slot_ptr 
                        -> FT_enum_rendermode 
                        -> IO FT_error     
    
    
foreign import ccall unsafe "freetype/freetype.h FT_Get_Kerning" 
    ft_get_kerning      :: FT_face_ptr 
                        -> FT_uint 
                        -> FT_uint 
                        -> FT_uint 
                        -> Ptr FT_struct_vector
                        -> IO FT_error 
    


foreign import ccall unsafe "freetype/freetype.h FT_Get_Track_Kerning" 
    ft_get_track_kerning  :: FT_face_ptr 
                          -> FT_fixed 
                          -> FT_int 
                          -> Ptr FT_fixed 
                          -> IO FT_error 



foreign import ccall unsafe "freetype/freetype.h FT_Get_Glyph_Name" 
    ft_get_glyph_name   :: FT_face_ptr 
                        -> FT_uint
                        -> FT_pointer 
                        -> FT_uint
                        -> IO FT_error 
                         

foreign import ccall unsafe "freetype/freetype.h FT_Get_Postscript_Name" 
    ft_get_postscript_name  :: FT_face_ptr 
                            -> IO CString 


foreign import ccall unsafe "freetype/freetype.h FT_Select_Charmap" 
    ft_select_charmap   :: FT_face_ptr 
                        -> FT_enum_encoding 
                        -> IO FT_error 


foreign import ccall unsafe "freetype/freetype.h FT_Set_Charmap" 
    ft_set_charmap      :: FT_face_ptr 
                        -> FT_charmap_ptr 
                        -> IO FT_error 


foreign import ccall unsafe "freetype/freetype.h FT_Get_Charmap_Index" 
    ft_get_charmap_index  :: FT_charmap_ptr 
                          -> IO FT_int 


foreign import ccall unsafe "freetype/freetype.h FT_Get_Char_Index" 
    ft_get_char_index   :: FT_face_ptr 
                        -> FT_ulong 
                        -> IO FT_uint 


foreign import ccall unsafe "freetype/freetype.h FT_Get_First_Char" 
    ft_get_first_char   :: FT_face_ptr 
                        -> Ptr FT_uint 
                        -> IO FT_ulong


foreign import ccall unsafe "freetype/freetype.h FT_Get_Next_Char" 
    ft_get_next_char    :: FT_face_ptr 
                        -> FT_ulong 
                        -> Ptr FT_uint 
                        -> IO FT_ulong


foreign import ccall unsafe "freetype/freetype.h FT_Get_Name_Index" 
    ft_get_name_index   :: FT_face_ptr 
                        -> Ptr FT_string 
                        -> IO FT_uint
    

foreign import ccall unsafe "freetype/freetype.h FT_Get_SubGlyph_Info" 
    ft_get_subblyph_info  :: FT_glyph_slot_ptr 
                          -> FT_uint
                          -> Ptr FT_int
                          -> Ptr FT_uint
                          -> Ptr FT_int
                          -> Ptr FT_int
                          -> Ptr FT_struct_matrix
                          -> IO FT_error

    


--------------------------------------------------------------------------------

-- Peek into a FT_face_ptr pointer\'s reference. 

peekFace_num_faces :: FT_face_ptr -> IO FT_long
peekFace_num_faces ptr = do 
      i <- #{peek FT_FaceRec, num_faces} ptr
      return i
              
peekFace_face_index :: FT_face_ptr -> IO FT_long
peekFace_face_index ptr = do 
      i <- #{peek FT_FaceRec, face_index} ptr
      return i
      
peekFace_num_glyphs :: FT_face_ptr -> IO FT_long
peekFace_num_glyphs ptr = do 
      i <- #{peek FT_FaceRec, num_glyphs} ptr
      return i
      
peekFace_family_name :: FT_face_ptr -> IO String
peekFace_family_name ptr = do 
      cstr <- #{peek FT_FaceRec, family_name} ptr
      str <- peekCString cstr
      return str

peekFace_style_name :: FT_face_ptr -> IO String
peekFace_style_name ptr = do 
      cstr <- #{peek FT_FaceRec, style_name} ptr
      str <- peekCString cstr
      return str

peekFace_glyph_slot :: FT_face_ptr -> IO FT_glyph_slot_ptr
peekFace_glyph_slot ptr = do
      g <- #{peek FT_FaceRec, glyph} ptr
      return g

peekFace_glyph_slot_bitmap_left :: FT_face_ptr -> IO FT_int
peekFace_glyph_slot_bitmap_left ptr = do 
      gptr <- #{peek FT_FaceRec, glyph} ptr
      i    <- #{peek FT_GlyphSlotRec, bitmap_left} gptr
      return i
      
peekFace_glyph_slot_bitmap_top :: FT_face_ptr -> IO FT_int
peekFace_glyph_slot_bitmap_top ptr = do 
      gptr <- #{peek FT_FaceRec, glyph} ptr
      i    <- #{peek FT_GlyphSlotRec, bitmap_top} gptr
      return i


-- Bitmap is one way traffic from the C-side to the Haskell side
-- hence we don't have a full storable instance.

marshalBitmap :: FT_struct_bitmap -> IO Bitmap    
marshalBitmap bmp = do
    buf <- peekArray (r * w) $ __buffer bmp
    return $ Bitmap r w (fromIntegral $ __pitch bmp) buf  
  where
    r = fromIntegral $ __rows bmp
    w = fromIntegral $ __width bmp
    
peekFace_bitmap :: FT_face_ptr -> IO Bitmap
peekFace_bitmap ptr = do 
      gptr <- #{peek FT_FaceRec, glyph} ptr
      bptr <- #{peek FT_GlyphSlotRec, bitmap} gptr
      marshalBitmap bptr
            
-- end of file
