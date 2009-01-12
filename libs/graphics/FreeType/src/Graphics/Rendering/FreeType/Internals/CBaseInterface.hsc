{-# LANGUAGE CPP                        #-}
{-# LANGUAGE ForeignFunctionInterface   #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.FreeType.Internals.CBaseInterface
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Internal module declaring all some datatypes and foreign declarations 
-- defined from FT_FREETYPE_H.
--
--
--------------------------------------------------------------------------------

module Graphics.Rendering.FreeType.Internals.CBaseInterface where

#include <ft2build.h>
#include FT_FREETYPE_H

import Graphics.Rendering.FreeType.Internals.CBasicDataTypes

import Foreign.C.String ( CString, peekCString )
import Foreign.Marshal.Array ( peekArray )
import Foreign.Ptr ( Ptr ) 
import Foreign.Storable ( peekByteOff )


--------------------------------------------------------------------------------
  
-- | @FT_Init_FreeType@.
foreign import ccall unsafe "freetype/freetype.h FT_Init_FreeType" 
    ft_init_freetype :: Ptr FT_Library -> IO FTerror

-- | @FT_Done_FreeType@.
foreign import ccall unsafe "freetype/freetype.h FT_Done_FreeType" 
    ft_done_freetype :: FT_Library -> IO FTerror


--------------------------------------------------------------------------------
  
      
foreign import ccall unsafe "freetype/freetype.h FT_New_Face" 
    ft_new_face :: FT_Library 
                -> CString 
                -> FTlong 
                -> Ptr FT_Face 
                -> IO FTerror


foreign import ccall unsafe "freetype/freetype.h  FT_New_Memory_Face" 
    ft_new_memory_face :: FT_Library 
                       -> Ptr FTbyte
                       -> FTlong 
                       -> FTlong
                       -> Ptr FT_Face 
                       -> IO FTerror


foreign import ccall unsafe "freetype/freetype.h FT_Open_Face" 
    ft_open_face :: FT_Library 
                 -> Ptr FTopenargs
                 -> FTlong 
                 -> FT_Face 
                 -> IO FTerror


foreign import ccall unsafe "freetype/freetype.h FT_Attach_File" 
    ft_attach_file :: FT_Face -> CString -> IO FTerror
     

foreign import ccall unsafe "freetype/freetype.h FT_Attach_Stream" 
    ft_attach_stream :: FT_Face -> Ptr FTopenargs -> IO FTerror


foreign import ccall unsafe "freetype/freetype.h FT_Done_Face" 
    ft_done_face :: FT_Face -> IO FTerror


foreign import ccall unsafe "freetype/freetype.h FT_Select_Size" 
    ft_select_size :: FT_Face -> FTint -> IO FTerror


foreign import ccall unsafe "freetype/freetype.h FT_Request_Size" 
    ft_request_size :: FT_Face -> FTsizerequest -> IO FTerror


foreign import ccall unsafe "freetype/freetype.h FT_Set_Char_Size" 
    ft_set_char_size :: FT_Face 
                     -> FTf26dot6
                     -> FTf26dot6
                     -> FTuint
                     -> FTuint
                     -> IO FTerror 


foreign import ccall unsafe "freetype/freetype.h FT_Set_Pixel_Sizes" 
    ft_set_pixel_sizes :: FT_Face -> FTuint -> FTuint -> IO FTerror 



foreign import ccall unsafe "freetype/freetype.h FT_Load_Glyph" 
    ft_load_glyph :: FT_Face -> FTuint -> FTint32 -> IO FTerror 


foreign import ccall unsafe "freetype/freetype.h FT_Load_Char" 
    ft_load_char :: FT_Face -> FTuint -> FTint32 -> IO FTerror 




foreign import ccall unsafe "freetype/freetype.h FT_Set_Transform" 
    ft_set_transform :: FT_Face -> Ptr FTmatrix -> Ptr FTvector -> IO () 
    
    
foreign import ccall unsafe "freetype/freetype.h FT_Render_Glyph" 
    ft_render_glyph :: FT_Glyph_Slot -> FTrendermode_ -> IO FTerror     
    
    
foreign import ccall unsafe "freetype/freetype.h FT_Get_Kerning" 
    ft_get_kerning :: FT_Face 
                   -> FTuint 
                   -> FTuint 
                   -> FTuint 
                   -> Ptr FTvector
                   -> IO FTerror 
    


foreign import ccall unsafe "freetype/freetype.h FT_Get_Track_Kerning" 
    ft_get_track_kerning :: FT_Face 
                         -> FTfixed 
                         -> FTint 
                         -> Ptr FTfixed 
                         -> IO FTerror 



foreign import ccall unsafe "freetype/freetype.h FT_Get_Glyph_Name" 
    ft_get_glyph_name :: FT_Face 
                      -> FTuint
                      -> FTpointer 
                      -> FTuint
                      -> IO FTerror 
                         

foreign import ccall unsafe "freetype/freetype.h FT_Get_Postscript_Name" 
    ft_get_postscript_name :: FT_Face -> IO CString 


foreign import ccall unsafe "freetype/freetype.h FT_Select_Charmap" 
    ft_select_charmap :: FT_Face -> FTencoding_ -> IO FTerror 


foreign import ccall unsafe "freetype/freetype.h FT_Set_Charmap" 
    ft_set_charmap :: FT_Face -> FTcharmap -> IO FTerror 


foreign import ccall unsafe "freetype/freetype.h FT_Get_Charmap_Index" 
    ft_get_charmap_index :: FTcharmap -> IO FTint 


foreign import ccall unsafe "freetype/freetype.h FT_Get_Char_Index" 
    ft_get_char_index :: FT_Face -> FTulong -> IO FTuint 


foreign import ccall unsafe "freetype/freetype.h FT_Get_First_Char" 
    ft_get_first_char :: FT_Face -> Ptr FTuint -> IO FTulong


foreign import ccall unsafe "freetype/freetype.h FT_Get_Next_Char" 
    ft_get_next_char :: FT_Face -> FTulong -> Ptr FTuint -> IO FTulong


foreign import ccall unsafe "freetype/freetype.h FT_Get_Name_Index" 
    ft_get_name_index :: FT_Face ->  Ptr FTstring -> IO FTuint
    

foreign import ccall unsafe "freetype/freetype.h FT_Get_SubGlyph_Info" 
    ft_get_subblyph_info :: FT_Glyph_Slot 
                         -> FTuint
                         -> Ptr FTint
                         -> Ptr FTuint
                         -> Ptr FTint
                         -> Ptr FTint
                         -> Ptr FTmatrix
                         -> IO FTerror

    
-- | @FT_Library_Version@.
foreign import ccall unsafe "freetype/freetype.h FT_Library_Version" 
    ft_library_version :: FT_Library 
                       -> Ptr FTint 
                       -> Ptr FTint 
                       -> Ptr FTint 
                       -> IO ()

--------------------------------------------------------------------------------

-- Peek into an FT_Face 

peekFace_num_faces :: FT_Face -> IO FTlong
peekFace_num_faces ptr = do 
      i <- #{peek FT_FaceRec, num_faces} ptr
      return i
              
peekFace_face_index :: FT_Face -> IO FTlong
peekFace_face_index ptr = do 
      i <- #{peek FT_FaceRec, face_index} ptr
      return i
      
peekFace_num_glyphs :: FT_Face -> IO FTlong
peekFace_num_glyphs ptr = do 
      i <- #{peek FT_FaceRec, num_glyphs} ptr
      return i
      
peekFace_family_name :: FT_Face -> IO String
peekFace_family_name ptr = do 
      cstr <- #{peek FT_FaceRec, family_name} ptr
      str <- peekCString cstr
      return str

peekFace_style_name :: FT_Face -> IO String
peekFace_style_name ptr = do 
      cstr <- #{peek FT_FaceRec, style_name} ptr
      str <- peekCString cstr
      return str

peekFace_glyph_slot :: FT_Face -> IO FT_Glyph_Slot
peekFace_glyph_slot ptr = do
      g <- #{peek FT_FaceRec, glyph} ptr
      return g

peekFace_glyph_slot_bitmap_left :: FT_Face -> IO FTint
peekFace_glyph_slot_bitmap_left ptr = do 
      gptr <- #{peek FT_FaceRec, glyph} ptr
      i    <- #{peek FT_GlyphSlotRec, bitmap_left} gptr
      return i
      
peekFace_glyph_slot_bitmap_top :: FT_Face -> IO FTint
peekFace_glyph_slot_bitmap_top ptr = do 
      gptr <- #{peek FT_FaceRec, glyph} ptr
      i    <- #{peek FT_GlyphSlotRec, bitmap_top} gptr
      return i


-- Bitmap is one way traffic from the C-side to the Haskell side
-- hence we don't have a full storable instance.

marshalBitmap :: FT_Bitmap -> IO Bitmap    
marshalBitmap bmp = do
    buf <- peekArray (r * w) $ __buffer bmp
    return $ Bitmap r w (fromIntegral $ __pitch bmp) buf  
  where
    r = fromIntegral $ __rows bmp
    w = fromIntegral $ __width bmp
    
peekFace_bitmap :: FT_Face -> IO Bitmap
peekFace_bitmap ptr = do 
      gptr <- #{peek FT_FaceRec, glyph} ptr
      bptr <- #{peek FT_GlyphSlotRec, bitmap} gptr
      marshalBitmap bptr
            
-- end of file
