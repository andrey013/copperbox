{-# LANGUAGE CPP                        #-}
{-# LANGUAGE ForeignFunctionInterface   #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.FreeType.CInternals
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

module Graphics.Rendering.FreeType.CFunDecls where

#include <ft2build.h>
#include FT_FREETYPE_H

import Graphics.Rendering.FreeType.CDataTypes

import Foreign.C.String ( CString )
import Foreign.Ptr ( Ptr ) 



--------------------------------------------------------------------------------
  
-- | @FT_Init_FreeType@.
foreign import ccall unsafe "freetype/freetype.h FT_Init_FreeType" 
    ft_Init_FreeType :: Ptr FTlibrary -> IO FTerror

-- | @FT_Done_FreeType@.
foreign import ccall unsafe "freetype/freetype.h FT_Done_FreeType" 
    ft_Done_FreeType :: FTlibrary ->IO FTerror


--------------------------------------------------------------------------------
  
      
foreign import ccall unsafe "freetype/freetype.h FT_New_Face" 
    ft_New_Face :: FTlibrary -> CString -> FTlong -> Ptr FTface -> IO FTerror


foreign import ccall unsafe "freetype/freetype.h FT_New_Face" 
    ft_New_Memory_Face :: FTlibrary 
                       -> Ptr FTbyte
                       -> FTlong 
                       -> FTlong
                       -> Ptr FTface 
                       -> IO FTerror

-- FT_Open_Face

foreign import ccall unsafe "freetype/freetype.h FT_Attach_File" 
    ft_Attach_File :: FTface -> CString -> IO FTerror
     

-- FT_Attach_Stream

foreign import ccall unsafe "freetype/freetype.h FT_Done_Face" 
    ft_Done_Face :: FTface -> IO FTerror


foreign import ccall unsafe "freetype/freetype.h FT_Select_Size" 
    ft_Select_Size :: FTface -> FTint -> IO FTerror

-- FT_Request_Size

foreign import ccall unsafe "freetype/freetype.h FT_Set_Char_Size" 
    ft_Set_Char_Size :: FTface 
                     -> FTf26dot6
                     -> FTf26dot6
                     -> FTuint
                     -> FTuint
                     -> IO FTerror 


foreign import ccall unsafe "freetype/freetype.h FT_Set_Pixel_Sizes" 
    ft_Set_Pixel_Sizes :: FTface -> FTuint -> FTuint -> IO FTerror 



foreign import ccall unsafe "freetype/freetype.h FT_Load_Glyph" 
    ft_Load_Glyph :: FTface -> FTuint -> FTint32 -> IO FTerror 


foreign import ccall unsafe "freetype/freetype.h FT_Load_Char" 
    ft_Load_Char :: FTface -> FTuint -> FTint32 -> IO FTerror 




foreign import ccall unsafe "freetype/freetype.h FT_Set_Transform" 
    ft_Set_Transform :: FTface -> Ptr FTmatrix -> Ptr FTvector -> IO () 
    
    
-- foreign import ccall unsafe "freetype/freetype.h FT_Render_Glyph" 
--    ft_Render_Glyph :: FTglyphslot -> FTrendermode_ -> IO FTerror     
    
    
foreign import ccall unsafe "freetype/freetype.h FT_Get_Kerning" 
    ft_Get_Kerning :: FTface 
                   -> FTuint 
                   -> FTuint 
                   -> FTuint 
                   -> Ptr FTvector
                   -> IO FTerror 
    


foreign import ccall unsafe "freetype/freetype.h FT_Get_Track_Kerning" 
    ft_Get_Track_Kerning :: FTface 
                         -> FTfixed 
                         -> FTint 
                         -> Ptr FTfixed 
                         -> IO FTerror 



foreign import ccall unsafe "freetype/freetype.h FT_Get_Glyph_Name" 
    ft_Get_Glyph_Name :: FTface 
                      -> FTuint
                      -> FTpointer 
                      -> FTuint
                      -> IO FTerror 
                         

foreign import ccall unsafe "freetype/freetype.h FT_Get_Postscript_Name" 
    ft_Get_Postscript_Name :: FTface -> IO CString 


foreign import ccall unsafe "freetype/freetype.h FT_Select_Charmap" 
    ft_Select_Charmap :: FTface -> FTencoding_ -> IO FTerror 


foreign import ccall unsafe "freetype/freetype.h FT_Set_Charmap" 
    ft_Set_Charmap :: FTface -> FTcharmap -> IO FTerror 


foreign import ccall unsafe "freetype/freetype.h FT_Get_Charmap_Index" 
    ft_Get_Charmap_Index :: FTcharmap -> IO FTint 


foreign import ccall unsafe "freetype/freetype.h FT_Get_Char_Index" 
    ft_Get_Char_Index :: FTface -> FTulong -> IO FTuint 


foreign import ccall unsafe "freetype/freetype.h FT_Get_First_Char" 
    ft_Get_First_Char :: FTface -> Ptr FTuint -> IO FTulong


foreign import ccall unsafe "freetype/freetype.h FT_Get_Next_Char" 
    ft_Get_Next_Char :: FTface -> FTulong -> Ptr FTuint -> IO FTulong


foreign import ccall unsafe "freetype/freetype.h FT_Get_Name_Index" 
    ft_Get_Name_Index :: FTface ->  Ptr FTstring -> IO FTuint
    

foreign import ccall unsafe "freetype/freetype.h FT_Get_SubGlyph_Info" 
    ft_Get_SubGlyph_Info :: FTglyphslot 
                         -> FTuint
                         -> Ptr FTint
                         -> Ptr FTuint
                         -> Ptr FTint
                         -> Ptr FTint
                         -> Ptr FTmatrix
                         -> IO FTerror

    
-- | @FT_Library_Version@.
foreign import ccall unsafe "freetype/freetype.h FT_Library_Version" 
    ft_Library_Version :: FTlibrary 
                       -> Ptr FTint 
                       -> Ptr FTint 
                       -> Ptr FTint 
                       -> IO ()


-- end of file
