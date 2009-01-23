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
import Graphics.Rendering.FreeType.Utils ( Marshal(..), Unmarshal (..) )

import Foreign.C.Types ( CInt )
import Foreign.Ptr

--------------------------------------------------------------------------------

data    FT_GLYPH_RCRD_      

type FT_glyph_ptr           = Ptr FT_GLYPH_RCRD_

newtype FT_glyph            = FT_glyph (Ptr FT_GLYPH_RCRD_) 





--------------------------------------------------------------------------------



foreign import ccall unsafe "freetype/freetype.h FT_Get_Glyph" 
    ft_get_glyph          :: FT_glyphslot_ptr 
                          -> Ptr FT_glyph_ptr 
                          -> IO FT_error


foreign import ccall unsafe "freetype/freetype.h FT_Glyph_Copy" 
    ft_glyph_copy         :: FT_glyph_ptr
                          -> Ptr FT_glyph_ptr 
                          -> IO FT_error


foreign import ccall unsafe "freetype/freetype.h FT_Glyph_Transform" 
    ft_glyph_transform    :: FT_glyph_ptr
                          -> Ptr Matrix
                          -> Ptr Vector
                          -> IO FT_error




--------------------------------------------------------------------------------
-- FT_Glyph_BBox_Mode


type FT_enum_glyph_bbox_mode      = CInt

#{enum FT_enum_glyph_bbox_mode ,
  , ft_GLYPH_BBOX_UNSCALED        = FT_GLYPH_BBOX_UNSCALED 
  , ft_GLYPH_BBOX_SUBPIXELS       = FT_GLYPH_BBOX_SUBPIXELS
  , ft_GLYPH_BBOX_GRIDFIT         = FT_GLYPH_BBOX_GRIDFIT
  , ft_GLYPH_BBOX_TRUNCATE        = FT_GLYPH_BBOX_TRUNCATE
  , ft_GLYPH_BBOX_PIXELS          = FT_GLYPH_BBOX_PIXELS
  }

data GlyphBBoxMode = 
      UnscaledBBox
    | Subpixels
    | Gridfit
    | Truncate
    | Pixels
    deriving ( Enum, Eq, Ord, Show )
    
instance Marshal GlyphBBoxMode where
  marshal x = case x of
      UnscaledBBox  -> ft_GLYPH_BBOX_UNSCALED
      Subpixels     -> ft_GLYPH_BBOX_SUBPIXELS
      Gridfit       -> ft_GLYPH_BBOX_GRIDFIT
      Truncate      -> ft_GLYPH_BBOX_TRUNCATE
      Pixels        -> ft_GLYPH_BBOX_PIXELS

instance Unmarshal GlyphBBoxMode where
  unmarshal x
      | x == ft_GLYPH_BBOX_UNSCALED     = UnscaledBBox 
      | x == ft_GLYPH_BBOX_SUBPIXELS    = Subpixels 
      | x == ft_GLYPH_BBOX_GRIDFIT      = Gridfit 
      | x == ft_GLYPH_BBOX_TRUNCATE     = Truncate 
      | x == ft_GLYPH_BBOX_PIXELS       = Pixels  
      | otherwise = error ("unmarshal: GlyphBBoxMode - illegal value " ++ show x)

--------------------------------------------------------------------------------


foreign import ccall unsafe "freetype/freetype.h FT_Glyph_Get_CBox" 
    ft_glyph_get_cbox     :: FT_glyph_ptr 
                          -> FT_uint 
                          -> Ptr FT_struct_bbox
                          -> IO ()
                          

--------------------------------------------------------------------------------

-- TODO FT_Glyph_To_Bitmap - typecasting!


foreign import ccall unsafe "freetype/freetype.h FT_Done_Glyph" 
    ft_done_glyph         :: FT_glyph_ptr 
                          -> IO ()

                            
-- end of file
