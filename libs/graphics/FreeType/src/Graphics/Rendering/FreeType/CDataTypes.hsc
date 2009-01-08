{-# LANGUAGE CPP                        #-}
{-# LANGUAGE ForeignFunctionInterface   #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.FreeType.CDataTypes
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Internal module aliasing Haskell datatypes to the ones defined in 
-- FT_FREETYPE_H.
--
--
--------------------------------------------------------------------------------

module Graphics.Rendering.FreeType.CDataTypes where

#include <ft2build.h>
#include FT_FREETYPE_H

import Data.Int
import Data.Word
import Foreign.C.Types ( CInt, CChar )
import Foreign.Ptr ( Ptr )
import Foreign.Storable 

data CVoid_

type FTbyte         = #type FT_Byte
type FTbytes        = Ptr FTbyte
type FTchar         = #type FT_Char
type FTint          = #type FT_Int
type FTuint         = #type FT_UInt
type FTint16        = #type FT_Int16
type FTuint16       = #type FT_UInt16
type FTint32        = #type FT_Int32
type FTuint32       = #type FT_UInt32
type FTshort        = #type FT_Short
type FTushort       = #type FT_UShort
type FTlong         = #type FT_Long
type FTulong        = #type FT_ULong
type FTbool         = #type FT_Bool

type FToffset       = #type FT_Offset
type FTptrdist      = #type FT_PtrDist
type FTstring       = #type FT_String
type FTtag          = #type FT_Tag
type FTerror        = #type FT_Error
type FTfixed        = #type FT_Fixed
type FTpointer      = Ptr CVoid_
type FTpos          = #type FT_Pos


type FTfword        = #type FT_FWord
type FTufword       = #type FT_UFWord
type FTf2dot14      = #type FT_F2Dot14

type FTf26dot6      = #type FT_F26Dot6

--------------------------------------------------------------------------------

-- Handles

data FTlibrary_ 
type FTlibrary          = Ptr FTlibrary_

data FTface_
type FTface             = Ptr FTface_

data FTsize_
type FTsize             = Ptr FTsize_

data FTglyphslot_
type FTglyphslot        = Ptr FTglyphslot_

data FTcharmap_
type FTcharmap          = Ptr FTcharmap_

data FTmodule_
type FTmodule           = Ptr FTmodule_

data FTdriver_
type FTdriver           = Ptr FTdriver_

data FTrenderer_
type FTrenderer         = Ptr FTrenderer_

data FTfaceinternal_
type FTfaceinternal     = Ptr FTfaceinternal_

data FTsizeinternal_
type FTsizeinternal     = Ptr FTsizeinternal_

data FTsubglyph_
type FTsubglyph         = Ptr FTsubglyph_

data FTslotinternal_
type FTslotinternal     = Ptr FTslotinternal_


--------------------------------------------------------------------------------
-- Enumerations

--------------------------------------------------------------------------------

type FTpixelmode_    = CInt

#{enum FTpixelmode_ ,
  , ft_PIXEL_MODE_NONE    = FT_PIXEL_MODE_NONE 
  , ft_PIXEL_MODE_MONO    = FT_PIXEL_MODE_MONO
  , ft_PIXEL_MODE_GRAY    = FT_PIXEL_MODE_GRAY
  , ft_PIXEL_MODE_GRAY2   = FT_PIXEL_MODE_GRAY2
  , ft_PIXEL_MODE_GRAY4   = FT_PIXEL_MODE_GRAY4
  , ft_PIXEL_MODE_LCD     = FT_PIXEL_MODE_LCD     
  , ft_PIXEL_MODE_LCD_V   = FT_PIXEL_MODE_LCD_V

  , ft_PIXEL_MODE_MAX     = FT_PIXEL_MODE_MAX 
  }

data FTpixelmode = 
      PixelModeNone
    | Mono
    | Gray
    | Gray2
    | Gray4
    | Lcd
    | LcdV
    | PixelModeMax
    deriving ( Enum, Eq, Ord, Show )
    
    
--------------------------------------------------------------------------------


type FTglyphformat_  = CInt

#{enum FTglyphformat_ ,
  , ft_GLYPH_FORMAT_NONE      = FT_GLYPH_FORMAT_NONE 
  
  , ft_GLYPH_FORMAT_COMPOSITE = FT_GLYPH_FORMAT_COMPOSITE
  , ft_GLYPH_FORMAT_BITMAP    = FT_GLYPH_FORMAT_BITMAP
  , ft_GLYPH_FORMAT_OUTLINE   = FT_GLYPH_FORMAT_OUTLINE
  , ft_GLYPH_FORMAT_PLOTTER   = FT_GLYPH_FORMAT_PLOTTER
  }

data FTglyphformat = 
      GlyphFormatNone
    | Composite
    | Bitmap
    | Outline
    | Plotter
    deriving ( Enum, Eq, Ord, Show )
      
--------------------------------------------------------------------------------

type FTencoding_     = CInt

#{enum FTencoding_ ,
  , ft_ENCODING_NONE              = FT_ENCODING_NONE 
  
  , ft_ENCODING_MS_SYMBOL         = FT_ENCODING_MS_SYMBOL
  , ft_ENCODING_UNICODE           = FT_ENCODING_UNICODE
  
  , ft_ENCODING_SJIS              = FT_ENCODING_SJIS
  , ft_ENCODING_GB2312            = FT_ENCODING_GB2312
  , ft_ENCODING_BIG5              = FT_ENCODING_BIG5
  , ft_ENCODING_WANSUNG           = FT_ENCODING_WANSUNG
  , ft_ENCODING_JOHAB             = FT_ENCODING_JOHAB
  
  , ft_ENCODING_ADOBE_STANDARD    = FT_ENCODING_ADOBE_STANDARD
  , ft_ENCODING_ADOBE_EXPERT      = FT_ENCODING_ADOBE_EXPERT
  , ft_ENCODING_ADOBE_CUSTOM      = FT_ENCODING_ADOBE_CUSTOM
  , ft_ENCODING_ADOBE_LATIN_1     = FT_ENCODING_ADOBE_LATIN_1
  
  , ft_ENCODING_OLD_LATIN_2       = FT_ENCODING_OLD_LATIN_2
  
  , ft_ENCODING_APPLE_ROMAN       = FT_ENCODING_APPLE_ROMAN
  }

data FTencoding = 
      EncodingNone
    | MsSymbol
    | Unicode
    | SJIS
    | GB2312
    | Big5
    | Wansung
    | Johab
    | AdobeStandard
    | AdobeExpert
    | AdobeCustom
    | AdobeLatin1
    | OldLatin2
    | AppleRoman
    deriving ( Enum, Eq, Ord, Show )
    
--------------------------------------------------------------------------------    
      
type FTkerningmode_ = CInt

#{enum FTkerningmode_ ,
  , ft_KERNING_DEFAULT            = FT_KERNING_DEFAULT 
  , ft_KERNING_UNFITTED           = FT_KERNING_UNFITTED
  , ft_KERNING_UNSCALED           = FT_KERNING_UNSCALED
  }

data FTkerningmode = 
      DefaultKerning
    | Unfitted
    | Unscaled
    deriving ( Enum, Eq, Ord, Show )

    
--------------------------------------------------------------------------------
-- Structs

-- alignment seems to be the first element of the Haskell data type.

-- | @FTvector@ corresponds to the FreeType type @FT_Vector@. 
data FTvector = FTvector { _xpos :: FTpos, _ypos :: FTpos }
  deriving (Eq, Show)

instance Storable FTvector where
  sizeOf    _ = #{size FT_Vector}
  alignment _ = alignment (undefined :: FTpos) 
  
  peek ptr = do 
      x <- #{peek FT_Vector, x} ptr
      y <- #{peek FT_Vector, y} ptr
      return $ FTvector x y
  
  poke ptr (FTvector x y) = do
        #{poke FT_Vector, x} ptr x
        #{poke FT_Vector, y} ptr y
 
--------------------------------------------------------------------------------

-- | @FTbbox@ corresponds to the FreeType type @FT_BBox@.      
data FTbbox = FTbbox { _xmin :: FTpos, _ymin :: FTpos,
                       _xmax :: FTpos, _ymax :: FTpos }
  deriving (Eq, Show)



instance Storable FTbbox where
  sizeOf    _ = #{size FT_BBox}
  alignment _ = alignment (undefined :: FTpos)
  
  peek ptr = do 
      xmin <- #{peek FT_BBox, xMin} ptr
      ymin <- #{peek FT_BBox, yMin} ptr
      xmax <- #{peek FT_BBox, xMax} ptr
      ymax <- #{peek FT_BBox, yMax} ptr
      return $ FTbbox xmin ymin xmax ymax
  
  poke ptr (FTbbox xmin ymin xmax ymax) = do
        #{poke FT_BBox, xMin} ptr xmin
        #{poke FT_BBox, yMin} ptr ymin
        #{poke FT_BBox, xMax} ptr xmax
        #{poke FT_BBox, yMax} ptr ymax

--------------------------------------------------------------------------------

-- | @FTmatrix@ corresponds to the FreeType type @FT_Matrix@.      
data FTmatrix = FTmatrix { _xx :: FTfixed, _xy :: FTfixed,
                           _yx :: FTfixed, _yy :: FTfixed }
  deriving (Eq, Show)



instance Storable FTmatrix where
  sizeOf    _ = #{size FT_Matrix}
  alignment _ = alignment (undefined :: FTfixed)
  
  peek ptr = do 
      xx <- #{peek FT_Matrix, xx} ptr
      xy <- #{peek FT_Matrix, xy} ptr
      yx <- #{peek FT_Matrix, yx} ptr
      yy <- #{peek FT_Matrix, yy} ptr
      return $ FTmatrix xx xy yx yy
  
  poke ptr (FTmatrix xx xy yx yy) = do
        #{poke FT_Matrix, xx} ptr xx
        #{poke FT_Matrix, xy} ptr xy
        #{poke FT_Matrix, yx} ptr yx
        #{poke FT_Matrix, yy} ptr yy        
   
--------------------------------------------------------------------------------

-- | @FTunitvector@ corresponds to the FreeType type @FT_UnitVector@. 
data FTunitvector = FTunitvector { _xpos' :: FTf2dot14, _ypos' :: FTf2dot14 }
  deriving (Eq, Show)

instance Storable FTunitvector where
  sizeOf    _ = #{size FT_UnitVector}
  alignment _ = alignment (undefined :: CInt)
  
  peek ptr = do 
      x <- #{peek FT_UnitVector, x} ptr
      y <- #{peek FT_UnitVector, y} ptr
      return $ FTunitvector x y
  
  poke ptr (FTunitvector x y) = do
        #{poke FT_UnitVector, x} ptr x
        #{poke FT_UnitVector, y} ptr y

--------------------------------------------------------------------------------

-- | @FTbitmap@ corresponds to the FreeType type @FT_Bitmap@.

data FTbitmap = FTbitmap {
      _rows           :: FTint,
      _width          :: FTint,
      _pitch          :: FTint,
      _buffer         :: Ptr FTbyte,
      _num_grays      :: FTshort,
      _pixel_mode     :: FTpixelmode,
      _palette_mode   :: CChar,
      _palette        :: Ptr CVoid_
    }



        
           
-- end of file
