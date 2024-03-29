{-# LANGUAGE CPP                        #-}
{-# LANGUAGE ForeignFunctionInterface   #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.FreeType.Internals.CBasicDataTypes
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Internal module mapping Haskell datatypes to FreeType\'s 
-- /Basic Data Types/.
--
--
--------------------------------------------------------------------------------

module Graphics.Rendering.FreeType.Internals.CBasicDataTypes where

#include <ft2build.h>
#include FT_FREETYPE_H

import Graphics.Rendering.FreeType.Utils ( Marshal(..), Unmarshal(..) )

import Data.Int
import Data.Word
import Foreign.C.String ( peekCStringLen, newCStringLen )
import Foreign.C.Types ( CInt, CShort, CChar )
import Foreign.Marshal.Alloc ( free )
import Foreign.Ptr ( Ptr, FunPtr )
import Foreign.Storable 

--------------------------------------------------------------------------------

data CVoid_
type VoidPtr            = FT_pointer
newtype FT_callback a   = FTCallback (FunPtr a) deriving Storable

type FT_byte            = #type FT_Byte
type FT_bytes           = Ptr FT_byte
type FT_char            = #type FT_Char
type FT_int             = #type FT_Int
type FT_uint            = #type FT_UInt
type FT_int16           = #type FT_Int16
type FT_uint16          = #type FT_UInt16
type FT_int32           = #type FT_Int32
type FT_uint32          = #type FT_UInt32
type FT_short           = #type FT_Short
type FT_ushort          = #type FT_UShort
type FT_long            = #type FT_Long
type FT_ulong           = #type FT_ULong
type FT_bool            = #type FT_Bool

type FT_offset          = #type FT_Offset
type FT_ptrdist         = #type FT_PtrDist
type FT_string          = #type FT_String
type FT_tag             = #type FT_Tag
type FT_error           = #type FT_Error
type FT_fixed           = #type FT_Fixed
type FT_pointer         = Ptr CVoid_
type FT_pos             = #type FT_Pos

--------------------------------------------------------------------------------
-- FT_Vector


data Vector = Vector { 
      _xpos   :: FT_pos, 
      _ypos   :: FT_pos 
    }
  deriving (Eq, Show)

-- TODO - need to work out consistent naming
type FT_vector = Vector
type FT_struct_vector = Vector


instance Storable Vector where
  sizeOf    _ = #{size FT_Vector}
  alignment _ = alignment (undefined :: FT_pos) 
  
  peek ptr = do 
      x <- #{peek FT_Vector, x} ptr
      y <- #{peek FT_Vector, y} ptr
      return $ Vector x y
  
  poke ptr (Vector x y) = do
        #{poke FT_Vector, x} ptr x
        #{poke FT_Vector, y} ptr y
 
 
 
--------------------------------------------------------------------------------
-- FT_BBox

      
data BBox = BBox { 
      _xmin   :: FT_pos, 
      _ymin   :: FT_pos,
      _xmax   :: FT_pos, 
      _ymax   :: FT_pos }
  deriving (Eq, Show)

type FT_struct_bbox = BBox

instance Storable BBox where
  sizeOf    _ = #{size FT_BBox}
  alignment _ = alignment (undefined :: FT_pos)
  
  peek ptr = do 
      xmin <- #{peek FT_BBox, xMin} ptr
      ymin <- #{peek FT_BBox, yMin} ptr
      xmax <- #{peek FT_BBox, xMax} ptr
      ymax <- #{peek FT_BBox, yMax} ptr
      return $ BBox xmin ymin xmax ymax
  
  poke ptr (BBox xmin ymin xmax ymax) = do
        #{poke FT_BBox, xMin} ptr xmin
        #{poke FT_BBox, yMin} ptr ymin
        #{poke FT_BBox, xMax} ptr xmax
        #{poke FT_BBox, yMax} ptr ymax

-------------------------------------------------------------------------------- 
-- FT_Matrix
      
data Matrix = Matrix { 
      _xx   :: FT_fixed, 
      _xy   :: FT_fixed,
      _yx   :: FT_fixed, 
      _yy   :: FT_fixed 
    }
  deriving (Eq, Show)

type FT_struct_matrix = Matrix

instance Storable Matrix where
  sizeOf    _ = #{size FT_Matrix}
  alignment _ = alignment (undefined :: FT_fixed)
  
  peek ptr = do 
      xx <- #{peek FT_Matrix, xx} ptr
      xy <- #{peek FT_Matrix, xy} ptr
      yx <- #{peek FT_Matrix, yx} ptr
      yy <- #{peek FT_Matrix, yy} ptr
      return $ Matrix xx xy yx yy
  
  poke ptr (Matrix xx xy yx yy) = do
        #{poke FT_Matrix, xx} ptr xx
        #{poke FT_Matrix, xy} ptr xy
        #{poke FT_Matrix, yx} ptr yx
        #{poke FT_Matrix, yy} ptr yy    

--------------------------------------------------------------------------------
        
type FT_fword           = #type FT_FWord
type FT_ufword          = #type FT_UFWord
type FT_f2dot14         = #type FT_F2Dot14
type FT_f26dot6         = #type FT_F26Dot6

--------------------------------------------------------------------------------
-- FT_UnitVector

data UnitVector = UnitVector { 
      _xpos'  :: FT_f2dot14, 
      _ypos'  :: FT_f2dot14 
    }
  deriving (Eq, Show)

type FT_struct_unitvector = UnitVector

instance Storable UnitVector where
  sizeOf    _ = #{size FT_UnitVector}
  alignment _ = alignment (undefined :: FT_f2dot14)
  
  peek ptr = do 
      x <- #{peek FT_UnitVector, x} ptr
      y <- #{peek FT_UnitVector, y} ptr
      return $ UnitVector x y
  
  poke ptr (UnitVector x y) = do
        #{poke FT_UnitVector, x} ptr x
        #{poke FT_UnitVector, y} ptr y


--------------------------------------------------------------------------------
-- FT_Pixel_Mode

type FT_enum_pixelmode    = CInt

#{enum FT_enum_pixelmode ,
  , ft_PIXEL_MODE_NONE    = FT_PIXEL_MODE_NONE 
  , ft_PIXEL_MODE_MONO    = FT_PIXEL_MODE_MONO
  , ft_PIXEL_MODE_GRAY    = FT_PIXEL_MODE_GRAY
  , ft_PIXEL_MODE_GRAY2   = FT_PIXEL_MODE_GRAY2
  , ft_PIXEL_MODE_GRAY4   = FT_PIXEL_MODE_GRAY4
  , ft_PIXEL_MODE_LCD     = FT_PIXEL_MODE_LCD     
  , ft_PIXEL_MODE_LCD_V   = FT_PIXEL_MODE_LCD_V

  , ft_PIXEL_MODE_MAX     = FT_PIXEL_MODE_MAX 
  }

data PixelMode = 
      PmNone
    | PmMono
    | PmGray
    | PmGray2
    | PmGray4
    | PmLcd
    | PmLcdV
    | PmMax
    deriving ( Eq, Ord, Show )

instance Marshal PixelMode where
  marshal x = case x of
      PmNone  -> ft_PIXEL_MODE_NONE
      PmMono  -> ft_PIXEL_MODE_MONO
      PmGray  -> ft_PIXEL_MODE_GRAY
      PmGray2 -> ft_PIXEL_MODE_GRAY2
      PmGray4 -> ft_PIXEL_MODE_GRAY4
      PmLcd   -> ft_PIXEL_MODE_LCD
      PmLcdV  -> ft_PIXEL_MODE_LCD_V
      PmMax   -> ft_PIXEL_MODE_MAX
      
      
instance Unmarshal PixelMode where
  unmarshal x
      | x == ft_PIXEL_MODE_NONE   = PmNone 
      | x == ft_PIXEL_MODE_MONO   = PmMono 
      | x == ft_PIXEL_MODE_GRAY   = PmGray 
      | x == ft_PIXEL_MODE_GRAY2  = PmGray2 
      | x == ft_PIXEL_MODE_GRAY4  = PmGray4 
      | x == ft_PIXEL_MODE_LCD    = PmLcd 
      | x == ft_PIXEL_MODE_LCD_V  = PmLcdV 
      | x == ft_PIXEL_MODE_MAX    = PmMax 
      | otherwise = error ("unmarshal: PixelMode - illegal value " ++ show x)                  
      
--------------------------------------------------------------------------------
-- FT_Palette_Mode

-- FT_Palette_Mode is deprecated in FreeType - there is no corresponding 
-- Haskell type.

--------------------------------------------------------------------------------
-- FT_Bitmap


-- Bitmaps should be /produceable/ as well as /consumable/ by Haskell,
-- hence they should be storable
-- See the function @FT_Outline_Get_Bitmap@ and the example 
-- application @ftgrid.c@.  

data Bitmap = Bitmap {
      _bmp_rows         :: CInt,
      _bmp_width        :: CInt,
      _bmp_pitch        :: CInt,
      _bmp_buffer       :: String,
      _num_grays        :: CShort,
      _pixel_mode       :: CChar,
      _palette_mode     :: CChar,
      _palette          :: Ptr CVoid_
    }

instance Storable Bitmap where
  sizeOf    _ = #{size FT_Bitmap}
  alignment _ = alignment (undefined :: CInt)
  
  peek ptr = do 
      r   <- #{peek FT_Bitmap, rows}          ptr
      w   <- #{peek FT_Bitmap, width}         ptr
      p   <- #{peek FT_Bitmap, pitch}         ptr
      let slen = fromIntegral $ r * w
      buf <- #{peek FT_Bitmap, buffer}        ptr >>= \s -> peekCStringLen (s,slen)
      n   <- #{peek FT_Bitmap, num_grays}     ptr
      pxm <- #{peek FT_Bitmap, pixel_mode}    ptr
      plm <- #{peek FT_Bitmap, palette_mode}  ptr
      pl  <- #{peek FT_Bitmap, palette}       ptr
      return $ Bitmap r w p buf n pxm plm pl 
 
  poke ptr (Bitmap r w p buf n pxm plm pl) = do
        -- might want to check len against r * w
        (sptr,_) <- newCStringLen buf
        #{poke FT_Bitmap, rows}         ptr r
        #{poke FT_Bitmap, width}        ptr w
        #{poke FT_Bitmap, pitch}        ptr p        
        #{poke FT_Bitmap, buffer}       ptr sptr
        #{poke FT_Bitmap, num_grays}    ptr n
        #{poke FT_Bitmap, pixel_mode}   ptr pxm
        #{poke FT_Bitmap, palette_mode} ptr plm
        #{poke FT_Bitmap, palette}      ptr pl
        free sptr


        
--------------------------------------------------------------------------------
-- FT_IMAGE_TAG

-- Currently no Haskell equivalent.
        
        
        
        
--------------------------------------------------------------------------------
-- FT_Glyph_Format

type FT_enum_glyphformat  = CInt

#{enum FT_enum_glyphformat ,
  , ft_GLYPH_FORMAT_NONE      = FT_GLYPH_FORMAT_NONE 
  
  , ft_GLYPH_FORMAT_COMPOSITE = FT_GLYPH_FORMAT_COMPOSITE
  , ft_GLYPH_FORMAT_BITMAP    = FT_GLYPH_FORMAT_BITMAP
  , ft_GLYPH_FORMAT_OUTLINE   = FT_GLYPH_FORMAT_OUTLINE
  , ft_GLYPH_FORMAT_PLOTTER   = FT_GLYPH_FORMAT_PLOTTER
  }

data GlyphFormat = 
      FormatNone
    | FormatComposite
    | FormatBitmap
    | FormatOutline
    | FormatPlotter
    deriving ( Enum, Eq, Ord, Show )

instance Marshal GlyphFormat where
  marshal x = case x of
      FormatNone        -> ft_GLYPH_FORMAT_NONE
      FormatComposite   -> ft_GLYPH_FORMAT_COMPOSITE
      FormatBitmap      -> ft_GLYPH_FORMAT_BITMAP
      FormatOutline     -> ft_GLYPH_FORMAT_OUTLINE
      FormatPlotter     -> ft_GLYPH_FORMAT_PLOTTER

instance Unmarshal GlyphFormat where
  unmarshal x
      | x == ft_GLYPH_FORMAT_NONE       = FormatNone  
      | x == ft_GLYPH_FORMAT_COMPOSITE  = FormatComposite  
      | x == ft_GLYPH_FORMAT_BITMAP     = FormatBitmap
      | x == ft_GLYPH_FORMAT_OUTLINE    = FormatOutline
      | x == ft_GLYPH_FORMAT_PLOTTER    = FormatPlotter  
      | otherwise = error ("unmarshal: GlyphFormat - illegal value " ++ show x)
         
                     
--------------------------------------------------------------------------------
-- FT_Data

data FT_struct_data = FT_struct_data {
      _data_pointer     :: Ptr FT_byte,
      _data_length      :: FT_int     
    }


--------------------------------------------------------------------------------
-- FT_Generic_Finalizer and FT_Generic

-- My working assumption is that FT_Generic_Finalizer and FT_Generic 
-- show not be exposed to Haskell. 
--
-- The purpose of FT_Generic is \"to associate their own data to a variety 
-- of FreeType core objects\" - while this is important for a C client 
-- appliction using FreeType, I don't see this having a naturally analogy
-- for a Haskell application. It seems sensible to have Haskell 
-- \"client-specific data\" entirely on the Haskell side.  






-- end of file
