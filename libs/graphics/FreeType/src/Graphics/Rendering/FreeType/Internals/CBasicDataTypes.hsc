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
-- Internal module aliasing Haskell datatypes to the ones defined in 
-- FT_FREETYPE_H.
--
--
--------------------------------------------------------------------------------

module Graphics.Rendering.FreeType.Internals.CBasicDataTypes where

#include <ft2build.h>
#include FT_FREETYPE_H


import Data.Int
import Data.Word
import Foreign.C.String ( CString )
import Foreign.C.Types ( CInt, CChar, CUChar )
import Foreign.ForeignPtr ( ForeignPtr )
import Foreign.Ptr ( Ptr, FunPtr )
import Foreign.Storable 

data CVoid_

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


type FT_fword           = #type FT_FWord
type FT_ufword          = #type FT_UFWord
type FT_f2dot14         = #type FT_F2Dot14

type FT_f26dot6         = #type FT_F26Dot6

type VoidPtr            = FT_pointer

newtype FT_callback a = FTCallback (FunPtr a) deriving Storable

--------------------------------------------------------------------------------

-- Handles

-- $FT_Library - there are three datatypes for the C type @FT_Library@.
-- @FT_LIBRARY_RCRD_ is the private type allowing us to form Haskell pointers.
-- @FT_library_ptr@ corresponds to the C type @FT_Library@ directly
-- @FT_library@ is the Haskell type to be exported outside the binding 
-- library, as @FT_Library@ is allocated on the C side it is foreign pointer.

data    FT_LIBRARY_RCRD_
type    FT_library_ptr    = Ptr FT_LIBRARY_RCRD_         
newtype FT_library        = FT_library (ForeignPtr FT_LIBRARY_RCRD_)


-- FT_Face \\ FT_FACE_REC_ is not actually opaque, we allow access to 
-- certain fields (see 'peekFace_num_faces' and others in CBaseInterface.hsc) 
-- but we don\'t go as far as making it a Storable instance as we consider it 
-- immutable and only creatable in the on the \'C side\'.   

data    FT_FACE_RCRD_ 
type    FT_face_ptr       = Ptr FT_FACE_RCRD_
newtype FT_face           = FT_face (ForeignPtr FT_FACE_RCRD_)




data FT_GLYPH_SLOT_RCRD_
type FT_glyph_slot_ptr    = Ptr FT_GLYPH_SLOT_RCRD_
newtype FT_glyphslot      = FT_glyphslot (ForeignPtr FT_GLYPH_SLOT_RCRD_)

-- Note - the types below haven't been used yet and will change.

data FT_size
type FT_size_ptr              = Ptr FT_size

data FT_charmap
type FT_charmap_ptr           = Ptr FT_charmap

data FT_module
type FT_module_ptr            = Ptr FT_module

data FT_driver
type FT_driver_ptr            = Ptr FT_driver

data FT_renderer
type FT_renderer_ptr          = Ptr FT_renderer

data FT_faceinternal
type FT_faceinternal_ptr      = Ptr FT_faceinternal

data FT_sizeinternal
type FT_sizeinternal_ptr      = Ptr FT_sizeinternal

data FT_sizerequest
type FT_sizerequest_ptr       = Ptr FT_sizerequest

data FT_subglyph
type FT_subglyph_ptr          = Ptr FT_subglyph

data FT_slotinternal
type FT_slotinternal_ptr      = Ptr FT_slotinternal

data FT_stream
type FT_stream_ptr            = Ptr FT_stream

--------------------------------------------------------------------------------
-- Enumerations

--------------------------------------------------------------------------------

class Marshal a where marshal :: a -> CInt
class Unmarshal a where unmarshal :: CInt -> a


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

type FT_enum_encoding        = CInt

#{enum FT_enum_encoding ,
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

data Encoding = 
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

instance Marshal Encoding where
  marshal x = case x of
      EncodingNone  -> ft_ENCODING_NONE
      MsSymbol      -> ft_ENCODING_MS_SYMBOL
      Unicode       -> ft_ENCODING_UNICODE
      SJIS          -> ft_ENCODING_SJIS
      GB2312        -> ft_ENCODING_GB2312
      Big5          -> ft_ENCODING_BIG5
      Wansung       -> ft_ENCODING_WANSUNG
      Johab         -> ft_ENCODING_JOHAB
      AdobeStandard -> ft_ENCODING_ADOBE_STANDARD
      AdobeExpert   -> ft_ENCODING_ADOBE_EXPERT
      AdobeCustom   -> ft_ENCODING_ADOBE_CUSTOM
      AdobeLatin1   -> ft_ENCODING_ADOBE_LATIN_1
      OldLatin2     -> ft_ENCODING_OLD_LATIN_2
      AppleRoman    -> ft_ENCODING_APPLE_ROMAN 

instance Unmarshal Encoding where
  unmarshal x
      | x == ft_ENCODING_NONE             = EncodingNone 
      | x == ft_ENCODING_MS_SYMBOL        = MsSymbol
      | x == ft_ENCODING_UNICODE          = Unicode 
      | x == ft_ENCODING_SJIS             = SJIS 
      | x == ft_ENCODING_GB2312           = GB2312 
      | x == ft_ENCODING_BIG5             = Big5 
      | x == ft_ENCODING_WANSUNG          = Wansung 
      | x == ft_ENCODING_JOHAB            = Johab 
      | x == ft_ENCODING_ADOBE_STANDARD   = AdobeStandard 
      | x == ft_ENCODING_ADOBE_EXPERT     = AdobeExpert 
      | x == ft_ENCODING_ADOBE_CUSTOM     = AdobeCustom 
      | x == ft_ENCODING_ADOBE_LATIN_1    = AdobeLatin1 
      | x == ft_ENCODING_OLD_LATIN_2      = OldLatin2 
      | x == ft_ENCODING_APPLE_ROMAN      = AppleRoman  
      | otherwise = error ("unmarshal: FTencoding - illegal value " ++ show x)
                
--------------------------------------------------------------------------------    
      
type FT_enum_kerningmode = CInt

#{enum FT_enum_kerningmode ,
  , ft_KERNING_DEFAULT            = FT_KERNING_DEFAULT 
  , ft_KERNING_UNFITTED           = FT_KERNING_UNFITTED
  , ft_KERNING_UNSCALED           = FT_KERNING_UNSCALED
  }

data KerningMode = 
      DefaultKerning
    | Unfitted
    | Unscaled
    deriving ( Enum, Eq, Ord, Show )

instance Marshal KerningMode where
  marshal x = case x of
      DefaultKerning  -> ft_KERNING_DEFAULT
      Unfitted        -> ft_KERNING_UNFITTED
      Unscaled        -> ft_KERNING_UNSCALED

instance Unmarshal KerningMode where
  unmarshal x
      | x == ft_KERNING_DEFAULT       = DefaultKerning  
      | x == ft_KERNING_UNFITTED      = Unfitted  
      | x == ft_KERNING_UNSCALED      = Unscaled  
      | otherwise = error ("unmarshal: KerningMode - illegal value " ++ show x)
              
--------------------------------------------------------------------------------    
      
type FT_enum_rendermode = CInt

#{enum FT_enum_rendermode ,
  , ft_RENDER_MODE_NORMAL     = FT_RENDER_MODE_NORMAL
  , ft_RENDER_MODE_LIGHT      = FT_RENDER_MODE_LIGHT
  , ft_RENDER_MODE_MONO       = FT_RENDER_MODE_MONO
  , ft_RENDER_MODE_LCD        = FT_RENDER_MODE_LCD
  , ft_RENDER_MODE_LCD_V      = FT_RENDER_MODE_LCD_V

  , ft_RENDER_MODE_MAX        = FT_RENDER_MODE_MAX

  }

data RenderMode = 
      RenderNormal
    | RenderLight
    | RenderMono
    | RenderLcd
    | RenderLcdV
    | RenderMax
    deriving ( Enum, Eq, Ord, Show )

instance Marshal RenderMode where
  marshal x = case x of
      RenderNormal  -> ft_RENDER_MODE_NORMAL
      RenderLight   -> ft_RENDER_MODE_LIGHT
      RenderMono    -> ft_RENDER_MODE_MONO
      RenderLcd     -> ft_RENDER_MODE_LCD
      RenderLcdV    -> ft_RENDER_MODE_LCD_V
      RenderMax     -> ft_RENDER_MODE_MAX

instance Unmarshal RenderMode where
  unmarshal x
      | x == ft_RENDER_MODE_NORMAL    = RenderNormal  
      | x == ft_RENDER_MODE_LIGHT     = RenderLight  
      | x == ft_RENDER_MODE_MONO      = RenderMono  
      | x == ft_RENDER_MODE_LCD       = RenderLcd  
      | x == ft_RENDER_MODE_LCD_V     = RenderLcdV  
      | x == ft_RENDER_MODE_MAX       = RenderMax       
      | otherwise = error ("unmarshal: RenderMode - illegal value " ++ show x)

--------------------------------------------------------------------------------    
      
type FT_enum_loadflags = CInt

#{enum FT_enum_loadflags ,
  , ft_LOAD_DEFAULT                     = FT_LOAD_DEFAULT
  , ft_LOAD_NO_SCALE                    = FT_LOAD_NO_SCALE
  , ft_LOAD_NO_HINTING                  = FT_LOAD_NO_HINTING
  , ft_LOAD_RENDER                      = FT_LOAD_RENDER
  , ft_LOAD_NO_BITMAP                   = FT_LOAD_NO_BITMAP
  , ft_LOAD_VERTICAL_LAYOUT             = FT_LOAD_VERTICAL_LAYOUT
  , ft_LOAD_FORCE_AUTOHINT              = FT_LOAD_FORCE_AUTOHINT
  , ft_LOAD_CROP_BITMAP                 = FT_LOAD_CROP_BITMAP
  , ft_LOAD_PEDANTIC                    = FT_LOAD_PEDANTIC
  , ft_LOAD_IGNORE_GLOBAL_ADVANCE_WIDTH = FT_LOAD_IGNORE_GLOBAL_ADVANCE_WIDTH
  , ft_LOAD_NO_RECURSE                  = FT_LOAD_NO_RECURSE
  , ft_LOAD_IGNORE_TRANSFORM            = FT_LOAD_IGNORE_TRANSFORM
  , ft_LOAD_MONOCHROME                  = FT_LOAD_MONOCHROME
  , ft_LOAD_LINEAR_DESIGN               = FT_LOAD_LINEAR_DESIGN
  , ft_LOAD_SBITS_ONLY                  = FT_LOAD_SBITS_ONLY
  , ft_LOAD_NO_AUTOHINT                 = FT_LOAD_NO_AUTOHINT
  }

data LoadFlag = 
    LoadDefault
  | NoScale
  | NoHinting
  | Render
  | NoBitmap
  | VerticalLayout
  | ForceAutohint
  | CropBitmap
  | Pedantic
  | IgnoreGlobalAdvanceWidth
  | NoRecurse
  | IgnoreTransform
  | Monochrome
  | LinearDesign
  | SbitsOnly
  | NoAutoHint
  deriving ( Enum, Eq, Ord, Show )

instance Marshal LoadFlag where
  marshal x = case x of
      LoadDefault               -> ft_LOAD_DEFAULT
      NoScale                   -> ft_LOAD_NO_SCALE
      NoHinting                 -> ft_LOAD_NO_HINTING
      Render                    -> ft_LOAD_RENDER
      NoBitmap                  -> ft_LOAD_NO_BITMAP
      VerticalLayout            -> ft_LOAD_VERTICAL_LAYOUT
      ForceAutohint             -> ft_LOAD_FORCE_AUTOHINT
      CropBitmap                -> ft_LOAD_CROP_BITMAP
      Pedantic                  -> ft_LOAD_PEDANTIC
      IgnoreGlobalAdvanceWidth  -> ft_LOAD_IGNORE_GLOBAL_ADVANCE_WIDTH
      NoRecurse                 -> ft_LOAD_NO_RECURSE
      IgnoreTransform           -> ft_LOAD_IGNORE_TRANSFORM
      Monochrome                -> ft_LOAD_MONOCHROME
      LinearDesign              -> ft_LOAD_LINEAR_DESIGN
      SbitsOnly                 -> ft_LOAD_SBITS_ONLY
      NoAutoHint                -> ft_LOAD_NO_AUTOHINT


          
--------------------------------------------------------------------------------
-- Structs

-- alignment seems to be the first element of the Haskell data type.


-- | @Vector@ corresponds to the FreeType type @FT_Vector@. 
data Vector = Vector { 
      _xpos   :: FT_pos, 
      _ypos   :: FT_pos 
    }
  deriving (Eq, Show)


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

-- | @FTbbox@ corresponds to the FreeType type @FT_BBox@.      
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

-- | @Matrix@ corresponds to the FreeType type @FT_Matrix@.      
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

-- | @UnitVector@ corresponds to the FreeType type @FT_UnitVector@. 

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

-- | @Bitmap@ is a Haskell /reduction/ of the FreeType type @FT_Bitmap@.

data Bitmap = Bitmap { 
      _rows         :: Int,
      _width        :: Int,
      _pitch        :: Int,
      _buffer       :: [CUChar]  -- oh no! change this to an array when working properly
   }
    deriving (Show)
    
     

data FT_struct_bitmap = FT_struct_bitmap {
      __rows            :: FT_int,
      __width           :: FT_int,
      __pitch           :: FT_int,
      __buffer          :: Ptr CUChar,
      __num_grays       :: FT_short,
      __pixel_mode      :: CChar,
      __palette_mode    :: CChar,
      __palette         :: Ptr CVoid_
    }

instance Storable FT_struct_bitmap where
  sizeOf    _ = #{size FT_Bitmap}
  alignment _ = alignment (undefined :: FT_int)
  
  peek ptr = do 
      r   <- #{peek FT_Bitmap, rows}          ptr
      w   <- #{peek FT_Bitmap, width}         ptr
      p   <- #{peek FT_Bitmap, pitch}         ptr
      b   <- #{peek FT_Bitmap, buffer}        ptr
      n   <- #{peek FT_Bitmap, num_grays}     ptr
      pxm <- #{peek FT_Bitmap, pixel_mode}    ptr
      plm <- #{peek FT_Bitmap, palette_mode}  ptr
      pl  <- #{peek FT_Bitmap, palette}       ptr
      return $ FT_struct_bitmap r w p b n pxm plm pl 
 
  poke ptr (FT_struct_bitmap r w p b n pxm plm pl) = do
        #{poke FT_Bitmap, rows}         ptr r
        #{poke FT_Bitmap, width}        ptr w
        #{poke FT_Bitmap, pitch}        ptr p
        #{poke FT_Bitmap, buffer}       ptr b
        #{poke FT_Bitmap, num_grays}    ptr n
        #{poke FT_Bitmap, pixel_mode}   ptr pxm
        #{poke FT_Bitmap, palette_mode} ptr plm
        #{poke FT_Bitmap, palette}      ptr pl
        

        
        
        
--------------------------------------------------------------------------------

-- | @FT_Parameter@ corresponds to the FreeType type @FT_Parameter@.

data FT_struct_parameter = FT_struct_parameter {
      _tag            :: FT_ulong,
      _data           :: FT_pointer
    }

instance Storable FT_struct_parameter where
  sizeOf    _ = #{size FT_Parameter}
  alignment _ = alignment (undefined :: FT_ulong)
  
  peek ptr = do 
      t <- #{peek FT_Parameter, tag} ptr
      d <- #{peek FT_Parameter, data} ptr
      return $ FT_struct_parameter t d
  
  poke ptr (FT_struct_parameter t d) = do
        #{poke FT_Parameter, tag}  ptr t
        #{poke FT_Parameter, data} ptr d
        

--------------------------------------------------------------------------------

-- | @FTopenargs@ corresponds to the FreeType type @FT_Open_Args@.

data FT_struct_openargs = FT_struct_openargs {
      _openargs_flags :: FT_uint,
      _memory_base    :: Ptr FT_byte,
      _memory_size    :: FT_long,
      _pathname       :: CString,
      _stream         :: FT_stream_ptr,
      _driver         :: FT_module_ptr,
      _num_params     :: FT_int,
      _params         :: Ptr FT_struct_parameter
    }

instance Storable FT_struct_openargs where
  sizeOf    _ = #{size FT_Open_Args}
  alignment _ = alignment (undefined :: FT_uint)
  
  peek ptr = do 
      fs <- #{peek FT_Open_Args, flags}       ptr
      mb <- #{peek FT_Open_Args, memory_base} ptr
      ms <- #{peek FT_Open_Args, memory_size} ptr
      p  <- #{peek FT_Open_Args, pathname}    ptr
      s  <- #{peek FT_Open_Args, stream}      ptr
      d  <- #{peek FT_Open_Args, driver}      ptr
      n  <- #{peek FT_Open_Args, num_params}  ptr
      ps <- #{peek FT_Open_Args, params}      ptr
      return $ FT_struct_openargs fs mb ms p s d n ps
  
  poke ptr (FT_struct_openargs fs mb ms p s d n ps) = do
        #{poke FT_Open_Args, flags}       ptr fs
        #{poke FT_Open_Args, memory_base} ptr mb
        #{poke FT_Open_Args, memory_size} ptr ms
        #{poke FT_Open_Args, pathname}    ptr p
        #{poke FT_Open_Args, stream}      ptr s
        #{poke FT_Open_Args, driver}      ptr d
        #{poke FT_Open_Args, num_params}  ptr n
        #{poke FT_Open_Args, params}      ptr ps



      
                 
-- end of file
