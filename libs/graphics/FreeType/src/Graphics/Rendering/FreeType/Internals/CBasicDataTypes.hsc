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

type VoidPtr        = FTpointer

newtype FTcallback a = FTCallback (FunPtr a) deriving Storable

--------------------------------------------------------------------------------

-- Handles

-- $FTlibrary - there are three datatypes for the C type @FT_Library@.
-- @FTLIBRARYREC_ is the private type allowing us to form Haskell pointers.
-- @FT_Library@ corresponds to the C type directly - it should only 
-- appear in ccall stubs.
-- @FTlibrary@ is the Haskell type to be exported outside the binding 
-- library, as @FT_Library@ is allocated on the C side it is foreign pointer.

data    FT_LIBRARY_REC_
type    FT_Library        = Ptr FT_LIBRARY_REC_         
newtype FTlibrary         = FTlibrary (ForeignPtr FT_LIBRARY_REC_)


-- FT_Face \\ FT_FACE_REC_ is not actually opaque, we allow access to 
-- certain fields (see 'peekFace_num_faces' and others in CBaseInterface.hsc) 
-- but we don\'t go as far as making it a Storable instance as we consider it 
-- immutable and only creatable in the on the \'C side\'.   

data    FT_FACE_REC_ 
type    FT_Face           = Ptr FT_FACE_REC_
newtype FTface            = FTface (ForeignPtr FT_FACE_REC_)


data FTsize_
type FTsize             = Ptr FTsize_

data FT_GLYPH_SLOT_
type FT_Glyph_Slot      = Ptr FT_GLYPH_SLOT_
newtype FTglyphslot     = FTglyphslot (ForeignPtr FT_GLYPH_SLOT_)


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

data FTsizerequest_
type FTsizerequest      = Ptr FTsizerequest_

data FTsubglyph_
type FTsubglyph         = Ptr FTsubglyph_

data FTslotinternal_
type FTslotinternal     = Ptr FTslotinternal_

data FTstream_
type FTstream           = Ptr FTstream_

--------------------------------------------------------------------------------
-- Enumerations

--------------------------------------------------------------------------------

class Marshal a where marshal :: a -> CInt
class Unmarshal a where unmarshal :: CInt -> a


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
      PmNone
    | PmMono
    | PmGray
    | PmGray2
    | PmGray4
    | PmLcd
    | PmLcdV
    | PmMax
    deriving ( Eq, Ord, Show )

instance Marshal FTpixelmode where
  marshal x = case x of
      PmNone  -> ft_PIXEL_MODE_NONE
      PmMono  -> ft_PIXEL_MODE_MONO
      PmGray  -> ft_PIXEL_MODE_GRAY
      PmGray2 -> ft_PIXEL_MODE_GRAY2
      PmGray4 -> ft_PIXEL_MODE_GRAY4
      PmLcd   -> ft_PIXEL_MODE_LCD
      PmLcdV  -> ft_PIXEL_MODE_LCD_V
      PmMax   -> ft_PIXEL_MODE_MAX
      
      
instance Unmarshal FTpixelmode where
  unmarshal x
      | x == ft_PIXEL_MODE_NONE   = PmNone 
      | x == ft_PIXEL_MODE_MONO   = PmMono 
      | x == ft_PIXEL_MODE_GRAY   = PmGray 
      | x == ft_PIXEL_MODE_GRAY2  = PmGray2 
      | x == ft_PIXEL_MODE_GRAY4  = PmGray4 
      | x == ft_PIXEL_MODE_LCD    = PmLcd 
      | x == ft_PIXEL_MODE_LCD_V  = PmLcdV 
      | x == ft_PIXEL_MODE_MAX    = PmMax 
      | otherwise = error ("unmarshal: FTpixelmode - illegal value " ++ show x)                  
      
      
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
      FormatNone
    | FormatComposite
    | FormatBitmap
    | FormatOutline
    | FormatPlotter
    deriving ( Enum, Eq, Ord, Show )

instance Marshal FTglyphformat where
  marshal x = case x of
      FormatNone        -> ft_GLYPH_FORMAT_NONE
      FormatComposite   -> ft_GLYPH_FORMAT_COMPOSITE
      FormatBitmap      -> ft_GLYPH_FORMAT_BITMAP
      FormatOutline     -> ft_GLYPH_FORMAT_OUTLINE
      FormatPlotter     -> ft_GLYPH_FORMAT_PLOTTER

instance Unmarshal FTglyphformat where
  unmarshal x
      | x == ft_GLYPH_FORMAT_NONE       = FormatNone  
      | x == ft_GLYPH_FORMAT_COMPOSITE  = FormatComposite  
      | x == ft_GLYPH_FORMAT_BITMAP     = FormatBitmap
      | x == ft_GLYPH_FORMAT_OUTLINE    = FormatOutline
      | x == ft_GLYPH_FORMAT_PLOTTER    = FormatPlotter  
      | otherwise = error ("unmarshal: FTglyphformat - illegal value " ++ show x)
         
                     
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

instance Marshal FTencoding where
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

instance Unmarshal FTencoding where
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

instance Marshal FTkerningmode where
  marshal x = case x of
      DefaultKerning  -> ft_KERNING_DEFAULT
      Unfitted        -> ft_KERNING_UNFITTED
      Unscaled        -> ft_KERNING_UNSCALED

instance Unmarshal FTkerningmode where
  unmarshal x
      | x == ft_KERNING_DEFAULT       = DefaultKerning  
      | x == ft_KERNING_UNFITTED      = Unfitted  
      | x == ft_KERNING_UNSCALED      = Unscaled  
      | otherwise = error ("unmarshal: FTkerningmode - illegal value " ++ show x)
              
--------------------------------------------------------------------------------    
      
type FTrendermode_ = CInt

#{enum FTrendermode_ ,
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
      
type FTloadflags_ = CInt

#{enum FTrendermode_ ,
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
  alignment _ = alignment (undefined :: FTf2dot14)
  
  peek ptr = do 
      x <- #{peek FT_UnitVector, x} ptr
      y <- #{peek FT_UnitVector, y} ptr
      return $ FTunitvector x y
  
  poke ptr (FTunitvector x y) = do
        #{poke FT_UnitVector, x} ptr x
        #{poke FT_UnitVector, y} ptr y

--------------------------------------------------------------------------------

-- | @FTbitmap@ corresponds to the FreeType type @FT_Bitmap@.

data Bitmap = Bitmap { 
      _rows         :: Int,
      _width        :: Int,
      _pitch        :: Int,
      _buffer       :: [CUChar]  -- oh no! change this to an array when working properly
   }
    deriving (Show)
    
     

data FT_Bitmap = FT_Bitmap {
      __rows            :: FTint,
      __width           :: FTint,
      __pitch           :: FTint,
      __buffer          :: Ptr CUChar,
      __num_grays       :: FTshort,
      __pixel_mode      :: CChar,
      __palette_mode    :: CChar,
      __palette         :: Ptr CVoid_
    }

instance Storable FT_Bitmap where
  sizeOf    _ = #{size FT_Bitmap}
  alignment _ = alignment (undefined :: FTint)
  
  peek ptr = do 
      r   <- #{peek FT_Bitmap, rows}          ptr
      w   <- #{peek FT_Bitmap, width}         ptr
      p   <- #{peek FT_Bitmap, pitch}         ptr
      b   <- #{peek FT_Bitmap, buffer}        ptr
      n   <- #{peek FT_Bitmap, num_grays}     ptr
      pxm <- #{peek FT_Bitmap, pixel_mode}    ptr
      plm <- #{peek FT_Bitmap, palette_mode}  ptr
      pl  <- #{peek FT_Bitmap, palette}       ptr
      return $ FT_Bitmap r w p b n pxm plm pl 
 
  poke ptr (FT_Bitmap r w p b n pxm plm pl) = do
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

data FTparameter = FTparameter {
      _tag            :: FTulong,
      _data           :: FTpointer
    }

instance Storable FTparameter where
  sizeOf    _ = #{size FT_Parameter}
  alignment _ = alignment (undefined :: FTulong)
  
  peek ptr = do 
      t <- #{peek FT_Parameter, tag} ptr
      d <- #{peek FT_Parameter, data} ptr
      return $ FTparameter t d
  
  poke ptr (FTparameter t d) = do
        #{poke FT_Parameter, tag}  ptr t
        #{poke FT_Parameter, data} ptr d
        

--------------------------------------------------------------------------------

-- | @FTopenargs@ corresponds to the FreeType type @FT_Open_Args@.

data FTopenargs = FTopenargs {
      _openargs_flags :: FTuint,
      _memory_base    :: Ptr FTbyte,
      _memory_size    :: FTlong,
      _pathname       :: CString,
      _stream         :: FTstream,
      _driver         :: FTmodule,
      _num_params     :: FTint,
      _params         :: Ptr FTparameter
    }

instance Storable FTopenargs where
  sizeOf    _ = #{size FT_Open_Args}
  alignment _ = alignment (undefined :: FTuint)
  
  peek ptr = do 
      fs <- #{peek FT_Open_Args, flags}       ptr
      mb <- #{peek FT_Open_Args, memory_base} ptr
      ms <- #{peek FT_Open_Args, memory_size} ptr
      p  <- #{peek FT_Open_Args, pathname}    ptr
      s  <- #{peek FT_Open_Args, stream}      ptr
      d  <- #{peek FT_Open_Args, driver}      ptr
      n  <- #{peek FT_Open_Args, num_params}  ptr
      ps <- #{peek FT_Open_Args, params}      ptr
      return $ FTopenargs fs mb ms p s d n ps
  
  poke ptr (FTopenargs fs mb ms p s d n ps) = do
        #{poke FT_Open_Args, flags}       ptr fs
        #{poke FT_Open_Args, memory_base} ptr mb
        #{poke FT_Open_Args, memory_size} ptr ms
        #{poke FT_Open_Args, pathname}    ptr p
        #{poke FT_Open_Args, stream}      ptr s
        #{poke FT_Open_Args, driver}      ptr d
        #{poke FT_Open_Args, num_params}  ptr n
        #{poke FT_Open_Args, params}      ptr ps



      
                 
-- end of file
