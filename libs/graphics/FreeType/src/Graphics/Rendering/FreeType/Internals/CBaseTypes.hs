{-# INCLUDE <ft2build.h> #-}
{-# INCLUDE FT_FREETYPE_H #-}
{-# LINE 1 ".\CBaseTypes.hsc" #-}
{-# LANGUAGE CPP                        #-}
{-# LINE 2 ".\CBaseTypes.hsc" #-}
{-# LANGUAGE ForeignFunctionInterface   #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.FreeType.Internals.CBaseTypes
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Haskell mappings to FreeType'\s /BaseInterface/ types. 
--
--------------------------------------------------------------------------------

module Graphics.Rendering.FreeType.Internals.CBaseTypes where


{-# LINE 24 ".\CBaseTypes.hsc" #-}

{-# LINE 25 ".\CBaseTypes.hsc" #-}


import Graphics.Rendering.FreeType.Internals.CBasicDataTypes
import Graphics.Rendering.FreeType.Utils ( Marshal(..), Unmarshal (..) )
import Data.Int
import Data.Word
import Foreign.C.String ( CString )
import Foreign.C.Types ( CInt, CChar, CUChar )
import Foreign.ForeignPtr ( ForeignPtr )
import Foreign.Ptr ( Ptr, FunPtr )
import Foreign.Storable 


--------------------------------------------------------------------------------

-- $FT_Library - there are three datatypes for the C type @FT_Library@.
-- @FT_LIBRARY_RCRD_ is the private type allowing us to form Haskell pointers.
-- @FT_library_ptr@ corresponds to the C type @FT_Library@ directly
-- @FT_library@ is the Haskell type to be exported outside the binding 
-- library, as @FT_Library@ is allocated on the C side it is foreign pointer.

data    FT_LIBRARY_RCRD_
type    FT_library_ptr    = Ptr FT_LIBRARY_RCRD_         
newtype FT_library        = FT_library (ForeignPtr FT_LIBRARY_RCRD_)


--------------------------------------------------------------------------------

-- FT_Face \\ FT_FACE_REC_ is not actually opaque, we allow access to 
-- certain fields (see 'peekFace_num_faces' and others in CBaseInterface.hsc) 
-- but we don\'t go as far as making it a Storable instance as we consider it 
-- immutable and only creatable in the on the \'C side\'.   

data    FT_FACE_RCRD_ 
type    FT_face_ptr       = Ptr FT_FACE_RCRD_
newtype FT_face           = FT_face (ForeignPtr FT_FACE_RCRD_)

--------------------------------------------------------------------------------

-- FT_Size
-- Subject to change.

data FT_size
type FT_size_ptr              = Ptr FT_size

--------------------------------------------------------------------------------
-- FT_GlyphSlot

data FT_GLYPH_SLOT_RCRD_
type FT_glyph_slot_ptr    = Ptr FT_GLYPH_SLOT_RCRD_
newtype FT_glyphslot      = FT_glyphslot (ForeignPtr FT_GLYPH_SLOT_RCRD_)

--------------------------------------------------------------------------------

-- FT_CharMap
-- Subject to change.

data FT_charmap
type FT_charmap_ptr           = Ptr FT_charmap

--------------------------------------------------------------------------------

-- FT_Encoding


type FT_enum_encoding        = CInt

ft_ENCODING_NONE               :: FT_enum_encoding
ft_ENCODING_NONE               =  0
ft_ENCODING_MS_SYMBOL          :: FT_enum_encoding
ft_ENCODING_MS_SYMBOL          =  1937337698
ft_ENCODING_UNICODE            :: FT_enum_encoding
ft_ENCODING_UNICODE            =  1970170211
ft_ENCODING_SJIS               :: FT_enum_encoding
ft_ENCODING_SJIS               =  1936353651
ft_ENCODING_GB2312             :: FT_enum_encoding
ft_ENCODING_GB2312             =  1734484000
ft_ENCODING_BIG5               :: FT_enum_encoding
ft_ENCODING_BIG5               =  1651074869
ft_ENCODING_WANSUNG            :: FT_enum_encoding
ft_ENCODING_WANSUNG            =  2002873971
ft_ENCODING_JOHAB              :: FT_enum_encoding
ft_ENCODING_JOHAB              =  1785686113
ft_ENCODING_ADOBE_STANDARD     :: FT_enum_encoding
ft_ENCODING_ADOBE_STANDARD     =  1094995778
ft_ENCODING_ADOBE_EXPERT       :: FT_enum_encoding
ft_ENCODING_ADOBE_EXPERT       =  1094992453
ft_ENCODING_ADOBE_CUSTOM       :: FT_enum_encoding
ft_ENCODING_ADOBE_CUSTOM       =  1094992451
ft_ENCODING_ADOBE_LATIN_1      :: FT_enum_encoding
ft_ENCODING_ADOBE_LATIN_1      =  1818326065
ft_ENCODING_OLD_LATIN_2        :: FT_enum_encoding
ft_ENCODING_OLD_LATIN_2        =  1818326066
ft_ENCODING_APPLE_ROMAN        :: FT_enum_encoding
ft_ENCODING_APPLE_ROMAN        =  1634889070

{-# LINE 113 ".\CBaseTypes.hsc" #-}

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
-- FT_Glyph_Metrics

-- Currently no Haskell equivalent.

-------------------------------------------------------------------------------- 
-- FT_Glyph_Metrics

-- FT_Bitmap_Size


-------------------------------------------------------------------------------- 
-- FT_Module
-- Subject to change.

data FT_module
type FT_module_ptr            = Ptr FT_module

-------------------------------------------------------------------------------- 
-- FT_Driver
-- Subject to change.

data FT_driver
type FT_driver_ptr            = Ptr FT_driver

-------------------------------------------------------------------------------- 
-- FT_Renderer
-- Subject to change.

data FT_renderer
type FT_renderer_ptr          = Ptr FT_renderer


-------------------------------------------------------------------------------- 
-- FT_CharMapRec

-- Currently no Haskell equivalent.

-------------------------------------------------------------------------------- 
-- FT_Face_Internal
-- Subject to change.

data FT_faceinternal
type FT_faceinternal_ptr      = Ptr FT_faceinternal

--------------------------------------------------------------------------------
-- FT_FaceRec

-- Currently no Haskell equivalent.

--------------------------------------------------------------------------------
-- FT_Size_Internal
-- Subject to change.

data FT_sizeinternal
type FT_sizeinternal_ptr      = Ptr FT_sizeinternal

--------------------------------------------------------------------------------
-- FT_Size_Metrics

-- Currently no Haskell equivalent.

--------------------------------------------------------------------------------
-- FT_SizeRec

-- Currently no Haskell equivalent.

--------------------------------------------------------------------------------
-- FT_SubGlyph
-- Subject to change.

data FT_subglyph
type FT_subglyph_ptr          = Ptr FT_subglyph

--------------------------------------------------------------------------------
-- FT_Slot_Internal
-- Subject to change.

data FT_slotinternal
type FT_slotinternal_ptr      = Ptr FT_slotinternal

--------------------------------------------------------------------------------
-- FT_GlyphSlotRec

-- Currently no Haskell equivalent.

--------------------------------------------------------------------------------
-- FT_Parameter

data FT_struct_parameter = FT_struct_parameter {
      _tag            :: FT_ulong,
      _data           :: FT_pointer
    }

instance Storable FT_struct_parameter where
  sizeOf    _ = (8)
{-# LINE 262 ".\CBaseTypes.hsc" #-}
  alignment _ = alignment (undefined :: FT_ulong)
  
  peek ptr = do 
      t <- (\hsc_ptr -> peekByteOff hsc_ptr 0) ptr
{-# LINE 266 ".\CBaseTypes.hsc" #-}
      d <- (\hsc_ptr -> peekByteOff hsc_ptr 4) ptr
{-# LINE 267 ".\CBaseTypes.hsc" #-}
      return $ FT_struct_parameter t d
  
  poke ptr (FT_struct_parameter t d) = do
        (\hsc_ptr -> pokeByteOff hsc_ptr 0)  ptr t
{-# LINE 271 ".\CBaseTypes.hsc" #-}
        (\hsc_ptr -> pokeByteOff hsc_ptr 4) ptr d
{-# LINE 272 ".\CBaseTypes.hsc" #-}
        
--------------------------------------------------------------------------------
-- FT_Open_Args    

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
  sizeOf    _ = (32)
{-# LINE 289 ".\CBaseTypes.hsc" #-}
  alignment _ = alignment (undefined :: FT_uint)
  
  peek ptr = do 
      fs <- (\hsc_ptr -> peekByteOff hsc_ptr 0)       ptr
{-# LINE 293 ".\CBaseTypes.hsc" #-}
      mb <- (\hsc_ptr -> peekByteOff hsc_ptr 4) ptr
{-# LINE 294 ".\CBaseTypes.hsc" #-}
      ms <- (\hsc_ptr -> peekByteOff hsc_ptr 8) ptr
{-# LINE 295 ".\CBaseTypes.hsc" #-}
      p  <- (\hsc_ptr -> peekByteOff hsc_ptr 12)    ptr
{-# LINE 296 ".\CBaseTypes.hsc" #-}
      s  <- (\hsc_ptr -> peekByteOff hsc_ptr 16)      ptr
{-# LINE 297 ".\CBaseTypes.hsc" #-}
      d  <- (\hsc_ptr -> peekByteOff hsc_ptr 20)      ptr
{-# LINE 298 ".\CBaseTypes.hsc" #-}
      n  <- (\hsc_ptr -> peekByteOff hsc_ptr 24)  ptr
{-# LINE 299 ".\CBaseTypes.hsc" #-}
      ps <- (\hsc_ptr -> peekByteOff hsc_ptr 28)      ptr
{-# LINE 300 ".\CBaseTypes.hsc" #-}
      return $ FT_struct_openargs fs mb ms p s d n ps
  
  poke ptr (FT_struct_openargs fs mb ms p s d n ps) = do
        (\hsc_ptr -> pokeByteOff hsc_ptr 0)       ptr fs
{-# LINE 304 ".\CBaseTypes.hsc" #-}
        (\hsc_ptr -> pokeByteOff hsc_ptr 4) ptr mb
{-# LINE 305 ".\CBaseTypes.hsc" #-}
        (\hsc_ptr -> pokeByteOff hsc_ptr 8) ptr ms
{-# LINE 306 ".\CBaseTypes.hsc" #-}
        (\hsc_ptr -> pokeByteOff hsc_ptr 12)    ptr p
{-# LINE 307 ".\CBaseTypes.hsc" #-}
        (\hsc_ptr -> pokeByteOff hsc_ptr 16)      ptr s
{-# LINE 308 ".\CBaseTypes.hsc" #-}
        (\hsc_ptr -> pokeByteOff hsc_ptr 20)      ptr d
{-# LINE 309 ".\CBaseTypes.hsc" #-}
        (\hsc_ptr -> pokeByteOff hsc_ptr 24)  ptr n
{-# LINE 310 ".\CBaseTypes.hsc" #-}
        (\hsc_ptr -> pokeByteOff hsc_ptr 28)      ptr ps
{-# LINE 311 ".\CBaseTypes.hsc" #-}
        
--------------------------------------------------------------------------------
-- FT_Size_Request_Type
-- Subject to change.

-- Currently no Haskell equivalent.

--------------------------------------------------------------------------------
-- FT_Size_RequestRec

-- Currently no Haskell equivalent.



--------------------------------------------------------------------------------
-- FT_Size_Request_Type
-- Subject to change.

data FT_sizerequest
type FT_sizerequest_ptr       = Ptr FT_sizerequest

--------------------------------------------------------------------------------
-- FT_Render_Mode

      
type FT_enum_rendermode = CInt

ft_RENDER_MODE_NORMAL      :: FT_enum_rendermode
ft_RENDER_MODE_NORMAL      =  0
ft_RENDER_MODE_LIGHT       :: FT_enum_rendermode
ft_RENDER_MODE_LIGHT       =  1
ft_RENDER_MODE_MONO        :: FT_enum_rendermode
ft_RENDER_MODE_MONO        =  2
ft_RENDER_MODE_LCD         :: FT_enum_rendermode
ft_RENDER_MODE_LCD         =  3
ft_RENDER_MODE_LCD_V       :: FT_enum_rendermode
ft_RENDER_MODE_LCD_V       =  4
ft_RENDER_MODE_MAX         :: FT_enum_rendermode
ft_RENDER_MODE_MAX         =  5

{-# LINE 348 ".\CBaseTypes.hsc" #-}

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
-- FT_Kerning_Mode      

type FT_enum_kerningmode = CInt

ft_KERNING_DEFAULT             :: FT_enum_kerningmode
ft_KERNING_DEFAULT             =  0
ft_KERNING_UNFITTED            :: FT_enum_kerningmode
ft_KERNING_UNFITTED            =  1
ft_KERNING_UNSCALED            :: FT_enum_kerningmode
ft_KERNING_UNSCALED            =  2

{-# LINE 387 ".\CBaseTypes.hsc" #-}

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
-- FT_LOAD_XXX
      
type FT_enum_loadflags = CInt

ft_LOAD_DEFAULT                      :: FT_enum_loadflags
ft_LOAD_DEFAULT                      =  0
ft_LOAD_NO_SCALE                     :: FT_enum_loadflags
ft_LOAD_NO_SCALE                     =  1
ft_LOAD_NO_HINTING                   :: FT_enum_loadflags
ft_LOAD_NO_HINTING                   =  2
ft_LOAD_RENDER                       :: FT_enum_loadflags
ft_LOAD_RENDER                       =  4
ft_LOAD_NO_BITMAP                    :: FT_enum_loadflags
ft_LOAD_NO_BITMAP                    =  8
ft_LOAD_VERTICAL_LAYOUT              :: FT_enum_loadflags
ft_LOAD_VERTICAL_LAYOUT              =  16
ft_LOAD_FORCE_AUTOHINT               :: FT_enum_loadflags
ft_LOAD_FORCE_AUTOHINT               =  32
ft_LOAD_CROP_BITMAP                  :: FT_enum_loadflags
ft_LOAD_CROP_BITMAP                  =  64
ft_LOAD_PEDANTIC                     :: FT_enum_loadflags
ft_LOAD_PEDANTIC                     =  128
ft_LOAD_IGNORE_GLOBAL_ADVANCE_WIDTH  :: FT_enum_loadflags
ft_LOAD_IGNORE_GLOBAL_ADVANCE_WIDTH  =  512
ft_LOAD_NO_RECURSE                   :: FT_enum_loadflags
ft_LOAD_NO_RECURSE                   =  1024
ft_LOAD_IGNORE_TRANSFORM             :: FT_enum_loadflags
ft_LOAD_IGNORE_TRANSFORM             =  2048
ft_LOAD_MONOCHROME                   :: FT_enum_loadflags
ft_LOAD_MONOCHROME                   =  4096
ft_LOAD_LINEAR_DESIGN                :: FT_enum_loadflags
ft_LOAD_LINEAR_DESIGN                =  8192
ft_LOAD_SBITS_ONLY                   :: FT_enum_loadflags
ft_LOAD_SBITS_ONLY                   =  16384
ft_LOAD_NO_AUTOHINT                  :: FT_enum_loadflags
ft_LOAD_NO_AUTOHINT                  =  32768

{-# LINE 431 ".\CBaseTypes.hsc" #-}

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
         
data FT_stream
type FT_stream_ptr            = Ptr FT_stream
      

        

--------------------------------------------------------------------------------

      
                 
-- end of file
