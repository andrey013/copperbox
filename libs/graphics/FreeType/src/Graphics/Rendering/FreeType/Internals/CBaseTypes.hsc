{-# LANGUAGE CPP                        #-}
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

#include <ft2build.h>
#include FT_FREETYPE_H


import Graphics.Rendering.FreeType.Internals.CBasicDataTypes
import Graphics.Rendering.FreeType.Utils ( Marshal(..), Unmarshal (..) )

import Control.Monad ( (>=>) )


import Foreign.C.String ( CString, peekCString )
import Foreign.C.Types ( CInt )
import Foreign.ForeignPtr ( ForeignPtr )
import Foreign.Ptr ( Ptr )
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

-- FT_FaceRec (the C struct corresponding to FT_FACE_RCRD_) is not 
-- actually opaque, but the struct contains private data that we 
-- don't give full access to it.


data    FT_FACE_RCRD_ 
type    FT_face_ptr       = Ptr FT_FACE_RCRD_
newtype FT_face           = FT_face (ForeignPtr FT_FACE_RCRD_)

--------------------------------------------------------------------------------

-- FT_Size

data FT_SIZE_RCRD_ 
type FT_size_ptr              = Ptr FT_SIZE_RCRD_

--------------------------------------------------------------------------------
-- FT_GlyphSlot

-- FT_GlyphSlotRec (the C struct corresponding to FT_GLYPH_SLOT_RCRD_) 
-- is not actually opaque, but the struct contains private data that we 
-- don't give full access to it.

data FT_GLYPHSLOT_RCRD_
type FT_glyphslot_ptr     = Ptr FT_GLYPHSLOT_RCRD_

-- Not a foreign Prt...
newtype FT_glyphslot      = FT_glyphslot { getGlyphslot :: FT_glyphslot_ptr }

  
--------------------------------------------------------------------------------

-- FT_CharMap

data FT_CHARMAP_RCRD_ 
type FT_charmap_ptr           = Ptr FT_CHARMAP_RCRD_

newtype FT_charmap            = FT_charmap { getCharmap :: FT_charmap_ptr }

--------------------------------------------------------------------------------

-- FT_Encoding


type FT_enum_encoding             = CInt

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
-- FT_Glyph_Metrics

data FT_struct_glyphmetrics = FT_struct_glyphmetrics {
      _gm_width         :: FT_pos,
      _gm_height        :: FT_pos,
      _hori_bearing_x   :: FT_pos,
      _hori_bearing_y   :: FT_pos,
      _hori_advance     :: FT_pos,
      _vert_bearing_x   :: FT_pos,
      _vert_bearing_y   :: FT_pos,
      _vert_advance     :: FT_pos
    }

instance Storable FT_struct_glyphmetrics where
  sizeOf    _ = #{size FT_Glyph_Metrics}
  alignment _ = alignment (undefined :: FT_pos)
  
  peek ptr = do 
      w   <- #{peek FT_Glyph_Metrics, width}        ptr
      h   <- #{peek FT_Glyph_Metrics, height}       ptr
      hbx <- #{peek FT_Glyph_Metrics, horiBearingX} ptr
      hby <- #{peek FT_Glyph_Metrics, horiBearingY} ptr
      ha  <- #{peek FT_Glyph_Metrics, horiAdvance}  ptr
      vbx <- #{peek FT_Glyph_Metrics, vertBearingX} ptr
      vby <- #{peek FT_Glyph_Metrics, vertBearingY} ptr
      va  <- #{peek FT_Glyph_Metrics, vertAdvance}  ptr
      return $ FT_struct_glyphmetrics w h hbx hby ha vbx vby va
  
  poke ptr (FT_struct_glyphmetrics w h hbx hby ha vbx vby va) = do
        #{poke FT_Glyph_Metrics, width}        ptr w
        #{poke FT_Glyph_Metrics, height}       ptr h
        #{poke FT_Glyph_Metrics, horiBearingX} ptr hbx
        #{poke FT_Glyph_Metrics, horiBearingY} ptr hby
        #{poke FT_Glyph_Metrics, horiAdvance}  ptr ha
        #{poke FT_Glyph_Metrics, vertBearingX} ptr vbx
        #{poke FT_Glyph_Metrics, vertBearingY} ptr vby
        #{poke FT_Glyph_Metrics, vertAdvance}  ptr va
        
              

--------------------------------------------------------------------------------
-- FT_Bitmap_Size

-- With a cursory reading of the FreeType API it looks as though 
-- FT_Bitmap_Size doesn't need Haskell-to-C marshalling. 
-- (FT_Bitmap_Size is only queried, never constructed by a client).
-- For the time being it has Storable instance - this may change.

data BitmapSize = BitmapSize {
      _bs_height  :: FT_short,
      _bs_width   :: FT_short,
      _bs_size    :: FT_pos,
      _bs_x_ppem  :: FT_pos,
      _bs_y_ppem  :: FT_pos
    }

instance Storable BitmapSize where
  sizeOf    _ = #{size FT_Bitmap_Size}
  alignment _ = alignment (undefined :: FT_short)
  
  peek ptr = do 
      h   <- #{peek FT_Bitmap_Size, height} ptr
      w   <- #{peek FT_Bitmap_Size, width}  ptr
      sz  <- #{peek FT_Bitmap_Size, size}   ptr
      xpp <- #{peek FT_Bitmap_Size, x_ppem} ptr
      ypp <- #{peek FT_Bitmap_Size, y_ppem} ptr
      return $ BitmapSize h w sz xpp ypp
  
  poke ptr (BitmapSize h w sz xpp ypp) = do
        #{poke FT_Bitmap_Size, height} ptr h
        #{poke FT_Bitmap_Size, width}  ptr w
        #{poke FT_Bitmap_Size, size}   ptr sz
        #{poke FT_Bitmap_Size, x_ppem} ptr xpp
        #{poke FT_Bitmap_Size, y_ppem} ptr ypp


-------------------------------------------------------------------------------- 
-- FT_Module

-- Opaque

data FT_module
type FT_module_ptr            = Ptr FT_module

-------------------------------------------------------------------------------- 
-- FT_Driver

-- Opaque


data FT_driver
type FT_driver_ptr            = Ptr FT_driver

-------------------------------------------------------------------------------- 
-- FT_Renderer

-- Opaque

data FT_renderer
type FT_renderer_ptr          = Ptr FT_renderer


-------------------------------------------------------------------------------- 
-- FT_CharMapRec

-- Leave the FT_CharMapRec on in C memory, instead provide field accessors. 
-- (No accessor to @face@ which is the parent pointer).

peekCharMap_encoding      :: FT_charmap_ptr -> IO Encoding
peekCharMap_encoding      = 
    #{peek FT_CharMapRec, encoding} >=> return . unmarshal
              
peekCharMap_platform_id   :: FT_charmap_ptr -> IO FT_ushort
peekCharMap_platform_id   = #{peek FT_CharMapRec, platform_id}

peekCharMap_encoding_id   :: FT_charmap_ptr -> IO FT_ushort
peekCharMap_encoding_id   = #{peek FT_CharMapRec, encoding_id}


-------------------------------------------------------------------------------- 
-- FT_Face_Internal

-- Opaque

data FT_faceinternal
type FT_faceinternal_ptr      = Ptr FT_faceinternal

--------------------------------------------------------------------------------
-- FT_FaceRec

-- Leave the FT_FaceRec on in C memory, instead provide field accessors. 

peekFace_num_faces :: FT_face_ptr -> IO FT_long
peekFace_num_faces = #{peek FT_FaceRec, num_faces}
              
peekFace_face_index :: FT_face_ptr -> IO FT_long
peekFace_face_index = #{peek FT_FaceRec, face_index}


-- TODO marshal this to a list of FaceFlag
peekFace_face_flags :: FT_face_ptr -> IO FT_long
peekFace_face_flags = #{peek FT_FaceRec, face_flags}

-- TODO marshal this to a list of StyleFlag
peekFace_style_flags :: FT_face_ptr -> IO FT_long
peekFace_style_flags = #{peek FT_FaceRec, style_flags}

      
peekFace_num_glyphs :: FT_face_ptr -> IO FT_long
peekFace_num_glyphs = #{peek FT_FaceRec, num_glyphs}
      
peekFace_family_name :: FT_face_ptr -> IO String
peekFace_family_name ptr = 
    #{peek FT_FaceRec, family_name} ptr >>= peekCString


peekFace_style_name :: FT_face_ptr -> IO String
peekFace_style_name ptr = do 
  #{peek FT_FaceRec, style_name} ptr >>= peekCString


-- Charmaps

-- Don't think there is a need to peek at the /generic/ field.

peekFace_bbox :: FT_face_ptr -> IO BBox
peekFace_bbox =  #{peek FT_FaceRec, bbox}


peekFace_units_per_em         :: FT_face_ptr -> IO FT_ushort
peekFace_units_per_em         = #{peek FT_FaceRec, units_per_EM}
      
peekFace_units_ascender       :: FT_face_ptr -> IO FT_short
peekFace_units_ascender       = #{peek FT_FaceRec, ascender}

peekFace_descender            :: FT_face_ptr -> IO FT_short
peekFace_descender            = #{peek FT_FaceRec, descender}

peekFace_height               :: FT_face_ptr -> IO FT_short
peekFace_height               = #{peek FT_FaceRec, height}


peekFace_max_advance_width    :: FT_face_ptr -> IO FT_short
peekFace_max_advance_width    = #{peek FT_FaceRec, max_advance_width}

peekFace_max_advance_height   :: FT_face_ptr -> IO FT_short
peekFace_max_advance_height   = #{peek FT_FaceRec, max_advance_height}

peekFace_underline_position   :: FT_face_ptr -> IO FT_short
peekFace_underline_position   = #{peek FT_FaceRec, underline_position}

peekFace_underline_thickness  :: FT_face_ptr -> IO FT_short
peekFace_underline_thickness  = #{peek FT_FaceRec, underline_thickness}


peekFace_glyph_slot   :: FT_face_ptr -> IO FT_glyphslot
peekFace_glyph_slot   = #{peek FT_FaceRec, glyph} >=> return . FT_glyphslot

peekFace_size_ptr         :: FT_face_ptr -> IO FT_size_ptr
peekFace_size_ptr         = #{peek FT_FaceRec, size}



      

--------------------------------------------------------------------------------
-- FT_FACE_FLAG_XXX

-- Note FT_enum_faceflag / FT_FACE_FLAG_XXX enumerates to a long.

type FT_enum_faceflag    = FT_long

-- Note the deprecated and internal enums in FreeType have no Haskell
-- equivalent

#{enum FT_enum_faceflag ,
  , ft_FACE_FLAG_SCALABLE         = FT_FACE_FLAG_SCALABLE
  , ft_FACE_FLAG_FIXED_SIZES      = FT_FACE_FLAG_FIXED_SIZES 
  , ft_FACE_FLAG_FIXED_WIDTH      = FT_FACE_FLAG_FIXED_WIDTH
  , ft_FACE_FLAG_SFNT             = FT_FACE_FLAG_SFNT
  , ft_FACE_FLAG_HORIZONTAL       = FT_FACE_FLAG_HORIZONTAL
  , ft_FACE_FLAG_VERTICAL         = FT_FACE_FLAG_VERTICAL
  , ft_FACE_FLAG_KERNING          = FT_FACE_FLAG_KERNING 

  , ft_FACE_FLAG_MULTIPLE_MASTERS = FT_FACE_FLAG_MULTIPLE_MASTERS
  , ft_FACE_FLAG_GLYPH_NAMES      = FT_FACE_FLAG_GLYPH_NAMES
  , ft_FACE_FLAG_HINTER           = FT_FACE_FLAG_HINTER
  , ft_FACE_FLAG_CID_KEYED        = FT_FACE_FLAG_CID_KEYED
  }
  
  
data FaceFlag = 
      Scalable
    | FixedSizes
    | FixedWidth
    | SfntStorage
    | HorizontalGlyphMetrics
    | VerticalalGlyphMetrics
    | KerningInfo
    | MultipleMasters
    | GlyphNames
    | FlagHinter
    | CidKeyed   
    deriving ( Eq, Ord, Show )

--------------------------------------------------------------------------------
-- FT_STYLE_FLAG_XXX

-- Note FT_enum_styleflag / FT_STYLE_FLAG_XXX enumerates to a long.

type FT_enum_styleflag    = FT_long

#{enum FT_enum_styleflag ,
  , ft_STYLE_FLAG_ITALIC          = FT_STYLE_FLAG_ITALIC
  , ft_STYLE_FLAG_BOLD            = FT_STYLE_FLAG_BOLD 
  }

data StyleFlag = 
      Italic
    | Bold
    deriving ( Eq, Ord, Show )

--------------------------------------------------------------------------------
-- FT_Size_Internal

-- Opaque

data FT_sizeinternal
type FT_sizeinternal_ptr      = Ptr FT_sizeinternal

--------------------------------------------------------------------------------
-- FT_Size_Metrics

data FT_struct_sizemetrics = FT_struct_sizemetrics {
      _x_ppem         :: FT_ushort,
      _y_ppem         :: FT_ushort,
      _x_scale        :: FT_fixed,
      _y_scale        :: FT_fixed,
      _sm_ascender    :: FT_pos,
      _sm_descender   :: FT_pos,
      _sm_height      :: FT_pos,
      _max_advance    :: FT_pos
    }
    
instance Storable FT_struct_sizemetrics where
  sizeOf    _ = #{size FT_Size_Metrics}
  alignment _ = alignment (undefined :: FT_face_ptr)
  
  peek ptr = do 
      xpp <- #{peek FT_Size_Metrics, x_ppem}      ptr
      ypp <- #{peek FT_Size_Metrics, y_ppem}      ptr
      xsc <- #{peek FT_Size_Metrics, x_scale}     ptr
      ysc <- #{peek FT_Size_Metrics, y_scale}     ptr
      asc <- #{peek FT_Size_Metrics, ascender}    ptr
      des <- #{peek FT_Size_Metrics, descender}   ptr
      h   <- #{peek FT_Size_Metrics, height}      ptr
      mxa <- #{peek FT_Size_Metrics, max_advance} ptr
      return $ FT_struct_sizemetrics xpp ypp xsc ysc asc des h mxa
  
  poke ptr (FT_struct_sizemetrics xpp ypp xsc ysc asc des h mxa) = do
        #{poke FT_Size_Metrics, x_ppem}      ptr xpp
        #{poke FT_Size_Metrics, y_ppem}      ptr ypp
        #{poke FT_Size_Metrics, x_scale}     ptr xsc
        #{poke FT_Size_Metrics, y_scale}     ptr ysc
        #{poke FT_Size_Metrics, ascender}    ptr asc 
        #{poke FT_Size_Metrics, descender}   ptr des
        #{poke FT_Size_Metrics, height}      ptr h 
        #{poke FT_Size_Metrics, max_advance} ptr mxa  

--------------------------------------------------------------------------------
-- FT_SizeRec

{-


data FT_struct_size = FT_struct_size {
      _sz_face         :: FT_face_ptr,
      _sz_generic      :: FT_struct_generic,
      _sz_metrics      :: FT_ushort,
      _sz_internal     :: FT_sizeinternal_ptr
    }

-}


--------------------------------------------------------------------------------
-- FT_SubGlyph

-- Opaque

data FT_subglyph
type FT_subglyph_ptr          = Ptr FT_subglyph

--------------------------------------------------------------------------------
-- FT_Slot_Internal

-- Opaque

data FT_slotinternal
type FT_slotinternal_ptr      = Ptr FT_slotinternal

--------------------------------------------------------------------------------
-- FT_GlyphSlotRec

-- Leave the FT_GlyphSlotRec on in C memory, instead provide field accessors. 

-- The C struct fields @library@ and @face@ are parent pointers,
-- we don't give access to them.

peekGlyphSlot_metrics         :: FT_glyphslot_ptr -> IO FT_struct_glyphmetrics
peekGlyphSlot_metrics         = #{peek FT_GlyphSlotRec, metrics}


peekGlyphSlot_linearHoriAdvance   :: FT_glyphslot_ptr -> IO FT_fixed
peekGlyphSlot_linearHoriAdvance   = #{peek FT_GlyphSlotRec, linearHoriAdvance}

peekGlyphSlot_linearVertAdvance   :: FT_glyphslot_ptr -> IO FT_fixed
peekGlyphSlot_linearVertAdvance   = #{peek FT_GlyphSlotRec, linearVertAdvance}

peekGlyphSlot_advance             :: FT_glyphslot_ptr -> IO FT_vector
peekGlyphSlot_advance             = #{peek FT_GlyphSlotRec, advance}



peekGlyphSlot_format              :: FT_glyphslot_ptr -> IO GlyphFormat
peekGlyphSlot_format ptr          = 
    #{peek FT_GlyphSlotRec, format} ptr >>= return . unmarshal
    

peekGlyphSlot_bitmap :: FT_glyphslot_ptr -> IO Bitmap
peekGlyphSlot_bitmap ptr = 
    #{peek FT_GlyphSlotRec, bitmap} ptr

    
peekGlyphSlot_bitmap_left       :: FT_glyphslot_ptr -> IO FT_int
peekGlyphSlot_bitmap_left       = #{peek FT_GlyphSlotRec, bitmap_left}

peekGlyphSlot_bitmap_top        :: FT_glyphslot_ptr -> IO FT_int
peekGlyphSlot_bitmap_top        = #{peek FT_GlyphSlotRec, bitmap_top}
      
peekGlyphSlot_num_subglyphs     ::  FT_glyphslot_ptr -> IO FT_uint
peekGlyphSlot_num_subglyphs     = #{peek FT_GlyphSlotRec, num_subglyphs}



--------------------------------------------------------------------------------




--------------------------------------------------------------------------------
-- FT_Parameter

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
-- FT_Open_Args    

data FT_struct_openargs = FT_struct_openargs {
      _openargs_flags :: FT_uint,
      _memory_base    :: Ptr FT_byte,
      _memory_size    :: FT_long,
      _pathname       :: CString,
      _oa_stream      :: FT_stream_ptr,
      _oa_driver      :: FT_module_ptr,
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



-- FT_Stream defined in FT_SYSTEM_H 

-- Opaque
         
data FT_stream
type FT_stream_ptr            = Ptr FT_stream

        
--------------------------------------------------------------------------------
-- FT_Size_Request_Type

type FT_enum_sizerequest_type        = CInt

#{enum FT_enum_sizerequest_type ,
  , ft_SIZE_REQUEST_TYPE_NOMINAL    = FT_SIZE_REQUEST_TYPE_NOMINAL 
  , ft_SIZE_REQUEST_TYPE_REAL_DIM   = FT_SIZE_REQUEST_TYPE_REAL_DIM
  , ft_SIZE_REQUEST_TYPE_BBOX       = FT_SIZE_REQUEST_TYPE_BBOX
  , ft_SIZE_REQUEST_TYPE_CELL       = FT_SIZE_REQUEST_TYPE_CELL
  , ft_SIZE_REQUEST_TYPE_SCALES     = FT_SIZE_REQUEST_TYPE_SCALES

  , ft_SIZE_REQUEST_TYPE_MAX        = FT_SIZE_REQUEST_TYPE_MAX
  }

data SizeResquestType = 
      Nominal
    | RealDim
    | SrtBBox
    | Cell
    | Scales
    | SrtMax
    deriving ( Eq, Ord, Show )

instance Marshal SizeResquestType where
  marshal x = case x of
      Nominal -> ft_SIZE_REQUEST_TYPE_NOMINAL
      RealDim -> ft_SIZE_REQUEST_TYPE_REAL_DIM
      SrtBBox -> ft_SIZE_REQUEST_TYPE_BBOX
      Cell    -> ft_SIZE_REQUEST_TYPE_CELL
      Scales  -> ft_SIZE_REQUEST_TYPE_SCALES
      SrtMax  -> ft_SIZE_REQUEST_TYPE_MAX

      
      
instance Unmarshal SizeResquestType where
  unmarshal x
      | x == ft_SIZE_REQUEST_TYPE_NOMINAL   = Nominal 
      | x == ft_SIZE_REQUEST_TYPE_REAL_DIM  = RealDim 
      | x == ft_SIZE_REQUEST_TYPE_BBOX      = SrtBBox 
      | x == ft_SIZE_REQUEST_TYPE_CELL      = Cell 
      | x == ft_SIZE_REQUEST_TYPE_SCALES    = Scales 
      | x == ft_SIZE_REQUEST_TYPE_MAX       = SrtMax 
      | otherwise = error ("unmarshal: SizeResquestType - illegal value " ++ show x) 
      
          
--------------------------------------------------------------------------------
-- FT_Size_RequestRec


data FT_struct_sizerequest = FT_struct_sizerequest {
      _sr_type    :: SizeResquestType,
      _sr_width   :: FT_long,
      _sr_height  :: FT_long,
      _h_res      :: FT_uint,
      _v_res      :: FT_uint
    }
    
    
instance Storable FT_struct_sizerequest where
  sizeOf    _ = #{size FT_Size_RequestRec}
  alignment _ = alignment (undefined :: CInt)
  
  peek ptr = do 
      t   <- #{peek FT_Size_RequestRec, type}           ptr
      w   <- #{peek FT_Size_RequestRec, width}          ptr
      h   <- #{peek FT_Size_RequestRec, height}         ptr
      hr  <- #{peek FT_Size_RequestRec, horiResolution} ptr
      vr  <- #{peek FT_Size_RequestRec, vertResolution} ptr        
      return $ FT_struct_sizerequest (unmarshal t) w h hr vr
  
  poke ptr (FT_struct_sizerequest t w h hr vr) = do
        #{poke FT_Size_RequestRec, type}           ptr (marshal t)
        #{poke FT_Size_RequestRec, width}          ptr w
        #{poke FT_Size_RequestRec, height}         ptr h
        #{poke FT_Size_RequestRec, horiResolution} ptr hr
        #{poke FT_Size_RequestRec, vertResolution} ptr vr
        

type FT_sizerequest_ptr       = Ptr FT_struct_sizerequest

--------------------------------------------------------------------------------
-- FT_Render_Mode

      
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
-- FT_Kerning_Mode      

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
-- FT_LOAD_XXX
      
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

      
                 
-- end of file
