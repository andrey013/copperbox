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
import Data.Int
import Data.Word
import Foreign.C.String ( CString )
import Foreign.C.Types ( CInt, CShort, CLong, CChar, CUChar )
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

type FT_size_ptr              = Ptr FT_struct_size

--------------------------------------------------------------------------------
-- FT_GlyphSlot

data FT_GLYPH_SLOT_RCRD_
type FT_glyphslot_ptr     = Ptr FT_GLYPH_SLOT_RCRD_
newtype FT_glyphslot      = FT_glyphslot (ForeignPtr FT_GLYPH_SLOT_RCRD_)

--------------------------------------------------------------------------------

-- FT_CharMap

type FT_charmap_ptr           = Ptr FT_struct_charmap

--------------------------------------------------------------------------------

-- FT_Encoding


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

data FT_struct_bitmapsize = FT_struct_bitmapsize {
      _bs_height  :: FT_short,
      _bs_width   :: FT_short,
      _bs_size    :: FT_pos,
      _bs_x_ppem  :: FT_pos,
      _bs_y_ppem  :: FT_pos
    }

instance Storable FT_struct_bitmapsize where
  sizeOf    _ = #{size FT_Bitmap_Size}
  alignment _ = alignment (undefined :: FT_short)
  
  peek ptr = do 
      h   <- #{peek FT_Bitmap_Size, height} ptr
      w   <- #{peek FT_Bitmap_Size, width}  ptr
      sz  <- #{peek FT_Bitmap_Size, size}   ptr
      xpp <- #{peek FT_Bitmap_Size, x_ppem} ptr
      ypp <- #{peek FT_Bitmap_Size, y_ppem} ptr
      return $ FT_struct_bitmapsize h w sz xpp ypp
  
  poke ptr (FT_struct_bitmapsize h w sz xpp ypp) = do
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

data FT_struct_charmap = FT_struct_charmap {
      _face         :: FT_face_ptr,
      _encoding     :: Encoding,
      _platform_id  :: FT_ushort,
      _encoding_id  :: FT_ushort
    }

instance Storable FT_struct_charmap where
  sizeOf    _ = #{size FT_CharMapRec}
  alignment _ = alignment (undefined :: FT_face_ptr)
  
  peek ptr = do 
      f   <- #{peek FT_CharMapRec, face}        ptr
      e   <- #{peek FT_CharMapRec, encoding}    ptr
      pid <- #{peek FT_CharMapRec, platform_id} ptr
      eid <- #{peek FT_CharMapRec, encoding_id} ptr
      return $ FT_struct_charmap f (unmarshal e) pid eid
  
  poke ptr (FT_struct_charmap f e pid eid) = do
        #{poke FT_CharMapRec, face}        ptr f
        #{poke FT_CharMapRec, encoding}    ptr (marshal e)
        #{poke FT_CharMapRec, platform_id} ptr pid
        #{poke FT_CharMapRec, encoding_id} ptr eid


-------------------------------------------------------------------------------- 
-- FT_Face_Internal

-- Opaque

data FT_faceinternal
type FT_faceinternal_ptr      = Ptr FT_faceinternal

--------------------------------------------------------------------------------
-- FT_FaceRec

data FT_struct_face = FT_struct_face {
      _num_faces          :: FT_long,
      _face_index         :: FT_long,

      _face_flags         :: FT_long,
      _style_flags        :: FT_long,

      _num_glyphs         :: FT_long,

      _family_name        :: CString,
      _style_name         :: CString,

      _num_fixed_sizes    :: FT_int,
      _available_sizes    :: Ptr FT_struct_bitmapsize,

      _num_charmaps       :: FT_int,
      _charmaps           :: Ptr FT_struct_charmap,

      _generic            :: FT_struct_generic,
      
      _bbox               :: BBox,

      _units_per_em       :: FT_ushort,
      _ascender           :: FT_short,
      _descender          :: FT_short,
      _height             :: FT_short,     

      _max_advance_width    :: FT_short, 
      _max_advance_height   :: FT_short,

      _underline_position   :: FT_short,
      _underline_thickness  :: FT_short,

      _glyph                :: FT_glyphslot_ptr,
      _size                 :: FT_size_ptr,
      _charmap              :: FT_charmap_ptr,

      -- Private data
      _driver               :: FT_driver_ptr,
      _memory               :: FT_memory_ptr,
      _stream               :: FT_stream_ptr,

      _sizes_list           :: FT_struct_list,

      _autohint             :: FT_struct_generic,
      _extensions           :: VoidPtr,

      _internal             :: FT_faceinternal_ptr
    }

-- FT_Memory defined in FT_SYSTEM_H 
data FT_memory
type FT_memory_ptr      = Ptr FT_memory


-- FT_ListRec defined in FT_TYPES_H
data FT_struct_list = FT_struct_list { 
      _ls_head      :: FT_listnode_ptr,
      _ls_tail      :: FT_listnode_ptr
    }
    
data FT_listnode
type FT_listnode_ptr    = Ptr FT_listnode




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


data FT_struct_size = FT_struct_size {
      _sz_face         :: FT_face_ptr,
      _sz_generic      :: FT_struct_generic,
      _sz_metrics      :: FT_ushort,
      _sz_internal     :: FT_sizeinternal_ptr
    }



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

-- Currently no Haskell equivalent.

data FT_struct_glyphslot = FT_struct_glyphslot {
      _gs_library         :: FT_library_ptr,
      _gs_face            :: FT_face_ptr,
      _gs_next            :: FT_glyphslot_ptr,
      _gs_reserved        :: FT_uint,
      _gs_generic         :: FT_struct_generic,
      _gs_metrics         :: FT_struct_glyphmetrics,
    
      _linear_h_advance   :: FT_fixed,
      _linear_v_advance   :: FT_fixed,
      _gs_advance         :: FT_struct_vector,
      
      _gs_format          :: GlyphFormat,

      _gs_bitmap          :: FT_struct_bitmap,
      _bitmap_left        :: FT_int,
      _bitmap_top         :: FT_int,
      
      _gs_outline         :: FT_struct_outline,
      _gs_num_subglyphs   :: FT_uint,
      _gs_subglyphs       :: FT_subglyph_ptr,
      
      _control_data       :: VoidPtr,
      _control_len        :: CLong,
      
      _lsb_delta          :: FT_pos,
      _rsb_delta          :: FT_pos,
      
      _gs_other           :: VoidPtr,
      
      _gs_internal        :: FT_slotinternal_ptr
    }   


--------------------------------------------------------------------------------

-- FT_Outline
-- Defined in FT_IMAGE_H 

data FT_struct_outline = FT_struct_outline {
      _n_contours     :: CShort,
      _n_points       :: CShort,
      _points         :: Ptr FT_struct_vector,
      _tags           :: Ptr CChar,
      _contours       :: Ptr CShort,
      _outline_flags  :: CInt
    }

instance Storable FT_struct_outline where
  sizeOf    _ = #{size FT_Outline}
  alignment _ = alignment (undefined :: CShort)
  
  peek ptr = do 
      a <- #{peek FT_Outline, n_contours} ptr
      b <- #{peek FT_Outline, n_points}   ptr
      c <- #{peek FT_Outline, points}     ptr
      d <- #{peek FT_Outline, tags}       ptr
      e <- #{peek FT_Outline, contours}   ptr
      f <- #{peek FT_Outline, flags}      ptr
      return $ FT_struct_outline a b c d e f
  
  poke ptr (FT_struct_outline a b c d e f) = do
        #{poke FT_Outline, n_contours}  ptr a
        #{poke FT_Outline, n_points}    ptr b
        #{poke FT_Outline, points}      ptr c
        #{poke FT_Outline, tags}        ptr d
        #{poke FT_Outline, contours}    ptr e
        #{poke FT_Outline, flags}       ptr f

type FT_outline_ptr = Ptr FT_struct_outline
newtype FT_outline  = FT_outline (ForeignPtr FT_struct_outline) 

type FT_enum_outlineflags    = CInt

#{enum FT_enum_outlineflags ,
  , ft_OUTLINE_NONE             = FT_OUTLINE_NONE
  , ft_OUTLINE_OWNER            = FT_OUTLINE_OWNER
  , ft_OUTLINE_EVEN_ODD_FILL    = FT_OUTLINE_EVEN_ODD_FILL
  , ft_OUTLINE_REVERSE_FILL     = FT_OUTLINE_REVERSE_FILL
  , ft_OUTLINE_IGNORE_DROPOUTS  = FT_OUTLINE_IGNORE_DROPOUTS
  , ft_OUTLINE_SMART_DROPOUTS   = FT_OUTLINE_SMART_DROPOUTS
  , ft_OUTLINE_INCLUDE_STUBS    = FT_OUTLINE_INCLUDE_STUBS

  , ft_OUTLINE_HIGH_PRECISION   = FT_OUTLINE_HIGH_PRECISION
  , ft_OUTLINE_SINGLE_PASS      = FT_OUTLINE_SINGLE_PASS 
  }




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
      _oa_stream         :: FT_stream_ptr,
      _oa_driver         :: FT_module_ptr,
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
