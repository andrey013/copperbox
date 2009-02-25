{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.SFont.Syntax
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Font file structured as a hierarchical syntax tree, rather than a 
-- graph of tables.
--
--------------------------------------------------------------------------------


module Graphics.SFont.Syntax where

import Graphics.SFont.PrimitiveDatatypes

-- | a TT or OT font file
data TTFF = TTFF 
      { sfnt_version      :: SfntVersion
      , number_of_tables  :: Int
      , font_header       :: FontHeader
      
      , name_recs         :: [NameRecord]
      , glyphs            :: [Glyph] 
      }
  deriving (Eq,Show)

data SfntVersion = 
        Sfnt_1_0
      | OTTO  
  deriving (Enum,Eq,Ord,Show) 

--------------------------------------------------------------------------------
-- head table 

data FontHeader = FontHeader
        { table_version_num       :: Fixed
        , font_revision           :: Fixed
        , check_sum_adjust        :: ULong
        , magic_number            :: ULong
        , head_flags              :: [HeadFlag]
        , units_per_em            :: UShort
        , created_timestamp       :: DateTime
        , modified_timestamp      :: DateTime
        , max_bb                  :: BoundingBox
        , mac_style               :: [MacStyle]
        , smallest_readable_size  :: UShort
        , font_direction_hint     :: Short      -- this could be a type...
        , index_to_loc_format     :: Short
        , glyph_data_format       :: Short
        }
  deriving (Eq,Show)

  
data HeadFlag = 
      H0_Baseline_at_y
    | H1_Left_sidebearing_at_x
    | H2_Depends_pt_size
    | H3_Force_ppem_int
    | H4_Alter_adv_width
    | H5_Undefined
    | H6_Undefined
    | H7_Undefined
    | H8_Undefined
    | H9_Undefined 
    | H10_Undefined    
    | H11_Lossless_compression
    | H12_Converted
    | H13_ClearType_optimised
    | H14_Reserved
    | H15_Reserved  
  deriving (Enum,Eq,Ord,Show)
  
    
data MacStyle = 
      S0_Bold
    | S1_Italic  
    | S2_Underline
    | S3_Outline
    | S4_Shadow
    | S5_Condensed
    | S6_Extended
    | S7_Reserved
    | S8_Reserved
    | S9_Reserved
    | S10_Reserved
    | S11_Reserved
    | S12_Reserved
    | S13_Reserved
    | S14_Reserved
    | S15_Reserved
  deriving (Enum,Eq,Ord,Show)
  
--------------------------------------------------------------------------------
-- glyf table 

type GlyphName = String 

data Glyph = 
      SimpleGlyph     GlyphName BoundingBox [Contour]
    | CompositeGlyph  GlyphName BoundingBox [CompositeElement]
  deriving (Eq,Show)    

             
newtype Contour = Contour { getContour :: [OutlinePoint] }
  deriving (Eq,Show)

data OutlinePoint = OnCurvePt  Short Short
                  | OffCurvePt Short Short
  deriving (Eq,Ord,Show)

type GylphIndex = Int

data CompositeElement = CompositeElement 
      { gylph_index     :: Int
      , composite_args  :: CompositeArgs
      , composite_trans :: CompositeTrans
      }
  deriving (Eq,Show)
      
data CompositeArgs = 
      OffsetArgs 
        { x_offset     :: Int 
        , y_offset    :: Int 
        }
    | PointNumbers 
        { parent_pt   :: Int
        , child_pt    :: Int
        } 
  deriving (Eq,Show)
  
  
data CompositeTrans =
      Scale
        { scale     :: F2Dot14 }
    | XyScale 
        { x_scale   :: F2Dot14
        , y_scale   :: F2Dot14 
        }
    | TwoByTwo
        { x_scale   :: F2Dot14
        , scale_01  :: F2Dot14
        , scale_02  :: F2Dot14 
        , y_scale   :: F2Dot14 
        } 
    | NoTrans
  deriving (Eq,Show)

--------------------------------------------------------------------------------
-- Name table

data NameRecord = NameRecord 
      { platform_id     :: PlatformId
      , encoding_id     :: EncodingId
      , language_id     :: UShort
      , name_id         :: NameId
      , name_text       :: String
    }
  deriving (Eq,Show)
  
data NameId = 
      Copyright_notice
    | Font_family_name
    | Font_subfamily_name
    | Unique_font_id
    | Full_font_name  
    | Version_string
    | PostScript_name
    | Trademark
    | Manufacturer_name
    | Designer_name
    | Description_text
    | Vendor_URL
    | Designer_URL
    | License_description
    | License_info_URL
    | Reserved_as_ZERO
    | Preferred_family
    | Preferred_subfamily
    | Compatible_full
    | Sample_text
    | PostScipt_CID
    | Reserved_name Int
  deriving (Eq,Ord,Show)

instance Enum NameId where
   fromEnum Copyright_notice    =  0
   fromEnum Font_family_name    =  1
   fromEnum Font_subfamily_name =  2
   fromEnum Unique_font_id      =  3
   fromEnum Full_font_name      =  4
   fromEnum Version_string      =  5
   fromEnum PostScript_name     =  6
   fromEnum Trademark           =  7
   fromEnum Manufacturer_name   =  8
   fromEnum Designer_name       =  9
   fromEnum Description_text    = 10
   fromEnum Vendor_URL          = 11
   fromEnum Designer_URL        = 12
   fromEnum License_description = 13
   fromEnum License_info_URL    = 14
   fromEnum Reserved_as_ZERO    = 15
   fromEnum Preferred_family    = 16
   fromEnum Preferred_subfamily = 17
   fromEnum Compatible_full     = 18
   fromEnum Sample_text         = 19
   fromEnum PostScipt_CID       = 20
   fromEnum (Reserved_name i)   = i
   
   
   toEnum  0 = Copyright_notice
   toEnum  1 = Font_family_name
   toEnum  2 = Font_subfamily_name
   toEnum  3 = Unique_font_id
   toEnum  4 = Full_font_name  
   toEnum  5 = Version_string
   toEnum  6 = PostScript_name
   toEnum  7 = Trademark
   toEnum  8 = Manufacturer_name
   toEnum  9 = Designer_name
   toEnum 10 = Description_text
   toEnum 11 = Vendor_URL
   toEnum 12 = Designer_URL
   toEnum 13 = License_description
   toEnum 14 = License_info_URL
   toEnum 15 = Reserved_as_ZERO
   toEnum 16 = Preferred_family
   toEnum 17 = Preferred_subfamily
   toEnum 18 = Compatible_full
   toEnum 19 = Sample_text
   toEnum 20 = PostScipt_CID
   toEnum  i = Reserved_name i 
   
--------------------------------------------------------------------------------
-- Common data types

-- PlatformID used in name table, cmap table ...

data PlatformId = 
      Unicode
    | Macintosh
    | ISO
    | Windows
    | Custom
    | PlatformId Int
  deriving (Eq,Ord,Show)

instance Enum PlatformId where
   fromEnum Unicode         = 0
   fromEnum Macintosh       = 1
   fromEnum ISO             = 2
   fromEnum Windows         = 3
   fromEnum Custom          = 4
   fromEnum (PlatformId i)  = fromIntegral i
  
   toEnum  0 = Unicode
   toEnum  1 = Macintosh
   toEnum  2 = ISO
   toEnum  3 = Windows
   toEnum  4 = Custom  
   toEnum  i = PlatformId $ fromIntegral i

-- PlatformID used in name table, cmap table ...

data EncodingId = 
      Unicode_1_0
    | Unicode_1_1
    | ISO_IEC_10646
    | Unicode_2_0_BMP
    | Unicode_2_0_full
    | EncodingId Int
  deriving (Eq,Ord,Show)

instance Enum EncodingId where
   fromEnum Unicode_1_0       = 0
   fromEnum Unicode_1_1       = 1
   fromEnum ISO_IEC_10646     = 2
   fromEnum Unicode_2_0_BMP   = 3
   fromEnum Unicode_2_0_full  = 4
   fromEnum (EncodingId i)    = fromIntegral i
  
   toEnum  0 = Unicode_1_0
   toEnum  1 = Unicode_1_1
   toEnum  2 = ISO_IEC_10646
   toEnum  3 = Unicode_2_0_BMP
   toEnum  4 = Unicode_2_0_full  
   toEnum  i = EncodingId $ fromIntegral i
   
      
     
-- Bounding box - do all bounding boxes in a font file (tt or ot)  
-- use short for the dimension? 
-- @head@ - does
-- @glyf@ - does
 
data BoundingBox = BoundingBox 
        { x_min   :: Short
        , y_min   :: Short
        , x_max   :: Short
        , y_max   :: Short
        }
  deriving (Eq,Show)     