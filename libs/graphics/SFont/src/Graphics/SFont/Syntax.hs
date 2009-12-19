{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
-- AST for font files.
--
--------------------------------------------------------------------------------


module Graphics.SFont.Syntax where

import Data.Int
import qualified Data.Map as Map
import Data.Word



type Uint8   = Word8
type Uint16  = Word16
type Uint32  = Word32



type Byte   = Word8

-- This is a @Char@ in terms of the OFF, naturally we use something else
type SByte  = Int8 

type UShort = Word16
type Short  = Int16 
    
type ULong  = Word32
type Long   = Int32

type Offset = Word16


--------------------------------------------------------------------------------
-- Numbers - these will all need sorting out at some point

-- 16.16 float
newtype Fixed = Fixed { unFixed :: Double }
  deriving (Eq,Ord,Num, Fractional)

instance Show Fixed where 
  show = show . unFixed
    
instance Read Fixed where
  readsPrec i s = map (\(d,r) -> (Fixed d,r)) $ readsPrec i s      

-- 16bit signed integer
newtype FWord = FWord { unFWord :: Int16 }
  deriving (Eq,Ord,Num)

instance Show FWord where 
  show = show . unFWord
    
instance Read FWord where
  readsPrec i s = map (\(d,r) -> (FWord d,r)) $ readsPrec i s   


newtype UFWord = UFWord { unUFWord :: Word16 }
  deriving (Eq,Ord,Num)

instance Show UFWord where 
  show = show . unUFWord
    
instance Read UFWord where
  readsPrec i s = map (\(d,r) -> (UFWord d,r)) $ readsPrec i s  

newtype F2Dot14 = F2Dot14 { unF2Dot14 :: Double }
  deriving (Eq,Ord,Num,Show)

instance Read F2Dot14 where
  readsPrec i s = map (\(d,r) -> (F2Dot14 d,r)) $ readsPrec i s 
  
    
--------------------------------------------------------------------------------
-- DateTime - needs sorting out at some point

data DateTime = DateTime Word64 ()

instance Show DateTime where
  show (DateTime i _) = show i
  
instance Eq DateTime where
  DateTime i _ == DateTime j _ = i == j

--------------------------------------------------------------------------------

-- A region within the font file which is loaded as an array of 
-- bytes: position x length.
--
data Region = Region !Int !Int
  deriving (Eq,Show)




-- | a TT or OT font file
data FontFile = FontFile 
        { ff_offset_table       :: OffsetTable
        , ff_head_table         :: HeadTable
        , ff_maxp_table         :: MaxpTable
        , ff_loca_table         :: LocaTable
        , ff_name_table         :: NameTable
        , ff_glyf_table         :: GlyfTable
        }
  deriving (Eq,Show)


--------------------------------------------------------------------------------
-- glyf table 

newtype GlyfTable = GlyfTable { glyfs :: [Glyf] }
  deriving (Eq,Show)

data Glyf = Glyf 
        { glyf_header           :: GlyfHeader
        , glyf_description      :: GlyfDescription
        }
  deriving (Eq,Show)

data GlyfHeader = GlyfHeader 
        { glyf_num_contours     :: Int16
        , glyf_bounding_rect    :: FWordBBox
        } 
  deriving (Eq,Show)
 

data GlyfDescription = 
      DescSimple     SimpleGlyf 
    | DescComposite  CompositeGlyf
  deriving (Eq,Show)    

data SimpleGlyf = SimpleGlyf
        { sglyf_end_points      :: [Uint16]
        , sglyf_instr_len       :: Uint16
        , sglyf_instructions    :: [Uint8]
        , sglyf_flags           :: [Uint8]
        , sglyf_outline_pts     :: [OutlinePoint]
        }
  deriving (Eq,Show)

data OutlinePoint = OnCurvePt  Short Short
                  | OffCurvePt Short Short
  deriving (Eq,Ord,Show)

data CompositeGlyf = CompositeGlyf
        { cglyf_flags           :: UShort
        , cglyf_index           :: UShort
        , cglyf_args            :: CompositeArgs
        , cglyf_trans           :: CompositeTrans

        }
   deriving (Eq,Show)

      
data CompositeArgs = 
      OffsetArgs 
        { x_offset    :: Int 
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
-- head table 

data HeadTable = HeadTable
        { ht_table_version_num        :: Fixed
        , ht_font_revision            :: Fixed
        , ht_check_sum_adjust         :: ULong
        , ht_magic_number             :: ULong
        , ht_head_flags               :: [HeadFlag]
        , ht_units_per_em             :: UShort
        , ht_created_timestamp        :: DateTime
        , ht_modified_timestamp       :: DateTime
        , ht_max_bb                   :: BBox
        , ht_mac_style                :: [MacStyle]
        , ht_smallest_readable_size   :: UShort
        , ht_font_direction_hint      :: Short      -- this could be a type...
        , ht_index_to_loc_format      :: LocaFormat
        , ht_glyph_data_format        :: Short
        }
  deriving (Eq,Show)

data LocaFormat = LocaShort | LocaLong
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
-- loca table

newtype LocaTable = LocaTable { loca_offsets :: [ULong] }
  deriving (Eq,Show)


--------------------------------------------------------------------------------
-- maxp table

data MaxpTable = MaxpTable 
        { maxp_version_number   :: Fixed
        , maxp_num_glyphs       :: UShort
        }
  deriving (Eq,Show)


--------------------------------------------------------------------------------
-- name table

data NameTable = NameTable
        { nt_format             :: UShort 
        , nt_count              :: UShort
        , nt_string_offset      :: UShort
        , nt_name_records       :: [NameRecord]
        }
  deriving (Eq,Show)

data NameRecord = NameRecord 
        { nr_platform_id     :: PlatformId
        , nr_encoding_id     :: EncodingId
        , nr_language_id     :: UShort
        , nr_name_id         :: NameId
        , nr_length          :: UShort
        , nr_offset          :: UShort
        , nr_name_text       :: String
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
-- offset table

data OffsetTable = OffsetTable 
        { ot_sfnt_version            :: SfntVersion
        , ot_number_of_tables        :: UShort
        , ot_search_range            :: UShort
        , ot_entry_selector          :: UShort
        , ot_range_shift             :: UShort
        }
  deriving (Eq,Show)

data SfntVersion = SFNT_1_0 | OTTO  
  deriving (Enum,Eq,Ord,Show) 
   
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
 
type BBox = BoundingBox Short

type FWordBBox = BoundingBox FWord

data BoundingBox a = BoundingBox 
        { x_min   :: !a
        , y_min   :: !a
        , x_max   :: !a
        , y_max   :: !a
        }
  deriving (Eq,Show)     

--------------------------------------------------------------------------------

-- Table locations map table names to there position in the font 
-- source file (where position is a Region - start, length) 
--
type TableLocs = Map.Map String Region 


-- The table directory contains a list of all tables and their offset (start) 
-- and length. Here they are called TableDescriptors. 
-- TableDescriptor (table_name,table_location  
data TableDescriptor = TableDescriptor 
        { table_name        :: String 
        , table_location    :: Region 
        }
  deriving (Eq,Show)

