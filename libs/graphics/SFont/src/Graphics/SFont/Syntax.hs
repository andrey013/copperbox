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


--------------------------------------------------------------------------------
-- primitives

type Uint8   = Word8
type Uint16  = Word16
type Uint32  = Word32

-- ints as per Data.Int

-- represents numbers between -2.0 and 1.9999.
newtype ShortFrac = ShortFrac { unShortFrac :: Double }
  deriving (Eq,Ord,Num)

-- 16.16 signed fixed-point number
newtype Fixed = Fixed { unFixed :: Double }
  deriving (Eq,Ord,Num, Fractional)



-- 16 bit signed integer
newtype FWord = FWord { unFWord :: Int16 }
  deriving (Eq,Ord,Num)

-- 16 bit unsigned integer
newtype UFWord = UFWord { unUFWord :: Word16 }
  deriving (Eq,Ord,Num)

-- 16 bit fixed number with 14 bits representing the fraction.
newtype F2Dot14 = F2Dot14 { unF2Dot14 :: Double }
  deriving (Eq,Ord,Num)


newtype DateTime = DateTime { unDateTime :: Word64 }
  deriving (Eq,Ord,Num)

-- Instances

instance Show ShortFrac where
  show = show . unShortFrac
    
instance Read ShortFrac where
  readsPrec i s = map (\(d,r) -> (ShortFrac d,r)) $ readsPrec i s      

-- 

instance Show Fixed where 
  show = show . unFixed
    
instance Read Fixed where
  readsPrec i s = map (\(d,r) -> (Fixed d,r)) $ readsPrec i s      



-- 

instance Show FWord where 
  show = show . unFWord
    
instance Read FWord where
  readsPrec i s = map (\(d,r) -> (FWord d,r)) $ readsPrec i s   

--

instance Show UFWord where 
  show = show . unUFWord
    
instance Read UFWord where
  readsPrec i s = map (\(d,r) -> (UFWord d,r)) $ readsPrec i s  

--

instance Show F2Dot14 where
  show = show . unF2Dot14

instance Read F2Dot14 where
  readsPrec i s = map (\(d,r) -> (F2Dot14 d,r)) $ readsPrec i s 

--

instance Show DateTime where
  show = show . unDateTime

-- no read instance for DateTime



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
        , ff_cmap_table         :: CmapTable
        , ff_maxp_table         :: MaxpTable
        , ff_loca_table         :: LocaTable
        , ff_name_table         :: NameTable
        , ff_glyf_table         :: GlyfTable
        }
  deriving (Eq,Show)


-- offset table

data OffsetTable = OffsetTable 
        { ot_sfnt_version            :: SfntVersion
        , ot_number_of_tables        :: Uint16
        , ot_search_range            :: Uint16
        , ot_entry_selector          :: Uint16
        , ot_range_shift             :: Uint16
        }
  deriving (Eq,Show)

data SfntVersion = SFNT_1_0 | OTTO  
  deriving (Enum,Eq,Ord,Show) 


--------------------------------------------------------------------------------
-- cmap table

data CmapTable = CmapTable
        { cmap_index            :: CmapIndex
        , cmap_subtables        :: [CmapSubtable]
        }
  deriving (Eq,Show)

data CmapIndex = CmapIndex
        { cmap_version          :: Uint16
        , cmap_num_subtables    :: Uint16
        }
  deriving (Eq,Show)

data CmapSubtable = CmapSubtable
        { cmap_subtable_header  :: CmapSubtableHeader
        , cmap_subtable_body    :: CmapSubtableBody
        }
  deriving (Eq,Show)


-- Format and length are /promoted/ out of subtables and are 
-- considered part of the header. This is because we want 
-- both for making decisions regarding parsing.
data CmapSubtableHeader = CmapSubtableHeader
        { cmap_platform_id      :: Uint16
        , cmap_platfrom_spec_id :: Uint16
        , cmap_offset           :: Uint32
        }
  deriving (Eq,Show)



-- cmap subtables 

data CmapSubtableBody = CmapFmt0  CmapFormat0
                      | CmapFmt2  CmapFormat2
                      | CmapFmt4  CmapFormat4
                      | CmapFmt6  CmapFormat6
                      | CmapFmt8  CmapFormat8
                      | CmapFmt10 CmapFormat10
                      | CmapFmt12 CmapFormat12
  deriving (Eq,Show)

data CmapFormat0 = CmapFormat0
        { fmt0_length           :: Uint16       -- should be 262
        , fmt0_lang_code        :: Uint16
        , fmt0_glyf_idxs        :: [Uint8]      -- TODO should be an array
        }
  deriving (Eq,Show)


data CmapFormat2 = CmapFormat2
        { fmt2_lang_code        :: Uint16
        , fmt2_subheader_keys   :: [Uint16]      -- TODO should be an array
        , fmt2_subheaders       :: [Format2_Subheader]
        , fmt2_glyf_idxs        :: [Uint8]
        }
  deriving (Eq,Show)


data Format2_Subheader = Format2_SubHeader
        { fmt2_first_code       :: Uint16
        , fmt2_entry_count      :: Uint16
        , fmt2_id_delta         :: Int16
        , fmt2_id_range_offset  :: Uint16
        }
  deriving (Eq,Show)


-- Not sure this is correct...
data CmapFormat4 = CmapFormat4
        { fmt4_length           :: Uint16
        , fmt4_lang_code        :: Uint16
        , fmt4_segcount_x2      :: Uint16
        , fmt4_search_range     :: Uint16
        , fmt4_entry_selector   :: Uint16
        , fmt4_range_shift      :: Uint16
        , fmt4_end_code         :: [Uint16]
        , fmt4_reserved_pad     :: Uint16
        , fmt4_start_code       :: [Uint16]
        , fmt4_id_delta         :: [Uint16]
        , fmt4_id_range_offset  :: [Uint16]
        , fmt4_glyf_idxs        :: [Uint16]
        }
  deriving (Eq,Show)


data CmapFormat6 = CmapFormat6
        { fmt6_length           :: Uint16
        , fmt6_lang_code        :: Uint16
        , fmt6_first_code       :: Uint16
        , fmt6_entry_count      :: Uint16
        , fmt6_glyf_idxs        :: [Uint8]
        }
  deriving (Eq,Show)


data CmapFormat8 = CmapFormat8
        { fmt8_length           :: Uint32
        , fmt8_lang_code        :: Uint32
        , fmt8_is_32            :: ()
        , fmt8_num_groups       :: Uint32
        , fmt8_groups           :: [CmapGroup]
        }
  deriving (Eq,Show)


data CmapFormat10 = CmapFormat10
        { fmt10_length          :: Uint32
        , fmt10_lang_code       :: Uint32
        , fmt10_start_char_code :: Uint32
        , fmt10_num_chars       :: Uint32
        , fmt10_glf_idxs        :: [Uint16]
        }
  deriving (Eq,Show)


data CmapFormat12 = CmapFormat12
        { fmt12_length          :: Uint32
        , fmt12_lang_code       :: Uint32
        , fmt12_num_groups      :: Uint32
        , fmt12_groups          :: [CmapGroup]
        }
  deriving (Eq,Show)



data CmapGroup = CmapGroup
        { cgrp_start_char_code  :: Uint32
        , cgrp_end_char_code    :: Uint32
        , cgrp_start_glyph_code :: Uint32
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
    | DescCompound   CompoundGlyf
  deriving (Eq,Show)    

data SimpleGlyf = SimpleGlyf
        { sglyf_end_points      :: [Uint16]
        , sglyf_instr_len       :: Uint16
        , sglyf_instructions    :: [Uint8]
        , sglyf_flags           :: [Uint8]
        , sglyf_x_coordinates   :: [OutlinePoint]
        , sglyf_y_coordinates   :: [OutlinePoint] 
        }
  deriving (Eq,Show)


data OutlinePoint = OnCurve  DeltaInt16
                  | OffCurve DeltaInt16
  deriving (Eq,Ord,Show)

data DeltaInt16 = Same | DInt16 Int16
  deriving (Eq,Ord,Show)


data CompoundGlyf = CompoundGlyf
        { cglyf_components      :: [ComponentGlyf]
        , cglyf_instructions    :: [Word8] 
        }
   deriving (Eq,Show)

data ComponentGlyf = ComponentGlyf 
        { cglyf_flags           :: Uint16
        , cglyf_index           :: Uint16
        , cglyf_argument1       :: OffsetOrIndex
        , cglyf_argument2       :: OffsetOrIndex
        , cglyf_trans           :: Matrix4
        }
  deriving (Eq,Show)

-- Offset or index is either a byte or an int16 in the file - if 
-- it is a byte it gets scaled up to be an int16.
--
data OffsetOrIndex = CG_Offset Int16 | CG_Index Int16
  deriving (Eq,Ord,Show)
      
data Matrix4 = Matrix4 OneDot14 OneDot14 OneDot14 OneDot14
  deriving (Eq,Show)

-- | Matrix elements are not transformed on parsing
--
data OneDot14 = OneDot14_Const Double | OneDot14_Int16 Int16
  deriving (Eq,Ord,Show)

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
        , ht_check_sum_adjust         :: Uint32
        , ht_magic_number             :: Uint32
        , ht_head_flags               :: Uint16
        , ht_units_per_em             :: Uint16
        , ht_created_timestamp        :: DateTime
        , ht_modified_timestamp       :: DateTime
        , ht_max_bb                   :: BoundingBox FWord
        , ht_mac_style                :: Uint16
        , ht_smallest_readable_size   :: Uint16
        , ht_font_direction_hint      :: Int16      -- this could be a type...
        , ht_index_to_loc_format      :: LocaFormat
        , ht_glyph_data_format        :: Int16
        }
  deriving (Eq,Show)

data LocaFormat = LocaShort | LocaLong
  deriving (Eq,Show)

--------------------------------------------------------------------------------
-- loca table

-- If the LocaFormat is short it will be scaled up during parsing.
newtype LocaTable = LocaTable { loca_offsets :: [Uint32] }
  deriving (Eq,Show)


--------------------------------------------------------------------------------
-- maxp table

data MaxpTable = MaxpTable 
        { maxp_version_number   :: Fixed
        , maxp_num_glyphs       :: Uint16
        , maxp_max_points       :: Uint16
        , maxp_max_contours     :: Uint16
        , maxp_max_comp_parts   :: Uint16
        , maxp_max_comp_ctours  :: Uint16
        , maxp_max_zones        :: Uint16
        , maxp_max_twilight_pts :: Uint16
        , maxp_max_storage      :: Uint16
        , maxp_max_fun_defs     :: Uint16
        , maxp_max_instr_defs   :: Uint16
        , maxp_max_stack_elems  :: Uint16
        , maxp_max_size_instrs  :: Uint16
        , maxp_max_comp_elems   :: Uint16
        , maxp_max_comp_depth   :: Uint16
        }
  deriving (Eq,Show)


--------------------------------------------------------------------------------
-- name table

data NameTable = NameTable
        { nt_format             :: Uint16 
        , nt_count              :: Uint16
        , nt_string_offset      :: Uint16
        , nt_name_records       :: [NameRecord]
        }
  deriving (Eq,Show)

data NameRecord = NameRecord 
        { nr_platform_id     :: Uint16
        , nr_encoding_id     :: Uint16
        , nr_language_id     :: Uint16
        , nr_name_id         :: Uint16
        , nr_length          :: Uint16
        , nr_offset          :: Uint16
        , nr_name_text       :: String  -- This is deduced during parsing
        }
  deriving (Eq,Show)
   
--------------------------------------------------------------------------------
-- Common data types


-- Bounding box

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

