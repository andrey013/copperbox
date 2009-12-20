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
        , sglyf_x_coordinates   :: [OutlinePoint]
        , sglyf_y_coordinates   :: [OutlinePoint] 
        }
  deriving (Eq,Show)


data OutlinePoint = OnCurve  DeltaInt16
                  | OffCurve DeltaInt16
  deriving (Eq,Ord,Show)

data DeltaInt16 = Same | DInt16 Int16
  deriving (Eq,Ord,Show)


data CompositeGlyf = CompositeGlyf
        { cglyf_flags           :: Uint16
        , cglyf_index           :: Uint16
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

