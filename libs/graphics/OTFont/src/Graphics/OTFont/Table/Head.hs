{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.OTFont.Table.Head
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Head Table
--
--------------------------------------------------------------------------------


module Graphics.OTFont.Table.Head where

import Graphics.OTFont.Datatypes
import Graphics.OTFont.Parse
import Graphics.OTFont.Pretty
import Graphics.OTFont.Utils

import Control.Applicative
-- import qualified Data.ByteString as BS
import Data.Int 
import Data.Word

import Text.PrettyPrint.Leijen ( Pretty(..) )


data HeadTable = HeadTable {
      table_version_num       :: Fixed,
      font_revision           :: Fixed,
      check_sum_adjust        :: Word32,
      magic_number            :: Word32,
      head_flags              :: [HeadFlag],
      units_per_em            :: Word16,
      created_timestamp       :: DateTime,
      modified_timestamp      :: DateTime,
      all_x_min               :: Int16,
      all_y_min               :: Int16,
      all_x_max               :: Int16,
      all_y_max               :: Int16,
      mac_style               :: [MacStyle],
      smallest_readable_size  :: Word16,
      font_direction_hint     :: Int16,      -- this could be a type...
      index_to_loc_format     :: Int16,
      glyph_data_format       :: Int16
  }
  deriving (Eq,Show)

readHeadTable :: Monad m => ReadTable m HeadTable
readHeadTable = HeadTable <$>
          fixed 
      <*> fixed 
      <*> ulong 
      <*> ulong
      <*> bitfield ushort       -- head_flags 
      <*> ushort  
      <*> longDateTime
      <*> longDateTime
      <*> short
      <*> short
      <*> short
      <*> short
      <*> bitfield ushort       -- mac_style
      <*> ushort
      <*> short
      <*> short
      <*> short

instance Pretty HeadTable where
  pretty t = ppTable "Head Table"  
      [ field "table_version_num"       24 (pretty     $ table_version_num t)
      , field "font_revision"           24 (pretty     $ font_revision t)
      , field "check_sum_adjust"        24 (integral   $ check_sum_adjust t)
      , field "magic_number"            24 (integral   $ magic_number t)
      , field "head_flags"              24 (ppBitfield $ head_flags t)
      , field "units_per_em"            24 (integral   $ units_per_em t)
      , field "created_timestamp"       24 (pretty     $ created_timestamp t)
      , field "modified_timestamp"      24 (pretty     $ modified_timestamp t)
      , field "x_min"                   24 (integral   $ all_x_min t)
      , field "y_min"                   24 (integral   $ all_y_min t)
      , field "x_max"                   24 (integral   $ all_x_max t)
      , field "y_max"                   24 (integral   $ all_y_max t)
      , field "mac_style"               24 (ppBitfield $ mac_style t)
      , field "smallest_readable_size"  24 (integral   $ smallest_readable_size t)
      , field "font_direction_hint"     24 (integral   $ font_direction_hint t)
      , field "index_to_loc_format"     24 (integral   $ index_to_loc_format t)
      , field "glyph_data_format"       24 (integral   $ glyph_data_format t)
      ]
      
instance BoundingBox HeadTable where
  x_min = all_x_min
  y_min = all_y_min
  x_max = all_x_max
  y_max = all_y_max
      
      

  
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

instance Meaning HeadFlag where
  meaning H0_Baseline_at_y          = "Baseline at y"
  meaning H1_Left_sidebearing_at_x  = "Left sidebearing at x"
  meaning H2_Depends_pt_size        = "Instructions may depend on point size"
  meaning H3_Force_ppem_int         = "Force ppem to integer values"
  meaning H4_Alter_adv_width        = "Instructions may alter advance width"
  meaning H5_Undefined              = "Not defined"
  meaning H6_Undefined              = "Not defined"
  meaning H7_Undefined              = "Not defined"
  meaning H8_Undefined              = "Not defined"
  meaning H9_Undefined              = "Not defined"
  meaning H10_Undefined             = "Not defined"
  meaning H11_Lossless_compression  = "Font data is lossless"
  meaning H12_Converted             = "Font converted"
  meaning H13_ClearType_optimised   = "Font optimised for ClearType"
  meaning H14_Reserved              = "Reserved"
  meaning H15_Reserved              = "Reserved"
  
  
instance Marshal HeadFlag where marshal = fromEnum
instance Unmarshal HeadFlag where unmarshal = toEnum

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

instance Meaning MacStyle where
  meaning S0_Bold         = "Bold"
  meaning S1_Italic       = "Itlaic"
  meaning S2_Underline    = "Underline"
  meaning S3_Outline      = "Outline"
  meaning S4_Shadow       = "Shadow"
  meaning S5_Condensed    = "Condensed"
  meaning S6_Extended     = "Extended"
  meaning _               = "Reserved"


instance Marshal MacStyle where marshal = fromEnum
instance Unmarshal MacStyle where unmarshal = toEnum


                            