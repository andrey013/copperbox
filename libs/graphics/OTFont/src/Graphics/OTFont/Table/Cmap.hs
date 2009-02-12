{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.OTFont.Table.Cmap
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Character to Glyph mapping table
--
--------------------------------------------------------------------------------


module Graphics.OTFont.Table.Cmap where

import Graphics.OTFont.Datatypes
import Graphics.OTFont.Parse
import Graphics.OTFont.Pretty
import Graphics.OTFont.Table.CommonDatatypes

import Text.ZParse

import Control.Applicative
import Data.Array.Unboxed


import Text.PrettyPrint.Leijen ( Pretty(..) )

data CmapTable = CmapTable { 
      cmap_header       :: CmapHeader,
      encoding_records  :: [EncodingRecord]
    }
  deriving (Eq,Show)

readCmapTable :: Monad m => ReadData m CmapTable
readCmapTable = do 
    hdr@(CmapHeader _ i) <- readCmapHeader
    ts                   <- count (fromIntegral i) readEncodingRecord
    return $ CmapTable hdr ts
    
instance Pretty CmapTable where
  pretty t = ppTable "Cmap Table" (hd:rest) where
      hd    = pretty $ cmap_header t
      rest  =  map pretty $ encoding_records t 


data CmapHeader = CmapHeader {
      cmap_table_version_num  :: UShort,
      cmap_num_tables         :: UShort
  }
  deriving (Eq,Show)

      

readCmapHeader :: Monad m => ReadData m CmapHeader
readCmapHeader = CmapHeader <$>
    ushort <*> ushort

instance Pretty CmapHeader where
  pretty t = ppTable "cmap Header"  
      [ field "table_version_num"   24 (integral   $ cmap_table_version_num t)
      , field "num_tables"          24 (integral   $ cmap_num_tables t)
      ]


data EncodingRecord = EncodingRecord {
      cmap_platform_id  :: PlatformId,
      cmap_encoding_id  :: EncodingId,
      cmap_offset       :: ULong
    }
  deriving (Eq,Show)
  
readEncodingRecord :: Monad m => ReadData m EncodingRecord
readEncodingRecord = EncodingRecord <$>
          platformId  
      <*> encodingId
      <*> ulong
      
instance Pretty EncodingRecord where
  pretty t = ppTable "Encoding record"  
      [ field "platform_id"   24 (meaningParensEnum $ cmap_platform_id t)
      , field "encoding_id"   24 (meaningParensEnum $ cmap_encoding_id t)
      , field "offset"        24 (integral $ cmap_offset t)
      ]
      
      
--------------------------------------------------------------------------------

-- All the formats have (format::ushort) when parsing you could 
-- peek this to see what to do next... 

data CmapSubtable = 
      Format0_BE { 
          subtable_format       :: UShort,
          subtable_length       :: UShort,
          subtable_language     :: UShort,
          fmt0_glyph_id_array   :: UArray Int Byte
      }
    | Format2_HBM { 
          subtable_format       :: UShort,
          subtable_length       :: UShort,
          subtable_language     :: UShort,
          fmt2_sub_header_keys  :: UArray Int UShort,
          fmt2_sub_headers      :: [Format2_SubHeader],   -- change?
          fmt2_glyph_indexes    :: [UShort]               -- change?
      } 
    | Format4_SMDV { 
          subtable_format       :: UShort,
          subtable_length       :: UShort,
          subtable_language     :: UShort,
          fmt4_seg_count_x2     :: UShort,
          fmt4_search_range     :: UShort,
          fmt4_entry_selector   :: UShort,
          fmt4_range_shift      :: UShort,
          fmt4_end_code         :: UArray Int UShort,
          fmt4_reserved_pad     :: UShort,
          fmt4_start_code       :: UArray Int UShort,
          fmt4_id_delta         :: UArray Int Short,
          fmt4_id_range_offset  :: UArray Int UShort,
          fmt4_glyph_id_array   :: [UShort] 
      }
    | Format6_TTM { 
          subtable_format       :: UShort,
          subtable_length       :: UShort,
          subtable_language     :: UShort,
          fmt6_first_code       :: UShort,
          fmt6_entry_count      :: UShort,
          fmt6_glyph_id_array   :: UArray Int UShort
      } 
    | Format8_Mx { 
          subtable_format       :: UShort,
          reserved              :: UShort,
          ul_length             :: ULong,
          ul_language           :: ULong,
          fmt8_is_32            :: UArray Int Byte,
          fmt8_num_groups       :: ULong,
          fmt8_groups           :: [CharacterCodeGroup]
      }
    | Format10_TA { 
          subtable_format       :: UShort,
          reserved              :: UShort,
          ul_length             :: ULong,
          ul_language           :: ULong,
          fmt10_start_char_code :: ULong,
          fmt10_num_chars       :: ULong,
          fmt10_glyphs          :: [UShort]
      }
    | Format12_SC { 
          subtable_format       :: UShort,
          reserved              :: UShort,
          ul_length             :: ULong,
          ul_language           :: ULong,
          fmt12_num_groups      :: ULong,
          fmt12_groups          :: [CharacterCodeGroup]
      }        
  deriving (Eq,Show)
     
  
data Format2_SubHeader = Format2_SubHeader {
      first_code        :: UShort,
      entry_count       :: UShort,
      id_delta          :: Short,
      id_range_offset   :: UShort
    }
  deriving (Eq,Show)  
      
data CharacterCodeGroup = CharacterCodeGroup {
      start_char_code   :: ULong,
      end_char_code     :: ULong,
      start_glyph_id    :: ULong
    }
  deriving (Eq,Show) 


                              