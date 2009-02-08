--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.OTFont.Datatypes
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Datatypes
--
--------------------------------------------------------------------------------


module Graphics.OTFont.Datatypes where

import qualified Data.ByteString as BS
import qualified Data.Map as Map
import Data.Word

type TableStreams = Map.Map String BS.ByteString

data LaxFont = LaxFont {
      offset_table    :: OffsetTable,
      table_dirs      :: [TableDirectory],
      table_data      :: TableStreams
    }
  deriving (Eq,Show) 
  
  
data OffsetTable = OffsetTable {
      sfnt_version    :: String,
      num_tables      :: Word16,
      search_range    :: Word16,
      entry_selector  :: Word16,
      range_shift     :: Word16
    } 
  deriving (Eq,Show)      
      
data TableDirectory = TableDirectory {
      tag             :: String,
      check_sum       :: Word32,
      offset          :: Word32,
      table_length    :: Word32
    } 
  deriving (Eq,Show)  
 

data CmapHeader = CmapHeader {
      cmap_version    :: Word16,
      cmap_num_tables :: Word16
    } 
  deriving (Eq,Show)  

  
data NameTable = NameTable { 
      nt_format       :: Word16,
      nt_count        :: Word16,
      string_offset   :: Word16,
      name_records    :: [NameRecord],
      string_data     :: BS.ByteString
    }
  deriving (Eq,Show)
  
data NameRecord = NameRecord {
      platform_id     :: Word16,
      encoding_id     :: Word16,
      language_id     :: Word16,
      name_id         :: Word16,
      string_length   :: Word16,
      str_offset      :: Word16
    }
  deriving (Eq,Show)
    