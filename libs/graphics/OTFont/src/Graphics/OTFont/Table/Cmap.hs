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
import Graphics.OTFont.Utils
import Graphics.OTFont.Table.CommonDatatypes

import Text.ZParse

import Control.Applicative
import Data.Array.Unboxed
import Data.Int 
import Data.Word

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
      cmap_table_version_num  :: Word16,
      cmap_num_tables         :: Word16
  }
  deriving (Eq,Show)

      

readCmapHeader :: Monad m => ReadData m CmapHeader
readCmapHeader = CmapHeader <$>
          ushort  
      <*> ushort

instance Pretty CmapHeader where
  pretty t = ppTable "cmap Header"  
      [ field "table_version_num"   24 (integral   $ cmap_table_version_num t)
      , field "num_tables"          24 (integral   $ cmap_num_tables t)
      ]


data EncodingRecord = EncodingRecord {
      cmap_platform_id  :: PlatformId,
      cmap_encoding_id  :: EncodingId,
      cmap_offset       :: Word16
    }
  deriving (Eq,Show)
  
readEncodingRecord :: Monad m => ReadData m EncodingRecord
readEncodingRecord = EncodingRecord <$>
          platformId  
      <*> encodingId
      <*> ushort
      
instance Pretty EncodingRecord where
  pretty t = ppTable "Encoding record"  
      [ field "platform_id"   24 (ppMeaning $ cmap_platform_id t)
      , field "encoding_id"   24 (ppMeaning $ cmap_encoding_id t)
      , field "offset"        24 (integral $ cmap_offset t)
      ]
      
      
--------------------------------------------------------------------------------
      
data Format0_BE = Format0_BE {
      f0_format         :: Word16,
      f0_length         :: Word16,
      f0_language       :: Word16,
      f0_glyph_id_array :: UArray Word8 Word8
    }
  deriving (Eq,Show)


readFormat0_BE :: Monad m => ReadTable m Format0_BE
readFormat0_BE = Format0_BE <$>
          ushort  
      <*> ushort
      <*> ushort
      <*> uarray 256 byte
      
      

data Format2_HBM = Format2_HBM {
      f2_format           :: Word16,
      f2_length           :: Word16,
      f2_language         :: Word16,
      f2_sub_header_keys  :: UArray Word8 Word16,
      f2_sub_headers      :: [Format2_SubHeader],   -- change?
      f2_glyph_indexes    :: [Word16]               -- change?
    }
  deriving (Eq,Show)
  
data Format2_SubHeader = Format2_SubHeader {
      first_code        :: Word16,
      entry_count       :: Word16,
      id_delta          :: Int16,
      id_range_offset   :: Word16
    }
  deriving (Eq,Show)  
      
       
                            