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
import Graphics.OTFont.ParserCombinators
import Graphics.OTFont.ParserExtras
import Graphics.OTFont.Pretty
import Graphics.OTFont.Table.CommonDatatypes

import Control.Applicative
import Data.Array.Unboxed hiding ( array )


import Text.PrettyPrint.Leijen ( Pretty(..), Doc )

data CmapTable = CmapTable { 
      cmap_header       :: CmapHeader,
      encoding_records  :: [EncodingRecord],
      cmap_subtables    :: [CmapSubtable]
    }
  deriving (Eq,Show)

readCmapTable :: ParserM r CmapTable
readCmapTable = do 
    hdr@(CmapHeader _ i)  <- readCmapHeader
    ts                    <- count (fromIntegral i) readEncodingRecord
    -- ss                   <- count (fromIntegral i) readCmapSubtable
    s                     <- readCmapSubtable
    return $ CmapTable hdr ts [s] -- ss
    
instance Pretty CmapTable where
  pretty t = ppTable "Cmap Table" ((hd:rest) ++ rest2 ) where
      hd    = pretty $ cmap_header t
      rest  = map pretty $ encoding_records t
      rest2 = map pretty $ cmap_subtables t


data CmapHeader = CmapHeader {
      cmap_table_version_num  :: UShort,
      cmap_num_tables         :: UShort
  }
  deriving (Eq,Show)

      

readCmapHeader :: ParserM r CmapHeader
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
  
readEncodingRecord :: ParserM r EncodingRecord
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

data SubtableHeader = SubtableHeader {
      st_format   :: UShort,
      st_length   :: ULong,
      st_language :: ULong
    }
  deriving (Eq,Show)
  
  

data CmapSubtable = 
      Format0 { 
          st_header             :: SubtableHeader,
          fmt0_glyph_id_array   :: USequence Byte
      }
    | Format2 { 
          st_header             :: SubtableHeader,
          fmt2_sub_header_keys  :: USequence UShort,
          fmt2_sub_headers      :: [Format2_SubHeader],   -- change?
          fmt2_glyph_indexes    :: [UShort]               -- change?
      } 
    | Format4 { 
          st_header             :: SubtableHeader,
          search_params         :: Format4_SearchParams,
          fmt4_end_code         :: USequence UShort,
          fmt4_start_code       :: USequence UShort,
          fmt4_id_delta         :: USequence Short,
          fmt4_id_range_offset  :: USequence UShort,
          fmt4_glyph_id_array   :: USequence UShort 
      }
    | Format6 { 
          st_header             :: SubtableHeader,
          fmt6_first_code       :: UShort,
          fmt6_entry_count      :: UShort,
          fmt6_glyph_id_array   :: USequence UShort
      } 
    | Format8 { 
          st_header             :: SubtableHeader,
          fmt8_is_32            :: USequence Byte,
          fmt8_num_groups       :: ULong,
          fmt8_groups           :: [CharacterCodeGroup]
      }
    | Format10 { 
          st_header             :: SubtableHeader,
          fmt10_start_char_code :: ULong,
          fmt10_num_chars       :: ULong,
          fmt10_glyphs          :: Array Int UShort
      }
    | Format12 { 
          st_header             :: SubtableHeader,
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

data Format4_SearchParams = Format4_SearchParams {
      seg_count_x2              :: UShort,
      format4_search_range      :: UShort,
      format4_entry_selector    :: UShort,
      format4_range_shift       :: UShort
    }
  deriving (Eq,Show) 
  
    
data CharacterCodeGroup = CharacterCodeGroup {
      start_char_code   :: ULong,
      end_char_code     :: ULong,
      start_glyph_id    :: ULong
    }
  deriving (Eq,Show) 


readCmapSubtable :: ParserM r CmapSubtable 
readCmapSubtable = ushort >>= subtable 
  where
    subtable  0 = readFormat0
    subtable  2 = readFormat2
    subtable  4 = readFormat4
    subtable  6 = readFormat6
    subtable  8 = readFormat8
    subtable 10 = readFormat10
    subtable 12 = readFormat12
    subtable  i = error $ "unrecognized cmap subtable " ++ show i
    
    readFormat0   = Format0 <$>
                        subH 0 <*> usequence 256 byte
                            
    readFormat2   = Format2 <$>
                            subH 2 
                        <*> usequence 256 ushort 
                        <*> undefined
                        <*> undefined
                            
    readFormat4   = do hdr      <- subH 4
                       sparams  <- readFormat4_SearchParams
                       let seg_count = fromIntegral 
                                          $ (seg_count_x2 sparams) `div` 2
                       ec_arr   <- usequence seg_count ushort
                       _rp      <- ushort
                       sc_arr   <- usequence seg_count ushort
                       idd_arr  <- usequence seg_count short
                       idr_arr  <- usequence seg_count ushort
                       gid_arr  <- usequence 0 ushort
                       return $ Format4 hdr        sparams
                                        ec_arr     sc_arr 
                                        idd_arr    idr_arr
                                        gid_arr  
                                                 
    readFormat6   = do hdr      <- subH 6
                       fc       <- ushort
                       ec       <- ushort
                       gid_arr  <- usequence (fromIntegral ec) ushort
                       return $ Format6 hdr fc ec gid_arr
                           
    readFormat8   = do hdr      <- subH 8
                       is32_arr <- usequence 8192 byte
                       n_grps   <- ulong
                       grps     <- count (fromIntegral n_grps) readCharacterCodeGroup
                       return $ Format8 hdr is32_arr n_grps grps
                           
    readFormat10  = do hdr      <- subH 10
                       scc      <- ulong
                       n_chars  <- ulong
                       g_arr    <- bxsequence (fromIntegral n_chars) ushort 
                       return $ Format10 hdr scc n_chars g_arr
          
          
    readFormat12  = do hdr      <- subH 12
                       n_grps   <- ulong
                       grps     <- count (fromIntegral n_grps) readCharacterCodeGroup
                       return $ Format12 hdr n_grps grps    


subH ::UShort -> ParserM r SubtableHeader
subH fmt | fmt <= 6   = shortH
         | otherwise  = longH 
  where 
    shortH = (\l l' -> SubtableHeader fmt (fromIntegral l) (fromIntegral l'))
        <$> ushort <*> ushort
        
    -- longer instance has 2 bytes padding and two longs
    longH  = (SubtableHeader fmt)
        <$> (ushort *> ulong) <*> ulong
        
readFormat4_SearchParams :: ParserM r Format4_SearchParams
readFormat4_SearchParams = Format4_SearchParams <$>
    ushort <*> ushort <*> ushort <*> ushort

                       
        
readCharacterCodeGroup :: ParserM r CharacterCodeGroup
readCharacterCodeGroup = CharacterCodeGroup <$>
      ulong <*> ulong <*> ulong
      

instance Pretty CmapSubtable where
  pretty t@(Format0 {}) = 
      ppTable "Format 0: Byte Enconding Table" (hdr ++ xs) where
          hdr = subtableHeaderFields (st_header t)
          xs  = []
  
  pretty t@(Format2 {}) = 
      ppTable "Format 2: High Byte Mapping through Table" (hdr ++ xs) where
          hdr = subtableHeaderFields (st_header t)
          xs  = []  
      
  pretty t@(Format4 {}) = 
      ppTable "Format 4: Segment Mapping to Delta Values" (hdr ++ ys ++ xs) 
        where
          hdr = subtableHeaderFields (st_header t)
          ys  = searchParamFields (search_params t)
          xs  = [ field "endCode"         24 (ppArray integral $ fmt4_end_code t)
                , field "startCode"       24 (ppArray integral $ fmt4_start_code t)
                , field "idDelta"         24 (ppArray integral $ fmt4_id_delta t)
                , field "idRangeOffset"   24 (ppArray integral $ fmt4_id_range_offset t)
                , field "glyphIdArray"    24 (ppArray integral $ fmt4_glyph_id_array t)
                ]

  pretty t@(Format6 {}) = 
      ppTable "Format 6: High Byte Mapping through Table" (hdr ++ xs) where
          hdr = subtableHeaderFields (st_header t)
          xs  = []  
          
  pretty t@(Format8 {}) = 
      ppTable "Format 8: mixed 16-bit and 32-bit coverage" (hdr ++ xs) where
          hdr = subtableHeaderFields (st_header t)
          xs  = []  

  
  pretty t@(Format10 {}) = 
      ppTable "Format 6: Trimmed array" (hdr ++ xs) where
          hdr = subtableHeaderFields (st_header t)
          xs  = []    
  
  pretty t@(Format12 {}) = 
      ppTable "Format 12: Segmented Coverage" (hdr ++ xs) where
          hdr = subtableHeaderFields (st_header t)
          xs  = []  

subtableHeaderFields :: SubtableHeader -> [Doc]
subtableHeaderFields (SubtableHeader fmt len lang) = 
    [ field "format"          24 (integral fmt)
    , field "length"          24 (integral len)
    , field "language"        24 (integral lang)
    ]

searchParamFields :: Format4_SearchParams -> [Doc]
searchParamFields (Format4_SearchParams sc sr es rs) =
    [ field "segCountX2"      24 (integral sc)
    , field "searchRange"     24 (integral sr)
    , field "entrySelector"   24 (integral es)
    , field "rangeShift"      24 (integral rs)
    ]
                                  