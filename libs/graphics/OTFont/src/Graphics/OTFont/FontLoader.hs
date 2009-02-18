{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.OTFont.FontLoader
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Cache loaded tables
--
--------------------------------------------------------------------------------


module Graphics.OTFont.FontLoader where

import Graphics.OTFont.Datatypes
import Graphics.OTFont.Parse
import Graphics.OTFont.ParseMonad
import Graphics.OTFont.ParserCombinators
import Graphics.OTFont.Pretty

import Control.Applicative
import qualified Data.Map as Map

import Text.PrettyPrint.Leijen ( Pretty(..), vsep )

type TableRegions = Map.Map String Region

data ProtoFace = ProtoFace {
      offset_table    :: OffsetTable,
      table_dirs      :: [TableDirectory],
      table_offsets   :: TableRegions
    }
  deriving (Eq,Show) 
  
  
data OffsetTable = OffsetTable {
      sfnt_version    :: String,
      num_tables      :: UShort,
      search_range    :: UShort,
      entry_selector  :: UShort,
      range_shift     :: UShort
    } 
  deriving (Eq,Show)      
      
data TableDirectory = TableDirectory {
      table_tag       :: String,
      check_sum       :: ULong,
      table_offset    :: ULong,
      table_length    :: ULong
    } 
  deriving (Eq,Show)  
 
findTableRegion :: String -> ProtoFace -> Maybe Region
findTableRegion name (ProtoFace _ _ fm) = Map.lookup name fm

          
--------------------------------------------------------------------------------    

parsefile :: FilePath -> (ProtoFace -> Parser r r) -> IO (Either ParseError r) 
parsefile path p = runParserFile path (protoFace >>= p) 


protoFace :: Parser r ProtoFace
protoFace = do 
    ot@(OffsetTable _ i _ _ _)  <- offsetTable
    dirs                        <- count (fromIntegral i) tableDirectory
    return $ ProtoFace ot dirs (mkTableRegions dirs) 
    
     
offsetTable :: Parser r OffsetTable 
offsetTable = OffsetTable
    <$> prefix <*> word16be <*> word16be <*> word16be <*> word16be
  where
    prefix = satisfies (count 4 char) (\s -> s == "OTTO" || s == o1oo)
    o1oo :: String
    o1oo = ['\0','\1','\0','\0']

tableDirectory :: Parser r TableDirectory 
tableDirectory = TableDirectory 
    <$> text 4 <*> word32be <*> word32be <*> word32be

mkTableRegions :: [TableDirectory] -> TableRegions
mkTableRegions ts = foldr fn Map.empty ts
  where
    fn (TableDirectory name _ o l) fm = 
        Map.insert name (fromIntegral o, fromIntegral l) fm
   
    
instance Pretty ProtoFace where
  pretty (ProtoFace ot dirs fm) = vsep $ 
      [ pretty ot 
      , vsep (map prettyThenLine dirs) 
      , field "map size" 16 (integral (Map.size fm))
      ]
      
      
instance Pretty OffsetTable where
  pretty (OffsetTable s nt sr es rs) = ppTable "Offset Table"  
      [ field "sfnt_version"    16 (pptag s)
      , field "num_tables"      16 (integral nt)
      , field "search_range"    16 (integral sr)
      , field "entry_selector"  16 (integral es)
      , field "range_shift"     16 (integral rs)
      ]

instance Pretty TableDirectory where
  pretty (TableDirectory t cs o tl) = ppTable "Table Directory" 
      [ field "tag"             16 (pptag t)
      , field "check_sum"       16 (integral cs)
      , field "offset"          16 (integral o)
      , field "table_length"    16 (integral tl)
      ]
      