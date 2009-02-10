{-# OPTIONS -Wall #-}
{-# OPTIONS -fno-warn-orphans #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.OTFont.Pretty
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Pretty print
--
--------------------------------------------------------------------------------


module Graphics.OTFont.Pretty where

import Graphics.OTFont.Datatypes
import Graphics.OTFont.Utils

import Data.Char
import qualified Data.Map as Map 
import Numeric ( showHex )
import Text.PrettyPrint.Leijen



ppMeaning :: Meaning a => a -> Doc
ppMeaning = text . meaning

instance Pretty LaxFont where
  pretty (LaxFont ot dirs fm) = 
      pretty ot <$> vsep (map prettyThenLine dirs) 
                <$> field "map size" 16 (integral (Map.size fm))

instance Pretty OffsetTable where
  pretty (OffsetTable s nt sr es rs) = title "Offset Table"  
      <$> field "sfnt_version"    16 (pptag s)
      <$> field "num_tables"      16 (integral nt)
      <$> field "search_range"    16 (integral sr)
      <$> field "entry_selector"  16 (integral es)
      <$> field "range_shift"     16 (integral rs)


instance Pretty TableDirectory where
  pretty (TableDirectory t cs o tl) = title "Table Directory" 
      <$> field "tag"             16 (pptag t)
      <$> field "check_sum"       16 (integral cs)
      <$> field "offset"          16 (integral o)
      <$> field "table_length"    16 (integral tl)

      
instance Pretty CmapHeader where
  pretty (CmapHeader v nt) = title "Cmap Header"
      <$> field "cmap_version"    16 (integral v)
      <$> field "num_tables"      16 (integral nt)


instance Pretty NameTable where
  pretty (NameTable nf nc so ns _) = title "Name Table" 
      <$> field "nt_format"       16 (integral nf)
      <$> field "nt_count"        16 (integral nc)
      <$> field "string_offset"   16 (integral so)
      <$> field "name_records"    16 (indent 0 $ vsep (map prettyThenLine ns))
      
  
instance Pretty NameRecord where 
  pretty (NameRecord pid ei li ni sl so) = title "Name Record"
      <$> field "platform_id"     16 (integral pid)
      <$> field "encoding_id"     16 (integral ei)
      <$> field "language_id"     16 (integral li)
      <$> field "name_id"         16 (integral ni)
      <$> field "string_length"   16 (integral sl)
      <$> field "str_offset"      16 (integral so)


prettyThenLine :: Pretty a => a -> Doc
prettyThenLine a = pretty a <> line

ppTable :: String -> [Doc] -> Doc
ppTable s ds = title s <$> vsep ds
  
title :: String -> Doc   
title s = string s <$> uline (length s) where
  uline i = text $ replicate i '-'

field :: String -> Int -> Doc -> Doc
field s i d = fill i (text s) <> equals <+> d
 
integral :: Integral i => i -> Doc
integral = integer . fromIntegral

instance Pretty Fixed where
  pretty = double . unFixed

ppBitfield :: Meaning a => [a] -> Doc
ppBitfield = list . map ppMeaning 

instance Pretty DateTime where
  pretty _ = text "ugh!" 

pptag :: String -> Doc
pptag s | all isPrint s = text s
        | otherwise     = hcat $ map (f . ord) s 
  where 
    f i | i < 10    = char '0' <> pphex i
        | otherwise = pphex i 

pphex :: Integral i => i -> Doc
pphex = text . (showHex `flip` [])

