{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.OTFont.View.Name
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- An interpretation (view) of the @name@ table.
--
--------------------------------------------------------------------------------


module Graphics.OTFont.View.Name where

import Graphics.OTFont.Datatypes
import Graphics.OTFont.Table.CommonDatatypes
import Graphics.OTFont.Table.Name
import Graphics.OTFont.Utils
import Graphics.OTFont.View.Relation4

import qualified Data.ByteString as BS
import Data.Char ( chr )



type NameRel = Rel4 PlatformId EncodingId Int NameId String

buildNameRel :: NameTable -> NameRel
buildNameRel (NameTable {name_records=recs, string_data=bs}) = 
    foldr fn empty recs
  where
    fn (NameRecord pid eid lid nid l o) r = 
        insert pid eid (fromIntegral lid) nid (extractText l o bs) r


    extractText :: Integral a => a -> a -> StringData -> String
    extractText l o s = 
        map (chr . fromIntegral) 
              $ BS.unpack 
              $ section (fromIntegral o) (fromIntegral l) s
          

allNames :: NameRel -> [String]
allNames = rangeL

   

     