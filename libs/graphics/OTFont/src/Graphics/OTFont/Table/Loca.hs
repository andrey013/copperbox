{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.OTFont.Table.Loca
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Loca - index to location mapping
--
--------------------------------------------------------------------------------


module Graphics.OTFont.Table.Loca where

import Graphics.OTFont.Datatypes
import Graphics.OTFont.Parse
import Graphics.OTFont.ParserExtras
import Graphics.OTFont.Pretty

import Control.Applicative

import Text.PrettyPrint.Leijen ( Pretty(..), string )

data ShortLocaTable = ShortLocaTable { 
      s_loca_offsets  :: USequence UShort
    }
  deriving (Eq,Show)

data LongLocaTable = LongLocaTable { 
      l_loca_offsets  :: USequence ULong
    }
  deriving (Eq,Show)


-- Note allignment issues to think about...

readShortLocaTable :: ULong -> Parser r ShortLocaTable
readShortLocaTable sz = ShortLocaTable <$>
    usequence (fromIntegral sz) ushort

readLongLocaTable :: ULong -> Parser r LongLocaTable
readLongLocaTable sz = LongLocaTable <$>
    usequence (fromIntegral sz) ulong
    
    
    
instance Pretty ShortLocaTable where
  pretty _t = ppTable "Loca Table (Short Version)" 
      [ string "todo" ]

instance Pretty LongLocaTable where
  pretty _t = ppTable "Loca Table (Long Version)" 
      [ string "todo" ] 

                 