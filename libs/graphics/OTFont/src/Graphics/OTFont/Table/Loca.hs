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
import Graphics.OTFont.ParserCombinators
import Graphics.OTFont.ParserExtras
import Graphics.OTFont.Pretty

import Control.Applicative
import Data.Array.Unboxed


import Text.PrettyPrint.Leijen ( Pretty(..), string )

data ShortLocaTable = ShortLocaTable { 
      s_loca_offsets  :: UArray ULong UShort
    }
  deriving (Eq,Show)

data LongLocaTable = LongLocaTable { 
      l_loca_offsets  :: UArray ULong ULong
    }
  deriving (Eq,Show)


-- Note allignment issues to think about...

readShortLocaTable :: ULong -> ParserM r ShortLocaTable
readShortLocaTable sz = ShortLocaTable <$>
    usequence sz ushort

readLongLocaTable :: ULong -> ParserM r LongLocaTable
readLongLocaTable sz = LongLocaTable <$>
    usequence sz ulong
    
    
    
instance Pretty ShortLocaTable where
  pretty _t = ppTable "Loca Table (Short Version)" 
      [ string "todo" ]

instance Pretty LongLocaTable where
  pretty _t = ppTable "Loca Table (Long Version)" 
      [ string "todo" ] 

                 