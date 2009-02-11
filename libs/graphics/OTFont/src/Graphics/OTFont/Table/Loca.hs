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
import Graphics.OTFont.Pretty
import Graphics.OTFont.Utils
import Graphics.OTFont.Table.CommonDatatypes

import Text.ZParse

import Control.Applicative
import Data.Array.Unboxed
import Data.Int 
import Data.Word

import Text.PrettyPrint.Leijen ( Pretty(..), string )

data ShortLocaTable = ShortLocaTable { 
      s_loca_offsets  :: UArray Word32 Word16
    }
  deriving (Eq,Show)

data LongLocaTable = LongLocaTable { 
      l_loca_offsets  :: UArray Word32 Word32
    }
  deriving (Eq,Show)


-- Note allignment issues to think about...

readShortLocaTable :: Monad m => Word32 -> ReadData m ShortLocaTable
readShortLocaTable sz = ShortLocaTable <$>
    uarray sz ushort

readLongLocaTable :: Monad m => Word32 -> ReadData m LongLocaTable
readLongLocaTable sz = LongLocaTable <$>
    uarray sz ulong
    
    
    
instance Pretty ShortLocaTable where
  pretty t = ppTable "Loca Table (Short Version)" 
      [ string "todo" ]

instance Pretty LongLocaTable where
  pretty t = ppTable "Loca Table (Long Version)" 
      [ string "todo" ] 

                 