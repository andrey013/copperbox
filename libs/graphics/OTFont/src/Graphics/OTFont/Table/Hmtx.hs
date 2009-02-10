{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.OTFont.Table.Hmtx
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Hmtx - horizontal metrics
--
--------------------------------------------------------------------------------


module Graphics.OTFont.Table.Hmtx where

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

data LongHorMetric = LongHorMetric { 
      lhm_advance_width   :: Word16,
      lhm_lsb             :: Int16
    } 
  deriving (Eq,Show)
  
data HmtxTable = HmtxTable { 
      h_metrics           :: [LongHorMetric],
      left_side_bearing   :: [Int16]
    }
  deriving (Eq,Show)


-- TODO
readHmtxTable :: Monad m => ReadData m HmtxTable
readHmtxTable = HmtxTable <$>
    undefined <*> undefined
    
instance Pretty HmtxTable where
  pretty t = ppTable "hmtx Table" 
      [ field "h_metrics"           24 (undefined)
      , field "left_side_bearing"   24 (undefined)
      ]

                          