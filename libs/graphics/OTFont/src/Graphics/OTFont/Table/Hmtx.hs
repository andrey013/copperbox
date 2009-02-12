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

import Control.Applicative


import Text.PrettyPrint.Leijen ( Pretty(..) )

data LongHorMetric = LongHorMetric { 
      lhm_advance_width   :: UShort,
      lhm_lsb             :: Short
    } 
  deriving (Eq,Show)
  
data HmtxTable = HmtxTable { 
      h_metrics           :: [LongHorMetric],
      left_side_bearing   :: [Short]
    }
  deriving (Eq,Show)


-- TODO
readHmtxTable :: Monad m => ReadData m HmtxTable
readHmtxTable = HmtxTable <$>
    undefined <*> undefined
    
instance Pretty HmtxTable where
  pretty _t = ppTable "hmtx Table" 
      [ field "h_metrics"           24 (undefined)
      , field "left_side_bearing"   24 (undefined)
      ]

                          